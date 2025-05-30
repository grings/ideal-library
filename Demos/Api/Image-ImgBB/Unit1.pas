unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.TabControl, FMX.Memo.Types,
  FMX.ScrollBox, FMX.Memo,

  IdeaL.Lib.Api.Image.ImgBB, FMX.Edit, FMX.Objects, FMX.Layouts;

type
  TForm1 = class(TForm)
    Button1: TButton;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    ProgressBar1: TProgressBar;
    memoResponse: TMemo;
    Edit1: TEdit;
    Edit2: TEdit;
    Layout1: TLayout;
    Layout2: TLayout;
    Button2: TButton;
    ProgressBar2: TProgressBar;
    Image1: TImage;
    Edit3: TEdit;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Edit1Typing(Sender: TObject);
  private
    const
    CConfigFileName = 'config.json';

    procedure SaveData;
    procedure LoadData;

    procedure SendDataEvent(const Sender: TObject; AContentLength: Int64; AWriteCount: Int64; var AAbort: Boolean);
    procedure ReceiveDataEvent(const Sender: TObject; AContentLength: Int64; AReadCount: Int64; var AAbort: Boolean);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  System.JSON,
  System.IOUtils,
  REST.Json;

{$R *.fmx}

procedure TForm1.Button1Click(Sender: TObject);
begin
  {
  It's not necessary to use Threads, I'm using it just to make it more responsive to user
  }
  ProgressBar1.Value := 0;
  memoResponse.Lines.Clear;
  TThread.CreateAnonymousThread(
  procedure
  var
    LResponse: TImageJsonResponse;
  begin
    LResponse := TImgBBApi.Upload(
      Edit1.Text,
      Edit2.Text,
      ftLocalFile,
      SendDataEvent
      );
    TThread.Synchronize(nil,
    procedure
    begin
      memoResponse.Lines.Add('Status: ' + LResponse.StatusCode.ToString + ' - ' + LResponse.StatusText);
      memoResponse.Lines.Add('Id: ' + LResponse.Id);
      memoResponse.Lines.Add('Title: ' + LResponse.Title);
      memoResponse.Lines.Add('Size: ' + LResponse.Size.ToString);
      memoResponse.Lines.Add('Mime: ' + LResponse.Mime);
      memoResponse.Lines.Add('UrlDelete: ' + LResponse.UrlDelete);
      memoResponse.Lines.Add('UrlImage: ' + LResponse.UrlImage);
      memoResponse.Lines.Add('UrlMedium: ' + LResponse.UrlMedium);
      memoResponse.Lines.Add('UrlThumb: ' + LResponse.UrlThumb);
      memoResponse.Lines.Add('Json: ' + LResponse.Json);
    end);
  end).Start;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  ProgressBar1.Value := 0;
  Image1.Bitmap.Assign(nil);

  TThread.CreateAnonymousThread(
  procedure
  var
    LFilePath: string;
    LFileExt: string;
  begin
    LFileExt := ExtractFileExt(Edit3.Text);
    LFilePath := System.SysUtils.GetCurrentDir;
    LFilePath := System.IOUtils.TPath.Combine(LFilePath, 'Image' + LFileExt);
    if TImgBBApi.Download(Edit3.Text, LFilePath, ReceiveDataEvent) then
    begin
      TThread.Synchronize(nil,
      procedure
      begin
        Image1.Bitmap.LoadFromFile(LFilePath);
      end);
    end;
  end).Start;
end;

procedure TForm1.Edit1Typing(Sender: TObject);
begin
  SaveData;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  TabControl1.TabIndex := 0;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  LoadData;
end;

procedure TForm1.LoadData;
begin
  var
  LJson := EmptyStr;
  if FileExists(CConfigFileName) then
    LJson := TFile.ReadAllText(CConfigFileName);

  if LJson.Trim.IsEmpty then
    Exit;

  var
  LJSONObj := TJSONObject.ParseJSONValue(LJson) as TJSONObject;
  // edtExportedToken.OnTyping := False;
  try
    var
    LStr := EmptyStr;
    if LJSONObj.TryGetValue<string>('key', LStr) then
      Edit1.Text := LStr;
    if LJSONObj.TryGetValue<string>('fullFilePath', LStr) then
      Edit2.Text := LStr;
  finally
    LJSONObj.Free;
  end;
end;

procedure TForm1.ReceiveDataEvent(const Sender: TObject; AContentLength,
  AReadCount: Int64; var AAbort: Boolean);
var
  LValue: Single;
begin
  if not AAbort then
  begin
    LValue := 0;
    if AContentLength > 0 then
      LValue := (AReadCount / AContentLength) * 100;
    if LValue < 0 then
      LValue := 0;
    // Also it's not necessary to use Synchronize, just if you using Threads
    TThread.Synchronize(nil,
    procedure
    begin
      ProgressBar2.Value := LValue;
    end);
  end;
end;

procedure TForm1.SaveData;
begin
  var
  LJSONObj := TJSONObject.Create;
  try
    LJSONObj.AddPair('key', Edit1.Text);
    LJSONObj.AddPair('fullFilePath', Edit2.Text);
    TFile.WriteAllText(CConfigFileName, LJSONObj.ToString);
  finally
    LJSONObj.Free;
  end;
end;

procedure TForm1.SendDataEvent(const Sender: TObject; AContentLength,
  AWriteCount: Int64; var AAbort: Boolean);
var
  LValue: Single;
begin
  if not AAbort then
  begin
    LValue := 0;
    if AContentLength > 0 then
      LValue := (AWriteCount / AContentLength) * 100;
    if LValue < 0 then
      LValue := 0;
    // Also it's not necessary to use Synchronize, just if you using Threads
    TThread.Synchronize(nil,
    procedure
    begin
      ProgressBar1.Value := LValue;
    end);
  end;
end;

end.
