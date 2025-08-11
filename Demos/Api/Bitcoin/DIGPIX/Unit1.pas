unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Edit, FMX.Layouts, FMX.Controls.Presentation, FMX.Memo.Types,
  FMX.ScrollBox, FMX.Memo, FMX.ListBox, FMX.Objects;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    Label8: TLabel;
    lytExportedToken: TLayout;
    Label5: TLabel;
    edtExportedToken: TEdit;
    btnPrivateDataShowHide: TButton;
    Label2: TLabel;
    Layout1: TLayout;
    HorzScrollBox1: THorzScrollBox;
    gbxDePIXCreate: TGroupBox;
    Label3: TLabel;
    lblAmount: TLabel;
    edtDePIXAmount: TEdit;
    btnDePIXCreate: TButton;
    edtDePIXCreateWebHook: TEdit;
    edtDePIXCreateSecret: TEdit;
    Label9: TLabel;
    Label16: TLabel;
    lbxJson: TListBox;
    lytBeutyJson: TLayout;
    memBeautyJson: TMemo;
    Label6: TLabel;
    btnQrCodeFromInvoice: TButton;
    Memo1: TMemo;
    Splitter1: TSplitter;
    lytQrCodePicBackground: TLayout;
    rctQrCodePicBackground: TRectangle;
    lytQrCodePic: TLayout;
    Layout2: TLayout;
    btnQrCodePicClose: TButton;
    imgQrCode: TImage;
    procedure btnPrivateDataShowHideClick(Sender: TObject);
    procedure edtExportedTokenTyping(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnDePIXCreateClick(Sender: TObject);
    procedure lbxJsonItemClick(const Sender: TCustomListBox;
      const Item: TListBoxItem);
    procedure FormCreate(Sender: TObject);
    procedure btnQrCodeFromInvoiceClick(Sender: TObject);
    procedure rctQrCodePicBackgroundClick(Sender: TObject);
  private
    const
    CConfigFileName = 'config.json';

    procedure SaveData;
    procedure LoadData;

    procedure AddListBox(AJson: string);
    procedure AddLog(ALog: string);

    procedure QrCodePicHide;
    procedure QrCodePicShow;
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

  REST.Json,

  System.Net.URLClient,
  System.Net.HttpClient,
  System.Net.HttpClientComponent,

  IdeaL.Lib.Api.Bitcoin.DIGPIX;

{$R *.fmx}

procedure TForm1.AddListBox(AJson: string);
begin
  lbxJson.Items.Insert(0, AJson);
end;

procedure TForm1.AddLog(ALog: string);
begin
  ALog := DateTimeToStr(Now) + ' - ' + ALog;
  Memo1.Lines.Insert(0, ALog);
end;

procedure TForm1.btnDePIXCreateClick(Sender: TObject);
begin
  {
  ONLY FOR TESTING !!!!!!
  Don't you have a WebHook? Please, use a public one on: https://webhook.site/
  }

  var
  LAPIDigPix := TAPIDigPix.Create;
  try
    LAPIDigPix.AuthToken(edtExportedToken.Text);
    var
    LAmount : Currency := StrToFloat(edtDePIXAmount.Text);
    var
    LResult := LAPIDigPix.PostQrCode(LAmount, edtDePIXCreateWebHook.Text, edtDePIXCreateSecret.Text);
    AddListBox(LResult);
    AddLog(LResult);
  finally
    LAPIDigPix.Free;
  end;
end;

procedure TForm1.btnPrivateDataShowHideClick(Sender: TObject);
begin
  edtExportedToken.Password := not edtExportedToken.Password;
end;

procedure TForm1.btnQrCodeFromInvoiceClick(Sender: TObject);
var
  HttpClient: THttpClient;
  HttpResponse: IHttpResponse;
begin
  // It's not related with DIGPIX, it's just extra stuff
  // You must be showing an Invoice valid JSON on the Memo
  // Tap the ListBox lbxJson, the correct Invoice result to show it in the memo
  var
  LUrlCopyAndPaste := EmptyStr;
  var
  LJson := TJSONObject.ParseJSONValue(memBeautyJson.Lines.Text);
  try
    var
    LJsonStr := EmptyStr;
    var
    LJObj : TJSONObject;
    if not LJson.TryGetValue<TJSONObject>('payment_data', LJObj) then
      raise Exception.Create('The JSON does not look like an Invoice. "payment_data" not found');

    if not LJObj.TryGetValue<TJSONObject>('response', LJObj) then
      raise Exception.Create('The JSON does not look like an Invoice. "response" not found');

    if not LJObj.TryGetValue<string>('qrImageUrl', LUrlCopyAndPaste) then
      raise Exception.Create('The JSON does not look like an Invoice. "qrImageUrl" not found');
  finally
    FreeAndNil(LJson);
  end;

  HttpClient := THttpClient.Create;
  try
    HttpResponse := HttpClient.Get(LUrlCopyAndPaste);

    var
    LStrm := HttpResponse.ContentStream;
    imgQrCode.Bitmap.LoadFromStream(LStrm);
    QrCodePicShow;
  finally
    HttpClient.Free;
  end;
end;

procedure TForm1.edtExportedTokenTyping(Sender: TObject);
begin
   SaveData;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FormatSettings.DecimalSeparator := '.';
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  LoadData;
  QrCodePicHide;
end;

procedure TForm1.lbxJsonItemClick(const Sender: TCustomListBox;
  const Item: TListBoxItem);
begin
  var
  LJson := TJSONObject.ParseJSONValue(Item.Text);
  try
    memBeautyJson.Lines.Text := TJSON.Format(LJson);
  finally
    FreeAndNil(LJson);
  end;
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
    if LJSONObj.TryGetValue<string>('exportedToken', LStr) then
      edtExportedToken.Text := LStr;
  finally
    LJSONObj.Free;
  end;
end;

procedure TForm1.QrCodePicHide;
begin
  lytQrCodePicBackground.Visible := False;
end;

procedure TForm1.QrCodePicShow;
begin
  lytQrCodePicBackground.Visible := True;
  lytQrCodePicBackground.BringToFront;
end;

procedure TForm1.rctQrCodePicBackgroundClick(Sender: TObject);
begin
  QrCodePicHide;
end;

procedure TForm1.SaveData;
begin
  var
  LJSONObj := TJSONObject.Create;
  try
    LJSONObj.AddPair('exportedToken', edtExportedToken.Text);
    TFile.WriteAllText(CConfigFileName, LJSONObj.ToString);
  finally
    LJSONObj.Free;
  end;
end;

end.
