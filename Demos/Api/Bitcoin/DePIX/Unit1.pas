unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Edit, FMX.Layouts, FMX.Controls.Presentation, FMX.Memo.Types,
  FMX.ScrollBox, FMX.Memo, FMX.ListBox, FMX.Objects, FMX.DateTimeCtrls;

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
    edtAmount: TEdit;
    btnDePIXCreate: TButton;
    edtAddress: TEdit;
    edtSplitAddress: TEdit;
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
    edtSplitFee: TEdit;
    gbxDepositList: TGroupBox;
    Label7: TLabel;
    btnGetDepositsList: TButton;
    bgxDepositStatus: TGroupBox;
    Label10: TLabel;
    edtDepositStatus: TEdit;
    btnGetDepositStatus: TButton;
    dedtStart: TDateEdit;
    Label11: TLabel;
    dedtEnd: TDateEdit;
    cbxStatus: TComboBox;
    Label12: TLabel;
    Label4: TLabel;
    edtUserTaxNumber: TEdit;
    procedure btnPrivateDataShowHideClick(Sender: TObject);
    procedure edtExportedTokenTyping(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnDePIXCreateClick(Sender: TObject);
    procedure lbxJsonItemClick(const Sender: TCustomListBox;
      const Item: TListBoxItem);
    procedure FormCreate(Sender: TObject);
    procedure btnQrCodeFromInvoiceClick(Sender: TObject);
    procedure rctQrCodePicBackgroundClick(Sender: TObject);
    procedure btnGetDepositsListClick(Sender: TObject);
    procedure btnGetDepositStatusClick(Sender: TObject);
  private
    const
    CConfigFileName = 'config.json';

    procedure SaveData;
    procedure LoadData;

    procedure AddListBox(ATitle, AJson: string);
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
  System.DateUtils,
  System.JSON,
  System.IOUtils,

  REST.Json,

  System.Net.URLClient,
  System.Net.HttpClient,
  System.Net.HttpClientComponent,

  IdeaL.Lib.Api.Bitcoin.DePIX;

{$R *.fmx}

procedure TForm1.AddListBox(ATitle, AJson: string);
begin
  var
  lbItem := TListBoxItem.Create(lbxJson);
  lbItem.Parent := lbxJson;
  lbItem.Text := FormatDateTime('hh:mm:ss.zzz', Now) + ' ' + ATitle;
  lbItem.TagString := AJson;

  AddLog(lbItem.Text + sLineBreak + AJson);
end;

procedure TForm1.AddLog(ALog: string);
begin
  Memo1.Lines.Text := ALog;
end;

procedure TForm1.btnDePIXCreateClick(Sender: TObject);
begin
  var
  LAPI := TAPIDePix.Create;
  try
    LAPI.AuthToken(edtExportedToken.Text);
    var
    LAmountInCents : Integer := Trunc(StrToFloat(edtAmount.Text) * 100);
    var
    LResult := LAPI.PostDeposit(
      LAmountInCents,
      edtAddress.Text,
      edtSplitAddress.Text,
      edtSplitFee.Text,
      edtUserTaxNumber.Text);
    AddListBox('Create', LResult);
  finally
    LAPI.Free;
  end;
end;

procedure TForm1.btnGetDepositsListClick(Sender: TObject);
begin
  var
  LAPI := TAPIDePix.Create;
  try
    LAPI.AuthToken(edtExportedToken.Text);
    var
    LResult := LAPI.GetDeposits(dedtStart.Date, dedtEnd.Date, cbxStatus.Text);
    AddListBox('List', LResult);
  finally
    LAPI.Free;
  end;
end;

procedure TForm1.btnGetDepositStatusClick(Sender: TObject);
begin
  var
  LAPI := TAPIDePix.Create;
  try
    LAPI.AuthToken(edtExportedToken.Text);
    var
    LResult := LAPI.GetDepositStatus(edtDepositStatus.Text);
    AddListBox('Status', LResult);
  finally
    LAPI.Free;
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
  // It's not related with DePIX, it's just extra stuff
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
  dedtStart.Date := IncYear(Now, -1);
  dedtEnd.Date := IncDay(Now, 1);
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  HorzScrollBox1.ShowScrollBars := True;
  LoadData;
  QrCodePicHide;
end;

procedure TForm1.lbxJsonItemClick(const Sender: TCustomListBox;
  const Item: TListBoxItem);
begin
  var
  LJson := TJSONObject.ParseJSONValue(Item.TagString);
  try
    memBeautyJson.Lines.Text := TJSON.Format(LJson);
    AddLog(
     Item.Text + sLineBreak +
     Item.TagString
     );
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
