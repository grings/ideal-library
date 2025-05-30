unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Memo.Types, FMX.Layouts,
  FMX.ListBox, FMX.Edit, FMX.ScrollBox, FMX.Memo, FMX.TabControl, FMX.Objects,
  FMX.DateTimeCtrls;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    TabControl1: TTabControl;
    tbiDemo: TTabItem;
    Memo1: TMemo;
    lytExportedToken: TLayout;
    Label5: TLabel;
    edtExportedToken: TEdit;
    gbxLnInvoiceCreate: TGroupBox;
    Label2: TLabel;
    lblAmount: TLabel;
    edtInvoiceLNAmount: TEdit;
    btnInvoiceLNCreate: TButton;
    lblGetInvoice: TLabel;
    edtGetInvoice: TEdit;
    btnCheckInvoice: TButton;
    edtInvoiceLNNote: TEdit;
    Label16: TLabel;
    gbxLnInvoicePay: TGroupBox;
    Label12: TLabel;
    Label14: TLabel;
    edtPayInvoice: TEdit;
    btnPay: TButton;
    Label17: TLabel;
    Layout3: TLayout;
    btnGetMe: TButton;
    lbxJson: TListBox;
    lytBeutyJson: TLayout;
    memBeautyJson: TMemo;
    Label6: TLabel;
    btnQrCodeFromInvoice: TButton;
    Splitter1: TSplitter;
    Label8: TLabel;
    lytQrCodePicBackground: TLayout;
    rctQrCodePicBackground: TRectangle;
    lytQrCodePic: TLayout;
    lytQrCodePicClose: TLayout;
    btnQrCodePicClose: TButton;
    imgQrCode: TImage;
    btnGetBalance: TButton;
    btnGetValue4Value: TButton;
    btnExportedTokenHowTo: TButton;
    hscrlTutorial: THorzScrollBox;
    lytTutorial: TLayout;
    rctTutorialBackground: TRectangle;
    rctTutorial1: TRectangle;
    lblTutorial1: TLabel;
    imgTutorial1: TImage;
    rctTutorial2: TRectangle;
    lblTutorial2: TLabel;
    imgTutorial2: TImage;
    rctTutorial3: TRectangle;
    lblTutorial3: TLabel;
    imgTutorial3: TImage;
    rctTutorial4: TRectangle;
    lblTutorial4: TLabel;
    imgTutorial4: TImage;
    rctTutorial5: TRectangle;
    lblTutorial5: TLabel;
    imgTutorial5: TImage;
    rctTutorial6: TRectangle;
    lblTutorial6: TLabel;
    imgTutorial6: TImage;
    rctTutorial7: TRectangle;
    lblTutorial7: TLabel;
    imgTutorial7: TImage;
    gbxStatement: TGroupBox;
    Label24: TLabel;
    Label26: TLabel;
    dedtStatementFrom: TDateEdit;
    dedtStatementTo: TDateEdit;
    Label25: TLabel;
    HorzScrollBox1: THorzScrollBox;
    btnGetInvoicesIn: TButton;
    btnGetInvoicesOut: TButton;
    lytTutorialClose: TLayout;
    btnTutorialClose: TButton;
    lytTutorialCenter: TLayout;
    gbxWebHook: TGroupBox;
    Label3: TLabel;
    btnWebHookCreate: TButton;
    btnWebHookDelete: TButton;
    Label4: TLabel;
    edtWebHookDescription: TEdit;
    edtWebHookUrl: TEdit;
    Label7: TLabel;
    edtWebHookId: TEdit;
    Label9: TLabel;
    btnWebHookGet: TButton;
    Button1: TButton;
    procedure Label1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure edtExportedTokenTyping(Sender: TObject);
    procedure btnQrCodeFromInvoiceClick(Sender: TObject);
    procedure btnGetMeClick(Sender: TObject);
    procedure lbxJsonItemClick(const Sender: TCustomListBox;
      const Item: TListBoxItem);
    procedure btnInvoiceLNCreateClick(Sender: TObject);
    procedure btnQrCodePicCloseClick(Sender: TObject);
    procedure btnPayClick(Sender: TObject);
    procedure rctTutorialBackgroundClick(Sender: TObject);
    procedure btnExportedTokenHowToClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnWebHookCreateClick(Sender: TObject);
  private
    const
    CConfigFileName = 'config.json';

    procedure SaveData;
    procedure LoadData;

    procedure AddListBox(AJson: string);
    procedure AddLog(ALog: string);

    procedure TutorialHide;
    procedure TutorialShow;

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
  System.StrUtils,

  FMX.DialogService,

  REST.JSON,

  System.Net.URLClient,
  System.Net.HttpClient,
  System.Net.HttpClientComponent,

  IdeaL.Lib.Utils,
  IdeaL.Lib.Api.Bitcoin.Alby;

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

procedure TForm1.btnExportedTokenHowToClick(Sender: TObject);
begin
  TutorialShow;
end;

procedure TForm1.btnGetMeClick(Sender: TObject);
begin
  var
  LApi := TAPIAlby.Create;
  try
    LApi.AuthToken(edtExportedToken.Text);
    var
    LResult := EmptyStr;
    case AnsiIndexText(TControl(Sender).Name, [
      btnGetMe.Name, // 0
      btnGetBalance.Name,
      btnGetValue4Value.Name,
      btnCheckInvoice.Name,
      btnGetInvoicesIn.Name,
      btnGetInvoicesOut.Name,
      btnWebHookGet.Name,
      btnWebHookDelete.Name // 6
      ])
    of
      0:
        LResult := LApi.GetMe;
      1:
        LResult := LApi.GetBalance;
      2:
        LResult := LApi.GetValue4Value;
      3:
        LResult := LApi.GetInvoice(edtGetInvoice.Text);
      4:
        LResult := LApi.GetInvoiceIn(dedtStatementFrom.Date, dedtStatementTo.Date, True);
      5:
        LResult := LApi.GetInvoiceOut(dedtStatementFrom.Date, dedtStatementTo.Date, True);
      6:
        LResult := LApi.GetWebWebHookById(edtWebHookId.Text);
      7:
        LResult := LApi.DelWebWebHookById(edtWebHookId.Text);
    end;

    AddListBox(LResult);
    AddLog(LResult);
  finally
    LApi.Free;
  end;
end;

procedure TForm1.btnInvoiceLNCreateClick(Sender: TObject);
begin
  var
  LApi := TAPIAlby.Create;
  try
    LApi.AuthToken(edtExportedToken.Text);
    var
    LAmount := StrToInt(edtInvoiceLNAmount.Text);
    var
    LResult := LApi.PostInvoice(LAmount, edtInvoiceLNNote.Text);
    AddListBox(LResult);
    AddLog(LResult);
  finally
    LApi.Free;
  end;
end;

procedure TForm1.btnPayClick(Sender: TObject);
begin
  var
  LApi := TAPIAlby.Create;
  try
    LApi.AuthToken(edtExportedToken.Text);
    var
    LResult := LApi.PostPaymentBolt11(edtPayInvoice.Text);
    AddListBox(LResult);
    AddLog(LResult);
  finally
    LApi.Free;
  end;
end;

procedure TForm1.btnQrCodeFromInvoiceClick(Sender: TObject);
var
  HttpClient: THttpClient;
  HttpResponse: IHttpResponse;
begin

  // It's not related with CoinOS, it's just extra stuff
  // You must be showing an Invoice valid JSON on the Memo
  // Tap the ListBox lbxJson, the correct Invoice result to show it in the memo
  var
  LUrl := Emptystr;
  var
  LHashInvoice := EmptyStr;
  var
  LJson := TJSONObject.ParseJSONValue(memBeautyJson.Lines.Text);
  try
    if not LJson.TryGetValue<string>('qr_code_png', LUrl) then
    begin
      if not LJson.TryGetValue<string>('payment_request', LHashInvoice) then
        raise Exception.Create('The JSON does not look like an Invoice')
      else
        LUrl := 'https://api.qrserver.com/v1/create-qr-code/?size=200x200&data=' + LHashInvoice;
    end;
  finally
    FreeAndNil(LJson);
  end;

  HttpClient := THttpClient.Create;
  try
    HttpResponse := HttpClient.Get(LUrl);

    var
    LStrm := HttpResponse.ContentStream;
    imgQrCode.Bitmap.LoadFromStream(LStrm);
    QrCodePicShow;
  finally
    HttpClient.Free;
  end;
end;

procedure TForm1.btnQrCodePicCloseClick(Sender: TObject);
begin
  if Sender = btnTutorialClose then
    TutorialHide
  else
    QrCodePicHide;
end;

procedure TForm1.btnWebHookCreateClick(Sender: TObject);
begin

  TDialogService.MessageDialog(
    'The "endpoint_secret" and "id" in the response body should be saved by the client in order to verify webhook posts to url and future management.' + sLineBreak +
    'Read more in the official documentation.' + sLineBreak +
    'Would you like to continue?',
    TMsgDlgType.mtConfirmation,
    [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo],
    TMsgDlgBtn.mbYes,0,
    procedure(const AResult: TModalResult)
    begin
      if AResult = mrYes then
      begin
        var
        LApi := TAPIAlby.Create;
        try
          LApi.AuthToken(edtExportedToken.Text);
          var
          LAmount := StrToInt(edtInvoiceLNAmount.Text);
          var
          LResult := LApi.PostWebWebHook(edtWebHookDescription.Text, edtWebHookUrl.Text, ['invoice.incoming.settled', 'invoice.outgoing.settled']);
          AddListBox(LResult);
          AddLog(LResult);
        finally
          LApi.Free;
        end;
      end;
    end
    );


end;

procedure TForm1.edtExportedTokenTyping(Sender: TObject);
begin
  SaveData;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  dedtStatementFrom.Date := IncMonth(Now, -1);
  dedtStatementTo.Date := Now;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  LoadData;
  TabControl1.ActiveTab := tbiDemo;
  QrCodePicHide;
  TutorialHide;
end;

procedure TForm1.Label1Click(Sender: TObject);
begin
  TUtils.OpenUrl(TLabel(Sender).Hint);
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

procedure TForm1.rctTutorialBackgroundClick(Sender: TObject);
begin
  TutorialHide;
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

procedure TForm1.TutorialHide;
begin
  lytTutorial.Visible := False;
end;

procedure TForm1.TutorialShow;
begin
  lytTutorial.Visible := True;
end;

end.
