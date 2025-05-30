unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Edit, FMX.Layouts,
  FMX.Memo.Types, FMX.ScrollBox, FMX.Memo, FMX.TabControl, FMX.ListBox,
  FMX.Objects, FMX.DateTimeCtrls;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    gbxLnInvoiceCreate: TGroupBox;
    Label2: TLabel;
    lblAmount: TLabel;
    edtInvoiceLNAmount: TEdit;
    btnInvoiceLNCreate: TButton;
    lytExportedToken: TLayout;
    Label5: TLabel;
    edtExportedToken: TEdit;
    Memo1: TMemo;
    lblGetInvoice: TLabel;
    edtGetInvoice: TEdit;
    btnCheckInvoice: TButton;
    TabControl1: TTabControl;
    tbiDemo: TTabItem;
    tbiInfo: TTabItem;
    Label3: TLabel;
    Label4: TLabel;
    Layout3: TLayout;
    lbxJson: TListBox;
    memBeautyJson: TMemo;
    lytBeutyJson: TLayout;
    Label6: TLabel;
    Splitter1: TSplitter;
    VertScrollBox1: TVertScrollBox;
    Label7: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    gbxLnInvoicePay: TGroupBox;
    Label12: TLabel;
    Label14: TLabel;
    edtPayInvoice: TEdit;
    Label15: TLabel;
    edtMaxFee: TEdit;
    btnPay: TButton;
    edtInvoiceLNWebHook: TEdit;
    edtInvoiceLNSecret: TEdit;
    Label9: TLabel;
    Label13: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    btnQrCodeFromInvoice: TButton;
    lytQrCodePic: TLayout;
    Layout2: TLayout;
    btnQrCodePicClose: TButton;
    rctQrCodePicBackground: TRectangle;
    lytQrCodePicBackground: TLayout;
    imgQrCode: TImage;
    btnRegister: TButton;
    Label18: TLabel;
    Label19: TLabel;
    edtPassword: TEdit;
    edtUsername: TEdit;
    btnLogin: TButton;
    lytAccount: TLayout;
    GroupBox1: TGroupBox;
    btnGetRates: TButton;
    gbxProfile: TGroupBox;
    btnGetMe: TButton;
    Label20: TLabel;
    edtProfileUsername: TEdit;
    Label21: TLabel;
    lblProfileBalanceSatoshi: TLabel;
    Label22: TLabel;
    cbxProfileCurrency: TComboBox;
    Label23: TLabel;
    edtProfilePictureUrl: TEdit;
    lblProfileBalanceFiat: TLabel;
    btnPostUser: TButton;
    btnPrivateDataShowHide: TButton;
    HorzScrollBox1: THorzScrollBox;
    gbxGeneral: TGroupBox;
    Label24: TLabel;
    Label26: TLabel;
    dedtPaymentsFrom: TDateEdit;
    dedtPaymentsTo: TDateEdit;
    Label25: TLabel;
    Label27: TLabel;
    edtPaymentsLimit: TEdit;
    edtPaymentsOffset: TEdit;
    Label28: TLabel;
    btnGetPayments: TButton;
    Label8: TLabel;
    procedure Label1Click(Sender: TObject);
    procedure btnInvoiceLNCreateClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure edtExportedTokenTyping(Sender: TObject);
    procedure btnCheckInvoiceClick(Sender: TObject);
    procedure lblAmountClick(Sender: TObject);
    procedure lblGetInvoiceClick(Sender: TObject);
    procedure btnPayClick(Sender: TObject);
    procedure Label4Click(Sender: TObject);
    procedure btnGetMeClick(Sender: TObject);
    procedure lbxJsonItemClick(const Sender: TCustomListBox;
      const Item: TListBoxItem);
    procedure Label17Click(Sender: TObject);
    procedure btnQrCodeFromInvoiceClick(Sender: TObject);
    procedure rctQrCodePicBackgroundClick(Sender: TObject);
    procedure btnGetRatesClick(Sender: TObject);
    procedure btnRegisterClick(Sender: TObject);
    procedure btnLoginClick(Sender: TObject);
    procedure btnPostUserClick(Sender: TObject);
    procedure btnPrivateDataShowHideClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnGetPaymentsClick(Sender: TObject);
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
  System.Math,
  System.DateUtils,
  
  REST.JSON,

  System.Net.URLClient,
  System.Net.HttpClient,
  System.Net.HttpClientComponent,

  IdeaL.Lib.Utils,
  IdeaL.Lib.Api.Bitcoin.CoinOS;

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

procedure TForm1.btnCheckInvoiceClick(Sender: TObject);
begin
  ShowMessage('Check method only works with CoinOS invoices');

  var
  LApiCoinOS := TAPICoinOS.Create;
  try
    LApiCoinOS.AuthToken(edtExportedToken.Text);
    var
    LResult := LApiCoinOS.GetInvoice(edtGetInvoice.Text);
    AddListBox(LResult);
    AddLog(LResult);
  finally
    LApiCoinOS.Free;
  end;
end;

procedure TForm1.btnGetMeClick(Sender: TObject);
begin
  var
  LResult := EmptyStr;
  var
  LApiCoinOS := TAPICoinOS.Create;
  try
    LApiCoinOS.AuthToken(edtExportedToken.Text);
    LResult := LApiCoinOS.GetMe;    
  finally
    LApiCoinOS.Free;
  end;
  edtProfileUsername.TagString := LResult;

  AddListBox(LResult);
  AddLog(LResult);

  begin
    // Reading and showing the User details

    lblProfileBalanceSatoshi.Text := '0 sats';
    lblProfileBalanceFiat.Text := '0';

    var
    LJson := TJSONObject.ParseJSONValue(LResult);
    var
    LBalance : Int64 := 0;
    var
    LValueStr := EmptyStr;
    try
      LJson.TryGetValue<Int64>('balance', LBalance);
      LJson.TryGetValue<string>('username', LValueStr);
      edtProfileUsername.Text := LValueStr;
      LJson.TryGetValue<string>('picture', LValueStr);
      edtProfilePictureUrl.Text := LValueStr;
      LJson.TryGetValue<string>('currency', LValueStr);
      cbxProfileCurrency.ItemIndex := cbxProfileCurrency.Items.IndexOf(LValueStr);    
      if cbxProfileCurrency.ItemIndex = -1 then
      begin // Add currency in case the selected Currency is not in the Combobox
        cbxProfileCurrency.Items.Add(LValueStr);
        cbxProfileCurrency.ItemIndex := Pred(cbxProfileCurrency.Items.Count);
      end;
      if Assigned(cbxProfileCurrency.Selected) then
      begin
        lblProfileBalanceFiat.Text := '0 ' + cbxProfileCurrency.Selected.Text;

        // only convert to fiat if balance>0
        if LBalance > 0 then
        begin // retrive the CoinOS Bitcoin prices to show the fiat amount
          lblProfileBalanceSatoshi.Text := LBalance.ToString + ' sats';
          LApiCoinOS := TAPICoinOS.Create;
          try
            LResult := LApiCoinOS.GetRates;    
          finally
            LApiCoinOS.Free;
          end;

          var
          LJsonRates := TJSONObject.ParseJSONValue(LResult);
          var
          LPrice : Double := 0;
          try
            if LJsonRates.TryGetValue<Double>(cbxProfileCurrency.Selected.Text, LPrice) then
            begin // only if the selected currency was found in the JSON
              var
              LBalanceFiat : Double := 0;
              LBalanceFiat := 
                (LBalance / 100000000) // 1BTC = 100.000.000 satoshis
                * LPrice;
              LBalanceFiat := System.Math.RoundTo(LBalanceFiat, -3);
              lblProfileBalanceFiat.Text := LBalanceFiat.ToString + ' ' + cbxProfileCurrency.Selected.Text;
            end;
          finally
            LJsonRates.Free;
          end;
        end;
      end;
    finally
      FreeAndNil(LJson);
    end;
  end;
end;

procedure TForm1.btnGetPaymentsClick(Sender: TObject);
begin
  var
  LInt := 0;
  if not TryStrToInt(edtPaymentsLimit.Text, LInt) then
    edtPaymentsLimit.Text := '0';
  if not TryStrToInt(edtPaymentsOffset.Text, LInt) then
    edtPaymentsOffset.Text := '0';
  
  var
  LApiCoinOS := TAPICoinOS.Create;
  try
    LApiCoinOS.AuthToken(edtExportedToken.Text);
    var
    LResult := LApiCoinOS.GetPayments(dedtPaymentsFrom.Date, dedtPaymentsTo.Date, edtPaymentsLimit.Text.ToInteger, edtPaymentsOffset.Text.ToInteger);
    AddListBox(LResult);
    AddLog(LResult);
  finally
    LApiCoinOS.Free;
  end;
end;

procedure TForm1.btnGetRatesClick(Sender: TObject);
begin
  var
  LApiCoinOS := TAPICoinOS.Create;
  try
    var
    LResult := LApiCoinOS.GetRates;
    AddListBox(LResult);
    AddLog(LResult);
  finally
    LApiCoinOS.Free;
  end;
end;

procedure TForm1.btnPayClick(Sender: TObject);
begin
  var
  LApiCoinOS := TAPICoinOS.Create;
  try
    LApiCoinOS.AuthToken(edtExportedToken.Text);
    var
    LResult := LApiCoinOS.PostPayment(edtPayInvoice.Text, StrToInt(edtMaxFee.Text));
    AddListBox(LResult);
    AddLog(LResult);
  finally
    LApiCoinOS.Free;
  end;
end;

procedure TForm1.btnPostUserClick(Sender: TObject);
begin
  // Check changes
  var
  LJsonLogged := TJSONObject.ParseJSONValue(edtProfileUsername.TagString);
  var
  LJsonPost := TJSONObject.Create;
  var
  LJson := EmptyStr;
  try
    var
    LVarStr := EmptyStr;
    LJsonLogged.TryGetValue<string>('username', LVarStr);
    if (not LVarStr.Equals(edtProfileUsername.Text)) then
      LJsonPost.AddPair('username', edtProfileUsername.Text);
    LJsonLogged.TryGetValue<string>('picture', LVarStr);
    if (not LVarStr.Equals(edtProfilePictureUrl.Text)) then
      LJsonPost.AddPair('picture', edtProfilePictureUrl.Text);
    LJsonLogged.TryGetValue<string>('currency', LVarStr);
    if (Assigned(cbxProfileCurrency.Selected)) and (not LVarStr.Equals(cbxProfileCurrency.Selected.Text)) then
      LJsonPost.AddPair('currency', cbxProfileCurrency.Selected.Text);
    if LJsonPost.Count > 0 then
      LJson := LJsonPost.ToJSON;
  finally
    LJsonPost.Free;
    LJsonLogged.Free;
  end;

  // Apply changes
  if not LJson.IsEmpty then
  begin
    var
    LApiCoinOS := TAPICoinOS.Create;
    try
      LApiCoinOS.AuthToken(edtExportedToken.Text);
      var
      LResult := LApiCoinOS.PostUser(LJson);
      AddListBox(LResult);
      AddLog(LResult);
    finally
      LApiCoinOS.Free;
    end;
  end;
end;

procedure TForm1.btnPrivateDataShowHideClick(Sender: TObject);
begin
  edtPassword.Password := not edtPassword.Password;
  edtExportedToken.Password := not edtExportedToken.Password;
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
  LHashInvoice := EmptyStr;
  var
  LJson := TJSONObject.ParseJSONValue(memBeautyJson.Lines.Text);
  try
    if not LJson.TryGetValue<string>('hash', LHashInvoice) then
      raise Exception.Create('The JSON does not look like an Invoice');
  finally
    FreeAndNil(LJson);
  end;

  var
  LUrl := 'https://api.qrserver.com/v1/create-qr-code/?size=200x200&data=' + LHashInvoice;
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

procedure TForm1.btnRegisterClick(Sender: TObject);
begin
  var
  LApiCoinOS := TAPICoinOS.Create;
  try    
    var
    LResult := LApiCoinOS.PostRegister(edtUsername.Text, edtPassword.Text);
    AddListBox(LResult);
    AddLog(LResult);
  finally
    LApiCoinOS.Free;
  end;  
end;

procedure TForm1.edtExportedTokenTyping(Sender: TObject);
begin
  SaveData;
end;

procedure TForm1.btnInvoiceLNCreateClick(Sender: TObject);
begin
  var
  LApiCoinOS := TAPICoinOS.Create;
  try
    LApiCoinOS.AuthToken(edtExportedToken.Text);
    var
    LAmount := StrToInt(edtInvoiceLNAmount.Text);
    var
    LResult := LApiCoinOS.PostInvoiceLightning(LAmount, edtInvoiceLNWebHook.Text, edtInvoiceLNSecret.Text);
    AddListBox(LResult);
    AddLog(LResult);
  finally
    LApiCoinOS.Free;
  end;
end;

procedure TForm1.btnLoginClick(Sender: TObject);
begin
  var
  LApiCoinOS := TAPICoinOS.Create;
  try    
    var
    LResult := LApiCoinOS.PostLogin(edtUsername.Text, edtPassword.Text);
    var
    LJSONObj := TJSONObject.ParseJSONValue(LResult) as TJSONObject;
    // edtExportedToken.OnTyping := False;
    try
      var
      LStr := EmptyStr;
      if LJSONObj.TryGetValue<string>('token', LStr) then
        edtExportedToken.Text := LStr;
    finally
      LJSONObj.Free;
    end;
    // To reset local logged user
    edtProfileUsername.TagString := EmptyStr;

    AddListBox(LResult);
    AddLog(LResult);
  finally
    LApiCoinOS.Free;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  dedtPaymentsFrom.Date := IncMonth(Now, -1);
  dedtPaymentsTo.Date := Now;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  LoadData;
  TabControl1.ActiveTab := tbiDemo;
  QrCodePicHide;
end;

procedure TForm1.Label17Click(Sender: TObject);
begin
  ShowMessage(
    'If you get Route related errors, try to increase the MAX FEE'
    );
end;

procedure TForm1.Label1Click(Sender: TObject);
begin
  TUtils.OpenUrl('https://coinos.io/docs');
end;

procedure TForm1.Label4Click(Sender: TObject);
begin
  TUtils.OpenUrl(TLabel(Sender).Hint);
end;

procedure TForm1.lblAmountClick(Sender: TObject);
begin
  ShowMessage('Use this to create your LN Invoice to be payed');
end;

procedure TForm1.lblGetInvoiceClick(Sender: TObject);
begin
  ShowMessage(
    'Use this check data from an invoice' + sLineBreak +
    'With this you can check if the Invoice was payed or not'
    );
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
    if LJSONObj.TryGetValue<string>('username', LStr) then
      edtUsername.Text := LStr;
    if LJSONObj.TryGetValue<string>('password', LStr) then
      edtPassword.Text := LStr;
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
    LJSONObj.AddPair('username', edtUsername.Text);
    LJSONObj.AddPair('password', edtPassword.Text);
    LJSONObj.AddPair('exportedToken', edtExportedToken.Text);
    TFile.WriteAllText(CConfigFileName, LJSONObj.ToString);
  finally
    LJSONObj.Free;
  end;
end;

end.
