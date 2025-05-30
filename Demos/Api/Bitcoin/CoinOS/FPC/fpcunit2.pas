unit FPCunit2;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls, MaskEdit;

type

  { TForm2 }

  TForm2 = class(TForm)
    btnCheckInvoice: TButton;
    btnGetMe: TButton;
    btnGetRates: TButton;
    btnPay: TButton;
    btnGetPayments: TButton;
    btnRegister: TButton;
    btnPostUser: TButton;
    btnQrCodeClose: TButton;
    btnQrCodeFromInvoice: TButton;
    btnInvoiceLNCreate: TButton;
    btnLogin: TButton;
    cbxProfileCurrency: TComboBox;
    edtMaxFee: TEdit;
    edtPaymentsOffset: TEdit;
    edtPaymentsLimit: TEdit;
    edtPayInvoice: TEdit;
    edtProfilePictureUrl: TEdit;
    edtInvoiceLNWebHook: TEdit;
    edtInvoiceLNSecret: TEdit;
    edtExportedToken: TEdit;
    edtInvoiceLNAmount: TEdit;
    edtGetInvoice: TEdit;
    edtPassword: TEdit;
    edtProfileUsername: TEdit;
    edtUsername: TEdit;
    gbxLnInvoiceCreate: TGroupBox;
    gbxLnInvoicePay: TGroupBox;
    gbxLnInvoicePay1: TGroupBox;
    gbxProfile: TGroupBox;
    gbxPublicEndpoints: TGroupBox;
    imgQrCode: TImage;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    lblProfileBalanceSatoshi: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    lblProfileBalanceFiat: TLabel;
    lbxJson: TListBox;
    dedtPaymentsFrom: TMaskEdit;
    dedtPaymentsTo: TMaskEdit;
    Memo1: TMemo;
    memBeautyJson: TMemo;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    pnlQrCode: TPanel;
    pnlQrCodePicBackground: TPanel;
    ScrollBox1: TScrollBox;
    Splitter1: TSplitter;
    tbsDemo: TTabSheet;
    tbsInfo: TTabSheet;
    procedure btnCheckInvoiceClick(Sender: TObject);
    procedure btnGetMeClick(Sender: TObject);
    procedure btnGetRatesClick(Sender: TObject);
    procedure btnInvoiceLNCreateClick(Sender: TObject);
    procedure btnLoginClick(Sender: TObject);
    procedure btnGetPaymentsClick(Sender: TObject);
    procedure btnPayClick(Sender: TObject);
    procedure btnPostUserClick(Sender: TObject);
    procedure btnQrCodeCloseClick(Sender: TObject);
    procedure btnQrCodeFromInvoiceClick(Sender: TObject);
    procedure btnRegisterClick(Sender: TObject);
    procedure edtExportedTokenChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Label20Click(Sender: TObject);
    procedure lbxJsonClick(Sender: TObject);
    procedure ScrollBox1Click(Sender: TObject);
  private
    const
    CConfigFileName = 'config.json';

    procedure SaveData;
    procedure LoadData;

    procedure AddListBox(AJson: string);
    procedure AddLog(ALog: string);

    procedure QrCodePicHide;
    procedure QrCodePicShow;
  public

  end;

var
  Form2: TForm2;

implementation

uses
  IdeaL.Lib.Api.Bitcoin.CoinOS,

  Math,
  fphttpclient,
  fpJson,
  jsonparser;

{$R *.lfm}

{ TForm2 }

procedure TForm2.FormShow(Sender: TObject);
begin
  edtUsername.OnChange := nil;
  edtPassword.OnChange := nil;
  edtExportedToken.OnChange := nil;
  try
    LoadData;
  finally
    edtUsername.OnChange := edtExportedTokenChange;
    edtPassword.OnChange := edtExportedTokenChange;
    edtExportedToken.OnChange := edtExportedTokenChange;
  end;
  PageControl1.ActivePage := tbsDemo;
  QrCodePicHide;
end;

procedure TForm2.Label20Click(Sender: TObject);
begin
  ShowMessage(
    'If you get Route related errors, try to increase the MAX FEE'
    );
end;

procedure TForm2.lbxJsonClick(Sender: TObject);
var
  LJSONData: TJSONData;
  LJson: string;
begin
  if lbxJson.ItemHeight < 0 then
    Exit;
  LJson := lbxJson.Items[lbxJson.ItemIndex];
  // Parse the JSON string into a TJSONData object
  LJSONData := GetJSON(LJson);
  try
    // Format the JSON data with indentations for easy reading
    LJson := LJSONData.FormatJSON([], 2);
  finally
    LJSONData.Free;
  end;
  memBeautyJson.Lines.Text := LJson;
end;

procedure TForm2.ScrollBox1Click(Sender: TObject);
begin

end;

procedure TForm2.edtExportedTokenChange(Sender: TObject);
begin
  SaveData;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  dedtPaymentsFrom.Text := DateToStr(IncMonth(Now, -1));
  dedtPaymentsTo.Text := DateToStr(Now);
end;

procedure TForm2.btnQrCodeCloseClick(Sender: TObject);
begin
  QrCodePicHide;
end;

procedure TForm2.btnGetMeClick(Sender: TObject);
var
  LApiCoinOS: TAPICoinOS;
  LResult: string;
  LBalance: Int64;
  LBalanceFiat: Double;
  LValueStr: string;

  LJsonData: TJSONData;
  LJsonData1: TJSONData;
  LJsonObject: TJSONObject;
begin
  LApiCoinOS := TAPICoinOS.Create;
  try
    LApiCoinOS.AuthToken(edtExportedToken.Text);
    LResult := LApiCoinOS.GetMe;
  finally
    LApiCoinOS.Free;
  end;

  edtProfileUsername.Hint := LResult;
  AddListBox(LResult);
  AddLog(LResult);

  begin
    // Reading and showing the User details

    lblProfileBalanceSatoshi.Caption := '0 sats';
    lblProfileBalanceFiat.Caption := '0';
    cbxProfileCurrency.ItemIndex := -1;

    LBalance := 0;
    LValueStr := EmptyStr;
    LJsonData :=  GetJSON(LResult);
    try
      LJsonObject := LJsonData as TJSONObject;
      if LJsonObject.Find('balance', LJsonData1) then
        LBalance := LJsonData1.AsInt64;
      if LJsonObject.Find('username', LJsonData1) then
        edtProfileUsername.Text := LJsonData1.AsString;
      if LJsonObject.Find('picture', LJsonData1) then
        edtProfilePictureUrl.Text := LJsonData1.AsString;
      if LJsonObject.Find('currency', LJsonData1) then
        LValueStr := LJsonData1.AsString;
      cbxProfileCurrency.ItemIndex := cbxProfileCurrency.Items.IndexOf(LValueStr);
      if (cbxProfileCurrency.ItemIndex = -1) and (not LValueStr.Trim.IsEmpty) then
      begin // Add currency in case the selected Currency is not in the Combobox
        cbxProfileCurrency.Items.Add(LValueStr);
        cbxProfileCurrency.ItemIndex := Pred(cbxProfileCurrency.Items.Count);
      end;
      if (cbxProfileCurrency.ItemIndex <> -1) then
      begin
        lblProfileBalanceFiat.Caption := '0 ' + cbxProfileCurrency.Text;
        // only convert to fiat if balance>0
        if LBalance > 0 then
        begin // retrive the CoinOS Bitcoin prices to show the fiat amount
          lblProfileBalanceSatoshi.Caption := LBalance.ToString + ' sats';
          LApiCoinOS := TAPICoinOS.Create;
          try
            LResult := LApiCoinOS.GetRates;
          finally
            LApiCoinOS.Free;
          end;

          LJsonData.Free;
          LJsonData :=  GetJSON(LResult);
          LJsonObject := LJsonData as TJSONObject;
          if LJsonObject.Find(cbxProfileCurrency.Text, LJsonData1) then
          begin // only if the selected currency was found in the JSON
            LBalanceFiat := 0;
            LBalanceFiat :=
              (LBalance / 100000000) // 1BTC = 100.000.000 satoshis
              * LJsonData1.AsFloat;
            LBalanceFiat := Math.RoundTo(LBalanceFiat, -3);
            lblProfileBalanceFiat.Caption := LBalanceFiat.ToString + ' ' + cbxProfileCurrency.Text;
          end;
        end;
      end;
    finally
      LJsonData.Free;
    end;
  end;
end;

procedure TForm2.btnGetRatesClick(Sender: TObject);
var
  LApiCoinOS: TAPICoinOS;
  LResult: string;
begin
  LApiCoinOS := TAPICoinOS.Create;
  try
    LResult := LApiCoinOS.GetRates;
    AddListBox(LResult);
    AddLog(LResult);
  finally
    LApiCoinOS.Free;
  end;
end;

procedure TForm2.btnCheckInvoiceClick(Sender: TObject);
var
  LApiCoinOS: TAPICoinOS;
  LResult: string;
begin
  ShowMessage('Check method only works with CoinOS invoices');

  LApiCoinOS := TAPICoinOS.Create;
  try
    LApiCoinOS.AuthToken(edtExportedToken.Text);
    LResult := LApiCoinOS.GetInvoice(edtGetInvoice.Text);
    AddListBox(LResult);
    AddLog(LResult);
  finally
    LApiCoinOS.Free;
  end;
end;

procedure TForm2.btnInvoiceLNCreateClick(Sender: TObject);
var
  LApiCoinOS: TAPICoinOS;
  LAmount: Integer;
  LResult: string;
begin
  LApiCoinOS := TAPICoinOS.Create;
  try
    LApiCoinOS.AuthToken(edtExportedToken.Text);
    LAmount := StrToInt(edtInvoiceLNAmount.Text);
    LResult := LApiCoinOS.PostInvoiceLightning(LAmount, edtInvoiceLNWebHook.Text, edtInvoiceLNSecret.Text);
    AddListBox(LResult);
    AddLog(LResult);
  finally
    LApiCoinOS.Free;
  end;
end;

procedure TForm2.btnLoginClick(Sender: TObject);
var
  LApiCoinOS: TAPICoinOS;
  LResult: string;

  LJsonData: TJSONData;
  LJsonData1: TJSONData;
  LJsonObject: TJSONObject;
begin
  LApiCoinOS := TAPICoinOS.Create;
  try
    LResult := LApiCoinOS.PostLogin(edtUsername.Text, edtPassword.Text);
    AddListBox(LResult);
    AddLog(LResult);
  finally
    LApiCoinOS.Free;
  end;
  // To reset local logged user
  edtProfileUsername.Hint := EmptyStr; // Will be used on "Get me"

  LJsonData :=  GetJSON(LResult);
  try
    LJsonObject := LJsonData as TJSONObject;
    if LJsonObject.Find('token', LJsonData1) then
      edtExportedToken.Text := LJsonData1.AsString;
  finally
    LJsonData.Free;
  end;
end;

procedure TForm2.btnGetPaymentsClick(Sender: TObject);
var
  LApiCoinOS: TAPICoinOS;
  LResult: string;
begin
  LApiCoinOS := TAPICoinOS.Create;
  try
    LApiCoinOS.AuthToken(edtExportedToken.Text);
    LResult := LApiCoinOS.GetPayments(StrToDate(dedtPaymentsFrom.Text), StrToDate(dedtPaymentsTo.Text), StrToInt(edtPaymentsLimit.Text), StrToInt(edtPaymentsOffset.Text));
    AddListBox(LResult);
    AddLog(LResult);
  finally
    LApiCoinOS.Free;
  end;
end;

procedure TForm2.btnPayClick(Sender: TObject);
var
  LApiCoinOS: TAPICoinOS;
  LResult: string;
begin
  LApiCoinOS := TAPICoinOS.Create;
  try
    LApiCoinOS.AuthToken(edtExportedToken.Text);
    LResult := LApiCoinOS.PostPayment(edtPayInvoice.Text, StrToInt(edtMaxFee.Text));
    AddListBox(LResult);
    AddLog(LResult);
  finally
    LApiCoinOS.Free;
  end;
end;

procedure TForm2.btnPostUserClick(Sender: TObject);
var
  LJsonData: TJSONData;
  LJsonDataNew: TJSONData;
  LJsonObject: TJSONObject;
  LJson: string;
  LApiCoinOS: TAPICoinOS;
  LResult: string;

  procedure AddValue(AKey, AValue: string);
  var
    LJsonData1: TJSONData;
    LValue: string;
  begin
    // check if the value is changed, if so, add to the JSONObject
    LValue := EmptyStr;
    if (Assigned(LJsonObject)) and (LJsonObject.Find(AKey, LJsonData1)) then
      LValue := LJsonData1.AsString;

    if LValue <> AValue then
    begin
      if not Assigned(LJsonDataNew) then
        LJsonDataNew := GetJSON('{}');
      TJSONObject(LJsonDataNew).Add(AKey, AValue);
    end;
  end;

begin
  // Check changes
  LJson := EmptyStr;

  LJsonDataNew := nil;
  LJsonData :=  GetJSON(edtProfileUsername.Hint);
  try
    LJsonObject := LJsonData as TJSONObject;
    AddValue('username', edtProfileUsername.Text);
    AddValue('picture', edtProfilePictureUrl.Text);
    if cbxProfileCurrency.ItemIndex > -1 then
      AddValue('currency', cbxProfileCurrency.Text);
    if Assigned(LJsonDataNew) then
      LJson := LJsonDataNew.AsJSON;
  finally
    LJsonDataNew.Free;
    LJsonData.Free;
  end;

  // Apply changes
  if not LJson.IsEmpty then
  begin
    LApiCoinOS := TAPICoinOS.Create;
    try
      LApiCoinOS.AuthToken(edtExportedToken.Text);
      LResult := LApiCoinOS.PostUser(LJson);
      AddListBox(LResult);
      AddLog(LResult);
    finally
      LApiCoinOS.Free;
    end;
  end;
end;

procedure TForm2.btnQrCodeFromInvoiceClick(Sender: TObject);
var
  LHttpClient: TFPHttpClient;
  LHashInvoice: string;
  LUrl: string;

  LJsonData: TJSONData;
  LJsonData1: TJSONData;
  LJsonObject: TJSONObject;

  LMemStr: TMemoryStream;
begin
  // It's not related with CoinOS, it's just extra stuff
  // You must be showing an Invoice valid JSON on the Memo
  // Tap the ListBox lbxJson, the correct Invoice result to show it in the memo
  LHashInvoice := EmptyStr;
  LJsonData :=  GetJSON(memBeautyJson.Lines.Text);
  try
    LJsonObject := LJsonData as TJSONObject;
    if not LJsonObject.Find('hash', LJsonData1) then
      raise Exception.Create('The JSON does not look like an Invoice');
    LHashInvoice := LJsonData1.AsString;
  finally
    FreeAndNil(LJsonData);
  end;

  LUrl := 'https://api.qrserver.com/v1/create-qr-code/?size=200x200&data=' + LHashInvoice;
  TAPICoinOS.GetHttpRequest(LHttpClient);
  try
    LMemStr := TMemoryStream.Create;
    try
      LMemStr.Position := 0;
      LHttpClient.Get(LUrl, LMemStr);
      LMemStr.Position := 0;
      imgQrCode.Picture.LoadFromStream(LMemStr);
    finally
      LMemStr.Free;
    end;
    QrCodePicShow;
  finally
    LHttpClient.Free;
  end;
end;

procedure TForm2.btnRegisterClick(Sender: TObject);
var
  LApiCoinOS: TAPICoinOS;
  LResult: string;
begin
  LApiCoinOS := TAPICoinOS.Create;
  try
    LResult := LApiCoinOS.PostRegister(edtUsername.Text, edtPassword.Text);
    AddListBox(LResult);
    AddLog(LResult);
  finally
    LApiCoinOS.Free;
  end;
end;

procedure TForm2.FormPaint(Sender: TObject);
begin
  if pnlQrCodePicBackground.Visible then
  begin
    pnlQrCodePicBackground.Height := Self.Height;
    pnlQrCodePicBackground.Width := Self.Width;
    pnlQrCodePicBackground.Left := 0;
    pnlQrCodePicBackground.Top := 0;
  end;
end;

procedure TForm2.SaveData;
var
  LJsonData: TJSONData;
  LJsonObject: TJSONObject;
  LStrLst: TStringList;
begin
  LJsonData :=  GetJSON('{}');
  LStrLst := TStringList.Create;
  try
    LJsonObject := LJsonData as TJSONObject;
    LJsonObject.Add('username', edtUsername.Text);
    LJsonObject.Add('password', edtPassword.Text);
    LJsonObject.Add('exportedToken', edtExportedToken.Text);
    LStrLst.Text := LJsonObject.AsJSON;
    LStrLst.SaveToFile(CConfigFileName);
  finally
    LStrLst.Free;
    LJsonData.Free;
  end;
end;

procedure TForm2.LoadData;
var
  LJsonData: TJSONData;
  LJsonData1: TJSONData;
  LJsonObject: TJSONObject;
  LStrLst: TStringList;
begin
  if not FileExists(CConfigFileName) then
     Exit;

  LStrLst := TStringList.Create;
  try
    LStrLst.LoadFromFile(CConfigFileName);
    LJsonData :=  GetJSON(LStrLst.Text);
    LJsonObject := LJsonData as TJSONObject;
    if LJsonObject.Find('username', LJsonData1) then
      edtUsername.Text := LJsonData1.AsString;
    if LJsonObject.Find('password', LJsonData1) then
      edtPassword.Text := LJsonData1.AsString;
    if LJsonObject.Find('exportedToken', LJsonData1) then
      edtExportedToken.Text := LJsonData1.AsString;
  finally
    LStrLst.Free;
    LJsonData.Free;
  end;
  {var
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
  end;}
end;

procedure TForm2.AddListBox(AJson: string);
begin
  lbxJson.Items.Insert(0, AJson);
end;

procedure TForm2.AddLog(ALog: string);
begin
  ALog := DateTimeToStr(Now) + ' - ' + ALog;
  Memo1.Lines.Insert(0, ALog);
end;

procedure TForm2.QrCodePicHide;
begin
  pnlQrCodePicBackground.Visible := False;
end;

procedure TForm2.QrCodePicShow;
begin
  pnlQrCodePicBackground.Visible := True;
  pnlQrCodePicBackground.BringToFront;
end;

end.

