unit IdeaL.Lib.Api.Bitcoin.CoinOS;

interface

uses
  IdeaL.Lib.Api,
{$IFDEF FPC}
  Classes,
  SysUtils,
  fpJson,

  fphttpclient
{$ELSE}
  System.Classes,
  System.SysUtils,
  System.IOUtils,
  System.JSON,

  System.Net.URLClient,
  System.Net.HttpClient,
  System.Net.HttpClientComponent,
  System.Net.Mime
{$ENDIF}
  ;

type
  /// <remarks>
  /// Check the official documentation
  /// <see cref="https://coinos.io/docs"/>
  /// Full list of EndPoints
  /// <see cref="https://github.com/coinos/coinos-server/blob/master/index.ts"/>
  /// </remarks>

  { TAPICoinOS }

  TAPICoinOS = class(TIdeaLApi)
  private
    FAuthToken: string;
    { private declarations }
  protected
{$IFDEF FPC}
    procedure GetNetHttpRequestLocal(
      out AHttpClient: TFPHttpClient;
      AOnData: TDataEvent = nil);
{$ELSE}
    procedure GetNetHttpRequestLocal(
      out ANetHttpClient: TNetHTTPClient;
      out ANetHttpRequest: TNetHTTPRequest;
      AOnSendDataEvent: TSendDataEvent = nil;
      AOnReceiveData: TReceiveDataEvent = nil);
{$ENDIF}

    function Post(AUrl, ABody: string): string;
    function Get(AUrl: string): string;
    { protected declarations }
  public
    const
    CUrlApi = 'https://coinos.io/api';
    { public declarations }
  public

    function AuthToken(AValue: string): TAPICoinOS; overload;
    function AuthToken: string; overload;

    function GetMe: string;
    function GetCredits: string;
    /// <summary> Get CoinOS BTC prices on several FIAT currencies
    /// </summary>
    /// <remarks> This is a public method, no need Authentication
    /// </remarks>
    function GetRates: string;

    function PostRegister(AUsername, APassword: string): string;
    function PostLogin(AUsername, APassword: string): string;
    function PostUser(AJson: string): string; 

    /// <summary> Register/Create a Lightning or Bitcoin invoice
    /// </summary>
    /// <param name="AType">lightning or bitcoin
    /// </param>
    /// <param name="AAmount">Price in Satothis
    /// </param>
    /// </param>
    /// <param name="AWebHook">(optional) endpoint to hit when the invoice is paid
    /// </param>
    /// </param>
    /// <param name="ASecret">(optional, mandatory with WebHook) endpoint to hit when the invoice is paid
    /// </param>
    /// <param name="AExpiration">(optional) Expiration time in second
    /// </param>
    /// <remarks> Amount must be in SATOSHIS, integer value
    /// </remarks>
    function PostInvoice(AType: string; AAmount: Integer; AWebHook: string = ''; ASecret: string = ''; AExpiration: Integer = 0): string;
    function PostInvoiceBitcoin(AAmount: Integer; AWebHook: string = ''; ASecret: string = ''; AExpiration: Integer = 0): string;
    function PostInvoiceLightning(AAmount: Integer; AWebHook: string = ''; ASecret: string = ''; AExpiration: Integer = 0): string;

    /// <summary> Fetch an invoice by passing a bitcoin address or lightning payment hash
    /// </summary>
    /// <param name="AValue">Lightning invoice hash OR bitcoin address
    /// </param>
    function GetInvoice(AValue: string): string;
    /// <summary> Get all payments sent or received by the current user
    /// </summary>
    /// <param name="AStart"> only payments after this unix time
    /// </param>
    /// <param name="AEnd"> only payments before this unix time
    /// </param>
    /// <param name="ALimit"> limit to this integer number of results
    /// </param>
    /// <param name="AOffset"> start limiting from this integer offset
    /// </param>
    function GetPayments(
      AStart: TDateTime = 0;
      AEnd: TDateTime = 0;
      ALimit: Integer = 0;
      AOffset: Integer = 0
      ): string;

    /// <summary> Send a lightning payment
    /// </summary>
    function PostPayment(AInvoice: string; AMaxFee: Integer = 0): string;
    /// <summary> Send an internal payment to another user
    /// </summary>
    function PostPaymentToUser(AAmount: Integer; AUserName: string): string;
    /// <summary> Send a bitcoin payment
    /// </summary>
    function PostBitcoinSend(AAmount: Integer; AAddress: string): string;

    /// <summary> Request Token to access ONLY "Create Invoice" and "Payment History"
    /// </summary>
    function GetRo(): string;
    { public declarations }
  end;

implementation

uses
{$IFDEF FPC}
  fpweb,
  openssl, { This implements the procedure InitSSLInterface }
  opensslsockets,
  DateUtils;
{$ELSE}
  System.DateUtils;  
{$ENDIF}

{ TAPICoinOS }

function TAPICoinOS.AuthToken: string;
begin
  Result := FAuthToken;
end;

function TAPICoinOS.GetRates: string;
var
  LUrl: string;
begin
  LUrl := Format('%s/rates', [CUrlApi]);
  Result := Get(LUrl);
end;

function TAPICoinOS.GetRo: string;
var
  LUrl: string;
  LInt64: Int64;
{$IFDEF FPC}
  LHttpClient: TFPHttpClient;
  LResponse : TStringList;
{$ELSE}
  LHttp: THTTPClient;
  LResponse: IHTTPResponse;
{$ENDIF}
begin
  LUrl := Format('%s/%s', [CUrlApi, 'ro']);
{$IFDEF FPC}
  InitSSLInterface;
  GetNetHttpRequestLocal(LHttpClient);
  try
    LResponse := TStringList.Create;
    try
      LHttpClient.ConnectTimeout := 20000;
      LHttpClient.Get(LUrl, LResponse);
      if LHttpClient.ResponseStatusCode <> 200 then
        raise exception.create(LHttpClient.ResponseStatusText);
      Result := LResponse.Text;
    finally
      LHttpClient.RequestBody.Free;
      LResponse.Free;
    end;
  finally
    LHttpClient.Free;
  end;
{$ELSE}
  var
  LHeader: TNetHeaders;
  LHeader :=
    [
      TNameValuePair.Create('Authorization', 'Bearer ' + FAuthToken),
      // TNameValuePair.Create('Content-Type', 'application/json; charset=utf-8'),
      TNameValuePair.Create('Accept', '*/*')
    ];

  LHttp := THTTPClient.Create;
  try
    LResponse := LHttp.Get(LUrl, nil, LHeader);
  finally
    FreeAndNil(LHttp);
  end;

  if LResponse.StatusCode <> 200 then
    raise Exception.Create(LResponse.StatusCode.ToString + ' ' + LResponse.StatusText);
  Result := LResponse.ContentAsString(TEncoding.UTF8);
{$ENDIF}
end;

function TAPICoinOS.Get(AUrl: string): string;
{$IFDEF FPC}
var
  LHttpClient: TFPHttpClient;
  LResponse : TStringList;
{$ELSE}
var
  LHttp: THTTPClient;
  LResponse: IHTTPResponse;
{$ENDIF}
begin
{$IFDEF FPC}
  InitSSLInterface;
  GetNetHttpRequestLocal(LHttpClient);
  try
    LResponse := TStringList.Create;
    try
      LHttpClient.Get(AUrl, LResponse);
      if LHttpClient.ResponseStatusCode <> 200 then
        raise exception.create(LHttpClient.ResponseStatusText);
      Result := LResponse.Text;
    finally
      LResponse.Free;
    end;
  finally
    LHttpClient.Free;
  end;
{$ELSE}
  var
  LHeader: TNetHeaders;
  LHeader :=
    [
      TNameValuePair.Create('Authorization', 'Bearer ' + FAuthToken),
      TNameValuePair.Create('Content-Type', 'application/json; charset=utf-8'),
      TNameValuePair.Create('Accept', '*/*')
    ];

  LHttp := THTTPClient.Create;
  try
    LResponse := LHttp.Get(AUrl, nil, LHeader);
  finally
    FreeAndNil(LHttp);
  end;

  if LResponse.StatusCode <> 200 then
    raise Exception.Create(LResponse.StatusCode.ToString + ' ' + LResponse.StatusText);
  Result := LResponse.ContentAsString(TEncoding.UTF8);
{$ENDIF}
end;

function TAPICoinOS.GetCredits: string;
begin
  var
  LUrl := Format('%s/%s', [CUrlApi, 'credits']);
  Result := Get(LUrl);
end;

function TAPICoinOS.GetInvoice(AValue: string): string;
var
  LUrl: string;
begin
  LUrl := Format('%s/%s/%s', [CUrlApi, 'invoice', AValue]);
  Result := Get(LUrl);
end;

function TAPICoinOS.GetMe: string;
var
  LUrl: string;
begin
  LUrl := Format('%s/me', [CUrlApi]);
  Result := Get(LUrl);
end;

{$IFDEF FPC}
procedure TAPICoinOS.GetNetHttpRequestLocal(
  out AHttpClient: TFPHttpClient;
  AOnData: TDataEvent = nil);
begin
  GetHttpRequest(AHttpClient, AOnData);
  AHttpClient.AddHeader('Content-Type', 'application/json');
  AHttpClient.AddHeader('Authorization', 'Bearer ' + FAuthToken);
end;
{$ELSE}
procedure TAPICoinOS.GetNetHttpRequestLocal(out ANetHttpClient: TNetHTTPClient;
  out ANetHttpRequest: TNetHTTPRequest; AOnSendDataEvent: TSendDataEvent;
  AOnReceiveData: TReceiveDataEvent);
begin
  GetNetHttpRequest(ANetHttpClient, ANetHttpRequest, AOnSendDataEvent, AOnReceiveData);
  ANetHttpClient.ContentType := 'application/json; charset=utf-8';
  ANetHttpClient.AcceptEncoding :='';

  ANetHttpClient.CustomHeaders['Content-Type'] := ANetHttpClient.ContentType;
  ANetHttpClient.CustomHeaders['Accept'] := ANetHttpClient.Accept;
  ANetHttpClient.CustomHeaders['Authorization'] := 'Bearer ' + FAuthToken;
end;
{$ENDIF}

function TAPICoinOS.GetPayments(AStart: TDateTime; AEnd: TDateTime;
  ALimit: Integer; AOffset: Integer): string;
var
  LUrl: string;
  LParams: string;
  LInt64: Int64;
{$IFDEF FPC}
  LHttpClient: TFPHttpClient;
  LResponse : TStringList;
{$ELSE}
  LHttp: THTTPClient;
  LResponse: IHTTPResponse;
{$ENDIF}
begin
  LParams := EmptyStr;
  if AStart > 0 then
  begin
    LInt64 := DateTimeToUnix(AStart) * 1000;
    LParams := LParams + '&start=' + LInt64.ToString;
  end;
  if AEnd > 0 then
  begin
    LInt64 := DateTimeToUnix(AEnd) * 1000;
    LParams := LParams + '&end=' + LInt64.ToString;
  end;
  if ALimit > 0 then
    LParams := LParams + '&limit=' + ALimit.ToString;
  if AOffset > 0 then
    LParams := LParams + '&offset=' + AOffset.ToString;
  if not LParams.Trim.IsEmpty then
    LParams := '?' + LParams.Trim(['&']);
  LUrl := Format('%s/%s%s', [CUrlApi, 'payments', LParams]);
{$IFDEF FPC}
  InitSSLInterface;
  GetNetHttpRequestLocal(LHttpClient);
  try
    LResponse := TStringList.Create;
    try
      LHttpClient.ConnectTimeout := 20000;
      LHttpClient.Get(LUrl, LResponse);
      if LHttpClient.ResponseStatusCode <> 200 then
        raise exception.create(LHttpClient.ResponseStatusText);
      Result := LResponse.Text;
    finally
      LHttpClient.RequestBody.Free;
      LResponse.Free;
    end;
  finally
    LHttpClient.Free;
  end;
{$ELSE}
  var
  LHeader: TNetHeaders;
  LHeader :=
    [
      TNameValuePair.Create('Authorization', 'Bearer ' + FAuthToken),
      TNameValuePair.Create('Content-Type', 'application/json; charset=utf-8'),
      TNameValuePair.Create('Accept', '*/*')
    ];

  LHttp := THTTPClient.Create;
  try
    LResponse := LHttp.Get(LUrl, nil, LHeader);
  finally
    FreeAndNil(LHttp);
  end;

  if LResponse.StatusCode <> 200 then
    raise Exception.Create(LResponse.StatusCode.ToString + ' ' + LResponse.StatusText);
  Result := LResponse.ContentAsString(TEncoding.UTF8);
{$ENDIF}
end;

function TAPICoinOS.Post(AUrl, ABody: string): string;
{$IFDEF FPC}
var
  LHttpClient: TFPHttpClient;
  LResponse : TStringList;
{$ELSE}
var
  LHttp: THTTPClient;
  LResponse: IHTTPResponse;
{$ENDIF}
begin
{$IFDEF FPC}
  InitSSLInterface;
  GetNetHttpRequestLocal(LHttpClient);
  try
    LResponse := TStringList.Create;
    LHttpClient.RequestBody := TRawByteStringStream.Create(ABody);
    try
      LHttpClient.ConnectTimeout := 30000;
      LHttpClient.Post(AUrl, LResponse);
      if LHttpClient.ResponseStatusCode <> 200 then
        raise exception.create(LHttpClient.ResponseStatusText);
      Result := LResponse.Text;
    finally
      LHttpClient.RequestBody.Free;
      LResponse.Free;
    end;
  finally
    LHttpClient.Free;
  end;
{$ELSE}
  var
  LHeader: TNetHeaders;
  LHeader :=
    [
      TNameValuePair.Create('Authorization', 'Bearer ' + FAuthToken),
      TNameValuePair.Create('Content-Type', 'application/json; charset=utf-8'),
      TNameValuePair.Create('Accept', '*/*')
    ];

  LHttp := THTTPClient.Create;
  var
  LStrm := TStringStream.Create(ABody, TEncoding.UTF8, False);
  try
    LStrm.Position := 0;
    LResponse := LHttp.Post(AUrl, LStrm, nil, LHeader);
  finally
    LStrm.Free;
    FreeAndNil(LHttp);
  end;

  if LResponse.StatusCode <> 200 then
  begin
    var
    LMsg := EmptyStr;
    try
      LMsg := LResponse.ContentAsString(TEncoding.UTF8);
      if not LMsg.Trim.IsEmpty then
        LMsg := ' - ' + LMsg;
    except

    end;
    raise Exception.Create(LResponse.StatusCode.ToString + ' ' + LResponse.StatusText + LMsg);
  end;
  Result := LResponse.ContentAsString(TEncoding.UTF8);
{$ENDIF}
end;

function TAPICoinOS.PostBitcoinSend(AAmount: Integer; AAddress: string): string;
var
  LUrl: string;
  LBody: string;
begin
  LUrl := Format('%s/%s', [CUrlApi, 'bitcoin/send']);
  LBody := EmptyStr;
{$IFDEF FPC}
  raise exception.create('TAPICoinOS.Get');
{$ELSE}
  var
  LJSONObj := TJSONObject.Create;
  try
    LJSONObj.AddPair('amount', AAmount);
    LJSONObj.AddPair('address', AAddress);
    LBody := LJSONObj.ToString
  finally
    LJSONObj.Free;
  end;
{$ENDIF}
  Result := Post(LUrl, LBody);
end;

function TAPICoinOS.PostInvoice(AType: string; AAmount: Integer;
  AWebHook: string; ASecret: string; AExpiration: Integer): string;
var
  LUrl: string;
  LBody: string;
{$IFDEF FPC}
  LJsonData: TJSONData;
  LJsonObject: TJSONObject;
  LStrLst: TStringList;
{$ENDIF}
begin
  LUrl := Format('%s/%s', [CUrlApi, 'invoice']);
  LBody := EmptyStr;
{$IFDEF FPC}
  LJsonData :=  GetJSON('{}');
  LStrLst := TStringList.Create;
  try
    LJsonObject := LJsonData as TJSONObject;
    LJsonObject.Add('amount', AAmount);
    LJsonObject.Add('type', AType);
    if not AWebHook.Trim.IsEmpty then
      LJsonObject.Add('webhook', AWebHook);
    if not ASecret.Trim.IsEmpty then
      LJsonObject.Add('secret', ASecret);
    if AExpiration > 0 then
      LJsonObject.Add('expiry', AExpiration);
    LJsonData :=  GetJSON('{}');
    TJSONObject(LJsonData).Add('invoice', LJsonObject);
    LBody := TJSONObject(LJsonData).AsJSON;
  finally
    LStrLst.Free;
    LJsonData.Free;
  end;
{$ELSE}
  var
  LJSONObj := TJSONObject.Create;
  try
    var
    LJSONObj1 := TJSONObject.Create;
    LJSONObj1.AddPair('amount', AAmount);
    LJSONObj1.AddPair('type', AType);
    if not AWebHook.Trim.IsEmpty then
      LJSONObj1.AddPair('webhook', AWebHook);
    if not ASecret.Trim.IsEmpty then
      LJSONObj1.AddPair('secret', ASecret);
    if AExpiration > 0 then
      LJSONObj1.AddPair('expiry', AExpiration);
    LJSONObj.AddPair('invoice', LJSONObj1);
    LBody := LJSONObj.ToString
  finally
    LJSONObj.Free;
  end;
{$ENDIF}
  Result := Post(LUrl, LBody);
end;

function TAPICoinOS.PostInvoiceBitcoin(AAmount: Integer; AWebHook: string;
  ASecret: string; AExpiration: Integer): string;
begin
  Result := PostInvoice('bitcoin', AAmount, AWebHook, ASecret, AExpiration);
end;

function TAPICoinOS.PostInvoiceLightning(AAmount: Integer; AWebHook: string;
  ASecret: string; AExpiration: Integer): string;
begin
  Result := PostInvoice('lightning', AAmount, AWebHook, ASecret, AExpiration);
end;

function TAPICoinOS.PostLogin(AUsername, APassword: string): string;
var
  LUrl: string;
  LBody: string;
{$IFDEF FPC}
  LJsonObject: TJSONObject;
{$ENDIF}
begin
  LUrl := Format('%s/%s', [CUrlApi, 'login']);
  LBody := EmptyStr;
{$IFDEF FPC}
  try
    LJsonObject := GetJSON('{}') as TJSONObject;
    LJsonObject.Add('username', AUsername);
    LJsonObject.Add('password', APassword);
    LBody := LJsonObject.AsJSON;
  finally
    LJsonObject.Free;
  end;
{$ELSE}
  var
  LJSONObj := TJSONObject.Create;
  try
    LJSONObj.AddPair('username', AUsername);
    LJSONObj.AddPair('password', APassword);
    LBody := LJSONObj.ToString
  finally
    LJSONObj.Free;
  end;
{$ENDIF}
  Result := Post(LUrl, LBody); 
end;

function TAPICoinOS.PostPayment(AInvoice: string; AMaxFee: Integer): string;
var
  LUrl: string;
  LBody: string;
{$IFDEF FPC}
    LJsonObject: TJSONObject;
{$ENDIF}
begin
  LUrl := Format('%s/%s', [CUrlApi, 'payments']);
  LBody := EmptyStr;
{$IFDEF FPC}
 try
    LJsonObject := GetJSON('{}') as TJSONObject;
    LJsonObject.Add('payreq', AInvoice);
    LJsonObject.Add('maxfee', AMaxFee);
    LBody := LJsonObject.AsJSON;
  finally
    LJsonObject.Free;
  end;
{$ELSE}

  var
  LJSONObj := TJSONObject.Create;
  try
    LJSONObj.AddPair('payreq', AInvoice);
    if AMaxFee > 0 then
      LJSONObj.AddPair('maxfee', AMaxFee);
    LBody := LJSONObj.ToString
  finally
    LJSONObj.Free;
  end;
{$ENDIF}
  Result := Post(LUrl, LBody);
end;

function TAPICoinOS.PostPaymentToUser(AAmount: Integer;
  AUserName: string): string;
var
  LUrl: string;
  LBody: string;
begin
  LUrl := Format('%s/%s', [CUrlApi, 'invoice']);
  LBody := EmptyStr;
{$IFDEF FPC}
  raise exception.create('TAPICoinOS.PostPaymentToUser');
{$ELSE}
  var
  LJSONObj := TJSONObject.Create;
  try
    var
    LJSONObj1 := TJSONObject.Create;
    LJSONObj1.AddPair('amount', AAmount);
    LJSONObj1.AddPair('type', 'lightning');
    LJSONObj.AddPair('invoice', LJSONObj1);
    LJSONObj1 := TJSONObject.Create;
    LJSONObj1.AddPair('username', AUserName);
    LJSONObj.AddPair('user', LJSONObj1);
    LBody := LJSONObj.ToString
  finally
    LJSONObj.Free;
  end;
{$ENDIF}
  Result := Post(LUrl, LBody);
end;

function TAPICoinOS.PostRegister(AUsername, APassword: string): string;
var
  LUrl: string;
  LBody: string;
{$IFDEF FPC}
  LJsonData: TJSONData;
  LJsonObject: TJSONObject;
{$ENDIF}
begin
  LUrl := Format('%s/%s', [CUrlApi, 'register']);
  LBody := EmptyStr;
{$IFDEF FPC}
  LJsonData :=  GetJSON('{}');
  try
    LJsonObject := LJsonData as TJSONObject;
    LJsonObject.Add('username', AUsername);
    LJsonObject.Add('password', APassword);
    LJsonData :=  GetJSON('{}');
    TJSONObject(LJsonData).Add('user', LJsonObject);
    LBody := TJSONObject(LJsonData).AsJSON;
  finally
    LJsonData.Free;
  end;
{$ELSE}
  var
  LJSONObj := TJSONObject.Create;
  try
    var
    LJSONObj1 := TJSONObject.Create;
    LJSONObj1.AddPair('username', AUsername);
    LJSONObj1.AddPair('password', APassword);
    LJSONObj.AddPair('user', LJSONObj1);
    LBody := LJSONObj.ToString
  finally
    LJSONObj.Free;
  end;
{$ENDIF}
  Result := Post(LUrl, LBody); 
end;

function TAPICoinOS.PostUser(AJson: string): string;
var
  LUrl: string;
begin
  LUrl := Format('%s/%s', [CUrlApi, 'user']);
  Result := Post(LUrl, AJson);
end;

function TAPICoinOS.AuthToken(AValue: string): TAPICoinOS;
begin
  Result := Self;
  FAuthToken := AValue;
end;

end.
