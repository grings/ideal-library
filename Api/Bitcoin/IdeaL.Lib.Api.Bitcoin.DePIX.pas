unit IdeaL.Lib.Api.Bitcoin.DePIX;

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
  /// <see cref="https://docs.eulen.app/"/>
  /// </remarks>
  TAPIDePix = class(TIdeaLApi)
  private
    FAuthToken: string;
    FXAsync: string;
    FXNonce: string;
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
    CUrlApi = 'https://depix.eulen.app/api';
  public
    constructor Create;

    function AuthToken(AValue: string): TAPIDePix; overload;
    function AuthToken: string; overload;
    function XAsync(AValue: string): TAPIDePix; overload;
    function XAsync: string; overload;
    function XNonce(AValue: string): TAPIDePix; overload;
    function XNonce: string; overload;

    function GetDepositStatus(AId: string): string;
    function GetPing: string;
    function GetUserInfo(AEuid: string): string;

    /// <summary> The system generates a dynamic QR code designed for deposits
    /// in Reais (BRL) through the PIX banking system
    /// <see cref="https://docs.eulen.app/deposit-pix-depix-12532107e0"/>
    /// </summary>
    /// <param name="AStart"> Start date in YYYY-MM-DD format or RFC3339.
    /// </param>
    /// <param name="AEnd"> End date in YYYY-MM-DD format or RFC3339.
    /// </param>
    /// <param name="AStatus"> A status to filter by.
    // Allowed values: pending, depix_sent, under_review, canceled, error, refunded, expired
    /// </param>
    function GetDeposits(AStart, AEnd, AStatus: string): string; overload;
    function GetDeposits(AStart, AEnd: TDate; AStatus: string): string; overload;

    /// <summary> The system generates a dynamic QR code designed for deposits
    /// in Reais (BRL) through the PIX banking system
    /// <see cref="https://docs.eulen.app/deposit-pix-depix-12532107e0"/>
    /// </summary>
    /// <param name="AAmountInCents"> Required - Integer value in cents of reais.
    /// For R$ 1,00 for example: the number passed must be 100. >= 1 <= 10000000
    /// </param>
    /// <param name="ADepixAddress"> Optional DePix Address
    /// </param>
    /// <param name="ADepixSplitAddress"> Optional DePix Split Address
    /// </param>
    /// <param name="ASplitFee"> Optional Split Fee (the percentage of
    /// amountInCents that will be splitted and sent to depixSplitAddress)
    /// </param>
    /// <param name="ASplitFee"> Optional lightning flag
    /// </param>
    function PostDeposit(AAmountInCents: Integer): string; overload;
    function PostDeposit(
      AAmountInCents: Integer;
      ADepixAddress: string;
      ADepixSplitAddress: string;
      ASplitFee: string;
      ALightning: Boolean): string; overload;
    { public declarations }
  end;

implementation

{ TAPIDePix }

function TAPIDePix.AuthToken(AValue: string): TAPIDePix;
begin
  Result := Self;
  FAuthToken := AValue;
end;

function TAPIDePix.AuthToken: string;
begin
  Result := FAuthToken;
end;

constructor TAPIDePix.Create;
begin
  FAuthToken := EmptyStr;
  FXAsync := 'false';
  FXNonce := EmptyStr;
end;

{$IFDEF FPC}
procedure TAPIDePix.GetNetHttpRequestLocal(
  out AHttpClient: TFPHttpClient;
  AOnData: TDataEvent = nil);
begin
  GetHttpRequest(AHttpClient, AOnData);
  AHttpClient.AddHeader('Content-Type', 'application/json');
  AHttpClient.AddHeader('Authorization', 'Bearer ' + FAuthToken);
  if not FXAsync.Trim.IsEmpty then
    AHttpClient.AddHeader('FXAsync', FXAsync);
  if not FXNonce.Trim.IsEmpty then
    AHttpClient.AddHeader('X-Nonce', FXNonce);
end;
{$ELSE}
procedure TAPIDePix.GetNetHttpRequestLocal(out ANetHttpClient: TNetHTTPClient;
  out ANetHttpRequest: TNetHTTPRequest; AOnSendDataEvent: TSendDataEvent;
  AOnReceiveData: TReceiveDataEvent);
begin
  GetNetHttpRequest(ANetHttpClient, ANetHttpRequest, AOnSendDataEvent, AOnReceiveData);
  ANetHttpClient.ContentType := 'application/json; charset=utf-8';
  ANetHttpClient.AcceptEncoding :='';

  ANetHttpClient.CustomHeaders['Content-Type'] := ANetHttpClient.ContentType;
  ANetHttpClient.CustomHeaders['Accept'] := ANetHttpClient.Accept;
  ANetHttpClient.CustomHeaders['Authorization'] := 'Bearer ' + FAuthToken;
  if not FXAsync.Trim.IsEmpty then
    ANetHttpClient.CustomHeaders['X-Async'] := FXAsync;
  if not FXNonce.Trim.IsEmpty then
    ANetHttpClient.CustomHeaders['X-Nonce'] := FXNonce;
end;

{$ENDIF}

function TAPIDePix.Get(AUrl: string): string;
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
  {var
  LHeader: TNetHeaders;
  LHeader :=
    [
      TNameValuePair.Create('Authorization', 'Bearer ' + FAuthToken),
      TNameValuePair.Create('Content-Type', 'application/json; charset=utf-8'),
      TNameValuePair.Create('Accept', '*/*')
    ];

  LHttp := THTTPClient.Create;}
  var
  LNetHttpClient: TNetHTTPClient := nil;
  var
  LNetHttpRequest: TNetHTTPRequest := nil;
  try
    GetNetHttpRequestLocal(LNetHttpClient, LNetHttpRequest, nil, nil);
    // LResponse := LHttp.Get(AUrl, nil, LHeader);
    LResponse := LNetHttpRequest.Get(AUrl);
  finally
    // FreeAndNil(LHttp);
    LNetHttpClient.Free;
    LNetHttpRequest.Free;
  end;

  if (LResponse.StatusCode < 200) or (LResponse.StatusCode > 299) then
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

function TAPIDePix.GetDeposits(AStart, AEnd, AStatus: string): string;
var
  LUrl: string;
begin
  LUrl := Format('%s/deposits?start=%s&end=%s', [CUrlApi, AStart, AEnd]);
  if not AStatus.Trim.IsEmpty then
    LUrl := LUrl + '&status=' + AStatus;
  Result := Get(LUrl);
end;

function TAPIDePix.GetDeposits(AStart, AEnd: TDate; AStatus: string): string;
begin
  var
  LStart := FormatDateTime('yyyy-mm-dd', AStart);
  var
  LEnd := FormatDateTime('yyyy-mm-dd', AEnd);
  Result := GetDeposits(LStart, LEnd, AStatus);
end;

function TAPIDePix.GetDepositStatus(AId: string): string;
var
  LUrl: string;
begin
  LUrl := Format('%s/deposit-status?id=%s', [CUrlApi, AId]);
  Result := Get(LUrl);
end;

function TAPIDePix.GetPing: string;
var
  LUrl: string;
begin
  LUrl := Format('%s/ping', [CUrlApi]);
  Result := Get(LUrl);
end;

function TAPIDePix.GetUserInfo(AEuid: string): string;
var
  LUrl: string;
begin
  LUrl := Format('%s/user-info?euid=%s', [CUrlApi, AEuid]);
  Result := Get(LUrl);
end;

function TAPIDePix.Post(AUrl, ABody: string): string;
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
  {var
  LHeader: TNetHeaders;
  LHeader :=
    [
      TNameValuePair.Create('Authorization', 'Bearer ' + FAuthToken),
      TNameValuePair.Create('Content-Type', 'application/json; charset=utf-8'),
      TNameValuePair.Create('Accept', '*/*')
    ];

  LHttp := THTTPClient.Create;}
  var
  LStrm := TStringStream.Create(ABody, TEncoding.UTF8, False);
  var
  LNetHttpClient: TNetHTTPClient := nil;
  var
  LNetHttpRequest: TNetHTTPRequest := nil;
  try
    GetNetHttpRequestLocal(LNetHttpClient, LNetHttpRequest, nil, nil);

    LStrm.Position := 0;
    // LResponse := LHttp.Post(AUrl, LStrm, nil, LHeader);
    LResponse := LNetHttpRequest.Post(AUrl, LStrm);
  finally
    LStrm.Free;
    // FreeAndNil(LHttp);
    LNetHttpClient.Free;
    LNetHttpRequest.Free;
  end;

  if (LResponse.StatusCode < 200) or (LResponse.StatusCode > 299) then
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

function TAPIDePix.PostDeposit(AAmountInCents: Integer): string;
begin
  Result := PostDeposit(AAmountInCents, EmptyStr, EmptyStr, EmptyStr, False);
end;

function TAPIDePix.PostDeposit(AAmountInCents: Integer; ADepixAddress,
  ADepixSplitAddress, ASplitFee: string; ALightning: Boolean): string;
var
  LUrl: string;
  LBody: string;
{$IFDEF FPC}
  LJsonData: TJSONData;
  LJsonObject: TJSONObject;
{$ENDIF}
begin
  if (AAmountInCents < 100) or (AAmountInCents > 10000000) then
    raise Exception.Create('Invalid value [amount]');

  LUrl := Format('%s/%s', [CUrlApi, 'deposit']);
  LBody := EmptyStr;
{$IFDEF FPC}
  LJsonData :=  GetJSON('{}');
  try
    LJsonObject := LJsonData as TJSONObject;
    LJsonObject.Add('amountInCents', AAmountInCents);
    if not ADepixAddress.Trim.IsEmpty then
      LJSONObj.AddPair('depixAddress', ADepixAddress);
    if (not ADepixSplitAddress.Trim.IsEmpty) and (not ASplitFee.Trim.IsEmpty) then
    begin
      LJSONObj.AddPair('depixSplitAddress', ADepixSplitAddress);
      LJSONObj.AddPair('splitFee', FormatFloat('0.00', StrToFloat(ASplitFee)) + '%');
    end;
    if ALightning then
      LJSONObj.AddPair('lightning', ALightning);
    LBody := TJSONObject(LJsonData).AsJSON;
  finally
    LJsonData.Free;
  end;
{$ELSE}
  var
  LJSONObj := TJSONObject.Create;
  try
    LJSONObj.AddPair('amountInCents', AAmountInCents);
    if not ADepixAddress.Trim.IsEmpty then
      LJSONObj.AddPair('depixAddress', ADepixAddress);

    if (not ADepixSplitAddress.Trim.IsEmpty) and (not ASplitFee.Trim.IsEmpty) then
    begin
      LJSONObj.AddPair('depixSplitAddress', ADepixSplitAddress);
      LJSONObj.AddPair('splitFee', FormatFloat('0.00', ASplitFee.ToDouble) + '%');
    end;
    if ALightning then
      LJSONObj.AddPair('lightning', ALightning);
    LBody := LJSONObj.ToString
  finally
    LJSONObj.Free;
  end;
{$ENDIF}
  Result := Post(LUrl, LBody);
end;

function TAPIDePix.XAsync: string;
begin
  Result := FXAsync;
end;

function TAPIDePix.XAsync(AValue: string): TAPIDePix;
begin
  Result := Self;
  FXAsync := AValue;
end;

function TAPIDePix.XNonce: string;
begin
  Result := FXNonce;
end;

function TAPIDePix.XNonce(AValue: string): TAPIDePix;
begin
  Result := Self;
  FXNonce := AValue;
end;

end.
