unit IdeaL.Lib.Api.Bitcoin.DIGPIX;

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
  /// <see cref="https://vempradig.com/digpix/api-doc"/>
  /// </remarks>
  TAPIDigPix = class(TIdeaLApi)
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
    { protected declarations }
  public
    const
    CUrlApi = 'https://vempradig.com/api/';
  public
    function AuthToken(AValue: string): TAPIDigPix; overload;
    function AuthToken: string; overload;

    /// <summary> Creates the PIX to be paied in DePIX environment
    /// </summary>
    /// <param name="Item">The item to remove
    /// </param>
    /// <returns> Returns a JSON Object with:
    /// id (from DePIX environment)
    /// qrCopyPaste: the copy paste PIX QRCode
    /// qrImageUrl: URL of the QRCode
    /// </returns>
    function PostQrCode(AAmount: Currency; const AWebHook, ASecret: string): string;
    { public declarations }
  end;

implementation

{ TAPIDigPix }

function TAPIDigPix.AuthToken(AValue: string): TAPIDigPix;
begin
  Result := Self;
  FAuthToken := AValue;
end;

function TAPIDigPix.AuthToken: string;
begin
  Result := FAuthToken;
end;

{$IFDEF FPC}
procedure TAPIDigPix.GetNetHttpRequestLocal(
  out AHttpClient: TFPHttpClient;
  AOnData: TDataEvent = nil);
begin
  GetHttpRequest(AHttpClient, AOnData);
  AHttpClient.AddHeader('Content-Type', 'application/json');
  AHttpClient.AddHeader('Authorization', 'Bearer ' + FAuthToken);
end;
{$ELSE}
procedure TAPIDigPix.GetNetHttpRequestLocal(out ANetHttpClient: TNetHTTPClient;
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

function TAPIDigPix.Post(AUrl, ABody: string): string;
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

function TAPIDigPix.PostQrCode(AAmount: Currency; const AWebHook,
  ASecret: string): string;
var
  LUrl: string;
  LBody: string;
{$IFDEF FPC}
  LJsonData: TJSONData;
  LJsonObject: TJSONObject;
{$ENDIF}
begin
  {
    All rules bellow were implemented based on official doc at 2025-08-11

    The PIX has 10min expiration time, this is not negotiable
  }


  if (AAmount < 10) or (AAmount > 5000) then
    raise Exception.Create('Invalid value [amount]');
  if
    (Trim(AWebHook) = EmptyStr) or
    (not LowerCase(AWebHook).StartsWith('http')) or
    (Length(AWebHook) > 255)
  then
    raise Exception.Create('Invalid value [webhook]');
  if
    (Trim(ASecret) = EmptyStr) or
    (Length(ASecret) > 64)
  then
    raise Exception.Create('Invalid value [webhook]');

  LUrl := Format('%s/%s', [CUrlApi, 'qrcode']);
  LBody := EmptyStr;
{$IFDEF FPC}
  LJsonData :=  GetJSON('{}');
  try
    LJsonObject := LJsonData as TJSONObject;
    LJsonObject.Add('amount', AAmount);
    if Trim(AWebHook) <> EmtpyStr then
      LJSONObj.AddPair('webhook', AWebHook);
    if Trim(ASecret) <> EmtpyStr then
      LJSONObj.AddPair('secret', ASecret);
    LBody := TJSONObject(LJsonData).AsJSON;
  finally
    LJsonData.Free;
  end;
{$ELSE}
  var
  LJSONObj := TJSONObject.Create;
  try
    LJSONObj.AddPair('amount', AAmount);
    LJSONObj.AddPair('webhook', AWebHook);
    LJSONObj.AddPair('secret', ASecret);
    LBody := LJSONObj.ToString
  finally
    LJSONObj.Free;
  end;
{$ENDIF}
  Result := Post(LUrl, LBody);
end;

end.
