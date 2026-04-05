unit IdeaL.Lib.Api;

interface

uses
{$IFDEF FPC}
  Classes,
  SysUtils,

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
  TIdeaLApi = class
  private
    { private declarations }
  protected
    { protected declarations }
  public
    constructor Create; virtual;
{$IFDEF FPC}
    class procedure GetHttpRequest(
          out AHttpClient: TFPHttpClient;
          AOnData: TDataEvent = nil); virtual;
{$ELSE}
    class procedure GetNetHttpRequest(
      out ANetHttpClient: TNetHTTPClient;
      out ANetHttpRequest: TNetHTTPRequest;
      AOnSendDataEvent: TSendDataEvent = nil;
      AOnReceiveData: TReceiveDataEvent = nil); virtual;
{$ENDIF}
    function Post(AUrl, ABody: string; AHeaders: TNetHeaders): string; virtual;
    function Get(AUrl: string; AHeaders: TNetHeaders): string; overload; virtual;
    { public declarations }
  end;

implementation

{$IFDEF FPC}
uses
  openssl, { This implements the procedure InitSSLInterface }
  opensslsockets;
{$ENDIF}

{ TIdeaLApi }
{$IFDEF FPC}
class procedure TIdeaLApi.GetHttpRequest(
  out AHttpClient: TFPHttpClient;
  AOnData: TDataEvent = nil);
begin
  AHttpClient := TFPHttpClient.Create(Nil);
  AHttpClient.OnDataReceived := AOnData;
  AHttpClient.AddHeader('Accept', '*/*');
  AHttpClient.AddHeader('Accept-Charset', 'utf-8');
  //AHttpClient.AddHeader('Content-Type', 'multipart/form-data');
  AHttpClient.AddHeader('User-Agent',
    'Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 6.0; Acoo Browser; ' +
    'GTB5; Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.1; SV1) ; ' +
    'Maxthon; InfoPath.1; .NET CLR 3.5.30729; .NET CLR 3.0.30618)');
end;
{$ELSE}
class procedure TIdeaLApi.GetNetHttpRequest(out ANetHttpClient: TNetHTTPClient;
  out ANetHttpRequest: TNetHTTPRequest; AOnSendDataEvent: TSendDataEvent;
  AOnReceiveData: TReceiveDataEvent);
begin
  ANetHttpClient := TNetHTTPClient.Create(nil);
  ANetHttpClient.Accept := '*/*';
  ANetHttpClient.AcceptCharSet := 'utf-8';
  ANetHttpClient.AcceptEncoding := 'gzip, deflate, br';
  //ANetHttpClient.ContentType := 'multipart/form-data';
  ANetHttpClient.UserAgent :=
    'Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 6.0; Acoo Browser; ' +
    'GTB5; Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.1; SV1) ; ' +
    'Maxthon; InfoPath.1; .NET CLR 3.5.30729; .NET CLR 3.0.30618)';

  ANetHttpRequest := TNetHTTPRequest.Create(nil);
  ANetHttpRequest.OnSendData := AOnSendDataEvent;
  ANetHttpRequest.OnReceiveData := AOnReceiveData;
  ANetHttpRequest.Client := ANetHttpClient;
end;
function TIdeaLApi.Post(AUrl, ABody: string; AHeaders: TNetHeaders): string;
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
  GetHttpRequest(LHttpClient);
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
  LHttp := THTTPClient.Create;
  var
  LStrm := TStringStream.Create(ABody, TEncoding.UTF8, False);
  try
    LStrm.Position := 0;
    LResponse := LHttp.Post(AUrl, LStrm, nil, AHeaders);
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

{$ENDIF}

constructor TIdeaLApi.Create;
begin

end;

function TIdeaLApi.Get(AUrl: string; AHeaders: TNetHeaders): string;
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
  GetHttpRequest(LHttpClient);
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
  LHttp := THTTPClient.Create;
  try
    LResponse := LHttp.Get(AUrl, nil, AHeaders);
  finally
    FreeAndNil(LHttp);
  end;

  if (LResponse.StatusCode < 200) or (LResponse.StatusCode > 299) then
    raise Exception.Create(LResponse.StatusCode.ToString + ' ' + LResponse.StatusText);
  Result := LResponse.ContentAsString(TEncoding.UTF8);
{$ENDIF}
end;

end.
