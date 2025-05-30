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
  AHttpClient.AddHeader('Content-Type', 'multipart/form-data');
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
  ANetHttpClient.ContentType := 'multipart/form-data';
  ANetHttpClient.UserAgent :=
    'Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 6.0; Acoo Browser; ' +
    'GTB5; Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.1; SV1) ; ' +
    'Maxthon; InfoPath.1; .NET CLR 3.5.30729; .NET CLR 3.0.30618)';

  ANetHttpRequest := TNetHTTPRequest.Create(nil);
  ANetHttpRequest.OnSendData := AOnSendDataEvent;
  ANetHttpRequest.OnReceiveData := AOnReceiveData;
  ANetHttpRequest.Client := ANetHttpClient;
end;
{$ENDIF}

end.
