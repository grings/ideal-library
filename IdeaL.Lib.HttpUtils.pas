unit IdeaL.Lib.HttpUtils;

interface

uses
  System.Classes,
  System.SysUtils,
  System.StrUtils,
  System.JSON,

  System.Net.URLClient,
  System.Net.HttpClient,
  System.Net.HttpClientComponent;

type
  TRequestType = (rtGet, rtPost);

  THttpUtils = class
  private
    FToken: string;
    FPassword: string;
    FUserName: string;
    procedure NetHTTPClientAuthEvent(const Sender: TObject;
      AnAuthTarget: TAuthTargetType; const ARealm, AUrl: string;
      var AUserName, APassword: string; var AbortAuth: Boolean;
      var Persistence: TAuthPersistenceType);
    procedure SetPassword(const Value: string);
    procedure SetToken(const Value: string);
    procedure SetUserName(const Value: string);
    { private declarations }
  protected
    { protected declarations }
  public
    constructor Create;

    property UserName: string read FUserName write SetUserName;
    property Password: string read FPassword write SetPassword;
    property Token: string read FToken write SetToken;
    function Net(
      const AUrl: string;
      const AContentType: string;
      const ABody: string = '';
      const ARequestType: TRequestType = rtGet;
      const AIsAsynchronous: Boolean = False;
      const ATimeOut: Integer = -1): string;
    { public declarations }
  end;

implementation

{ THttp }

constructor THttpUtils.Create;
begin
  FUserName := EmptyStr;
  FPassword := EmptyStr;
  FToken := EmptyStr;
end;

function THttpUtils.Net(const AUrl, AContentType, ABody: string;
  const ARequestType: TRequestType; const AIsAsynchronous: Boolean;
  const ATimeOut: Integer): string;
var
  LBody: TStringList;
  LBodyStr: string;
  LResponse: IHttpResponse;
  HttpClient: TNetHTTPClient;
  HTTPRequest: TNetHTTPRequest;
begin
  try
    LBody := nil;
    Result := EmptyStr;
    HttpClient := TNetHTTPClient.Create(nil);
    HttpClient.OnAuthEvent := NetHTTPClientAuthEvent;
    HttpClient.ContentType := AContentType;
    // HttpClient.Asynchronous := AIsAsynchronous;
    HTTPRequest := TNetHTTPRequest.Create(nil);
    HTTPRequest.Client := HttpClient;
    HTTPRequest.Asynchronous := AIsAsynchronous;

    if (ATimeOut <> -1) then
    begin
      HttpClient.ConnectionTimeout := ATimeOut;
      HttpClient.ResponseTimeout := ATimeOut;
      HTTPRequest.ConnectionTimeout := ATimeOut;
      HTTPRequest.ResponseTimeout := ATimeOut;
    end;

    if (ABody.Trim.isEmpty) then
    begin
      LBodyStr := ABody;
      LBodyStr := ABody;
      LBody := TStringList.Create;
      LBody.Add(LBodyStr);
    end;

    case ARequestType of
      rtGet:
      begin
        LResponse := //
          HTTPRequest.Get( //
          AUrl, //
          nil, //
          [TNameValuePair.Create('Content-Type', AContentType)] //
          );
      end;
      rtPost:
      begin
        LResponse := //
        HTTPRequest.Post( //
        AUrl, //
        LBody, //
        nil, //
        nil, //
        [TNameValuePair.Create('Content-Type', AContentType)] //
        );
      end;
    end;

    if not(AIsAsynchronous) then
    begin
      Result := LResponse.ContentAsString(TEncoding.UTF8);
      // Result := JsonResultValidate(Result);
    end;
  finally
    FreeAndNil(LBody);
    FreeAndNil(HttpClient);
    FreeAndNil(HTTPRequest);
  end;
end;

procedure THttpUtils.NetHTTPClientAuthEvent(const Sender: TObject;
  AnAuthTarget: TAuthTargetType; const ARealm, AUrl: string; var AUserName,
  APassword: string; var AbortAuth: Boolean;
  var Persistence: TAuthPersistenceType);
begin
  if AnAuthTarget = TAuthTargetType.Server then
  begin
    AUserName := FUserName;
    APassword := FPassword;
  end;
end;

procedure THttpUtils.SetPassword(const Value: string);
begin
  FPassword := Value;
end;

procedure THttpUtils.SetToken(const Value: string);
begin
  FToken := Value;
end;

procedure THttpUtils.SetUserName(const Value: string);
begin
  FUserName := Value;
end;

end.
