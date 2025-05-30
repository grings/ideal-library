unit IdeaL.Lib.RestDataware.Utils;

interface

uses
  System.Classes,
  System.SysUtils,
  System.StrUtils,
  System.JSON,

  uDWJSONObject,
  uRESTDWServerEvents,
  uDWConstsData,
  uDWAbout,
  uDWResponseTranslator,
  uDWConstsCharset,

  System.Net.URLClient,
  System.Net.HttpClient,
  System.Net.HttpClientComponent,

  IdBaseComponent,
  IdComponent,
  IdTCPConnection,
  IdTCPClient,
  IdHTTP;

type
  TRdwUtils = class
  private
    FPassWord: string;
    FUserName: string;
    procedure SetPassWord(const Value: string);
    procedure SetUserName(const Value: string);

    procedure NetHTTPClientAuthEvent(const Sender: TObject;
      AnAuthTarget: TAuthTargetType; const ARealm, AUrl: string;
      var AUserName, APassword: string; var AbortAuth: Boolean;
      var Persistence: TAuthPersistenceType);
    function JsonResultValidate(const AJson: string): string;
    { private declarations }
  protected
    { protected declarations }
  public
    class function SendEvent(ADataModule: TDataModule;
      const AEventName, AContextName: String; AParamList: TStringList = nil)
      : String; overload;
    class function SendEvent(ADataModule: TDataModule;
      const AEventName, AContextName, AStrParamList: String): String; overload;
    class function VerifyParamsRequired(const ADwParams: TDWParams;
      const AParamsToVerify: string; const AShowException: Boolean = True)
      : Boolean; overload;
    class function VerifyParamsRequired(const AStrParams: string;
      const AParamsToVerify: string; const AShowException: Boolean = True)
      : Boolean; overload;
    class function GetParamValue(const AParams, AParamName: string): string;

    function SendEventWithNetHttp(const AUrl, AContentType: string;
      const ABody: string = ''; const AIsAsynchronous: Boolean = False;
      const ATimeOut: Integer = -1): string;
    function SendEventWithIdHttp(const AUrl, AContentType: string;
      const ABody: string = ''; const ATimeOut: Integer = -1): string;
    { function SendEventWithRdwClientRest(const AUrl, AContentType: string;
      const ABody: string = ''; const AIsAsynchronous: Boolean = False): string; }

    property UserName: string read FUserName write SetUserName;
    property PassWord: string read FPassWord write SetPassWord;
    { public declarations }
  published
    { published declarations }
  end;

implementation

{ TRdwUtils }

class function TRdwUtils.SendEvent(ADataModule: TDataModule;
  const AEventName, AContextName: String; AParamList: TStringList): String;
var
  LDwParams: TDWParams;
  LDWClientEvents: TDWClientEvents;
  LAssyncSend: Boolean;
  LMessageError: string;
  i: Integer;
  LComponentName: string;
  LText: string;

  LException: Boolean;

  LJsonArr: System.JSON.TJSONArray;
  LJsonValue: System.JSON.TJSONValue;
  LItem: System.JSON.TJSONValue;
begin
  try
    LMessageError := EmptyStr;
    LDWClientEvents := nil;
    LJsonArr := nil;
    LJsonValue := nil;
    LItem := nil;
    LException := False;
    LAssyncSend := False;

    LComponentName := 'CE' + Copy(AContextName, 3, AContextName.Length);
    LDWClientEvents := TDWClientEvents
      (ADataModule.FindComponent(LComponentName));

    if (LDWClientEvents = nil) then
      raise Exception.Create('ContextName [' + LComponentName + '] no found');

    LDWClientEvents.CreateDWParams(AEventName, LDwParams);

    if (Assigned(AParamList)) then
      for i := 0 to Pred(AParamList.Count) do
      begin
        LDwParams.ItemsString[AParamList.Names[i]].AsString :=
          AParamList.ValueFromIndex[i];
      end;

    LAssyncSend := LDWClientEvents.RESTClientPooler.ThreadRequest;
    LDWClientEvents.RESTClientPooler.ThreadRequest := False;

    if (LAssyncSend) then
      LDWClientEvents.SendEvent(AEventName, LDwParams, LMessageError,
        TSendEvent.sePOST, True)
    else
      LDWClientEvents.SendEvent(AEventName, LDwParams, LMessageError, Result);

    // Volto aa config "original"
    LDWClientEvents.RESTClientPooler.ThreadRequest := LAssyncSend;

    if not(LMessageError.isEmpty) then
      raise Exception.Create(LMessageError);

    // Se for com Thread, nao deve aguardar retorno
    if (LAssyncSend) then
      Exit;

    // Recebo o JsonArray
    LJsonArr := System.JSON.TJSONObject.ParseJSONValue
      (TEncoding.UTF8.GetBytes(Result), 0) as System.JSON.TJSONArray;

    for LJsonValue in LJsonArr do
    begin // Percorro tds os arrays
      for LItem in System.JSON.TJSONArray(LJsonValue) do
      begin // Percorro todos os Index pegando os valores
        case AnsiIndexStr((System.JSON.TJSONPair(LItem).JsonString.Value),
          ['Result', // 0
          'Message', // 1
          'Values', // 2
          '']) of
          0:
            begin
              case AnsiIndexStr(UpperCase(System.JSON.TJSONPair(LItem)
                .JsonString.Value), ['OK', // 0
                'ERROR', // 1
                'WARNING', // 2
                '']) of
                1:
                  begin
                    LException := True;
                    Break;
                  end;
              end;
            end;
          1:
            begin
              raise Exception.Create(System.JSON.TJSONPair(LItem)
                .JsonValue.Value);
            end;
          2:
            begin
              Result := System.JSON.TJSONPair(LItem).JsonValue.ToJSON;
            end;
        end;
      end;

      if (LException) then
        // Ele so entra aqui se teve erro no servidor mas nao teve a MESSAGE
        raise Exception.Create
          ('Erro desconhecido no servidor. Contate o suporte técnico.');
    end;
  finally
    LJsonArr.DisposeOf;
    LDwParams.DisposeOf;
  end;
end;

class function TRdwUtils.GetParamValue(const AParams,
  AParamName: string): string;
var
  LStrListValues: TStringList;
  LIndex: Integer;
  LMsg: string;
begin
  try
    LMsg := EmptyStr;
    Result := EmptyStr;

    LStrListValues := TStringList.Create;
    LStrListValues.Delimiter := '|';
    LStrListValues.NameValueSeparator := '=';
    LStrListValues.DelimitedText := AParams;

    LIndex := LStrListValues.IndexOfName(AParamName);

    if (LIndex = -1) then
      LMsg := 'Param [' + AParamName + '] not found';

    Result := LStrListValues.ValueFromIndex[LIndex];
  finally
    FreeAndNil(LStrListValues);
  end;

  if not(LMsg.Trim.isEmpty) then
    raise Exception.Create(LMsg);
end;

function TRdwUtils.JsonResultValidate(const AJson: string): string;
var
  LException: Boolean;

  LJsonArr: System.JSON.TJSONArray;
  LJsonValue: System.JSON.TJSONValue;
  LItem: System.JSON.TJSONValue;
  LJson: string;
begin
  try
    LJsonArr := nil;
    LJsonValue := nil;
    LItem := nil;
    LException := False;

    LJson := AJson;
    if not(LeftStr(AJson, 1).Equals('[')) then
      LJson := '[' + LJson + ']';

    // Recebo o JsonArray
    LJsonArr := System.JSON.TJSONObject.ParseJSONValue
      (TEncoding.UTF8.GetBytes(LJson), 0) as System.JSON.TJSONArray;

    if LJsonArr = nil then
      raise Exception.Create('Invalid JSON: ' + LJson);

    for LJsonValue in LJsonArr do
    begin // Percorro tds os arrays
      for LItem in System.JSON.TJSONArray(LJsonValue) do
      begin // Percorro todos os Index pegando os valores
        case AnsiIndexStr((System.JSON.TJSONPair(LItem).JsonString.Value),
          ['Result', // 0
          'Message', // 1
          'Values', // 2
          '']) of
          0:
            begin
              case AnsiIndexStr(UpperCase(System.JSON.TJSONPair(LItem)
                .JsonString.Value), ['OK', // 0
                'ERROR', // 1
                'WARNING', // 2
                '']) of
                1:
                  begin
                    LException := True;
                    Break;
                  end;
              end;
            end;
          1:
            begin
              LJson := System.JSON.TJSONPair(LItem).JsonValue.Value;
              raise Exception.Create(LJson);
            end;
          2:
            begin
              Result := System.JSON.TJSONPair(LItem).JsonValue.ToJSON;
            end;
        end;
      end;

      if (LException) then
        // Ele so entra aqui se teve erro no servidor mas nao teve a MESSAGE
        raise Exception.Create
          ('Erro desconhecido no servidor. Contate o suporte técnico.');
    end;
  finally
    LJsonArr.DisposeOf;
  end;
end;

procedure TRdwUtils.NetHTTPClientAuthEvent(const Sender: TObject;
  AnAuthTarget: TAuthTargetType; const ARealm, AUrl: string;
  var AUserName, APassword: string; var AbortAuth: Boolean;
  var Persistence: TAuthPersistenceType);
begin
  if AnAuthTarget = TAuthTargetType.Server then
  begin
    AUserName := FUserName;
    APassword := FPassWord;
  end;
end;

class function TRdwUtils.SendEvent(ADataModule: TDataModule;
  const AEventName, AContextName, AStrParamList: String): String;
var
  LParamList: TStringList;
begin
  try
    LParamList := nil;

    if not(AStrParamList.Trim.isEmpty) then
    begin
      LParamList := TStringList.Create;
      LParamList.Clear;
      LParamList.NameValueSeparator := '=';
      LParamList.Delimiter := '|';
      LParamList.DelimitedText := AStrParamList;
    end;

    Result := SendEvent(ADataModule, AEventName, AContextName, LParamList);
  finally
    FreeAndNil(LParamList);
  end;
end;

function TRdwUtils.SendEventWithIdHttp(const AUrl, AContentType, ABody: string;
  const ATimeOut: Integer): string;
var
  LIdHTTP: TIdHTTP;
  LParams: TStringList;
begin
  try
    LParams := nil;

    if not(ABody.Trim.isEmpty) then
    begin
      LParams := TStringList.Create;
      LParams.Add(#13 + ABody);
    end;

    LIdHTTP := TIdHTTP.Create(nil);
    if not(AContentType.Trim.isEmpty) then
      LIdHTTP.Request.ContentType := AContentType;
    LIdHTTP.Request.CharSet := 'utf-8';
    LIdHTTP.Request.BasicAuthentication := True;
    LIdHTTP.Request.UserName := UserName;
    LIdHTTP.Request.PassWord := PassWord;
    if (ATimeOut <> -1) then
      LIdHTTP.ReadTimeout := ATimeOut;

    if (ABody.Trim.isEmpty) then
      Result := LIdHTTP.Get(AUrl)
    else
      Result := LIdHTTP.Post(AUrl, LParams);

    Result := JsonResultValidate(Result);
  finally
    FreeAndNil(LIdHTTP);
    FreeAndNil(LParams);
  end;
end;

function TRdwUtils.SendEventWithNetHttp(const AUrl, AContentType: string;
  const ABody: string; const AIsAsynchronous: Boolean;
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
      LResponse := //
        HTTPRequest.Get( //
        AUrl, //
        nil, //
        [TNameValuePair.Create('Content-Type', AContentType)] //
        );
    end
    else
    begin
      LBodyStr := #13 + ABody;
      LBodyStr := ABody;
      LBody := TStringList.Create;
      LBody.Add(LBodyStr);

      LResponse := //
        HTTPRequest.Post( //
        AUrl, //
        LBody, //
        nil, //
        nil, //
        [TNameValuePair.Create('Content-Type', AContentType)] //
        );

      { const AURL: string;
        const ASource: TStrings;
        const AResponseContent: TStream;
        const AEncoding: TEncoding;
        const AHeaders: TNetHeaders }
    end;

    if not(AIsAsynchronous) then
      Result := JsonResultValidate(LResponse.ContentAsString(TEncoding.UTF8));

  finally
    FreeAndNil(LBody);
    FreeAndNil(HttpClient);
    FreeAndNil(HTTPRequest);
  end;
end;

(* function TRdwUtils.SendEventWithRdwClientRest(const AUrl, AContentType,
  ABody: string; const AIsAsynchronous: Boolean): string;
  var
  LDWClientREST: TDWClientREST;
  LBody: TStringList;
  LResponse: TStringStream;
  LResponseStr: string;
  begin
  try
  Result := EmptyStr;
  LResponse := nil;
  LResponse := TStringStream.Create;
  LBody := nil;

  LDWClientREST := TDWClientREST.Create(nil);
  LDWClientREST.UserAgent := 'Mozilla/3.0 (compatible; Indy Library)';
  LDWClientREST.Accept :=
  'text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8';
  LDWClientREST.ContentEncoding := 'multipart/form';
  LDWClientREST.ContentType := AContentType;
  LDWClientREST.UseSSL := False;
  LDWClientREST.SSLVersions := [];
  LDWClientREST.RequestCharset := TEncodeSelect.esUtf8;
  LDWClientREST.RequestTimeOut := 1000000;
  LDWClientREST.AuthOptions.HasAuthentication := True;
  LDWClientREST.AuthOptions.UserName := UserName;
  LDWClientREST.AuthOptions.PassWord := PassWord;

  if (ABody.Trim.isEmpty) then
  LDWClientREST.Get(AUrl, nil, LResponse)
  else
  begin
  LBody := TStringList.Create;
  LBody.Add(#13 + ABody);

  LDWClientREST.Post(AUrl, LBody, LResponse);
  end;

  if not(AIsAsynchronous) then
  begin
  LResponseStr := LResponse.DataString;

  if (LResponseStr.Contains('401 Unauthorized')) then
  raise Exception.Create(LResponseStr);

  Result := JsonResultValidate(LResponseStr);
  end;
  finally
  FreeAndNil(LResponse);
  FreeAndNil(LBody);
  FreeAndNil(LDWClientREST);
  end;
  end; *)

procedure TRdwUtils.SetPassWord(const Value: string);
begin
  FPassWord := Value;
end;

procedure TRdwUtils.SetUserName(const Value: string);
begin
  FUserName := Value;
end;

(* Para que funcione, AStrParams deve ser passado juntamente com os valores
  separador de parametros | separador chave e valor = *)
class function TRdwUtils.VerifyParamsRequired(const AStrParams, AParamsToVerify
  : string; const AShowException: Boolean = True): Boolean;
var
  LStrListValues: TStringList;
  LStrListVerify: TStringList;
  i: Integer;
  LIndex: Integer;
  LMsg: string;
  LValue: string;
begin
  try
    Result := False;
    LMsg := EmptyStr;
    LStrListVerify := TStringList.Create;
    LStrListVerify.Delimiter := '|';
    LStrListVerify.DelimitedText := AParamsToVerify;

    LStrListValues := TStringList.Create;
    LStrListValues.Delimiter := '|';
    LStrListValues.NameValueSeparator := '=';
    LStrListValues.DelimitedText := AStrParams;

    for i := 0 to Pred(LStrListVerify.Count) do
    begin
      LIndex := LStrListValues.IndexOfName(LStrListVerify[i]);
      LValue := LStrListValues.ValueFromIndex[LIndex];

      if (LIndex = -1) or (LValue.Trim.isEmpty) then
      begin
        LMsg := 'Param [' + LStrListVerify[i] + '] is required';
        Break;
      end;
    end;
  finally
    FreeAndNil(LStrListVerify);
    FreeAndNil(LStrListValues);
  end;

  Result := LMsg.Trim.isEmpty;

  if not(Result) and (AShowException) then
    raise Exception.Create(LMsg);
end;

class function TRdwUtils.VerifyParamsRequired(const ADwParams: TDWParams;
  const AParamsToVerify: string; const AShowException: Boolean = True): Boolean;
var
  LStrList: TStringList;
  i: Integer;
  LMsg: string;
begin
  try
    Result := False;
    LMsg := EmptyStr;

    LStrList := TStringList.Create;
    LStrList.Delimiter := '|';
    LStrList.DelimitedText := AParamsToVerify;
    for i := 0 to Pred(LStrList.Count) do
    begin
      if (ADwParams.ItemsString[LStrList[i]] = nil) or
        (ADwParams.ItemsString[LStrList[i]].AsString.isEmpty) then
      begin
        LMsg := 'Value to params [' + LStrList[i] + '] is required.';
        Break;
      end;
    end;
  finally
    FreeAndNil(LStrList);
  end;

  Result := LMsg.Trim.isEmpty;

  if not(Result) and (AShowException) then
    raise Exception.Create(LMsg);
end;

end.
