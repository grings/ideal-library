unit IdeaL.Lib.Api.DropBox;

{ ***
  https://www.dropbox.com/developers/documentation/http/documentation
  *** }

interface

uses
  System.Classes,
  System.SysUtils,
  System.IOUtils,

  System.Net.URLClient,
  System.Net.HttpClient,
  System.Net.HttpClientComponent;

const
  cUrlApiFiles = 'https://api.dropboxapi.com/2/files';
  cUrlContentFiles = 'https://content.dropboxapi.com/2/files';

  // Api
  cEventCreateFolder = 'create_folder_v2';
  cEventDelete = 'delete';
  cEventListFolder = 'list_folder';
  cEventGetMetadata = 'get_metadata';
  // Content
  cEventDownload = 'download';
  cEventUpload = 'upload';

type
  TDownloadStatusUpdate = reference to procedure(const ALength, ACount: Int64);

  TDropBoxApiNetHttp = class
  private
    FNetHttpRequest: TNetHTTPRequest;
    FNetHttpClient: TNetHTTPClient;
    { private declarations }
  protected
    { protected declarations }
  public
    destructor Destroy; override;

    class function GetInstance(const AContentType: string = ''; const AMethodString: string = ''): TDropBoxApiNetHttp;

    property NetHttpRequest: TNetHTTPRequest read FNetHttpRequest;
    property NetHttpClient: TNetHTTPClient read FNetHttpClient;
    { public declarations }
  end;

  TDropBoxApi = class
  private
    FNetHttpRequest: TNetHTTPRequest;
    FNetHttpClient: TNetHTTPClient;
    FAccessToken: string;
    FDownloadStatusUpdate: TDownloadStatusUpdate;
    FRequestCompletedEvent: TRequestCompletedEvent;
    FReadCount: Int64;
    FContentLength: Int64;

    function GetNetHttpRequest(const AContentType: string = ''; const AMethodString: string = ''): TNetHTTPRequest;

    function GetUrlCreateFolder(): string;
    function GetUrlDelete(): string;
    function GetUrlDownload(): string;
    function GetUrlListFolder(): string;
    function GetUrlGetMetadata(): string;
    function GetUrlUpload(): string;

    function GetAccessToken(): string;

    // Library
    procedure ValidateResponse(AResponse: IHTTPResponse);
    function IfThenString(const AValue: Boolean; const AMin, AMax: string): string; overload;
    function IfThenString(const AValue: Boolean): string; overload;
    { private declarations }
  protected
    property ContentLength: Int64 read FContentLength write FContentLength;
    property ReadCount: Int64 read FReadCount write FReadCount;

    procedure DestroyObjs;
    procedure NetHttpReceiveDataEvent(const Sender: TObject; AContentLength: Int64; AReadCount: Int64; var Abort: Boolean);
    procedure NetHttpRequestCompletedEvent(const Sender: TObject; const AResponse: IHTTPResponse);
    { protected declarations }
  public
    class var DropBoxAccessToken: string;

    constructor Create();
    destructor Destroy; override;

    procedure CreateFolder(
      const AServerForlderFullPath: string;
      const AAutoRename: Boolean = False
      );
    procedure Delete( // Delete file or folder
      const AServerForlderFullPath: string
      );
    procedure Download(
      const ALocalFileFullName: string;
      const AServerFileFullName: string
      );
    procedure DownloadRange(
      const ALocalFileFullName: string;
      const AServerFileFullName: string
      );
    function ListFolder(
      const AServerForlderFullPath: string;
      const ARecursive: Boolean = False;
      const AIncludeMediaInfo: Boolean = False;
      const AIncludeDeleted: Boolean = False;
      const AIncludeHasExplicitSharedMembers: Boolean = False;
      const AIncludeMountedFolders: Boolean = True;
      const AIncludeNonDownloadableFiles: Boolean = True
      ): string;
    function GetMetadata(
      const AServerForlderOrFileFullPath: string;
      const AIncludeMediaInfo: Boolean = False;
      const AIncludeDeleted: Boolean = False;
      const AIncludeHasExplicitSharedMembers: Boolean = False
      ): string;
    procedure Upload(
      const ALocalFileFullName: string;
      const AServerFileFullName: string
      );

    property AccessToken: string read GetAccessToken write FAccessToken;

    property OnDownloadStatusUpdate: TDownloadStatusUpdate read FDownloadStatusUpdate write FDownloadStatusUpdate;
    property OnRequestCompletedEvent: TRequestCompletedEvent read FRequestCompletedEvent write FRequestCompletedEvent;
    { public declarations }
  end;

  TDropBoxApiV2 = class
  private
    const
    DROPBOX_API_URL = 'https://api.dropboxapi.com/2/';
    DROPBOX_CONTENT_URL = 'https://content.dropboxapi.com/2/';
    DROPBOX_TOKEN_URL = 'https://api.dropboxapi.com/oauth2/token';
    CContentTypeJson = 'application/json';
    var
    FAccessToken: string;
    FOnDownloadStatusUpdate: TDownloadStatusUpdate;
    FOnRequestCompletedEvent: TRequestCompletedEvent;
    FContentLength: Int64; // For DownloadRange
    FReadCount: Int64; // For DownloadRange
    { private declarations }
  protected
    function ExecutePostRequest(
      const AUrl: string;
      const AParams: TStrings;
      const AContentType: string;
      AHeaders: TNetHeaders): string; overload;
    function ExecutePostRequest(
      const AUrl: string;
      const AParams: TStrings;
      const AContentType: string;
      AHeaders: TNetHeaders;
      out ARespStream: TStream): string; overload;

    // http events
    procedure NetHttpReceiveDataEvent(const Sender: TObject; AContentLength: Int64; AReadCount: Int64; var Abort: Boolean);
    procedure NetHttpRequestCompletedEvent(const Sender: TObject; const AResponse: IHTTPResponse);
    { protected declarations }
  public
    constructor Create;

    function SetAccessToken(AValue: string): TDropBoxApiV2;
    function SetOnDownloadStatusUpdate(AValue: TDownloadStatusUpdate): TDropBoxApiV2;
    function SetOnRequestCompletedEvent(AValue: TRequestCompletedEvent): TDropBoxApiV2;

    procedure Download(
      const ALocalFileFullName: string;
      const AServerFileFullName: string);

    function ListFiles(const APath: string): string;

    property ContentLength: Int64 read FContentLength;
    property ReadCount: Int64 read FReadCount;
    { public declarations }
  end;

implementation

uses
  REST.Types,
  System.NetEncoding,
  System.JSON;

{ TDropBox }

constructor TDropBoxApi.Create();
begin
  inherited;
  FAccessToken := EmptyStr;
  FDownloadStatusUpdate := nil;
  FRequestCompletedEvent := nil;
  FNetHttpRequest := nil;
  FNetHttpClient := nil;
  ContentLength := 0;
  ReadCount := 0;
end;

procedure TDropBoxApi.CreateFolder(const AServerForlderFullPath: string;
  const AAutoRename: Boolean);
var
  LJson: string;
  LAutoRename: string;
  LResponse: IHTTPResponse;
  JsonToSend: TStringStream;
begin
  try
    LAutoRename := 'false';
    GetNetHttpRequest('application/json', 'POST');
    if AAutoRename then
      LAutoRename := 'true';

    LJson :=
      '{' +
      '"path": "' + AServerForlderFullPath + '"' +
      ', "autorename": ' + IfThenString(AAutoRename) +
      '}'
      ;

    JsonToSend := TStringStream.Create(UTF8Encode(LJson));

    ValidateResponse(
      FNetHttpRequest.Post(
      GetUrlCreateFolder,
      JsonToSend
      ));
  finally
    FreeAndNil(JsonToSend);
  end;
end;

procedure TDropBoxApi.Delete(const AServerForlderFullPath: string);
var
  LJson: string;
  LResponse: IHTTPResponse;
  JsonToSend: TStringStream;
begin
  try
    GetNetHttpRequest('application/json', 'POST');

    LJson :=
      '{' +
      '"path": "' + AServerForlderFullPath + '"' +
      '}'
      ;

    JsonToSend := TStringStream.Create(UTF8Encode(LJson));

    ValidateResponse(
      FNetHttpRequest.Post(
      GetUrlDelete,
      JsonToSend
      ));
  finally
    FreeAndNil(JsonToSend);
  end;
end;

destructor TDropBoxApi.Destroy;
begin
  DestroyObjs;
  inherited;
end;

procedure TDropBoxApi.DestroyObjs;
begin
  if (Assigned(FNetHttpClient)) then
    FreeAndNil(FNetHttpClient);
  if (Assigned(FNetHttpRequest)) then
    FreeAndNil(FNetHttpRequest);
end;

procedure TDropBoxApi.Download(const ALocalFileFullName,
  AServerFileFullName: string);
var
  LStrResp: TMemoryStream;
  LResponse: IHTTPResponse;
begin
  LStrResp := TMemoryStream.Create;
  try
    GetNetHttpRequest;
    FNetHttpRequest.CustomHeaders['Dropbox-API-Arg'] := Format('{"path": "%s"}', [AServerFileFullName]);
    LResponse := FNetHttpRequest.Get(GetUrlDownload, LStrResp);
    ValidateResponse(LResponse);
    LStrResp.Position := 0;

    TThread.Synchronize(nil,
      procedure()
      begin
        LStrResp.SaveToFile(ALocalFileFullName);

        if not FileExists(ALocalFileFullName) then
         raise Exception.Create(
          'TDropBoxApi.Download couldn''t save the file : ' + ALocalFileFullName + sLineBreak +
          'Check if you have the right permissions'
          );
      end);

  finally
    FreeAndNil(LStrResp);
  end;
end;

procedure TDropBoxApi.DownloadRange(const ALocalFileFullName,
  AServerFileFullName: string);
const
  C1MB = 999999; // 1000000;

var
  LName: string;
  LNameDb: string;
  LJson: string;
  LSizeDb: Int64;
  LSize: Int64;
  LStart: Int64;
  LEnd: Int64;
  LJsonObj: System.Json.TJSONObject;

  LStrResp: TMemoryStream;
  LResponse: IHTTPResponse;

  LStrmDest: TStream;

  LOnRequestCompletedEvent: TRequestCompletedEvent;
begin
  // For Donwload Range we don't need CompletedEvent to be called every time
  LOnRequestCompletedEvent := FRequestCompletedEvent;
  OnRequestCompletedEvent := nil;

  var
  LPath := StringReplace(AServerFileFullName, '/', '\', [rfReplaceAll]);
  LName := ExtractFileName(LPath);
  //LPath := ExtractFilePath(LPath);
  LPath := StringReplace(LPath, '\', '/', [rfReplaceAll]);
  LJson := GetMetadata(LPath);

  // the documentation says, it expects a JSON Object
  LJsonObj := System.Json.TJSONObject.ParseJSONValue(TEncoding.UTF8.GetBytes(LJson), 0) as System.Json.TJSONObject;
  try
    try
      if (not LJsonObj.TryGetValue<string>('name', LNameDb)) or
        (not LJsonObj.TryGetValue<Int64>('size', LSizeDb))
      then
        raise Exception.Create('Rasing anything, the TryExcept bellow must delegate it');
    except
      raise Exception.Create('TDropBoxApi.DownloadRange could not find the key "name" or "size" from GetMetadata on DropBox JSON result');
    end;
  finally
    FreeAndNil(LJsonObj);
  end;

  LStrmDest := nil;
  LStrResp := TMemoryStream.Create;
  try
    LStrResp.Position := 0;
    GetNetHttpRequest;
    FNetHttpRequest.CustomHeaders['Dropbox-API-Arg'] := Format('{"path": "%s"}', [AServerFileFullName]);
    ContentLength := LSizeDb;
    ReadCount := 0;
    LStart := 0;
    LEnd := 0;

    while LEnd < LSizeDb do
    begin
      LEnd := LEnd + C1MB;
      if LEnd > LSizeDb then
        LEnd := LSizeDb;
      LStrResp.Clear;
      LStrResp.Position := 0;
      LResponse := FNetHttpRequest.GetRange(GetUrlDownload, LStart, LEnd, LStrResp);
      ValidateResponse(LResponse);
      LStart := LEnd + 1;
      LStrResp.Position := 0;

      if FileExists(ALocalFileFullName) then
      begin
        LStrmDest := TFileStream.Create(ALocalFileFullName, fmOpenReadWrite);
        LStrmDest.Seek(0, soEnd);
      end
      else
      begin
        LStrmDest := TFileStream.Create(ALocalFileFullName, fmCreate);
        LStrmDest.Position := 0;
      end;
      LStrmDest.CopyFrom(LStrResp, 0);
      FreeAndNil(LStrmDest);

      ReadCount := ReadCount + LStrResp.Size;
    end;
  finally
    LStrmDest.Free;
    LStrResp.Free;
    if Assigned(LOnRequestCompletedEvent) then
      LOnRequestCompletedEvent(Self, LResponse);
  end;
end;

function TDropBoxApi.GetAccessToken: string;
begin
  Result := FAccessToken;
  if (Result.Trim.IsEmpty) then
    Result := DropBoxAccessToken;
end;

function TDropBoxApi.GetMetadata(const AServerForlderOrFileFullPath: string;
  const AIncludeMediaInfo, AIncludeDeleted,
  AIncludeHasExplicitSharedMembers: Boolean): string;
var
  LJson: string;
  JsonToSend: TStringStream;
  LResponse: IHTTPResponse;
begin
  try
    Result := EmptyStr;
    GetNetHttpRequest('application/json', 'POST');

    LJson :=
      '{' +
      '"path": "' + AServerForlderOrFileFullPath + '"' +
      ',"include_media_info": ' + IfThenString(AIncludeMediaInfo) +
      ',"include_deleted": ' + IfThenString(AIncludeDeleted) +
      ',"include_has_explicit_shared_members": ' +
      IfThenString(AIncludeHasExplicitSharedMembers) +
      '}'
      ;

    JsonToSend := TStringStream.Create(UTF8Encode(LJson));

    LResponse :=
      FNetHttpRequest.Post(
      GetUrlGetMetadata,
      JsonToSend
      );
    ValidateResponse(LResponse);
    Result := LResponse.ContentAsString();
  finally
    FreeAndNil(JsonToSend);
  end;
end;

function TDropBoxApi.GetNetHttpRequest(const AContentType,
  AMethodString: string): TNetHTTPRequest;
begin
  DestroyObjs;

  FNetHttpClient := TNetHTTPClient.Create(nil);
  FNetHttpClient.Accept := '*/*';
  FNetHttpClient.AcceptCharSet := 'utf-8';
  FNetHttpClient.AcceptEncoding := 'gzip, deflate, br';
  FNetHttpClient.ContentType := AContentType;
  FNetHttpClient.UserAgent :=
    'Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 6.0; Acoo Browser; ' +
    'GTB5; Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.1; SV1) ; ' +
    'Maxthon; InfoPath.1; .NET CLR 3.5.30729; .NET CLR 3.0.30618)';

  FNetHttpRequest := TNetHTTPRequest.Create(nil);
  FNetHttpRequest.OnReceiveData := NetHttpReceiveDataEvent;
  FNetHttpRequest.OnSendData := NetHttpReceiveDataEvent;
  FNetHttpRequest.OnRequestCompleted := NetHttpRequestCompletedEvent;
  FNetHttpRequest.Client := FNetHttpClient;
  FNetHttpRequest.CustomHeaders['Authorization'] := 'Bearer ' + AccessToken;
  FNetHttpRequest.Accept := '*/*';
  FNetHttpRequest.AcceptCharSet := 'utf-8';
  FNetHttpRequest.AcceptEncoding := 'gzip, deflate, br';
  FNetHttpRequest.MethodString := AMethodString;

  Result := FNetHttpRequest;
end;

function TDropBoxApi.GetUrlCreateFolder: string;
begin
  Result := cUrlApiFiles + '/' + cEventCreateFolder;
end;

function TDropBoxApi.GetUrlDelete: string;
begin
  Result := cUrlApiFiles + '/' + cEventDelete;
end;

function TDropBoxApi.GetUrlDownload: string;
begin
  Result := cUrlContentFiles + '/' + cEventDownload;
end;

function TDropBoxApi.GetUrlGetMetadata: string;
begin
  Result := cUrlApiFiles + '/' + cEventGetMetadata;
end;

function TDropBoxApi.GetUrlListFolder: string;
begin
  Result := cUrlApiFiles + '/' + cEventListFolder;
end;

function TDropBoxApi.GetUrlUpload: string;
begin
  Result := cUrlContentFiles + '/' + cEventUpload;
end;

function TDropBoxApi.IfThenString(const AValue: Boolean): string;
begin
  Result := IfThenString(AValue, 'true', 'false');
end;

function TDropBoxApi.IfThenString(const AValue: Boolean;
const AMin, AMax: string): string;
begin
  if AValue then
    Result := AMin
  else
    Result := AMax;
end;

function TDropBoxApi.ListFolder(const AServerForlderFullPath: string;
const ARecursive, AIncludeMediaInfo, AIncludeDeleted,
  AIncludeHasExplicitSharedMembers, AIncludeMountedFolders,
  AIncludeNonDownloadableFiles: Boolean): string;
var
  LJson: string;
  JsonToSend: TStringStream;
  LResponse: IHTTPResponse;
begin
  try
    Result := EmptyStr;
    GetNetHttpRequest('application/json', 'POST');

    LJson :=
      '{' +
      '"path": "' + AServerForlderFullPath + '"' +
      ',"recursive": ' + IfThenString(ARecursive) +
      ',"include_media_info": ' + IfThenString(AIncludeMediaInfo) +
      ',"include_deleted": ' + IfThenString(AIncludeDeleted) +
      ',"include_has_explicit_shared_members": ' +
      IfThenString(AIncludeHasExplicitSharedMembers) +
      ',"include_mounted_folders": ' + IfThenString(AIncludeMountedFolders) +
      ',"include_non_downloadable_files": ' +
      IfThenString(AIncludeNonDownloadableFiles) +
      '}'
      ;

    JsonToSend := TStringStream.Create(UTF8Encode(LJson));

    LResponse :=
      FNetHttpRequest.Post(
      GetUrlListFolder,
      JsonToSend
      );
    ValidateResponse(LResponse);
    Result := LResponse.ContentAsString();
  finally
    FreeAndNil(JsonToSend);
  end;
end;

procedure TDropBoxApi.NetHttpReceiveDataEvent(const Sender: TObject;
AContentLength, AReadCount: Int64; var Abort: Boolean);
var
  LContentLength: Int64;
  LReadCount: Int64;
begin
  if ContentLength > 0 then
  begin
    LContentLength := ContentLength;
    LReadCount := ReadCount + AReadCount;
  end
  else
  begin
    LContentLength := AContentLength;
    LReadCount := AReadCount;
  end;

  if (Assigned(FDownloadStatusUpdate)) then
    FDownloadStatusUpdate(LContentLength, LReadCount);
end;

procedure TDropBoxApi.NetHttpRequestCompletedEvent(const Sender: TObject;
const AResponse: IHTTPResponse);
begin
  if (Assigned(FRequestCompletedEvent)) then
    FRequestCompletedEvent(Sender, AResponse);
end;

procedure TDropBoxApi.Upload(const ALocalFileFullName,
  AServerFileFullName: string);
var
  LStrResp: TFileStream;
  LResponse: IHTTPResponse;
begin
  try
    LStrResp :=
      TFileStream.Create(ALocalFileFullName, fmOpenRead or fmShareDenyWrite);

    GetNetHttpRequest();
    FNetHttpRequest.CustomHeaders['Dropbox-API-Arg'] :=
      Format('{"path": "%s"}', [AServerFileFullName]);
    FNetHttpRequest.Client.ContentType := 'application/octet-stream';
    LResponse := FNetHttpRequest.Post(GetUrlUpload, LStrResp);
    ValidateResponse(LResponse);
  finally
    FreeAndNil(LStrResp);
  end;
end;

procedure TDropBoxApi.ValidateResponse(AResponse: IHTTPResponse);
begin
  If (AResponse.StatusCode < 200) or (AResponse.StatusCode > 299) Then
    raise Exception.Create(
      'Error: ' + AResponse.StatusCode.ToString + ' ' + AResponse.StatusText);
end;

{ TDropBoxApiV2 }

constructor TDropBoxApiV2.Create;
begin
  FOnDownloadStatusUpdate := nil;
  FOnRequestCompletedEvent := nil;
  FContentLength := 0;
  FReadCount := 0;
end;

procedure TDropBoxApiV2.Download(const ALocalFileFullName,
  AServerFileFullName: string);
begin
  var
  LJObj := TJSONObject.Create;
  var
  LJson := EmptyStr;
  try
    LJObj.AddPair('path', AServerFileFullName);
    LJson := LJObj.ToString;
  finally
    LJObj.Free;
  end;
  var
  LHeaders : TNetHeaders := [TNetHeader.Create('Dropbox-API-Arg', LJson)];
  var
  LStrm : TStream := TMemoryStream.Create;
  try
    ExecutePostRequest(DROPBOX_CONTENT_URL + 'files/download', nil, EmptyStr, LHeaders, LStrm);
    LStrm.Position := 0;
    TThread.Synchronize(nil,
      procedure()
      begin
        TMemoryStream(LStrm).SaveToFile(ALocalFileFullName);

        if not FileExists(ALocalFileFullName) then
         raise Exception.Create(
          'TDropBoxApi.Download couldn''t save the file : ' + ALocalFileFullName + sLineBreak +
          'Check if you have the right permissions'
          );
      end);
  finally
    FreeAndNil(LStrm);
  end;
end;

function TDropBoxApiV2.ExecutePostRequest(const AUrl: string;
  const AParams: TStrings; const AContentType: string;
  AHeaders: TNetHeaders): string;
begin
  var
  LStrm : TStream := nil;
  try
    ExecutePostRequest(AUrl, AParams, AContentType, AHeaders, LStrm);
  finally
    FreeAndNil(LStrm);
  end;
end;

function TDropBoxApiV2.ExecutePostRequest(const AUrl: string;
  const AParams: TStrings; const AContentType: string;
  AHeaders: TNetHeaders; out ARespStream: TStream): string;
begin
  if FAccessToken.Trim.IsEmpty then
    raise Exception.Create('Access token can not be empty');

  var
  LReq := TDropBoxApiNetHttp.GetInstance(AContentType, 'POST');
  try
    LReq.NetHttpRequest.CustHeaders.Add('Authorization', 'Bearer ' + FAccessToken);
    LReq.NetHttpRequest.OnReceiveData := NetHttpReceiveDataEvent;
    LReq.NetHttpRequest.OnSendData := NetHttpReceiveDataEvent;
    LReq.NetHttpRequest.OnRequestCompleted := NetHttpRequestCompletedEvent;
    if Length(AHeaders) > 0 then
    begin
      for var i := Low(AHeaders) to High(AHeaders) do
        LReq.NetHttpRequest.CustHeaders.Add(AHeaders[i].Name, AHeaders[i].Value);
    end;

    var
    LReqStream : TStringStream := nil;
    if (Assigned(AParams)) and (not AParams.Text.Trim.IsEmpty) then
      LReqStream := TStringStream.Create(AParams.Text.Trim);
    var
    LResponseStream := TStringStream.Create;
    try
      var
      LResponse :=
          LReq.NetHttpRequest.Post(
          AUrl,
          LReqStream,
          ARespStream
          );
      if IsTextualContentType(LResponse.HeaderValue['Content-Type']) then
        Result := LResponse.ContentAsString();
    finally
      LReqStream.Free;
      LResponseStream.Free;
    end;
  finally
    LReq.Free;
  end;
end;

function TDropBoxApiV2.ListFiles(const APath: string): string;
var
  LParams: TStrings;
  LJson: TJSONObject;
begin
  LParams := TStringList.Create;
  try
    LJson := TJSONObject.Create;
    try
      LJson.AddPair('include_deleted', False);
      LJson.AddPair('include_has_explicit_shared_members', False);
      LJson.AddPair('include_media_info', False);
      LJson.AddPair('include_mounted_folders', True);
      LJson.AddPair('path', APath);
      LJson.AddPair('recursive', False);
      LParams.Add(LJson.ToString);
    finally
      LJson.Free;
    end;
    Result := ExecutePostRequest(DROPBOX_API_URL + 'files/list_folder', LParams, CContentTypeJson, []);
  finally
    LParams.Free;
  end;
end;

procedure TDropBoxApiV2.NetHttpReceiveDataEvent(const Sender: TObject;
  AContentLength, AReadCount: Int64; var Abort: Boolean);
var
  LContentLength: Int64;
  LReadCount: Int64;
begin
  if ContentLength > 0 then
  begin
    LContentLength := ContentLength;
    LReadCount := ReadCount + AReadCount;
  end
  else
  begin
    LContentLength := AContentLength;
    LReadCount := AReadCount;
  end;

  if (Assigned(FOnDownloadStatusUpdate)) then
    FOnDownloadStatusUpdate(LContentLength, LReadCount);
end;

procedure TDropBoxApiV2.NetHttpRequestCompletedEvent(const Sender: TObject;
  const AResponse: IHTTPResponse);
begin
  if Assigned(FOnRequestCompletedEvent) then
    FOnRequestCompletedEvent(Sender, AResponse);
end;

function TDropBoxApiV2.SetAccessToken(AValue: string): TDropBoxApiV2;
begin
  Result := Self;
  FAccessToken := AVAlue;
end;

function TDropBoxApiV2.SetOnDownloadStatusUpdate(
  AValue: TDownloadStatusUpdate): TDropBoxApiV2;
begin
  Result := Self;
  FOnDownloadStatusUpdate := AValue;
end;

function TDropBoxApiV2.SetOnRequestCompletedEvent(
  AValue: TRequestCompletedEvent): TDropBoxApiV2;
begin
  Result := Self;
  FOnRequestCompletedEvent := AValue;
end;

{ TDropBoxApiNetHttp }

destructor TDropBoxApiNetHttp.Destroy;
begin
  FreeAndNil(FNetHttpRequest);
  FreeAndNil(FNetHttpClient);
  inherited;
end;

class function TDropBoxApiNetHttp.GetInstance(const AContentType,
  AMethodString: string): TDropBoxApiNetHttp;
begin
  Result := TDropBoxApiNetHttp.Create;

  Result.FNetHttpClient := TNetHTTPClient.Create(nil);
  Result.FNetHttpClient.Accept := '*/*';
  Result.FNetHttpClient.AcceptCharSet := 'utf-8';
  Result.FNetHttpClient.AcceptEncoding := 'gzip, deflate, br';
  Result.FNetHttpClient.ContentType := AContentType;
  Result.FNetHttpClient.UserAgent :=
    'Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 6.0; Acoo Browser; ' +
    'GTB5; Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.1; SV1) ; ' +
    'Maxthon; InfoPath.1; .NET CLR 3.5.30729; .NET CLR 3.0.30618)';

  Result.FNetHttpRequest := TNetHTTPRequest.Create(nil);
  // FNetHttpRequest.OnReceiveData := NetHttpReceiveDataEvent;
  // FNetHttpRequest.OnSendData := NetHttpReceiveDataEvent;
  // FNetHttpRequest.OnRequestCompleted := NetHttpRequestCompletedEvent;
  Result.FNetHttpRequest.Client := Result.FNetHttpClient;
  if not AContentType.Trim.IsEmpty then
    Result.FNetHttpRequest.CustHeaders.Add('Content-Type', AContentType);
  Result.FNetHttpRequest.Accept := '*/*';
  Result.FNetHttpRequest.AcceptCharSet := 'utf-8';
  Result.FNetHttpRequest.AcceptEncoding := 'gzip, deflate, br';
  Result.FNetHttpRequest.MethodString := 'POST';
end;

end.