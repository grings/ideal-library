unit IdeaL.Lib.Download.DropBox;

interface

uses
  System.Classes,
  System.SysUtils,
  System.IOUtils,

  // **Para Donwload do DropBox com NetHttp
  System.Net.URLClient,
  System.Net.HttpClient,
  System.Net.HttpClientComponent,

  // **Para Donwload do DropBox com Indy
  IdComponent,
  IdHTTP,
  IdSSLOpenSSL,

  // **Para funcionar no Android
  // Tbm sera necessario baixar a OpenSSL1.2h e adicionar no Deployment .\assets\internal\
  // IdSSLOpenSSL,
  IdSSLOpenSSLHeaders;

type
  TDownloadStatusUpdate = reference to procedure(const ALength, ACount: Int64);
  TDoDownloadUploadWith = (dbIdHttp, dbNetHttp);

  TDownloadDropBox = class
  private
  { private declarations }
    const
    cUrlDownload = 'https://content.dropboxapi.com/2/files/download';

  const
    cUrlUpload = 'https://content.dropboxapi.com/2/files/upload';

  var
    FFileLength: Int64;
    FFileCount: Int64;
    FDownloadStatusUpdate: TDownloadStatusUpdate;
    FNetHttpRequestCompletedEvent: TRequestCompletedEvent;

    procedure IdHTTPWorkBegin(ASender: TObject; AWorkMode: TWorkMode;
      AWorkCountMax: Int64);
    procedure IdHTTPWork(ASender: TObject; AWorkMode: TWorkMode;
      AWorkCount: Int64);

    procedure NetHttpReceiveAudioEvent(const Sender: TObject;
      AContentLength: Int64; AReadCount: Int64; var Abort: Boolean);
    procedure NetHttpRequestCompletedEvent(const Sender: TObject;
      const AResponse: IHTTPResponse);
    procedure SetNetHttpRequestCompletedEvent(const Value
      : TRequestCompletedEvent);
  protected
    { protected declarations }
    procedure DowloadWithIdHttp(const AAccessToken, ALocalFilePath,
      AServerFileName: string);
    procedure UploadtWithIdHttp(const AAccessToken, AFilePath,
      AServerFileName: string);

    procedure DowloadWithNetHttp(const AAccessToken, ALocalFilePath,
      AServerFileName: string);
    procedure UploadtWithNetHttp(const AAccessToken, AFilePath,
      AServerFileName: string);

  public
    { public declarations }
    constructor Create(); // override;
    procedure Dowload(const ADownloadWith: TDoDownloadUploadWith;
      const AAccessToken, ALocalFilePath, AServerFileName: string;
      const ADownloadStatusUpdate: TDownloadStatusUpdate = nil);
    procedure Upload(const ADownloadWith: TDoDownloadUploadWith;
      const AAccessToken, ALocalFilePath, AServerFileName: string);

    property OnNetHttpRequestCompletedEvent: TRequestCompletedEvent
      read FNetHttpRequestCompletedEvent write SetNetHttpRequestCompletedEvent;

  published
    { published declarations }
  end;

implementation

{ TDownloadDropBox }

constructor TDownloadDropBox.Create();
begin
  // inherited;
  FDownloadStatusUpdate := nil;
end;

procedure TDownloadDropBox.Dowload(const ADownloadWith: TDoDownloadUploadWith;
  const AAccessToken, ALocalFilePath, AServerFileName: string;
  const ADownloadStatusUpdate: TDownloadStatusUpdate = nil);
begin
  if (Assigned(ADownloadStatusUpdate)) then
    FDownloadStatusUpdate := ADownloadStatusUpdate;
  case ADownloadWith of
    dbIdHttp:
      DowloadWithIdHttp(AAccessToken, ALocalFilePath, AServerFileName);
    dbNetHttp:
      DowloadWithNetHttp(AAccessToken, ALocalFilePath, AServerFileName);
  end;
end;

procedure TDownloadDropBox.DowloadWithIdHttp(const AAccessToken, ALocalFilePath,
  AServerFileName: string);
var
  LIdHttp: TIdHTTP;
  LStrResp: TMemoryStream;
begin
  try
{$IFDEF ANDROID}
    IdOpenSSLSetLibPath(System.IOUtils.TPath.GetDocumentsPath);
{$ENDIF}
    LStrResp := TMemoryStream.Create;
    LIdHttp := TIdHTTP.Create(nil);
    LIdHttp.AllowCookies := False;
    LIdHttp.HandleRedirects := True;
    LIdHttp.OnWork := IdHTTPWork;
    LIdHttp.OnWorkBegin := IdHTTPWorkBegin;

    LIdHttp.Request.CustomHeaders.Values['Authorization'] := 'Bearer ' +
      AAccessToken;
    LIdHttp.Request.CustomHeaders.Values['Dropbox-API-Arg'] :=
      Format('{"path": "%s"}', ['/' + AServerFileName]);
    LIdHttp.Request.ContentType := '';

    { Inicio o Donwload caso encontre o arquivo no For anterior }

    LIdHttp.Get(cUrlDownload, LStrResp);

    LStrResp.Position := 0;
    If LIdHttp.ResponseCode = 200 Then
    begin
      LStrResp.SaveToFile(ALocalFilePath);
    end;
  finally
    FreeAndNil(LStrResp);
    LIdHttp.Disconnect();
    FreeAndNil(LIdHttp);
  end;
end;

procedure TDownloadDropBox.DowloadWithNetHttp(const AAccessToken,
  ALocalFilePath, AServerFileName: string);
var
  LNetHttpRequest: TNetHTTPRequest;
  LNetHttpClient: TNetHTTPClient;
  LStrResp: TMemoryStream;
begin
  try
    LStrResp := TMemoryStream.Create;
    LNetHttpClient := TNetHTTPClient.Create(nil);
    LNetHttpRequest := TNetHTTPRequest.Create(nil);
    LNetHttpRequest.OnReceiveData := NetHttpReceiveAudioEvent;
    LNetHttpRequest.OnRequestCompleted := NetHttpRequestCompletedEvent;
    LNetHttpRequest.Client := LNetHttpClient;
    LNetHttpRequest.CustomHeaders['Authorization'] := 'Bearer ' + AAccessToken;
    LNetHttpRequest.CustomHeaders['Dropbox-API-Arg'] :=
      Format('{"path": "%s"}', ['/' + AServerFileName]);
    LNetHttpRequest.Get(cUrlDownload, LStrResp);
    LStrResp.Position := 0;

    // If vNetHttpRequest.ToString = 200 Then
    begin
      TThread.Synchronize(nil,
        procedure()
        begin
          LStrResp.SaveToFile(ALocalFilePath);
        end);
    end;
  finally
    FreeAndNil(LStrResp);
    FreeAndNil(LNetHttpRequest);
    FreeAndNil(LNetHttpClient);
  end;
end;

procedure TDownloadDropBox.IdHTTPWork(ASender: TObject; AWorkMode: TWorkMode;
AWorkCount: Int64);
begin
  if (Assigned(FDownloadStatusUpdate)) then
    FDownloadStatusUpdate(FFileLength, AWorkCount);
end;

procedure TDownloadDropBox.IdHTTPWorkBegin(ASender: TObject;
AWorkMode: TWorkMode; AWorkCountMax: Int64);
begin
  FFileLength := TIdHTTP(ASender).Response.ContentLength;

  if (FFileLength <= 0) then
    raise Exception.Create('File not found to Download');
end;

procedure TDownloadDropBox.NetHttpReceiveAudioEvent(const Sender: TObject;
AContentLength, AReadCount: Int64; var Abort: Boolean);
begin
  if (Assigned(FDownloadStatusUpdate)) then
    FDownloadStatusUpdate(AContentLength, AReadCount);
end;

procedure TDownloadDropBox.NetHttpRequestCompletedEvent(const Sender: TObject;
const AResponse: IHTTPResponse);
begin
  if (Assigned(FNetHttpRequestCompletedEvent)) then
    FNetHttpRequestCompletedEvent(Sender, AResponse);
end;

procedure TDownloadDropBox.SetNetHttpRequestCompletedEvent
  (const Value: TRequestCompletedEvent);
begin
  FNetHttpRequestCompletedEvent := Value;
end;

procedure TDownloadDropBox.Upload(const ADownloadWith: TDoDownloadUploadWith;
const AAccessToken, ALocalFilePath, AServerFileName: string);
begin
  case ADownloadWith of
    dbIdHttp:
      UploadtWithNetHttp(AAccessToken, ALocalFilePath, AServerFileName);
    dbNetHttp:
      UploadtWithNetHttp(AAccessToken, ALocalFilePath, AServerFileName);
  end;
end;

procedure TDownloadDropBox.UploadtWithIdHttp(const AAccessToken, AFilePath,
  AServerFileName: string);
var
  LIdHttp: TIdHTTP;
  LStrResp: TFileStream;
begin
  try
{$IFDEF ANDROID}
    IdOpenSSLSetLibPath(System.IOUtils.TPath.GetDocumentsPath);
{$ENDIF}
    LStrResp := TFileStream.Create(AFilePath, fmOpenRead or fmShareDenyWrite);

    LIdHttp := TIdHTTP.Create(nil);
    LIdHttp.AllowCookies := False;
    LIdHttp.HandleRedirects := True;
    //LIdHttp.OnWork := IdHTTPWork;
    //LIdHttp.OnWorkBegin := IdHTTPWorkBegin;

    LIdHttp.Request.CustomHeaders.Values['Authorization'] := 'Bearer ' +
      AAccessToken;
    LIdHttp.Request.CustomHeaders.Values['Dropbox-API-Arg'] :=
      Format('{"path": "%s"}', ['/' + AServerFileName]);
    LIdHttp.Request.ContentType := 'application/octet-stream';

    LIdHttp.Post(cUrlDownload, LStrResp);
  finally
    FreeAndNil(LStrResp);
    LIdHttp.Disconnect();
    FreeAndNil(LIdHttp);
  end;
end;

procedure TDownloadDropBox.UploadtWithNetHttp(const AAccessToken, AFilePath,
  AServerFileName: string);
var
  LNetHttpRequest: TNetHTTPRequest;
  LNetHttpClient: TNetHTTPClient;
  LStrResp: TFileStream;
begin
  try
    LStrResp := TFileStream.Create(AFilePath, fmOpenRead or fmShareDenyWrite);
    LNetHttpClient := TNetHTTPClient.Create(nil);
    LNetHttpRequest := TNetHTTPRequest.Create(nil);
    LNetHttpRequest.Client := LNetHttpClient;
    LNetHttpRequest.OnRequestCompleted := NetHttpRequestCompletedEvent;
    LNetHttpRequest.CustomHeaders['Authorization'] := 'Bearer ' + AAccessToken;
    LNetHttpRequest.CustomHeaders['Dropbox-API-Arg'] :=
      Format('{"path": "%s"}', ['/' + AServerFileName]);
    LNetHttpRequest.Client.ContentType := 'application/octet-stream';
    LNetHttpRequest.Post(cUrlUpload, LStrResp);
  finally
    FreeAndNil(LStrResp);
    FreeAndNil(LNetHttpRequest);
    FreeAndNil(LNetHttpClient)
  end;
end;

end.
