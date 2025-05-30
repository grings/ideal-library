unit IdeaL.Lib.DownloadUpload;

interface

uses
  System.Classes,
  System.SysUtils,
  System.IOUtils,

  // **Para Donwload com NetHttp
  System.Net.URLClient,
  System.Net.HttpClient,
  System.Net.HttpClientComponent,

  // **Para Donwload com Indy
  IdComponent,
  IdHTTP,
  IdSSLOpenSSL,

  // **Para funcionar no Android
  // Tbm sera necessario baixar a OpenSSL1.2h e adicionar no Deployment .\assets\internal\
  // IdSSLOpenSSL,
  IdSSLOpenSSLHeaders;

type
  TDoDownloadUploadWith = (duIdHttp, duNetHttp);

  TDownloadUpload = class
  private
    { private declarations }
    function DownloadIdHttp(const AUrl: string): TMemoryStream;

    procedure IdHTTPWorkBegin(ASender: TObject; AWorkMode: TWorkMode;
      AWorkCountMax: Int64);
    procedure IdHTTPWork(ASender: TObject; AWorkMode: TWorkMode;
      AWorkCount: Int64);
  protected
    { protected declarations }
  public
    { public declarations }
    function DonwloadToStream(const ADoWith: TDoDownloadUploadWith;
      const AUrl: string): TMemoryStream;
  published
    { published declarations }
  end;

implementation

{ TDownloadUpload }

function TDownloadUpload.DonwloadToStream(const ADoWith
  : TDoDownloadUploadWith; const AUrl: string): TMemoryStream;
var
  LStrResp: TMemoryStream;
begin
  LStrResp := nil;
  case ADoWith of
    duIdHttp:
      LStrResp := DownloadIdHttp(AUrl);
    duNetHttp:
      ;
  end;

  Result := LStrResp;
end;

function TDownloadUpload.DownloadIdHttp(const AUrl: string)
  : TMemoryStream;
var
  LIdHttp: TIdHTTP;
  LStrResp: TMemoryStream;
begin
  try
    Result := nil;
{$IFDEF ANDROID}
    IdOpenSSLSetLibPath(System.IOUtils.TPath.GetDocumentsPath);
{$ENDIF}
    LStrResp := TMemoryStream.Create;
    LIdHttp := TIdHTTP.Create(nil);
    LIdHttp.AllowCookies := False;
    LIdHttp.HandleRedirects := True;
    LIdHttp.OnWork := IdHTTPWork;
    LIdHttp.OnWorkBegin := IdHTTPWorkBegin;

    { Inicio o Donwload caso encontre o arquivo no For anterior }

    LIdHttp.Get(AUrl, LStrResp);

    LStrResp.Position := 0;
    Result := LStrResp;
    { If LIdHttp.ResponseCode = 200 Then
      begin
      LStrResp.SaveToFile(ALocalFilePath);
      end; }
  finally
    // FreeAndNil(LStrResp);
    LIdHttp.Disconnect();
    FreeAndNil(LIdHttp);
  end;
end;

procedure TDownloadUpload.IdHTTPWork(ASender: TObject; AWorkMode: TWorkMode;
  AWorkCount: Int64);
begin

end;

procedure TDownloadUpload.IdHTTPWorkBegin(ASender: TObject;
  AWorkMode: TWorkMode; AWorkCountMax: Int64);
begin

end;

end.
