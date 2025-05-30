unit Unit1;

{
  Project was built on Delphi 11.3 native, without any work around for Android13+
  
  Thanks for Georges, for being so patient and sharing the NetHttp liberary repository 
  https://github.com/Khorkhe

  NetHttp:
  -- The sample was taken from this library, on Chapter06\RECIPE11:
  https://github.com/PacktPublishing/Delphi-Cookbook-Third-Edition

  --To run the origial sample project, you are gonna need to fix the Source DPR,
  there is a unit declared from RECIPE05, just change it to RECIPE06
  --AsyncTask in '..\..\Chapter05\CODE\RECIPE06\AsyncTask.pas

  --Indy:
  ----Windows:
  ----the only files worked for me was the 1.0.2g

  --iOS:
  ----the only files worked for me was the 1.0.2g
  ----Just put the files on the same .DPR folder

  --Android:
  ----the only files worked for me was the 1.0.2u
  ----Check the Deployment for libcrypto.so and libssl.so, make sure of have 32 and 64 bits properly configured


  --RR4D:
  ----Library can be downloaded from https://github.com/viniciussanchez/RESTRequest4Delphi
  ----Configure the Project-Options-DelphiCompiler-SearchPath for all platforms
  ----Configure it to work with Indy, read its README*****
  ----Configure everything as the previous Indy section says
  ----You can configure everything on the .DRP, since all are class's methods

  --RestClient
  ----Til this day, IT DOES NOT WORK with SelfSignedCertificate
}

interface

uses
{$IF defined(ANDROID) or defined(IOS)}
  IdSSLOpenSSLHeaders,
{$IF defined(IOS)}
  IdSSLOpenSSLHeaders_Static,
{$ENDIF}
{$ENDIF}
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Layouts, System.Net.URLClient,
  System.Net.HttpClient, System.Net.HttpClientComponent, FMX.Memo.Types,
  FMX.ScrollBox, FMX.Memo, IdBaseComponent, IdComponent, IdTCPConnection,
  IdTCPClient, IdHTTP;

type
  TForm1 = class(TForm)
    GroupBox1: TGroupBox;
    rdbIndy: TRadioButton;
    rdbNetHttp: TRadioButton;
    Layout1: TLayout;
    btnGoodCert: TButton;
    btnSelfSignedCert: TButton;
    rdbRr4dIndy: TRadioButton;
    memResponse: TMemo;
    procedure btnGoodCertClick(Sender: TObject);
    procedure btnSelfSignedCertClick(Sender: TObject);
  private
    const
    CUrlPublic = 'https://livingdocsio.github.io/openapi/livingdocs-openapi.json';

    procedure Get(AUrl: string);

    procedure GetNetHttp(AUrl: string);
    procedure NetHttpClientValidateServerCertificate(
      const Sender: TObject;
      const ARequest: TURLRequest;
      const Certificate: TCertificate;
      var Accepted: Boolean);

    procedure GetIndy(AUrl: string);
    function GetIndySslLibDir: string;

    procedure GetRr4d(AUrl: string);

    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  RESTRequest4D,
  HiddenUnit,
  System.DateUtils,
  System.IOUtils,
  IdSSLOpenSSL,
  IdeaL.Lib.Utils;

{$R *.fmx}

{ TForm1 }

procedure TForm1.btnGoodCertClick(Sender: TObject);
begin
  Get(CUrlPublic);
end;

procedure TForm1.btnSelfSignedCertClick(Sender: TObject);
begin
  Get(HiddenUnit.CUrl);
end;

procedure TForm1.Get(AUrl: string);
begin
  if rdbIndy.IsChecked then
    GetIndy(AUrl)
  else if rdbNetHttp.IsChecked then
    GetNetHttp(AUrl)
  else if rdbRr4dIndy.IsChecked then
    GetRr4d(AUrl);
end;

procedure TForm1.GetIndy(AUrl: string);
begin
{$IF defined(ANDROID)}
  IdOpenSSLSetLibPath(GetIndySslLibDir);
{$IFEND}
  var
  LIdHttp := TIdHTTP.Create(nil);
  try
    try
      var // This case no need to FREE, because they Owner is going to release it
      LIdHandlerSocket := TIdSSLIOHandlerSocketOpenSSL.Create(LIdHttp);
      LIdHandlerSocket.SSLOptions.SSLVersions := [sslvSSLv2, sslvSSLv23, sslvSSLv3, sslvTLSv1, sslvTLSv1_1, sslvTLSv1_2];
      LIdHttp.IOHandler := LIdHandlerSocket;
      LIdHttp.Get(AUrl);
      memResponse.Lines.Insert(0, LIdHttp.ResponseCode.ToString + ' - ' + LIdHttp.ResponseText);
    except
      on E: Exception do
        memResponse.Lines.Insert(0, E.Message);
    end;
  finally
    LIdHttp.Free;
  end;
end;

function TForm1.GetIndySslLibDir: string;
begin
  Result := TUtils.GetApplicationPath;
{$IFDEF ANDROID}
  Result := System.IOUtils.TPath.Combine(Result, 'OpenSSL');
{$IFDEF ANDROID64}
  Result := System.IOUtils.TPath.Combine(Result, '64');
{$ELSE}
  Result := System.IOUtils.TPath.Combine(Result, '32');
{$ENDIF}
{$ENDIF}
end;

procedure TForm1.GetNetHttp(AUrl: string);
var
  LResponse: IHTTPResponse;
begin
  var
  LHttpClient := THttpClient.Create;
  try
    LHttpClient.ContentType := 'application/json';
    try
      LHttpClient.OnValidateServerCertificate := NetHttpClientValidateServerCertificate;
      LResponse := LHttpClient.Get(AUrl);
      memResponse.Lines.Insert(0, LResponse.StatusCode.ToString + ' - ' + LResponse.StatusText);
    except
      on E: Exception do
        memResponse.Lines.Insert(0, E.Message);
    end;
  finally
    LHttpClient.Free;
  end;
end;

procedure TForm1.GetRr4d(AUrl: string);
begin
{$IF defined(ANDROID)}
  IdOpenSSLSetLibPath(GetIndySslLibDir);
{$IFEND}
  try
    var
    LResponse := TRequest.New.BaseURL(AUrl)
      .Accept('application/json')
      .Get;
    memResponse.Lines.Insert(0, LResponse.StatusCode.ToString + ' - ' + LResponse.StatusText);
  except
    on E: Exception do
      memResponse.Lines.Insert(0, E.Message);
  end;
end;

procedure TForm1.NetHttpClientValidateServerCertificate(const Sender: TObject;
  const ARequest: TURLRequest; const Certificate: TCertificate;
  var Accepted: Boolean);
begin
  {
    In case of SelfSignedCert it will RAISE an exception, but since you are checking
    its expiration, if the result is true, all will go smooth

    For DEBUG stuff, just ignore the exceptions
  }
  var
  UTCNow := TTimeZone.Local.ToUniversalTime(Now);
  Accepted := (Certificate.Start <= UTCNow) and (Certificate.Expiry >= UTCNow);
end;

end.
