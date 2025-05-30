unit Unit1;

{
  You are going to need the followed permissions:
  Internet, Read and Write external storage and Request install packages
  Entitlement List: Secure file sharing = TRUE
  Remind doing these changes for 32 and 64 bits, for debug and release
  (better if you use All Config always)

  Delphi 10.4.2 or older? Make sure to put it on your
  AndroidManifest.template.xml, in the 'application' tag:
  android:requestLegacyExternalStorage="true"
  Why? Try to read the Android documentation at least once, then you will get it

  Delphi 11+
  don't need to work or replace the provider_paths.xml,
  just make sure of checking Secure file sharing, so Delphi must do all needed
  stuff
  Android.Manifest.Template.xml can be the auto generated one

  File provider (some of these links might help you):
  (take a look at the Manifest.Template on D11 example folder)
  https://stackoverflow.com/questions/56598480/couldnt-find-meta-data-for-provider-with-authority
  https://stackoverflow.com/questions/54510186/delphi-use-android-fileprovider-to-send-intent-to-open-and-image-file-with-the-d?noredirect=1&lq=1
  https://imperiumdelphi.wordpress.com/2019/03/04/o-tal-do-fileprovider-do-android/
  https://www.delphipraxis.net/200744-fehler-beim-android-fileprovider-datei-wird-nicht-geoeffnet.html
  https://blogs.embarcadero.com/opening-a-pdf-on-android-with-delphi/
}
interface

uses
  IdeaL.Lib.Utils,
  System.Permissions,
  System.Math,
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Memo.Types, FMX.ScrollBox,
  FMX.Memo;

type
  TUTils = class(IdeaL.Lib.Utils.TUTils)
  private
    class procedure DoPermissionNotGranted;
    { private declarations }
  protected
    { protected declarations }
  public
    class procedure GetPermissionReadWriteExternalStorage(AGranted: TProc);
    { public declarations }
  end;

  TForm1 =
    class(TForm)
    Button1: TButton;
    Label1: TLabel;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    const
    CAccessToken = 'YourDropBoxTokenHere';

    function DownloadFile: string;
    procedure DownloadUploadStatusUpdate(const ALength, ACount: Int64);
    procedure AddLog(const AMsg: string);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  System.IOUtils,
  IdeaL.Lib.Api.DropBox
{$IFDEF ANDROID}
    ,
  IdeaL.Lib.Android.Permissions
{$ENDIF};

{$R *.fmx}


procedure TForm1.AddLog(const AMsg: string);
begin
  TThread.Queue(nil,
    procedure
    begin
      Memo1.Lines.Insert(0, AMsg);
    end);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  {
    Yes, ALWAYS ask for permission and execute the process in the CallBack
    Notice that to request permission is an assynchronous process, it changes evertyhing
  }
  Memo1.Lines.Clear;
  TUTils.GetPermissionReadWriteExternalStorage(
    procedure
    begin
      TThread.CreateAnonymousThread(
        procedure
        var
          LPath: string;
        begin
          try
            LPath := DownloadFile;
            AddLog('Openning APK');
            TUTils.OpenApkFile(LPath);
            AddLog('Done');
          except
            on E: Exception do
            begin
              AddLog(E.Message);
            end;
          end;
        end).Start;
    end);
end;

function TForm1.DownloadFile: string;
const
  // the name of the file that is downloaded from DropBox
  CApkFileName = 'YourAppName.apk';

var
  LPathLocal: string;
  LPathRemote: string;
  LDb: TDropBoxApi;
begin
  AddLog('DownloadFile began');
  {
    Doesn't matter where you store the file and how you download it, I chose
    to work with DropBox because I liked it
    what matters is where you are going to store it locally

    #OLD I chose SharedDocumentsPath instead DocumentsPath because I'm too lazy working
    with provider flags

    I'm using GetPublicPath because this is APP related path, reacheble from PC
    or File Manager APPs and don't have new File secure rules (YET)
  }
  Result := EmptyStr;
  LPathLocal := TPath.GetPublicPath;
  // use your project name istead SelfUpdate
  LPathLocal := TPath.Combine(LPathLocal, 'SelfUpdate');
  ForceDirectories(LPathLocal);
  LPathLocal := TPath.Combine(LPathLocal, CApkFileName);
  // You can use subpaths if you have it on DropBox
  LPathRemote := '/' + CApkFileName;
  AddLog('Local path: ' + LPathLocal);
  AddLog('Remote path: ' + LPathRemote);
  AddLog('Token: ' + CAccessToken);
  // Take a look at Demos\Api\DropBox to see how to use DropBox class
  LDb := TDropBoxApi.Create;
  try
    try
      LDb.AccessToken := CAccessToken;
      LDb.OnDownloadStatusUpdate := DownloadUploadStatusUpdate;
      AddLog('Downloading file');
      LDb.Download(LPathLocal, LPathRemote);
      AddLog('Download finished, no errors');
    except
      on E: Exception do
        AddLog('Download finished, error: ' + E.Message);
    end;
  finally
    FreeAndNil(LDb);
  end;
  // If everything goes right it will return the full file path
  Result := LPathLocal;
end;

procedure TForm1.DownloadUploadStatusUpdate(const ALength, ACount: Int64);
var
  LPerc: Integer;
begin
  LPerc := Trunc((ACount / System.Math.IfThen(ALength = 0, 1, ALength)) * 100);
  AddLog(LPerc.ToString + '%');
end;

{ TUTils }

class procedure TUTils.DoPermissionNotGranted;
begin
  ShowMessage('Need permissions');
end;

class procedure TUTils.GetPermissionReadWriteExternalStorage(AGranted: TProc);
begin
{$IFDEF ANDROID}
  {
    Don't know how to do that your self and don't want to use My solution?
    Take a look at Samples\Object Pascal\Multi-Device Samples\Media\PhotoEditorDemo
    and learn how to do that
  }
  IdeaL.Lib.Android.Permissions.TPermissions.GetPermissions(
    [ptReadExternalStorage, ptWriteExternalStorage],
    AGranted,
    DoPermissionNotGranted
    )
{$ELSE}
  AGranted;
{$ENDIF}
end;

end.
