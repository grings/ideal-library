unit Unit1;

{
  Remind, any configuration must be done for 32 and 64 bits, preferably All Configurations
  Needed permissions on Android
  Read and Write External Storages
  Project-Options-Application-Entitlement List-Secure File Sharing = True

  For D10.4+ it is not needed to config the Provider Paths XML file, while checking
  SecureFileShare Delphi is going to create a default file, which is enough
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
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Layout1: TLayout;
    Button2: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    RadioButton1: TRadioButton;
    RadioButton3: TRadioButton;
    RadioButton2: TRadioButton;
    procedure Button1Click(Sender: TObject);
    procedure Button1Tap(Sender: TObject; const Point: TPointF);
    procedure Button2Tap(Sender: TObject; const Point: TPointF);
    procedure Layout1Painting(Sender: TObject; Canvas: TCanvas;
      const ARect: TRectF);
  private
    function GetFilePathFull: string;
    procedure PdfOpen;
    procedure PdfShare;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  System.IOUtils,
  IdeaL.Lib.Utils,
  IdeaL.Lib.PDFViewer,
  IdeaL.Lib.Android.Permissions;
{$R *.fmx}


procedure TForm1.Button1Click(Sender: TObject);
begin
{$IFDEF MSWINDOWS}
  if Assigned(TControl(Sender).OnTap) then
    TControl(Sender).OnTap(Sender, TPoint.Create(0, 0));
{$ENDIF}
end;

procedure TForm1.Button1Tap(Sender: TObject; const Point: TPointF);
begin
{$IFDEF ANDROID}
  if TUtils.GetOsVersionInt >= 13 then
  begin
    {
    Please read:
    https://developer.android.com/about/versions/13/behavior-changes-13#granular-media-permissions
    https://stackoverflow.com/a/76130238

    Long story short:
    On Android13 you do not need to ask for Read and Write External Storage permissions
    it is given by default

    ***!!!!!!!!**** It does not apply for the Scoped Storage rules (API29+)
    }

    PdfOpen;
  end
  else
  begin
    TPermissions.GetPermissions(
      [ptReadExternalStorage, ptWriteExternalStorage],
      PdfOpen,
      procedure
      begin
        ShowMessage('The permissions are needed')
      end
      );
  end;
{$ELSE}
  OpenPdf;
{$ENDIF}
end;

procedure TForm1.Button2Tap(Sender: TObject; const Point: TPointF);
begin
{$IFDEF ANDROID}
  TPermissions.GetPermissions(
    [ptReadExternalStorage, ptWriteExternalStorage],
    PdfShare,
    procedure
    begin
      ShowMessage('The permissions are needed')
    end
    );
{$ELSE}
  PdfShare;
{$ENDIF}
end;

function TForm1.GetFilePathFull: string;
var
  LPath: string;
begin
  {
    This is the private internal path, there are some rules to work on it, BUT
    you are probably downloading the File from wherelse, so please, do not
    use this folder, this sample APP is using it to make the things easier, so
    I don't need to download it or create in runtime
  }
  LPath := System.IOUtils.TPath.GetDocumentsPath;
  LPath := System.IOUtils.TPath.Combine(LPath, 'pdf-test.pdf');
  {
    Use public path instead!!!!
    Save all your file here and remember of deleting them whenever is possible
  }

  if RadioButton1.IsChecked then
    Result := TPath.GetPublicPath
  else if RadioButton2.IsChecked then
    Result := TPath.GetDocumentsPath
  else if RadioButton3.IsChecked then
    Result := TPath.GetSharedDocumentsPath;

  if not DirectoryExists(Result) then
    Result := TPath.GetPublicPath
  else if not DirectoryExists(Result) then
    Result := TPath.GetSharedDocumentsPath
  else if not DirectoryExists(Result) then
    Result := TPath.GetDocumentsPath;

  if not DirectoryExists(Result) then
    raise Exception.Create('Directory [' + Result + ' does not exist');

  Result := TPath.Combine(Result, 'pdf-test.pdf');
  if not FileExists(Result) then
    TFile.Copy(LPath, Result);

  if not(FileExists(Result)) then
    raise Exception.Create('File [' + Result + '] does not exist');
end;

procedure TForm1.Layout1Painting(Sender: TObject; Canvas: TCanvas;
  const ARect: TRectF);
begin
  if DirectoryExists(TPath.GetPublicPath) then
    Label1.Text := TPath.GetPublicPath
  else
    Label1.Text := 'No PublicPath';
  if DirectoryExists(TPath.GetDocumentsPath) then
    Label2.Text := TPath.GetDocumentsPath
  else
    Label2.Text := 'No DocumentsPath';
  if DirectoryExists(TPath.GetSharedDocumentsPath) then
    Label3.Text := TPath.GetSharedDocumentsPath
  else
    Label3.Text := 'No SharedDocumentsPath';
end;

procedure TForm1.PdfOpen;
var
  LPath: string;
begin
  try
    LPath := GetFilePathFull;
    TPDFViewer.OpenPdf(LPath);
  except
    on E:Exception do
      ShowMessage(E.Message);
  end;
end;

procedure TForm1.PdfShare;
var
  LPath: string;
  LPathShared: string;
begin
  try
    LPath := GetFilePathFull;
    TUtils.ShareSheetFile(LPathShared);
  except
    on E:Exception do
      ShowMessage(E.Message);
  end;
end;

end.