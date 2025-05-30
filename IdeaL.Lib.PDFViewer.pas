{*******************************************************}
{ 2023.06.15                                            }
{ Thanks for Lucas Neves https://github.com/lucasdarkye }
{   for testing and finding a fix for Delphi10.4+ and   }
{              Androids 7.1.1 and lesser                }
{*******************************************************}

unit IdeaL.Lib.PDFViewer;

interface

uses
  System.IOUtils,
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.DateUtils,

  IdURI,

  Data.DB,

{$IFDEF ANDROID}
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.JavaTypes, // Save doc as Stream
  Androidapi.JNI.Net, // Open File
  Androidapi.Helpers, // StringToJString
  FMX.Helpers.Android,
  {$IF CompilerVersion >= 34.0}Androidapi.JNI.Support,{$ELSE}{$ERROR}DW.Androidapi.JNI.FileProvider,{$ENDIF}
{$ENDIF }
{$IF DEFINED(IOS)}
  iOSApi.Foundation,
  iOSapi.UIKit,
  Macapi.Helpers,
  FMX.Helpers.iOS,
  FMX.Forms,
  FMX.WebBrowser,
  FMX.Types,
  FMX.StdCtrls,
  FMX.Dialogs,
  FMX.Platform.iOS,
{$ENDIF}
{$IFDEF MACOS}
  Posix.Stdlib,
{$ENDIF MACOS}
{$IFDEF MSWINDOWS}
  Winapi.ShellAPI, Winapi.Windows,
{$ENDIF MSWINDOWS}
  FMX.Objects,
  FMX.Surfaces;

type
  TPDFViewer = class
  private
    {$IFDEF ANDROID}
      class procedure OpenApi26Less(const AFilePath: string);
      class procedure OpenApi26More(const AFilePath: string);
    {$ENDIF}
    class procedure Open(const AFilePath: string);
    { private declarations }
  protected
    { protected declarations }
  public
    class procedure OpenPdf(const AFilePath: string);
    class procedure Share(const AFilePath: string);
    { public declarations }
  published
    { published declarations }
  end;

implementation

uses
IdeaL.Lib.Utils;

{ TPDFViewer }

{$IFDEF ANDROID}

class procedure TPDFViewer.OpenApi26Less(const AFilePath: string);
var
  LIntent         : JIntent;
  LtmpFile        : string;
{$IF CompilerVersion >= 34.0}
  LFile: JFile;
{$ENDIF}
begin
{$IF CompilerVersion >= 34.0}
  (* 10.4+ *)
  LFile   := TJFile.JavaClass.init(StringToJString(AFilePath));
  LIntent := TJIntent.JavaClass.init(TJIntent.JavaClass.ACTION_VIEW);
  LIntent.setDataAndType(TAndroidHelper.JFileToJURI(LFile), StringToJString('application/pdf'));
  LIntent.setFlags(TJIntent.JavaClass.FLAG_GRANT_READ_URI_PERMISSION);
  TAndroidHelper.Activity.startActivity(LIntent);
{$ELSE}
  LIntent := TJIntent.Create;
  LIntent.setAction(TJIntent.JavaClass.ACTION_VIEW);

  LtmpFile := StringReplace(AFilePath, ' ', '%20', [rfReplaceAll]);

  LIntent.setDataAndType(StrToJURI('file://' + AFilePath), StringToJString('application/pdf'));

  SharedActivity.startActivity(LIntent);
{$ENDIF}
end;

class procedure TPDFViewer.OpenApi26More(const AFilePath: string);
var
  LIntent: JIntent;
  LUri: Jnet_Uri;
  LJFile: Jfile;
  LStrFileProvider: string;
  LPath: string;
  LName: string;
begin
  LStrFileProvider := JStringToString
      (TAndroidHelper.Context.getApplicationContext.getPackageName) +
      '.fileprovider';

  LPath := System.IOUtils.TPath.GetDirectoryName(AFilePath);
  LName := System.IOUtils.TPath.GetFileName(AFilePath);

  LJFile := TJfile.JavaClass.init(
    Androidapi.Helpers.StringToJString(LPath),
    Androidapi.Helpers.StringToJString(LName)
    );

  LUri := {$IF CompilerVersion >= 35.0}TJcontent_FileProvider{$ELSE}TJFileProvider{$ENDIF}.JavaClass.getUriForFile(
    TAndroidHelper.Context,
    Androidapi.Helpers.StringToJString(LStrFileProvider),
    TJFile.JavaClass.init(StringToJString(AFilePath))
    );

  LIntent := TJIntent.JavaClass.init(TJIntent.JavaClass.ACTION_VIEW);
  LIntent.setDataAndType(LUri, StringToJString('application/pdf'));
  LIntent.setFlags(TJIntent.JavaClass.FLAG_GRANT_READ_URI_PERMISSION);
  TAndroidHelper.Activity.startActivity(LIntent);
end;

class procedure TPDFViewer.Open(const AFilePath: string);
begin
  if (IdeaL.Lib.Utils.TUtils.GetOsVersionInt >= 8) then
    OpenApi26More(AFilePath)
  else
    OpenApi26Less(AFilePath);
end;
{$ENDIF }

{$IF DEFINED(IOS)}
type
  TCloseParentFormHelper = class
  public
    procedure OnClickClose(Sender: TObject);
  end;

procedure TCloseParentFormHelper.OnClickClose(Sender: TObject);
begin
  TForm(TComponent(Sender).Owner).Close();
end;

class procedure TPDFViewer.Open(const AFilePath: string);
var
  NSU                      : NSUrl;
  OK                       : Boolean;
  frm                      : TForm;
  WebBrowser               : TWebBrowser;
  btn                      : TButton;
  btnShare                 : TButton;
  toolSuperior             : TToolBar;
  Evnt                     : TCloseParentFormHelper;
  tmpFile                  : String;
begin
  Frm                      := TForm.CreateNew(nil);

  toolSuperior             := TToolBar.Create(frm);
  toolSuperior.Align       := TAlignLayout.Top;
  toolSuperior.StyleLookup := 'toolbarstyle';
  toolSuperior.Parent      := frm;

  {Bot�o Back}
  btn                      := TButton.Create(frm);
  btn.Align                := TAlignLayout.Left;
  btn.Margins.Left         := 8;
  btn.StyleLookup          := 'backtoolbutton';
  btn.Text                 := 'Voltar';
  btn.Parent               := toolSuperior;

  WebBrowser               := TWebBrowser.Create(frm);
  WebBrowser.Parent        := frm;
  WebBrowser.Align         := TAlignLayout.Client;

  evnt                     := TCloseParentFormHelper.Create;
  btn.OnClick              := evnt.OnClickClose;

  {if AExternalURL then
  begin
    tmpFile := StringReplace(AFilePath, ' ', '%20', [rfReplaceAll]);
    WebBrowser.Navigate('http://' + tmpFile);
  end
  else}
    WebBrowser.Navigate('file://' + System.IoUtils.TPath.Combine(System.IoUtils.TPath.GetDocumentsPath, AFilePath));

  frm.ShowModal();

end;
{$ENDIF}

{$IF DEFINED(MACOS) and not DEFINED(IOS)}
class procedure TPDFViewer.Open(const AFilePath: string);
begin
  _system(PAnsiChar('open '+'"'+AnsiString(AFilePath)+'"'));
end;
{$IFEND}

{$IFDEF MSWINDOWS}

class procedure TPDFViewer.Open(const AFilePath: string);
begin
  ShellExecute(0, 'OPEN', PChar(AFilePath), '', '', SW_SHOWNORMAL);
end;
{$ENDIF MSWINDOWS}

class procedure TPDFViewer.OpenPdf(const AFilePath: string);
begin
  TPDFViewer.Open(AFilePath);
end;

{Solution copied from https://github.com/Sareto/printPDF/blob/master/uPdfPrint.pas

Thanks to Mauricio Sareto
mauricio_sareto@hotmail.com
}
class procedure TPDFViewer.Share(const AFilePath: string);
{$IFDEF ANDROID}
Var
  IntentShare: JIntent;
  uri: Jnet_Uri;
  Uris: JArrayList;
  AttFile: JFile;
  Path: String;
{$ENDIF}
Begin
{$IFDEF ANDROID}
  IntentShare := TJIntent.JavaClass.init(TJIntent.JavaClass.ACTION_SEND);
  Uris := TJArrayList.Create;
  AttFile := TJFile.JavaClass.init(StringToJString(AFilePath));
  uri := TJnet_Uri.JavaClass.fromFile(AttFile);
  Uris.add(0, uri);
  IntentShare.putExtra(TJIntent.JavaClass.EXTRA_TEXT, StringToJString(''));
  IntentShare.setType(StringToJString('application/pdf'));
  IntentShare.putParcelableArrayListExtra(TJIntent.JavaClass.EXTRA_STREAM, Uris);
  TAndroidHelper.Activity.StartActivity(TJIntent.JavaClass.createChooser(IntentShare, StrToJCharSequence('Compartilhar com:')));
{$ENDIF}
end;

end.
