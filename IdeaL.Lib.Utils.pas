unit IdeaL.Lib.Utils;

interface

uses
{$IFDEF GrijjyLogger}
  Grijjy.CloudLogging,
  (*This is a Log and Memory tracker for DEBUG mode, very useful. The repository README says it works on iOS and Android, I (Igor) didn't test it yet

    *It is needed the DLL at the same EXE folder, the 32 or 64 bits depending of the EXE you want to track;
    * For our projects, we have a method called Lib.Utils.TUtils.GrijjyLogSend that you can use to broadcast msgs to the Logger;
    * For Memory trakker, just add the unit as the first USES of your .DPR, as followed:
    {$IFDEF GrijjyLogger}
    Grijjy.CloudLogging.InstanceTracker,
    {$ENDIF}
    Check the "projekte/Lib/Includes/GlobalDefines.inc" to check the definition GrijjyLogger
    Also add the following path to your Project Search path (taking care just about the correct ..\):
    ..\Lib\Controller\API\SendGrid
    ..\Lib\Third Party\Grijjy\Foundation
    ..\Lib\Third Party\Grijjy\DelphiZeroMQ

    Referencies:
    https://github.com/grijjy/GrijjyCloudLogger
    https://github.com/grijjy/GrijjyFoundation
    https://github.com/grijjy/DelphiZeroMQ

  *)

{$ENDIF}
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.Types,
  System.StrUtils,
  System.Zip,
  System.ZLib,
  System.MaskUtils,
  System.UIConsts,
  System.UITypes,
  System.JSON,
  System.Rtti,
  System.Math,
  System.Masks,
  System.Generics.Collections,

  REST.JSON,

  Data.DB,
  Data.Bind.DBScope,
  Data.Bind.Components

{$IFDEF FMX}
    ,
  FMX.Controls,
  FMX.Edit,
  FMX.DateTimeCtrls,
  FMX.Layouts,
  FMX.TabControl,
  FMX.Platform,
  FMX.Types,
  FMX.Forms,
  FMX.MultiView,
  FMX.StdCtrls,
  FMX.VirtualKeyboard,
  FMX.ListBox,
  FMX.MultiResBitmap,
  FMX.ImgList,
  FMX.Graphics,
  FMX.Controls.Presentation
{$ENDIF}

{$IFDEF ANDROID}
    ,
  FMX.Platform.Android,
  FMX.Helpers.Android,
  Androidapi.JNI.Telephony,
  Androidapi.JNI.Provider,
  Androidapi.JNI.Net,
  Androidapi.JNI.Java.Net,

  Androidapi.JNI.App,
  Androidapi.Helpers,
  Androidapi.JNIBridge,
  Androidapi.JNI.OS,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.GraphicsContentViewText,
  // for any DW units, take a look at https://github.com/DelphiWorlds/Kastri
{$IFDEF KASTRI}
  DW.Androidapi.JNI.OS,
  DW.OSLog,
  // DW.Androidapi.JNI.Util,
  DW.ShareItems,
{$ENDIF}
{$IF CompilerVersion >= 34.0}Androidapi.JNI.Support, {$ELSE}DW.Androidapi.JNI.FileProvider, {$ENDIF}
  Androidapi.IOUtils
{$ENDIF}
{$IFDEF IOS}
    ,
  iOSapi.UIKit,
  iOSapi.Foundation,
  Macapi.Helpers,
  FMX.Helpers.iOS,

  Posix.NetIf,

  System.Net.URLClient
{$IFDEF KASTRI}
  , 
  DW.OSDevice,
  DW.Connectivity,
  DW.ShareItems
{$ENDIF}
{$ENDIF}
{$IF declared(FireMonkeyVersion)}
{$IFDEF MACOS}
    , Macapi.CoreFoundation
{$IFEND MACOS}
{$IFEND}
{$IFDEF MSWINDOWS}
    , Vcl.Forms,
    Vcl.Clipbrd,

  System.Win.Registry,

  Winapi.WinINet,

  Winapi.Windows,
  Winapi.ShellAPI
{$IFEND}
    , IdTCPClient;

type
  TLogType = (ltInformation, ltWarning, ltError);

  TUtils = class
  private
{$IFDEF FMX}
    class var FChangeTabAction: TChangeTabAction;
    class procedure SetChangeTabAction(const Value: TChangeTabAction); static;
{$ENDIF}
{$IF DEFINED (KASTRI) AND (DEFINED (ANDROID) OR DEFINED(IOS))}
    class procedure ShareFilesCompletedHandler(Sender: TObject; const AActivity: TShareActivity; const AError: string);
{$ENDIF}
  public
    type
    TFileType = (ftCsv, ftPdf);
{$IF DEFINED (KASTRI) AND (DEFINED (ANDROID) OR DEFINED(IOS))}
    TShareActivity = DW.ShareItems.TShareActivity;
{$ENDIF}

  class var
    VarTryStrToInt: Integer;
    VarTryStrToDateTime: TDateTime;
    VarTryStrToFloat: Double;

{$IFDEF FMX}
    class property ChangeTabAction: TChangeTabAction read FChangeTabAction write SetChangeTabAction;
{$ENDIF}
    class function CountSubString(const AText, aSubstring: string): Integer;
    class function GetWord(const AText: string; const APos: Integer): string;
    class function FirstWord(const AText: string): string;
    /// <summary> Extract the first letter from each word from AText
    /// </summary>
    class function FirstLetterEachWord(const AText: string; const ACountLetters: Integer; const ASwitchSpace: string = ''): string;
    /// <summary> Upper case first letter from each word
    /// </summary>
    class function FirstLetterUpperCase(const AText: String; const ALowerCaseAll: Boolean = True): String;
    class function RemovePathFromDir(const APath: string; const ANumberToRemove: Integer): string;
    class procedure ForceDirectory(const ADirectory: string);
    class function ClipboardCopyText(const AValue: string): Boolean; virtual;
    class function JustNumber(const AValue: string; const AStartWithOne: Boolean = False): string;
    class function RemoveSpecialCharacter(const AValue: string): string;
    class function FormatDate(const AValue: string): string;
	class function FormatCnpj(const AValue: String): String;														
    class function FormatCpfCnpj(const AValue: String): String;
    class function FormatCep(const AValue: string; AIsAddDot: Boolean = True): String;
    class function FormatPhoneNumber(const AValue: String): String;
    class function FormatCurrency(AValue: String; ADecimalLenght: Integer = 2; ABackspaceIsPressedEdtCurrency: Boolean = False): string;
    class function isCNPJ(AValue: string): Boolean;
    class function isCPF(AValue: string): Boolean;
    class function isInscricaoEstadual(const AIe, AUf: String): Boolean;
    class function isEMail(AValue: String): Boolean;
    class function Combine(const APath1: string; const APath2: array of string): string;

    // DateTime
    class function NowToUniversalTime: TDateTime;
    class function UniversalTimeToLocal(AUtc: Int64; AIsUTC: Boolean = True): TDateTime;
    class function DateTimeToUniversalTime(AValue: TDateTime; AIsUTC: Boolean = True): Int64;

    class function TryStrToDateTime(ADateTimeStr: string; var ADateTime: TDateTime): Boolean;

    class function ZipFile(const AZipFile, AFileName: string): Boolean;
    class function UnZipFile(const AZipFile, APathToExtract: string): Boolean;
    class function CompressString(AValue: string): string;
    class function DecompressString(AValue: string): string;
    class function GetFiles(const APath: string; const AExtractFileName: Boolean = False): string;
    class function GetFilesList(const APath: string; const AExtractFileName: Boolean = False): TStringList;

    class function ExecuteFile(const AFileName, AParams, ADefaultDir: string; AShowCmd: Integer): THandle;
    class function CheckInternet(const AHost: string; const APort: Integer; const ATimeOut: Integer = 2000): Boolean; overload;
{$IFDEF FMX}
    class function ComboBoxSearch(AComboBox: TComboBox; AText: string): Integer;
{$ENDIF}
    class procedure SetWebBrowserPermissions;
    class function GetApplicationPath(): string;
    class function GetPathSharedDocuments(): string;
    class function GetPathDocuments(): string;
    class function GetDocumentsTemp(const APathMaster: string = ''): string; virtual;
    class function GetApplicationVersion(): string;
    class function GetOsVersion(): string;
    class function GetOsVersionInt: Integer;
    class function GetOsPlatform: string;
    class function GetLocalIp: string; virtual;

    class procedure GravaSqlEmTxt(aStrSql: string; aManterTexto: Boolean = False);
    class procedure GravaDadosDataSetToTxt(aDataSet: TDataSet; aManterTexto: Boolean = False);
    class function LerTxt(const aFileFullPath: String): String;

    class function LogPath: string;
    class procedure LogWrite(const AMsg: string; const ALogType: TLogType);
    class procedure LogWriteInformation(const AMsg: string);
    class procedure LogWriteWarning(const AMsg: string);
    class procedure LogWriteError(const AMsg: string);

    // Strings
    class function Contains(const Value: string; AToCompare: array of string): Boolean;
    class function StreamToString(aStream: TStream): string;
{$IFDEF FMX}
    class procedure ClearVtsList(AVertScroll: TVertScrollBox; AClassType: TClass);
{$ENDIF}
    class function AddStrLeft(AVlaue, AStrAdd: string; ACount: Integer): string;
    class function AddStrRight(AVlaue, AStrAdd: string; ACount: Integer): string;
    class function StrRightStr(const AText: string; const ACount: Integer): string;
    class function GetStrJosnEnconded(const AValue: string; const AIsArray: Boolean = True): string; virtual;
    class function GetGUID: string;
    class function GetGUIDAsTableId: string;
    class function GetGUIDAsComponentName: string;

{$IFDEF FMX}
    class procedure ManipulaComponentes(aTela: TComponent);
    class procedure SetFocus(const AComponent: TControl);
{$ENDIF}
    class function StringToAlphaColor(const AColor: string): TAlphaColor;

    class function JsonNormalize(const AText: string): string;
    class function GetParamValueFromJsonObject(const AParamName, AJsonObj: string): string; overload;
    class function GetParamValueFromJsonObject(const AParamName, AJsonObj: string; var AResult: string): string; overload;
    class function GetParamValueFromJsonObject(const AParamName, AJsonObj: string; var AResult: string; const ADefultValue: string): string; overload;
    class function GetJsonObjectFromJsonArray(AJson: string; const AIndex: Integer): string; overload;
    class function GetJsonArraySize(AJson: string): Integer;
    class function GetJsonToClassObject<T: class, constructor>(AJson: string): T;

{$IFDEF FMX}
    class function IsAssigned(AObj: TFmxObject): Boolean;
    class procedure ChangeTab(ATabControl: TTabControl; ATabItem: TTabItem; AOnFinish: TNotifyEvent = nil);
    class procedure OpenForm(
      const AFrmClass: TComponentClass;
      ATarget: TFmxObject;
      var AFrmActive: FMX.Forms.TForm;
      AMainMenu: TMultiView;
      const AMasterButtonName: string = '';
      AOwner: TFmxObject = nil;
      const AIsOwnerNil: Boolean = False
      );
    class procedure OpenFrame(
      const AFrmClass: TComponentClass;
      ATarget: TFmxObject;
      var AFrmActive: FMX.Forms.TFrame;
      AMainMenu: TMultiView;
      const AMasterButtonName: string = '';
      AOwner: TFmxObject = nil;
      const AIsOwnerNil: Boolean = False
      );
    class procedure HideKeyboard(const AComponentSetFocus: TControl = nil);
    class procedure ShowKeyboard(const AControl: TFmxObject = nil);
{$ENDIF}
    class function PadL(S: string; Ch: Char; Len: Integer): string;
    class function PadR(S: string; Ch: Char; Len: Integer): string;

    // SO
    class function CallNumber(const AValue: string): Boolean; virtual;
    class function HasCallService: Boolean;
    class function GetDeviceId: string;
    class function GetDeviceModel: string;
    class function GetDeviceName: string;
    class function GetDeviceSerial: string;
    class function GetIMEI(): string;
    class function OpenApkFile(const AFile: string): Boolean;
    class procedure OpenUrl(const AUrl: string); overload; virtual;
    class procedure OpenGooglePlayStore(const APackageName: string);
    class function IsAppInstalled(const AAppName: string): Boolean;
    class procedure OpenExternalApp(const AAppName: string);
    class procedure OpenMap(const AAdsress: string); overload;
    class procedure OpenMapShareSheet(ALat, ALong: Single); overload;
    class procedure OpenMapShareSheet(AAddress: string); overload;
    class procedure OpenMapShareSheet(ALat, ALong: Single; AAddress: string); overload;
    /// <summary> Only on DEBUG mode will write to the IDE Messages event log
    /// </summary>
    class procedure EventLogIde(AMsg: string);
    class procedure EventLogConsole(AMsg: string); virtual;
    class procedure EventLogGrijjySend(const AMsg, AValue: string);
    /// <summary>
    /// Method is broadcasting Log info to Logcat (Android), NSLog for MACOS,
    /// If Config.App.IsLogActive will also save local file LOG (fixed maServiceLogWriteSave)
    /// On Windows, also doing GrijjyLogSend
    /// </summary>
    class procedure EventLog(AMsg: string; AIsForceBraodCast: Boolean = False); virtual;
{$IFDEF FMX}
    class function IsVirtualKeyboardShwon: Boolean;
{$ENDIF}
    class procedure MemoryUsage(out ATotal, AAllocated, AFree: Double);

    class function RandomNumber(const ALimit: Integer): Integer;
{$IFDEF FMX}
    class function ImgListGetImageByName(const AName: string; AImgList: FMX.ImgList.TImageList): FMX.Graphics.TBitmap;
{$ENDIF}
    class procedure ShareSheetText(const AText: string);
    class procedure ShareSheetFile(const AFilePathFull: string;
{$IF NOT (DEFINED (KASTRI) AND (DEFINED (ANDROID) OR DEFINED(IOS)))} AFileType: TFileType = ftPdf{$ELSE}
      const AControl: TControl; AOnCompletedEvent: TShareCompletedEvent = nil{$ENDIF});

    class function GetEnumName<T { : enum } >(AValue: T): string;
    class function GetEnumValue<T { : enum } >(AValue: string): T;

    // Exceptions
    class procedure ThrowExceptionCouldNotFindTheResource(AValue: string = ''); overload; virtual;
    class procedure ThrowExceptionForbidden; virtual;
    class procedure ThrowExceptionMethodIsNotImplemented(AValue: string = '');
    class procedure ThrowExceptionParamIsRequired(const AValue: string);
    class procedure ThrowExceptionInvalidJson;
    class procedure ThrowExceptionInvalidJsonValue(const AValue: string);
    class procedure ThrowExceptionInvalidValue(const AValue: string);
    class procedure ThrowExceptionSomethingWentWrong(AValue: string = ''); virtual;
    class procedure ThrowExceptionTheSessionHasExpired;
    class procedure ThrowExceptionValueCannotBeEmptyOrNull(AValue: string = ''); overload; virtual;
  private
  end;

{$IF defined(ANDROID) and not defined(KASTRI)}
type
  Jutil_LogClass = interface(JObjectClass)
    ['{62108FE8-1DBB-4C4F-A0C7-35D12BD116DC}']
    {class} function _GetASSERT: Integer; cdecl;
    {class} function _GetDEBUG: Integer; cdecl;
    {class} function _GetERROR: Integer; cdecl;
    {class} function _GetINFO: Integer; cdecl;
    {class} function _GetVERBOSE: Integer; cdecl;
    {class} function _GetWARN: Integer; cdecl;
    {class} function d(tag: JString; msg: JString): Integer; cdecl; overload;
    {class} function d(tag: JString; msg: JString; tr: JThrowable): Integer; cdecl; overload;
    {class} function e(tag: JString; msg: JString): Integer; cdecl; overload;
    {class} function e(tag: JString; msg: JString; tr: JThrowable): Integer; cdecl; overload;
    {class} function getStackTraceString(tr: JThrowable): JString; cdecl;
    {class} function i(tag: JString; msg: JString): Integer; cdecl; overload;
    {class} function i(tag: JString; msg: JString; tr: JThrowable): Integer; cdecl; overload;
    {class} function isLoggable(tag: JString; level: Integer): Boolean; cdecl;
    {class} function println(priority: Integer; tag: JString; msg: JString): Integer; cdecl;
    {class} function v(tag: JString; msg: JString): Integer; cdecl; overload;
    {class} function v(tag: JString; msg: JString; tr: JThrowable): Integer; cdecl; overload;
    {class} function w(tag: JString; msg: JString): Integer; cdecl; overload;
    {class} function w(tag: JString; msg: JString; tr: JThrowable): Integer; cdecl; overload;
    {class} function w(tag: JString; tr: JThrowable): Integer; cdecl; overload;
    {class} function wtf(tag: JString; msg: JString): Integer; cdecl; overload;
    {class} function wtf(tag: JString; tr: JThrowable): Integer; cdecl; overload;
    {class} function wtf(tag: JString; msg: JString; tr: JThrowable): Integer; cdecl; overload;
    {class} property ASSERT: Integer read _GetASSERT;
    {class} property DEBUG: Integer read _GetDEBUG;
    {class} property ERROR: Integer read _GetERROR;
    {class} property INFO: Integer read _GetINFO;
    {class} property VERBOSE: Integer read _GetVERBOSE;
    {class} property WARN: Integer read _GetWARN;
  end;

  [JavaSignature('android/util/Log')]
  Jutil_Log = interface(JObject)
    ['{6A5EC34E-CB76-4AB0-A11D-7CCB3B40C571}']
  end;
  TJutil_Log = class(TJavaGenericImport<Jutil_LogClass, Jutil_Log>) end;
{$ENDIF}

implementation

uses
  System.RegularExpressions,
  System.DateUtils
{$IFDEF FMX}
  ,
  FMX.PhoneDialer
{$ENDIF}
{$IFDEF IOS}
  ,
  Posix.SysSysctl,
  Posix.StdDef
{$ENDIF}
;

{ TUtils }

{$IFDEF FMX}


class procedure TUtils.ManipulaComponentes(aTela: TComponent);
var
  i: Integer;
begin
  for i := 0 to Pred(aTela.ComponentCount) do
  begin
    if (TLinkControlToField = aTela.Components[i].ClassType) and
      (TLinkControlToField(aTela.Components[i]).Control <> nil) and
      (TLinkControlToField(aTela.Components[i]).Control is TEdit) and
      (Trim(TLinkControlToField(aTela.Components[i]).FieldName) <> '') and
      (TLinkControlToField(aTela.Components[i]).DataSource <> nil) and
      (TBindSourceDB(TLinkControlToField(aTela.Components[i]).DataSource)
      .DataSet <> nil) then
    begin
      TEdit(TLinkControlToField(aTela.Components[i]).Control).MaxLength :=
        TBindSourceDB(TLinkControlToField(aTela.Components[i]).DataSource)
        .DataSet.FieldByName(TLinkControlToField(aTela.Components[i])
        .FieldName).Size;
    end
    else if (TDateEdit = aTela.Components[i].ClassType) then
    begin
      TDateEdit(aTela.Components[i]).IsEmpty := True;
      TDateEdit(aTela.Components[i]).TodayDefault := True;
    end;
  end;
end;

class procedure TUtils.SetChangeTabAction(const Value: TChangeTabAction);
begin
  FChangeTabAction := Value;
end;

class function TUtils.ComboBoxSearch(AComboBox: TComboBox;
  AText: string): Integer;
var
  LString: TStrings;
  i: Integer;
  LTextLenght: Integer;
  LItemText: String;
begin
  Result := -1;
  LString := AComboBox.Items;

  for i := 0 to LString.Count - 1 do
  begin
    LTextLenght := AText.Length;
    LItemText := Copy(LString.Strings[i], 1, LTextLenght);
    if (AText = LItemText) then
    begin
      Result := i;
      Break;
    end;
  end;
end;

class procedure TUtils.ClearVtsList(AVertScroll: TVertScrollBox;
  AClassType: TClass);
var
  i: Integer;
  LFrame: TComponent;
begin
  if not(Assigned(AVertScroll)) then
    Exit;
  try
    // Pesquisar e deixar isso no formulario padrao de listas.
    AVertScroll.BeginUpdate;
    for i := Pred(AVertScroll.Content.ChildrenCount) downto 0 do
    begin
      if (AVertScroll.Content.Children[i] is AClassType) then
      begin
        LFrame := TComponent(AVertScroll.Content.Children[i] as AClassType);
        LFrame.DisposeOf;
        LFrame := nil;
      end;

    end;
  finally
    AVertScroll.EndUpdate;
  end;
end;

class procedure TUtils.HideKeyboard(const AComponentSetFocus: TControl = nil);
var
  LService: IFMXVirtualKeyboardService;
begin
  TPlatformServices.Current.SupportsPlatformService(IFMXVirtualKeyboardService, IInterface(LService));
  if (LService <> nil) then
  begin
    LService.HideVirtualKeyboard;
  end;

  if (Assigned(AComponentSetFocus)) then
    SetFocus(AComponentSetFocus);
end;

class procedure TUtils.ShowKeyboard(const AControl: TFmxObject);
var
  LService: IFMXVirtualKeyboardService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXVirtualKeyboardService, LService) then
  begin
    LService.ShowVirtualKeyboard(AControl);
  end;
end;

class procedure TUtils.SetFocus(const AComponent: TControl);
var
  LCanFocus: Boolean;
begin
  TThread.CreateAnonymousThread(
    procedure
    begin
      TThread.Synchronize(nil,
        procedure()
        var
          LTabStopBefore: Boolean;
        begin
          LTabStopBefore := AComponent.TabStop;
          AComponent.TabStop := True;
          AComponent.SetFocus;
          AComponent.TabStop := LTabStopBefore;
        end);

    end).Start;
end;

class function TUtils.IsAssigned(AObj: TFmxObject): Boolean;
begin
  try
    Result := Assigned(AObj) and not(Trim(AObj.Name).IsEmpty);
  except
    Result := False;
  end;
end;

class procedure TUtils.ChangeTab(ATabControl: TTabControl; ATabItem: TTabItem;
AOnFinish: TNotifyEvent);
begin
  TThread.Synchronize(nil,
    procedure
    begin
      if Assigned(AOnFinish) then
      begin
        var
        LDirection := TTabTransitionDirection.Normal;
        if ATabControl.TabIndex > ATabItem.Index then
          LDirection := TTabTransitionDirection.Reversed;

        ATabControl.SetActiveTabWithTransitionAsync(
          ATabItem,
          TTabTransition.Slide,
          LDirection,
          AOnFinish
          );
      end
      else
      begin
        // ATabControl.SetActiveTabWithTransition(ATabItem, TTabTransition.Slide);
        // ATabControl.ActiveTab := ATabItem;
        FChangeTabAction.Tab := ATabItem;
        FChangeTabAction.ExecuteTarget(ATabControl);
      end;
    end);
end;

class procedure TUtils.OpenForm(const AFrmClass: TComponentClass;
ATarget: TFmxObject; var AFrmActive: FMX.Forms.TForm; AMainMenu: TMultiView;
const AMasterButtonName: string; AOwner: TFmxObject;
const AIsOwnerNil: Boolean);
var
  LLytBase: TComponent;
  LBtnMenu: TComponent;
  AFrmActiveToRemove: FMX.Forms.TForm;
  LFrmActiveName: string;
begin
  if (Assigned(AFrmActive)) then
  begin
    if (AFrmActive.ClassType = AFrmClass) then
    begin
      Exit;
    end
    else
    begin
      LFrmActiveName := AFrmActive.Name;
      AFrmActive.DisposeOf;

      AFrmActiveToRemove :=
        FMX.Forms.TForm(ATarget.FindComponent(LFrmActiveName));
      if (AFrmActiveToRemove <> nil) then
        ATarget.RemoveObject(AFrmActiveToRemove);
    end;
  end;

  if AIsOwnerNil then
    AFrmActive := FMX.Forms.TForm(AFrmClass.Create(nil))
  else
    if (AOwner <> nil) then
    AFrmActive := FMX.Forms.TForm(AFrmClass.Create(AOwner))
  else
    Application.CreateForm(AFrmClass, AFrmActive);
  // AFrmActive := FMX.Forms.TForm(AFrmClass.Create(nil));

  LLytBase := AFrmActive.FindComponent('lytBackground');
  LBtnMenu := nil;
  if not(AMasterButtonName.IsEmpty) then
    LBtnMenu := AFrmActive.FindComponent(AMasterButtonName);
  if (Assigned(LLytBase)) then
  begin
    TLayout(ATarget).AddObject(TLayout(LLytBase));

    if (Assigned(AMainMenu)) then
    begin
      if Assigned(LBtnMenu) then
        AMainMenu.MasterButton := TButton(LBtnMenu);
      AMainMenu.HideMaster;
    end;
  end;
end;

class procedure TUtils.OpenFrame(const AFrmClass: TComponentClass;
ATarget: TFmxObject; var AFrmActive: FMX.Forms.TFrame; AMainMenu: TMultiView;
const AMasterButtonName: string; AOwner: TFmxObject;
const AIsOwnerNil: Boolean);
var
  LLytBase: TComponent;
  LBtnMenu: TComponent;
  AFrmActiveToRemove: FMX.Forms.TFrame;
  LFrmActiveName: string;
begin
  if (Assigned(AFrmActive)) then
  begin
    if (AFrmActive.ClassType = AFrmClass) then
    begin
      Exit;
    end
    else
    begin
      LFrmActiveName := AFrmActive.Name;

      AFrmActiveToRemove := FMX.Forms.TFrame
        (ATarget.FindComponent(LFrmActiveName));
      if (AFrmActiveToRemove <> nil) then
        ATarget.RemoveObject(AFrmActiveToRemove);
      try
        FreeAndNil(AFrmActive);
      except
        AFrmActive := nil;
      end;
    end;
  end;

  if AIsOwnerNil then
    AFrmActive := FMX.Forms.TFrame(AFrmClass.Create(nil))
  else
    if (AOwner <> nil) then
    AFrmActive := FMX.Forms.TFrame(AFrmClass.Create(AOwner))
  else
    Application.CreateForm(AFrmClass, AFrmActive);
  // AFrmActive := FMX.Forms.TFrame(AFrmClass.Create(nil));

  LLytBase := AFrmActive.FindComponent('lytBackground');
  LBtnMenu := nil;
  if not(AMasterButtonName.IsEmpty) then
    LBtnMenu := AFrmActive.FindComponent(AMasterButtonName);
  if (Assigned(LLytBase)) then
  begin
    TLayout(ATarget).AddObject(TLayout(LLytBase));

    if (Assigned(AMainMenu)) then
    begin
      if Assigned(LBtnMenu) then
        AMainMenu.MasterButton := TSpeedButton(LBtnMenu);
      AMainMenu.HideMaster;
    end;
  end;
end;

class function TUtils.ImgListGetImageByName(const AName: string;
AImgList: FMX.ImgList.TImageList): FMX.Graphics.TBitmap;
var
  LItem: TCustomBitmapItem;
  LSize: TSize;
begin
  Result := nil;
  if AImgList.BitmapItemByName(AName, LItem, LSize) then
    Result := LItem.MultiResBitmap.Bitmaps[1.0];
end;

class function TUtils.IsVirtualKeyboardShwon: Boolean;
var
  FService: IFMXVirtualKeyboardService;
begin
  TPlatformServices.Current.SupportsPlatformService(IFMXVirtualKeyboardService, IInterface(FService));
  Result := TVirtualKeyboardState.Visible in FService.VirtualKeyboardState;
end;
{$ENDIF}

class function TUtils.HasCallService: Boolean;
begin
{$IFDEF FMX}
  // test whether the PhoneDialer services are supported
  Result := TPlatformServices.Current.SupportsPlatformService
    (IFMXPhoneDialerService);
{$ELSE}
  Result := False;
{$ENDIF}
end;

class function TUtils.CallNumber(const AValue: string): Boolean;
var
  LValue: string;
{$IFDEF FMX}
  PhoneDialerService: IFMXPhoneDialerService;
{$ENDIF}
begin
  Result := False;
  if HasCallService then
  begin
{$IFDEF FMX}
    LValue := AValue;
    LValue := StringReplace(LValue, ' ', EmptyStr, [rfReplaceAll]);
    LValue := StringReplace(LValue, '-', EmptyStr, [rfReplaceAll]);
    if not LValue.Trim.IsEmpty then
    begin
      // test whether the PhoneDialer services are supported
      if TPlatformServices.Current.SupportsPlatformService(IFMXPhoneDialerService,
        IInterface(PhoneDialerService))
      then
      begin
        PhoneDialerService.Call(LValue);
        Result := True;
      end;
    end;
{$ELSE}
    raise Exception.Create('TUtils.CallNumber');
{$ENDIF}
  end;
end;

class function TUtils.ClipboardCopyText(const AValue: string): Boolean;
{$IFDEF FMX}
var
  Svc: IFMXClipboardService;
{$ENDIF}
begin
  Result := False;
{$IFDEF FMX}
  if TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, Svc)
  then
  begin
    Svc.SetClipboard(AValue);
    Result := True;
  end;
{$ELSEIF defined(MSWINDOWS)}
  Clipboard.AsText := AValue;
{$ELSE}
  TUtils.ThrowExceptionMethodIsNotImplemented('IdeaL.Lib.Utils.TUtils.ClipboardCopyText');
{$ENDIF}
end;


class function TUtils.NowToUniversalTime: TDateTime;
begin
  Result := TTimeZone.Local.ToUniversalTime(Now);
end;

class function TUtils.OpenApkFile(const AFile: string): Boolean;
var
{$IFDEF ANDROID}
  LJFile: Jfile;
  Intent: JIntent;
  VUriArquivo: Jnet_Uri;
  LPath: string;
  LName: string;
  LJPath: JString;
  LJName: JString;
{$ENDIF}
  LStrFileProvider: string;
  LVersion: string;
begin
  Result := False;
  LVersion := GetOsVersion;
{$IFDEF ANDROID}
  if (Pos('.', LVersion) > 0) then
    LVersion := Copy(LVersion, 0, Pos('.', LVersion) - 1);

  VarTryStrToInt := 0;
  if not(TryStrToInt(LVersion, VarTryStrToInt)) or //
    (VarTryStrToInt = 0) //
  then
    raise Exception.Create('Version [' + LVersion + ']is not valid');

  if (VarTryStrToInt >= 7) then
  begin
    LPath := System.IOUtils.TPath.GetDirectoryName(AFile);
    LName := System.IOUtils.TPath.GetFileName(AFile);
    LJPath := Androidapi.Helpers.StringToJString(LPath);
    LJName := Androidapi.Helpers.StringToJString(LName);

    LJFile := TJfile.JavaClass.init(LJPath, LJName);

    LStrFileProvider := JStringToString
      (TAndroidHelper.Context.getApplicationContext.getPackageName) +
      '.fileprovider';

    VUriArquivo := {$IF CompilerVersion >= 35.0}TJcontent_FileProvider{$ELSE}TJFileProvider{$ENDIF}.JavaClass.getUriForFile
      (TAndroidHelper.Context,
      Androidapi.Helpers.StringToJString
      (LStrFileProvider),
      LJFile
      );

    Intent := TJIntent.Create;
    Intent.setAction(TJIntent.JavaClass.ACTION_VIEW);
    Intent.addFlags(TJIntent.JavaClass.FLAG_ACTIVITY_NEW_TASK);
    Intent.addFlags(TJIntent.JavaClass.FLAG_GRANT_READ_URI_PERMISSION);
    Intent.setDataAndType(VUriArquivo,
      Androidapi.Helpers.StringToJString
      ('application/vnd.android.package-archive'));
    SharedActivityContext.startActivity(Intent);
  end
  else
  begin
    { Intent := TJIntent.Create;
      Intent.setAction(TJIntent.JavaClass.ACTION_VIEW);
      Intent.setData(StrToJURI(AFile));
      TAndroidHelper.Activity.startActivity(Intent); }

    // aFile  := TJfile.JavaClass.Init(stringtojstring(GetDirDownloads), StringToJString(VApkFileNameDownloaded));

    LPath := System.IOUtils.TPath.GetDirectoryName(AFile);
    LName := System.IOUtils.TPath.GetFileName(AFile);
    LJPath := Androidapi.Helpers.StringToJString(LPath);
    LJName := Androidapi.Helpers.StringToJString(LName);

    LJFile := TJfile.JavaClass.init(LJPath, LJName);

    Intent := TJIntent.Create;
    Intent.setAction(TJIntent.JavaClass.ACTION_VIEW);
    Intent.addFlags(TJIntent.JavaClass.FLAG_ACTIVITY_NEW_TASK);
    Intent.setDataAndType(TJnet_Uri.JavaClass.fromFile(LJFile),
      StringToJString('application/vnd.android.package-archive'));

    SharedActivityContext.startActivity(Intent);
  end;
{$ENDIF}
  Result := True;
end;

class procedure TUtils.OpenExternalApp(const AAppName: string);
{$IFDEF ANDROID}
var
  Intent: JIntent;
{$ENDIF}
begin
{$IFDEF ANDROID}
  Intent := TJIntent.Create;
  Intent :=
    SharedActivity.getPackageManager.getLaunchIntentForPackage(
    StringToJString(AAppName)
    );
  SharedActivity.startActivity(Intent);
  { Intent := TJIntent.Create;
    Intent.setPackage(StringToJString(AAppName));
    Intent.setAction(TJIntent.JavaClass.ACTION_MAIN);
    SharedActivity.startActivity(Intent); }
{$ELSE}
  raise Exception.Create('OpenExternalApp not implemented');
{$ENDIF}
end;

class procedure TUtils.OpenGooglePlayStore(const APackageName: string);
begin
  OpenUrl('market://details?id=' + APackageName);
end;

class procedure TUtils.OpenMap(const AAdsress: string);
begin
{$IF defined(ANDROID)}
  // TUtils.OpenUrl('http://maps.google.com/maps?q=' + AAdsress.Trim([',', ' ']));
  OpenMapShareSheet(AAdsress.Trim([',', ' ']));
{$ELSEIF defined(IOS)}
  // TUtils.OpenUrl(UrlEncode('http://maps.apple.com?daddr=' + AAdsress.Trim([',', ' '])));
  TUtils.OpenUrl(System.Net.URLClient.TURI.URLEncode('http://maps.google.com/maps?q=' + AAdsress.Trim([',', ' '])));
{$ELSE}
  ThrowExceptionMethodIsNotImplemented('TUtils.OpenMap');
{$ENDIF}
end;

class procedure TUtils.OpenMapShareSheet(ALat, ALong: Single);
begin
  OpenMapShareSheet(ALat, ALong, EmptyStr);
end;

class procedure TUtils.OpenMapShareSheet(AAddress: string);
begin
  OpenMapShareSheet(0, 0, AAddress);
end;

class procedure TUtils.OpenMapShareSheet(ALat, ALong: Single; AAddress: string);
var
{$IF defined(ANDROID)}
  LIntent: JIntent;
{$ENDIF}
  LUriStr: string;
begin
{$IF defined(ANDROID)}
  try
    LUriStr := EmptyStr;
    if (ALat > 0) and (ALong > 0) then
    begin
      LUriStr := Format('geo:%f,%f', [ALat, ALong]);
      AAddress := EmptyStr;
    end
    else
      LUriStr := 'geo:0,0';

    if not AAddress.Trim.IsEmpty then
      LUriStr := LUriStr + '?q=' + StringReplace(AAddress, '''', EmptyStr, [rfReplaceAll]);

    LIntent := TJIntent.Create;
    LIntent.setAction(TJIntent.JavaClass.ACTION_VIEW);
    LIntent.setData(StrToJURI(LUriStr));
    TAndroidHelper.Activity.startActivity(LIntent);
  except
    on E: Exception do
      raise Exception.Create('TUtils.OpenMapShareSheet' + sLineBreak + E.Message);
  end;
{$ELSE}
  ThrowExceptionMethodIsNotImplemented('TUtils.OpenMapShareSheet');
{$ENDIF}
end;

class procedure TUtils.OpenUrl(const AUrl: string);
{$IFDEF ANDROID}
var
  Intent: JIntent;
{$ELSEIF defined(IOS)}
var
    App : UIApplication;
    url : NSurl;
{$ENDIF}
begin
{$IF defined(ANDROID)}
  if GetOsVersionInt >= 12 then
  begin
	if (not AUrl.ToLower.StartsWith('http')) and
      (not AUrl.ToLower.StartsWith('market://'))
    then
      raise Exception.Create('URL must contain HTTP or HTTPS or MARKET: ' + AUrl);
  end;

  Intent := TJIntent.Create;
  Intent.setAction(TJIntent.JavaClass.ACTION_VIEW);
  Intent.setData(StrToJURI(AUrl));
  TAndroidHelper.Activity.startActivity(Intent);
{$ELSEIF defined(MSWINDOWS)}
  ShellExecute(0, 'OPEN', PWideChar(AUrl), nil, nil, SW_SHOWNORMAL);
{$ELSEIF defined(IOS)}
{$IFDEF KASTRI}
  TOSDevice.OpenURL(AUrl);
{$ELSE}

  if SharedApplication.canOpenURL(StrToNSUrl(AUrl)) then
    SharedApplication.OpenUrl(StrToNSUrl(AUrl))
  else
    raise Exception.Create('TUtils.OpenUrl ' + AUrl);
{$ENDIF}
{$ELSEIF defined(MACOS)}
  _system(PAnsiChar('open ' + AnsiString(AUrl)));
{$ELSE}
  ThrowExceptionMethodIsNotImplemented('TUtils.OpenUrl');
{$IFEND}
end;

class function TUtils.GetDeviceId: string;
{$IF defined(iOS)}
var
  Device: UIDevice;
{$ELSEIF defined(MSWINDOWS)}
var
  NotUsed, VolFlags: DWORD;
  VolSN: DWORD;
  VolumeSerialNumber: string;
{$IFEND}
begin
{$IF defined(ANDROID)}
  Result :=
    JStringToString(
    TJSettings_Secure.JavaClass.getString(
    TAndroidHelper.Activity.getContentResolver,
    TJSettings_Secure.JavaClass.ANDROID_ID
    ));
{$ELSEIF defined(iOS)}
  Device := TUIDevice.Wrap(TUIDevice.OCClass.currentDevice);
  Result := string(Device.identifierForVendor.UUIDString.UTF8String);
{$ELSEIF defined(MSWINDOWS)}
  SetCurrentDirectory(PChar(ExtractFilePath(GetCurrentDir)));
  GetVolumeInformation(nil, nil, 0, @VolSN, NotUsed, VolFlags, nil, 0);
  VolumeSerialNumber := AnsiUpperCase(IntToHex(VolSN, 8));
  Result := Copy(VolumeSerialNumber, 1, 4) + '-' + Copy(VolumeSerialNumber, 5, 4);
{$ELSE}
  ThrowExceptionMethodIsNotImplemented('TUtils.GetDeviceId')
{$IFEND}
end;

class function TUtils.GetDeviceModel: string;
{$IF defined(ANDROID)}
{$ELSEIF defined(IOS)}
var
  LSize: size_t;
  LDeviceModelBuffer: array of Byte;
{$ELSE}  
{$IFEND}
begin
{$IF defined(ANDROID)}
  Result := JStringToString(TJBuild.JavaClass.MODEL);
{$ELSEIF defined(IOS)}
   Result := EmptyStr;
  try
    sysctlbyname('hw.machine', nil, @LSize, nil, 0);
    if LSize > 0 then
    begin
      SetLength(LDeviceModelBuffer, LSize);
      sysctlbyname('hw.machine', @LDeviceModelBuffer[0], @LSize, nil, 0);
      Result := UTF8ToString(MarshaledAString(LDeviceModelBuffer));
      var
      LDictModel := TDictionary<string, string>.Create;
      try
{$REGION 'Fill data in'}
        {
          checkout this link https://gist.github.com/adamawolf/3048717
          Keep it in Alphabetic order, please
        }
        LDictModel.AddOrSetValue('i386', 'iPhone Simulator');
        LDictModel.AddOrSetValue('x86_64', 'iPhone Simulator');
        LDictModel.AddOrSetValue('arm64', 'iPhone Simulator');
        LDictModel.AddOrSetValue('iPhone1,1', 'iPhone');
        LDictModel.AddOrSetValue('iPhone1,2', 'iPhone 3G');
        LDictModel.AddOrSetValue('iPhone2,1', 'iPhone 3GS');
        LDictModel.AddOrSetValue('iPhone3,1', 'iPhone 4');
        LDictModel.AddOrSetValue('iPhone3,2', 'iPhone 4 GSM Rev A');
        LDictModel.AddOrSetValue('iPhone3,3', 'iPhone 4 CDMA');
        LDictModel.AddOrSetValue('iPhone4,1', 'iPhone 4S');
        LDictModel.AddOrSetValue('iPhone5,1', 'iPhone 5 (GSM)');
        LDictModel.AddOrSetValue('iPhone5,2', 'iPhone 5 (GSM+CDMA)');
        LDictModel.AddOrSetValue('iPhone5,3', 'iPhone 5C (GSM)');
        LDictModel.AddOrSetValue('iPhone5,4', 'iPhone 5C (Global)');
        LDictModel.AddOrSetValue('iPhone6,1', 'iPhone 5S (GSM)');
        LDictModel.AddOrSetValue('iPhone6,2', 'iPhone 5S (Global)');
        LDictModel.AddOrSetValue('iPhone7,1', 'iPhone 6 Plus');
        LDictModel.AddOrSetValue('iPhone7,2', 'iPhone 6');
        LDictModel.AddOrSetValue('iPhone8,1', 'iPhone 6s');
        LDictModel.AddOrSetValue('iPhone8,2', 'iPhone 6s Plus');
        LDictModel.AddOrSetValue('iPhone8,4', 'iPhone SE (GSM)');
        LDictModel.AddOrSetValue('iPhone9,1', 'iPhone 7');
        LDictModel.AddOrSetValue('iPhone9,2', 'iPhone 7 Plus');
        LDictModel.AddOrSetValue('iPhone9,3', 'iPhone 7');
        LDictModel.AddOrSetValue('iPhone9,4', 'iPhone 7 Plus');
        LDictModel.AddOrSetValue('iPhone10,1', 'iPhone 8');
        LDictModel.AddOrSetValue('iPhone10,2', 'iPhone 8 Plus');
        LDictModel.AddOrSetValue('iPhone10,3', 'iPhone X Global');
        LDictModel.AddOrSetValue('iPhone10,4', 'iPhone 8');
        LDictModel.AddOrSetValue('iPhone10,5', 'iPhone 8 Plus');
        LDictModel.AddOrSetValue('iPhone10,6', 'iPhone X GSM');
        LDictModel.AddOrSetValue('iPhone11,2', 'iPhone XS');
        LDictModel.AddOrSetValue('iPhone11,4', 'iPhone XS Max');
        LDictModel.AddOrSetValue('iPhone11,6', 'iPhone XS Max Global');
        LDictModel.AddOrSetValue('iPhone11,8', 'iPhone XR');
        LDictModel.AddOrSetValue('iPhone12,1', 'iPhone 11');
        LDictModel.AddOrSetValue('iPhone12,3', 'iPhone 11 Pro');
        LDictModel.AddOrSetValue('iPhone12,5', 'iPhone 11 Pro Max');
        LDictModel.AddOrSetValue('iPhone12,8', 'iPhone SE 2nd Gen');
        LDictModel.AddOrSetValue('iPhone13,1', 'iPhone 12 Mini');
        LDictModel.AddOrSetValue('iPhone13,2', 'iPhone 12');
        LDictModel.AddOrSetValue('iPhone13,3', 'iPhone 12 Pro');
        LDictModel.AddOrSetValue('iPhone13,4', 'iPhone 12 Pro Max');
        LDictModel.AddOrSetValue('iPhone14,2', 'iPhone 13 Pro');
        LDictModel.AddOrSetValue('iPhone14,3', 'iPhone 13 Pro Max');
        LDictModel.AddOrSetValue('iPhone14,4', 'iPhone 13 Mini');
        LDictModel.AddOrSetValue('iPhone14,5', 'iPhone 13');
        LDictModel.AddOrSetValue('iPhone14,6', 'iPhone SE 3rd Gen');
        LDictModel.AddOrSetValue('iPhone14,7', 'iPhone 14');
        LDictModel.AddOrSetValue('iPhone14,8', 'iPhone 14 Plus');
        LDictModel.AddOrSetValue('iPhone15,2', 'iPhone 14 Pro');
        LDictModel.AddOrSetValue('iPhone15,3', 'iPhone 14 Pro Max');
        LDictModel.AddOrSetValue('iPhone15,4', 'iPhone 15');
        LDictModel.AddOrSetValue('iPhone15,5', 'iPhone 15 Plus');
        LDictModel.AddOrSetValue('iPhone16,1', 'iPhone 15 Pro');
        LDictModel.AddOrSetValue('iPhone16,2', 'iPhone 15 Pro Max');
        LDictModel.AddOrSetValue('iPhone17,1', 'iPhone 16 Pro');
        LDictModel.AddOrSetValue('iPhone17,2', 'iPhone 16 Pro Max');
        LDictModel.AddOrSetValue('iPhone17,3', 'iPhone 16');
        LDictModel.AddOrSetValue('iPhone17,4', 'iPhone 16 Plus');
        LDictModel.AddOrSetValue('iPhone17,5', 'iPhone 16e');

        LDictModel.AddOrSetValue('iPod1,1', '1st Gen iPod');
        LDictModel.AddOrSetValue('iPod2,1', '2nd Gen iPod');
        LDictModel.AddOrSetValue('iPod3,1', '3rd Gen iPod');
        LDictModel.AddOrSetValue('iPod4,1', '4th Gen iPod');
        LDictModel.AddOrSetValue('iPod5,1', '5th Gen iPod');
        LDictModel.AddOrSetValue('iPod7,1', '6th Gen iPod');
        LDictModel.AddOrSetValue('iPod9,1', '7th Gen iPod');

        LDictModel.AddOrSetValue('iPad1,1', 'iPad');
        LDictModel.AddOrSetValue('iPad1,2', 'iPad 3G');
        LDictModel.AddOrSetValue('iPad2,1', '2nd Gen iPad');
        LDictModel.AddOrSetValue('iPad2,2', '2nd Gen iPad GSM');
        LDictModel.AddOrSetValue('iPad2,3', '2nd Gen iPad CDMA');
        LDictModel.AddOrSetValue('iPad2,4', '2nd Gen iPad New Revision');
        LDictModel.AddOrSetValue('iPad3,1', '3rd Gen iPad');
        LDictModel.AddOrSetValue('iPad3,2', '3rd Gen iPad CDMA');
        LDictModel.AddOrSetValue('iPad3,3', '3rd Gen iPad GSM');
        LDictModel.AddOrSetValue('iPad2,5', 'iPad mini');
        LDictModel.AddOrSetValue('iPad2,6', 'iPad mini GSM+LTE');
        LDictModel.AddOrSetValue('iPad2,7', 'iPad mini CDMA+LTE');
        LDictModel.AddOrSetValue('iPad3,4', '4th Gen iPad');
        LDictModel.AddOrSetValue('iPad3,5', '4th Gen iPad GSM+LTE');
        LDictModel.AddOrSetValue('iPad3,6', '4th Gen iPad CDMA+LTE');
        LDictModel.AddOrSetValue('iPad4,1', 'iPad Air (WiFi)');
        LDictModel.AddOrSetValue('iPad4,2', 'iPad Air (GSM+CDMA)');
        LDictModel.AddOrSetValue('iPad4,3', '1st Gen iPad Air (China)');
        LDictModel.AddOrSetValue('iPad4,4', 'iPad mini Retina (WiFi)');
        LDictModel.AddOrSetValue('iPad4,5', 'iPad mini Retina (GSM+CDMA)');
        LDictModel.AddOrSetValue('iPad4,6', 'iPad mini Retina (China)');
        LDictModel.AddOrSetValue('iPad4,7', 'iPad mini 3 (WiFi)');
        LDictModel.AddOrSetValue('iPad4,8', 'iPad mini 3 (GSM+CDMA)');
        LDictModel.AddOrSetValue('iPad4,9', 'iPad Mini 3 (China)');
        LDictModel.AddOrSetValue('iPad5,1', 'iPad mini 4 (WiFi)');
        LDictModel.AddOrSetValue('iPad5,2', '4th Gen iPad mini (WiFi+Cellular)');
        LDictModel.AddOrSetValue('iPad5,3', 'iPad Air 2 (WiFi)');
        LDictModel.AddOrSetValue('iPad5,4', 'iPad Air 2 (Cellular)');
        LDictModel.AddOrSetValue('iPad6,3', 'iPad Pro (9.7 inch, WiFi)');
        LDictModel.AddOrSetValue('iPad6,4', 'iPad Pro (9.7 inch, WiFi+LTE)');
        LDictModel.AddOrSetValue('iPad6,7', 'iPad Pro (12.9 inch, WiFi)');
        LDictModel.AddOrSetValue('iPad6,8', 'iPad Pro (12.9 inch, WiFi+LTE)');
        LDictModel.AddOrSetValue('iPad6,11', 'iPad (2017)');
        LDictModel.AddOrSetValue('iPad6,12', 'iPad (2017)');
        LDictModel.AddOrSetValue('iPad7,1', 'iPad Pro 2nd Gen (WiFi)');
        LDictModel.AddOrSetValue('iPad7,2', 'iPad Pro 2nd Gen (WiFi+Cellular)');
        LDictModel.AddOrSetValue('iPad7,3', 'iPad Pro 10.5-inch 2nd Gen');
        LDictModel.AddOrSetValue('iPad7,4', 'iPad Pro 10.5-inch 2nd Gen');
        LDictModel.AddOrSetValue('iPad7,5', 'iPad 6th Gen (WiFi)');
        LDictModel.AddOrSetValue('iPad7,6', 'iPad 6th Gen (WiFi+Cellular)');
        LDictModel.AddOrSetValue('iPad7,11', 'iPad 7th Gen 10.2-inch (WiFi)');
        LDictModel.AddOrSetValue('iPad7,12', 'iPad 7th Gen 10.2-inch (WiFi+Cellular)');
        LDictModel.AddOrSetValue('iPad8,1', 'iPad Pro 11 inch 3rd Gen (WiFi)');
        LDictModel.AddOrSetValue('iPad8,2', 'iPad Pro 11 inch 3rd Gen (1TB, WiFi)');
        LDictModel.AddOrSetValue('iPad8,3', 'iPad Pro 11 inch 3rd Gen (WiFi+Cellular)');
        LDictModel.AddOrSetValue('iPad8,4', 'iPad Pro 11 inch 3rd Gen (1TB, WiFi+Cellular)');
        LDictModel.AddOrSetValue('iPad8,5', 'iPad Pro 12.9 inch 3rd Gen (WiFi)');
        LDictModel.AddOrSetValue('iPad8,6', 'iPad Pro 12.9 inch 3rd Gen (1TB, WiFi)');
        LDictModel.AddOrSetValue('iPad8,7', 'iPad Pro 12.9 inch 3rd Gen (WiFi+Cellular)');
        LDictModel.AddOrSetValue('iPad8,8', 'iPad Pro 12.9 inch 3rd Gen (1TB, WiFi+Cellular)');
        LDictModel.AddOrSetValue('iPad8,9', 'iPad Pro 11 inch 4th Gen (WiFi)');
        LDictModel.AddOrSetValue('iPad8,10', 'iPad Pro 11 inch 4th Gen (WiFi+Cellular)');
        LDictModel.AddOrSetValue('iPad8,11', 'iPad Pro 12.9 inch 4th Gen (WiFi)');
        LDictModel.AddOrSetValue('iPad8,12', 'iPad Pro 12.9 inch 4th Gen (WiFi+Cellular)');
        LDictModel.AddOrSetValue('iPad11,1', 'iPad mini 5th Gen (WiFi)');
        LDictModel.AddOrSetValue('iPad11,2', 'iPad mini 5th Gen');
        LDictModel.AddOrSetValue('iPad11,3', 'iPad Air 3rd Gen (WiFi)');
        LDictModel.AddOrSetValue('iPad11,4', 'iPad Air 3rd Gen');
        LDictModel.AddOrSetValue('iPad11,6', 'iPad 8th Gen (WiFi)');
        LDictModel.AddOrSetValue('iPad11,7', 'iPad 8th Gen (WiFi+Cellular)');
        LDictModel.AddOrSetValue('iPad12,1', 'iPad 9th Gen (WiFi)');
        LDictModel.AddOrSetValue('iPad12,2', 'iPad 9th Gen (WiFi+Cellular)');
        LDictModel.AddOrSetValue('iPad14,1', 'iPad mini 6th Gen (WiFi)');
        LDictModel.AddOrSetValue('iPad14,2', 'iPad mini 6th Gen (WiFi+Cellular)');
        LDictModel.AddOrSetValue('iPad13,1', 'iPad Air 4th Gen (WiFi)');
        LDictModel.AddOrSetValue('iPad13,2', 'iPad Air 4th Gen (WiFi+Cellular)');
        LDictModel.AddOrSetValue('iPad13,4', 'iPad Pro 11 inch 5th Gen');
        LDictModel.AddOrSetValue('iPad13,5', 'iPad Pro 11 inch 5th Gen');
        LDictModel.AddOrSetValue('iPad13,6', 'iPad Pro 11 inch 5th Gen');
        LDictModel.AddOrSetValue('iPad13,7', 'iPad Pro 11 inch 5th Gen');
        LDictModel.AddOrSetValue('iPad13,8', 'iPad Pro 12.9 inch 5th Gen');
        LDictModel.AddOrSetValue('iPad13,9', 'iPad Pro 12.9 inch 5th Gen');
        LDictModel.AddOrSetValue('iPad13,10', 'iPad Pro 12.9 inch 5th Gen');
        LDictModel.AddOrSetValue('iPad13,11', 'iPad Pro 12.9 inch 5th Gen');
        LDictModel.AddOrSetValue('iPad13,16', 'iPad Air 5th Gen (WiFi)');
        LDictModel.AddOrSetValue('iPad13,17', 'iPad Air 5th Gen (WiFi+Cellular)');
        LDictModel.AddOrSetValue('iPad13,18', 'iPad 10th Gen');
        LDictModel.AddOrSetValue('iPad13,19', 'iPad 10th Gen');
        LDictModel.AddOrSetValue('iPad14,3', 'iPad Pro 11 inch 4th Gen');
        LDictModel.AddOrSetValue('iPad14,4', 'iPad Pro 11 inch 4th Gen');
        LDictModel.AddOrSetValue('iPad14,5', 'iPad Pro 12.9 inch 6th Gen');
        LDictModel.AddOrSetValue('iPad14,6', 'iPad Pro 12.9 inch 6th Gen');
        LDictModel.AddOrSetValue('iPad14,8', 'iPad Air 11 inch 6th Gen (WiFi)');
        LDictModel.AddOrSetValue('iPad14,9', 'iPad Air 11 inch 6th Gen (WiFi+Cellular)');
        LDictModel.AddOrSetValue('iPad14,10', 'iPad Air 13 inch 6th Gen (WiFi)');
        LDictModel.AddOrSetValue('iPad14,11', 'iPad Air 13 inch 6th Gen (WiFi+Cellular)');
        LDictModel.AddOrSetValue('iPad15,3', 'iPad Air 11-inch 7th Gen (WiFi)');
        LDictModel.AddOrSetValue('iPad15,4', 'iPad Air 11-inch 7th Gen (WiFi+Cellular)');
        LDictModel.AddOrSetValue('iPad15,5', 'iPad Air 13-inch 7th Gen (WiFi)');
        LDictModel.AddOrSetValue('iPad15,6', 'iPad Air 13-inch 7th Gen (WiFi+Cellular)');
        LDictModel.AddOrSetValue('iPad15,7', 'iPad 11th Gen (WiFi)');
        LDictModel.AddOrSetValue('iPad15,8', 'iPad 11th Gen (WiFi+Cellular)');
        LDictModel.AddOrSetValue('iPad16,1', 'iPad mini 7th Gen (WiFi)');
        LDictModel.AddOrSetValue('iPad16,2', 'iPad mini 7th Gen (WiFi+Cellular)');
        LDictModel.AddOrSetValue('iPad16,3', 'iPad Pro 11 inch 5th Gen');
        LDictModel.AddOrSetValue('iPad16,4', 'iPad Pro 11 inch 5th Gen');
        LDictModel.AddOrSetValue('iPad16,5', 'iPad Pro 12.9 inch 7th Gen');
        LDictModel.AddOrSetValue('iPad16,6', 'iPad Pro 12.9 inch 7th Gen');

        LDictModel.AddOrSetValue('Watch1,1', 'Apple Watch 38mm case');
        LDictModel.AddOrSetValue('Watch1,2', 'Apple Watch 42mm case');
        LDictModel.AddOrSetValue('Watch2,6', 'Apple Watch Series 1 38mm case');
        LDictModel.AddOrSetValue('Watch2,7', 'Apple Watch Series 1 42mm case');
        LDictModel.AddOrSetValue('Watch2,3', 'Apple Watch Series 2 38mm case');
        LDictModel.AddOrSetValue('Watch2,4', 'Apple Watch Series 2 42mm case');
        LDictModel.AddOrSetValue('Watch3,1', 'Apple Watch Series 3 38mm case (GPS+Cellular)');
        LDictModel.AddOrSetValue('Watch3,2', 'Apple Watch Series 3 42mm case (GPS+Cellular)');
        LDictModel.AddOrSetValue('Watch3,3', 'Apple Watch Series 3 38mm case (GPS)');
        LDictModel.AddOrSetValue('Watch3,4', 'Apple Watch Series 3 42mm case (GPS)');
        LDictModel.AddOrSetValue('Watch4,1', 'Apple Watch Series 4 40mm case (GPS)');
        LDictModel.AddOrSetValue('Watch4,2', 'Apple Watch Series 4 44mm case (GPS)');
        LDictModel.AddOrSetValue('Watch4,3', 'Apple Watch Series 4 40mm case (GPS+Cellular)');
        LDictModel.AddOrSetValue('Watch4,4', 'Apple Watch Series 4 44mm case (GPS+Cellular)');
        LDictModel.AddOrSetValue('Watch5,1', 'Apple Watch Series 5 40mm case (GPS)');
        LDictModel.AddOrSetValue('Watch5,2', 'Apple Watch Series 5 44mm case (GPS)');
        LDictModel.AddOrSetValue('Watch5,3', 'Apple Watch Series 5 40mm case (GPS+Cellular)');
        LDictModel.AddOrSetValue('Watch5,4', 'Apple Watch Series 5 44mm case (GPS+Cellular)');
        LDictModel.AddOrSetValue('Watch5,9', 'Apple Watch SE 40mm case (GPS)');
        LDictModel.AddOrSetValue('Watch5,10', 'Apple Watch SE 44mm case (GPS)');
        LDictModel.AddOrSetValue('Watch5,11', 'Apple Watch SE 40mm case (GPS+Cellular)');
        LDictModel.AddOrSetValue('Watch5,12', 'Apple Watch SE 44mm case (GPS+Cellular)');
        LDictModel.AddOrSetValue('Watch6,1', 'Apple Watch Series 6 40mm case (GPS)');
        LDictModel.AddOrSetValue('Watch6,2', 'Apple Watch Series 6 44mm case (GPS)');
        LDictModel.AddOrSetValue('Watch6,3', 'Apple Watch Series 6 40mm case (GPS+Cellular)');
        LDictModel.AddOrSetValue('Watch6,4', 'Apple Watch Series 6 44mm case (GPS+Cellular)');
        LDictModel.AddOrSetValue('Watch6,6', 'Apple Watch Series 7 41mm case (GPS)');
        LDictModel.AddOrSetValue('Watch6,7', 'Apple Watch Series 7 45mm case (GPS)');
        LDictModel.AddOrSetValue('Watch6,8', 'Apple Watch Series 7 41mm case (GPS+Cellular)');
        LDictModel.AddOrSetValue('Watch6,9', 'Apple Watch Series 7 45mm case (GPS+Cellular)');
        LDictModel.AddOrSetValue('Watch6,10', 'Apple Watch SE 40mm case (GPS)');
        LDictModel.AddOrSetValue('Watch6,11', 'Apple Watch SE 44mm case (GPS)');
        LDictModel.AddOrSetValue('Watch6,12', 'Apple Watch SE 40mm case (GPS+Cellular)');
        LDictModel.AddOrSetValue('Watch6,13', 'Apple Watch SE 44mm case (GPS+Cellular)');
        LDictModel.AddOrSetValue('Watch6,14', 'Apple Watch Series 8 41mm case (GPS)');
        LDictModel.AddOrSetValue('Watch6,15', 'Apple Watch Series 8 45mm case (GPS)');
        LDictModel.AddOrSetValue('Watch6,16', 'Apple Watch Series 8 41mm case (GPS+Cellular)');
        LDictModel.AddOrSetValue('Watch6,17', 'Apple Watch Series 8 45mm case (GPS+Cellular)');
        LDictModel.AddOrSetValue('Watch6,18', 'Apple Watch Ultra');
        LDictModel.AddOrSetValue('Watch7,1', 'Apple Watch Series 9 41mm case (GPS)');
        LDictModel.AddOrSetValue('Watch7,2', 'Apple Watch Series 9 45mm case (GPS)');
        LDictModel.AddOrSetValue('Watch7,3', 'Apple Watch Series 9 41mm case (GPS+Cellular)');
        LDictModel.AddOrSetValue('Watch7,4', 'Apple Watch Series 9 45mm case (GPS+Cellular)');
        LDictModel.AddOrSetValue('Watch7,5', 'Apple Watch Ultra 2');
        LDictModel.AddOrSetValue('Watch7,8', 'Apple Watch Series 10 42mm case (GPS)');
        LDictModel.AddOrSetValue('Watch7,9', 'Apple Watch Series 10 46mm case (GPS');
        LDictModel.AddOrSetValue('Watch7,10', 'Apple Watch Series 10 42mm case (GPS+Cellular)');
        LDictModel.AddOrSetValue('Watch7,11', 'Apple Watch Series 10 46mm (GPS+Cellular)');
{$endregion}
        var
        LModel := EmptyStr;
        if LDictModel.TryGetValue(Result, LModel) then
          Result := LModel;
      finally
        LDictModel.Free;
      end;
    end;
  except
    // Do nothing
  end;
{$ELSE}  
{$IFEND}
end;

class function TUtils.GetDeviceName: string;
begin
  Result := 'Unknown';
{$IF defined(ANDROID)}
  Result := GetDeviceModel;
{$ELSEIF defined(IOS)}
  try
    // Obtém o nome do dispositivo configurado pelo usuário
    Result := NSStrToStr(TUIDevice.Wrap(TUIDevice.OCClass.currentDevice).name);
  except

  end;
{$ELSE}
{$IFEND}
end;

class function TUtils.GetDeviceSerial: string;
var
  NotUsed, VolFlags: DWORD;
  VolSN: DWORD; 
begin
{$IF defined(ANDROID)}
  Result := JStringToString(TJBuild.JavaClass.SERIAL);
{$ELSEIF defined(IOS)}
  { 
  the Serial UDID for iOS is not available since iOS6+
  Check the following articles:
  https://stackoverflow.com/a/13457044
  https://oleb.net/blog/2012/09/udid-apis-in-ios-6/
  }
  Result := GetIMEI;
{$ELSEIF defined(MSWINDOWS)}
  SetCurrentDirectory(PChar(ExtractFilePath(GetCurrentDir)));
  GetVolumeInformation(nil, nil, 0, @VolSN, NotUsed, VolFlags, nil, 0);
  var
  VolumeSerialNumber := AnsiUpperCase(IntToHex(VolSN, 8));
  Result := Copy(VolumeSerialNumber, 1, 4) + '-' + Copy(VolumeSerialNumber, 5, 4);
{$ELSE}
  ThrowExceptionMethodIsNotImplemented('TUtils.GetDeviceSerial')
{$IFEND}
end;

class function TUtils.GetDocumentsTemp(const APathMaster: string): string;
begin // OnProcess
{$IFDEF IOS} // there is no PublicPath on iOS Device https://docwiki.embarcadero.com/Libraries/Athens/en/System.IOUtils.TPath.GetPublicPath
  Result := GetPathDocuments;
{$ELSE}
  Result := System.IOUtils.TPath.GetPublicPath;
{$ENDIF}
  Result := System.IOUtils.TPath.Combine(Result, APathMaster);
  if not(DirectoryExists(Result)) then
    ForceDirectories(Result);
  Result := System.IOUtils.TPath.Combine(Result, 'Temp');
  if not(DirectoryExists(Result)) then
    ForceDirectories(Result);
end;

class function TUtils.GetEnumName<T>(AValue: T): string;
begin
  Result := TRttiEnumerationType.GetName(AValue);
end;

class function TUtils.GetEnumValue<T>(AValue: string): T;
begin
  Result := TRttiEnumerationType.GetValue<T>(AValue);
end;

class function TUtils.GetIMEI: string;
{$IFDEF ANDROID}
var
  obj: JObject;
  tm: JTelephonyManager;
  IMEI: String;
{$ENDIF}
{$IFDEF IOS}
var
  Device: UIDevice;
{$ENDIF IOS}
begin
  Result := 'SEM PERMISSAO';
{$IFDEF MSWINDOWS}
  Result := 'Windows';
{$ENDIF MSWINDOWS}
{$IFDEF ANDROID}
  IMEI := EmptyStr;
  Result := 'SEM PERMISSAO';
  if GetOsVersionInt < 10 then
  begin
    obj := TAndroidHelper.Context.getSystemService
      (TJContext.JavaClass.TELEPHONY_SERVICE);
    if obj <> nil then
    begin
      tm := TJTelephonyManager.Wrap((obj as ILocalObject).GetObjectID);
      if tm <> nil then
        IMEI := JStringToString(tm.GetDeviceId);
    end;
  end;
  if IMEI = '' then
    IMEI := JStringToString(TJSettings_Secure.JavaClass.getString
      (TAndroidHelper.Activity.getContentResolver,
      TJSettings_Secure.JavaClass.ANDROID_ID));

  Result := IMEI;
{$ENDIF ANDROID}
{$IFDEF IOS}
  Device := TUIDevice.Wrap(TUIDevice.OCClass.currentDevice);
  Result := '00'; // Device.uniqueIdentifier.UTF8String;
{$ENDIF IOS}
end;

class function TUtils.GetJsonArraySize(AJson: string): Integer;
var
  LAry: TJSONArray;
begin
  Result := 0;
  AJson := '[' + AJson.Trim(['[', ']']) + ']';
  LAry := TJSONObject.ParseJSONValue(AJson) as TJSONArray;
  try
    Result := LAry.Count;
  finally
    FreeAndNil(LAry);
  end;
end;

class function TUtils.GetJsonObjectFromJsonArray(AJson: string; const AIndex: Integer): string;
var
  LAry: TJSONArray;
  LValue: TJSONValue;
begin
  Result := EmptyStr;
  AJson := '[' + AJson.Trim(['[', ']']) + ']';
  LAry := TJSONObject.ParseJSONValue(AJson) as TJSONArray;
  try
    if AIndex < LAry.Count then
    begin
      LValue := LAry.Items[AIndex];
      Result := LValue.ToString;
    end;
  finally
    FreeAndNil(LAry);
  end;
end;

class function TUtils.GetJsonToClassObject<T>(AJson: string): T;
var
  i: Integer;
begin
  Result := T.Create;

  try
    Result := TJSON.JsonToObject<T>(TJSONObject.ParseJSONValue(AJson) as TJSONObject);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

class function TUtils.GetLocalIp: string;
var
  IPAddress: Integer;
{$IFDEF ANDROID}
  LInterfaces, LAddresses: JEnumeration;
  LAddress: JInetAddress;
  LClassName: string;
{$ENDIF}
begin
  Result := EmptyStr;
{$IFDEF IOS}
{$IFDEF KASTRI}
  var
  LIPAddresses := TConnectivity.GetLocalAddresses;
  if Length(LIPAddresses) = 0 then
    ThrowExceptionCouldNotFindTheResource('IdeaL.Lib.Utils.TUtils.GetLocalIp TConnectivity IPAddresses');
  for var I := Low(LIPAddresses) to High(LIPAddresses) do
  begin
    if LIPAddresses[i].Version = TIPVersion.IPv4 then
    begin
      Result := LIPAddresses[i].IP;
      Break;
    end;
  end;
  if Result.Trim.IsEmpty then
    Result := LIPAddresses[0].IP;
{$ELSE}  
  ThrowExceptionMethodIsNotImplemented('IdeaL.Lib.Utils.TUtils.GetLocalIp iOS');
{$ENDIF}
  // uses System.SysUtils, System.Classes, Macapi.Helpers, Macapi.CoreFoundation, Macapi.CFNetwork;
  {Interface := CFNetworkCopySystemProxySettings;
  if Interface <> nil then
  begin
    NetworkInterfaces := CFArrayGetValueAtIndex(Interface, 0); // Aqui pegamos a primeira interface de rede
    if NetworkInterfaces <> nil then
    begin
      InetAddr := PChar(NetworkInterfaces);
      Result := string(InetAddr); // Aqui você deve tratar o InetAddr para extrair o IP local
    end;
  end;}
{$ELSEIF defined(ANDROID)}
  // Solucao copiada da lib Kastri DW.Connectivity.Android.TPlatformConnectivity.GetLocalAddresses
  LInterfaces := TJNetworkInterface.JavaClass.getNetworkInterfaces;
  while LInterfaces.hasMoreElements do
  begin
    LAddresses := TJNetworkInterface.Wrap(LInterfaces.nextElement).getInetAddresses;
    while LAddresses.hasMoreElements do
    begin
      LAddress := TJInetAddress.Wrap(LAddresses.nextElement);
      if not LAddress.isLoopbackAddress then
      begin
        LClassName := JStringToString(LAddress.getClass.getName);
        if LClassName.Contains('Inet4Address') then
        begin
          var
          LIP := JStringToString(LAddress.getHostAddress);
          if LIP.IndexOf('%') > -1 then
            LIP := LIP.Substring(0, LIP.IndexOf('%'));
          Result := Result + ',' + LIP;
        end;
      end;
    end;
  end;
  
  Result := Result.Trim([',']);
{$ELSE}
  ThrowExceptionMethodIsNotImplemented('IdeaL.Lib.Utils.TUtils.GetLocalIp');
{$ENDIF}
end;

class function TUtils.GetOsPlatform: string;
begin
  Result := GetEnumName<System.SysUtils.TOSVersion.TPlatform>(System.SysUtils.TOSVersion.Platform);
end;

class function TUtils.GetOsVersion: string;
var
  LVersion: string;
{$IFDEF IOS}
  LDevice: UIDevice;
{$ENDIF}
{$IFDEF ANDROID}
{$ENDIF}
begin
  {
    Ver possibilidade de alterar, possui varios outros resultados
  }

{$IF defined(IOS)}
  LDevice := TUIDevice.Wrap(TUIDevice.OCClass.currentDevice);
  LVersion := NSStrToStr(LDevice.systemVersion);
{$ELSEIF defined(ANDROID)}
  LVersion := JStringToString(TJBuild_VERSION.JavaClass.RELEASE);
{$ELSEIF defined(MSWINDOWS) or defined(LINUX)}
  LVersion := System.SysUtils.TOSVersion.Major.ToString;
{$ELSE}
  ThrowExceptionMethodIsNotImplemented('TUtils.GetOsVersion');
{$ENDIF}
  Result := LVersion;
end;

class function TUtils.GetOsVersionInt: Integer;
var
  LVersion: string;
begin
  LVersion := GetOsVersion;
  if Pos('.', LVersion) > 0 then
    LVersion := LeftStr(LVersion, Pos('.', LVersion) - 1);

  Result := StrToInt(LVersion);
end;

class function TUtils.GetParamValueFromJsonObject(const AParamName,
  AJsonObj: string): string;
begin
  TUtils.GetParamValueFromJsonObject(AParamName, AJsonObj, Result, EmptyStr);
end;

class function TUtils.GetParamValueFromJsonObject(const AParamName,
  AJsonObj: string; var AResult: string; const ADefultValue: string): string;
var
  LJsonObj: TJSONObject;
  LJsonValue: TJSONValue;
begin
  try
    Result := ADefultValue;
    LJsonObj := TJSONObject.Create;
    LJsonValue := LJsonObj.ParseJSONValue(AJsonObj);

    if not(Assigned(LJsonValue)) then
      raise Exception.Create('Invalid JSON');

    try
      if LJsonValue.FindValue(AParamName) <> nil then
      begin
        if LJsonValue.FindValue(AParamName) is TJSONObject then
          Result := (LJsonValue.FindValue(AParamName) as TJSONObject).ToString
        else
          if LJsonValue.FindValue(AParamName) is TJSONArray then
          Result := (LJsonValue.FindValue(AParamName) as TJSONArray).ToString
        else
          LJsonValue.TryGetValue<string>(AParamName, Result);
      end;
    except

    end;
    AResult := Result;
  finally
    FreeAndNil(LJsonValue);
    FreeAndNil(LJsonObj);
  end;
end;

class function TUtils.GetParamValueFromJsonObject(const AParamName,
  AJsonObj: string; var AResult: string): string;
begin
  Result := TUtils.GetParamValueFromJsonObject(AParamName, AJsonObj, AResult, EmptyStr);
end;

class function TUtils.GetPathDocuments: string;
begin
  Result := System.IOUtils.TPath.GetDocumentsPath;
end;

class function TUtils.GetPathSharedDocuments: string;
begin
  Result := System.IOUtils.TPath.GetSharedDocumentsPath;
end;

class function TUtils.GetStrJosnEnconded(const AValue: string;
const AIsArray: Boolean): string;
var
  LJsonArr: System.JSON.TJSONArray;
  LJsonObj: System.JSON.TJSONObject;
  LValue: string;
begin
  try
    LJsonArr := nil;
    LJsonObj := nil;
    LValue := REST.JSON.TJSON.JsonEncode(AValue);

    if AIsArray then
    begin
      if not(LValue.StartsWith('[')) then
        LValue := '[' + LValue + ']';
      LJsonArr := System.JSON.TJSONObject.ParseJSONValue
        (TEncoding.UTF8.GetBytes(LValue), 0) as System.JSON.TJSONArray;
      if (LJsonArr = nil) then
        raise Exception.Create('Invalid value from JSON: ' + LValue);
      Result := LJsonArr.ToJSON;
    end
    else
    begin
      LJsonObj := System.JSON.TJSONObject.ParseJSONValue
      // (TEncoding.UTF8.GetBytes(LValue), 0) as System.JSON.TJSONObject;
        (LValue, True) as System.JSON.TJSONObject;
      if (LJsonObj = nil) then
        raise Exception.Create('Invalid value from JSON: ' + LValue);
      Result := LJsonObj.ToJSON;
    end;
  finally
    FreeAndNil(LJsonObj);
    FreeAndNil(LJsonArr);
  end;
end;

class function TUtils.GetWord(const AText: string; const APos: Integer): string;
var
  i: Integer;
  LIndex: Integer;
  LText: string;
begin
  raise Exception.Create('nao implementado');
  LText := AText;
  for i := 0 to Pred(APos) do
  begin
    LIndex := Pos(' ', LText);
    if LIndex > 0 then
    begin
      Result := Copy(LText, 1, LIndex - 1);
    end
    else
    begin
      Result := LText;
      Break;
    end;
  end;
end;

class function TUtils.PadL(S: string; Ch: Char; Len: Integer): string;
var
  RestLen: Integer;
begin
  Result := S;
  RestLen := Len - Length(S);
  if RestLen < 1 then
    Exit;
  Result := S + StringOfChar(Ch, RestLen);
end;

class function TUtils.PadR(S: string; Ch: Char; Len: Integer): string;
var
  RestLen: Integer;
begin
  Result := S;
  RestLen := Len - Length(S);
  if RestLen < 1 then
    Exit;
  Result := StringOfChar(Ch, RestLen) + S;
end;

class function TUtils.RandomNumber(const ALimit: Integer): Integer;
begin
  Result := Random(ALimit) + 1;
end;

class function TUtils.RemovePathFromDir(const APath: string;
const ANumberToRemove: Integer): string;
var
  i: Integer;
  LText: string;
begin
  Result := APath;
  if (Copy(Result, Result.Length, Result.Length) = TPath.DirectorySeparatorChar)
  then
    Result := Copy(Result, 0, Result.Length - 1);

  for i := 1 to ANumberToRemove do
  begin
    if (Pos(TPath.DirectorySeparatorChar, Result) = 0) then
      Break;
    Result := Copy(Result, 0, LastDelimiter(TPath.DirectorySeparatorChar,
      Result) - 1);
  end;
end;

class function TUtils.RemoveSpecialCharacter(const AValue: string): string;
Const
  ComAcento = 'àâêôûãõáéíóúçüñÀÂÊÔÛÃÕÁÉÍÓÚÇÜÑ';
  SemAcento = 'aaeouaoaeioucunAAEOUAOAEIOUCUN';
Var
  x: Integer;
  LStr: string;
Begin
  // Tem que corrigir para rodar no Mobile
  LStr := AValue;
  For x := 1 to Length(LStr) do
  begin
    if Pos(LStr[x], ComAcento) <> 0 Then
      LStr[x] := SemAcento[Pos(LStr[x], ComAcento)];
  end;
  Result := UpperCase(LStr);
end;

class function TUtils.AddStrLeft(AVlaue, AStrAdd: string;
ACount: Integer): string;
var
  i, LTam: Integer;
  LAux: string;
begin
  LAux := AVlaue;
  LTam := Length(AVlaue);
  Result := EmptyStr;
  for i := 1 to ACount - LTam do
    Result := AStrAdd + Result;
  Result := Result + LAux;
end;

class function TUtils.AddStrRight(AVlaue, AStrAdd: string;
ACount: Integer): string;
var
  i, LTam: Integer;
  LAux: string;
begin
  LAux := AVlaue;
  LTam := Length(AVlaue);
  Result := EmptyStr;
  for i := 1 to ACount - LTam do
    Result := Result + AStrAdd;
  Result := Result + LAux;
end;

class function TUtils.CheckInternet(const AHost: string;
const APort, ATimeOut: Integer): Boolean;
var
  IdTCPClient: TIdTCPClient;
begin
  Result := False;
  try
    try
      IdTCPClient := TIdTCPClient.Create();
      IdTCPClient.ReadTimeout := ATimeOut;
      IdTCPClient.ConnectTimeout := ATimeOut;
      IdTCPClient.Port := APort;
      IdTCPClient.Host := AHost;
      IdTCPClient.Connect;
      IdTCPClient.Disconnect;
      Result := True;
    Except

    end;
  finally
    FreeAndNil(IdTCPClient);
  end;
end;

class function TUtils.Combine(const APath1: string;
const APath2: array of string): string;
var
  i: Integer;
begin
  Result := APath1;
  for i := 0 to High(APath2) do
  begin
    Result := System.IOUtils.TPath.Combine(Result, APath2[i]);
  end;
end;

class function TUtils.CompressString(AValue: string): string;
var
  strInput, strOutput: TStringStream;
  Zipper: TZCompressionStream;
begin
  Result := '';
  strInput := TStringStream.Create(AValue);
  strOutput := TStringStream.Create;
  try
    Zipper := TZCompressionStream.Create(TCompressionLevel.clMax, strOutput);
    try
      Zipper.CopyFrom(strInput, strInput.Size);
    finally
      Zipper.Free;
    end;
    Result := strOutput.DataString;
  finally
    strInput.Free;
    strOutput.Free;
  end;
end;

class function TUtils.Contains(const Value: string;
AToCompare: array of string): Boolean;
begin
  Result := False;
  for var i := Low(AToCompare) to High(AToCompare) do
  begin
    if ContainsText(Value, AToCompare[i]) then
    begin
      Result := True;
      Break;
    end;
  end;
end;

class function TUtils.CountSubString(const AText, aSubstring: string): Integer;
var
  offset: Integer;
begin
  Result := 0;
  offset := PosEx(aSubstring, AText, 1);
  while offset <> 0 do
  begin
    inc(Result);
    offset := PosEx(aSubstring, AText, offset + Length(aSubstring));
  end;
end;

class function TUtils.DateTimeToUniversalTime(AValue: TDateTime; AIsUTC: Boolean): Int64;
begin
  Result := System.DateUtils.DateTimeToUnix(AValue, AIsUTC);
end;

class function TUtils.DecompressString(AValue: string): string;
var
  strInput, strOutput: TStringStream;
  Unzipper: TZDecompressionStream;
begin
  Result := '';
  strInput := TStringStream.Create(AValue);
  strOutput := TStringStream.Create;
  try
    Unzipper := TZDecompressionStream.Create(strInput);
    try
      strOutput.CopyFrom(Unzipper, Unzipper.Size);
    finally
      Unzipper.Free;
    end;
    Result := strOutput.DataString;
  finally
    strInput.Free;
    strOutput.Free;
  end;
end;

class function TUtils.GetFiles(const APath: string; const AExtractFileName: Boolean): string;
var
  FileList: TStringDynArray;
  LStr: string;
  LFilePath: string;
begin
  Result := EmptyStr;
  FileList := nil;
  FileList := System.IOUtils.TDirectory.GetFiles(APath);
  for LStr in FileList do
  begin
    LFilePath := LStr;
    if AExtractFileName then
      LFilePath := ExtractFileName(LFilePath);
    Result := Result + '|' + LFilePath;
  end;
  Result := Result.Trim(['|']);
end;

class function TUtils.GetFilesList(const APath: string; const AExtractFileName: Boolean): TStringList;
var
  LFiles: string;
begin
  LFiles := IdeaL.Lib.Utils.TUtils.GetFiles(APath, AExtractFileName);

  Result := TStringList.Create;
  Result.StrictDelimiter := True;
  Result.Delimiter := '|';
  Result.QuoteChar := '|';
  Result.DelimitedText := LFiles;
end;

class function TUtils.GetGUID: string;
var
  LGuid: TGUID;
begin
  CreateGUID(LGuid);
  Result := GUIDToString(LGuid);
end;

class function TUtils.GetGUIDAsComponentName: string;

  function StringReplaceLocal(AText, AStr: string): string;
  begin
    Result := StringReplace(AText, AStr, EmptyStr, [rfReplaceAll]);
  end;

begin
  Result := GetGUIDAsTableId;
  Result := StringReplaceLocal(Result, '-');
end;

class function TUtils.GetGUIDAsTableId: string;

  function StringReplaceLocal(AText, AStr: string): string;
  begin
    Result := StringReplace(AText, AStr, EmptyStr, [rfReplaceAll]);
  end;

begin
  {
    format XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX
    https://en.wikipedia.org/wiki/Universally_unique_identifier
  }
  Result := GetGUID;
  Result := StringReplaceLocal(Result, '{');
  Result := StringReplaceLocal(Result, '}');
  Result := StringReplaceLocal(Result, '[');
  Result := StringReplaceLocal(Result, '[');
end;

class procedure TUtils.EventLog(AMsg: string; AIsForceBraodCast: Boolean);
begin
  EventLogIde(AMsg);
  EventLogConsole(AMsg);
  EventLogGrijjySend(EmptyStr, AMsg);
end;

class procedure TUtils.EventLogConsole(AMsg: string);
begin
{$IF defined(ANDROID)} // Show on Logcat
{$IFDEF KASTRI}  // Add KASTRI to your "Conditional defines"
  { TJutil_Log.JavaClass.d( // needs DW.Androidapi.JNI.Util
    StringToJString(TJutil_Log.JavaClass.VERBOSE.ToString),
    StringToJString(AMsg)
    ); }
  TOSLog.d(AMsg); // needs the DW.OSLog
{$ELSE}
  TJutil_Log.JavaClass.d(StringToJString('TUtils.EventLogConsole'), StringToJString(AMsg));
{$ENDIF}
{$ELSEIF defined(MACOS)}
  NSLog(StringToID(AMsg)); // needs iOSapi.Foundation
{$ENDIF}
end;

class procedure TUtils.EventLogIde(AMsg: string);
var
  LMsg: string;
begin
{$IFDEF DEBUG}
  LMsg := 'IDE Event Log ' + DateTimeToStr(Now) + ': ' + AMsg;
{$IF defined(VCL)}
  OutputDebugString(PChar(LMsg));
{$ELSEIF defined(FMX)}
  FMX.Types.Log.d(LMsg);
{$ENDIF}
{$ENDIF}
end;

class function TUtils.ExecuteFile(const AFileName, AParams, ADefaultDir: string;
AShowCmd: Integer): THandle;
begin
{$IFDEF MSWINDOWS}
  Result := ShellExecute(0, 'Open', PChar(AFileName), PChar(AParams),
    PChar(ADefaultDir), AShowCmd);
{$ENDIF}
{$IFDEF MACOSX}
  _system(PAnsiChar('open ' + AnsiString(AFileName)));
{$ENDIF}
end;

class function TUtils.FirstLetterEachWord(const AText: string;
const ACountLetters: Integer; const ASwitchSpace: string): string;
var
  LNomes: array of string;
  i, n: Integer;
  LText: string;
begin

  Result := AText;
  LText := Trim(AText);
  LText := LText + #32;

  i := Pos(#32, LText);

  if i > 0 then
  begin

    n := 0;

    { Separa os Nomes }
    while i > 0 do
    begin
      SetLength(LNomes, Length(LNomes) + 1);
      LNomes[n] := Copy(LText, 1, i - 1);
      Delete(LText, 1, i);
      i := Pos(#32, LText);
      inc(n);
    end;

    if n > 1 then
    begin

      { Abreviar a partir do segundo nome, exceto o último. }
      for i := 0 to n - 1 do
        { Contém mais de 3 letras? (ignorar de, da, das, do, dos, etc.) }
        if Length(LNomes[i]) > 3 then
          LNomes[i] := LNomes[i][1];

      Result := '';
      for i := 0 to n - 1 do
        Result := Result + Trim(LNomes[i]) + #32;

    end
    else if (Result.Length > 1) then
    begin
      Result := Copy(Result, 1, ACountLetters);
    end;

    Result := StringReplace(Result, #32, ASwitchSpace, [rfReplaceAll]);
  end;
end;

class function TUtils.FirstLetterUpperCase(const AText: String;
const ALowerCaseAll: Boolean): String;
const
  CDelimiters = [#9, #10, #13, ' ', ',', '.', ':', ';', '"',
    '\', '/', '(', ')', '[', ']', '{', '}'];
var
  i: Integer;
begin
  Result := AText;
  if (Result <> '') then
  begin
    if (ALowerCaseAll) then
      Result := LowerCase(Result);
    if Length(Result) = 1 then
      Result := UpperCase(Result)
    else
    begin
      for i := Low(Result) to Pred(Length(Result)) do
        if (i = Low(Result)) or
{$IF CompilerVersion < 35.0}
          (Result[Pred(i)] in CDelimiters)
{$ELSE}
          CharInSet(Result[Pred(i)], CDelimiters)
{$ENDIF}
        then
          Result[i] := UpCase(Result[i]);
    end;
  end;
end;

class function TUtils.FirstWord(const AText: string): string;
var
  i: Integer;
begin
  i := Pos(' ', AText);
  if i > 0 then
    Result := Copy(AText, 1, i - 1)
  else
    Result := AText;
end;

class procedure TUtils.ForceDirectory(const ADirectory: string);
var
  LStrList: TStringList;
  LDirectory: string;
  LDirectorySeparator: string;
  i: Integer;
begin
  try
    LDirectorySeparator := System.IOUtils.TPath.DirectorySeparatorChar;
    LDirectory := StringReplace(ADirectory, LDirectorySeparator, '|',
      [rfReplaceAll]);
    LDirectory := StringReplace(LDirectory, ' ', '_123Espaco123_',
      [rfReplaceAll]);
    LStrList := TStringList.Create;
    LStrList.Delimiter := '|';
    LStrList.DelimitedText := LDirectory;
    LDirectory := EmptyStr;

    LDirectory := LStrList[0] + System.IOUtils.TPath.DirectorySeparatorChar;

    if not(DirectoryExists(LDirectory)) then
      System.SysUtils.ForceDirectories(LDirectory);

    for i := 1 to Pred(LStrList.Count) do
    begin
      LDirectory := System.IOUtils.TPath.Combine(LDirectory, LStrList[i]);

      LDirectory := StringReplace(LDirectory, '_123Espaco123_', ' ',
        [rfReplaceAll]);

      if not(DirectoryExists(LDirectory)) then
        System.SysUtils.ForceDirectories(LDirectory);
    end;
  finally
    FreeAndNil(LStrList);
  end;
end;

class function TUtils.FormatCep(const AValue: string; AIsAddDot: Boolean): String;
var
  LDouble: Double;
  LFormat: string;
  LMsg: string;
  LLength: Integer;
begin
  LMsg := 'Invalid value';
  Result := Copy(AValue, 0, 8);
  if Result.Trim.IsEmpty then
    Exit;
  if not(TryStrToFloat(Result, LDouble)) then
    raise Exception.Create(LMsg);

  var
  LDot := EmptyStr;
  if AIsAddDot then
    LDot := '.';

  LLength := Result.Length;
  case LLength of
    1:
      LFormat := '#';
    2:
      LFormat := '99;0';
    3:
      LFormat := '99' + LDot + '9;0';
    4:
      LFormat := '99' + LDot + '99;0';
    5:
      LFormat := '99' + LDot + '999;0';
    6:
      LFormat := '99' + LDot + '999-9;0';
    7:
      LFormat := '99' + LDot + '999-99;0';
    8:
      LFormat := '99' + LDot + '999-999;0';
  else
    raise Exception.Create(LMsg);
  end;

  Result := FormatMaskText(LFormat, Result);
end;

class function TUtils.FormatCnpj(const AValue: String): String;
var
  LDouble: Double;
  LFormat: string;
  LMsg: string;
  LLength: Integer;
begin
  LMsg := 'Invalid value to CPF/CNPJ: ' + AValue;
  Result := Copy(AValue, 0, 14);
  if Result.Trim.IsEmpty then
    Exit;
  if not(TryStrToFloat(Result, LDouble)) then
    raise Exception.Create(LMsg);

  LLength := Result.Length;
  case LLength of
    1:
      LFormat := '#';
    2:
      LFormat := '99;0';
    3:
      LFormat := '99.9;0';
    4:
      LFormat := '99.99;0';
    5:
      LFormat := '99.999;0';
    6:
      LFormat := '99.999.9;0';
    7:
      LFormat := '99.999.99;0';
    8:
      LFormat := '99.999.999;0';
    9:
      LFormat := '99.999.999/9;0';
    10:
      LFormat := '99.999.999/99;0';
    11:
      LFormat := '99.999.999/999;0';
    12:
      LFormat := '99.999.999/9999;0';
    13:
      LFormat := '99.999.999/9999-9;0';
    14:
      LFormat := '99.999.999/9999-99;0';
  else
    raise Exception.Create(LMsg);
  end;

  Result := FormatMaskText(LFormat, Result);
end;															   
class function TUtils.FormatCpfCnpj(const AValue: String): String;
var
  LDouble: Double;
  LFormat: string;
  LMsg: string;
  LLength: Integer;
begin
  LMsg := 'Invalid value to CPF/CNPJ: ' + AValue;
  Result := Copy(AValue, 0, 14);
  if Result.Trim.IsEmpty then
    Exit;
  if not(TryStrToFloat(Result, LDouble)) then
    raise Exception.Create(LMsg);

  LLength := Result.Length;
  case LLength of
    1:
      LFormat := '#';
    2:
      LFormat := '99;0';
    3:
      LFormat := '999;0';
    4:
      LFormat := '999.9;0';
    5:
      LFormat := '999.99;0';
    6:
      LFormat := '999.999;0';
    7:
      LFormat := '999.999.9;0';
    8:
      LFormat := '999.999.99;0';
    9:
      LFormat := '999.999.999;0';
    10:
      LFormat := '999.999.999-9;0';
    11:
      LFormat := '999.999.999-99;0';
    12:
      LFormat := '99.999.999/9999;0';
    13:
      LFormat := '99.999.999/9999-9;0';
    14:
      LFormat := '99.999.999/9999-99;0';
  else
    raise Exception.Create(LMsg);
  end;

  Result := FormatMaskText(LFormat, Result);
end;

class function TUtils.FormatCurrency(AValue: String; ADecimalLenght: Integer;
ABackspaceIsPressedEdtCurrency: Boolean): String;
var
  LBeforeSeparate: string;
  LAfterSeparate: string;
  LLength: Integer;
  LIsNegative: Boolean;
  i: Integer;

  LExtendedFormat: string;
  LExtended: Extended;
begin
  if FormatSettings.DecimalSeparator = FormatSettings.ThousandSeparator then
    ThrowExceptionSomethingWentWrong('IdeaL.Lib.Utils.TUtils.FormatCurrency FormatSettings.DecimalSeparator = FormatSettings.ThousandSeparator');

  LIsNegative := AValue.Trim.StartsWith('-');
  AValue := StringReplace(AValue, '-', EmptyStr, []);

  if (Pos(FormatSettings.DecimalSeparator, AValue) <= 0) then
  begin
    AValue := AValue + FormatSettings.DecimalSeparator + PadR(EmptyStr, '0', ADecimalLenght);
  end;

  if not(ABackspaceIsPressedEdtCurrency) then
    if ((Length(AValue)) - (Pos(FormatSettings.DecimalSeparator, AValue)) < ADecimalLenght) then
    begin
      while ((Length(AValue)) - (Pos(FormatSettings.DecimalSeparator, AValue)) < ADecimalLenght) do
      begin
        AValue := AValue + '0';
      end;
    end;

  AValue := StringReplace(AValue, FormatSettings.DecimalSeparator, EmptyStr, [rfReplaceAll]);
  AValue := StringReplace(AValue, FormatSettings.ThousandSeparator, EmptyStr, [rfReplaceAll]);

  { remove 0 on left }
  if (TryStrToInt(AValue, i)) then
    AValue := IntToStr(i);

  while (AValue.Length <= ADecimalLenght) do
  begin
    AValue := '0' + AValue;
  end;

  LLength := AValue.Length;

  LBeforeSeparate := LeftStr(AValue, LLength - ADecimalLenght);
  LAfterSeparate := RightStr(AValue, ADecimalLenght);

  Result := LBeforeSeparate + FormatSettings.DecimalSeparator + LAfterSeparate;

  if TryStrToFloat(Result, LExtended) then
  begin
    var
    LDecimalStr := EmptyStr;
    for i := 1 to ADecimalLenght do
      LDecimalStr := LDecimalStr + '0';

    LExtendedFormat := '#0' + FormatSettings.ThousandSeparator + FormatSettings.DecimalSeparator + LDecimalStr;
    Result := System.SysUtils.FormatFloat(LExtendedFormat, LExtended);
  end;

  var
  LDouble : Double := 0.0;
  if (LIsNegative) and (TryStrToFloat(Result, LDouble)) and (LDouble <> 0) then
  Result := '-' + Result;
end;

class function TUtils.FormatDate(const AValue: string): string;
var
  LDouble: Double;
  LFormat: string;
  LMsg: string;
  LSep: string; // Date separator
  LLength: Integer;
begin
  LMsg := 'Invalid value';
  Result := Copy(AValue, 0, 8);
  if Result.Trim.IsEmpty then
    Exit;
  if not(TryStrToFloat(Result, LDouble)) then
    raise Exception.Create(LMsg);
  LSep := FormatSettings.DateSeparator;
  LLength := Result.Length;
  case LLength of
    1:
      LFormat := '#';
    2:
      LFormat := '99;0';
    3:
      LFormat := '99' + LSep + '9;0';
    4:
      LFormat := '99' + LSep + '99;0';
    5:
      LFormat := '99' + LSep + '99' + LSep + '9;0';
    6:
      LFormat := '99' + LSep + '99' + LSep + '99;0';
    7:
      LFormat := '99' + LSep + '99' + LSep + '999;0';
    8:
      LFormat := '99' + LSep + '99' + LSep + '9999;0';
  else
    raise Exception.Create(LMsg);
  end;

  Result := FormatMaskText(LFormat, Result);
end;

class function TUtils.FormatPhoneNumber(const AValue: String): String;
var
  LDouble: Double;
  LFormat: string;
  LMsg: string;
  LLength: Integer;
begin
  LMsg := 'Invalid value';
  Result := Copy(AValue, 0, 11);
  if Result.Trim.IsEmpty then
    Exit;
  if not(TryStrToFloat(Result, LDouble)) then
    raise Exception.Create(LMsg);

  LLength := Result.Length;
  case LLength of
    1:
      LFormat := '#';
    2:
      LFormat := '99;0';
    3:
      LFormat := '(99)9;0';
    4:
      LFormat := '(99)99;0';
    5:
      LFormat := '(99)999;0';
    6:
      LFormat := '(99)9999;0';
    7:
      LFormat := '(99)9999-9;0';
    8:
      LFormat := '(99)9999-99;0';
    9:
      LFormat := '(99)9999-999;0';
    10:
      LFormat := '(99)9999-9999;0';
    11:
      LFormat := '(99)9 9999-9999;0';
  else
    raise Exception.Create(LMsg);
  end;

  Result := FormatMaskText(LFormat, Result);
end;

class function TUtils.GetApplicationPath: string;
begin
{$IFDEF MSWINDOWS}
  Result := ExtractFilePath(Application.ExeName);
{$ELSEIF defined(ANDROID)}
  Result := System.IOUtils.TPath.GetHomePath;
{$ELSEIF defined(IOS)}
  Result := System.IOUtils.TPath.GetDocumentsPath;
{$ELSEIF defined(LINUX)}
  Result := System.IOUtils.TDirectory.GetCurrentDirectory;
{$ENDIF}
end;

{$REGION 'GetApplicationVersion'}


class function TUtils.GetApplicationVersion: string;
{$IFDEF ANDROID}
var
  PackageManager: JPackageManager;
  PackageInfo: JPackageInfo;
{$ENDIF}
{$IFDEF MACOS}
var
  CFStr: CFStringRef;
  Range: CFRange;
{$ENDIF}
{$IFDEF  MSWINDOWS}
var
  Exe: string;
  Size, Handle: DWORD;
  Buffer: TBytes;
  FixedPtr: PVSFixedFileInfo;
{$ENDIF}
begin
{$IFDEF MACOS}
  CFStr := CFBundleGetValueForInfoDictionaryKey(CFBundleGetMainBundle,
    kCFBundleVersionKey);
  Range.location := 0;
  Range.Length := CFStringGetLength(CFStr);
  SetLength(Result, Range.Length);
  CFStringGetCharacters(CFStr, Range, PChar(Result));
{$ENDIF}
{$IFDEF MSWINDOWS}
  try
    Exe := ParamStr(0);
    Size := GetFileVersionInfoSize(PChar(Exe), Handle);
    if Size = 0 then
      RaiseLastOSError;
    SetLength(Buffer, Size);
    if not GetFileVersionInfo(PChar(Exe), Handle, Size, Buffer) then
      RaiseLastOSError;
    if not VerQueryValue(Buffer, '\', Pointer(FixedPtr), Size) then
      RaiseLastOSError;
    Result := Format('%d.%d.%d.%d', [LongRec(FixedPtr.dwFileVersionMS).Hi,
    // major
    LongRec(FixedPtr.dwFileVersionMS).Lo, // minor
    LongRec(FixedPtr.dwFileVersionLS).Hi, // release
    LongRec(FixedPtr.dwFileVersionLS).Lo]) // build
  except
    Result := 'vWindows';
  end;
{$ENDIF}
{$IFDEF ANDROID}
  PackageManager := SharedActivity.getPackageManager;
  PackageInfo := PackageManager.getPackageInfo
    (SharedActivityContext.getPackageName(),
    TJPackageManager.JavaClass.GET_ACTIVITIES);
  Result := JStringToString(PackageInfo.versionName);
{$ENDIF}
end;
{$ENDREGION}


class procedure TUtils.GravaDadosDataSetToTxt(aDataSet: TDataSet;
aManterTexto: Boolean);
var
  VArq: TextFile;
  VTextoLido: String;
  vTextoAtual: TStrings;
  i: Integer;
begin
{$IFDEF DEBUG}
  // Exit;
  try
    try
      try
        { [ 1 ] Associa a variável do programa "arq" ao arquivo externo "tabuada.txt" na unidade de disco "d" }
        AssignFile(VArq, TPath.Combine(System.SysUtils.GetCurrentDir,
          'debugDadosDataSet.txt'));

        vTextoAtual := TStringList.Create();
        VTextoLido := '';

        if (aManterTexto) then
        begin
          Reset(VArq);

          // Display the file contents
          while not Eof(VArq) do
          begin
            ReadLn(VArq, VTextoLido);
            vTextoAtual.Add(VTextoLido);
          end;
        end;

        vTextoAtual.Add('');
        vTextoAtual.Add('/* ********** */');
        vTextoAtual.Add('');

        with (aDataSet) do
        begin
          try
            DisableControls;
            First;

            while not(Eof) do
            begin
              VTextoLido := '';
              for i := 0 to Fields.Count - 1 do
              begin
                VTextoLido := VTextoLido + ';' + Fields[i].AsString;
              end;
              vTextoAtual.Add(VTextoLido);
              Next;
            end;
          finally
            EnableControls;
          end;
        end;

        { [ 2 ] Cria o arquivo texto "tabuada.txt" na unidade de disco "d" }
        Rewrite(VArq);
        { [ 8 ] Grava uma linha da tabuada no arquivo }
        Writeln(VArq, vTextoAtual.Text);
      finally
        { [ 8 ] Fecha o arquivo texto "tabuada.txt". }
        try
          FreeAndNil(vTextoAtual);
        except
        end;
        CloseFile(VArq);
      end;
    finally
      // try FreeAndNil(VArq); except end;
    end;
  except
    on E: Exception do
    begin
      StrToInt('1');
    end;
  end;
{$ENDIF}
end;

class procedure TUtils.GravaSqlEmTxt(aStrSql: string; aManterTexto: Boolean);
var
  VArq: TextFile;
  vStrList: TStringList;
  vTextoAtual: String;
begin
{$IFDEF MSWINDOWS}
{$IFDEF DEBUG}
  try
    try
      vStrList := TStringList.Create;

      { [ 1 ] Associa a variável do programa "arq" ao arquivo externo "tabuada.txt" na unidade de disco "d" }
      AssignFile(VArq, TPath.Combine(System.SysUtils.GetCurrentDir,
        'debugSql.txt'));

      vTextoAtual := '';
      if (aManterTexto) then
      begin
        Reset(VArq);

        // Display the file contents
        while not Eof(VArq) do
        begin
          ReadLn(VArq, vTextoAtual);
          vStrList.Add(vTextoAtual);
        end;

        vStrList.Add('');
        vStrList.Add('/* ********** */');
        vStrList.Add('');
      end;

      vStrList.Add(aStrSql + ';');

      { [ 2 ] Cria o arquivo texto "tabuada.txt" na unidade de disco "d" }
      Rewrite(VArq);
      { [ 8 ] Grava uma linha da tabuada no arquivo }
      Writeln(VArq, vStrList.Text);
    finally
      { [ 8 ] Fecha o arquivo texto "tabuada.txt". }
      try
        FreeAndNil(vStrList);
      except
      end;
      CloseFile(VArq);
    end;
  except

  end;
{$ENDIF}
{$ENDIF}
end;

class procedure TUtils.EventLogGrijjySend(const AMsg, AValue: string);
begin
{$IFDEF GrijjyLogger}
  GrijjyLog.Send(AMsg, AValue);
{$ENDIF}
end;

class function TUtils.IsAppInstalled(const AAppName: string): Boolean;
{$IFDEF ANDROID}
var
  PackageManager: JPackageManager;
{$ENDIF}
begin
  Result := False;
{$IFDEF ANDROID}
  {
    going to need the permission QUERY_ALL_PACKAGES on Project Options or add
    the specific PackageName to the Manifest.Template
    https://stackoverflow.com/questions/62345805/namenotfoundexception-when-calling-getpackageinfo-on-android-11
  }
  PackageManager := SharedActivity.getPackageManager;
  try
    PackageManager.getPackageInfo(StringToJString(AAppName), TJPackageManager.JavaClass.GET_ACTIVITIES);
    Result := True;
  except
    on Ex: Exception do
      Result := False;
  end;
{$ENDIF}
end;

class function TUtils.isCNPJ(AValue: string): Boolean;
var
  dig13, dig14: string;
  sm, i, r, peso: Integer;
begin
  AValue := JustNumber(AValue);
  // length - retorna o tamanho da string do CNPJ (CNPJ é um número formado por 14 dígitos)
  if ((AValue = '00000000000000') or (AValue = '11111111111111') or
    (AValue = '22222222222222') or (AValue = '33333333333333') or
    (AValue = '44444444444444') or (AValue = '55555555555555') or
    (AValue = '66666666666666') or (AValue = '77777777777777') or
    (AValue = '88888888888888') or (AValue = '99999999999999') or
    (AValue.Length <> 14)) then
  begin
    isCNPJ := False;
    Exit;
  end;

  // "try" - protege o código para eventuais erros de conversão de tipo através da função "StrToInt"
  try
    { *-- Cálculo do 1o. Digito Verificador --* }
    sm := 0;
    peso := 2;
    for i := 12 downto 1 do
    begin
      // StrToInt converte o i-ésimo caractere do CNPJ em um número
      sm := sm + (StrToInt(Copy(AValue, i, 1)) * peso);
      peso := peso + 1;
      if (peso = 10) then
        peso := 2;
    end;

    r := sm mod 11;
    if ((r = 0) or (r = 1)) then
      dig13 := '0'
    else
      str((11 - r): 1, dig13);
    // converte um número no respectivo caractere numérico

    { *-- Cálculo do 2o. Digito Verificador --* }
    sm := 0;
    peso := 2;
    for i := 13 downto 1 do
    begin
      sm := sm + (StrToInt(Copy(AValue, i, 1)) * peso);
      peso := peso + 1;
      if (peso = 10) then
        peso := 2;
    end;

    r := sm mod 11;
    if ((r = 0) or (r = 1)) then
      dig14 := '0'
    else
      str((11 - r): 1, dig14);

    { Verifica se os digitos calculados conferem com os digitos informados. }
    if ((dig13 = Copy(AValue, 13, 1)) and (dig14 = Copy(AValue, 14, 1))) then
      isCNPJ := True
    else
      isCNPJ := False;
  except
    isCNPJ := False
  end;
end;

class function TUtils.isCPF(AValue: string): Boolean;
var
  dig10, dig11: string;
  S, i, r, peso: Integer;
begin
  AValue := JustNumber(AValue);
  // length - retorna o tamanho da string (CPF é um número formado por 11 dígitos)
  if ((AValue = '00000000000') or (AValue = '11111111111') or
    (AValue = '22222222222') or (AValue = '33333333333') or
    (AValue = '44444444444') or (AValue = '55555555555') or
    (AValue = '66666666666') or (AValue = '77777777777') or
    (AValue = '88888888888') or (AValue = '99999999999') or
    (AValue.Length <> 11)) then
  begin
    isCPF := False;
    Exit;
  end;

  // try - protege o código para eventuais erros de conversão de tipo na função StrToInt
  try
    { *-- Cálculo do 1o. Digito Verificador --* }
    S := 0;
    peso := 10;
    for i := 1 to 9 do
    begin
      // StrToInt converte o i-ésimo caractere do CPF em um número
      S := S + (StrToInt(Copy(AValue, i, 1)) * peso);
      peso := peso - 1;
    end;

    r := 11 - (S mod 11);
    if ((r = 10) or (r = 11)) then
      dig10 := '0'
    else
      str(r: 1, dig10); // converte um número no respectivo caractere numérico

    { *-- Cálculo do 2o. Digito Verificador --* }
    S := 0;
    peso := 11;
    for i := 1 to 10 do
    begin
      S := S + (StrToInt(Copy(AValue, i, 1)) * peso);
      peso := peso - 1;
    end;

    r := 11 - (S mod 11);
    if ((r = 10) or (r = 11)) then
      dig11 := '0'
    else
      str(r: 1, dig11);

    { Verifica se os digitos calculados conferem com os digitos informados. }
    if ((dig10 = Copy(AValue, 10, 1)) and (dig11 = Copy(AValue, 11, 1))) then
      isCPF := True
    else
      isCPF := False;
  except
    isCPF := False
  end;
end;

class function TUtils.isEMail(AValue: String): Boolean;
begin
  AValue := Trim(UpperCase(AValue));
  if Pos('@', AValue) > 1 then
  begin
    Delete(AValue, 1, Pos('@', AValue));
    Result := (Length(AValue) > 0) and (Pos('.COM', UpperCase(AValue)) >= 1);
  end
  else
    Result := False;
end;

class function TUtils.isInscricaoEstadual(const AIe, AUf: String): Boolean;
//
{$REGION 'MascaraInscricao'}
  Function Mascara_Inscricao(Inscricao, Estado: String): String;
  Var
    vMascara: String;

    vContador1: Integer;
    vContador2: Integer;
  Begin

    IF Estado = 'AC' Then
      vMascara := '**.***.***/***-**';
    IF Estado = 'AL' Then
      vMascara := '*********';
    IF Estado = 'AP' Then
      vMascara := '*********';
    IF Estado = 'AM' Then
      vMascara := '**.***.***-*';
    IF Estado = 'BA' Then
      vMascara := '******-**';
    IF Estado = 'CE' Then
      vMascara := '********-*';
    IF Estado = 'DF' Then
      vMascara := '***********-**';
    IF Estado = 'ES' Then
      vMascara := '*********';
    IF Estado = 'GO' Then
      vMascara := '**.***.***-*';
    IF Estado = 'MA' Then
      vMascara := '*********';
    IF Estado = 'MT' Then
      vMascara := '**********-*';
    IF Estado = 'MS' Then
      vMascara := '*********';
    IF Estado = 'MG' Then
      vMascara := '***.***.***/****';
    IF Estado = 'PA' Then
      vMascara := '**-******-*';
    IF Estado = 'PB' Then
      vMascara := '********-*';
    IF Estado = 'PR' Then
      vMascara := '********-**';
    IF Estado = 'PE' Then
      vMascara := '**.*.***.*******-*';
    IF Estado = 'PI' Then
      vMascara := '*********';
    IF Estado = 'RJ' Then
      vMascara := '**.***.**-*';
    IF Estado = 'RN' Then
      vMascara := '**.***.***-*';
    IF Estado = 'RS' Then
      vMascara := '***/*******';
    IF Estado = 'RO' Then
      vMascara := '***.*****-*';
    IF Estado = 'RR' Then
      vMascara := '********-*';
    IF Estado = 'SC' Then
      vMascara := '***.***.***';
    IF Estado = 'SP' Then
      vMascara := '***.***.***.***';
    IF Estado = 'SE' Then
      vMascara := '*********-*';
    IF Estado = 'TO' Then
      vMascara := '***********';

    vContador2 := 1;

    Result := '';

    vMascara := vMascara + '****';

    For vContador1 := 1 To Length(vMascara) Do
    Begin
      IF Copy(vMascara, vContador1, 1) = '*' Then
        Result := Result + Copy(Inscricao, vContador2, 1);
      IF Copy(vMascara, vContador1, 1) <> '*' Then
        Result := Result + Copy(vMascara, vContador1, 1);

      IF Copy(vMascara, vContador1, 1) = '*' Then
        vContador2 := vContador2 + 1;
    End;

    Result := Trim(Result);
  End;
{$ENDREGION}


//
var
  Contador: ShortInt;
  Casos: ShortInt;
  Digitos: ShortInt;

  Tabela_1: String;
  Tabela_2: String;
  Tabela_3: String;

  Base_1: String;
  Base_2: String;
  Base_3: String;

  Valor_1: ShortInt;

  Soma_1: Integer;
  Soma_2: Integer;

  Erro_1: ShortInt;
  Erro_2: ShortInt;
  Erro_3: ShortInt;

  Posicao_1: string;
  Posicao_2: String;

  Tabela: String;
  Rotina: String;
  Modulo: ShortInt;
  peso: String;

  Digito: ShortInt;

  Resultado: String;
  Retorno: Boolean;
Begin
  Try
    Tabela_1 := ' ';
    Tabela_2 := ' ';
    Tabela_3 := ' ';

    { }                                                                                                                 { }
    { Valores possiveis para os digitos (j) }
    { }
    { 0 a 9 = Somente o digito indicado. }
    { N = Numeros 0 1 2 3 4 5 6 7 8 ou 9 }
    { A = Numeros 1 2 3 4 5 6 7 8 ou 9 }
    { B = Numeros 0 3 5 7 ou 8 }
    { C = Numeros 4 ou 7 }
    { D = Numeros 3 ou 4 }
    { E = Numeros 0 ou 8 }
    { F = Numeros 0 1 ou 5 }
    { G = Numeros 1 7 8 ou 9 }
    { H = Numeros 0 1 2 ou 3 }
    { I = Numeros 0 1 2 3 ou 4 }
    { J = Numeros 0 ou 9 }
    { K = Numeros 1 2 3 ou 9 }
    { }
    { -------------------------------------------------------- }
    { }
    { Valores possiveis para as rotinas (d) e (g) }
    { }
    { A a E = Somente a Letra indicada. }
    { 0 = B e D }
    { 1 = C e E }
    { 2 = A e E }
    { }
    { -------------------------------------------------------- }
    { }
    { C T  F R M  P  R M  P }
    { A A  A O O  E  O O  E }
    { S M  T T D  S  T D  S }
    { }
    { a b  c d e  f  g h  i  jjjjjjjjjjjjjj }
    { 0000000001111111111222222222233333333 }
    { 1234567890123456789012345678901234567 }

    IF AUf = 'AC' Then
      Tabela_1 := '1.09.0.E.11.01. .  .  .     01NNNNNNX.14.00';
    IF AUf = 'AC' Then
      Tabela_2 := '2.13.0.E.11.02.E.11.01. 01NNNNNNNNNXY.13.14';
    IF AUf = 'AL' Then
      Tabela_1 := '1.09.0.0.11.01. .  .  .     24BNNNNNX.14.00';
    IF AUf = 'AP' Then
      Tabela_1 := '1.09.0.1.11.01. .  .  .     03NNNNNNX.14.00';
    IF AUf = 'AP' Then
      Tabela_2 := '2.09.1.1.11.01. .  .  .     03NNNNNNX.14.00';
    IF AUf = 'AP' Then
      Tabela_3 := '3.09.0.E.11.01. .  .  .     03NNNNNNX.14.00';
    IF AUf = 'AM' Then
      Tabela_1 := '1.09.0.E.11.01. .  .  .     0CNNNNNNX.14.00';
    IF AUf = 'BA' Then
      Tabela_1 := '1.08.0.E.10.02.E.10.03.      NNNNNNYX.14.13';
    IF AUf = 'BA' Then
      Tabela_2 := '2.08.0.E.11.02.E.11.03.      NNNNNNYX.14.13';
    IF AUf = 'CE' Then
      Tabela_1 := '1.09.0.E.11.01. .  .  .     0NNNNNNNX.14.13';
    IF AUf = 'DF' Then
      Tabela_1 := '1.13.0.E.11.02.E.11.01. 07DNNNNNNNNXY.13.14';
    IF AUf = 'ES' Then
      Tabela_1 := '1.09.0.E.11.01. .  .  .     0ENNNNNNX.14.00';
    IF AUf = 'GO' Then
      Tabela_1 := '1.09.1.E.11.01. .  .  .     1FNNNNNNX.14.00';
    IF AUf = 'GO' Then
      Tabela_2 := '2.09.0.E.11.01. .  .  .     1FNNNNNNX.14.00';
    IF AUf = 'MA' Then
      Tabela_1 := '1.09.0.E.11.01. .  .  .     12NNNNNNX.14.00';
    IF AUf = 'MT' Then
      Tabela_1 := '1.11.0.E.11.01. .  .  .   NNNNNNNNNNX.14.00';
    IF AUf = 'MS' Then
      Tabela_1 := '1.09.0.E.11.01. .  .  .     28NNNNNNX.14.00';
    IF AUf = 'MG' Then
      Tabela_1 := '1.13.0.2.10.10.E.11.11. NNNNNNNNNNNXY.13.14';
    IF AUf = 'PA' Then
      Tabela_1 := '1.09.0.E.11.01. .  .  .     15NNNNNNX.14.00';
    IF AUf = 'PB' Then
      Tabela_1 := '1.09.0.E.11.01. .  .  .     16NNNNNNX.14.00';
    IF AUf = 'PR' Then
      Tabela_1 := '1.10.0.E.11.09.E.11.08.    NNNNNNNNXY.13.14';
    IF AUf = 'PE' Then
      Tabela_1 := '1.14.1.E.11.07. .  .  .18ANNNNNNNNNNX.14.00';
    IF AUf = 'PI' Then
      Tabela_1 := '1.09.0.E.11.01. .  .  .     19NNNNNNX.14.00';
    IF AUf = 'RJ' Then
      Tabela_1 := '1.08.0.E.11.08. .  .  .      GNNNNNNX.14.00';
    IF AUf = 'RN' Then
      Tabela_1 := '1.09.0.0.11.01. .  .  .     20HNNNNNX.14.00';
    IF AUf = 'RS' Then
      Tabela_1 := '1.10.0.E.11.01. .  .  .    INNNNNNNNX.14.00';
    IF AUf = 'RO' Then
      Tabela_1 := '1.09.1.E.11.04. .  .  .     ANNNNNNNX.14.00';
    IF AUf = 'RO' Then
      Tabela_2 := '2.14.0.E.11.01. .  .  .NNNNNNNNNNNNNX.14.00';
    IF AUf = 'RR' Then
      Tabela_1 := '1.09.0.D.09.05. .  .  .     24NNNNNNX.14.00';
    IF AUf = 'SC' Then
      Tabela_1 := '1.09.0.E.11.01. .  .  .     NNNNNNNNX.14.00';
    IF AUf = 'SP' Then
      Tabela_1 := '1.12.0.D.11.12.D.11.13.  NNNNNNNNXNNY.11.14';
    IF AUf = 'SP' Then
      Tabela_2 := '2.12.0.D.11.12. .  .  .  NNNNNNNNXNNN.11.00';
    IF AUf = 'SE' Then
      Tabela_1 := '1.09.0.E.11.01. .  .  .     NNNNNNNNX.14.00';
    IF AUf = 'TO' Then
      Tabela_1 := '1.11.0.E.11.06. .  .  .   29JKNNNNNNX.14.00';

    IF AUf = 'CNPJ' Then
      Tabela_1 := '1.14.0.E.11.21.E.11.22.NNNNNNNNNNNNXY.13.14';
    IF AUf = 'CPF' Then
      Tabela_1 := '1.11.0.E.11.31.E.11.32.   NNNNNNNNNXY.13.14';

    { Deixa somente os numeros }

    Base_1 := '';

    For Contador := 1 TO 30 Do
      IF Pos(Copy(AIe, Contador, 1), '0123456789') <> 0 Then
        Base_1 := Base_1 + Copy(AIe, Contador, 1);

    { Repete 3x - 1 para cada caso possivel }

    Casos := 0;

    Erro_1 := 0;
    Erro_2 := 0;
    Erro_3 := 0;

    While Casos < 3 Do
    Begin
      Casos := Casos + 1;

      IF Casos = 1 Then
        Tabela := Tabela_1;
      IF Casos = 2 Then
        Erro_1 := Erro_3;
      IF Casos = 2 Then
        Tabela := Tabela_2;
      IF Casos = 3 Then
        Erro_2 := Erro_3;
      IF Casos = 3 Then
        Tabela := Tabela_3;

      Erro_3 := 0;

      IF Copy(Tabela, 1, 1) <> ' ' Then
      Begin

        { Verifica o Tamanho }

        IF Length(Trim(Base_1)) <> (StrToInt(Copy(Tabela, 3, 2))) Then
          Erro_3 := 1;

        IF Erro_3 = 0 Then
        Begin
          { Ajusta o Tamanho }

          Base_2 := Copy('              ' + Base_1,
            Length('              ' + Base_1) - 13, 14);

          { Compara com valores possivel para cada uma da 14 posições }

          Contador := 0;

          While (Contador < 14) AND (Erro_3 = 0) Do
          Begin
            Contador := Contador + 1;

            Posicao_1 := Copy(Copy(Tabela, 24, 14), Contador, 1);
            Posicao_2 := Copy(Base_2, Contador, 1);

            IF (Posicao_1 = ' ') AND (Posicao_2 <> ' ') Then
              Erro_3 := 1;
            IF (Posicao_1 = 'N') AND (Pos(Posicao_2, '0123456789') = 0) Then
              Erro_3 := 1;
            IF (Posicao_1 = 'A') AND (Pos(Posicao_2, '123456789') = 0) Then
              Erro_3 := 1;
            IF (Posicao_1 = 'B') AND (Pos(Posicao_2, '03578') = 0) Then
              Erro_3 := 1;
            IF (Posicao_1 = 'C') AND (Pos(Posicao_2, '47') = 0) Then
              Erro_3 := 1;
            IF (Posicao_1 = 'D') AND (Pos(Posicao_2, '34') = 0) Then
              Erro_3 := 1;
            IF (Posicao_1 = 'E') AND (Pos(Posicao_2, '08') = 0) Then
              Erro_3 := 1;
            IF (Posicao_1 = 'F') AND (Pos(Posicao_2, '015') = 0) Then
              Erro_3 := 1;
            IF (Posicao_1 = 'G') AND (Pos(Posicao_2, '1789') = 0) Then
              Erro_3 := 1;
            IF (Posicao_1 = 'H') AND (Pos(Posicao_2, '0123') = 0) Then
              Erro_3 := 1;
            IF (Posicao_1 = 'I') AND (Pos(Posicao_2, '01234') = 0) Then
              Erro_3 := 1;
            IF (Posicao_1 = 'J') AND (Pos(Posicao_2, '09') = 0) Then
              Erro_3 := 1;
            IF (Posicao_1 = 'K') AND (Pos(Posicao_2, '1239') = 0) Then
              Erro_3 := 1;
            IF (Posicao_1 <> Posicao_2) AND
              (Pos(Posicao_1, '0123456789') > 0) Then
              Erro_3 := 1;

          End;

          { Calcula os Digitos }

          Rotina := ' ';
          Digitos := 000;
          Digito := 000;

          While (Digitos < 2) AND (Erro_3 = 0) Do
          Begin

            Digitos := Digitos + 1;

            { Carrega peso }

            peso := Copy(Tabela, 5 + (Digitos * 8), 2);

            IF peso <> '  ' Then
            Begin

              Rotina := Copy(Tabela, 0 + (Digitos * 8), 1);
              Modulo := StrToInt(Copy(Tabela, 2 + (Digitos * 8), 2));

              IF peso = '01' Then
                peso := '06.05.04.03.02.09.08.07.06.05.04.03.02.00';
              IF peso = '02' Then
                peso := '05.04.03.02.09.08.07.06.05.04.03.02.00.00';
              IF peso = '03' Then
                peso := '06.05.04.03.02.09.08.07.06.05.04.03.00.02';
              IF peso = '04' Then
                peso := '00.00.00.00.00.00.00.00.06.05.04.03.02.00';
              IF peso = '05' Then
                peso := '00.00.00.00.00.01.02.03.04.05.06.07.08.00';
              IF peso = '06' Then
                peso := '00.00.00.09.08.00.00.07.06.05.04.03.02.00';
              IF peso = '07' Then
                peso := '05.04.03.02.01.09.08.07.06.05.04.03.02.00';
              IF peso = '08' Then
                peso := '08.07.06.05.04.03.02.07.06.05.04.03.02.00';
              IF peso = '09' Then
                peso := '07.06.05.04.03.02.07.06.05.04.03.02.00.00';
              IF peso = '10' Then
                peso := '00.01.02.01.01.02.01.02.01.02.01.02.00.00';
              IF peso = '11' Then
                peso := '00.03.02.11.10.09.08.07.06.05.04.03.02.00';
              IF peso = '12' Then
                peso := '00.00.01.03.04.05.06.07.08.10.00.00.00.00';
              IF peso = '13' Then
                peso := '00.00.03.02.10.09.08.07.06.05.04.03.02.00';
              IF peso = '21' Then
                peso := '05.04.03.02.09.08.07.06.05.04.03.02.00.00';
              IF peso = '22' Then
                peso := '06.05.04.03.02.09.08.07.06.05.04.03.02.00';
              IF peso = '31' Then
                peso := '00.00.00.10.09.08.07.06.05.04.03.02.00.00';
              IF peso = '32' Then
                peso := '00.00.00.11.10.09.08.07.06.05.04.03.02.00';

              { Multiplica }

              Base_3 := Copy(('0000000000000000' + Trim(Base_2)),
                Length(('0000000000000000' + Trim(Base_2))) - 13, 14);

              Soma_1 := 0;
              Soma_2 := 0;

              For Contador := 1 To 14 Do
              Begin

                Valor_1 := (StrToInt(Copy(Base_3, Contador, 01)) *
                  StrToInt(Copy(peso, Contador * 3 - 2, 2)));

                Soma_1 := Soma_1 + Valor_1;

                IF Valor_1 > 9 Then
                  Valor_1 := Valor_1 - 9;

                Soma_2 := Soma_2 + Valor_1;

              End;

              { Ajusta valor da soma }

              IF Pos(Rotina, 'A2') > 0 Then
                Soma_1 := Soma_2;
              IF Pos(Rotina, 'B0') > 0 Then
                Soma_1 := Soma_1 * 10;
              IF Pos(Rotina, 'C1') > 0 Then
                Soma_1 := Soma_1 + (5 + 4 * StrToInt(Copy(Tabela, 6, 1)));

              { Calcula o Digito }

              IF Pos(Rotina, 'D0') > 0 Then
                Digito := Soma_1 Mod Modulo;
              IF Pos(Rotina, 'E12') > 0 Then
                Digito := Modulo - (Soma_1 Mod Modulo);

              IF Digito < 10 Then
                Resultado := IntToStr(Digito);
              IF Digito = 10 Then
                Resultado := '0';
              IF Digito = 11 Then
                Resultado := Copy(Tabela, 6, 1);

              { Verifica o Digito }

              IF (Copy(Base_2, StrToInt(Copy(Tabela, 36 + (Digitos * 3), 2)), 1)
                <> Resultado) Then
                Erro_3 := 1;
            End;
          End;
        End;
      End;
    End;

    { Retorna o resultado da Verificação }

    Retorno := False;

    IF (Trim(Tabela_1) <> '') AND (Erro_1 = 0) Then
      Retorno := True;
    IF (Trim(Tabela_2) <> '') AND (Erro_2 = 0) Then
      Retorno := True;
    IF (Trim(Tabela_3) <> '') AND (Erro_3 = 0) Then
      Retorno := True;

    IF Trim(AIe) = 'ISENTO' Then
      Retorno := True;

    Result := Retorno;

  Except
    Result := False;
  End;
end;

class function TUtils.JsonNormalize(const AText: string): string;
var
  LJsonObj: TJSONObject;
begin
  Result := AText;
  try
    LJsonObj := TJSONObject.Create;
    LJsonObj.AddPair('Normalize', Result);
    Result := LJsonObj.GetValue('Normalize').ToJSON;
    Result := Copy(Result, 2, Result.Length - 2);
  finally
    FreeAndNil(LJsonObj);
  end;
end;

class function TUtils.JustNumber(const AValue: string;
const AStartWithOne: Boolean): string;
const
  CNumbers = '0123456789';
var
  i: Integer;
  LValue: string;
  LValueOriginal: string;
{$IFDEF FMX}
  LEdt: TEdit;

  regex: TRegEx;
  match: TMatch;
  matches: TMatchCollection;
{$ENDIF}
begin
{$IFDEF FMX}
  {try
    Result := EmptyStr;
    LEdt := TEdit.Create(nil);
    LEdt.BeginUpdate;
    LEdt.FilterChar := CNumbers;
    LEdt.Text := AValue;
    Result := LEdt.Text;
  finally
    FreeAndNil(LEdt);
  end;}
  Result := TRegEx.Replace(AValue, '\D', '');
{$ELSE}
  Result := EmptyStr;
  LValueOriginal := AValue;

  for i := 1 to LValueOriginal.Length do
  begin
    LValue := Copy(LValueOriginal, i, 1);
    if (CNumbers.Contains(LValue)) then
      Result := Result + LValue;
  end;
{$ENDIF}
end;

class function TUtils.LerTxt(const aFileFullPath: String): String;
var
  VArq: TextFile; { declarando a variável "arq" do tipo arquivo texto }
  vStrList: TStringList;
  vLinha: string;
begin
  try
    vStrList := TStringList.Create;
    // [ 1 ] Associa a variável do programa "arq" ao arquivo externo "tabuada.txt"
    // na unidade de disco "d"
    AssignFile(VArq, aFileFullPath);

{$I-}         // desativa a diretiva de Input
    Reset(VArq); // [ 3 ] Abre o arquivo texto para leitura
{$I+}         // ativa a diretiva de Input

    if (IOResult <> 0) // verifica o resultado da operação de abertura
    then
      Result := '[ERRO] Erro na abertura do arquivo'
    else
    begin
      // [ 11 ] verifica se o ponteiro de arquivo atingiu a marca de final de arquivo
      while (not Eof(VArq)) do
      begin
        ReadLn(VArq, vLinha);
        // [ 6 ] Lê uma linha do arquivo
        vStrList.Add(vLinha);
      end;

      CloseFile(VArq); // [ 8 ] Fecha o arquivo texto aberto

      Result := vStrList.Text
    end;
  finally
    try
      FreeAndNil(vStrList);
    except
    end;
  end;
end;

class function TUtils.LogPath: string;
begin
  Result := TUtils.GetApplicationPath;
  Result := TUtils.Combine(Result, ['Log']);
end;

class procedure TUtils.LogWrite(const AMsg: string; const ALogType: TLogType);
var
  LMsg: string;
  LPathFile: string;
  LFileName: string;

  // LStrList: TStringList;
begin
  LMsg := '';

  case ALogType of
    ltWarning:
      LMsg := 'WARNING';
    ltError:
      LMsg := 'ERROR ';
  end;

  LMsg := LMsg + FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now) + ' ' + AMsg;
  LFileName := FormatDateTime('yyyymmdd', Now) + '.log';

  LPathFile := LogPath;
  if not(DirectoryExists(LPathFile)) then
    ForceDirectories(LPathFile);

  LPathFile := TUtils.Combine(LPathFile, [LFileName]);

  EventLog(LMsg);

  TFile.AppendAllText(LPathFile, LMsg, TEncoding.UTF8);
end;

class procedure TUtils.LogWriteError(const AMsg: string);
begin
  LogWrite(AMsg, TLogType.ltError);
end;

class procedure TUtils.LogWriteInformation(const AMsg: string);
begin
  LogWrite(AMsg, TLogType.ltInformation)
end;

class procedure TUtils.LogWriteWarning(const AMsg: string);
begin
  LogWrite(AMsg, TLogType.ltWarning)
end;

class procedure TUtils.MemoryUsage(out ATotal, AAllocated, AFree: Double);
const
  cBytesPerMegabyte = 1024 * 1024;
{$IF defined(MSWINDOWS)}
var
  LMemManStt: THeapStatus;
{$IFEND}
begin
{$IF defined(KASTRI) and DEFINED (ANDROID)}
  ATotal := TJDebug.JavaClass.getNativeHeapSize;
  AAllocated := TJDebug.JavaClass.getNativeHeapAllocatedSize;
  AFree := TJDebug.JavaClass.getNativeHeapFreeSize;
{$ELSEIF defined(MSWINDOWS)}
  LMemManStt := System.GetHeapStatus;
  ATotal := LMemManStt.TotalCommitted;
  AAllocated := LMemManStt.TotalAllocated;
  AFree := LMemManStt.TotalFree;
{$ELSE}
  ATotal := 0;
  AAllocated := 0;
  AFree := 0;
{$IFEND}
  ATotal := RoundTo(ATotal / cBytesPerMegabyte, -2);
  AAllocated := RoundTo(AAllocated / cBytesPerMegabyte, -2);
  AFree := RoundTo(AFree / cBytesPerMegabyte, -2);

end;

class procedure TUtils.SetWebBrowserPermissions;
{$IFDEF MSWINDOWS}
(* const
  cHomePath = 'SOFTWARE';
  cFeatureBrowserEmulation =
  'Microsoft\Internet Explorer\Main\FeatureControl\FEATURE_BROWSER_EMULATION\';
  cIE11 = 11001;

  var
  Reg: TRegIniFile;
  sKey: string; *)
var
  lpEntryInfo: PInternetCacheEntryInfo;
  hCacheDir: LongWord;
  dwEntrySize: LongWord;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  (* sKey := ExtractFileName(ParamStr(0));
    Reg := TRegIniFile.Create(cHomePath);
    try
    if Reg.OpenKey(cFeatureBrowserEmulation, True) and
    not(TRegistry(Reg).KeyExists(sKey) and (TRegistry(Reg).ReadInteger(sKey)
    = cIE11)) then
    TRegistry(Reg).WriteInteger(sKey, cIE11);
    finally
    Reg.Free;
    end; *)

  { DeleteIECache }
  dwEntrySize := 0;

  FindFirstUrlCacheEntry(nil, TInternetCacheEntryInfo(nil^), dwEntrySize);

  GetMem(lpEntryInfo, dwEntrySize);

  if dwEntrySize > 0 then
    lpEntryInfo^.dwStructSize := dwEntrySize;

  hCacheDir := FindFirstUrlCacheEntry(nil, lpEntryInfo^, dwEntrySize);

  if hCacheDir <> 0 then
  begin
    repeat
      DeleteUrlCacheEntry(lpEntryInfo^.lpszSourceUrlName);
      FreeMem(lpEntryInfo, dwEntrySize);
      dwEntrySize := 0;
      FindNextUrlCacheEntry(hCacheDir, TInternetCacheEntryInfo(nil^),
        dwEntrySize);
      GetMem(lpEntryInfo, dwEntrySize);
      if dwEntrySize > 0 then
        lpEntryInfo^.dwStructSize := dwEntrySize;
    until not FindNextUrlCacheEntry(hCacheDir, lpEntryInfo^, dwEntrySize);
  end;
  { hCacheDir<>0 }
  FreeMem(lpEntryInfo, dwEntrySize);

  FindCloseUrlCache(hCacheDir)
{$ENDIF}
end;

class procedure TUtils.ShareSheetFile(const AFilePathFull: string;
{$IF NOT (DEFINED (KASTRI) AND (DEFINED (ANDROID) OR DEFINED(IOS)))} AFileType: TFileType{$ELSE}
const AControl: TControl; AOnCompletedEvent: TShareCompletedEvent
{$ENDIF});
var
{$IF DEFINED (KASTRI) AND (DEFINED (ANDROID) OR DEFINED(IOS))}
  LShareItems: TShareItems;
  LExcluded: TShareActivities;
{$ELSEIF DEFINED (ANDROID)}
  IntentShare: JIntent;
  Uri: Jnet_Uri;
  Uris: JArrayList;
  AttFile: Jfile;
{$ENDIF}
  LFileType: string;
begin
{$IF DEFINED (KASTRI) AND (DEFINED (ANDROID) OR DEFINED(IOS))}
  LShareItems := TShareItems.Create;
  LExcluded := [];
  try
{$IFDEF IOS}
    LExcluded := TShareItems.AllShareActivities - [TShareActivity.Message, TShareActivity.Mail, TShareActivity.CopyToPasteboard];
{$ENDIF}
    var
    LDoCompletedHandler := ShareFilesCompletedHandler;
    if Assigned(AOnCompletedEvent) then
      LDoCompletedHandler := AOnCompletedEvent;
    LShareItems.OnShareCompleted := LDoCompletedHandler;
    LShareItems.AddFile(AFilePathFull);
    LShareItems.Share(AControl, LExcluded);
  finally
  end;
{$ELSEIF DEFINED (ANDROID)}
  case AFileType of
    ftPdf:
      LFileType := 'Application/pdf';
  else
    raise Exception.Create('ShareSheetFile unknown File Type: ' + GetEnumName(AFileType));
  end;
  IntentShare := TJIntent.JavaClass.init(TJIntent.JavaClass.ACTION_SEND);
  Uris := TJArrayList.Create;
  AttFile := TJfile.JavaClass.init(StringToJString(AFilePathFull));
  Uri := TJnet_Uri.JavaClass.fromFile(AttFile);
  Uris.Add(0, Uri);
  IntentShare.putExtra(TJIntent.JavaClass.EXTRA_TEXT, StringToJString(''));
  IntentShare.setType(StringToJString(LFileType));
  IntentShare.putParcelableArrayListExtra(TJIntent.JavaClass.EXTRA_STREAM, Uris);
  TAndroidHelper.Activity.startActivity(
    TJIntent.JavaClass.createChooser(IntentShare, StrToJCharSequence('Share with:')));
{$ELSE}
  raise Exception.Create('ShareSheetFile not implemented');
{$ENDIF}
end;

{$IF DEFINED (KASTRI) AND (DEFINED (ANDROID) OR DEFINED(IOS))}

class procedure TUtils.ShareFilesCompletedHandler(Sender: TObject; const AActivity: TShareActivity; const AError: string);
begin
  var
  LRef := EmptyStr;
  if not AError.Trim.IsEmpty then
    LRef := AError.Trim
  else if AActivity = TShareActivity.None then
    LRef := 'Share cancelled'
  else
    LRef := 'Share completed';
end;
{$ENDIF}

class procedure TUtils.ShareSheetText(const AText: string);
var
{$IFDEF ANDROID}
  IntentWhats: JIntent;
{$ENDIF}
  LMsg: string;
begin
  LMsg := AText;
{$IFDEF ANDROID}
  IntentWhats := TJIntent.JavaClass.init(TJIntent.JavaClass.ACTION_SEND);
  IntentWhats.setType(StringToJString('text/plain'));
  IntentWhats.putExtra(TJIntent.JavaClass.EXTRA_TEXT, StringToJString(LMsg));
  SharedActivity.startActivity(IntentWhats);
{$ELSE}
  raise Exception.Create('ShareSheetText is not implemented');
{$ENDIF}
end;

class function TUtils.StreamToString(aStream: TStream): string;
var
  SS: TStringStream;
begin
  if aStream <> nil then
  begin
    SS := TStringStream.Create('');
    try
      SS.CopyFrom(aStream, 0);
      // No need to position at 0 nor provide size
      Result := SS.DataString;
    finally
      try
        FreeAndNil(SS);
      except
      end;
    end;
  end
  else
  begin
    Result := '';
  end;
end;

class function TUtils.StringToAlphaColor(const AColor: string): TAlphaColor;
begin
  Result := System.UIConsts.StringToAlphaColor(AColor);
end;

class function TUtils.StrRightStr(const AText: string;
const ACount: Integer): string;
begin
  Result := RightStr(AText, ACount);
end;

class procedure TUtils.ThrowExceptionCouldNotFindTheResource(AValue: string);
begin
  if not AValue.Trim.IsEmpty then
    AValue := ': ' + AValue;
  raise Exception.Create(Format('Daten wurden nicht gefunden%s', [AValue]));
end;

class procedure TUtils.ThrowExceptionForbidden;
begin
  raise Exception.Create('Forbidden');
end;

class procedure TUtils.ThrowExceptionInvalidJson;
begin
  raise Exception.Create('Invalid JSON');
end;

class procedure TUtils.ThrowExceptionInvalidJsonValue(const AValue: string);
var
  LValue: string;
begin
  LValue := AValue;
  if not LValue.Trim.IsEmpty then
    LValue := ': ' + LValue;
  raise Exception.Create(Format('Invalid JSON value%s', [LValue]));
end;

class procedure TUtils.ThrowExceptionInvalidValue(const AValue: string);
var
  LValue: string;
begin
  LValue := AValue;
  if not LValue.Trim.IsEmpty then
    LValue := ': ' + LValue;
  raise Exception.Create(Format('Invalid value%s', [LValue]));
end;

class procedure TUtils.ThrowExceptionMethodIsNotImplemented(AValue: string);
begin
  if not AValue.Trim.IsEmpty then
    AValue := ': ' + AValue;
  raise Exception.Create(Format('Method is not implemented%s', [AValue]));
end;

class procedure TUtils.ThrowExceptionParamIsRequired(const AValue: string);
begin
  raise Exception.Create(Format('Param [%s] is required', [AValue]));
end;

class procedure TUtils.ThrowExceptionSomethingWentWrong(AValue: string);
var
  LValue: string;
begin
  if AValue.Trim.IsEmpty then
    LValue := EmptyStr
  else
    LValue := ': ' + AValue;
  raise Exception.Create(Format('Something went wrong%s', [LValue]));
end;

class procedure TUtils.ThrowExceptionTheSessionHasExpired;
begin
  raise Exception.Create('Your session has expired, please log in again!');
end;

class procedure TUtils.ThrowExceptionValueCannotBeEmptyOrNull(AValue: string);
var
  LValue: string;
begin
  if AValue.Trim.IsEmpty then
    LValue := EmptyStr
  else
    LValue := ': ' + AValue;
  raise Exception.Create(Format('The value cannot be empty or null%s', [LValue]));
end;

class function TUtils.TryStrToDateTime(ADateTimeStr: string; var ADateTime: TDateTime): Boolean;
var
  LDateSeparator: Char;
  LShortDate: string;
  LDate: string;
  LShortTime: string;
  LTime: string;

  LDateSeparatorFormat: Char;
  LShortDateFormat: string;
  LLongDateFormat: string;
  LShortTimeFormat: string;
  LLongTimeFormat: string;
begin
  Result := False;
  ADateTime := 0;

  // Default
  LDateSeparator := '/';
  LShortDate := 'dd/mm';
  LDate := 'dd/mm/yyyy';
  LShortTime := 'hh:nn';
  LTime := 'hh:nn:ss.zzz';

  // Get current formats
  LDateSeparatorFormat := FormatSettings.DateSeparator;
  LShortDateFormat := FormatSettings.ShortDateFormat;
  LLongDateFormat := FormatSettings.LongDateFormat;
  LShortTimeFormat := FormatSettings.ShortTimeFormat;
  LLongTimeFormat := FormatSettings.LongTimeFormat;

  ADateTimeStr := UpperCase(ADateTimeStr);

  if
    (System.Masks.MatchesMask(ADateTimeStr, '[0-9][0-9]/[0-9][0-9]/[0-9][0-9][0-9][0-9]T[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9][0-9]Z')) // dd/mm/yyyyThh:nn:ss.zzzZ
    or (System.Masks.MatchesMask(ADateTimeStr, '[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]T[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9][0-9]Z')) // yyyy-mm-ddThh:nn:ss.zzzZ
  then
  begin
    ADateTimeStr := StringReplace(ADateTimeStr, 'T', ' ', []);
    ADateTimeStr := StringReplace(ADateTimeStr, 'Z', EmptyStr, []);
  end;

  if (System.Masks.MatchesMask(ADateTimeStr, '[0-9][0-9]/[0-9][0-9]/[0-9][0-9][0-9][0-9]')) // dd/mm/yyyy
    or (System.Masks.MatchesMask(ADateTimeStr, '[0-9][0-9]/[0-9][0-9]/[0-9][0-9][0-9][0-9] [0-9][0-9]:[0-9][0-9]:[0-9][0-9]')) // dd/mm/yyyy hh:nn:ss
    or (System.Masks.MatchesMask(ADateTimeStr, '[0-9][0-9]/[0-9][0-9]/[0-9][0-9][0-9][0-9] [0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9][0-9]')) // dd/mm/yyyy hh:nn:ss.zzz
  then
  begin
    LDateSeparator := '/';
    LShortDate := 'dd/mm';
    LDate := 'dd/mm/yyyy';
  end
  else
    if (System.Masks.MatchesMask(ADateTimeStr, '[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]')) // yyyy-mm-dd
    or (System.Masks.MatchesMask(ADateTimeStr, '[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9] [0-9][0-9]:[0-9][0-9]:[0-9][0-9]')) // yyyy-mm-dd hh:nn:ss
    or (System.Masks.MatchesMask(ADateTimeStr, '[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9] [0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9][0-9]')) // yyyy-mm-dd hh:nn:ss.zzz
  then
  begin
    LDateSeparator := '-';
    LShortDate := 'yyyy-mm-dd';
    LDate := 'yyyy-mm-dd';
  end
  else
      ;
  FormatSettings.DateSeparator := LDateSeparator;
  FormatSettings.ShortDateFormat := LShortDate;
  FormatSettings.LongDateFormat := LDate;
  FormatSettings.ShortTimeFormat := LShortTime;
  FormatSettings.LongTimeFormat := LTime;

  Result := System.SysUtils.TryStrToDateTime(ADateTimeStr, ADateTime);

  FormatSettings.DateSeparator := LDateSeparatorFormat;
  FormatSettings.ShortDateFormat := LShortDateFormat;
  FormatSettings.LongDateFormat := LLongDateFormat;
  FormatSettings.ShortTimeFormat := LShortTimeFormat;
  FormatSettings.LongTimeFormat := LLongTimeFormat;
end;

class function TUtils.UniversalTimeToLocal(AUtc: Int64; AIsUTC: Boolean): TDateTime;
begin
  Result := System.DateUtils.UnixToDateTime(AUtc, AIsUTC);
end;

class function TUtils.UnZipFile(const AZipFile, APathToExtract: string): Boolean;
var
  LZip: TZipFile;
begin
  Result := False;
  try
    LZip := TZipFile.Create;
    LZip.Open(AZipFile, zmRead);
    LZip.ExtractAll(APathToExtract);
    LZip.Close;
    Result := True;
  finally
    FreeAndNil(LZip);
  end;
end;

class function TUtils.ZipFile(const AZipFile, AFileName: string): Boolean;
var
  LZip: TZipFile;
begin
  Result := False;
  try
    LZip := TZipFile.Create;
    if FileExists(AZipFile) then
      LZip.Open(AZipFile, zmReadWrite)
    else
      LZip.Open(AZipFile, zmWrite);
    LZip.Add(AFileName, ExtractFileName(AFileName));
    LZip.Close;
    Result := True;
  finally
    FreeAndNil(LZip);
  end;
end;

end.
