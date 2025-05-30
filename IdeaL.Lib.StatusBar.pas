unit IdeaL.Lib.StatusBar;

{
Tests on:
Delphi 10.4.1

iOS 14.4

Andrdoi 10

References:
for iOS https://github.com/rzaripov1990/FMX.StatusBar/blob/master/FMX.StatusBar.pas
for Android (Sorry, I really don't remember where I took that sample)
}

interface

uses
  System.UITypes,
  System.SysUtils,

  FMX.Platform,
  FMX.Forms,
  FMX.Graphics
{$IF defined(ANDROID)}
    , Androidapi.JNIBridge,
  Androidapi.Helpers,
  Androidapi.JNI.OS,
  Androidapi.JNI.JavaTypes,
  Fmx.Helpers.Android,


  Androidapi.JNI.Location,
  Androidapi.JNI.GraphicsContentViewText,

  Androidapi.JNI.App
{$ELSEIF defined(IOS)}
    , FMX.Helpers.iOS,
  FMX.Platform.iOS,
  iOSapi.UIKit,
  iOSapi.Foundation,
  iOSapi.CoreGraphics,
  iOSapi.CoreImage
{$ENDIF};

type
  TStatusBar = class
  private
{$IFDEF IOS}
{$IF CompilerVersion < 32.0} // Seattle, Berlin, earlier
    class function GetStatusBarView: UIView;
    class procedure SetStatusBarBackgroundColor(const ABackgroundColor: TAlphaColor);
{$ENDIF}
{$ENDIF}
    { private declarations }
  protected
    { protected declarations }
  public
    class procedure SetBackgroundColor(const AForm: TForm; const AColor: TAlphaColor);
    { public declarations }
  published
    { published declarations }
  end;

implementation

{ TStatusBar }

{$IFDEF IOS}
{$IF CompilerVersion < 32.0}
class function TStatusBarGetStatusBarView: UIView;
var
  I: Integer;
  LViews: NSArray;
  LView: UIView;
begin
  Result := nil;
  LViews := SharedApplication.keyWindow.rootViewController.view.subviews;
  for I := 0 to LViews.count - 1 do
  begin
    LView := TUIView.Wrap(LViews.objectAtIndex(I));
    if CGRectEqualToRect(LView.frame, SharedApplication.statusBarFrame) <> 0 then
    begin
      Result := LView;
      Break;
    end;
  end;
end;

class procedure TStatusBar.SetStatusBarBackgroundColor(const ABackgroundColor: TAlphaColor);
var
  Red: Single;
  Green: Single;
  Blue: Single;
  ColorCI: CIColor;
  ColorUI: UIColor;
  StatusBarView: UIView;
begin
  StatusBarView := GetStatusBarView;
  if StatusBarView = nil then
    Exit;
  Red := TAlphaColorRec(ABackgroundColor).r / 255;
  Green := TAlphaColorRec(ABackgroundColor).g / 255;
  Blue := TAlphaColorRec(ABackgroundColor).b / 255;
  ColorCI := TCIColor.Wrap(TCIColor.OCClass.colorWithRed(Red, Green, Blue));
  ColorUI := TUIColor.Wrap(TUIColor.OCClass.colorWithCIColor(ColorCI));
  StatusBarView.SetBackgroundColor(ColorUI);
  if TOSVersion.Check(7, 0) then
    SharedApplication.keyWindow.rootViewController.setNeedsStatusBarAppearanceUpdate;
end;
{$ENDIF}
{$ENDIF}

class procedure TStatusBar.SetBackgroundColor(const AForm: TForm;
  const AColor: TAlphaColor);
begin
{$IFDEF IOS}
{$IF CompilerVersion >= 32.0} // Tokyo, later
  AForm.SystemStatusBar.Visibility := TFormSystemStatusBar.TVisibilityMode.Visible;
  AForm.SystemStatusBar.BackgroundColor := AColor;
{$ENDIF}

{$IF CompilerVersion < 32.0} // Seattle, Berlin, earlier
  SetStatusBarBackgroundColor(AColor);
{$ENDIF}
{$ENDIF}

{$IFDEF ANDROID}
  CallInUIThreadAndWaitFinishing(
    procedure
    var
      LActivity: JActivity;
      LWindow: JWindow;
      function IsLightColor(Color: TColor): Boolean;
      begin
        // Color := ColorToRGB(Color);
        Result := ((Color and $FF) + (Color shr 8 and $FF) +
        (Color shr 16 and $FF)) >= $180;
      end;
    begin
      LActivity := TAndroidHelper.Activity;
      LWindow := LActivity.getWindow;

      if IsLightColor(AColor) then
      begin // Ligth StatusBar with Dark details
        LWindow.getDecorView.setSystemUiVisibility
          (TJView.JavaClass.SYSTEM_UI_FLAG_LIGHT_STATUS_BAR);
      end
      else
      begin // Dark StatusBar with Light details
        LWindow.getDecorView.setSystemUiVisibility
          (TJView.JavaClass.SYSTEM_UI_FLAG_LIGHT_STATUS_BAR xor
          TJView.JavaClass.SYSTEM_UI_FLAG_LIGHT_STATUS_BAR);
      end;

      LWindow.setStatusBarColor(AColor);
      LWindow.setNavigationBarColor(AColor);
    end);
{$ENDIF}
end;

end.

(*OLD
interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.UIConsts,

  FMX.Types,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Platform

{$IFDEF ANDROID}
    , Androidapi.Helpers,
  Androidapi.Jni.GraphicsContentViewText,
  Androidapi.Jni.App,
  FMX.Helpers.Android
{$ENDIF};

type
  TStatusBar = class
  private
    class var FTransparente: Boolean;
    class var FCorStatusBar: TAlphaColor;
    class var FDark: Boolean;

    class procedure ChangeColorPrivate(const AColor: TAlphaColor;
      const ADark: Boolean = True); overload;
    class procedure ChangeColorPrivate(const AColor: string;
      const ADark: Boolean = True); overload;
    { private declarations }
  protected

    { protected declarations }
  public
    class procedure Transparent(AIsTransparent: Boolean);
    class procedure ChangeColor(const AColor: TAlphaColor;
      const ADark: Boolean = True); overload;
    class procedure ChangeColor(const AColor: string;
      const ADark: Boolean = True); overload;
    { public declarations }
  published
    class function AppEvent(AAppEvent: TApplicationEvent;
      AContext: TObject): Boolean;
    class procedure AppInactive;
    { published declarations }
  end;

implementation

{ TStatusBar }

class function TStatusBar.AppEvent(AAppEvent: TApplicationEvent;
  AContext: TObject): Boolean;
begin
{$IFDEF ANDROID}
  if (AAppEvent = TApplicationEvent.BecameActive) then
  begin
    Transparent(FTransparente);
    ChangeColorPrivate(FCorStatusBar, FDark);
  end;
{$ENDIF}
end;

class procedure TStatusBar.AppInactive;
var
  LAppEventSvc: IFMXApplicationEventService;
begin
  // Quando o aplicativo ficar inativo
  if TPlatformServices.Current.SupportsPlatformService
    (IFMXApplicationEventService, IInterface(LAppEventSvc))
  then
  begin
    LAppEventSvc.SetApplicationEventHandler(AppEvent);
  end;
end;

class procedure TStatusBar.ChangeColor(const AColor: string;
  const ADark: Boolean);
begin
  ChangeColor(StringToAlphaColor(AColor), ADark);
end;

class procedure TStatusBar.ChangeColor(const AColor: TAlphaColor;
  const ADark: Boolean);
begin
{$IFDEF ANDROID OR IOS}
  CallInUIThreadAndWaitFinishing(
    procedure
    begin
      ChangeColorPrivate(AColor, ADark);
    end);
{$ENDIF}
end;

class procedure TStatusBar.ChangeColorPrivate(const AColor: string;
const ADark: Boolean);
begin
  ChangeColorPrivate(StringToAlphaColor(AColor), ADark);
end;

class procedure TStatusBar.ChangeColorPrivate(const AColor: TAlphaColor;
const ADark: Boolean);
{$IFDEF ANDROID}
var
  LActivity: JActivity;
  LWindow: JWindow;
{$ENDIF}
begin
{$IFDEF ANDROID}
  LActivity := TAndroidHelper.Activity;
  LWindow := LActivity.getWindow;

  AppInactive;

  if ADark = True then
  begin // StatusBar clara, com elementos pretos
    LWindow.getDecorView.setSystemUiVisibility
      (TJView.JavaClass.SYSTEM_UI_FLAG_LIGHT_STATUS_BAR);
  end
  else if ADark = False then
  begin // StatusBar escura, com elementos claros
    LWindow.getDecorView.setSystemUiVisibility
      (TJView.JavaClass.SYSTEM_UI_FLAG_LIGHT_STATUS_BAR xor
      TJView.JavaClass.SYSTEM_UI_FLAG_LIGHT_STATUS_BAR);
  end;

  LWindow.setStatusBarColor(AColor);

  FDark := ADark;
  FCorStatusBar := AColor;
{$ENDIF}
end;

class procedure TStatusBar.Transparent(AIsTransparent: Boolean);
{$IFDEF ANDROID}
var
  LActivity: JActivity;
  LWindow: JWindow;
{$ENDIF}
begin
{$IFDEF ANDROID}
  LActivity := TAndroidHelper.Activity;
  LWindow := LActivity.getWindow;

  // StatusBar e NavBar Transparente
  if AIsTransparent = True then
  begin
    LWindow.setFlags(TJWindowManager_LayoutParams.JavaClass.
      FLAG_TRANSLUCENT_STATUS,
      TJWindowManager_LayoutParams.JavaClass.FLAG_TRANSLUCENT_STATUS);

    LWindow.setFlags(TJWindowManager_LayoutParams.JavaClass.
      FLAG_LAYOUT_NO_LIMITS,
      TJWindowManager_LayoutParams.JavaClass.FLAG_LAYOUT_NO_LIMITS);
  end;

  // StatusBar e NavBar convencionais (sem transpar�ncia)
  if AIsTransparent = False then
  begin
    LWindow.clearFlags(TJWindowManager_LayoutParams.JavaClass.
      FLAG_TRANSLUCENT_STATUS);
    LWindow.clearFlags(TJWindowManager_LayoutParams.JavaClass.
      FLAG_LAYOUT_NO_LIMITS);
  end;

  FTransparente := AIsTransparent;
{$ENDIF}
end;

end.*)