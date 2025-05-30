unit IdeaL.Lib.Android.Permissions;

interface

uses
  System.SysUtils,
  System.Permissions,
{$IF CompilerVersion >= 35.0}
  System.Types,
{$IFEND}
  FMX.DialogService

{$IFDEF ANDROID}
    ,
  Androidapi.Helpers,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Os
{$ENDIF}
    ;

type
  TPermissionsType = (
    ptReadPhoneState,
    ptReadExternalStorage,
    ptWriteExternalStorage,
    ptAccessLocaltionExtraCommands,
    ptWakeLock,
    ptCamera,
    ptCallPhone,
    ptPostNotifications, // Android13+ https://developer.android.com/develop/ui/views/notifications/notification-permission
    // https://developer.android.com/about/versions/13/behavior-changes-13
    ptReadMediaImages,
    ptReadMediaVideo,
    ptReadMediaAudio
    //
    );

type
  TPermissions = class
  private
    class var FDoRequestPermissionsResult: TProc;
    class var FDoWhenPermissionsDoNotGranted: TProc;
    class var FPermissionGaranted: Boolean;
    class function GetArrayPermissions(APermissionsTypes: array of TPermissionsType): TArray<string>;
    class procedure DisplayRationale(
      Sender: TObject;
{$IF CompilerVersion >= 35.0}
      const APermissions: TClassicStringDynArray;
{$ELSE}
      const APermissions: TArray<string>;
{$IFEND}
      const APostRationaleProc: TProc
      );
    class function ArrayPermissionsContains(
      const APermissionsTypes: array of TPermissionsType;
      const APermissionsType: TPermissionsType
      ): Boolean;
    class procedure OnRequestPermissionsResultEvent(
      Sender: TObject;
{$IF CompilerVersion >= 35.0}
      const APermissions: TClassicStringDynArray;
      const AGrantResults: TClassicPermissionStatusDynArray
{$ELSE}
      const APermissions: TArray<string>;
      const AGrantResults: TArray<TPermissionStatus>
{$IFEND}
      );
    { private declarations }
  protected
    { protected declarations }
  public
    class function IsPermissionGranted(APermissionsType: TPermissionsType): Boolean; overload;
    class function IsPermissionGranted(APermissionsTypes: array of TPermissionsType): Boolean; overload;
    class function GetPermissions(
      const APermissionsTypes: array of TPermissionsType;
      ADoRequestPermissionsResult: TProc;
      ADoWhenPermissionsDoNotGranted: TProc
      ): Boolean;
    { public declarations }
  published
    { published declarations }
  end;

implementation

{ TPermissions }

class function TPermissions.ArrayPermissionsContains(const APermissionsTypes
  : array of TPermissionsType;
  const APermissionsType: TPermissionsType): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := Low(APermissionsTypes) to High(APermissionsTypes) do
    if APermissionsTypes[i] = APermissionsType then
    begin
      Result := True;
      Break;
    end;
end;

class procedure TPermissions.DisplayRationale(
  Sender: TObject;
{$IF CompilerVersion >= 35.0}
  const APermissions: TClassicStringDynArray;
{$ELSE}
  const APermissions: TArray<string>;
{$IFEND}
  const APostRationaleProc: TProc);
begin
  APostRationaleProc;
end;

class function TPermissions.GetArrayPermissions(
  APermissionsTypes: array of TPermissionsType): TArray<string>;
begin
  SetLength(Result, Length(APermissionsTypes));
{$IF defined(ANDROID)}
  var i: integer := 0;
  // ReadPhoneState
  if ArrayPermissionsContains(APermissionsTypes,
    TPermissionsType.ptReadPhoneState)
  then
  begin
    Result[i] := JStringToString
      (TJManifest_permission.JavaClass.READ_PHONE_STATE);
    Inc(i, 1);
  end;
  // ReadExternalStorage
  if ArrayPermissionsContains(APermissionsTypes,
    TPermissionsType.ptReadExternalStorage)
  then
  begin
    Result[i] := JStringToString
      (TJManifest_permission.JavaClass.READ_EXTERNAL_STORAGE);
    Inc(i, 1);
  end;
  // WriteExternalStorage
  if ArrayPermissionsContains(APermissionsTypes,
    TPermissionsType.ptWriteExternalStorage)
  then
  begin
    Result[i] := JStringToString
      (TJManifest_permission.JavaClass.WRITE_EXTERNAL_STORAGE);
    Inc(i, 1);
  end;
  // AccessLocationExtraCommands
  if ArrayPermissionsContains(APermissionsTypes,
    TPermissionsType.ptAccessLocaltionExtraCommands)
  then
  begin
    Result[i] := JStringToString
      (TJManifest_permission.JavaClass.ACCESS_LOCATION_EXTRA_COMMANDS);
    Inc(i, 1);
  end;
  // WakeLock
  if ArrayPermissionsContains(APermissionsTypes, TPermissionsType.ptWakeLock)
  then
  begin
    Result[i] := JStringToString(TJManifest_permission.JavaClass.WAKE_LOCK);
    Inc(i, 1);
  end;
  // Camera
  if ArrayPermissionsContains(APermissionsTypes, TPermissionsType.ptCamera) then
  begin
    Result[i] := JStringToString(TJManifest_permission.JavaClass.CAMERA);
    Inc(i, 1);
  end;
  // CallPhone
  if ArrayPermissionsContains(APermissionsTypes, TPermissionsType.ptCallPhone) then
  begin
    Result[i] := JStringToString(TJManifest_permission.JavaClass.CALL_PHONE);
    Inc(i, 1);
  end;
  // Post Notifications
  if ArrayPermissionsContains(APermissionsTypes, TPermissionsType.ptPostNotifications) then
  begin
    Result[i] := 'android.permission.POST_NOTIFICATIONS';
    Inc(i, 1);
  end;
  //
  if ArrayPermissionsContains(APermissionsTypes, TPermissionsType.ptReadMediaImages) then
  begin
    Result[i] := 'android.permission.READ_MEDIA_IMAGES';
    Inc(i, 1);
  end;
  //
  if ArrayPermissionsContains(APermissionsTypes, TPermissionsType.ptReadMediaVideo) then
  begin
    Result[i] := 'android.permission.READ_MEDIA_VIDEO';
    Inc(i, 1);
  end;
  //
  if ArrayPermissionsContains(APermissionsTypes, TPermissionsType.ptReadMediaAudio) then
  begin
    Result[i] := 'android.permission.READ_MEDIA_AUDIO';
    Inc(i, 1);
  end;
{$IFEND}
end;

class function TPermissions.GetPermissions(const APermissionsTypes
  : array of TPermissionsType; ADoRequestPermissionsResult: TProc;
  ADoWhenPermissionsDoNotGranted: TProc): Boolean;
var
  i: Integer;
  LArr: TArray<string>;
begin
  Result := False;

  var
  LLength := Length(APermissionsTypes);
  SetLength(LArr, LLength);
  i := 0;

  FDoRequestPermissionsResult := ADoRequestPermissionsResult;
  FDoWhenPermissionsDoNotGranted := ADoWhenPermissionsDoNotGranted;

  LArr := GetArrayPermissions(APermissionsTypes);

  PermissionsService.RequestPermissions(
    LArr,
    OnRequestPermissionsResultEvent,
    DisplayRationale
    );
end;

class function TPermissions.IsPermissionGranted(
  APermissionsType: TPermissionsType): Boolean;
begin
  Result := IsPermissionGranted([APermissionsType]);
end;

class function TPermissions.IsPermissionGranted(
  APermissionsTypes: array of TPermissionsType): Boolean;
var
  LArr: TArray<string>;
begin
{$IF defined(ANDROID) or defined(IOS)}
  LArr := GetArrayPermissions(APermissionsTypes);
  Result := PermissionsService.IsEveryPermissionGranted(LArr);
{$ELSE}
  Result := True;
{$ENDIF}
end;

class procedure TPermissions.OnRequestPermissionsResultEvent(Sender: TObject;
{$IF CompilerVersion >= 35.0}
  const APermissions: TClassicStringDynArray;
  const AGrantResults: TClassicPermissionStatusDynArray
{$ELSE}
  const APermissions: TArray<string>;
  const AGrantResults: TArray<TPermissionStatus>
{$IFEND}
  );
var
  LPermissionsGranted: Boolean;
  i: Integer;
  LText: string;
begin
  LPermissionsGranted := True;

  for i := 0 to Pred(Length(AGrantResults)) do
  begin
    LText := APermissions[i];
    if not(AGrantResults[i] = TPermissionStatus.Granted) then
    begin
      LPermissionsGranted := False;
      Break;
    end;
  end;

  if not(LPermissionsGranted) then
  begin
    if (Assigned(FDoWhenPermissionsDoNotGranted)) then
      FDoWhenPermissionsDoNotGranted;
    Exit;
  end;

  if (Assigned(FDoRequestPermissionsResult)) then
    FDoRequestPermissionsResult;
end;

end.

{$IFDEF IOS},
  iOSapi.AVFoundation
{$ENDIF}
  class procedure RequestRecordVideoHandlerOniOS
(AGranted: Boolean);

{$IFDEF IOS}
// Camera
if ArrayPermissionsContains
(APermissionsTypes, TPermissionsType.ptCamera) then
begin
  TAVCaptureDevice.OCClass.requestAccessForMediaType(AVMediaTypeVideo, RequestRecordVideoHandlerOniOS);
end
else if
(Assigned(FDoRequestPermissionsResult)) then
  FDoRequestPermissionsResult;
{$ELSE}
if
(Assigned(FDoRequestPermissionsResult)) then
  FDoRequestPermissionsResult;
{$ENDIF}

class procedure TPermissions.RequestRecordVideoHandlerOniOS
(AGranted: Boolean);
begin
  TThread.Synchronize
(TThread.CurrentThread,
  procedure
begin
  if AGranted then
begin
  if (Assigned(FDoRequestPermissionsResult)) then
  FDoRequestPermissionsResult;
end
else
begin
  if (Assigned(FDoWhenPermissionsDoNotGranted)) then
  FDoWhenPermissionsDoNotGranted;
end;
end);
end;
