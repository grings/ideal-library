unit Utils;

interface

uses
  IdeaL.Lib.Android.Permissions,

  System.SysUtils;

type
  TUtils = class
  private
    { private declarations }
  protected
    { protected declarations }
  public
    class procedure GetPermissionReadWriteExternalStorage(
      ADoRequestPermissionsResult: TProc;
      ADoWhenPermissionsDoNotGranted: TProc);

    { public declarations }
  published
    { published declarations }
  end;

implementation

{ TUtils }

class procedure TUtils.GetPermissionReadWriteExternalStorage(
  ADoRequestPermissionsResult, ADoWhenPermissionsDoNotGranted: TProc);
begin
{$IFDEF ANDROID}
  TPermissions.GetPermissions(
    [ptReadExternalStorage, ptWriteExternalStorage],
    ADoRequestPermissionsResult,
    ADoWhenPermissionsDoNotGranted
    );
{$ELSE}
   ADoRequestPermissionsResult;
{$ENDIF}
end;

end.
