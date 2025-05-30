unit FormMain;

// See IdeaL.Lib.PushNotification for all comments

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.StdCtrls, FMX.ScrollBox, FMX.Memo, FMX.Controls.Presentation, FMX.Layouts,
  FMX.Objects;

type
  TForm2 = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    memoMessage: TMemo;
    lblDeviceToken: TLabel;
    lblDeviceId: TLabel;
    Line1: TLine;
    Line2: TLine;
    lytBackground: TLayout;
    procedure lblDeviceTokenTap(Sender: TObject; const Point: TPointF);
    procedure lblDeviceTokenClick(Sender: TObject);
    procedure lytBackgroundPainting(Sender: TObject; Canvas: TCanvas;
      const ARect: TRectF);
  private
    procedure DoReceivingNotification(AMessage: string);
    procedure DoDeviceTokenHasBeenTaken;

    class function ClipboardCopyText(const AValue: string): Boolean;
    procedure StartPushNotificationService();
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

uses
  FMX.Platform,
  IdeaL.Lib.Utils,
  IdeaL.Lib.Android.Permissions,
  IdeaL.Lib.PushNotification;

{$R *.fmx}

{ TForm2 }

class function TForm2.ClipboardCopyText(const AValue: string): Boolean;
var
  Svc: IFMXClipboardService;
begin
  Result := False;
  if TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, Svc)
  then
  begin
    Svc.SetClipboard(AValue);
    Result := True;
  end;
end;

procedure TForm2.DoDeviceTokenHasBeenTaken;
begin
  lblDeviceToken.Text := TPushNotificationReceiver.Instance.DeviceToken;
end;

procedure TForm2.DoReceivingNotification(AMessage: string);
begin
  memoMessage.Lines.Add(AMessage);
end;

procedure TForm2.lblDeviceTokenClick(Sender: TObject);
begin
  if Assigned(TControl(Sender).OnTap) then
    TControl(Sender).OnTap(Sender, TPointF.Create(0, 0));
end;

procedure TForm2.lblDeviceTokenTap(Sender: TObject; const Point: TPointF);
begin
  if lblDeviceToken.Text.Trim.IsEmpty then
  begin
    TPushNotificationReceiver.Instance.Active := False;
    TPushNotificationReceiver.Instance.Active := True;
    lblDeviceToken.Text := TPushNotificationReceiver.Instance.DeviceToken;
  end;
  ClipboardCopyText(TLabel(Sender).Text);
end;

procedure TForm2.lytBackgroundPainting(Sender: TObject; Canvas: TCanvas;
  const ARect: TRectF);
begin
  if lytBackground.Tag <> 0 then
    Exit;
  lytBackground.Tag := 1;

  lblDeviceToken.Text := EmptyStr;
  lblDeviceId.Text := EmptyStr;

  if TUtils.GetOsVersionInt >= 13 then
  begin // Android13+ https://developer.android.com/develop/ui/views/notifications/notification-permission
    IdeaL.Lib.Android.Permissions.TPermissions.GetPermissions(
      [TPermissionsType.ptPostNotifications],
      StartPushNotificationService, nil
      );
  end
  else
    StartPushNotificationService;
end;

procedure TForm2.StartPushNotificationService;
var
  LArray: TArray<string>;
begin
  lblDeviceId.Text := TPushNotificationReceiver.Instance.DeviceId;
  TPushNotificationReceiver.Instance.OnReceiveNotificationEvent := DoReceivingNotification;
  TPushNotificationReceiver.Instance.OnDeviceTokenHasBeenTaken := DoDeviceTokenHasBeenTaken;
  LArray := TPushNotificationReceiver.Instance.StartupNotifications;
  for var i := 0 to Pred(Length(LArray)) do
    memoMessage.Lines.Add(LArray[i]);
end;

end.
