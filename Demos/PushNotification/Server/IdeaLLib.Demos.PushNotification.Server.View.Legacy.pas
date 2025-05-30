unit IdeaLLib.Demos.PushNotification.Server.View.Legacy;

interface

uses
  System.IniFiles,

  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Memo.Types, FMX.Edit, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,
  FMX.Layouts;

type
  TfrmLegacy = class(TFrame)
    lytBackground: TLayout;
    memoLog: TMemo;
    Label3: TLabel;
    memoMessage: TMemo;
    Label2: TLabel;
    Layout2: TLayout;
    CheckBox1: TCheckBox;
    edtNotificationBody: TEdit;
    edtNotificationId: TEdit;
    edtNotificationTitle: TEdit;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Button1: TButton;
    Layout3: TLayout;
    Label7: TLabel;
    edtApiKey: TEdit;
    edtSenderId: TEdit;
    Label8: TLabel;
    Label1: TLabel;
    memoDeviceToken: TMemo;
    Label9: TLabel;
    procedure lytBackgroundPainting(Sender: TObject; Canvas: TCanvas;
      const ARect: TRectF);
    procedure Button1Click(Sender: TObject);
    procedure edtApiKeyChangeTracking(Sender: TObject);
    procedure Label9Click(Sender: TObject);
    procedure Label7Click(Sender: TObject);
    procedure memoDeviceTokenChangeTracking(Sender: TObject);
  private
    const
    CIniSectionName = 'FirebaseCloudMessaging';

  var

    FIni: TIniFile;
    procedure WriteMemoLog(Value: string);
    { Private declarations }
  public
    destructor Destroy; override;
    { Public declarations }
  end;

implementation

uses
  System.IOUtils,
  IdeaL.Lib.Utils,
  IdeaL.Lib.Encryption,
  IdeaL.Lib.PushNotification;

{$R *.fmx}


procedure TfrmLegacy.Button1Click(Sender: TObject);
var
  i: Integer;
  LTitle: string;
  LId: string;
  LBody: string;
  ADeviceTokens: TArray<string>;
begin
  if
    (memoDeviceToken.Lines.Text.Trim.IsEmpty) or
    (CheckBox1.IsChecked) and ((edtNotificationId.Text.Trim.IsEmpty) or
    (edtNotificationTitle.Text.Trim.IsEmpty) or
    (edtNotificationBody.Text.Trim.IsEmpty))
  then
    raise Exception.Create('Notification info needs to be filled out');

  TPushNotificationSender.Instance.SenderId := edtSenderId.Text;
  TPushNotificationSender.Instance.ApiKey := edtApiKey.Text;

  LTitle := '';
  LId := '';
  LBody := '';

  if CheckBox1.IsChecked then
  begin // Sending that infomation will force the notification to appear
    LTitle := edtNotificationTitle.Text;
    LId := edtNotificationId.Text;
    LBody := edtNotificationBody.Text;
  end;

  SetLength(ADeviceTokens, memoDeviceToken.Lines.Count);

  for i := 0 to Pred(memoDeviceToken.Lines.Count) do
  begin
    ADeviceTokens[i] := memoDeviceToken.Lines[i];
  end;

  memoLog.Lines.Clear;

  TPushNotificationSender.Instance.Send(
    LTitle,
    LId,
    LBody,
    ADeviceTokens,
    ['message'],
    [memoMessage.Lines.Text], WriteMemoLog, WriteMemoLog);
end;

destructor TfrmLegacy.Destroy;
begin
  if Assigned(FIni) then
    FreeAndNil(FIni);
  inherited;
end;

procedure TfrmLegacy.edtApiKeyChangeTracking(Sender: TObject);
begin
  if (Sender.InheritsFrom(TEdit)) and (not TEdit(Sender).Hint.Trim.IsEmpty) then
    FIni.WriteString(CIniSectionName, TEdit(Sender).Hint, TEdit(Sender).Text);
end;

procedure TfrmLegacy.Label7Click(Sender: TObject);
begin
  TUtils.OpenUrl('https://ibb.co/swSHgt0');
end;

procedure TfrmLegacy.Label9Click(Sender: TObject);
begin
  TUtils.OpenUrl('https://firebase.google.com/docs/cloud-messaging/migrate-v1');
end;

procedure TfrmLegacy.lytBackgroundPainting(Sender: TObject; Canvas: TCanvas;
  const ARect: TRectF);
var
  LFS: TFileStream;
  LFileName: string;
begin
  if lytBackground.Tag <> 0 then
    Exit;
  lytBackground.Tag := 1;


  LFileName := System.IOUtils.TPath.Combine(
    System.SysUtils.GetCurrentDir,
    'config.ini');
  if not FileExists(LFileName) then
  begin
    try
      LFS := TFileStream.Create(LFileName, fmCreate);
    finally
      FreeAndNil(LFS);
    end;
  end;
  FIni := TIniFile.Create(LFileName);

  edtApiKey.Text := FIni.ReadString(CIniSectionName, 'APIKey', EmptyStr);
  edtSenderId.Text := FIni.ReadString(CIniSectionName, 'SenderID', EmptyStr);
  memoDeviceToken.Lines.Text := FIni.ReadString(CIniSectionName, 'DeviceTokens', EmptyStr);
  if not memoDeviceToken.Lines.Text.Trim.IsEmpty then
    memoDeviceToken.Lines.Text := TEncryption.DecryptStr(memoDeviceToken.Lines.Text, 'IdeaLTecD');
end;

procedure TfrmLegacy.memoDeviceTokenChangeTracking(Sender: TObject);
begin
  // Saving it just to avoid you to tapping all again
  FIni.WriteString(CIniSectionName, 'DeviceTokens', TEncryption.EncryptStr(memoDeviceToken.Lines.Text, 'IdeaLTecD'));
end;

procedure TfrmLegacy.WriteMemoLog(Value: string);
begin
  memoLog.Lines.Add(Value);
end;

end.
