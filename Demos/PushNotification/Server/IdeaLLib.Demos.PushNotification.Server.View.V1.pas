unit IdeaLLib.Demos.PushNotification.Server.View.V1;

interface

uses
  System.IniFiles,

  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Edit, FMX.Controls.Presentation, FMX.Layouts, FMX.Memo.Types,
  FMX.ScrollBox, FMX.Memo;

type
  TfrmV1 = class(TFrame)
    Layout3: TLayout;
    Label7: TLabel;
    Label3: TLabel;
    memoLog: TMemo;
    lytBackground: TLayout;
    edtPrivateKeyJsonPath: TEdit;
    memoDeviceToken: TMemo;
    Label2: TLabel;
    Label9: TLabel;
    Layout2: TLayout;
    CheckBox1: TCheckBox;
    edtNotificationBody: TEdit;
    edtChannelId: TEdit;
    edtNotificationTitle: TEdit;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Button1: TButton;
    memoData: TMemo;
    Label1: TLabel;
    Label8: TLabel;
    procedure lytBackgroundPainting(Sender: TObject; Canvas: TCanvas;
      const ARect: TRectF);
    procedure Label7Click(Sender: TObject);
    procedure edtPrivateKeyJsonPathChangeTracking(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure memoDeviceTokenChangeTracking(Sender: TObject);
    procedure Label9Click(Sender: TObject);
    procedure memoDataChangeTracking(Sender: TObject);
  private
    const
    CIniSectionName = 'FirebaseCloudMessagingV1';

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

{ TFrame1 }

procedure TfrmV1.Button1Click(Sender: TObject);
var
  i: Integer;
  LTitle: string;
  LChannelId: string;
  LBody: string;
  ADeviceTokens: TArray<string>;
begin
  if
    (memoDeviceToken.Lines.Text.Trim.IsEmpty) or
    (CheckBox1.IsChecked) and (
    (edtNotificationTitle.Text.Trim.IsEmpty) or
    (edtNotificationBody.Text.Trim.IsEmpty)
    )
  then // while ShowNotification=True, must have Title and Body. If Tokens empty, will send to all registered devices
    raise Exception.Create('Notification info needs to be filled out');

  if
    (not CheckBox1.IsChecked) and (
    (memoDeviceToken.Lines.Text.Trim.IsEmpty) or
    (memoData.Lines.Text.Trim.IsEmpty)
    )
  then // while ShowNotification=False, must have Tokens and Data
    raise Exception.Create('Notification info needs to be filled out');

  LTitle := '';
  LChannelId := '';
  LBody := '';

  if CheckBox1.IsChecked then
  begin // Sending that inforation will force the notification to appear
    LTitle := edtNotificationTitle.Text;
    LChannelId := edtChannelId.Text;
    LBody := edtNotificationBody.Text;
  end;

  SetLength(ADeviceTokens, memoDeviceToken.Lines.Count);

  for i := 0 to Pred(memoDeviceToken.Lines.Count) do
  begin
    ADeviceTokens[i] := memoDeviceToken.Lines[i];
  end;

  memoLog.Lines.Clear;

  TPushNotificationSenderV1.Instance
    .JsonPrivateKeyPath(edtPrivateKeyJsonPath.Text)
    // .JsonStr('Your JSON string here')
    .Send(
    LTitle,
    LChannelId,
    LBody,
    memoData.Lines.Text,
    ADeviceTokens,
    not CheckBox1.IsChecked,
    WriteMemoLog
    );
end;

destructor TfrmV1.Destroy;
begin
  if Assigned(FIni) then
    FreeAndNil(FIni);
  inherited;
end;

procedure TfrmV1.edtPrivateKeyJsonPathChangeTracking(Sender: TObject);
begin
  if (Sender.InheritsFrom(TEdit)) and (not TEdit(Sender).Hint.Trim.IsEmpty) then
    FIni.WriteString(CIniSectionName, TEdit(Sender).Hint, TEdit(Sender).Text);
end;

procedure TfrmV1.Label7Click(Sender: TObject);
begin
  TUtils.OpenUrl('https://ibb.co/qdSnWPF');
end;

procedure TfrmV1.Label9Click(Sender: TObject);
begin
  TUtils.OpenUrl('https://github.com/DelphiWorlds/Kastri/blob/master/Features/Firebase/FCMSender.md');
end;

procedure TfrmV1.lytBackgroundPainting(Sender: TObject; Canvas: TCanvas;
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

  edtPrivateKeyJsonPath.Text := FIni.ReadString(CIniSectionName, 'JsonPrivateKeyPath', EmptyStr);
  memoDeviceToken.Lines.Text := FIni.ReadString(CIniSectionName, 'DeviceTokens', EmptyStr);
  if not memoDeviceToken.Lines.Text.Trim.IsEmpty then
    memoDeviceToken.Lines.Text := TEncryption.DecryptStr(memoDeviceToken.Lines.Text, 'IdeaLTecD');
  memoData.Lines.Text := FIni.ReadString(CIniSectionName, 'Data', EmptyStr);
  if not memoData.Lines.Text.Trim.IsEmpty then
    memoData.Lines.Text := TEncryption.DecryptStr(memoData.Lines.Text, 'IdeaLTecD');
end;

procedure TfrmV1.memoDataChangeTracking(Sender: TObject);
begin
  FIni.WriteString(CIniSectionName, 'Data', TEncryption.EncryptStr(memoData.Lines.Text, 'IdeaLTecD'));
end;

procedure TfrmV1.memoDeviceTokenChangeTracking(Sender: TObject);
begin
  // Saving it just to avoid you to tapping all again
  FIni.WriteString(CIniSectionName, 'DeviceTokens', TEncryption.EncryptStr(memoDeviceToken.Lines.Text, 'IdeaLTecD'));
end;

procedure TfrmV1.WriteMemoLog(Value: string);
begin
  memoLog.Lines.Add(Value);
end;

end.
