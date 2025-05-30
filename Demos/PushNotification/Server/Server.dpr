program Server;

uses
  System.StartUpCopy,
  FMX.Forms,
  FormMain in 'FormMain.pas' {Form1},
  IdeaL.Lib.PushNotification in '..\..\..\IdeaL.Lib.PushNotification.pas',
  IdeaL.Lib.Utils in '..\..\..\IdeaL.Lib.Utils.pas',
  IdeaLLib.Demos.PushNotification.Server.View.Legacy in 'IdeaLLib.Demos.PushNotification.Server.View.Legacy.pas' {frmLegacy: TFrame},
  IdeaLLib.Demos.PushNotification.Server.View.V1 in 'IdeaLLib.Demos.PushNotification.Server.View.V1.pas' {frmV1: TFrame},
  DW.FCMSender in '..\..\..\Others\Kastri\Features\Firebase\DW.FCMSender.pas',
  OpenSSL.Api_11 in '..\..\..\Others\Kastri\Features\Firebase\OpenSSL.Api_11.pas',
  IdeaL.Lib.Encryption in '..\..\..\IdeaL.Lib.Encryption.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;

  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
