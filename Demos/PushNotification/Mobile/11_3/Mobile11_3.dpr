program Mobile11_3;

uses
  System.StartUpCopy,
  FMX.Forms,
  FormMain in '..\FormMain.pas' {Form2},
  IdeaL.Lib.Android.Permissions in '..\..\..\..\IdeaL.Lib.Android.Permissions.pas',
  IdeaL.Lib.PushNotification in '..\..\..\..\IdeaL.Lib.PushNotification.pas',
  IdeaL.Lib.Utils in '..\..\..\..\IdeaL.Lib.Utils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
