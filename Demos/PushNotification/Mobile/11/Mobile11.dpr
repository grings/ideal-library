program Mobile11;

uses
  System.StartUpCopy,
  FMX.Forms,
  FormMain in '../FormMain.pas' {Form2},
  IdeaL.Lib.PushNotification in '..\..\..\..\IdeaL.Lib.PushNotification.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
