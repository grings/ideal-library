program Project;

uses
  System.StartUpCopy,
  FMX.Forms,
  IdeaL.Lib.CustomMediaPlayer in '..\..\..\IdeaL.Lib.CustomMediaPlayer.pas',
  CustomMediaPlayer.View.FormMain in '..\View\CustomMediaPlayer.View.FormMain.pas' {FormMain};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;

  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
