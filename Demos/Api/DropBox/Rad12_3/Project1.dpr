program Project1;

uses
  System.StartUpCopy,
  FMX.Forms,
  Api.DropBox.View.FormMain in '..\View\Api.DropBox.View.FormMain.pas' {FormMain},
  IdeaL.Lib.Api.DropBox in '..\..\..\..\Api\IdeaL.Lib.Api.DropBox.pas',
  IdeaL.Lib.Utils in '..\..\..\..\IdeaL.Lib.Utils.pas';
{$R *.res}
begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
