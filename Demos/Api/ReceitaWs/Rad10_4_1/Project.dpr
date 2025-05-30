program Project;

uses
  System.StartUpCopy,
  FMX.Forms,
  Api.ReceitaWs.View.FormMain in '..\View\Api.ReceitaWs.View.FormMain.pas' {Form1},
  IdeaL.Lib.Api.ReceitaWs in '..\..\..\..\Api\IdeaL.Lib.Api.ReceitaWs.pas',
  IdeaL.Lib.Utils in '..\..\..\..\IdeaL.Lib.Utils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
