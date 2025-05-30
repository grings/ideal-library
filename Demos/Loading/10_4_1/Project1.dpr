program Project1;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit1 in '..\Unit1.pas' {Form1},
  IdeaL.Lib.PopUp.Loading in '..\..\..\IdeaL.Lib.PopUp.Loading.pas',
  IdeaL.Lib.PopUp in '..\..\..\IdeaL.Lib.PopUp.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;

  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
