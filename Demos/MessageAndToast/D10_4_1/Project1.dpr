program Project1;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit1 in '..\Unit1.pas' {Form1},
  IdeaL.Lib.CommonTypes in '..\..\..\IdeaL.Lib.CommonTypes.pas',
  IdeaL.Lib.PopUp in '..\..\..\IdeaL.Lib.PopUp.pas',
  IdeaL.Lib.PopUp.Message in '..\..\..\IdeaL.Lib.PopUp.Message.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;

  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
