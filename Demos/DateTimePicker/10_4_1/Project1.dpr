program Project1;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit1 in '..\Unit1.pas' {Form1},
  IdeaL.Lib.Utils in '..\..\..\IdeaL.Lib.Utils.pas',
  IdeaL.Lib.PathSvg in '..\..\..\IdeaL.Lib.PathSvg.pas',
  IdeaL.Lib.PopUp.DateTimePicker in '..\..\..\IdeaL.Lib.PopUp.DateTimePicker.pas',
  IdeaL.Lib.PopUp in '..\..\..\IdeaL.Lib.PopUp.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := DebugHook <> 0;

  Application.Initialize;
  Application.FormFactor.Orientations := [TFormOrientation.Portrait, TFormOrientation.InvertedPortrait, TFormOrientation.Landscape, TFormOrientation.InvertedLandscape];
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
