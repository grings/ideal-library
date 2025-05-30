program Project1;

uses
  System.StartUpCopy,
  FMX.Forms,
  Onboarding.View in '..\Onboarding.View.pas' {OnboardingView: TFrame},
  Unit1 in '..\Unit1.pas' {Form1},
  IdeaL.Lib.View.TabControl in '..\..\..\View\IdeaL.Lib.View.TabControl.pas' {IdeaLTabControl: TFrame};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;

  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
