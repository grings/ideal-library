program API_Image_ImgBBdproj;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  IdeaL.Lib.Api.Image.ImgBB in '..\..\..\Api\IdeaL.Lib.Api.Image.ImgBB.pas',
  IdeaL.Lib.Api in '..\..\..\Api\IdeaL.Lib.Api.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;

  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
