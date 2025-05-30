program Project1;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit1 in '..\Unit1.pas' {Form1},
  IdeaL.Lib.Utils in '..\..\..\IdeaL.Lib.Utils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
