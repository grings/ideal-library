program Project1;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  IdeaL.Lib.Utils in '..\..\..\..\IdeaL.Lib.Utils.pas',
  IdeaL.Lib.Api.Bitcoin.CoinOS in '..\..\..\..\Api\Bitcoin\IdeaL.Lib.Api.Bitcoin.CoinOS.pas',
  IdeaL.Lib.Api in '..\..\..\..\Api\IdeaL.Lib.Api.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;

  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
