program Project1;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  IdeaL.Lib.Utils in '..\..\..\..\IdeaL.Lib.Utils.pas',
  IdeaL.Lib.Api in '..\..\..\..\Api\IdeaL.Lib.Api.pas',
  IdeaL.Lib.Api.Bitcoin.Alby in '..\..\..\..\Api\Bitcoin\IdeaL.Lib.Api.Bitcoin.Alby.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
