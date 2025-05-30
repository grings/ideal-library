program Project1;

uses
  Unit1 in '..\Unit1.pas' {Form1},
  System.StartUpCopy,
  System.SysUtils,
  System.Classes,
  FMX.Forms,
  Skia.FMX,
  FMX.Types,
  IdeaL.Lib.View.Fmx.FrameItemListModel in '..\..\..\View\Fmx\List\IdeaL.Lib.View.Fmx.FrameItemListModel.pas' {FrameItemListModel: TFrame},
  IdeaL.Demo.ScrollBox.FrameItemList.Vert1 in '..\IdeaL.Demo.ScrollBox.FrameItemList.Vert1.pas' {FilVert1: TFrame},
  IdeaL.Demo.ScrollBox.FrameItemList.Horz1 in '..\IdeaL.Demo.ScrollBox.FrameItemList.Horz1.pas' {FilHorz1: TFrame},
  IdeaL.Demo.ScrollBox.FrameItemList.Horz2 in '..\IdeaL.Demo.ScrollBox.FrameItemList.Horz2.pas' {FilHorz2: TFrame};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;

  GlobalUseSkia := True;

  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
