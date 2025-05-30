unit Unit1;

{

  Please, check:

  -- Make sure of changing it for all target configuration plataforms, if you
  don't know what it means, please,make sure of learning it first!!!!!!!!!!!!!!!!

  Project-Options- Building-Delphi Compiler-Conditional defines
  Project-Options- Building-Delphi Compiler-Searh path

  It needs a "new" FMX.Controls.pas, if you use a different Delphi version, please,
  compare the .PAS files from the folder "TipScrollBox\fixes" and apply it to
  your own FMX.Controls.pas native file.

  The project is compatibly with SKIA, enables it if you want

  ********** I am not responsible for any mistakes you may make.


  Tests
  Delphi 11.1
  Samsung Galaxy S21 5G, Android 12
  Xiaomi Redmi Note 7, Android 10
  Galaxy J7 Pro, Android 8.1

  Delphi 11.3
  Xiaomi Redmi Note 7, Android 10
}

interface

uses
  IdeaL.Lib.View.Fmx.FrameListModel,

  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  Fmx.Types, Fmx.Controls, Fmx.Forms, Fmx.Graphics, Fmx.Dialogs, Fmx.Layouts,
  Fmx.Objects;

type
  TForm1 = class(TForm)
    Layout1: TLayout;
    procedure Layout1Painting(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
  private
    FFrmLstVert: TFrameListModel;
    FFrmLstHorz1: TFrameListModel;
    FFrmLstHorz2: TFrameListModel;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  IdeaL.Lib.View.Utils,

  IdeaL.Demo.ScrollBox.FrameItemList.Horz1,
  IdeaL.Demo.ScrollBox.FrameItemList.Horz2,
  IdeaL.Demo.ScrollBox.FrameItemList.Vert1;

{$R *.fmx}


procedure TForm1.Layout1Painting(Sender: TObject; Canvas: TCanvas;
  const ARect: TRectF);
var
  LFil: TFrameItemListModel;
  LYPos: Single;
begin
  TUtils.TextMessageColorOpacity := 'Black';

  if Layout1.Tag <> 0 then
    Exit;
  Layout1.Tag := 1;

  FFrmLstVert := TFrameListModel.Create(Self);
  try
    FFrmLstVert.BeginUpdate;
    FFrmLstVert.Parent := Layout1;
    FFrmLstVert.Align := TAlignLayout.Client;
    FFrmLstVert.IsGradientTransparency := True;

    for var i := 0 to 5 do
    begin
      FFrmLstVert.AddItem(TFilVert1);
    end;

    FFrmLstHorz1 := TFrameListModel.Create(Self);
    FFrmLstHorz1.Align := TAlignLayout.Top;
    FFrmLstHorz1.Height := 50;
    FFrmLstHorz1.IsGradientTransparency := True;
    TFrameListModel(FFrmLstHorz1).ItemAlign := TAlignLayout.Left;
    LYPos := FFrmLstVert.ContentHeight;
    FFrmLstVert.VtsList.AddObject(FFrmLstHorz1);
    FFrmLstHorz1.Position.Y := LYPos;
    for var i := 0 to 5 do
    begin
      LFil := FFrmLstHorz1.AddItem(TFilHorz1);
    end;

    for var i := 0 to 5 do
    begin
      FFrmLstVert.AddItem(TFilVert1);
    end;

    FFrmLstHorz2 := TFrameListModel.Create(Self);
    FFrmLstHorz2.Align := TAlignLayout.Top;
    FFrmLstHorz2.Height := 50;
    FFrmLstHorz2.IsGradientTransparency := True;
    TFrameListModel(FFrmLstHorz2).ItemAlign := TAlignLayout.Left;
    LYPos := FFrmLstVert.ContentHeight;
    FFrmLstVert.VtsList.AddObject(FFrmLstHorz2);
    FFrmLstHorz2.Position.Y := LYPos;
    for var i := 0 to 10 do
    begin
      LFil := FFrmLstHorz2.AddItem(TFilHorz2);
    end;

    for var i := 0 to 5 do
    begin
      FFrmLstVert.AddItem(TFilVert1);
    end;
  finally
    FFrmLstVert.EndUpdate;
  end;
end;

end.
