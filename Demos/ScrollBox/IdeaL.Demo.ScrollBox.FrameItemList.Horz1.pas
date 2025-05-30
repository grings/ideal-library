unit IdeaL.Demo.ScrollBox.FrameItemList.Horz1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  IdeaL.Lib.View.Fmx.FrameItemListModel, FMX.Controls.Presentation, FMX.Objects,
  FMX.Layouts;

type
  TFilHorz1 = class(TFrameItemListModel)
    Label1: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FilHorz1: TFilHorz1;

implementation

{$R *.fmx}

end.
