unit FormMain;

// See IdeaL.Lib.PushNotification for all comments

interface

uses

  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.ScrollBox, FMX.Memo, FMX.StdCtrls, FMX.Controls.Presentation, FMX.Edit,
  FMX.Layouts, FMX.TabControl;

type
  TForm1 = class(TForm)
    tbcMain: TTabControl;
    tbiLegacy: TTabItem;
    tbiV1: TTabItem;
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  IdeaLLib.Demos.PushNotification.Server.View.V1,
  IdeaLLib.Demos.PushNotification.Server.View.Legacy;

{$R *.fmx}

procedure TForm1.FormShow(Sender: TObject);
var
  LFrm: TFrame;
begin
  tbcMain.TabIndex := 0;

  LFrm := TfrmV1.Create(Self);
  LFrm.Parent := tbiV1;
  LFrm.Align := TAlignLayout.Contents;

  LFrm := TfrmLegacy.Create(Self);
  LFrm.Parent := tbiLegacy;
  LFrm.Align := TAlignLayout.Contents;
end;

end.
