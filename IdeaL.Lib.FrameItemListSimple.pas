unit IdeaL.Lib.FrameItemListSimple;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  IdeaL.Lib.FrameItemListModel, FMX.Objects, FMX.Layouts,
  FMX.Controls.Presentation;

type
  TFrameItemListSimple = class(TFrameItemListModel)
    lblDetail: TLabel;
    procedure lytBackgroundPainting(Sender: TObject; Canvas: TCanvas;
      const ARect: TRectF);
  private
    FDetail: string;
    procedure SetDetail(const Value: string);
    { Private declarations }
  public
    property Detail: string read FDetail write SetDetail;
    procedure ShowValues;
    { Public declarations }
  end;

var
  FrameItemListSimple: TFrameItemListSimple;

implementation

{$R *.fmx}

{ TFrameItemListSimple }

procedure TFrameItemListSimple.lytBackgroundPainting(Sender: TObject;
  Canvas: TCanvas; const ARect: TRectF);
begin
  if lytBackground.Tag <> 0 then
    Exit;
  lytBackground.Tag := 1;
  inherited;
  ShowValues;
end;

procedure TFrameItemListSimple.SetDetail(const Value: string);
begin
  FDetail := Value;
end;

procedure TFrameItemListSimple.ShowValues;
begin
  lblDetail.Text := FDetail;
end;

end.
