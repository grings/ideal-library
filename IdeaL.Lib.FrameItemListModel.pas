unit IdeaL.Lib.FrameItemListModel;

interface

uses
  System.SysUtils,
  System.Types,
  System.UIConsts,
  System.UITypes,
  System.Classes,
  System.Variants,

  FMX.Types,
  FMX.Graphics,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.StdCtrls,
  FMX.Objects,
  FMX.Layouts;

type
  TFrameItemListModel = class(TFrame)
    lytBackground: TLayout;
    rctBackground: TRectangle;
    rctClient: TRectangle;
    lneBottom: TLine;
    procedure lytBackgroundPainting(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
    procedure OnClickDefault(Sender: TObject);
  private
    FIdentify: string;
    { Private declarations }
  protected
    procedure SetIdentify(const Value: string); virtual;
    { Protected declarations }
  public
    constructor Create(AOwner: TComponent); override;
    procedure ChangeBackgroundColor(const AColor: string);
    procedure ChangeClientColor(const AColor: string);

    property Identify: string read FIdentify write SetIdentify;
    procedure SetDefaultClick(AEvent: TNotifyEvent);
    procedure SetDefaultTap(AEvent: TTapEvent);
    { Public declarations }
  end;

implementation

{$R *.fmx}
{ TFrameItemListModel }

procedure TFrameItemListModel.ChangeClientColor(const AColor: string);
begin
  rctClient.Fill.Color := StringToAlphaColor(AColor);
end;

constructor TFrameItemListModel.Create(AOwner: TComponent);
begin
  inherited;
  FIdentify := EmptyStr;
  rctBackground.Visible := False;
end;

procedure TFrameItemListModel.lytBackgroundPainting(Sender: TObject;
  Canvas: TCanvas; const ARect: TRectF);
begin
  {if lytBackground.Tag <> 0 then
    Exit;
  lytBackground.Tag := 1;}
  rctBackground.Visible := True;
end;

procedure TFrameItemListModel.OnClickDefault(Sender: TObject);
begin
{$IFDEF MSWINDOWS}
  if Sender.InheritsFrom(TControl) and (Assigned(TControl(Sender).OnTap)) then
    TControl(Sender).OnTap(Sender, TPointF.Create(0, 0));
{$ENDIF}
end;

procedure TFrameItemListModel.ChangeBackgroundColor(const AColor: string);
begin
  rctBackground.Fill.Color := StringToAlphaColor(AColor);
end;

procedure TFrameItemListModel.SetDefaultClick(AEvent: TNotifyEvent);
begin
  rctBackground.OnClick := AEvent;
end;

procedure TFrameItemListModel.SetDefaultTap(AEvent: TTapEvent);
begin
  rctBackground.OnTap := AEvent;
end;

procedure TFrameItemListModel.SetIdentify(const Value: string);
begin
  FIdentify := Value;
end;

end.
