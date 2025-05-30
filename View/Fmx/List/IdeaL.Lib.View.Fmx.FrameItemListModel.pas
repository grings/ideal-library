unit IdeaL.Lib.View.Fmx.FrameItemListModel;

interface

uses
  System.SysUtils,
  System.Types,
  System.UIConsts,
  System.UITypes,
  System.Classes,
  System.Variants,

  Fmx.Types,
  Fmx.Graphics,
  Fmx.Controls,
  Fmx.Forms,
  Fmx.Dialogs,
  Fmx.StdCtrls,
  Fmx.Objects,
  Fmx.Layouts,
  Fmx.Controls.Presentation;

type
  TFrameItemListModel = class(TFrame)
    lytBackground: TLayout;
    rctBackground: TRectangle;
    rctClient: TRectangle;
    lneBottom: TLine;
    btnSetFocus: TButton;
    procedure lytBackgroundPainting(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
    procedure lytBackgroundClick(Sender: TObject);
    procedure btnSetFocusPainting(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
    procedure OnClickDefault(Sender: TObject);
  private
    FIdentify: string;
    FFrmBase: TFrame;

    procedure SetIdentify(const Value: string);
    { Private declarations }
  protected
    FExtraHeight: Single;
    FOriginalHeight: Single;

    procedure SetFrmBase(const Value: TFrame); virtual;
    { Protected declarations }
  public
    constructor Create(AOwner: TComponent); override;

    property Identify: string read FIdentify write SetIdentify;

    procedure ChangeBackgroundColor(const AColor: string);
    procedure ChangeClientColor(const AColor: string);
    procedure SetDefaultClick(AEvent: TNotifyEvent);
    procedure SetDefaultTap(AEvent: TTapEvent);

    procedure ShowValues; virtual;

    // To store what is the Frame where it is being shwon
    property FrmBase: TFrame read FFrmBase write SetFrmBase;
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
  FOriginalHeight := Self.Height;
  FExtraHeight := 0;
  FFrmBase := nil;

  btnSetFocus.Position.Y := -100;
  btnSetFocus.Position.X := -100;
end;

procedure TFrameItemListModel.lytBackgroundClick(Sender: TObject);
begin
{$IFDEF MSWINDOWS}
  if (Sender.InheritsFrom(TControl)) and (Assigned(TControl(Sender).OnTap)) then
    TControl(Sender).OnTap(Sender, TPointF.Create(0, 0));
{$ENDIF}
end;

procedure TFrameItemListModel.lytBackgroundPainting(Sender: TObject;
  Canvas: TCanvas; const ARect: TRectF);
begin
  rctBackground.Visible := True;
end;

procedure TFrameItemListModel.OnClickDefault(Sender: TObject);
begin
{$IFDEF MSWINDOWS}
  if Sender.InheritsFrom(TControl) and (Assigned(TControl(Sender).OnTap)) then
    TControl(Sender).OnTap(Sender, TPointF.Create(0, 0));
{$ENDIF}
end;

procedure TFrameItemListModel.btnSetFocusPainting(Sender: TObject;
  Canvas: TCanvas; const ARect: TRectF);
begin
  btnSetFocus.Position.Y := btnSetFocus.Position.Y - 1;
  btnSetFocus.Position.X := btnSetFocus.Position.X - 1;
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

procedure TFrameItemListModel.SetFrmBase(const Value: TFrame);
begin
  FFrmBase := Value;
end;

procedure TFrameItemListModel.SetIdentify(const Value: string);
begin
  FIdentify := Value;
end;

procedure TFrameItemListModel.ShowValues;
begin
  rctBackground.Visible := True;
end;

end.
