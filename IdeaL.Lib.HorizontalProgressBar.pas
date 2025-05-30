unit IdeaL.Lib.HorizontalProgressBar;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Variants,
  System.UITypes,
  System.UIConsts,

  FMX.Layouts,
  FMX.Types,
  FMX.StdCtrls,
  FMX.Objects,
  FMX.Graphics;

type
  THorizontalProgressBar = class(TComponent)
  private
    FLytBase: TLayout;
    FLblStatus: TLabel;
    FValue: Single;
    FMaxValue: Single;
    FFontFamily: string;
    FFontSize: Single;
    FFontColor: TAlphaColor;
    FLytProgressBar: TLayout;
    FRctBackground: TRectangle;
    FBackgroundColor: TAlphaColor;
    FRctProgress: TRectangle;
    FProgressColor: TAlphaColor;
    procedure SetMaxValue(const Value: Single);
    procedure SetValue(const Value: Single);
    procedure SetFontColor(const Value: TAlphaColor); overload;
    procedure SetFontFamily(const Value: string);
    procedure SetFontSize(const Value: Single);
    procedure SetBackgroundColor(const Value: TAlphaColor);  overload;
    procedure SetProgressColor(const Value: TAlphaColor); overload;

    procedure Risized(Sender: TObject);
    { private declarations }
  protected
    { protected declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property LytBase: TLayout read FLytBase;
    property LblStatus: TLabel read FLblStatus;
    property LytProgressBar: TLayout read FLytProgressBar;
    property RctBackground: TRectangle read FRctBackground;
    property RctProgress: TRectangle read FRctProgress;

    property MaxValue: Single read FMaxValue write SetMaxValue;
    property Value: Single read FValue write SetValue;
    property FontFamily: string read FFontFamily write SetFontFamily;
    property FontSize: Single read FFontSize write SetFontSize;
    property FontColor: TAlphaColor read FFontColor write SetFontColor;
    property BackgroundColor: TAlphaColor read FBackgroundColor write SetBackgroundColor;
    property ProgressColor: TAlphaColor read FProgressColor write SetProgressColor;

    procedure SetFontColor(const Value: string); overload;
    procedure SetBackgroundColor(const Value: string); overload;
    procedure SetProgressColor(const Value: string); overload;
    { public declarations }
  published
    { published declarations }
  end;

implementation

{ THorizontalProgressBar }

constructor THorizontalProgressBar.Create(AOwner: TComponent);
begin
  inherited;
  MaxValue := 100;
  FValue := 0;

  FLytBase := TLayout.Create(AOwner);
  FLytBase.OnResized := Risized;

  FLblStatus := TLabel.Create(FLytBase);
  FLblStatus.Parent := TFmxObject(FLblStatus.Owner);
  FLblStatus.Align := TAlignLayout.Top;
  FLblStatus.AutoSize := True;
  FLblStatus.TextSettings.WordWrap := False;
  FLblStatus.TextSettings.HorzAlign := TTextAlign.Center;
  FLblStatus.TextSettings.Font.Size := 12;
  FLblStatus.StyledSettings := [];

  FLytProgressBar := TLayout.Create(FLytBase);
  FLytProgressBar.Parent := TFmxObject(FLytProgressBar.Owner);
  FLytProgressBar.Align := TAlignLayout.Client;

  FRctBackground := TRectangle.Create(FLytProgressBar);
  FRctBackground.Parent := TFmxObject(FRctBackground.Owner);
  FRctBackground.Align := TAlignLayout.Client;
  FRctBackground.Stroke.Color := StringToAlphaColor('Null');
  FRctBackground.Stroke.Kind := TBrushKind.None;
  FRctBackground.Fill.Color := StringToAlphaColor('#FFE0E0E0');

  FRctProgress := TRectangle.Create(FLytProgressBar);
  FRctProgress.Parent := TFmxObject(FRctProgress.Owner);
  FRctProgress.Align := TAlignLayout.Left;
  FRctProgress.Stroke.Color := StringToAlphaColor('Null');
  FRctProgress.Stroke.Kind := TBrushKind.None;
  FRctProgress.Fill.Color := StringToAlphaColor('#FF0075C9');
  FRctProgress.Width := 0;

  Value := 0;
end;

destructor THorizontalProgressBar.Destroy;
begin

  inherited;
end;

procedure THorizontalProgressBar.Risized(Sender: TObject);
begin
  Value := FValue;
end;

procedure THorizontalProgressBar.SetFontColor(const Value: TAlphaColor);
begin
  FFontColor := Value;
  FLblStatus.TextSettings.FontColor := Value;
end;

procedure THorizontalProgressBar.SetBackgroundColor(const Value: TAlphaColor);
begin
  FBackgroundColor := Value;
  FRctBackground.Fill.Color := Value;
end;

procedure THorizontalProgressBar.SetBackgroundColor(const Value: string);
begin
  BackgroundColor := StringToAlphaColor(Value);
end;

procedure THorizontalProgressBar.SetFontColor(const Value: string);
begin
  FontColor := StringToAlphaColor(Value);
end;

procedure THorizontalProgressBar.SetFontFamily(const Value: string);
begin
  FFontFamily := Value;
  FLblStatus.TextSettings.Font.Family := Value;
end;

procedure THorizontalProgressBar.SetFontSize(const Value: Single);
begin
  FFontSize := Value;
end;

procedure THorizontalProgressBar.SetMaxValue(const Value: Single);
begin
  FMaxValue := Value;
end;

procedure THorizontalProgressBar.SetProgressColor(const Value: string);
begin
  ProgressColor := StringToAlphaColor(Value);
end;

procedure THorizontalProgressBar.SetProgressColor(const Value: TAlphaColor);
begin
  FProgressColor := Value;
  FRctProgress.Fill.Color := Value;
end;

procedure THorizontalProgressBar.SetValue(const Value: Single);
var
  LPerc: Single;
begin
  FValue := Value;

  LPerc := 0;
  if(FMaxValue > 0)then
  begin
    LPerc := Trunc((Value / FMaxValue) * 100);
  end;

  FLblStatus.Text := LPerc.ToString + ' %';
  FLblStatus.BringToFront;
  FRctProgress.Width := Trunc((FLytProgressBar.Width * LPerc) / 100);
end;

end.
