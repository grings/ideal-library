unit IdeaL.Lib.CircularProgressBar;

interface

uses
  System.Classes,
  System.UIConsts,
  System.SysUtils,
  System.UITypes,

  FMX.Objects,
  FMX.Graphics,
  FMX.Types,
  FMX.Ani;

type
  TCircularProgressBar = class
  private
    { private declarations }
    FValue: Single { Ever 0 to 100 (percent) };
    FArc: TArc;
    FFloatAnimation: TFloatAnimation;
    FLoading: Boolean;

    function GetThickness: Single;
    function GetParent: FMX.Types.TFmxObject;
    function GetHeight: Single;
    function GetMarginBottom: Single;
    function GetMarginLeft: Single;
    function GetMarginRight: Single;
    function GetMarginTop: Single;
    function GetWidth: Single;

    procedure SetValue(const Value: Single);
    procedure SetThickness(const Value: Single);
    procedure SetParent(const Value: FMX.Types.TFmxObject);
    procedure SetHeight(const Value: Single);
    procedure SetMarginBottom(const Value: Single);
    procedure SetMarginLeft(const Value: Single);
    procedure SetMarginRight(const Value: Single);
    procedure SetMarginTop(const Value: Single);
    procedure SetWidth(const Value: Single);
    function GetStrokeColor: TAlphaColor;
    procedure SetStrokeColor(const Value: TAlphaColor);
    function GetName: string;
    procedure SetName(const Value: string);
    function GetAlign: TAlignLayout;
    procedure SetAlign(const Value: TAlignLayout);
    function GetVisible: Boolean;
    procedure SetVisible(const Value: Boolean);
    function GetPositionX: Single;
    procedure SetPositionX(const Value: Single);
    procedure DoLoading(const Value: Boolean);
  protected
    { protected declarations }
  public
    { public declarations }
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;

    property PositionX: Single read GetPositionX write SetPositionX;
    property Visible: Boolean read GetVisible write SetVisible;
    property Name: string read GetName write SetName;
    property Value: Single read FValue write SetValue;
    property Thickness: Single read GetThickness write SetThickness;
    property Parent: FMX.Types.TFmxObject read GetParent write SetParent;
    property MarginBottom: Single read GetMarginBottom write SetMarginBottom;
    property MarginLeft: Single read GetMarginLeft write SetMarginLeft;
    property MarginRight: Single read GetMarginRight write SetMarginRight;
    property MarginTop: Single read GetMarginTop write SetMarginTop;
    property Height: Single read GetHeight write SetHeight;
    property Width: Single read GetWidth write SetWidth;
    property StrokeColor: TAlphaColor read GetStrokeColor write SetStrokeColor;
    property Align: TAlignLayout read GetAlign write SetAlign;
    property Loading: Boolean read FLoading write DoLoading;
  published
    { published declarations }
  end;

implementation

{ TCircularProgressBar }

constructor TCircularProgressBar.Create(AOwner: TComponent);
begin
  FArc := TArc.Create(AOwner);
  FArc.Fill.Color := StringToAlphaColor('Null');
  FArc.Fill.Kind := TBrushKind.None;
  FArc.Stroke.Thickness := 4;
  FArc.RotationAngle := 270;
  Value := 0;

  FFloatAnimation := TFloatAnimation.Create(FArc);
  FFloatAnimation.Enabled := False;
  FFloatAnimation.Parent := FArc;
  FFloatAnimation.PropertyName := 'RotationAngle';
  FFloatAnimation.Duration := 2;
  FFloatAnimation.Loop := True;
  FFloatAnimation.StartValue := 0;
  FFloatAnimation.StopValue := 360;
end;

destructor TCircularProgressBar.Destroy;
begin
  FreeAndNil(FFloatAnimation);
  FreeAndNil(FArc);
  inherited;
end;

procedure TCircularProgressBar.DoLoading(const Value: Boolean);
begin
  FLoading := Value;

  FFloatAnimation.Enabled := Value;
  if(Value)then
  begin
    FArc.EndAngle := 340;
  end else
    FArc.RotationAngle := 270
  ;
end;

function TCircularProgressBar.GetAlign: TAlignLayout;
begin
  Result := FArc.Align;
end;

function TCircularProgressBar.GetHeight: Single;
begin
  Result := FArc.Height;
end;

function TCircularProgressBar.GetMarginBottom: Single;
begin
  Result := FArc.Margins.Bottom;
end;

function TCircularProgressBar.GetMarginLeft: Single;
begin
  Result := FArc.Margins.Left;
end;

function TCircularProgressBar.GetMarginRight: Single;
begin
  Result := FArc.Margins.Right;
end;

function TCircularProgressBar.GetMarginTop: Single;
begin
  Result := FArc.Margins.Top;
end;

function TCircularProgressBar.GetName: string;
begin
  Result := FArc.Name;
end;

function TCircularProgressBar.GetParent: FMX.Types.TFmxObject;
begin
  Result := FArc.Parent;
end;

function TCircularProgressBar.GetPositionX: Single;
begin
  Result := FArc.Position.X;
end;

function TCircularProgressBar.GetStrokeColor: TAlphaColor;
begin
  Result := FArc.Stroke.Color;
end;

function TCircularProgressBar.GetThickness: Single;
begin
  Result := FArc.Stroke.Thickness;
end;

function TCircularProgressBar.GetVisible: Boolean;
begin
  Result := FArc.Visible;
end;

function TCircularProgressBar.GetWidth: Single;
begin
  Result := FArc.Width;
end;

procedure TCircularProgressBar.SetStrokeColor(const Value: TAlphaColor);
begin
  FArc.Stroke.Color := Value;
end;

procedure TCircularProgressBar.SetAlign(const Value: TAlignLayout);
begin
  FArc.Align := Value;
end;

procedure TCircularProgressBar.SetHeight(const Value: Single);
begin
  FArc.Height := Value;
end;

procedure TCircularProgressBar.SetMarginBottom(const Value: Single);
begin
  FArc.Margins.Bottom := Value;
end;

procedure TCircularProgressBar.SetMarginLeft(const Value: Single);
begin
  FArc.Margins.Left := Value;
end;

procedure TCircularProgressBar.SetMarginRight(const Value: Single);
begin
  FArc.Margins.Right := Value;
end;

procedure TCircularProgressBar.SetMarginTop(const Value: Single);
begin
  FArc.Margins.Top := Value;
end;

procedure TCircularProgressBar.SetName(const Value: string);
begin
  FArc.Name := Value;
end;

procedure TCircularProgressBar.SetParent(const Value: FMX.Types.TFmxObject);
begin
  FArc.Parent := Value;
end;

procedure TCircularProgressBar.SetPositionX(const Value: Single);
begin
  FArc.Position.X := Value;
end;

procedure TCircularProgressBar.SetThickness(const Value: Single);
begin
  FArc.Stroke.Thickness := Value;
end;

procedure TCircularProgressBar.SetValue(const Value: Single);
begin
  if (Value < 0) or (Value > 100) then
    raise Exception.Create('TCircularProgressBar: Invaled value. Must be 0 to 100');

  FValue := Value;
  FArc.EndAngle := Trunc((Value * 360) / 100);
end;

procedure TCircularProgressBar.SetVisible(const Value: Boolean);
begin
  FArc.Visible := Value;
end;

procedure TCircularProgressBar.SetWidth(const Value: Single);
begin
  FArc.Width := Value;
end;

end.
