unit iPub.FMX.HorzScrollBox.Base;

interface

{$SCOPEDENUMS ON}
{$IFDEF ALCINOE}

uses
  { Delphi }
  System.Classes,
  System.Types,
  FMX.Layouts,
  FMX.Types,

  { Third-party }
  ALFmxLayouts;

type
  TScrollBarsVisibility = ALFmxLayouts.TScrollBarsVisibility;

  { TipHorzScrollBoxBase }

  TipHorzScrollBoxBase = class(TALCustomScrollBox)
  private
    FLastContentBounds: TRectF;
    FOnCalcContentBounds: TOnCalcContentBoundsEvent;
    FScreenScale: Single;
    function GetIsTracking: Boolean;
  protected
    procedure AniMouseDown(const ATouch: Boolean; const AX, AY: Single); virtual;
    procedure AniMouseMove(const ATouch: Boolean; const AX, AY: Single); virtual;
    procedure AniMouseUp(const ATouch: Boolean; const AX, AY: Single); virtual;
    function CalcContentBounds: TRectF; override;
    function CreateAniCalculations: TALScrollBoxAniCalculations; override;
    procedure CustomContentChanged(const ACustomContentSize: TSizeF);
    procedure DefineProperties(AFiler: TFiler); override;
    function DoCalcContentBounds: TRectF; virtual;
    function GetBoundsAnimation: Boolean; virtual;
    function GetViewportPosition: TPointF;
    procedure Paint; override;
    procedure SetBoundsAnimation(AValue: Boolean); virtual;
    procedure SetViewportPosition(const AValue: TPointF);
  public
    constructor Create(AOwner: TComponent); override;
    property IsTracking: Boolean read GetIsTracking;
  published
    property MaxContentWidth;
    property HScrollBar;
    property AniCalculations;
    property Align;
    property Anchors;
    property AutoHide;
    property ClipParent;
    property Cursor;
    property DisableMouseWheel;
    property DragMode;
    property Enabled;
    property EnableDragHighlight;
    property Height;
    property HitTest;
    property Locked;
    property Margins;
    property Opacity;
    property Padding;
    property PopupMenu;
    property Position;
    property RotationAngle;
    property RotationCenter;
    property Scale;
    property ScrollBarsVisibility;
    property Size;
    property TabOrder;
    property TabStop;
    property TouchTargetExpansion;
    property Visible;
    property ViewportPosition: TPointF read GetViewportPosition write SetViewportPosition;
    property Width;
    { Events }
    property OnPainting;
    property OnPaint;
    property OnResize;
    {$IF CompilerVersion >= 32} // tokyo
    property OnResized;
    {$ENDIF}
    { Drag and Drop events }
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    { Mouse events }
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;
    { ScrollBox events }
    property OnCalcContentBounds: TOnCalcContentBoundsEvent read FOnCalcContentBounds write FOnCalcContentBounds;
    property OnViewportPositionChange;
    property OnScrollBarInit;
    property OnAniStart;
    property OnAniStop;
  end;

implementation

uses
  { Delphi }
  System.Math,
  System.Math.Vectors,
  System.UITypes,
  System.SysUtils,
  FMX.Consts,
  FMX.Platform,
  FMX.Controls;

type
  TipScrollBoxAniCalculations = class(TALScrollBoxAniCalculations)
  protected
    procedure MouseDown(X, Y: Double); override;
    procedure MouseMove(X, Y: Double); override;
    procedure MouseUp(X, Y: Double); override;
  end;


{ TipHorzScrollBoxBase }

procedure TipHorzScrollBoxBase.AniMouseDown(const ATouch: Boolean; const AX,
  AY: Single);
begin
end;

procedure TipHorzScrollBoxBase.AniMouseMove(const ATouch: Boolean; const AX,
  AY: Single);
begin
end;

procedure TipHorzScrollBoxBase.AniMouseUp(const ATouch: Boolean; const AX,
  AY: Single);
begin
end;

function TipHorzScrollBoxBase.CalcContentBounds: TRectF;
begin
  Result := DoCalcContentBounds;
  if Assigned(FOnCalcContentBounds) then
    FOnCalcContentBounds(Self, Result);
  if FLastContentBounds <> Result then
  begin
    FLastContentBounds := Result;
    if Assigned(OnViewportPositionChange) then
      OnViewportPositionChange(Self, ViewportPosition, ViewportPosition, True);
  end;
end;

constructor TipHorzScrollBoxBase.Create(AOwner: TComponent);
var
  LScreenSrv: IFMXScreenService;
begin
  FScreenScale := 1;
  inherited;
  if TPlatformServices.Current.SupportsPlatformService(IFMXScreenService, LScreenSrv) then
    FScreenScale := LScreenSrv.GetScreenScale;
  HScrollBar := CreateScrollBar(TOrientation.Horizontal);
end;

function TipHorzScrollBoxBase.CreateAniCalculations: TALScrollBoxAniCalculations;
begin
  Result := TipScrollBoxAniCalculations.Create(Self);
  Result.BeginUpdate;
  try
    Result.DecelerationRate := 2.3;
    Result.VelocityFactor := 1.2;
    Result.Elasticity := 100;
    Result.ElasticityDecelerationFactor := 1.3;
    Result.Animation := True;
    Result.TouchTracking := [System.UITypes.ttHorizontal];
    Result.BoundsAnimation := True;
//    Result.Animation := HasTouchScreen;
//    if HasTouchScreen then
//      Result.TouchTracking := [System.UITypes.ttHorizontal]
//    else
//      Result.TouchTracking := [];
//    Result.BoundsAnimation := HasTouchScreen;
    Result.AutoShowing := HasTouchScreen;
  finally
    Result.EndUpdate;
  end;
end;

procedure TipHorzScrollBoxBase.CustomContentChanged(const ACustomContentSize: TSizeF);
begin
end;

procedure TipHorzScrollBoxBase.DefineProperties(AFiler: TFiler);
begin
  inherited;
  AFiler.DefineProperty('UseSmallScrollBars', IgnoreBooleanValue, nil, False);
  AFiler.DefineProperty('MouseTracking', IgnoreBooleanValue, nil, False);
  AFiler.DefineProperty('Viewport.Width', IgnoreFloatValue, nil, False);
  AFiler.DefineProperty('Viewport.Height', IgnoreFloatValue, nil, False);
end;

function TipHorzScrollBoxBase.DoCalcContentBounds: TRectF;
begin
  Result := inherited CalcContentBounds;
  if not SameValue(MaxContentHeight, 0, TEpsilon.Position) then
  begin
    Result.Height := Min(MaxContentHeight, Height);
    AnchoredContentOffset := PointF(AnchoredContentOffset.X, Round(((Height - Result.Height) / 2) * FScreenScale) / FScreenScale);
  end
  else
    Result.Height := Height;
end;

function TipHorzScrollBoxBase.GetBoundsAnimation: Boolean;
begin
  Result := AniCalculations.BoundsAnimation;
end;

function TipHorzScrollBoxBase.GetIsTracking: Boolean;
begin
  Result := (Assigned(VScrollBar) and VScrollBar.IsTracking) or
    (Assigned(HScrollBar) and HScrollBar.IsTracking);
end;

function TipHorzScrollBoxBase.GetViewportPosition: TPointF;
begin
  Result := PointF(AniCalculations.ViewportPosition.X, 0);//PointF(HScrollBar.Value, 0);
end;

procedure TipHorzScrollBoxBase.Paint;
begin
  inherited;
  if (csDesigning in ComponentState) and not Locked then
    DrawDesignBorder;
end;

procedure TipHorzScrollBoxBase.SetBoundsAnimation(AValue: Boolean);
begin
  AniCalculations.BoundsAnimation := AValue;
end;

procedure TipHorzScrollBoxBase.SetViewportPosition(const AValue: TPointF);
begin
  AniCalculations.ViewportPositionF := AValue;
  //HScrollBar.Value := AValue.X;
end;


{ TipScrollBoxAniCalculations }

procedure TipScrollBoxAniCalculations.MouseDown(X, Y: Double);
begin
  TipHorzScrollBoxBase(ScrollBox).AniMouseDown(True, X, Y);
  inherited;
end;

procedure TipScrollBoxAniCalculations.MouseMove(X, Y: Double);
begin
  inherited;
  TipHorzScrollBoxBase(ScrollBox).AniMouseMove(True, X, Y);
end;

procedure TipScrollBoxAniCalculations.MouseUp(X, Y: Double);
begin
  inherited;
  TipHorzScrollBoxBase(ScrollBox).AniMouseUp(True, X, Y);
end;

initialization
  RegisterFMXClasses([TipHorzScrollBoxBase]);

{$ELSE}
uses
  { Delphi }
  System.Types,
  System.UITypes,
  System.Classes,
  FMX.Types,
  FMX.Controls,
  FMX.Layouts;

type
  TScrollBarsVisibility = (Always, OnMouseInteractions, None);

  { TipHorzScrollBoxBase }

  TipHorzScrollBoxBase = class(THorzScrollBox)
  private
    FBoundsAnimation: Boolean;
    FMouseEvents: Boolean;
    FScrollBarsVisibility: TScrollBarsVisibility;
    function GetIsTracking: Boolean;
    procedure SetScrollBarsVisibility(AValue: TScrollBarsVisibility);
  {$IFDEF DELPHI_FIXES}
  private
    procedure MousePosToAni(const AObject: TControl; var X, Y: Single);
  protected
    procedure ChildrenMouseDown(const AObject: TControl; AButton: TMouseButton;
      AShift: TShiftState; X, Y: Single); override;
    procedure ChildrenMouseMove(const AObject: TControl; AShift: TShiftState;
      X, Y: Single); override;
    procedure ChildrenMouseUp(const AObject: TControl; AButton: TMouseButton;
      AShift: TShiftState; X, Y: Single); override;
    procedure ChildrenMouseLeave(const AObject: TControl); override;
  {$ENDIF}
  protected
    procedure CMGesture(var AEventInfo: TGestureEventInfo); override;
    procedure CustomContentChanged(const ACustomContentSize: TSizeF);
    procedure DoUpdateAniCalculations(const AAniCalculations: TScrollCalculations); override;
    function GetBoundsAnimation: Boolean; virtual;
    procedure MouseDown(AButton: TMouseButton; AShift: TShiftState; AX, AY: Single); override;
    procedure SetBoundsAnimation(AValue: Boolean); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    property IsTracking: Boolean read GetIsTracking;
  published
    property ShowScrollBars stored False;
    property ScrollBarsVisibility: TScrollBarsVisibility read FScrollBarsVisibility write SetScrollBarsVisibility default TScrollBarsVisibility.Always;
  end;

implementation

{ TipHorzScrollBoxBase }

{$IFDEF DELPHI_FIXES}
uses
  { Delphi }
  System.SysUtils,
  FMX.Text;

type
  TipControlAccess = class(TControl);

function CheckControlInParentTree(AControl, AParent: TControl): Boolean;
begin
  Result := False;
  while Assigned(AControl) do
  begin
    if AControl.Parent = AParent then
      Exit(True);
    if AControl.Parent is TControl then
      AControl := TControl(AControl.Parent)
    else
      Break;
  end;
end;

procedure TipHorzScrollBoxBase.ChildrenMouseDown(const AObject: TControl;
  AButton: TMouseButton; AShift: TShiftState; X, Y: Single);
begin
  if (Assigned(Root.Captured) and (TControl(Root.Captured) <> AObject)) or Supports(AObject, ITextInput) or (Supports(Root.Focused, ICaret) and (Assigned(Root.Captured) or CheckControlInParentTree(AObject, TControl(Root.Focused)))) then
    Exit;
  FMouseEvents := True;
  if not AObject.AutoCapture then
    TipControlAccess(AObject).Capture;
  if AButton = TMouseButton.mbLeft then
  begin
    MousePosToAni(AObject, X, Y);
    AniMouseDown(True{ssTouch in AShift}, X, Y);
  end;
end;

procedure TipHorzScrollBoxBase.ChildrenMouseMove(const AObject: TControl;
  AShift: TShiftState; X, Y: Single);
begin
  if (Assigned(Root.Captured) and (TControl(Root.Captured) <> AObject)) or Supports(AObject, ITextInput) or (Supports(Root.Focused, ICaret) and (Assigned(Root.Captured) or CheckControlInParentTree(AObject, TControl(Root.Focused)))) then
    Exit;
  FMouseEvents := True;
  if AniCalculations.Down then
  begin
    MousePosToAni(AObject, X, Y);
    AniMouseMove(ssTouch in AShift, X, Y);
  end;
end;

procedure TipHorzScrollBoxBase.ChildrenMouseUp(const AObject: TControl;
  AButton: TMouseButton; AShift: TShiftState; X, Y: Single);
begin
  if (Assigned(Root.Captured) and (TControl(Root.Captured) <> AObject)) or Supports(AObject, ITextInput) or (Supports(Root.Focused, ICaret) and (Assigned(Root.Captured) or CheckControlInParentTree(AObject, TControl(Root.Focused)))) then
    Exit;
  FMouseEvents := True;
  if not AObject.AutoCapture then
    TipControlAccess(AObject).ReleaseCapture;
  if AButton = TMouseButton.mbLeft then
  begin
    MousePosToAni(AObject, X, Y);
    AniMouseUp(ssTouch in AShift, X, Y);
  end;
end;

procedure TipHorzScrollBoxBase.ChildrenMouseLeave(const AObject: TControl);
begin
  if FMouseEvents and AniCalculations.Down then
  begin
    AniCalculations.MouseLeave;
    if (AniCalculations.LowVelocity) or
       (not AniCalculations.Animation) then
      AniCalculations.MouseLeave;
  end;
end;
{$ENDIF}

procedure TipHorzScrollBoxBase.CMGesture(var AEventInfo: TGestureEventInfo);
begin
  if (ContentLayout <> nil) and (AEventInfo.GestureID = igiPan) then
    FMouseEvents := False;
  inherited;
end;

constructor TipHorzScrollBoxBase.Create(AOwner: TComponent);
begin
  inherited;
  Touch.DefaultInteractiveGestures := Touch.DefaultInteractiveGestures + [TInteractiveGesture.Pan];
  Touch.InteractiveGestures := Touch.InteractiveGestures + [TInteractiveGesture.Pan];
end;

procedure TipHorzScrollBoxBase.CustomContentChanged(
  const ACustomContentSize: TSizeF);
begin
  InvalidateContentSize;
end;

procedure TipHorzScrollBoxBase.DoUpdateAniCalculations(
  const AAniCalculations: TScrollCalculations);
begin
  inherited;
  AAniCalculations.BoundsAnimation := True;
  //AAniCalculations.Animation := True;
  AAniCalculations.AutoShowing := True;
end;

function TipHorzScrollBoxBase.GetBoundsAnimation: Boolean;
begin
  Result := FBoundsAnimation;
end;

function TipHorzScrollBoxBase.GetIsTracking: Boolean;
begin
  Result := False;
end;

procedure TipHorzScrollBoxBase.MouseDown(AButton: TMouseButton;
  AShift: TShiftState; AX, AY: Single);
begin
  if AButton = TMouseButton.mbLeft then
    Include(AShift, ssTouch);
  inherited;
end;

{$IFDEF DELPHI_FIXES}
procedure TipHorzScrollBoxBase.MousePosToAni(const AObject: TControl; var X, Y: Single);
var
  LPoint: TPointF;
begin
  LPoint := TPointF.Create(X, Y);
  if ContentLayout <> nil then
    LPoint := ContentLayout.AbsoluteToLocal(AObject.LocalToAbsolute(LPoint))
  else
    LPoint := AbsoluteToLocal(AObject.LocalToAbsolute(LPoint));
  X := LPoint.X;
  Y := LPoint.Y;
end;
{$ENDIF}

procedure TipHorzScrollBoxBase.SetBoundsAnimation(AValue: Boolean);
begin
  FBoundsAnimation := AValue;
end;

procedure TipHorzScrollBoxBase.SetScrollBarsVisibility(AValue: TScrollBarsVisibility);
begin
  FScrollBarsVisibility := AValue;
  if AValue = TScrollBarsVisibility.Always then
    ShowScrollBars := True
  else
    ShowScrollBars := False;
end;

initialization
  RegisterFMXClasses([TipHorzScrollBoxBase]);
{$ENDIF}
end.
