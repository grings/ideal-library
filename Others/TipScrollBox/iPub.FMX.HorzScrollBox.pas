unit iPub.FMX.HorzScrollBox;

interface

{$SCOPEDENUMS ON}

uses
  { Delphi }
  System.Types,
  System.Classes,
  FMX.Controls,
  FMX.Types,
  FMX.Layouts,

  { iPub }
  iPub.FMX.HorzScrollBox.Base;

type
  TScrollBarsVisibility = iPub.FMX.HorzScrollBox.Base.TScrollBarsVisibility;


  { IipDontInsideContent }

  IipDontInsideContent = interface
    ['{C041EB96-EA68-456D-B873-5A12B8797BAB}']
  end;


  { TipHorzScrollBox }

  [ComponentPlatforms(pidWin32 or pidWin64 or pidLinux64 or pidAndroidArm32 or pidAndroidArm64 or pidiOSDevice64 or pidOSX64 or pidOSXArm64)]
  TipHorzScrollBox = class(TipHorzScrollBoxBase)
  private type
    TipContentSizeChanged = procedure(ASender: TObject; const ABounds: TRectF) of object;
  private const
    DEFAULT_RIGHT_PADDING = 0;
    DEFAULT_BOUNDS_ANIMATION = True;
    DEFAULT_FOOTER_MARGIN = 22;
    DEFAULT_LEFT_PADDING = 0;
  private
    //FChangingFooter: Boolean;
    FCustomContentWidth: Single;
    [Weak] FFooter: TControl;
    FFooterMargin: Single;
    FOnContentSizeChanged: TipContentSizeChanged;
    FShowDesignBorder: Boolean;
    function GetRightPadding: Single;
    function GetLeftPadding: Single;
    function IsRightPaddingStored: Boolean;
    function IsBoundsAnimationStored: Boolean;
    function IsShowDesignBorderStored: Boolean;
    function IsFooterMarginStored: Boolean;
    function IsLeftPaddingStored: Boolean;
    procedure SetRightPadding(AValue: Single);
    procedure SetCustomContentWidth(AValue: Single);
    procedure SetFooter(AValue: TControl);
    procedure SetFooterMargin(AValue: Single);
    procedure SetFooterPosition;
    procedure SetShowDesignBorder(AValue: Boolean);
    procedure SetLeftPadding(AValue: Single);
  protected
    procedure ChangedRightPadding(AOldValue, ANewValue: Single); virtual;
    procedure ChangedLeftPadding(AOldValue, ANewValue: Single); virtual;
    function DefaultShowDesignBorder: Boolean; virtual;
    function DoCalcContentBounds: TRectF; override;
    procedure DoRealignContent(ARect: TRectF); override;
    function GetBoundsAnimation: Boolean; override;
    function IsAddToContent(const AObject: TFmxObject): Boolean; override;
    procedure Paint; override;
    procedure SetBoundsAnimation(AValue: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure LockScroll;
    procedure UnlockScroll;
    property CustomContentWidth: Single read FCustomContentWidth write SetCustomContentWidth;
    property DisabledOpacity;
    property OnContentSizeChanged: TipContentSizeChanged read FOnContentSizeChanged write FOnContentSizeChanged;
  published
    property RightPadding: Single read GetRightPadding write SetRightPadding stored IsRightPaddingStored;
    property BoundsAnimation: Boolean read GetBoundsAnimation write SetBoundsAnimation stored IsBoundsAnimationStored;
    property Footer: TControl read FFooter write SetFooter;
    property FooterMargin: Single read FFooterMargin write SetFooterMargin stored IsFooterMarginStored;
    property ShowDesignBorder: Boolean read FShowDesignBorder write SetShowDesignBorder stored IsShowDesignBorderStored;
    property LeftPadding: Single read GetLeftPadding write SetLeftPadding stored IsLeftPaddingStored;
  end;

procedure Register;

implementation

uses
  { Delphi }
  System.UITypes,
  System.SysUtils,
  System.Math,
  System.Math.Vectors,
  FMX.Consts;

{ TipHorzScrollBox }

function TipHorzScrollBox.DefaultShowDesignBorder: Boolean;
begin
  Result := True;
end;

function TipHorzScrollBox.DoCalcContentBounds: TRectF;
begin
  if not SameValue(FCustomContentWidth, 0, TEpsilon.Position) then
  begin
    Result := LocalRect;
    Result.Width := Max(Result.Width, FCustomContentWidth);
  end
  else
  begin
    if Assigned(FFooter) then
      SetFooterPosition;
    Result := inherited DoCalcContentBounds;
  end;
end;

procedure TipHorzScrollBox.DoRealignContent(ARect: TRectF);
begin
  inherited;
  if Assigned(FOnContentSizeChanged) then
    FOnContentSizeChanged(Self, ARect);
end;

procedure TipHorzScrollBox.ChangedRightPadding(AOldValue, ANewValue: Single);
begin
end;

procedure TipHorzScrollBox.ChangedLeftPadding(AOldValue, ANewValue: Single);
begin
end;

constructor TipHorzScrollBox.Create(AOwner: TComponent);
begin
  inherited;
  AniCalculations.TouchTracking := [ttHorizontal];
  FFooterMargin := DEFAULT_FOOTER_MARGIN;
  FShowDesignBorder := DefaultShowDesignBorder;
  BoundsAnimation := DEFAULT_BOUNDS_ANIMATION;
end;

function TipHorzScrollBox.GetRightPadding: Single;
begin
  Result := Content.Padding.Right;
end;

function TipHorzScrollBox.GetBoundsAnimation: Boolean;
begin
  Result := inherited;
end;

function TipHorzScrollBox.GetLeftPadding: Single;
begin
  Result := Content.Padding.Left;
end;

function TipHorzScrollBox.IsAddToContent(const AObject: TFmxObject): Boolean;
begin
  Result := inherited and not Supports(AObject, IipDontInsideContent);
end;

function TipHorzScrollBox.IsRightPaddingStored: Boolean;
begin
  Result := not SameValue(Content.Padding.Right, DEFAULT_RIGHT_PADDING, TEpsilon.Position);
end;

function TipHorzScrollBox.IsBoundsAnimationStored: Boolean;
begin
  Result := BoundsAnimation <> DEFAULT_BOUNDS_ANIMATION;
end;

function TipHorzScrollBox.IsFooterMarginStored: Boolean;
begin
  Result := not SameValue(FFooterMargin, DEFAULT_FOOTER_MARGIN, TEpsilon.Position);
end;

function TipHorzScrollBox.IsShowDesignBorderStored: Boolean;
begin
  Result := FShowDesignBorder <> DefaultShowDesignBorder;
end;

function TipHorzScrollBox.IsLeftPaddingStored: Boolean;
begin
  Result := not SameValue(Content.Padding.Left, DEFAULT_LEFT_PADDING, TEpsilon.Position);
end;

procedure TipHorzScrollBox.LockScroll;
begin
  AniCalculations.TouchTracking := [];
end;

procedure TipHorzScrollBox.Paint;
begin
  if FShowDesignBorder and (csDesigning in ComponentState) and not Locked then
    DrawDesignBorder;
end;

procedure TipHorzScrollBox.SetRightPadding(AValue: Single);
var
  LOldValue: Single;
begin
  LOldValue := Content.Padding.Right;
  if not SameValue(LOldValue, AValue, Epsilon) then
  begin
    Content.Padding.Right := AValue;
    ChangedRightPadding(LOldValue, AValue);
  end;
end;

procedure TipHorzScrollBox.SetBoundsAnimation(AValue: Boolean);
begin
  inherited;
end;

procedure TipHorzScrollBox.SetCustomContentWidth(AValue: Single);
begin
  if not SameValue(FCustomContentWidth, AValue, TEpsilon.Position) then
  begin
    FCustomContentWidth := AValue;
    CustomContentChanged(TSizeF.Create(0, FCustomContentWidth));
    Realign;
  end;
end;

procedure TipHorzScrollBox.SetFooter(AValue: TControl);
begin
  if AValue = FFooter then
    Exit;
  if Assigned(FFooter) then
    FFooter.Parent := nil;
  FFooter := AValue;
  if Assigned(FFooter) then
  begin
    //if FFooter.Parent = Self then
      FFooter.Parent := nil;
    SetFooterPosition;
    FFooter.Align := TAlignLayout.Left;
    FFooter.Parent := Self;
  end;
end;

procedure TipHorzScrollBox.SetFooterMargin(AValue: Single);
var
  LFooter: TControl;
begin
  if AValue = FFooterMargin then
    Exit;
  FFooterMargin := AValue;
  if Assigned(FFooter) then
  begin
    LFooter := FFooter;
    Footer := nil;
    Footer := LFooter;
  end;
end;

procedure TipHorzScrollBox.SetFooterPosition;
var
  I: Integer;
  LRect: TRectF;
  LControlsCount: Integer;
begin
  if not Assigned(FFooter) then
    Exit;
  LRect := RectF(0, 0, 0, 0);
  LControlsCount := 0;
  if (Content <> nil) then
    for I := 0 to Content.ControlsCount - 1 do
      if Content.Controls[I].Visible and (Content.Controls[I] <> FFooter) then
      begin
        {$IFDEF MSWINDOWS}
        if (csDesigning in ComponentState) and Supports(Content.Controls[I], IDesignerControl) then
          Continue;
        {$ENDIF}
        LRect.Union(Content.Controls[I].BoundsRect);
        Inc(LControlsCount);
      end;
  if LRect.Left < 0 then
    LRect.Left := 0;
  if LRect.Left < 0 then
    LRect.Left := 0;

  if LControlsCount = 0 then
    FFooter.Margins.Left := Width - Content.Padding.Left - FFooter.Width
  else if ((LRect.Right < Width) or SameValue(LRect.Right, Width, TEpsilon.Position)) and
    (Width - LRect.Right > FFooterMargin + FFooter.Width) then
  begin
    FFooter.Margins.Left := (Width - LRect.Right) - FFooter.Width;
    FFooter.Position.X := LRect.Right + FFooter.Margins.Left;
  end
  else
  begin
    FFooter.Margins.Left := FFooterMargin;
    FFooter.Position.X := LRect.Right + FFooter.Margins.Left;
  end;
end;

procedure TipHorzScrollBox.SetShowDesignBorder(AValue: Boolean);
begin
  if FShowDesignBorder <> AValue then
  begin
    FShowDesignBorder := AValue;
    if csDesigning in ComponentState then
      Repaint;
  end;
end;

procedure TipHorzScrollBox.SetLeftPadding(AValue: Single);
var
  LOldValue: Single;
begin
  LOldValue := Content.Padding.Left;
  if not SameValue(LOldValue, AValue, Epsilon) then
  begin
    Content.Padding.Left := AValue;
    ChangedLeftPadding(LOldValue, AValue);
  end;
end;

procedure TipHorzScrollBox.UnlockScroll;
begin
  AniCalculations.TouchTracking := [ttHorizontal];
end;


procedure Register;
begin
  //RegisterComponents(TUtils.CPageName, [TipHorzScrollBox]);
end;

initialization
  RegisterFMXClasses([TipHorzScrollBox]);
end.
