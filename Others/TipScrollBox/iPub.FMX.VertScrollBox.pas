unit iPub.FMX.VertScrollBox;

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
  iPub.FMX.VertScrollBox.Base;

type
  TScrollBarsVisibility = iPub.FMX.VertScrollBox.Base.TScrollBarsVisibility;


  { IipDontInsideContent }

  IipDontInsideContent = interface
    ['{C041EB96-EA68-456D-B873-5A12B8797BAB}']
  end;


  { TipVertScrollBox }

  [ComponentPlatforms(pidWin32 or pidWin64 or pidLinux64 or pidAndroidArm32 or pidAndroidArm64 or pidiOSDevice64 or pidOSX64 or pidOSXArm64)]
  TipVertScrollBox = class(TipVertScrollBoxBase)
  private type
    TipContentSizeChanged = procedure(ASender: TObject; const ABounds: TRectF) of object;
  private const
    DEFAULT_BOTTOM_PADDING = 0;
    DEFAULT_BOUNDS_ANIMATION = True;
    DEFAULT_FOOTER_MARGIN = 22;
    DEFAULT_TOP_PADDING = 0;
  private
    //FChangingFooter: Boolean;
    FCustomContentHeight: Single;
    [Weak] FFooter: TControl;
    FFooterMargin: Single;
    FOnContentSizeChanged: TipContentSizeChanged;
    FShowDesignBorder: Boolean;
    function GetBottomPadding: Single;
    function GetTopPadding: Single;
    function IsBottomPaddingStored: Boolean;
    function IsBoundsAnimationStored: Boolean;
    function IsShowDesignBorderStored: Boolean;
    function IsFooterMarginStored: Boolean;
    function IsTopPaddingStored: Boolean;
    procedure SetBottomPadding(AValue: Single);
    procedure SetCustomContentHeight(AValue: Single);
    procedure SetFooter(AValue: TControl);
    procedure SetFooterMargin(AValue: Single);
    procedure SetFooterPosition;
    procedure SetShowDesignBorder(AValue: Boolean);
    procedure SetTopPadding(AValue: Single);
  protected
    procedure ChangedBottomPadding(AOldValue, ANewValue: Single); virtual;
    procedure ChangedTopPadding(AOldValue, ANewValue: Single); virtual;
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
    property CustomContentHeight: Single read FCustomContentHeight write SetCustomContentHeight;
    property DisabledOpacity;
    property OnContentSizeChanged: TipContentSizeChanged read FOnContentSizeChanged write FOnContentSizeChanged;
  published
    property BottomPadding: Single read GetBottomPadding write SetBottomPadding stored IsBottomPaddingStored;
    property BoundsAnimation: Boolean read GetBoundsAnimation write SetBoundsAnimation stored IsBoundsAnimationStored;
    property Footer: TControl read FFooter write SetFooter;
    property FooterMargin: Single read FFooterMargin write SetFooterMargin stored IsFooterMarginStored;
    property ShowDesignBorder: Boolean read FShowDesignBorder write SetShowDesignBorder stored IsShowDesignBorderStored;
    property TopPadding: Single read GetTopPadding write SetTopPadding stored IsTopPaddingStored;
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

{ TipVertScrollBox }

function TipVertScrollBox.DefaultShowDesignBorder: Boolean;
begin
  Result := True;
end;

function TipVertScrollBox.DoCalcContentBounds: TRectF;
begin
  if not SameValue(FCustomContentHeight, 0, TEpsilon.Position) then
  begin
    Result := LocalRect;
    Result.Height := Max(Result.Height, FCustomContentHeight);
  end
  else
  begin
    if Assigned(FFooter) then
      SetFooterPosition;
    Result := inherited DoCalcContentBounds;
  end;
end;

procedure TipVertScrollBox.DoRealignContent(ARect: TRectF);
begin
  inherited;
  if Assigned(FOnContentSizeChanged) then
    FOnContentSizeChanged(Self, ARect);
end;

procedure TipVertScrollBox.ChangedBottomPadding(AOldValue, ANewValue: Single);
begin
end;

procedure TipVertScrollBox.ChangedTopPadding(AOldValue, ANewValue: Single);
begin
end;

constructor TipVertScrollBox.Create(AOwner: TComponent);
begin
  inherited;
  AniCalculations.TouchTracking := [ttVertical];
  FFooterMargin := DEFAULT_FOOTER_MARGIN;
  FShowDesignBorder := DefaultShowDesignBorder;
  BoundsAnimation := DEFAULT_BOUNDS_ANIMATION;
end;

function TipVertScrollBox.GetBottomPadding: Single;
begin
  Result := Content.Padding.Bottom;
end;

function TipVertScrollBox.GetBoundsAnimation: Boolean;
begin
  Result := inherited;
end;

function TipVertScrollBox.GetTopPadding: Single;
begin
  Result := Content.Padding.Top;
end;

function TipVertScrollBox.IsAddToContent(const AObject: TFmxObject): Boolean;
begin
  Result := inherited and not Supports(AObject, IipDontInsideContent);
end;

function TipVertScrollBox.IsBottomPaddingStored: Boolean;
begin
  Result := not SameValue(Content.Padding.Bottom, DEFAULT_BOTTOM_PADDING, TEpsilon.Position);
end;

function TipVertScrollBox.IsBoundsAnimationStored: Boolean;
begin
  Result := BoundsAnimation <> DEFAULT_BOUNDS_ANIMATION;
end;

function TipVertScrollBox.IsFooterMarginStored: Boolean;
begin
  Result := not SameValue(FFooterMargin, DEFAULT_FOOTER_MARGIN, TEpsilon.Position);
end;

function TipVertScrollBox.IsShowDesignBorderStored: Boolean;
begin
  Result := FShowDesignBorder <> DefaultShowDesignBorder;
end;

function TipVertScrollBox.IsTopPaddingStored: Boolean;
begin
  Result := not SameValue(Content.Padding.Top, DEFAULT_TOP_PADDING, TEpsilon.Position);
end;

procedure TipVertScrollBox.LockScroll;
begin
  AniCalculations.TouchTracking := [];
end;

procedure TipVertScrollBox.Paint;
begin
  if FShowDesignBorder and (csDesigning in ComponentState) and not Locked then
    DrawDesignBorder;
end;

procedure TipVertScrollBox.SetBottomPadding(AValue: Single);
var
  LOldValue: Single;
begin
  LOldValue := Content.Padding.Bottom;
  if not SameValue(LOldValue, AValue, Epsilon) then
  begin
    Content.Padding.Bottom := AValue;
    ChangedBottomPadding(LOldValue, AValue);
  end;
end;

procedure TipVertScrollBox.SetBoundsAnimation(AValue: Boolean);
begin
  inherited;
end;

procedure TipVertScrollBox.SetCustomContentHeight(AValue: Single);
begin
  if not SameValue(FCustomContentHeight, AValue, TEpsilon.Position) then
  begin
    FCustomContentHeight := AValue;
    CustomContentChanged(TSizeF.Create(0, FCustomContentHeight));
    Realign;
  end;
end;

procedure TipVertScrollBox.SetFooter(AValue: TControl);
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
    FFooter.Align := TAlignLayout.Top;
    FFooter.Parent := Self;
  end;
end;

procedure TipVertScrollBox.SetFooterMargin(AValue: Single);
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

procedure TipVertScrollBox.SetFooterPosition;
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
  if LRect.Top < 0 then
    LRect.Top := 0;
  if LRect.Left < 0 then
    LRect.Left := 0;

  if LControlsCount = 0 then
    FFooter.Margins.Top := Height - Content.Padding.Top - FFooter.Height
  else if ((LRect.Bottom < Height) or SameValue(LRect.Bottom, Height, TEpsilon.Position)) and
    (Height - LRect.Bottom > FFooterMargin + FFooter.Height) then
  begin
    FFooter.Margins.Top := (Height - LRect.Bottom) - FFooter.Height;
    FFooter.Position.Y := LRect.Bottom + FFooter.Margins.Top;
  end
  else
  begin
    FFooter.Margins.Top := FFooterMargin;
    FFooter.Position.Y := LRect.Bottom + FFooter.Margins.Top;
  end;
end;

procedure TipVertScrollBox.SetShowDesignBorder(AValue: Boolean);
begin
  if FShowDesignBorder <> AValue then
  begin
    FShowDesignBorder := AValue;
    if csDesigning in ComponentState then
      Repaint;
  end;
end;

procedure TipVertScrollBox.SetTopPadding(AValue: Single);
var
  LOldValue: Single;
begin
  LOldValue := Content.Padding.Top;
  if not SameValue(LOldValue, AValue, Epsilon) then
  begin
    Content.Padding.Top := AValue;
    ChangedTopPadding(LOldValue, AValue);
  end;
end;

procedure TipVertScrollBox.UnlockScroll;
begin
  AniCalculations.TouchTracking := [ttVertical];
end;


procedure Register;
begin
//  RegisterComponents(TUtils.CPageName, [TipVertScrollBox]);
end;

initialization
  RegisterFMXClasses([TipVertScrollBox]);
end.
