{ ******************************************************* }
{ This component makes usage of Alcinoe library where }
{ can be found its license information on: }
{ https://github.com/MagicFoundation/Alcinoe/blob/master/license.txt }
{ }
{ Magic Foundation, All rights reserved }
{ }
{ ******************************************************* }

unit IdeaL.Lib.View.Fmx.FrameListModel;

interface

uses
  System.Math,
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Generics.Collections,

  Fmx.Types,
  Fmx.Graphics,
  Fmx.Controls,
  Fmx.Forms,
  Fmx.Dialogs,
  Fmx.StdCtrls,
  Fmx.Layouts,
  Fmx.Objects,
  Fmx.Controls.Presentation,

{$IFDEF ComponentTipScrollBox}
  ALFmxLayouts,
  iPub.Fmx.HorzScrollBox.Base,
  iPub.Fmx.HorzScrollBox,
  iPub.Fmx.VertScrollBox.Base,
  iPub.Fmx.VertScrollBox,
{$ENDIF}
  IdeaL.Lib.View.Fmx.FrameItemListModel;

type
  TFrameItemListModel = IdeaL.Lib.View.Fmx.FrameItemListModel.TFrameItemListModel;

  IScrollBoxLocal = interface
    ['{BAFE747B-FA28-4598-97BB-3B0F8B1EF314}']
    procedure SetIsGradientTransparency(const Value: Boolean);
    function GetIsGradientTransparency: Boolean;
    property IsGradientTransparency: Boolean read GetIsGradientTransparency write SetIsGradientTransparency;
  end;

  TScrollBoxLocal = class(TScrollBox, IScrollBoxLocal)
  private
    FIsGradientTransparency: Boolean;
    function GetIsGradientTransparency: Boolean;
    procedure SetIsGradientTransparency(const Value: Boolean);
    { private declarations }
  protected
    procedure PaintChildren; override;
    { protected declarations }
  public
    property IsGradientTransparency: Boolean read GetIsGradientTransparency write SetIsGradientTransparency;
    { public declarations }
  end;

{$IFDEF ComponentTipScrollBox}

  TipHorzScrollBoxLocal = class(TipHorzScrollBox, IScrollBoxLocal)
  private
    FIsGradientTransparency: Boolean;
    FIsEndOfListing: Boolean;

    function GetIsGradientTransparency: Boolean;
    procedure SetIsGradientTransparency(const Value: Boolean);

    function MaxViewPort: Single;
    { private declarations }
  protected
    procedure PaintChildren; override;
    procedure ViewportPositionChange(
      const AOldViewportPosition,
      ANewViewportPosition: TPointF;
      const AContentSizeChanged: Boolean
      ); override;
    { protected declarations }
  public
    property IsGradientTransparency: Boolean read GetIsGradientTransparency write SetIsGradientTransparency;
    { public declarations }
  end;

  TipVertScrollBoxLocal = class(TipVertScrollBox, IScrollBoxLocal)
  private
    FIsGradientTransparency: Boolean;
    FIsEndOfListing: Boolean;

    function GetIsGradientTransparency: Boolean;
    procedure SetIsGradientTransparency(const Value: Boolean);

    function MaxViewPort: Single;
    { private declarations }
  protected
    procedure PaintChildren; override;
    procedure ViewportPositionChange(
      const AOldViewportPosition,
      ANewViewportPosition: TPointF;
      const AContentSizeChanged: Boolean
      ); override;
    { protected declarations }
  public
    constructor Create(AOwner: TComponent); override;
    property IsGradientTransparency: Boolean read GetIsGradientTransparency write SetIsGradientTransparency;
    { public declarations }
  end;
{$ENDIF}

  TCustomScrollBox =
{$IFDEF ComponentTipScrollBox}
    TALCustomScrollBox
{$ELSE}
    TScrollBox
{$ENDIF}
    ;

  TFrameListModel = class(TFrame)
    lytBackground: TLayout;
    lytEmptyResult: TLayout;
    lblEmptyResult: TLabel;
    pthEmptyResult: TPath;
    lytEmptyResultIcon: TLayout;
  private
    FClickItem: TNotifyEvent;
    FItemSelected: TControl;
    FFrameListOnDemand: TFrame;
    FOnDemandBoolean: TProc<Boolean>;
    FIsHideEmptyResultIcon: Boolean;
    FItemAlign: TAlignLayout;
    FDemandCountShow: Integer;
    FIsGradientTransparency: Boolean;

    procedure ClickItem(Sender: TObject);
    procedure TapItem(Sender: TObject; const Point: TPointF);
    function GetChildrenCount: Integer;
    function GetContentHeight: Single;
    function GetContentWidth: Single;
    procedure SetItemSelected(const Value: TControl);
    procedure SetFrameListOnDemand(const Value: TFrame);
    procedure OnPaitingOnDemand(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
    procedure SetOnDemandBoolean(const Value: TProc<Boolean>);
    procedure SetItemAlign(const Value: TAlignLayout);
    procedure SetIsHideEmptyResultIcon(const Value: Boolean);
    procedure SetIsGradientTransparency(const Value: Boolean);
    { Private declarations }
  protected
    FObjList: TObjectList<TControl>;
    FScrlList: TCustomScrollBox;
    { Protected declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure BeginUpdate; override;
    procedure EndUpdate; override;

    property ChildrenCount: Integer read GetChildrenCount;
    property ContentHeight: Single read GetContentHeight;
    property ContentWidth: Single read GetContentWidth;
    property OnClickItem: TNotifyEvent read FClickItem write FClickItem;
    property ObjList: TObjectList<TControl> read FObjList;
    property IsHideEmptyResultIcon: Boolean read FIsHideEmptyResultIcon write SetIsHideEmptyResultIcon;

    procedure ClearList(ATarget: TFmxObject; AClickItem: TNotifyEvent); overload;
    procedure ClearList(AIsDoBeginUpdate: Boolean = True); overload;
    function AddItem(AClass: TComponentClass): TFrameItemListModel; overload;
    function AddItem(AControl: TControl): Integer; overload;
    procedure RemoveItem(AItem: TFrameItemListModel);
    procedure FinishListing;

    property ItemSelected: TControl read FItemSelected write SetItemSelected;
    property ItemAlign: TAlignLayout read FItemAlign write SetItemAlign;
    property VtsList: TCustomScrollBox read FScrlList;

    property DemandCountShow: Integer read FDemandCountShow write FDemandCountShow;
    property FrameListOnDemand: TFrame read FFrameListOnDemand write SetFrameListOnDemand;
    property OnDemandBoolean: TProc<Boolean> read FOnDemandBoolean write SetOnDemandBoolean;

    /// <summary> Show a transparency gradient to give an idea of 'there is more items'
    /// </summary>
    property IsGradientTransparency: Boolean read FIsGradientTransparency write SetIsGradientTransparency;
    { Public declarations }
  end;

implementation

uses
{$IFDEF SKIA}
  System.Skia,
  FMX.Skia.Canvas,
{$ENDIF}

  IdeaL.Lib.View.Utils,
  Fmx.Utils,
  System.UIConsts;

{$R *.fmx}

{ TFrameListModel }

procedure TFrameListModel.ClickItem(Sender: TObject);
begin
  if Assigned(FClickItem) then
    FClickItem(Sender);
end;

constructor TFrameListModel.Create(AOwner: TComponent);
begin
  inherited;
  Name := Self.ClassName + TUtils.GetGUIDAsComponentName;
  FClickItem := nil;
  FObjList := TObjectList<TControl>.Create;
  FItemSelected := nil;
  FFrameListOnDemand := nil;
  FOnDemandBoolean := nil;
  FScrlList := nil;
  ItemAlign := TAlignLayout.Top;
  lytEmptyResult.Visible := False;
  pthEmptyResult.Fill.Color := StringToAlphaColor(TUtils.TextMessageColorOpacity);
  lblEmptyResult.TextSettings.FontColor := StringToAlphaColor(TUtils.TextMessageColorOpacity);
  IsHideEmptyResultIcon := False;
  DemandCountShow := 50;
  IsGradientTransparency := True;
end;

destructor TFrameListModel.Destroy;
begin
  if (Assigned(FObjList)) then
  begin
    while FObjList.Count > 0 do
      FObjList.ExtractAt(0);
    FreeAndNil(FObjList);
  end;
  inherited;
end;

procedure TFrameListModel.FinishListing;
begin
  if ((ChildrenCount Mod DemandCountShow) = 0) then
  begin
    FrameListOnDemand := ObjList[Pred(ObjList.Count)] as TFrame;
  end;
end;

function TFrameListModel.GetChildrenCount: Integer;
begin
  Result := 0;
  if Assigned(FObjList) then
    Result := FObjList.Count;
end;

function TFrameListModel.GetContentHeight: Single;
begin
  Result := 0;
  if (TUtils.IsAssigned(Self)) and (Assigned(FObjList)) then
  begin
    for var LItem in FObjList do
    begin
      if Assigned(LItem) then
        Result := Result +
          TControl(LItem).Height +
          TControl(LItem).Margins.Bottom +
          TControl(LItem).Margins.Top;
    end;
  end;
end;

function TFrameListModel.GetContentWidth: Single;
begin
  Result := 0;
  if (TUtils.IsAssigned(Self)) and (Assigned(FObjList)) then
  begin
    for var LItem in FObjList do
    begin
      if Assigned(LItem) then
        Result := Result +
          TControl(LItem).Width +
          TControl(LItem).Margins.Left +
          TControl(LItem).Margins.Right;
    end;
  end;
end;

procedure TFrameListModel.OnPaitingOnDemand(Sender: TObject; Canvas: TCanvas;
const ARect: TRectF);
begin
  if Assigned(FFrameListOnDemand) then
  begin // Se estiver assinado tem que remover o Painting
    FFrameListOnDemand.OnPainting := nil;
    FFrameListOnDemand := nil;
  end;
  if Assigned(FOnDemandBoolean) then
    FOnDemandBoolean(False);
end;

procedure TFrameListModel.RemoveItem(AItem: TFrameItemListModel);
begin
  if (Assigned(FObjList)) and (FObjList.Contains(AItem)) then
  begin
    FObjList.Remove(AItem);
    if FObjList.Count = 0 then
      EndUpdate;
  end;
end;

procedure TFrameListModel.SetIsGradientTransparency(const Value: Boolean);
begin
  FIsGradientTransparency := Value;
  var
  LName := FScrlList.ClassName;
  if (Assigned(FScrlList)) then
  begin
    (FScrlList as IScrollBoxLocal).IsGradientTransparency := Value;
  end;
end;

procedure TFrameListModel.SetFrameListOnDemand(const Value: TFrame);
begin
  if Assigned(FFrameListOnDemand) then
  begin // Se estiver assinado tem que remover o Painting
    FFrameListOnDemand.OnPainting := nil;
    FFrameListOnDemand := nil;
  end;
  FFrameListOnDemand := Value;
  FFrameListOnDemand.OnPainting := OnPaitingOnDemand;
end;

procedure TFrameListModel.SetIsHideEmptyResultIcon(const Value: Boolean);
begin
  FIsHideEmptyResultIcon := Value;
end;

procedure TFrameListModel.SetItemAlign(const Value: TAlignLayout);
begin
  if (TUtils.IsAssigned(FScrlList)) and
    (Assigned(FObjList)) and
    (FObjList.Count > 0)
  then
    raise Exception.Create('ItemAlign could not be changed, please clear the list first');

  TThread.Synchronize(nil,
    procedure
    begin
      if (TUtils.IsAssigned(FScrlList)) then
        FreeAndNil(FScrlList);
{$IFDEF ComponentTipScrollBox}
      var
      LName := EmptyStr;
      LName := TUtils.GetEnumName<TAlignLayout>(Value);

      if TUtils.Contains(LName.Trim.ToUpper, ['LEFT', 'RIGHT']) then
      begin
        FScrlList := TipHorzScrollBoxLocal.Create(Self);
        
		// also need the followin "Conditional Defines" TipScrollBox;ALCINOE;DELPHI_FIXES;ComponentTipScrollBox
		TipHorzScrollBoxLocal(FScrlList).BoundsAnimation := False;
      end
      else
      begin
        FScrlList := TipVertScrollBoxLocal.Create(Self);
        TipVertScrollBoxLocal(FScrlList).BoundsAnimation := False;
      end;
{$ELSE}
      FScrlList := TScrollBoxLocal.Create(Self);
      TScrollBoxLocal(FScrlList).AniCalculations.BoundsAnimation := False;
{$ENDIF}
      (FScrlList as IScrollBoxLocal).IsGradientTransparency := IsGradientTransparency;
      FScrlList.Parent := lytBackground;
      FScrlList.Align := TAlignLayout.Contents;
      FScrlList.TabStop := False;
      FScrlList.ShowScrollBars := False;
      FScrlList.SendToBack;
    end);
  FItemAlign := Value;
end;

procedure TFrameListModel.SetItemSelected(const Value: TControl);
begin
  FItemSelected := Value;
end;

procedure TFrameListModel.SetOnDemandBoolean(const Value: TProc<Boolean>);
begin
  FOnDemandBoolean := Value;
end;

function TFrameListModel.AddItem(AClass: TComponentClass): TFrameItemListModel;
var
  LName: string;
  LCount: Integer;
begin
  var
  LBool := FScrlList.IsUpdating;
  LBool := FScrlList.Content.IsUpdating;

  LName := FormatDateTime('yyyymmddhhnnsszzz', Now) + '_' + (Random(1000000) + 1).ToString;
  LCount := 0;
  if Assigned(FScrlList.Content) then
    LCount := FScrlList.Content.ChildrenCount;
  Result := (AClass.Create(Self) as TFrameItemListModel);
  try
    Result.Name := Result.ClassName + LCount.ToString + '_' + LName;
    Result.Align := ItemAlign;

    case ItemAlign of
      TAlignLayout.Top, TAlignLayout.Bottom:
        Result.Position.Y := ContentHeight;
      TAlignLayout.Left, TAlignLayout.Right:
        Result.Position.X := ContentWidth;
    end;

    if IsUpdating then
      Result.BeginUpdate;
    FObjList.Add(Result);
    FScrlList.Content.AddObject(Result);

    if Assigned(FClickItem) then
    begin
{$IFDEF MSWINDOWS}
      Result.SetDefaultClick(ClickItem);
{$ELSE}
      Result.SetDefaultTap(TapItem);
{$ENDIF}
    end;
    Result.SendToBack;
  except
    Result.Free;
    raise;
  end;
end;

function TFrameListModel.AddItem(AControl: TControl): Integer;
begin
  Result := FObjList.Add(AControl);
  FScrlList.Content.AddObject(AControl);
end;

procedure TFrameListModel.BeginUpdate;
begin
  //FScrlList.Visible := False;
  if Assigned(FScrlList) then
    FScrlList.BeginUpdate;
  if Assigned(FScrlList.Content) then
    FScrlList.Content.BeginUpdate;
  // lytEmptyResult.Visible := False;
  inherited;
end;

procedure TFrameListModel.EndUpdate;
begin
  TThread.Synchronize(nil,
    procedure
    begin
      try
        if Assigned(FScrlList.Content) then
          FScrlList.Content.EndUpdate;
        FScrlList.EndUpdate;
        FScrlList.Visible := True;
        for var LItem in FObjList do
        begin
          LItem.EndUpdate;
        end;
        FScrlList.BringToFront;
        if not IsHideEmptyResultIcon then
        begin
          lytEmptyResult.Visible := ChildrenCount = 0;
          if lytEmptyResult.Visible then
            lytEmptyResult.BringToFront;
        end;
        FScrlList.Height := FScrlList.Height + 1;
      except

      end;
      inherited;
      Repaint;
    end);
end;

procedure TFrameListModel.ClearList(AIsDoBeginUpdate: Boolean);
begin
  try
    if AIsDoBeginUpdate then
      BeginUpdate;
    FObjList.Clear;
  finally
    if AIsDoBeginUpdate then
      EndUpdate;
  end;
end;

procedure TFrameListModel.ClearList(ATarget: TFmxObject;
AClickItem: TNotifyEvent);
begin
  FClickItem := AClickItem;
  FObjList.Clear;

  if Assigned(ATarget) then
    lytBackground.Parent := ATarget;
end;

procedure TFrameListModel.TapItem(Sender: TObject; const Point: TPointF);
begin
  ClickItem(Sender);
end;

{$IFDEF ComponentTipScrollBox}

{ TipHorzScrollBoxLocal }

function TipHorzScrollBoxLocal.GetIsGradientTransparency: Boolean;
begin
  Result := FIsGradientTransparency;
end;

function TipHorzScrollBoxLocal.MaxViewPort: Single;
begin
  Result := Self.Content.Width;
end;

procedure TipHorzScrollBoxLocal.PaintChildren;
begin
  if (IsUpdating) then
    Exit;
{$IFDEF SKIA}
  var
  LViewPort := Abs(Self.ViewportPosition.X);
  var
  LClientHeight := LViewPort + Self.Width;
  var
  LIsEndOfListing := (MaxViewPort < (LClientHeight * 1.01)); // means reached the end of

  if (not IsUpdating) and (Canvas is TSkCanvasCustom) and (IsGradientTransparency) and (not LIsEndOfListing) then
  begin
    var
      LCanvas: ISkCanvas := TSkCanvasCustom(Canvas).Canvas;
    var
    LShader := TSkShader.MakeGradientLinear(PointF(0, 0), PointF(Width, 0), [TAlphaColors.Black, TAlphaColors.Null], [0.9, 0.97]);
    var
      LPaint: ISkPaint := TSkPaint.Create;
    LPaint.ImageFilter := TSkImageFilter.MakeBlend(TSkBlendMode.SrcIn, TSkImageFilter.MakeShader(LShader, False));
    LCanvas.SaveLayer(LPaint);
    try
      inherited;
    finally
      LCanvas.Restore;
    end;
  end
  else
{$ENDIF}
    inherited;
end;

procedure TipHorzScrollBoxLocal.SetIsGradientTransparency(const Value: Boolean);
begin
  FIsGradientTransparency := Value;
end;

procedure TipHorzScrollBoxLocal.ViewportPositionChange(
  const AOldViewportPosition, ANewViewportPosition: TPointF;
  const AContentSizeChanged: Boolean);
begin
  inherited;
  
end;

{ TipVertScrollBoxLocal }

constructor TipVertScrollBoxLocal.Create(AOwner: TComponent);
begin
  inherited;

end;

function TipVertScrollBoxLocal.GetIsGradientTransparency: Boolean;
begin
  Result := FIsGradientTransparency;
end;

function TipVertScrollBoxLocal.MaxViewPort: Single;
begin
  Result := Self.Content.Height;
  //Result := TRectF(CalcContentBounds * LocalRect).Top - Height;
end;

procedure TipVertScrollBoxLocal.PaintChildren;
begin
  if (IsUpdating) then
    Exit;
{$IFDEF SKIA}
  var
  LViewPort := Abs(Self.ViewportPosition.Y);
  var
  LClientHeight := LViewPort + Self.Height;
  var
  LIsEndOfListing := (MaxViewPort < (LClientHeight * 1.01)); // means reached the end of

  if (Canvas is TSkCanvasCustom) and (IsGradientTransparency) and (not LIsEndOfListing) then
  begin
    var
      LCanvas: ISkCanvas := TSkCanvasCustom(Canvas).Canvas;
    var
    LShader := TSkShader.MakeGradientLinear(PointF(0, 0), PointF(0, Height), [TAlphaColors.Black, TAlphaColors.Null], [0.9, 0.97]);
    // LShader := TSkShader.MakeGradientLinear(PointF(0, 0), PointF(0, Height), [TAlphaColors.Black, InterpolateColor(TAlphaColors.Black, TAlphaColors.Null, 1)], [0.9, 0.97]);

    var
      LPaint: ISkPaint := TSkPaint.Create;
    LPaint.ImageFilter := TSkImageFilter.MakeBlend(TSkBlendMode.SrcIn, TSkImageFilter.MakeShader(LShader, False));
    LCanvas.SaveLayer(LPaint);
    try
      inherited;
    finally
      LCanvas.Restore;
    end;
  end
  else
{$ENDIF}
    inherited;
end;

procedure TipVertScrollBoxLocal.SetIsGradientTransparency(const Value: Boolean);
begin
  FIsGradientTransparency := Value;
end;

procedure TipVertScrollBoxLocal.ViewportPositionChange(
  const AOldViewportPosition, ANewViewportPosition: TPointF;
const AContentSizeChanged: Boolean);
begin
  inherited;

end;

{$ENDIF}

{ TScrollBoxLocal }

function TScrollBoxLocal.GetIsGradientTransparency: Boolean;
begin
  Result := FIsGradientTransparency;
end;

procedure TScrollBoxLocal.PaintChildren;
begin
  if (IsUpdating) then
    Exit;
{$IFDEF SKIA}
  // here it must check if Vert or Horz scroll...
  if (Canvas is TSkCanvasCustom) and (IsGradientTransparency) then
  begin
    var
      LCanvas: ISkCanvas := TSkCanvasCustom(Canvas).Canvas;
    var
    LShader := TSkShader.MakeGradientLinear(PointF(0, 0), PointF(0, Height), [TAlphaColors.Black, TAlphaColors.Null], [0.9, 0.97]);
    var
      LPaint: ISkPaint := TSkPaint.Create;
    LPaint.ImageFilter := TSkImageFilter.MakeBlend(TSkBlendMode.SrcIn, TSkImageFilter.MakeShader(LShader, False));
    LCanvas.SaveLayer(LPaint);
    try
      inherited;
    finally
      LCanvas.Restore;
    end;
  end
  else
{$ENDIF}
    inherited;
end;

procedure TScrollBoxLocal.SetIsGradientTransparency(const Value: Boolean);
begin
  FIsGradientTransparency := Value;
end;

end.
