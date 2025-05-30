unit IdeaL.Lib.PopUp.DateTimePicker;

interface

{

  Thanks for all your help:
  * Josafá Orrico widly known as "Algoritmos"
  * José Nilto


  Any date management, please, use System.SysUtils to work with TFormatSettings
  straight way

}

uses
  IdeaL.Lib.PopUp,

  System.Classes,
  System.SysUtils,
  System.StrUtils,
  System.UITypes,
  System.Types,
  System.DateUtils,
  System.Messaging,
  System.Math,
  System.UIConsts,

  FMX.Layouts,
  FMX.Types,
  FMX.Objects,
  FMX.Controls,
  FMX.Graphics,
  FMX.StdCtrls,
  FMX.Forms;

type
  TPickerType = (ptDate, ptTime);

  TDateTimePicker = class;
  TPickerMonthItem = class;

  TMessagingAction = class(TMessage)
  private
    { Private declarations }
  public
    class procedure Unsubscribe(AId: Integer; AImmediate: Boolean = True);
    class function Subscribe(const AListenerMethod: TMessageListenerMethod): Integer;
    { Public declarations }
  protected
    { protected declarations }
  end;

  TMessagingDateTimeChanged = class(TMessagingAction)
  private
    FDateTime: TDateTime;
    { Private declarations }
  public
    constructor Create(ADateTime: TDateTime); virtual;

    property DateTime: TDateTime read FDateTime;

    class procedure SendAction(Sender: TObject; ADateTime: TDateTime);
    { Public declarations }
  protected
    { protected declarations }
  end;

  TMessagingPickerTimeChanged = class(TMessagingAction)
  private
    FReference: string;
    { private declarations }
  protected
    { protected declarations }
  public
    constructor Create(AReference: string); virtual;

    property Reference: string read FReference;

    class procedure SendAction(Sender: TObject; AReference: string);
    { public declarations }
  end;

  TPickerItem = class(TVertScrollBox)
  private
    { private declarations }
  protected
    { protected declarations }
  public
    constructor Create(AOwner: TDateTimePicker); reintroduce; virtual;

    class function GetInstance(AOwner: TDateTimePicker): TPickerItem; virtual; abstract;

    procedure DoResize; virtual;

    function Prepare(AParent: TControl): TPickerItem; virtual;
    function Show: TPickerItem; virtual;
    { public declarations }
  published
    { published declarations }
  end;

  TPickerTime = class(TPickerItem)
  private
  var
    FLytCenter: TLayout;
    FRctCenter: TRectangle;
    FMessagingId: Integer;

  type
    TVtsSelection = class(TVertScrollBox)
    private
      FRangeStart: Integer;
      FRangeEnd: Integer;
      FSelectionHeight: Single;
      FMessagingId: Integer;

      FCurrentPosition: TPointF;
      FTmrPosition: TTimer;
      function GetCurrentIndex: Integer;
      procedure GetExtraPos(out AInt: Integer; out ADec: Double); overload;
      function GetExtraPos: Integer; overload;
      function GetValue: Integer;
      { private declarations }
    protected
      procedure ChangetViewportPosition(APointF: TPointF);
      procedure DoTimer(Sender: TObject);
      procedure DoViewportPositionChange(Sender: TObject;
        const OldViewportPosition, NewViewportPosition: TPointF;
        const ContentSizeChanged: Boolean);

      property CurrentIndex: Integer read GetCurrentIndex;
      { protected declarations }
    public
      constructor Create(AOwner: TComponent); override;

      property Value: Integer read GetValue;

      function Range(AStart, AEnd: Integer): TVtsSelection;
      function Prepare(AHeight: Single): TVtsSelection;
      function SetScrollByIndex(const Value: Integer): TVtsSelection;
      function SetScrollByValue(const Value: Integer): TVtsSelection;
      { public declarations }
    published
      { published declarations }
    end;

    TTxtSelection = class(TText)
    private
      FMessagingId: Integer;
      function GetValue: Integer;
      procedure SetValue(const Value: Integer);
      { private declarations }
    protected
      procedure PickerTimeChangedMessagingListener(const Sender: TObject; const M: TMessage);
      { protected declarations }
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;

      property Value: Integer read GetValue write SetValue;
      { public declarations }
    published
      { published declarations }
    end;

  var
    FVtsLeft: TVtsSelection;
    FVtsRight: TVtsSelection;
    { private declarations }
  protected
    procedure PickerTimeChangedMessagingListener(const Sender: TObject; const M: TMessage);
    { protected declarations }
  public
    constructor Create(AOwner: TDateTimePicker); override;
    destructor Destroy; override;

    class function GetInstance(AOwner: TDateTimePicker): TPickerTime; reintroduce;

    function Prepare(AParent: TControl): TPickerTime; reintroduce;
    procedure DoResize; override;
    function Show: TPickerTime; reintroduce;
    { public declarations }
  published
    { published declarations }
  end;

  TPickerYear = class(TPickerItem)
  private
    const
    CHeightDefault = 40;

    procedure ClickTxtYear(Sender: TObject);
    procedure TapTxtYear(Sender: TObject; const Point: TPointF);
    { private declarations }
  protected
    { protected declarations }
  public
    class function GetInstance(AOwner: TDateTimePicker): TPickerYear; reintroduce;

    function Prepare(AParent: TControl): TPickerYear; reintroduce;
    function Show: TPickerYear; reintroduce;
    { public declarations }
  published
    { published declarations }
  end;

  TPickerMonth = class(TPickerItem)
  private
    FDateTimeCurrent: TDateTime;
    FLytContents: TLayout;
    FLytButtonTop: TLayout;
    FBtnLeft: TSpeedButton;
    FBtnRight: TSpeedButton;

    FIsMonthTapped: Boolean;
    FPosXHrozScroll: Single;
    FPosXMouse: Single;
    FBackgroundHorzScroll: THorzScrollBox;
    FTouchTracking: TTouchTracking;

    FItem1: TPickerMonthItem;
    FItem2: TPickerMonthItem;
    FItem3: TPickerMonthItem;

    procedure OnClickBtnHeader(Sender: TObject);
    procedure OnTapBtnHeader(Sender: TObject; const Point: TPointF);

    procedure OnMouseDownBackgroundHorzScroll(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure OnMouseMoveBackgroundHorzScroll(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure OnMouseUpBackgroundHorzScroll(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    { private declarations }
  protected
    { protected declarations }
  public
    constructor Create(AOwner: TDateTimePicker); override;
    class function GetInstance(AOwner: TDateTimePicker): TPickerMonth; reintroduce;

    procedure DoResize; override;

    function Prepare(AParent: TControl): TPickerMonth; reintroduce;
    function Show: TPickerMonth; reintroduce;
    { public declarations }
  published
    { published declarations }
  end;

  TPickerMonthItem = class(TLayout)
  private
    FDateTime: TDateTime;
    FTxtHeader: TText;

    FLytBackground: TLayout;
    FGrdLytWeek: TGridLayout;
    FGrdLytDay: TGridLayout;
    { private declarations }
  protected
    { protected declarations }
  public
    class function GetInstance(AOwner: TDateTimePicker; AExtraName: string): TPickerMonthItem;

    constructor Create(AOwner: TDateTimePicker; AExtraName: string); reintroduce;

    procedure DoResize;

    function Prepare(AParent: TControl; ADateTime: TDateTime): TPickerMonthItem;
    { public declarations }
  published
    { published declarations }
  end;

  TMonthItemDay = class(TCircle)
  private
    FTxt: TText;
    FMonthItem: TPickerMonthItem;

    FDateTimeChangedIdMessaging: Integer;
    procedure DateTimeChangedMessagingListener(const Sender: TObject; const M: TMessage);

    function GetPropText: string;
    procedure SetPropText(const Value: string);
    function GetCurrentDate: TDate;

    procedure DoOnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure DoOnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    { private declarations }
  protected
    procedure OnClickItemDay(Sender: TObject);
    procedure OnTapItemDay(Sender: TObject; const Point: TPointF);
    { protected declarations }
  public
    class function GetInstance(AOwner: TDateTimePicker): TMonthItemDay;

    constructor Create(AOwner: TDateTimePicker); reintroduce;
    destructor Destroy; override;

    function SetText(AValue: string): TMonthItemDay;
    function CheckIsSelected: TMonthItemDay; overload;
    function CheckIsSelected(out AIsToday, AIsSelected: Boolean): TMonthItemDay; overload;
    function MonthItem(AValue: TPickerMonthItem): TMonthItemDay;

    property Text: string read GetPropText write SetPropText;
    property CurrentDate: TDate read GetCurrentDate;
    { public declarations }
  published
    { published declarations }
  end;

  TBackground = class(IdeaL.Lib.PopUp.TPopUpBackground)
  private
    const
    CMaxHeight = 400;
    CMaxWidth = 300;

  var
    FLytCenter: TLayout;
    FRctCenter: TRectangle;

    FLytHeader: TLayout;
    FRctHeader: TRectangle;
    FTxtYear: TText;
    FTxtAbstrc: TText;

    FLytButton: TLayout;
    FBtnCancel: TButton;
    FBtnOk: TButton;

    FItemShown: TPickerItem;

    FDateTimeChangedIdMessaging: Integer;

    procedure DateTimeChangedMessagingListener(const Sender: TObject; const M: TMessage);

    procedure PickerTimerCreate;

    procedure ClickBtnOk(Sender: TObject);
    procedure ClickTxtAbstrc(Sender: TObject);
    procedure ClickTxtYear(Sender: TObject);
    procedure TapBtnOk(Sender: TObject; const Point: TPointF);
    procedure TapTxtAbstrc(Sender: TObject; const Point: TPointF);
    procedure TapTxtYear(Sender: TObject; const Point: TPointF);
    { private declarations }
  protected
    { protected declarations }
  public
    constructor Create(AOwner: TDateTimePicker); reintroduce;
    destructor Destroy; override;

    procedure Show(AType: TPickerType); reintroduce;

    procedure DoResize; override;
    { public declarations }
  published
    { published declarations }
  end;

  TDateTimePicker = class(IdeaL.Lib.PopUp.TPopUp)
  private
    FDateTime: TDateTime;

    FTextOk: string;
    FTextCancel: string;
    FBtnStyleLookup: string;

    FHeaderTextColor: TAlphaColor;
    FHeaderTextSettingsAbstrc: TTextSettings;
    FHeaderTextSettingsYear: TTextSettings;

    FDefaultBackgroundColor: TAlphaColor;

    FCallbackOk: TProc<TDateTime>;

    procedure SetDateTime(const Value: TDateTime);
    { private declarations }
  protected
    { protected declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    class function GetInstance(AParent: TFmxObject): TDateTimePicker; reintroduce;

    function Text(AOk, ACancel: string): TDateTimePicker;
    /// <summary> Configure here your button style: Color, Font, FontSize, Bold, etc
    /// </summary>
    function BtnStyleLookup(AValue: string): TDateTimePicker;
    function HeaderTextColor(AValue: TAlphaColor): TDateTimePicker;
    function HeaderTextSettingsAbstrc(AValue: TTextSettings): TDateTimePicker;
    function HeaderTextSettingsYear(AValue: TTextSettings): TDateTimePicker;
    /// <summary> Used to color the Header, the selected day, the current day
    /// </summary>
    function DefaultBackgroundColor(AValue: TAlphaColor): TDateTimePicker;
    function Prepare: TDateTimePicker; reintroduce;
    function CallbackOk(AValue: TProc<TDateTime>): TDateTimePicker;
    /// <summary> Always the last one to call
    /// </summary>
    function Show(ADateTime: TDateTime; AType: TPickerType): TDateTimePicker; reintroduce;

    property DateTime: TDateTime read FDateTime write SetDateTime;
    { public declarations }
  published
    { published declarations }
  end;

implementation

uses
  IdeaL.Lib.View.Utils,
  IdeaL.Lib.PathSvg;


{ TDateTimePicker }

function TDateTimePicker.BtnStyleLookup(AValue: string): TDateTimePicker;
begin
  Result := Self;

  FBtnStyleLookup := AValue;
end;

function TDateTimePicker.CallbackOk(AValue: TProc<TDateTime>): TDateTimePicker;
begin
  Result := Self;

  FCallbackOk := AValue;
end;

constructor TDateTimePicker.Create(AOwner: TComponent);
begin
  inherited;
  FTextOk := 'OK';
  FTextCancel := 'Cancel';
  FBtnStyleLookup := EmptyStr;
  FHeaderTextColor := TAlphaColorRec.White;
  FDefaultBackgroundColor := TAlphaColorRec.Gray;
  FCallbackOk := nil;
  FDateTime := Now;

  FHeaderTextSettingsAbstrc := TTextSettings.Create(nil);
  FHeaderTextSettingsAbstrc.Font.Size := 20;
  FHeaderTextSettingsAbstrc.HorzAlign := TTextAlign.Leading;
  FHeaderTextSettingsAbstrc.FontColor := FHeaderTextColor;

  FHeaderTextSettingsYear := TTextSettings.Create(nil);
  FHeaderTextSettingsYear.Font.Size := 14;
  FHeaderTextSettingsYear.HorzAlign := TTextAlign.Leading;
  FHeaderTextSettingsYear.FontColor := FHeaderTextColor;
end;

destructor TDateTimePicker.Destroy;
begin
  if Assigned(FHeaderTextSettingsAbstrc) then
    FreeAndNil(FHeaderTextSettingsAbstrc);

  if Assigned(FHeaderTextSettingsYear) then
    FreeAndNil(FHeaderTextSettingsYear);

  inherited;
end;

class function TDateTimePicker.GetInstance(
  AParent: TFmxObject): TDateTimePicker;
begin
  Result := inherited GetInstance(AParent) as TDateTimePicker;
end;

function TDateTimePicker.DefaultBackgroundColor(
  AValue: TAlphaColor): TDateTimePicker;
begin
  Result := Self;

  FDefaultBackgroundColor := AValue;
end;

function TDateTimePicker.HeaderTextColor(AValue: TAlphaColor): TDateTimePicker;
begin
  Result := Self;
  FHeaderTextColor := AValue;
end;

function TDateTimePicker.HeaderTextSettingsAbstrc(
  AValue: TTextSettings): TDateTimePicker;
begin
  Result := Self;

  FreeAndNil(FHeaderTextSettingsAbstrc);
  FHeaderTextSettingsAbstrc := AValue;
end;

function TDateTimePicker.HeaderTextSettingsYear(
  AValue: TTextSettings): TDateTimePicker;
begin
  Result := Self;

  FreeAndNil(FHeaderTextSettingsYear);
  FHeaderTextSettingsYear := AValue;
end;

function TDateTimePicker.Prepare: TDateTimePicker;
begin
  Background := TBackground.Create(Self);
  Result := (inherited Prepare as TDateTimePicker);
end;

procedure TDateTimePicker.SetDateTime(const Value: TDateTime);
begin
  FDateTime := Value;

  TMessagingDateTimeChanged.SendAction(Self, FDateTime);
end;

function TDateTimePicker.Show(ADateTime: TDateTime; AType: TPickerType): TDateTimePicker;
begin
  DateTime := ADateTime;

  TBackground(Background).Show(AType);

  Result := (inherited Show as TDateTimePicker);
end;

function TDateTimePicker.Text(AOk, ACancel: string): TDateTimePicker;
begin
  Result := Self;
  FTextOk := AOk;
  FTextCancel := ACancel;
end;

{ TBackground }

procedure TBackground.ClickBtnOk(Sender: TObject);
begin
  if Assigned(TDateTimePicker(Self.Owner).FCallbackOk) then
    TDateTimePicker(Self.Owner).FCallbackOk(TDateTimePicker(Owner).FDateTime);

  if Assigned(FBtnCancel) and
    not(Trim(FBtnCancel.Name).IsEmpty) and
    (Assigned(FBtnCancel.{$IFDEF MSWINDOWS}OnClick{$ELSE}OnTap{$ENDIF}))
  then
    FBtnCancel.{$IFDEF MSWINDOWS}OnClick{$ELSE}OnTap{$ENDIF}(Sender{$IFNDEF MSWINDOWS}, TPointF.Create(0, 0){$ENDIF});
end;

procedure TBackground.ClickTxtAbstrc(Sender: TObject);
begin
  if Assigned(FItemShown) and not(Trim(FItemShown.Name).IsEmpty) then
  begin
    if FItemShown.InheritsFrom(TPickerMonth) then
      Exit
    else
      FreeAndNil(FItemShown);
  end;

  FItemShown :=
    TPickerMonth.GetInstance(TDateTimePicker(Owner))
    .Prepare(FRctCenter)
    .Show;
end;

procedure TBackground.ClickTxtYear(Sender: TObject);
begin
  if Assigned(FItemShown) and not(Trim(FItemShown.Name).IsEmpty) then
  begin
    if FItemShown.InheritsFrom(TPickerYear) then
      Exit
    else
      FreeAndNil(FItemShown);
  end;

  FItemShown :=
    TPickerYear.GetInstance(TDateTimePicker(Owner))
    .Prepare(FRctCenter)
    .Show;
end;

constructor TBackground.Create(AOwner: TDateTimePicker);
begin
  inherited Create(AOwner);

  FLytCenter := TLayout.Create(AOwner);
  FLytCenter.Name := FLytCenter.ClassName + 'Center' + TagString;
  FLytCenter.TabStop := False;
  FLytCenter.Parent := Self;
  FLytCenter.Align := TAlignLayout.Center;
  FLytCenter.Height := CMaxHeight;
  FLytCenter.Width := CMaxWidth;
  FLytCenter.BringToFront;

  FRctCenter := TRectangle.Create(AOwner);
  FRctCenter.Name := FRctCenter.ClassName + 'Center' + TagString;
  FRctCenter.TabStop := False;
  FRctCenter.Parent := FLytCenter;
  FRctCenter.Align := TAlignLayout.Contents;
  FRctCenter.Stroke.Kind := TBrushKind.None;
  FRctCenter.Fill.Color := TAlphaColorRec.White;
  FRctCenter.XRadius := 20;
  FRctCenter.YRadius := 20;
  FRctCenter.ClipChildren := True;

  FLytHeader := TLayout.Create(AOwner);
  FLytHeader.Name := FLytHeader.ClassName + 'Header' + TagString;
  FLytHeader.TabStop := False;
  FLytHeader.Parent := FRctCenter;
  FLytHeader.Align := TAlignLayout.Top;
  FLytHeader.Height := 70;
  FLytHeader.ClipChildren := True;

  FRctHeader := TRectangle.Create(AOwner);
  FRctHeader.Name := FRctHeader.ClassName + 'Header' + TagString;
  FRctHeader.TabStop := False;
  FRctHeader.Parent := FLytHeader;
  FRctHeader.Align := TAlignLayout.Top;
  FRctHeader.Stroke.Color := AOwner.FDefaultBackgroundColor;
  FRctHeader.Fill.Color := AOwner.FDefaultBackgroundColor;
  FRctHeader.Height := FLytHeader.Height + 20;
  FRctHeader.XRadius := 20;
  FRctHeader.YRadius := 20;
  FRctHeader.Padding.Bottom := 10;
  FRctHeader.Padding.Left := 20;
  FRctHeader.Padding.Right := 20;
  FRctHeader.Padding.Top := 10;

  var
  LLytYear := TLayout.Create(AOwner);
  LLytYear.Name := LLytYear.ClassName + 'HeaderYear' + TagString;
  LLytYear.Parent := FRctHeader;
  LLytYear.HitTest := True;
  LLytYear.TabStop := False;
  LLytYear.Align := TAlignLayout.MostTop;

  FTxtYear := TText.Create(AOwner);
  FTxtYear.Name := FTxtYear.ClassName + 'HeaderYear' + TagString;
  FTxtYear.HitTest := False;
  FTxtYear.TabStop := False;
  FTxtYear.Parent := LLytYear;
  FTxtYear.Align := TAlignLayout.MostTop;
  FTxtYear.AutoSize := True;
  FTxtYear.WordWrap := False;
  FTxtYear.TextSettings := AOwner.FHeaderTextSettingsYear;

  var
  LLytAbstrc := TLayout.Create(AOwner);
  LLytAbstrc.Name := LLytAbstrc.ClassName + 'HeaderAbstrc' + TagString;
  LLytAbstrc.Parent := FRctHeader;
  LLytAbstrc.HitTest := True;
  LLytAbstrc.TabStop := False;
  LLytAbstrc.Align := TAlignLayout.Top;

  FTxtAbstrc := TText.Create(AOwner);
  FTxtAbstrc.Name := FTxtAbstrc.ClassName + 'HeaderAbstrc' + TagString;
  FTxtAbstrc.HitTest := False;
  FTxtAbstrc.TabStop := False;
  FTxtAbstrc.Parent := LLytAbstrc;
  FTxtAbstrc.Align := TAlignLayout.Top;
  FTxtAbstrc.AutoSize := True;
  FTxtAbstrc.WordWrap := False;
  FTxtAbstrc.TextSettings := AOwner.FHeaderTextSettingsAbstrc;

  FLytButton := TLayout.Create(AOwner);
  FLytButton.Name := FLytButton.ClassName + 'Button' + TagString;
  FLytButton.TabStop := False;
  FLytButton.Parent := FRctCenter;
  FLytButton.Align := TAlignLayout.Bottom;
  FLytButton.ClipChildren := True;
  FLytButton.Margins.Bottom := FRctCenter.YRadius;

  FBtnCancel := TButton.Create(AOwner);
  FBtnCancel.Name := FBtnCancel.ClassName + 'Cancel' + TagString;
  FBtnCancel.TabStop := False;
  FBtnCancel.CanFocus := False;
  FBtnCancel.Parent := FLytButton;
  FBtnCancel.Align := TAlignLayout.Left;
  FBtnCancel.Text := AOwner.FTextCancel;
  FBtnCancel.StyleLookup := AOwner.FBtnStyleLookup;

  FBtnOk := TButton.Create(AOwner);
  FBtnOk.Name := FBtnOk.ClassName + 'OK' + TagString;
  FBtnOk.TabStop := False;
  FBtnOk.CanFocus := False;
  FBtnOk.Parent := FLytButton;
  FBtnOk.Align := TAlignLayout.Client;
  FBtnOk.Text := AOwner.FTextOk;
  FBtnOk.StyleLookup := AOwner.FBtnStyleLookup;

  FItemShown := nil;

{$IFDEF MSWINDOWS}
  LLytYear.OnClick := ClickTxtYear;
  LLytAbstrc.OnClick := ClickTxtAbstrc;
  FBtnCancel.OnClick := ClickRct;
  FBtnOk.OnClick := ClickBtnOk;
{$ELSE}
  LLytYear.OnTap := TapTxtYear;
  LLytAbstrc.OnTap := TapTxtAbstrc;
  FBtnCancel.OnTap := TapRct;
  FBtnOk.OnTap := TapBtnOk;
{$ENDIF}
  FDateTimeChangedIdMessaging := TMessagingDateTimeChanged.Subscribe(DateTimeChangedMessagingListener);
end;

procedure TBackground.DateTimeChangedMessagingListener(const Sender: TObject;
  const M: TMessage);
var
  LReference: string;
  LM: TMessagingDateTimeChanged;
begin
  if Sender = Owner then
  begin
    if M.InheritsFrom(TMessagingDateTimeChanged) then
    begin
      LM := (M as TMessagingDateTimeChanged);

      if TControl(FTxtYear.Parent).Visible then
      begin
        if Assigned(FTxtYear) and not(Trim(FTxtYear.Name).IsEmpty) then
          FTxtYear.Text := FormatDateTime('yyyy', TDateTimePicker(Owner).FDateTime);
      end;

      if TControl(FTxtAbstrc.Parent).Visible then
      begin
        if
          Assigned(FItemShown) and
          not(Trim(FItemShown.Name).IsEmpty)
          and FItemShown.InheritsFrom(TPickerTime)
        then
        begin
          FTxtAbstrc.Text := FormatDateTime('hh:nn', TDateTimePicker(Owner).FDateTime);
        end
        else
          if Assigned(FTxtAbstrc) and not(Trim(FTxtAbstrc.Name).IsEmpty) then
        begin
          FTxtAbstrc.Text := FormatDateTime('ddd, mmm dd', TDateTimePicker(Owner).FDateTime);
          FTxtAbstrc.Text := TUtils.FirstLetterUpperCase(FTxtAbstrc.Text);
        end;

        if Assigned(FItemShown) and not(Trim(FItemShown.Name).IsEmpty) and FItemShown.InheritsFrom(TPickerYear) then
        begin
          ClickTxtAbstrc(FTxtAbstrc);
        end;
      end;
    end;
  end;
end;

destructor TBackground.Destroy;
begin
  if FDateTimeChangedIdMessaging <> -1 then
  begin
    TMessagingDateTimeChanged.Unsubscribe(FDateTimeChangedIdMessaging);
    FDateTimeChangedIdMessaging := -1;
  end;

  inherited;
end;

procedure TBackground.DoResize;
var
  LHeight: Single;
  LWidth: Single;
begin
  inherited;

  LHeight := CMaxHeight;
  LWidth := CMaxWidth;

  if (Self.Height * 0.8) < LHeight then
    LHeight := Self.Height * 0.8;
  if (Self.Width * 0.8) < LWidth then
    LWidth := Self.Width * 0.8;
  FLytCenter.Height := LHeight;
  FLytCenter.Width := LWidth;

  FBtnCancel.Width := Trunc(FLytButton.Width / 2);

  TControl(FTxtYear.Parent).Height := FTxtYear.Height + 1;
  TControl(FTxtAbstrc.Parent).Height := FTxtAbstrc.Height + 1;

  if Assigned(FItemShown) then
    FItemShown.DoResize;

  if Tag <> 0 then
    Exit;
  Tag := 1;
  TDateTimePicker(Owner).DateTime := TDateTimePicker(Owner).DateTime;
end;

procedure TBackground.TapBtnOk(Sender: TObject; const Point: TPointF);
begin
  ClickBtnOk(Sender);
end;

procedure TBackground.TapTxtAbstrc(Sender: TObject; const Point: TPointF);
begin
  ClickTxtAbstrc(Sender);
end;

procedure TBackground.TapTxtYear(Sender: TObject; const Point: TPointF);
begin
  ClickTxtYear(Sender);
end;

procedure TBackground.PickerTimerCreate;
begin
  if Assigned(FItemShown) and not(Trim(FItemShown.Name).IsEmpty) then
  begin
    if FItemShown.InheritsFrom(TPickerTime) then
      Exit
    else
      FreeAndNil(FItemShown);
  end;

  FItemShown :=
    TPickerTime.GetInstance(TDateTimePicker(Owner))
    .Prepare(FRctCenter);
  TPickerTime(FItemShown).Show;
end;

procedure TBackground.Show(AType: TPickerType);
begin
  case AType of
    ptDate:
      ClickTxtAbstrc(Self);
    ptTime:
      begin
        TControl(FTxtYear.Parent).Visible := False;
        PickerTimerCreate;
        FTxtAbstrc.TextSettings.HorzAlign := TTextAlign.Center;
        FTxtAbstrc.TextSettings.Font.Size := 40;
        TControl(FTxtAbstrc.Parent).OnClick := nil;
        TControl(FTxtAbstrc.Parent).OnTap := nil;
      end;
  end;

  inherited Show;
end;

{ TPickerYear }

procedure TPickerYear.ClickTxtYear(Sender: TObject);
var
  LDateTime: TDateTime;
  LDifYear: Integer;
begin
  LDateTime := TDateTimePicker(Owner).DateTime;

  LDifYear := TText(Sender).Tag - YearOf(LDateTime);
  LDateTime := IncYear(LDateTime, LDifYear);

  TDateTimePicker(Owner).DateTime := LDateTime;
end;

class function TPickerYear.GetInstance(
  AOwner: TDateTimePicker): TPickerYear;
begin
  Result := TPickerYear.Create(AOwner);
end;

function TPickerYear.Prepare(AParent: TControl): TPickerYear;
var
  LCurrentYear: Integer;
begin
  Result := inherited Prepare(AParent) as TPickerYear;

  LCurrentYear := YearOf(Now);

  for var i := LCurrentYear - 100 to LCurrentYear + 100 do
  begin
    var
    LTxt := TText.Create(Owner);
    LTxt.TagString := TDateTimePicker(Owner).TagString;
    LTxt.Name := LTxt.ClassName + Self.ClassName + i.ToString + TagString;
    LTxt.Align := TAlignLayout.Top;
    LTxt.Height := CHeightDefault;
    LTxt.Tag := i;
    LTxt.Text := LTxt.Tag.ToString;
{$IFDEF MSWINDOWS}
    LTxt.OnClick := ClickTxtYear;
{$ELSE}
    LTxt.OnTap := TapTxtYear;
{$ENDIF}
    AddObject(LTxt);
    LTxt.Position.Y := LTxt.Height * Content.ChildrenCount;
  end;
end;

function TPickerYear.Show: TPickerYear;
begin
  Result := inherited Show as TPickerYear;

  var
  LYearCurrent := YearOf(Now);
  var
  LYearSent := YearOf(TDateTimePicker(Owner).FDateTime);
  var
  LDif := LYearSent - LYearCurrent;
  var
  LIndex := 100 + LDif;
  var
  LPos := (LIndex - 2) * CHeightDefault;

  TText(Content.Children[LIndex]).TextSettings.FontColor := TDateTimePicker(Owner).FDefaultBackgroundColor;
  TText(Content.Children[LIndex]).TextSettings.Font.Style := [TFontStyle.fsBold];

  ViewportPosition := TPointF.Create(0, LPos);
end;

procedure TPickerYear.TapTxtYear(Sender: TObject; const Point: TPointF);
begin
  ClickTxtYear(Sender);
end;

{ TMessagingDateTimeChanged }

constructor TMessagingDateTimeChanged.Create(ADateTime: TDateTime);
begin
  FDateTime := ADateTime;
end;

class procedure TMessagingDateTimeChanged.SendAction(Sender: TObject; ADateTime: TDateTime);
begin
  TMessageManager.DefaultManager.SendMessage(Sender, Self.Create(ADateTime));
end;

{ TMessagingAction }

class function TMessagingAction.Subscribe(
  const AListenerMethod: TMessageListenerMethod): Integer;
begin
  Result := TMessageManager.DefaultManager.SubscribeToMessage(Self, AListenerMethod);
end;

class procedure TMessagingAction.Unsubscribe(AId: Integer; AImmediate: Boolean);
begin
  TMessageManager.DefaultManager.Unsubscribe(Self, AId, AImmediate);
end;

{ TMessagingPickerTimeChanged }

constructor TMessagingPickerTimeChanged.Create(AReference: string);
begin
  FReference := AReference;
end;

class procedure TMessagingPickerTimeChanged.SendAction(Sender: TObject;
  AReference: string);
begin
  TMessageManager.DefaultManager.SendMessage(Sender, Self.Create(AReference));
end;

{ TPickerItem }

constructor TPickerItem.Create(AOwner: TDateTimePicker);
begin
  inherited Create(AOwner);
  Visible := False;
  TagString := AOwner.TagString;
  Name := Self.ClassName + TagString;
  TabStop := False;
  Align := TAlignLayout.Client;
  ShowScrollBars := False;
  Margins.Left := 10;
  Margins.Right := 10;
end;

procedure TPickerItem.DoResize;
begin
  // Do on the inherited object
end;

function TPickerItem.Prepare(AParent: TControl): TPickerItem;
begin
  Result := Self;

  Parent := AParent;
end;

function TPickerItem.Show: TPickerItem;
begin
  Result := Self;

  Visible := True;
end;

{ TPickerMonth }

constructor TPickerMonth.Create(AOwner: TDateTimePicker);

  procedure CreatePath(AParent: TSpeedButton; AName, APthData: string);
  begin
    var
    LPth := FMX.Objects.TPath.Create(AOwner);
    LPth.Name := LPth.ClassName + 'Btn' + AName + TagString;
    LPth.TabStop := False;
    LPth.HitTest := False;
    LPth.Align := TAlignLayout.Center;
    LPth.Height := 12;
    LPth.Width := 7;
    LPth.Stroke.Kind := TBrushKind.None;
    LPth.Fill.Color := TAlphaColorRec.Black;
    LPth.Data.Data := APthData;
    LPth.Parent := AParent;
  end;

begin
  inherited;
  AniCalculations.TouchTracking := [];

  FLytContents := TLayout.Create(AOwner);
  FLytContents.Name := FLytContents.ClassName + 'Contents' + TagString;
  FLytContents.TabStop := False;
  FLytContents.Parent := Self;
  FLytContents.Align := TAlignLayout.Contents;

  FLytButtonTop := TLayout.Create(AOwner);
  FLytButtonTop.Name := FLytButtonTop.ClassName + 'ButtonTop' + TagString;
  FLytButtonTop.TabStop := False;
  FLytButtonTop.Parent := FLytContents;
  FLytButtonTop.Align := TAlignLayout.Top;

  FBtnLeft := TSpeedButton.Create(AOwner);
  FBtnLeft.Name := FBtnLeft.ClassName + 'BtnLeft' + TagString;
  FBtnLeft.TabStop := False;
  FBtnLeft.Parent := FLytButtonTop;
  FBtnLeft.Align := TAlignLayout.Left;
  FBtnLeft.Width := 50;
  FBtnLeft.Text := ' ';
  CreatePath(FBtnLeft, 'Left', C_PATH_ARROW_SINGLE_LEFT);

  FBtnRight := TSpeedButton.Create(AOwner);
  FBtnRight.Name := FBtnRight.ClassName + 'BtnRight' + TagString;
  FBtnRight.TabStop := False;
  FBtnRight.Parent := FLytButtonTop;
  FBtnRight.Align := TAlignLayout.Right;
  FBtnRight.Width := 50;
  FBtnRight.Text := ' ';
  CreatePath(FBtnRight, 'Right',C_PATH_ARROW_SINGLE_RIGHT);

{$IFDEF MSWINDOWS}
  FBtnLeft.OnClick := OnClickBtnHeader;
  FBtnRight.OnClick := OnClickBtnHeader;
{$ELSE}
  FBtnLeft.OnTap := OnTapBtnHeader;
  FBtnRight.OnTap := OnTapBtnHeader;
{$ENDIF}
  FIsMonthTapped := False;

  FBackgroundHorzScroll := THorzScrollBox.Create(AOwner);
  FBackgroundHorzScroll.Name := FBackgroundHorzScroll.ClassName + 'BackgroundHorzScroll' + TagString;
  FBackgroundHorzScroll.TabStop := False;
  FBackgroundHorzScroll.Parent := Self;
  FBackgroundHorzScroll.Align := TAlignLayout.Client;
  // FBackgroundHorzScroll.OnMouseUp := OnMouseUpBackgroundHorzScroll;
  // FBackgroundHorzScroll.OnMouseDown := OnMouseDownBackgroundHorzScroll;
  FBackgroundHorzScroll.ShowScrollBars := False;
  FTouchTracking := FBackgroundHorzScroll.AniCalculations.TouchTracking;
  FBackgroundHorzScroll.AniCalculations.TouchTracking := [];
  FBackgroundHorzScroll.HitTest := False;
{$IFDEF MSWINDOWS}
  // FBackgroundHorzScroll.OnMouseMove := OnMouseMoveBackgroundHorzScroll;
{$ENDIF}
  FItem1 := nil;
  FItem2 := nil;
  FItem3 := nil;
end;

procedure TPickerMonth.DoResize;
var
  LHeight: Single;
  LWidth: Single;
begin
  inherited;

  if FIsMonthTapped then
    Exit;

  FItem1.DoResize;
  FItem2.DoResize;
  FItem3.DoResize;

  FBackgroundHorzScroll.ViewportPosition := TPointF.Create(FItem2.Width, 0);
end;

class function TPickerMonth.GetInstance(AOwner: TDateTimePicker): TPickerMonth;
begin
  Result := TPickerMonth.Create(AOwner);
end;

procedure TPickerMonth.OnClickBtnHeader(Sender: TObject);
var
  LMultiplier: Integer;
  LIndex: Integer;
begin
  LIndex := 1;
  LMultiplier := 1;
  if Sender = FBtnLeft then
    LMultiplier := -1;

  FBackgroundHorzScroll.ViewportPosition := TPointF.Create(FItem1.Width * (LIndex + LMultiplier), 0);

  OnMouseUpBackgroundHorzScroll(
    Self,
    TMouseButton.mbLeft,
    [],
    FItem1.Width * (LIndex + LMultiplier),
    0);
end;

procedure TPickerMonth.OnMouseDownBackgroundHorzScroll(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  FBackgroundHorzScroll.AniCalculations.TouchTracking := FTouchTracking;
  FIsMonthTapped := True;
  FPosXHrozScroll := FBackgroundHorzScroll.ViewportPosition.X;
  FPosXMouse := X;
end;

procedure TPickerMonth.OnMouseMoveBackgroundHorzScroll(Sender: TObject;
  Shift: TShiftState; X, Y: Single);
var
  LScale: Double;
  LX: Single;
begin
  if not FIsMonthTapped then
    Exit;

  LScale := 1;
  if Scene <> nil then
    LScale := Scene.GetSceneScale;

  LX := ((FPosXMouse - X) / 2);

  FBackgroundHorzScroll.ViewportPosition := TPointF.Create(
    FBackgroundHorzScroll.ViewportPosition.X + LX
    , 0);
end;

procedure TPickerMonth.OnMouseUpBackgroundHorzScroll(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  LX: Single;
  LIndex: Integer;
begin
  FIsMonthTapped := False;
  LX := FBackgroundHorzScroll.ViewportPosition.X;

  LIndex := Trunc(RoundTo(LX / FItem1.Width, 0));
  FBackgroundHorzScroll.ViewportPosition := TPointF.Create((LIndex * FItem1.Width), 0);

  FDateTimeCurrent := IncMonth(FDateTimeCurrent, LIndex - 1);
  FBackgroundHorzScroll.AniCalculations.TouchTracking := [];
  try
    FBackgroundHorzScroll.Content.BeginUpdate;
    FBackgroundHorzScroll.BeginUpdate;
    try
      FItem1.Prepare(FBackgroundHorzScroll, IncMonth(FDateTimeCurrent, -1));
      FItem2.Prepare(FBackgroundHorzScroll, IncMonth(FDateTimeCurrent, 0));
      FItem3.Prepare(FBackgroundHorzScroll, IncMonth(FDateTimeCurrent, 1));

      FBackgroundHorzScroll.ViewportPosition := TPointF.Create(FItem2.Width, 0);
    finally
      FBackgroundHorzScroll.Content.EndUpdate;
      FBackgroundHorzScroll.EndUpdate;
    end;
  finally
    // FBackgroundHorzScroll.AniCalculations.TouchTracking := FTouchTracking;
  end;
end;

procedure TPickerMonth.OnTapBtnHeader(Sender: TObject; const Point: TPointF);
begin
  OnClickBtnHeader(Sender);
end;

function TPickerMonth.Prepare(AParent: TControl): TPickerMonth;
begin
  Result := inherited Prepare(AParent) as TPickerMonth;

  FDateTimeCurrent := TDateTimePicker(Owner).DateTime;

  FItem1 :=
    TPickerMonthItem.GetInstance(TDateTimePicker(Owner), '1')
    .Prepare(FBackgroundHorzScroll, IncMonth(FDateTimeCurrent, -1));

  FItem2 :=
    TPickerMonthItem.GetInstance(TDateTimePicker(Owner), '2')
    .Prepare(FBackgroundHorzScroll, IncMonth(FDateTimeCurrent, 0));

  FItem3 :=
    TPickerMonthItem.GetInstance(TDateTimePicker(Owner), '3')
    .Prepare(FBackgroundHorzScroll, IncMonth(FDateTimeCurrent, 1));

  FLytContents.BringToFront;
end;

function TPickerMonth.Show: TPickerMonth;
begin
  Result := inherited Show as TPickerMonth;

  FBackgroundHorzScroll.ViewportPosition := TPointF.Create(FItem2.Width, 0);
end;

{ TPickerMonthItem }

constructor TPickerMonthItem.Create(AOwner: TDateTimePicker; AExtraName: string);
var
  LName: string;

  procedure AddTxt(AParent: TGridLayout; ATxt: string);
  begin
    var
    LTxt := TText.Create(AOwner);
    AParent.AddObject(LTxt);
    LTxt.Text := ATxt;
    LTxt.TabStop := False;
    LTxt.HitTest := False;
  end;

  procedure AddItemDay(AParent: TGridLayout; ATxt: string);
  begin
    var
    LTxt :=
      TMonthItemDay.GetInstance(AOwner)
      .SetText(ATxt)
      .MonthItem(Self);
    ;
    AParent.AddObject(LTxt);
    LTxt.TabStop := False;
  end;

begin
  inherited Create(AOwner);

  TagString := TControl(AOwner).TagString;
  LName := Self.ClassName + AExtraName;
  Name := LName + TagString;
  TabStop := False;
  Align := TAlignLayout.Left;
  Width := 280;

  FTxtHeader := TText.Create(AOwner);
  FTxtHeader.Name := FTxtHeader.ClassName + LName + 'TxtHeader' + TagString;
  FTxtHeader.Parent := Self;
  FTxtHeader.Align := TAlignLayout.MostTop;
  FTxtHeader.HorzTextAlign := TTextAlign.Center;
  FTxtHeader.TabStop := False;
  FTxtHeader.HitTest := False;

  FLytBackground := TLayout.Create(AOwner);
  FLytBackground.Name := FLytBackground.ClassName + LName + 'LytBackground' + TagString;
  FLytBackground.Parent := Self;
  FLytBackground.Align := TAlignLayout.Client;
  FLytBackground.TabStop := False;
  FLytBackground.HitTest := False;

  FGrdLytWeek := TGridLayout.Create(AOwner);
  FGrdLytWeek.Name := FGrdLytWeek.ClassName + LName + 'GrdLytWeek' + TagString;
  FGrdLytWeek.Parent := FLytBackground;
  FGrdLytWeek.Align := TAlignLayout.Top;
  FGrdLytWeek.TabStop := False;
  FGrdLytWeek.HitTest := False;

  for var i := 1 to 7 do
  begin
    AddTxt(FGrdLytWeek, TUtils.FirstLetterUpperCase(Copy(FormatSettings.ShortDayNames[i], 1, 1)));
  end;

  FGrdLytDay := TGridLayout.Create(AOwner);
  FGrdLytDay.Name := FGrdLytDay.ClassName + LName + 'GrdLytDay' + TagString;
  FGrdLytDay.Parent := FLytBackground;
  FGrdLytDay.Align := TAlignLayout.Top;
  FGrdLytDay.TabStop := False;
  FGrdLytDay.HitTest := False;

  for var i := 1 to 6 do
  begin
    for var j := 1 to 7 do
    begin
      AddItemDay(FGrdLytDay, ' ');
    end;
  end;

  { var
    LRct := TRectangle.Create(AOwner);
    LRct.Parent := FLytBackground;
    LRct.Align := TAlignLayout.Contents;
    LRct.HitTest := False;
    LRct.SendToBack; }
end;

procedure TPickerMonthItem.DoResize;
var
  LItemHeight: Single;
  LItemWidth: Single;
begin
  Width := TControl(Parent).Width;

  LItemHeight := Trunc(FLytBackground.Height / 7);
  LItemWidth := Trunc(FLytBackground.Width / 7);

  FGrdLytWeek.ItemHeight := LItemHeight;
  FGrdLytWeek.ItemWidth := LItemWidth;
  FGrdLytWeek.Height := LItemHeight;

  FGrdLytDay.ItemHeight := LItemHeight;
  FGrdLytDay.ItemWidth := LItemWidth;
  FGrdLytDay.Height := LItemHeight * 6;
end;

class function TPickerMonthItem.GetInstance(
  AOwner: TDateTimePicker; AExtraName: string): TPickerMonthItem;
begin
  Result := TPickerMonthItem.Create(AOwner, AExtraName);
end;

function TPickerMonthItem.Prepare(AParent: TControl; ADateTime: TDateTime): TPickerMonthItem;
var
  LIndex: Integer;
  LStartMonth: TDateTime;
  LEndMonth: TDateTime;
  LStartDay: Integer;
  LEndDay: Integer;
  L1DayWeek: Integer;
begin
  Result := Self;
  Position.X := Width * THorzScrollBox(AParent).Content.ChildrenCount;
  AParent.AddObject(Self);

  FDateTime := ADateTime;

  FTxtHeader.Text :=
    TUtils.FirstLetterUpperCase(FormatDateTime('mmmm', ADateTime)) + ' ' +
    FormatDateTime('yyyy', ADateTime);

  LStartMonth := StartOfTheMonth(FDateTime);
  LEndMonth := EndOfTheMonth(FDateTime);
  LStartDay := DayOf(LStartMonth);
  LEndDay := DayOf(LEndMonth);

  L1DayWeek := System.SysUtils.DayOfWeek(LStartMonth);
  var
  j := 1;
  for var i := 0 to Pred(FGrdLytDay.Children.Count) do
  begin
    if (i >= Pred(L1DayWeek)) and (j >= LStartDay) and (j <= LEndDay) then
    begin
      TMonthItemDay(FGrdLytDay.Children[i])
        .SetText(j.ToString)
        .CheckIsSelected;
      Inc(j, 1);
    end
    else
      TMonthItemDay(FGrdLytDay.Children[i])
        .SetText(' ')
        .CheckIsSelected;;
  end;
end;

{ TMonthItemDay }

function TMonthItemDay.CheckIsSelected(out AIsToday, AIsSelected: Boolean): TMonthItemDay;
var
  LCurrentDate: Double;
begin
  Result := Self;

  AIsToday := False;
  AIsSelected := False;

  if not(Text.Trim.IsEmpty) and (Assigned(FMonthItem)) and not(Trim(FMonthItem.Name).IsEmpty) then
  begin
    LCurrentDate := Trunc(CurrentDate);

    AIsToday := LCurrentDate = Trunc(Now);
    AIsSelected := LCurrentDate = Trunc(TDateTimePicker(Owner).DateTime);;
  end;

  Stroke.Color := TAlphaColorRec.Null;
  Fill.Color := TAlphaColorRec.Null;

  if AIsSelected then
  begin
    Stroke.Color := TDateTimePicker(Owner).FDefaultBackgroundColor;
    Fill.Color := TDateTimePicker(Owner).FDefaultBackgroundColor;
    FTxt.TextSettings.FontColor := TDateTimePicker(Owner).FHeaderTextSettingsAbstrc.FontColor;
  end
  else if AIsToday then
  begin
    FTxt.TextSettings.FontColor := TDateTimePicker(Owner).FDefaultBackgroundColor;
  end
  else
  begin
    FTxt.TextSettings.FontColor := TAlphaColorRec.Black;
  end;
end;

function TMonthItemDay.CheckIsSelected: TMonthItemDay;
var
  LIsToday, LIsSelected: Boolean;
begin
  Result := Self;

  CheckIsSelected(LIsToday, LIsSelected);
end;

constructor TMonthItemDay.Create(AOwner: TDateTimePicker);
begin
  inherited Create(AOwner);
  TagString := AOwner.TagString;
  TabStop := False;
  HitTest := True;

  OnMouseDown := DoOnMouseDown;
  OnMouseUp := DoOnMouseUp;

  Stroke.Color := TAlphaColorRec.Null;
  Fill.Color := TAlphaColorRec.Null;

  FTxt := TText.Create(AOwner);
  FTxt.HitTest := False;
  FTxt.TabStop := False;
  FTxt.Parent := Self;
  FTxt.Align := TAlignLayout.Center;
  FTxt.AutoSize := True;
  FTxt.WordWrap := False;

{$IFDEF MSWINDOWS}
  Self.OnClick := OnClickItemDay;
{$ELSE}
  Self.OnTap := OnTapItemDay;
{$ENDIF}
  FDateTimeChangedIdMessaging := TMessagingDateTimeChanged.Subscribe(DateTimeChangedMessagingListener);
end;

procedure TMonthItemDay.DateTimeChangedMessagingListener(const Sender: TObject;
  const M: TMessage);
var
  LReference: string;
  LM: TMessagingDateTimeChanged;
begin
  if (Sender = Owner) then
  begin
    if M.InheritsFrom(TMessagingDateTimeChanged) then
    begin
      CheckIsSelected;
    end;
  end;
end;

destructor TMonthItemDay.Destroy;
begin
  if FDateTimeChangedIdMessaging <> -1 then
    TMessagingDateTimeChanged.Unsubscribe(FDateTimeChangedIdMessaging);
  FDateTimeChangedIdMessaging := -1;
  inherited;
end;

function TMonthItemDay.GetCurrentDate: TDate;
var
  LDay: Integer;
begin
  Result := 0;
  if (Assigned(FMonthItem)) and not(Trim(FMonthItem.Name).IsEmpty) and TryStrToInt(Text, LDay) then
  begin
    Result := RecodeDay(FMonthItem.FDateTime, LDay);
  end;
  Result := Result;
end;

class function TMonthItemDay.GetInstance(
  AOwner: TDateTimePicker): TMonthItemDay;
begin
  Result := TMonthItemDay.Create(AOwner);
end;

function TMonthItemDay.GetPropText: string;
begin
  Result := FTxt.Text;
end;

function TMonthItemDay.MonthItem(AValue: TPickerMonthItem): TMonthItemDay;
begin
  Result := Self;

  FMonthItem := AValue;
end;

procedure TMonthItemDay.OnClickItemDay(Sender: TObject);
begin
  if TryStrToInt(Text, TUtils.VarTryStrToInt) then
    TDateTimePicker(Owner).DateTime := CurrentDate;
end;

procedure TMonthItemDay.DoOnMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  {
    FMonthItem = TPickerMonthItem
    .Parent = TVertSrollBox
    .Parent = TPickerMonth
  }
  // TPickerMonth(FMonthItem.Parent.Parent).OnMouseDown(FMonthItem.Parent, Button, Shift, X, Y);
end;

procedure TMonthItemDay.DoOnMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  // TPickerMonth(FMonthItem.Parent.Parent).OnMouseUp(FMonthItem.Parent, Button, Shift, X, Y);
end;

procedure TMonthItemDay.OnTapItemDay(Sender: TObject; const Point: TPointF);
begin
  OnClickItemDay(Sender);
end;

function TMonthItemDay.SetText(AValue: string): TMonthItemDay;
begin
  Result := Self;

  Text := AValue;
end;

procedure TMonthItemDay.SetPropText(const Value: string);
begin
  FTxt.Text := Value;
end;

{ TPickerTime }

constructor TPickerTime.Create(AOwner: TDateTimePicker);
begin
  inherited;
  AniCalculations.TouchTracking := [];

  FLytCenter := TLayout.Create(AOwner);
  FLytCenter.Name := FLytCenter.ClassName + Self.ClassName + 'Center' + TagString;
  FLytCenter.TabStop := False;
  FLytCenter.HitTest := False;
  FLytCenter.Parent := Self;
  FLytCenter.Align := TAlignLayout.Center;
  FLytCenter.Width := 152;

  var
  LLytContents := TLayout.Create(AOwner);
  LLytContents.TabStop := False;
  LLytContents.HitTest := False;
  LLytContents.Parent := FLytCenter;
  LLytContents.Align := TAlignLayout.Contents;

  FRctCenter := TRectangle.Create(AOwner);
  FRctCenter.Name := FRctCenter.ClassName + Self.ClassName + 'Center' + TagString;
  FRctCenter.TabStop := False;
  FRctCenter.HitTest := False;
  FRctCenter.Parent := LLytContents;
  FRctCenter.Align := TAlignLayout.VertCenter;
  FRctCenter.Height := 40;
  FRctCenter.XRadius := 10;
  FRctCenter.YRadius := 10;
  FRctCenter.Fill.Kind := TBrushKind.None;
  FRctCenter.Stroke.Color := StringToAlphaColor('#FFE6E6E6');

  var
  LTxt := {$IFDEF SKIA}TLabel{$ELSE}TText{$ENDIF}.Create(AOwner);
  LTxt.HitTest := False;
  LTxt.Align := TAlignLayout.Center;
  LTxt.AutoSize := True;
  LTxt.Text := ':';
  {$IFDEF SKIA}
  LTxt.StyledSettings := LTxt.StyledSettings - [TStyledSetting.FontColor];
  {$ENDIF}
  LTxt.TextSettings.WordWrap := False;
  LTxt.TextSettings.FontColor := StringToAlphaColor('#FFE6E6E6');
  LTxt.TextSettings.Font.Size := 24;
  LTxt.Parent := FRctCenter;

  FVtsLeft := TVtsSelection.Create(AOwner);
  FVtsLeft.Parent := FLytCenter;
  FVtsLeft.Align := TAlignLayout.Left;

  FVtsRight := TVtsSelection.Create(AOwner);
  FVtsRight.Parent := FLytCenter;
  FVtsRight.Align := TAlignLayout.Client;

  LLytContents.BringToFront;

  FMessagingId := TMessagingPickerTimeChanged.Subscribe(PickerTimeChangedMessagingListener);
end;

destructor TPickerTime.Destroy;
begin
  if FMessagingId <> -1 then
  begin
    TMessagingPickerTimeChanged.Unsubscribe(FMessagingId);
    FMessagingId := -1;
  end;
  inherited;
end;

procedure TPickerTime.DoResize;
begin
  inherited;
  FLytCenter.Height := FRctCenter.Height * 3;
  if FLytCenter.Height > TControl(FLytCenter.Parent).Height then
    FLytCenter.Height := TControl(FLytCenter.Parent).Height;

  FVtsLeft.Width := Trunc(FLytCenter.Width / 2);
end;

class function TPickerTime.GetInstance(AOwner: TDateTimePicker): TPickerTime;
begin
  Result := TPickerTime.Create(AOwner);
end;

procedure TPickerTime.PickerTimeChangedMessagingListener(const Sender: TObject;
  const M: TMessage);
var
  LReference: string;
  LM: TMessagingPickerTimeChanged;
  LTag: Integer;
  LDateTime: TDateTime;
begin
  if Sender = Owner then
  begin
    if M.InheritsFrom(TMessagingPickerTimeChanged) then
    begin
      LM := (M as TMessagingPickerTimeChanged);

      if (LM.Reference.Equals('StoppedScrolling')) then
      begin
        LDateTime := TDateTimePicker(Owner).DateTime;
        LDateTime := RecodeHour(LDateTime, FVtsLeft.Value);
        LDateTime := RecodeMinute(LDateTime, FVtsRight.Value);
        TDateTimePicker(Owner).DateTime := LDateTime;
      end;
    end;
  end;
end;

function TPickerTime.Prepare(AParent: TControl): TPickerTime;
begin
  Result := inherited Prepare(AParent) as TPickerTime;

  Parent := AParent;

  FVtsLeft
    .Range(0, 23)
    .Prepare(FRctCenter.Height);
  FVtsRight
    .Range(0, 59)
    .Prepare(FRctCenter.Height);
end;

function TPickerTime.Show: TPickerTime;
begin
  Result := inherited Show as TPickerTime;

  FVtsLeft
    .SetScrollByValue(HourOf(TDateTimePicker(Owner).DateTime));
  FVtsRight
    .SetScrollByValue(MinuteOf(TDateTimePicker(Owner).DateTime));
end;

{ TPickerTime.TVtsSelection }

procedure TPickerTime.TVtsSelection.ChangetViewportPosition(APointF: TPointF);
begin
  TThread.Queue(nil,
    procedure
    begin
      ViewportPosition := APointF;
    end);
end;

constructor TPickerTime.TVtsSelection.Create(AOwner: TComponent);
begin
  inherited;
  TagString := TControl(AOwner).TagString;
  TabStop := False;
  ShowScrollBars := False;

  FCurrentPosition := TPointF.Create(0, 0);

  FTmrPosition := TTimer.Create(Self);
  FTmrPosition.Enabled := False;
{$IFDEF MSWINDOWS}
  FTmrPosition.Interval := 200;
{$ELSE}
  FTmrPosition.Interval := 100;
{$ENDIF}
  FTmrPosition.OnTimer := DoTimer;

  OnViewportPositionChange := DoViewportPositionChange;
end;

procedure TPickerTime.TVtsSelection.DoTimer(Sender: TObject);
var
  LPosInt: Integer;
  LPosDec: Double;
begin
  inherited;
  FTmrPosition.Enabled := False;
  OnViewportPositionChange := nil;
  try
    GetExtraPos(LPosInt, LPosDec);
    LPosInt := Trunc((LPosInt + LPosDec) * FSelectionHeight);
    // if FCurrentPosition.Y <> LPosInt then
    begin
      FCurrentPosition.Y := LPosInt;
      ChangetViewportPosition(FCurrentPosition);
      TMessagingPickerTimeChanged.SendAction(Self.Content, CurrentIndex.ToString);
      TMessagingPickerTimeChanged.SendAction(Owner, 'StoppedScrolling');
    end;
  finally
    OnViewportPositionChange := DoViewportPositionChange;
  end;
end;

procedure TPickerTime.TVtsSelection.DoViewportPositionChange(Sender: TObject;
const OldViewportPosition, NewViewportPosition: TPointF;
const ContentSizeChanged: Boolean);
begin
  FTmrPosition.Enabled := False;
  FCurrentPosition := NewViewportPosition;

  TMessagingPickerTimeChanged.SendAction(Self.Content, CurrentIndex.ToString);
  FTmrPosition.Enabled := True;
end;

function TPickerTime.TVtsSelection.GetCurrentIndex: Integer;
begin
  Result := Trunc(FCurrentPosition.Y / FSelectionHeight) + 1 + GetExtraPos;
end;

function TPickerTime.TVtsSelection.GetExtraPos: Integer;
var
  LInt: Integer;
  LDec: Double;
begin
  GetExtraPos(LInt, LDec);
  Result := Trunc(LDec);
end;

function TPickerTime.TVtsSelection.GetValue: Integer;
begin
  Result := 0;

  var
  LIndex := CurrentIndex;
  if Content.ChildrenCount > LIndex then
  begin
    var
    LLbl := Content.Children[LIndex] as TPickerTime.TTxtSelection;
    TryStrToInt(LLbl.Text, Result);
  end;
end;

procedure TPickerTime.TVtsSelection.GetExtraPos(out AInt: Integer; out ADec: Double);
begin
  ADec := FCurrentPosition.Y / FSelectionHeight;
  AInt := Trunc(ADec);
  ADec := ADec - AInt;
  if ADec <= 0.6 then
    ADec := 0
  else
    ADec := 1;
end;

function TPickerTime.TVtsSelection.Prepare(AHeight: Single): TVtsSelection;

  procedure AddTxt(AValue: Integer);
  begin
    var
    LTxt := TTxtSelection.Create(Owner);
    LTxt.Height := AHeight;
    LTxt.Tag := Content.ChildrenCount;
    AddObject(LTxt);
    LTxt.Position.Y := LTxt.Height * Content.ChildrenCount;
    LTxt.Value := AValue;
  end;

begin
  Result := Self;

  FSelectionHeight := AHeight;

  AddTxt(-1);
  for var i := FRangeStart to FRangeEnd do
  begin
    AddTxt(i);
  end;
  AddTxt(-1);
end;

function TPickerTime.TVtsSelection.Range(AStart, AEnd: Integer): TVtsSelection;
begin
  Result := Self;

  FRangeStart := AStart;
  FRangeEnd := AEnd;
end;

function TPickerTime.TVtsSelection.SetScrollByIndex(const Value: Integer): TVtsSelection;
begin
  Result := Self;

  ChangetViewportPosition(TPointF.Zero.Create(0, Pred(Value) * Trunc(FSelectionHeight)));
end;

function TPickerTime.TVtsSelection.SetScrollByValue(
  const Value: Integer): TVtsSelection;
var
  LValue: Integer;
begin
  Result := Self;
  for var i := 0 to Pred(Content.ChildrenCount) do
  begin
    var
    LLbl := Content.Children[i] as TPickerTime.TTxtSelection;
    if TryStrToInt(LLbl.Text, LValue) then
    begin
      if LLbl.Text.ToInteger = Value then
      begin
        SetScrollByIndex(i);
        Break;
      end;
    end;
  end;
end;

{ TPickerTime.TTxtSelection }

constructor TPickerTime.TTxtSelection.Create(AOwner: TComponent);
begin
  inherited;
  Align := TAlignLayout.Top;
  TextSettings.HorzAlign := TTextAlign.Center;
  TextSettings.FontColor := StringToAlphaColor('#FFE6E6E6');
  HitTest := False;
  TextSettings.Font.Size := 24;

  FMessagingId := TMessagingPickerTimeChanged.Subscribe(PickerTimeChangedMessagingListener);
end;

destructor TPickerTime.TTxtSelection.Destroy;
begin
  if FMessagingId <> -1 then
  begin
    TMessagingPickerTimeChanged.Unsubscribe(FMessagingId);
    FMessagingId := -1;
  end;
  inherited;
end;

function TPickerTime.TTxtSelection.GetValue: Integer;
begin
  Result := -1;
  TryStrToInt(Text, Result);
end;

procedure TPickerTime.TTxtSelection.PickerTimeChangedMessagingListener(
  const Sender: TObject; const M: TMessage);
var
  LReference: string;
  LM: TMessagingPickerTimeChanged;
  LTag: Integer;
begin
  if Sender = Parent then
  begin
    if M.InheritsFrom(TMessagingPickerTimeChanged) then
    begin
      LM := (M as TMessagingPickerTimeChanged);

      if TryStrToInt(LM.Reference, LTag) then
      begin
        if LTag = Tag then
        begin
          TextSettings.FontColor := TDateTimePicker(Owner).FDefaultBackgroundColor;
        end
        else
        begin
          TextSettings.FontColor := StringToAlphaColor('#FFE6E6E6');
        end;
      end;
    end;
  end;
end;

procedure TPickerTime.TTxtSelection.SetValue(const Value: Integer);
begin
  if Value < 0 then
    Text := ' '
  else
    Text := Format('%.*d', [2, Value]);
end;

end.
