unit IdeaL.Lib.PopUp;

interface

uses
  FMX.Types,
  FMX.Layouts,
  FMX.Objects,
  FMX.Graphics,
  FMX.Controls,
  FMX.StdCtrls,
  FMX.Forms,

  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes;

type
  TPopUp = class;

  TPopUpUtils = class
  private
    { private declarations }
  protected
    { protected declarations }
  public
    class function GetButton(
      AOwner, AParent: TFmxObject;
      AAlign: TAlignLayout;
      AText, AStyleLookup: string;
      AModalResult: Integer;
      ABtnWidth: Single): TLayout;
    { public declarations }
  end;

  TPopUpItem = class(TLayout)
  private
    FRctBackground: TRectangle;
    FLytBackground: TLayout;
    FHeightMin: Single;
    FWidthMax: Single;
    { private declarations }
  protected
    // procedure DoOnPainting(Sender: TObject; Canvas: TCanvas; const ARect: TRectF); virtual;
    function GetHeight: Single; virtual;
    property RctBackground: TRectangle read FRctBackground;
    property LytBackground: TLayout read FLytBackground;
    //property HeightMin: Single read FHeightMin write FHeightMin;
    // property WidthMax: Single read FWidthMax write FWidthMax;
    { protected declarations }
  public
    constructor Create(AOwner: TPopUp); reintroduce; virtual;
    destructor Destroy; override;

    function DoResize: TPopUpItem; virtual;

    function HeightMin(AValue: Single): TPopUpItem;
    function WidthMax(AValue: Single): TPopUpItem;
    function BackgroundColor(AValue: TAlphaColor): TPopUpItem;
    function BackgroundColorStroke(AValue: TAlphaColor): TPopUpItem;
    function Prepare(AParent: TFmxObject): TPopUpItem; virtual;
    function Show: TPopUpItem; virtual;
    { public declarations }
  end;

  TPopUpTextItem = class(TPopUpItem)
  private
    FVertScrl: TVertScrollBox;
    FTxtMessage: TLabel;
    FTextSettingsTxtMessage: TTextSettings;
    { private declarations }
  protected
    function GetHeight: Single; override;
    property TxtSettingsTxtMessage: TTextSettings read FTextSettingsTxtMessage;
    property VertScrl: TVertScrollBox read FVertScrl;
    property TxtMessage: TLabel read FTxtMessage;
    { protected declarations }
  public
    constructor Create(AOwner: TPopUp); reintroduce;
    destructor Destroy; override;

    function TextMessage(AValue: string): TPopUpTextItem; virtual;
    function TextSettingsTxtMessage(AValue: TTextSettings): TPopUpTextItem;

    function DoResize: TPopUpItem; override;
    { public declarations }
  end;

  TPopUpBackground = class(TLayout)
  private
  var
    FRct: TRectangle;

    { private declarations }
  protected
    property Rct: TRectangle read FRct;

    procedure ClickRct(Sender: TObject);
    procedure TapRct(Sender: TObject; const Point: TPointF);
    procedure DoOnPainting(Sender: TObject; Canvas: TCanvas; const ARect: TRectF); virtual;
    procedure DoOnPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF); virtual;
    { protected declarations }
  public
    constructor Create(AOwner: TPopUp); reintroduce; virtual;
    destructor Destroy; override;

    procedure Hide; virtual;
    function Show: TPopUpBackground; virtual;

    procedure DoResize; virtual;
    { public declarations }
  end;

  TPopUp = class(TFmxObject)
  private
    class var FParentName: string; // for testing stuff
    var
    FParent: TFmxObject;
    FBackground: TPopUpBackground;
    { private declarations }
  protected
    property Background: TPopUpBackground read FBackground write FBackground;
    { protected declarations }
  public
    class constructor OnCreate;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    class function GetInstance(AParent: TFmxObject): TPopUp; virtual;

    function Hide: TPopUp; virtual;
    function Prepare: TPopUp; virtual;
    function Show: TPopUp; virtual;
    function IsShowing: Boolean; virtual;
    { public declarations }
  published
    { published declarations }
  end;

implementation

uses
  System.UIConsts,

  IdeaL.Lib.Utils;

{ TPopUp }

constructor TPopUp.Create(AOwner: TComponent);
var
  LNewGUID: TGUID;
  LComp: TComponent;
  LName: string;
  LParentName: string;
begin
  inherited Create(AOwner);
  LComp := FParent;
  while LComp <> nil do
  begin
    LName := Trim(TComponent(LComp).Name);
    if Trim(LName) = EmptyStr then
      LName := LComp.ClassName;

    LParentName := LParentName + '.' + LName;
    LComp := LComp.Owner;
  end;

  FParentName := (LParentName + ';' + FParentName).Trim(['.',';']);

  System.SysUtils.CreateGUID(LNewGUID);
  TagString := GUIDToString(LNewGUID);
  TagString := TagString.Trim(['{', '}']);
  TagString := StringReplace(TagString, '-', EmptyStr, [rfReplaceAll]);
  Name := Self.ClassName + TagString;

  FBackground := nil;
end;

destructor TPopUp.Destroy;
var
  LName: string;
begin
  TUtils.EventLogIde('TPopUp.Destroy');
  LName := FParentName;
  if TUtils.IsAssigned(FBackground) then
  begin
    FBackground.Visible := False;
  end;
  inherited;
end;

class function TPopUp.GetInstance(AParent: TFmxObject): TPopUp;
begin
  Result := Self.Create(Application.Mainform);
  Result.FParent := AParent;
end;

function TPopUp.Hide: TPopUp;
begin
  Result := Self;
  if TUtils.IsAssigned(FBackground) then
    FBackground.Hide;
end;

function TPopUp.IsShowing: Boolean;
begin
  Result := (TUtils.IsAssigned(FBackground)) and (FBackground.Visible);
end;

class constructor TPopUp.OnCreate;
begin
  FParentName := EmptyStr;
end;

function TPopUp.Prepare: TPopUp;
begin
  Result := Self;

  if not TUtils.IsAssigned(FBackground) then
    FBackground := TPopUpBackground.Create(Self);
  Background.Hide;
  Background.Parent := FParent;
end;

function TPopUp.Show: TPopUp;
begin
  Result := Self;

  Background.Show;
end;

{ TPopUpBackground }

procedure TPopUpBackground.ClickRct(Sender: TObject);
begin
  Hide;
end;

constructor TPopUpBackground.Create(AOwner: TPopUp);
begin
  inherited Create(AOwner);

  TagString := AOwner.TagString;
  Name := Self.ClassName + TagString;
  TabStop := False;
  Align := TAlignLayout.Contents;
  Visible := False;

  FRct := TRectangle.Create(AOwner);
  FRct.Parent := Self;
  FRct.Align := TAlignLayout.Contents;
  FRct.Fill.Color := TAlphaColorRec.Black;
  FRct.TabStop := False;
  FRct.Opacity := 0.3;

{$IFDEF MSWINDOWS}
  FRct.OnClick := ClickRct;
{$ELSE}
  FRct.OnTap := TapRct;
{$ENDIF}
  OnPainting := DoOnPainting;
  OnPaint := DoOnPaint;
end;

destructor TPopUpBackground.Destroy;
begin
  TUtils.EventLogIde('TPopUpBackground.Destroy');
  OnPainting := nil;
  OnPaint := nil;
  inherited;
end;

procedure TPopUpBackground.DoOnPaint(Sender: TObject; Canvas: TCanvas;
  const ARect: TRectF);
begin
  // OnPainting := nil;
  try
    DoResize;
  finally
    { if not Trim(Name).IsEmpty then
      OnPainting := DoOnPainting; }
  end;
end;

procedure TPopUpBackground.DoOnPainting(Sender: TObject; Canvas: TCanvas;
  const ARect: TRectF);
begin

end;

procedure TPopUpBackground.DoResize;
begin
  // implement it on the Inherited class
{$IFDEF MSWINDOWS}
  Height := Height + 1;
{$ENDIF}
end;

procedure TPopUpBackground.Hide;
begin
  Visible := False;
end;

function TPopUpBackground.Show: TPopUpBackground;
begin
  Result := Self;
  Visible := True;
  BringToFront;
end;

procedure TPopUpBackground.TapRct(Sender: TObject; const Point: TPointF);
begin
  ClickRct(Sender);
end;

{ TPopUpItem }

function TPopUpItem.BackgroundColor(AValue: TAlphaColor): TPopUpItem;
begin
  Result := Self;

  FRctBackground.Fill.Color := AValue;
end;

function TPopUpItem.BackgroundColorStroke(AValue: TAlphaColor): TPopUpItem;
begin
  Result := Self;

  FRctBackground.Stroke.Color := AValue;
  if FRctBackground.Stroke.Color = TAlphaColorRec.Null then
    FRctBackground.Stroke.Kind := TBrushKind.None
  else
    FRctBackground.Stroke.Kind := TBrushKind.Solid;
end;

constructor TPopUpItem.Create(AOwner: TPopUp);
begin
  inherited Create(AOwner);
  var
  LName := TControl(Owner).TagString + Random(9999999).ToString + FormatDateTime('sszzz', Now);
  Name := Self.ClassName + LName;
  TabStop := False;
  Align := TAlignLayout.Center;
  Visible := False;
  Height := 1;
  FHeightMin := 50;
  FWidthMax := 0;

  FRctBackground := TRectangle.Create(AOwner);
  FRctBackground.Name := FRctBackground.ClassName + LName;
  FRctBackground.TabStop := False;
  FRctBackground.Parent := Self;
  FRctBackground.Align := TAlignLayout.Contents;
  FRctBackground.Stroke.Kind := TBrushKind.None;
  FRctBackground.Fill.Color := TAlphaColorRec.White;
  FRctBackground.XRadius := 10;
  FRctBackground.YRadius := 10;
  FRctBackground.Padding.Bottom := 10;
  FRctBackground.Padding.Left := 10;
  FRctBackground.Padding.Right := 10;
  FRctBackground.Padding.Top := 10;
  FRctBackground.ClipChildren := True;

  FLytBackground := TLayout.Create(AOwner);
  FLytBackground.Name := FLytBackground.ClassName + LName;
  FLytBackground.TabStop := False;
  FLytBackground.Parent := Self;
  FLytBackground.Align := TAlignLayout.Contents;
  FLytBackground.Padding.Bottom := 15;
  FLytBackground.Padding.Left := 15;
  FLytBackground.Padding.Right := 15;
  FLytBackground.Padding.Top := 15;
  FLytBackground.ClipChildren := True;
  FLytBackground.BringToFront;
end;

destructor TPopUpItem.Destroy;
begin
  TUtils.EventLogIde('TPopUpItem.Destroy');
  inherited;
end;

function TPopUpItem.DoResize: TPopUpItem;
var
  LHeight: Single;
  LWidth: Single;
  LParent: TControl;
  LParentAux: TControl;
begin
  Result := Self;
  LHeight := GetHeight;

  if LHeight < FHeightMin then
    LHeight := FHeightMin;

  LParent := TControl(Parent);
  LParentAux := LParent;
  while True do
  begin
    if LParentAux.InheritsFrom(TPopUpBackground) then
    begin
      LParent := LParentAux;
      Break;
    end
    else
    if
      LParentAux.InheritsFrom(TPopUp) or
      not TUtils.IsAssigned(LParentAux.Parent) or
      not LParentAux.Parent.InheritsFrom(TControl)
    then
      Break
    else
      LParentAux := TControl(LParentAux.Parent);
  end;

  LHeight := LHeight {+ 20};
  if LHeight > (LParent.Height * 0.8) then
    LHeight := LParent.Height * 0.8;

  LWidth := LParent.Width * 0.8;
  if (FWidthMax > 0) and (LWidth > FWidthMax) then
    LWidth := FWidthMax;

  if LHeight <> Height then
    Height := LHeight;
  if LWidth <> Width then
    Width := LWidth;
end;

function TPopUpItem.GetHeight: Single;
begin
  Result := 0;
  if TUtils.IsAssigned(FLytBackground) then
    Result :=
      FLytBackground.Padding.Bottom +
      FLytBackground.Padding.Top
end;

function TPopUpItem.HeightMin(AValue: Single): TPopUpItem;
begin
  Result := Self;

  FHeightMin := AValue;
end;

function TPopUpItem.Prepare(AParent: TFmxObject): TPopUpItem;
begin
  Result := Self;

  Parent := AParent;
end;

function TPopUpItem.Show: TPopUpItem;
begin
  Result := Self;
  Visible := True;
  BringToFront;
end;

function TPopUpItem.WidthMax(AValue: Single): TPopUpItem;
begin
  Result := Self;

  FWidthMax := AValue;
end;

{ TPopUpUtils }

class function TPopUpUtils.GetButton(AOwner, AParent: TFmxObject;
  AAlign: TAlignLayout; AText, AStyleLookup: string;
  AModalResult: Integer; ABtnWidth: Single): TLayout;
begin
  Result := TLayout.Create(AOwner);
  Result.TabStop := False;
  Result.CanFocus := False;
  Result.Align := AAlign;
  Result.Parent := AParent;

  var
  LBtn := TButton.Create(AOwner);
  LBtn.TabStop := False;
  LBtn.CanFocus := False;
  LBtn.Parent := Result;
  LBtn.Align := TAlignLayout.Client;
  LBtn.Text := AText;
  LBtn.StyleLookup := AStyleLookup;
  LBtn.Tag := AModalResult; // Do not use the normal ModalResult, it raises exceptions on Mobile

  if ABtnWidth > 0 then
  begin
    LBtn.Align := TAlignLayout.HorzCenter;
    LBtn.Width := ABtnWidth;
  end;
end;

{ TPopUpTextItem }

constructor TPopUpTextItem.Create(AOwner: TPopUp);
begin
  inherited Create(AOwner);

  FVertScrl := TVertScrollBox.Create(AOwner);
  FVertScrl.Parent := LytBackground;
  FVertScrl.Align := TAlignLayout.Client;
  FVertScrl.ShowScrollBars := False;

  FTextSettingsTxtMessage := TTextSettings.Create(AOwner);
  FTextSettingsTxtMessage.Font.Size := 16;
  FTextSettingsTxtMessage.HorzAlign := TTextAlign.Center;
  FTextSettingsTxtMessage.VertAlign := TTextAlign.Center;
  FTextSettingsTxtMessage.WordWrap := True;

  FTxtMessage := TLabel.Create(AOwner);
  FTxtMessage.Align := TAlignLayout.Top;
  FTxtMessage.AutoSize := False;
  FTxtMessage.StyledSettings := [];
  FTxtMessage.Text := EmptyStr;
  FTxtMessage.Height := 0;
  FTxtMessage.HitTest := False;
  FVertScrl.AddObject(FTxtMessage);
  FTxtMessage.TextSettings := FTextSettingsTxtMessage;
end;

destructor TPopUpTextItem.Destroy;
begin
  TUtils.EventLogIde('TPopUpTextItem.Destroy');
  if (Assigned(Self)) and (Trim(Name) <> EmptyStr)  then
  begin
    try
      if Assigned(FTextSettingsTxtMessage) and (Assigned(FTextSettingsTxtMessage.Owner)) then
      begin
          FreeAndNil(FTextSettingsTxtMessage);
      end;
    except
        on E:Exception do
        begin
          try
            TUtils.ThrowExceptionSomethingWentWrong('TPopUpTextItem.Destroy ' + E.Message);
          except
            StrToInt('1');
          end;
        end;
    end;
  end;
  inherited;
end;

function TPopUpTextItem.DoResize: TPopUpItem;
begin
  FTxtMessage.AutoSize := True;
  Result := inherited;

  //FTxtMessage.AutoSize := False;
  FTxtMessage.Width := TControl(FTxtMessage.Parent).Width;
end;

function TPopUpTextItem.GetHeight: Single;
var
  LHeight: Single;
begin
  Result := inherited;
  LHeight := FTxtMessage.Height;
  Result := Result + FTxtMessage.Height;
end;

function TPopUpTextItem.TextMessage(AValue: string): TPopUpTextItem;
begin
  Result := Self;
  FTxtMessage.Text := AValue;
end;

function TPopUpTextItem.TextSettingsTxtMessage(
  AValue: TTextSettings): TPopUpTextItem;
begin
  Result := Self;

  FreeAndNil(FTextSettingsTxtMessage);
  FTextSettingsTxtMessage := AValue;
  FTxtMessage.TextSettings := AValue;
end;

end.
