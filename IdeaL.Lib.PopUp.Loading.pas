unit IdeaL.Lib.PopUp.Loading;

interface

uses
  IdeaL.Lib.PopUp,

  System.UITypes,
  System.UIConsts,
  System.SysUtils,
  System.Types,
  System.Classes,

  FMX.Controls,
  FMX.Objects,
  FMX.Graphics,
  FMX.Layouts,
  FMX.Types,
  FMX.StdCtrls,
  FMX.Effects,
  FMX.Dialogs,
  FMX.Forms,
  FMX.Filter.Effects;

type
  TFmxLoading = class;

  IAniLoading = interface
    ['{595EE3AC-EA74-4331-93CA-6889B2EC4E16}']
    function GetAniColor: TAlphaColor;
    procedure SetAniColor(const Value: TAlphaColor);
    property AniColor: TAlphaColor read GetAniColor write SetAniColor;

    function GetControl: TControl;
    property Control: TControl read GetControl;

    procedure Start;
    procedure Stop;
  end;

  TFmxLoadingItem = class(IdeaL.Lib.PopUp.TPopUpTextItem)
  private
    FLytAni: TLayout;
    FAniLoadingClass: TComponentClass;
    FAniLoading: IAniLoading;
    FAniIndicator: TAniIndicator;
    FFillRGBEffect: TFillRGBEffect;
    { private declarations }
  protected
    function GetHeight: Single; override;
    { protected declarations }
  public
    constructor Create(AOwner: TFmxLoading); reintroduce;
    destructor Destroy; override;

    function AniLoadingClass(AValue: TComponentClass): TFmxLoadingItem;
    function AniColor(AValue: TAlphaColor): TFmxLoadingItem;

    function DoResize: TPopUpItem; override;
    function Show: TFmxLoadingItem; reintroduce;
    { public declarations }
  published
    { published declarations }
  end;

  TBackground = class(TPopUpBackground)
  private
    FItem: TFmxLoadingItem;
    { private declarations }
  protected
    { protected declarations }
  public
    constructor Create(AOwner: TFmxLoading); reintroduce;
    destructor Destroy; override;

    function Loading: TFmxLoadingItem;

    function Show: TPopUpBackground; override;
    procedure DoResize; override;
    { public declarations }
  published
    { published declarations }
  end;

  TFmxLoading = class(IdeaL.Lib.PopUp.TPopUp)
  private
    function GetBackground: TBackground;
    { private declarations }
  protected
    { protected declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function Prepare: TFmxLoading; reintroduce;

    class function GetInstance(AParent: TFmxObject): TFmxLoading; reintroduce;

    property BackgroundLoad: TBackground read GetBackground;
    { public declarations }
  published
    { published declarations }
  end;

implementation

uses
  IdeaL.Lib.Utils;

{ TFmxLoading }

constructor TFmxLoading.Create(AOwner: TComponent);
begin
  inherited;

end;

destructor TFmxLoading.Destroy;
begin
  TUtils.EventLogIde('TFmxLoading.Destroy');
  inherited;
end;

function TFmxLoading.GetBackground: TBackground;
begin
  Result := Background as TBackground;
end;

class function TFmxLoading.GetInstance(AParent: TFmxObject): TFmxLoading;
begin
  Result := inherited GetInstance(AParent) as TFmxLoading;
  Result.Prepare;
end;

function TFmxLoading.Prepare: TFmxLoading;
begin
  Background := TBackground.Create(Self);
  Result := (inherited Prepare as TFmxLoading);
end;

{ TBackground }

constructor TBackground.Create(AOwner: TFmxLoading);
begin
  inherited Create(AOwner);
  OnPainting := nil;

  FItem := nil;

  Rct.OnClick := nil;
  Rct.OnTap := nil;
end;

destructor TBackground.Destroy;
begin
  TUtils.EventLogIde('TBackground.Destroy');
  OnPainting := nil;
  inherited;
end;

procedure TBackground.DoResize;
var
  LHeight: Single;
  LWidth: Single;
begin
  inherited;
  LHeight := Self.Height;
  LWidth := Self.Width;

  if (Self.Height * 0.8) < LHeight then
    LHeight := Self.Height * 0.8;
  if (Self.Width * 0.8) < LWidth then
    LWidth := Self.Width * 0.8;

  if TUtils.IsAssigned(FItem) then
  begin
    FItem.DoResize;
  end;
end;

function TBackground.Loading: TFmxLoadingItem;
begin
  if TUtils.IsAssigned(FItem) then
  begin
    if not(FItem.InheritsFrom(TFmxLoadingItem)) then
      raise Exception.Create('TBackground.Loading Items is not ' + TFmxLoadingItem.ClassName);
  end
  else
  begin
    FItem := TFmxLoadingItem.Create(TFmxLoading(Owner));
  end;
  FItem.Prepare(Self);
  Result := FItem;
end;

function TBackground.Show: TPopUpBackground;
begin
  OnPainting := nil;
  Result := inherited as TBackground;

  // It forces to resize the items
  TThread.CreateAnonymousThread(
    procedure
    begin
      //try
        TThread.Sleep(50);
        TThread.Synchronize(nil,
          procedure
          begin
            if TUtils.IsAssigned(FItem) then
            begin
              FItem.Show;
            end;
            OnPainting := DoOnPainting;
          end);
        TThread.Sleep(5);
        TThread.Synchronize(nil,
          procedure
          begin
  //{$IFNDEF IOS}
            try
              if
                (TUtils.IsAssigned(Self)) and
                (not Trim(Name).IsEmpty)
              then
                Height := Height + 1;
            except

            end;
  // {$ENDIF}
          end);
      {except
        // Crash on iOS forced this
      end;}
    end).Start;
end;

{ TFmxLoadingItem }

function TFmxLoadingItem.AniColor(AValue: TAlphaColor): TFmxLoadingItem;
begin
  Result := Self;

  if AValue = TAlphaColorRec.Null then
  begin
    FFillRGBEffect.Parent := nil;
    FFillRGBEffect.Color := TAlphaColorRec.Black;
  end
  else
  begin
    FFillRGBEffect.Color := AValue;
  end;

  if Assigned(FAniLoading) then
  begin
    try
      FAniLoading.AniColor := FFillRGBEffect.Color;
    except
      on E:Exception do
        raise Exception.Create('TFmxLoadingItem.AniColor ' + E.Message);
    end;
  end;
end;

function TFmxLoadingItem.AniLoadingClass(
  AValue: TComponentClass): TFmxLoadingItem;
begin
  Result := Self;

  if Assigned(AValue) and
    not(Supports(AValue, IAniLoading))
  then
    raise Exception.Create(FAniLoadingClass.ClassName + ' does not implement IAniLoading');

  if (Assigned(AValue)) then
  begin
    FAniLoadingClass := AValue;

    FAniIndicator.Enabled := False;
    FAniIndicator.Parent := nil;
    FFillRGBEffect.Parent := nil;

    if
      Assigned(FAniLoading) and
      not(TObject(FAniLoading).InheritsFrom(TInterfacedObject))
    then
    begin
      try
        try
          TObject(FAniLoading).Free;
        except
          StrToInt('1');
        end;
      finally
        FAniLoading := nil;
      end;
    end;

    FAniLoading := (FAniLoadingClass.Create(Self) as IAniLoading);
    FAniLoading.AniColor := FFillRGBEffect.Color;
    FAniLoading.Control.Parent := FLytAni;
    FAniLoading.Control.Align := TAlignLayout.Center;
    FAniLoading.Control.Height := 50;
    FAniLoading.Control.Width := 50;
  end;
end;

constructor TFmxLoadingItem.Create(AOwner: TFmxLoading);
begin
  inherited Create(AOwner);

  FAniLoading := nil;
  FAniLoadingClass := nil;

  FLytAni := TLayout.Create(Self);
  FLytAni.Parent := LytBackground;
  FLytAni.Align := TAlignLayout.MostLeft;
  FLytAni.HitTest := False;
  FLytAni.TabStop := False;

  FAniIndicator := TAniIndicator.Create(Self);
  FAniIndicator.Parent := FLytAni;
  FAniIndicator.Enabled := False;
  FAniIndicator.TabStop := False;
  FAniIndicator.HitTest := False;
  FAniIndicator.Align := TAlignLayout.Center;

  FFillRGBEffect := TFillRGBEffect.Create(Self);
  FFillRGBEffect.Enabled := False;
end;

destructor TFmxLoadingItem.Destroy;
begin
  TUtils.EventLogIde('TFmxLoadingItem.Destroy');
  inherited;
end;

function TFmxLoadingItem.DoResize: TPopUpItem;
var
  LHeight: Single;
  LHeight1: Single;
  LBool: Boolean;
begin
  Result := inherited;
  LHeight := TxtMessage.Height;
  LHeight1 := TControl(TxtMessage.Parent).Height;
  LBool := TxtMessage.AutoSize;

  if TxtMessage.Height <= TControl(TxtMessage.Parent).Height then
    TxtMessage.Align := TAlignLayout.VertCenter
  else
    TxtMessage.Align := TAlignLayout.Top;
end;

function TFmxLoadingItem.GetHeight: Single;
var
  LHeight: Single;
begin
  Result := inherited GetHeight;

  LHeight := 0;
  if FAniLoading <> nil then
    LHeight := TControl(FAniLoading).Height + TControl(FAniLoading).Position.Y
  else if Assigned(FAniIndicator) then
    LHeight := FAniIndicator.Height + FAniIndicator.Position.Y;

  if TxtMessage.Height < LHeight then
    Result := Result + LHeight - TxtMessage.Height;
end;

function TFmxLoadingItem.Show: TFmxLoadingItem;
begin
  Result := inherited Show as TFmxLoadingItem;
  if Assigned(FAniLoading) then
  begin
    FAniLoading.Start;
  end
  else
  begin
    TThread.Queue(nil,
      procedure
      begin
        FAniIndicator.Enabled := False;
        FFillRGBEffect.Enabled := FFillRGBEffect.Color <> TAlphaColorRec.Null;

        if FFillRGBEffect.Enabled then
          FFillRGBEffect.Parent := FLytAni
        else
          FFillRGBEffect.Parent := nil;
        FAniIndicator.BringToFront;
        FAniIndicator.PrepareForPaint;
        FAniIndicator.Repaint;
        FAniIndicator.Enabled := True;
      end);
  end;
  if Assigned(FLytAni) then
    FLytAni.BringToFront;
end;

end.
