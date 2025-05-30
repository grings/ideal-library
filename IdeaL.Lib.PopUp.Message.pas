unit IdeaL.Lib.PopUp.Message;

interface

uses
  IdeaL.Lib.PopUp,

  Fmx.Types,
  Fmx.Layouts,
  Fmx.Objects,
  Fmx.Controls,
  Fmx.Graphics,
  Fmx.StdCtrls,
  Fmx.Dialogs,
  Fmx.Ani,
  Fmx.Edit,
  Fmx.MediaLibrary,

  System.Classes,
  System.Generics.Collections,
  System.Types,
  System.SysUtils;

type
  TMsgType = (mtInformation, mtQuestion, mtError);
  TMsgTypeButton = (mtbOk, mtbOkCancel, mtbYesNo, mtbLoading, mtbNone);

  TArrayString = array of string;

  TFmxMessage = class;

  TFmxMessageItem = class(IdeaL.Lib.PopUp.TPopUpTextItem)
  private
    FText: string;
    FExtraHeight: Single;
    FIsToDestroy: Boolean;
    { private declarations }
  protected
  var
    FCloseDialogProc: TInputCloseDialogProc;
    property ExtraHeight: Single read FExtraHeight write FExtraHeight;
    property IsToDestroy: Boolean read FIsToDestroy write FIsToDestroy;
    { protected declarations }
  public
    constructor Create(AOwner: TFmxMessage); reintroduce;

    function CloseDialogProc(AValue: TInputCloseDialogProc): TFmxMessageItem; virtual;
    { public declarations }
  end;

  TFmxMessageToast = class(TFmxMessageItem)
  private
    FLytBottom: TLayout;
    { private declarations }
  protected
    FFloatAnimation: TFloatAnimation;
    FOnToastHide: TProc;

    procedure FloatAnimationFinish(Sender: TObject); virtual;
    procedure DoToastClick(Sender: TObject);
    procedure DoToastTap(Sender: TObject; const Point: TPointF);
    { protected declarations }
  public
    constructor Create(AOwner: TFmxMessage); reintroduce;
    function BackgroundOpacity(AValue: Single): TFmxMessageItem;
    function Show: TPopUpItem; override;
    function Prepare(AParent: TFmxObject): TPopUpItem; override;
    function DoResize: TPopUpItem; override;
    function MarginBottom(AValue: Single): TFmxMessageToast; virtual;
    // in seconds
    function Delay(AValue: Integer): TFmxMessageToast; virtual;
    function CloseDialogProc(AValue: TInputCloseDialogProc): TFmxMessageItem; override;
    function OnToastHide(ANotifyEvent: TProc): TFmxMessageItem; virtual;
    { public declarations }
  end;

  TFmxMessageDialog = class(TFmxMessageItem)
  private
    FTxtHeader: TText;
    FTextSettingsTxtHeader: TTextSettings;
    FMsgTypeButton: TMsgTypeButton;
    FBtnStyleLookup: string;
    FLytButton: TLayout;
    FHeader: string;
    FBtnWidth: Single;

    FBtnTxtOk: string;
    FBtnTxtCancel: string;
    FBtnTxtYes: string;
    FBtnTxtNo: string;
    { private declarations }
  protected
    function GetHeight: Single; override;
    procedure DoClickButton(Sender: TObject); virtual;
    procedure DoTapButton(Sender: TObject; const Point: TPointF);
    { protected declarations }
  public
    constructor Create(AOwner: TFmxMessage); reintroduce;
    destructor Destroy; override;

    function ButtonType(AValue: TMsgTypeButton): TFmxMessageDialog; virtual;
    function TextHeader(AValue: string): TFmxMessageDialog;
    function TextSettingsTxtHeader(AValue: TTextSettings): TFmxMessageDialog;
    function BtnWidth(AValue: Single): TFmxMessageDialog;
    function BtnStyleLookup(AValue: string): TFmxMessageDialog;
    function BtnTxtOk(AOk: string): TFmxMessageDialog;
    function BtnTxtOkCancel(AOk, ACancel: string): TFmxMessageDialog;
    function BtnTxtYesNo(AYes, ANo: string): TFmxMessageDialog;
    function BtnTxt(AOk, ACancel, AYes, ANo: string): TFmxMessageDialog;

    function Show: TFmxMessageItem; reintroduce;
    function Prepare: TFmxMessageItem; overload; virtual;
    function DoResize: TPopUpItem; override;
    { public declarations }
  published
    { published declarations }
  end;

  TFmxMessageInputQuery = class(TFmxMessageDialog)
  private
  var
    FCloseQueryProc: TInputCloseQueryProc;
    FPrompt: array of string;
    FValue: array of string;
    FEdtStyleLookup: string;
    FObjLstEdt: TObjectList<TEdit>;

    function TextMessage(AValue: string): TFmxMessageInputQuery; reintroduce;
    { private declarations }
  protected
    function GetHeight: Single; override;
    procedure DoClickButton(Sender: TObject); override;

    function ButtonType(AValue: TMsgTypeButton): TFmxMessageDialog; override;
    { protected declarations }
  public
    constructor Create(AOwner: TFmxMessage); reintroduce;
    destructor Destroy; override;

    function EdtStyleLookup(AValue: string): TFmxMessageInputQuery;
    function Prompt(AValue: array of string): TFmxMessageInputQuery;
    function Value(AValue: array of string): TFmxMessageInputQuery;
    function Prepare: TFmxMessageItem; override;
    function DoResize: TPopUpItem; override;
    function CloseDialogProc(AValue: TInputCloseQueryProc): TFmxMessageInputQuery; reintroduce;
    { public declarations }
  published
    { published declarations }
  end;

  TBackground = class(TPopUpBackground)
  private
    FItems: TObjectList<TFmxMessageItem>;
    FItemShown: TFmxMessageItem;
    { private declarations }
  protected
    { protected declarations }
  public
    constructor Create(AOwner: TFmxMessage); reintroduce;
    destructor Destroy; override;

    function Dialog: TFmxMessageDialog;
    function Toast: TFmxMessageToast;
    function InputQuery: TFmxMessageInputQuery;

    function Show: TPopUpBackground; override;

    procedure DoResize; override;
    { public declarations }
  published
    { published declarations }
  end;

  TFmxMessage = class(IdeaL.Lib.PopUp.TPopUp)
  private
    function GetBackground: TBackground;
    { private declarations }
  protected
    { protected declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function Prepare: TFmxMessage; reintroduce;
    function Show: TFmxMessage; reintroduce;

    class function GetInstance(AParent: TFmxObject): TFmxMessage; reintroduce;

    property BackgroundMsg: TBackground read GetBackground;
    { public declarations }
  published
    { published declarations }
  end;

implementation

uses
  IdeaL.Lib.View.Utils,
  IdeaL.Lib.PathSvg,
  System.UITypes;

{ TFmxMessage }

constructor TFmxMessage.Create(AOwner: TComponent);
begin
  inherited;

end;

destructor TFmxMessage.Destroy;
begin

  inherited;
end;

class function TFmxMessage.GetInstance(AParent: TFmxObject): TFmxMessage;
begin
  Result := inherited GetInstance(AParent) as TFmxMessage;
  Result.Prepare;
end;

function TFmxMessage.GetBackground: TBackground;
begin
  Result := Background as TBackground;
end;

function TFmxMessage.Prepare: TFmxMessage;
begin
  Background := TBackground.Create(Self);
  Result := (inherited Prepare as TFmxMessage);
end;

function TFmxMessage.Show: TFmxMessage;
begin
  Result := (inherited Show as TFmxMessage);
end;

{ TBackground }

constructor TBackground.Create(AOwner: TFmxMessage);
begin
  inherited Create(AOwner);
  OnPainting := nil;
  FItems := TObjectList<TFmxMessageItem>.Create;

  FItemShown := nil;
end;

destructor TBackground.Destroy;
begin
  while FItems.Count > 0 do
    FItems.ExtractAt(0);
  FreeAndNil(FItems);
  inherited;
end;

function TBackground.Dialog: TFmxMessageDialog;
var
  LItem: TFmxMessageDialog;
begin
  if (FItems.Count > 0) then
  begin
    if not(FItems[0].InheritsFrom(TFmxMessageDialog)) then
      raise Exception.Create('TBackground.Dialog Items is not ' + TFmxMessageDialog.ClassName);
    LItem := TFmxMessageDialog(FItems[0]);
  end
  else
  begin
    LItem := TFmxMessageDialog.Create(TFmxMessage(Owner));
    FItems.Add(LItem);
  end;
  LItem.Prepare(Self);
  Result := LItem;
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
  if FItems.Count > 0 then
  begin
    var
    i := 0;
    while i < FItems.Count do
    begin
      if Assigned(FItems[i]) and not(Trim(FItems[i].Name).IsEmpty) then
      begin
        if FItems[i].IsToDestroy then
        begin
          (FItems.ExtractAt(i)).Free;
        end
        else
        begin
          FItems[i].DoResize;
          Inc(i, 1);
        end;
      end
      else
        Inc(i, 1);
    end;
  end;
end;

function TBackground.InputQuery: TFmxMessageInputQuery;
var
  LItem: TFmxMessageInputQuery;
begin
  if (FItems.Count > 0) then
  begin
    if not(FItems[0].InheritsFrom(TFmxMessageInputQuery)) then
      raise Exception.Create('TBackground.InputQuery Items is not ' + TFmxMessageInputQuery.ClassName);
    LItem := TFmxMessageInputQuery(FItems[0]);
  end
  else
  begin
    LItem := TFmxMessageInputQuery.Create(TFmxMessage(Owner));
    FItems.Add(LItem);
  end;
  LItem.Prepare(Self);
  Result := LItem;
end;

function TBackground.Show: TPopUpBackground;
begin
  OnPainting := nil;
  Result := inherited as TBackground;

  // Foces to resize the items
  TThread.CreateAnonymousThread(
    procedure
    begin
      TThread.Synchronize(nil,
        procedure
        begin
          if FItems.Count > 0 then
          begin
            // for var i := 0 to Pred(FItems.Count) do
            begin
              FItems.Last.Show;
            end;
          end;
          OnPainting := DoOnPainting;
        end);
      TThread.Sleep(5);
      TThread.Synchronize(nil,
        procedure
        begin
          if not Trim(Name).IsEmpty then
            Height := Height + 1;
          var
          LHeight := Height;
          Repaint;
        end);
    end).Start;
end;

function TBackground.Toast: TFmxMessageToast;
var
  LItem: TFmxMessageToast;
begin
  for var i := 0 to Pred(FItems.Count) do
    if not FItems[i].InheritsFrom(TFmxMessageToast) then
      raise Exception.Create('TBackground.Toast FItemShown is not ' + TFmxMessageToast.ClassName);

  LItem := TFmxMessageToast.Create(TFmxMessage(Owner));
  LItem.Prepare(Self);
  { if FItems.Count > 0 then
    begin
    var LVar1:= TControl(FItems.Last.Parent).Position.Y;
    var LVar2 := TControl(FItems.Last.Parent).Height;

    TControl(LItem.Parent).Position.Y := TControl(FItems.Last.Parent).Position.Y - TControl(FItems.Last.Parent).Height;
    end; }
  FItems.Add(LItem);
  Rct.Visible := False;
  Result := LItem;
end;

{ TFmxMessageItem }

function TFmxMessageItem.CloseDialogProc(
  AValue: TInputCloseDialogProc): TFmxMessageItem;
begin
  Result := Self;

  FCloseDialogProc := AValue;
end;

constructor TFmxMessageItem.Create(AOwner: TFmxMessage);
begin
  inherited Create(AOwner);

  ExtraHeight := 0;
  FCloseDialogProc := nil;
  FIsToDestroy := False;
end;

{ TFmxMessageDialog }

function TFmxMessageDialog.BtnStyleLookup(AValue: string): TFmxMessageDialog;
begin
  Result := Self;

  FBtnStyleLookup := AValue;
end;

function TFmxMessageDialog.BtnTxt(AOk, ACancel, AYes,
  ANo: string): TFmxMessageDialog;
begin
  Result := Self;
  BtnTxtOkCancel(AOk, ACancel);
  BtnTxtYesNo(AYes, ANo);
end;

function TFmxMessageDialog.BtnTxtOk(AOk: string): TFmxMessageDialog;
begin
  Result := Self;
  FBtnTxtOk := AOk;
end;

function TFmxMessageDialog.BtnTxtOkCancel(AOk,
  ACancel: string): TFmxMessageDialog;
begin
  Result := Self;
  BtnTxtOk(AOk);
  FBtnTxtCancel := ACancel;
end;

function TFmxMessageDialog.BtnTxtYesNo(AYes, ANo: string): TFmxMessageDialog;
begin
  Result := Self;
  FBtnTxtYes := AYes;
  FBtnTxtNo := ANo;
end;

function TFmxMessageDialog.BtnWidth(AValue: Single): TFmxMessageDialog;
begin
  Result := Self;
  FBtnWidth := AValue;
end;

function TFmxMessageDialog.ButtonType(
  AValue: TMsgTypeButton): TFmxMessageDialog;
begin
  Result := Self;

  FMsgTypeButton := AValue;
end;

constructor TFmxMessageDialog.Create(AOwner: TFmxMessage);
begin
  inherited Create(AOwner);
  FBtnStyleLookup := EmptyStr;
  FBtnTxtOk := 'Ok';
  FBtnTxtCancel := 'Cancel';
  FBtnTxtYes := 'Yes';
  FBtnTxtNo := 'No';
  FBtnWidth := 0;

  FTxtHeader := TText.Create(AOwner);
  FTxtHeader.Parent := LytBackground;
  FTxtHeader.Align := TAlignLayout.MostTop;
  FTxtHeader.AutoSize := False;
  TextHeader(EmptyStr);
  FTxtHeader.Height := 0;
  FTxtHeader.HitTest := False;
  FTxtHeader.TextSettings := FTextSettingsTxtHeader;
  FTxtHeader.Margins.Bottom := 10;

  FLytButton := TLayout.Create(AOwner);
  FLytButton.HitTest := False;
  FLytButton.TabStop := False;
  FLytButton.Align := TAlignLayout.MostBottom;
  FLytButton.Parent := LytBackground;

  FMsgTypeButton := TMsgTypeButton.mtbOk;
end;

destructor TFmxMessageDialog.Destroy;
begin
  if Assigned(FTextSettingsTxtHeader) then
    FreeAndNil(FTextSettingsTxtHeader);
  inherited;
end;

procedure TFmxMessageDialog.DoClickButton(Sender: TObject);
begin
{$IFDEF MSWINDOWS}
  if Sender.InheritsFrom(TControl) and (Assigned(TControl(Sender).OnTap)) then
    TControl(Sender).OnTap(Sender, TPointF.Create(0, 0));
{$ENDIF}
end;

function TFmxMessageDialog.DoResize: TPopUpItem;
var
  LWidth: Single;
begin
  FTxtHeader.AutoSize := True;
  Result := inherited;

  LWidth := 0;
  if FLytButton.ChildrenCount > 0 then
    LWidth := Trunc(FLytButton.Width / FLytButton.ChildrenCount);

  FTxtHeader.AutoSize := False;
  for var i := 0 to Pred(FLytButton.ChildrenCount) do
  begin
    TControl(FLytButton.Children[i]).Width := LWidth;
  end;

  FTxtHeader.Width := TControl(FTxtHeader.Parent).Width;
  Self.Height := GetHeight;
end;

procedure TFmxMessageDialog.DoTapButton(Sender: TObject; const Point: TPointF);
begin
  IdeaL.Lib.PopUp.TPopUp(Owner).Hide;

  if (not Assigned(Sender)) or
    (not Sender.InheritsFrom(TControl)) or
    (not Assigned(FCloseDialogProc))
  then
    Exit;

  var
  LTag := TControl(Sender).Tag;

  TThread.CreateAnonymousThread(
    procedure
    begin
      TThread.Sleep(100);
      TThread.ForceQueue(nil,
        procedure
        begin
          FCloseDialogProc(LTag);
        end);
    end).Start;
end;

function TFmxMessageDialog.GetHeight: Single;
begin
  Result := inherited;

  Result := Result +
    FTxtHeader.Height +
    FLytButton.Height +
    FTxtHeader.Margins.Bottom;
end;

function TFmxMessageDialog.Prepare: TFmxMessageItem;
  procedure AddBtn(AText: string; AModalResult: Integer);
  var
    LBtn: TButton;
  begin
    var
    LLyt := TPopUpUtils.GetButton(
      TFmxObject(Owner),
      FLytButton,
      TAlignLayout.Left,
      AText,
      FBtnStyleLookup,
      AModalResult,
      FBtnWidth
      );
    LBtn := LLyt.Children[0] as TButton;
{$IFDEF MSWINDOWS}
    LBtn.OnClick := DoClickButton;
{$ELSE}
    LBtn.OnTap := DoTapButton;
{$ENDIF}
  end;

begin
  Result := Self;

  FLytButton.DeleteChildren;

  case FMsgTypeButton of
    mtbOk, mtbOkCancel:
      begin
        case FMsgTypeButton of
          mtbOkCancel:
            begin
              AddBtn(FBtnTxtCancel, System.UITypes.idCancel);
            end;
        end;
        AddBtn(FBtnTxtOk, System.UITypes.idOK);
      end;
    mtbYesNo:
      begin
        AddBtn(FBtnTxtNo, System.UITypes.idNo);
        AddBtn(FBtnTxtYes, System.UITypes.idYes);
      end;
  end;
end;

function TFmxMessageDialog.Show: TFmxMessageItem;
begin
  Result := inherited Show as TFmxMessageItem;
end;

function TFmxMessageDialog.TextHeader(AValue: string): TFmxMessageDialog;
begin
  Result := Self;
  FHeader := AValue;
  FTxtHeader.Text := FHeader;
  FTxtHeader.Visible := not FTxtHeader.Text.IsEmpty;
end;

function TFmxMessageDialog.TextSettingsTxtHeader(
  AValue: TTextSettings): TFmxMessageDialog;
begin
  Result := Self;

  FreeAndNil(FTextSettingsTxtHeader);
  FTextSettingsTxtHeader := AValue;
  FTxtHeader.TextSettings := FTextSettingsTxtHeader;
end;

{ TFmxMessageToast }

function TFmxMessageToast.BackgroundOpacity(AValue: Single): TFmxMessageItem;
begin
  Result := Self;
  RctBackground.Opacity := AValue;
end;

function TFmxMessageToast.CloseDialogProc(
  AValue: TInputCloseDialogProc): TFmxMessageItem;
begin
  TUtils.ThrowExceptionMethodIsNotImplemented('TFmxMessageToast.CloseDialogProc');
  // Need to fix an AV while tapping, also there is the TEXT with HitsTest
  Result := inherited;
  RctBackground.OnClick := DoToastClick;
  RctBackground.OnTap := DoToastTap;
end;

constructor TFmxMessageToast.Create(AOwner: TFmxMessage);
begin
  inherited Create(AOwner);
  FOnToastHide := nil;

  BackgroundColor(TAlphaColorRec.Black);
  BackgroundColorStroke(TAlphaColorRec.Null);
  BackgroundOpacity(0.7);
  TxtSettingsTxtMessage.Font.Size := 14;
  TxtSettingsTxtMessage.Font.Style := [TFontStyle.fsBold];
  TxtSettingsTxtMessage.FontColor := TAlphaColorRec.White;
  TxtMessage.TextSettings := TxtSettingsTxtMessage;

  FLytBottom := TLayout.Create(AOwner);
  FLytBottom.TabStop := False;
  FLytBottom.HitTest := False;
  FLytBottom.Align := TAlignLayout.Bottom;
  FLytBottom.Margins.Bottom := 10;
  FLytBottom.Visible := False;

  FFloatAnimation := TFloatAnimation.Create(FLytBottom);
  FFloatAnimation.Delay := 2;
  FFloatAnimation.Duration := 0.2;
  FFloatAnimation.PropertyName := 'Opacity';
  FFloatAnimation.StartValue := 1;
  FFloatAnimation.StopValue := 0;
  FFloatAnimation.OnFinish := FloatAnimationFinish;
  FFloatAnimation.Parent := FLytBottom;
end;

function TFmxMessageToast.Delay(AValue: Integer): TFmxMessageToast;
begin
  Result := Self;

  FFloatAnimation.Delay := AValue;
end;

function TFmxMessageToast.DoResize: TPopUpItem;
begin
  Result := Self;
  if not FLytBottom.Visible then
    Exit;
  Result := inherited;

  FLytBottom.Align := TAlignLayout.Bottom;

  FLytBottom.Height := Height;
end;

procedure TFmxMessageToast.DoToastClick(Sender: TObject);
begin
{$IFDEF MSWINDOWS}
  if Sender.InheritsFrom(TControl) and (Assigned(TControl(Sender).OnTap)) then
    TControl(Sender).OnTap(Sender, TPointF.Create(0, 0));
{$ENDIF}
end;

procedure TFmxMessageToast.DoToastTap(Sender: TObject; const Point: TPointF);
begin
  FFloatAnimation.Enabled := False;
  FloatAnimationFinish(FFloatAnimation);

  if (not Assigned(Sender)) or
    (not Sender.InheritsFrom(TControl)) or
    (not Assigned(FCloseDialogProc))
  then
    Exit;

  var
  LTag := TControl(Sender).Tag;

  TThread.CreateAnonymousThread(
    procedure
    begin
      TThread.Sleep(100);
      TThread.ForceQueue(nil,
        procedure
        begin
          FCloseDialogProc(LTag);
        end);
    end).Start;
end;

procedure TFmxMessageToast.FloatAnimationFinish(Sender: TObject);
begin
  FLytBottom.Visible := False;

  if Assigned(FOnToastHide) then
  begin
    var
    LProc := FOnToastHide;
    TThread.CreateAnonymousThread(
      procedure
      begin
        TThread.Sleep(100);
        TThread.ForceQueue(nil,
          procedure
          begin
            LProc();
          end);
      end).Start;
  end;

  IsToDestroy := True;
end;

function TFmxMessageToast.MarginBottom(AValue: Single): TFmxMessageToast;
begin
  Result := Self;

  FLytBottom.Margins.Bottom := 10 + AValue;
end;

function TFmxMessageToast.OnToastHide(
  ANotifyEvent: TProc): TFmxMessageItem;
begin
  Result := Self;
  FOnToastHide := ANotifyEvent;
end;

function TFmxMessageToast.Prepare(AParent: TFmxObject): TPopUpItem;
begin
  Result := inherited Prepare(FLytBottom);
  FLytBottom.Parent := AParent;
end;

function TFmxMessageToast.Show: TPopUpItem;
begin
  Result := inherited;
  FLytBottom.Visible := True;
  FLytBottom.Position.Y := -100;

  FFloatAnimation.Enabled := True;
end;

{ TFmxMessageInputQuery }

function TFmxMessageInputQuery.ButtonType(
  AValue: TMsgTypeButton): TFmxMessageDialog;
begin
  raise Exception.Create('Not implemented TFmxMessageInputQuery.ButtonType');
end;

function TFmxMessageInputQuery.CloseDialogProc(
  AValue: TInputCloseQueryProc): TFmxMessageInputQuery;
begin
  Result := Self;

  FCloseQueryProc := AValue;
end;

constructor TFmxMessageInputQuery.Create(AOwner: TFmxMessage);
begin
  inherited Create(AOwner);
  FCloseQueryProc := nil;

  FMsgTypeButton := TMsgTypeButton.mtbOkCancel;
  SetLength(FPrompt, 0);
  SetLength(FValue, 0);
  FObjLstEdt := TObjectList<TEdit>.Create;
end;

destructor TFmxMessageInputQuery.Destroy;
begin
  while FObjLstEdt.Count > 0 do
    FObjLstEdt.ExtractAt(0);
  FreeAndNil(FObjLstEdt);
  inherited;
end;

procedure TFmxMessageInputQuery.DoClickButton(Sender: TObject);
var
  LValues: array of string;
begin
  FCloseDialogProc := nil;
  inherited;

  SetLength(LValues, Length(FValue));

  for var i := 0 to Pred(Length(FValue)) do
  begin
    LValues[i] := FObjLstEdt[i].Text;
  end;

  if Assigned(FCloseQueryProc) then
    FCloseQueryProc(TControl(Sender).Tag, LValues);
end;

function TFmxMessageInputQuery.DoResize: TPopUpItem;
begin
  for var LEdt in FObjLstEdt do
  begin
    var
    LLyt := TLayout(LEdt.Parent);
    TControl(LLyt.Children[0]).Width := LLyt.Width;
  end;

  Result := inherited;

end;

function TFmxMessageInputQuery.EdtStyleLookup(
  AValue: string): TFmxMessageInputQuery;
begin
  Result := Self;

  FEdtStyleLookup := AValue;
end;

function TFmxMessageInputQuery.GetHeight: Single;
var
  LHeight: Single;
begin
  Result := inherited;

  for var LEdt in FObjLstEdt do
  begin
    LHeight := LEdt.Height;
    var
    LLyt := TLayout(LEdt.Parent);
    LHeight := LHeight + TControl(LLyt.Children[0]).Height;
    LLyt.Height := LHeight;

    Result := Result + Trunc(LLyt.Margins.Bottom + LLyt.Margins.Top) + LHeight;
  end;
end;

function TFmxMessageInputQuery.Prepare: TFmxMessageItem;
begin
  Result := inherited Prepare;

  if (Length(FPrompt) <= 0) or
    (Length(FValue) <= 0)
  then
    raise Exception.Create('TFmxMessageInputQuery.Prepare no Prompt or Value was set');

  if (Length(FPrompt) <> Length(FValue)) then
    raise Exception.Create('TFmxMessageInputQuery.Prepare no Prompt and Value have different sizes');

  for var i := 0 to Pred(Length(FPrompt)) do
  begin
    var
    LLyt := TLayout.Create(Owner);
    LLyt.HitTest := False;
    LLyt.TabStop := False;
    LLyt.Align := TAlignLayout.Top;
    VertScrl.AddObject(LLyt);
    LLyt.Position.Y := VertScrl.Content.ChildrenCount * LLyt.Height;
    LLyt.Margins.Bottom := 4;
    VertScrl.ShowScrollBars := False;

    var
    LTxt := TText.Create(Owner);
    LTxt.TabStop := False;
    LTxt.Parent := LLyt;
    LTxt.Text := FPrompt[i];
    LTxt.Align := TAlignLayout.Top;
    LTxt.AutoSize := True;
    LTxt.WordWrap := False;
    LTxt.TextSettings := TxtSettingsTxtMessage;

    var
    LEdt := TEdit.Create(Owner);
    LEdt.Parent := LLyt;
    LEdt.Text := FValue[i];
    LEdt.Align := TAlignLayout.Top;
    LEdt.StyleLookup := FEdtStyleLookup;
    LEdt.TextSettings := TxtSettingsTxtMessage;
    LEdt.ReturnKeyType := TReturnKeyType.Next;
    FObjLstEdt.Add(LEdt);
  end;

  if FObjLstEdt.Count > 0 then
  begin
    FObjLstEdt.Last.ReturnKeyType := TReturnKeyType.Done;
    FObjLstEdt.Last.KillFocusByReturn := True;
  end;
end;

function TFmxMessageInputQuery.Prompt(
  AValue: array of string): TFmxMessageInputQuery;
begin
  Result := Self;
  SetLength(FPrompt, Length(AValue));

  for var i := 0 to Pred(Length(AValue)) do
  begin
    FPrompt[i] := AValue[i];
  end;
end;

function TFmxMessageInputQuery.TextMessage(AValue: string): TFmxMessageInputQuery;
begin
  raise Exception.Create('Not implemented TFmxMessageInputQuery.TextMessage');
end;

function TFmxMessageInputQuery.Value(
  AValue: array of string): TFmxMessageInputQuery;
begin
  Result := Self;

  SetLength(FValue, Length(AValue));

  for var i := 0 to Pred(Length(AValue)) do
  begin
    FValue[i] := AValue[i];
  end;
end;

end.
