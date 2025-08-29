unit Unit1;

///
/// Comments
/// Please, create your own TButton CustomStyle, all depends on that
/// You must create it for Android, Windows and iOS
/// Use FixedWidth to make sure the button is going to be big enough
/// Create more than one CustomStyle if you need it
///
/// If you figure out how to get the width of the Component based on the
/// width of its Text, PLEASE, contact me
///
///
///
///
/// WOP or PoG (programação orientada à gambiarra in Portuguese)
///
/// You might notice that I have a TRectangle on the Background, it is
/// mandatory to have it, other wise you will get an AV on Mobile OSs
/// usually we always have an Rectangle on the background, so I guess you
/// don't need to worry about that, but this shit is true ¬¬
///
/// Also, you might notice that we have a TButton called btnSetFocus and we
/// call it in end of every last message. But why?
/// I didn't understand, let me know if you get it, but without this function
/// we always get an AV on Mobile OSs while closing the Application.
/// I guess it is because some TButton focus probelm, because it doesn't happen
/// with TRectangle, anyway, it is needed and you are welcome to contact me
/// if you figure it out
///
/// Kisses, peace, and keep you towel close
///

interface

uses
  IdeaL.Lib.PopUp.Message,

  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Ani, FMX.Layouts, FMX.Objects;

type
  TUtils = class
  private
    class var FMsgToast: TFmxObject;
    class var FMsgDialog: TFmxObject;
    class var FMsgInputQuery: TFmxObject;
    { private declarations }
  protected
    class procedure OnInitialization;
    { protected declarations }
  public
    class procedure ShowMessageOk(AParent: TFmxObject; AMsg, AHeader: string; ACloseDialogProc: TInputCloseDialogProc = nil);
    class procedure ShowMessageError(AParent: TFmxObject; AMsg, AHeader: string; ACloseDialogProc: TInputCloseDialogProc = nil);
    class procedure ShowMessageYesNo(AParent: TFmxObject; AMsg, AHeader: string; ACloseDialogProc: TInputCloseDialogProc = nil);

    class procedure Dialog(
      AParent: TFmxObject;
      AMsg, AHeader: string;
      AMsgBtnType: TMsgTypeButton;
      AMsgType: TMsgType = TMsgType.mtInformation;
      ACloseDialogProc: TInputCloseDialogProc = nil);
    class procedure InputQuery(
      AParent: TFmxObject;
      AHeader: string;
      APrompts, AValues: array of string;
      ACloseQueryProc: TInputCloseQueryProc = nil);
    class procedure Toast(AParent: TFmxObject; AMsg: string; ADelay: Integer = 2);
    { public declarations }
  published
    { published declarations }
  end;

  TForm1 = class(TForm)
    Button1: TButton;
    Layout1: TLayout;
    Button2: TButton;
    Button3: TButton;
    StyleBook1: TStyleBook;
    btnSetFocus: TButton;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure SetFocus(const AComponent: TControl);

    procedure CloseDialogProc(const AResult: TModalResult);
    procedure CloseInputQueryProc(const AResult: TModalResult; const AValues: array of string);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  System.StrUtils;

{$R *.fmx}


procedure TForm1.Button1Click(Sender: TObject);
begin
  TUtils.Toast(Self,
    'Look, it is a Toast message ' + DateTimeToStr(Now),
    Random(8) + 2 // Random delay
    );
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  TUtils.ShowMessageYesNo(Self,
    'Which option do you want?' +
    System.StrUtils.DupeString(sLineBreak + 'Test', 10),
    'Message Question ' + DateTimeToStr(Now),
    CloseDialogProc
    );
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  TUtils.InputQuery(Self,
    'InputQuery Header',
    ['Promp1', 'Promp2', 'Promp3'],
    ['Value1', EmptyStr, EmptyStr],
    CloseInputQueryProc
    );
end;

procedure TForm1.CloseDialogProc(const AResult: TModalResult);
begin
  SetFocus(btnSetFocus);

  case AResult of
    idYes:
      TUtils.ShowMessageYesNo(Self,
        'You pressed yes.' + sLineBreak + 'What will you choose now?' +
        System.StrUtils.DupeString(sLineBreak + 'Test', 15),
        'Message Question ' + DateTimeToStr(Now),
        CloseDialogProc
        );
    idNo:
      TUtils.ShowMessageOk(Self,
        'You pressed No' +
        System.StrUtils.DupeString(sLineBreak + 'Test', 5),
        'Message OK ' + DateTimeToStr(Now)
        );
  end;
end;

procedure TForm1.CloseInputQueryProc(const AResult: TModalResult;
  const AValues: array of string);
var
  LMsg: string;
  LHeader: string;
begin
  SetFocus(btnSetFocus);
  // do here any kind of validation
  LMsg := EmptyStr;
  case AResult of
    idCancel:
      begin
        LHeader := 'Cancel was pressed';
      end;
    idOk:
      begin
        LHeader := 'OK was pressed';
      end;
  end;

  for var I := 0 to Pred(Length(AValues)) do
    LMsg := LMsg + sLineBreak + 'Value' + I.ToString + ': ' + AValues[I];

  TUtils.ShowMessageOk(Self,
    LMsg,
    LHeader
    );
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Hiding the btnSetFocus, noone needs to see it
  btnSetFocus.Position.X := -100;
  btnSetFocus.Position.Y := -100;
end;

procedure TForm1.SetFocus(const AComponent: TControl);
var
  LCanFocus: Boolean;
begin
  TThread.CreateAnonymousThread(
    procedure
    begin
      TThread.Synchronize(nil,
        procedure()
        var
          LTabStopBefore: Boolean;
        begin
          LTabStopBefore := AComponent.TabStop;
          AComponent.TabStop := True;
          AComponent.SetFocus;
          AComponent.TabStop := LTabStopBefore;
        end);

    end).Start;
end;

{ TUtils }

class procedure TUtils.Dialog(
  AParent: TFmxObject;
AMsg, AHeader: string;
AMsgBtnType: TMsgTypeButton;
AMsgType: TMsgType;
ACloseDialogProc: TInputCloseDialogProc);
begin
{$IF defined(ANDROID) or defined(IOS)}
  {
    Thread was needed just in case that showing other msg straight way
    Shouldn't raise exceptions in other cases
    I'll fix it some day
  }

  TThread.CreateAnonymousThread(
    procedure
    begin
      TThread.Synchronize(nil,
        procedure
        begin
{$IFEND}
          if Assigned(FMsgDialog) then
            FreeAndNil(FMsgDialog);
          FMsgDialog :=
            IdeaL.Lib.PopUp.Message.TFmxMessage.GetInstance(AParent);
          var
          LBkg := IdeaL.Lib.PopUp.Message.TFmxMessage(FMsgDialog).BackgroundMsg;
          var
          LItem := LBkg.Dialog;

          // Backgroung
          // LDiag.BackgroundColor(TAlphaColorRec.Pink);
          // LDiag.BackgroundColorStroke(TAlphaColorRec.Red);

          // Message Text Settings
          var
          LTxtStgs := TTextSettings.Create(FMsgDialog);
          LTxtStgs.FontColor := TAlphaColorRec.Black;
          LTxtStgs.Font.Style := [TFontStyle.fsBold];
          LTxtStgs.HorzAlign := TTextAlign.Center;
          LTxtStgs.WordWrap := True;
          LItem.TextSettingsTxtMessage(LTxtStgs);
          LItem.TextMessage(AMsg);

          // Header Text Settings
          var
          LTxtStgsHeader := TTextSettings.Create(FMsgDialog);
          LTxtStgsHeader.FontColor := TAlphaColorRec.Black;
          LTxtStgsHeader.Font.Style := [TFontStyle.fsBold];
          LItem.TextSettingsTxtHeader(LTxtStgsHeader);
          LItem.TextHeader(AHeader);

		  // Button Text Settings
          var
          LTxtStgsBtn := TTextSettings.Create(FMsgDialog);
          LTxtStgsBtn.Font.Style := [TFontStyle.fsBold];
          LTxtStgsBtn.HorzAlign := TTextAlign.Center;
          LTxtStgsBtn.VertAlign := TTextAlign.Center;
          LItem.BtnTextSettings(LTxtStgsBtn);

          LItem.BtnStyleLookup('btnTransparent');
          LItem.ButtonType(AMsgBtnType);
          // LItem.MsgType(AMsgType);}
          LItem.BtnTxt('Ok', 'Cancelar', 'Sim', 'Não');

          // Changes its max sizes
          // LItem.WidthMax(100);

          LItem.CloseDialogProc(ACloseDialogProc);

          // BtnWidth
          case AMsgBtnType of
            TMsgTypeButton.mtbOk, TMsgTypeButton.mtbYesNo:
              LItem.BtnWidth(50);
            TMsgTypeButton.mtbOkCancel:
              LItem.BtnWidth(70);

          end;

          // Last thing before o Show
          LItem.Prepare;

          // Show
          IdeaL.Lib.PopUp.Message.TFmxMessage(FMsgDialog).Show;
{$IF  defined(ANDROID) or defined(IOS)}
        end);
    end).Start;
{$IFEND}
end;

class procedure TUtils.InputQuery(AParent: TFmxObject; AHeader: string;
APrompts, AValues: array of string; ACloseQueryProc: TInputCloseQueryProc);
var
  LPrompts, LValues: array of string;
begin
  SetLength(LPrompts, Length(APrompts));
  SetLength(LValues, Length(AValues));

  for var i := 0 to Pred(Length(APrompts)) do
  begin
    LPrompts[i] := APrompts[i];
    LValues[i] := AValues[i];
  end;
{$IF defined(ANDROID) or defined(IOS)}
  {
    Thread was needed just in case that showing other msg straight way
    Shouldn't raise exceptions in other cases
    I'll fix it some day
  }

  TThread.CreateAnonymousThread(
    procedure

    begin
      TThread.Synchronize(nil,
        procedure
        begin
{$IFEND}
          if Assigned(FMsgInputQuery) then
            FreeAndNil(FMsgInputQuery);
          FMsgInputQuery :=
            IdeaL.Lib.PopUp.Message.TFmxMessage.GetInstance(AParent);
          var
          LBkg := IdeaL.Lib.PopUp.Message.TFmxMessage(FMsgInputQuery).BackgroundMsg;
          var
          LItem := LBkg.InputQuery;

          // Backgroung
          // InpQry.BackgroundColor(TAlphaColorRec.Pink);
          // InpQry.BackgroundColorStroke(TAlphaColorRec.Red);

          // Prompt1 Text Settings
          var
          LTxtStgs := TTextSettings.Create(FMsgInputQuery);
          LTxtStgs.FontColor := TAlphaColorRec.Black;
          LTxtStgs.Font.Style := [TFontStyle.fsBold];
          LTxtStgs.HorzAlign := TTextAlign.Center;
          LTxtStgs.WordWrap := True;
          LItem.TextSettingsTxtMessage(LTxtStgs);

          // Header Text Settings
          var
          LTxtStgsHeader := TTextSettings.Create(FMsgInputQuery);
          LTxtStgsHeader.FontColor := TAlphaColorRec.Black;
          LTxtStgsHeader.Font.Style := [TFontStyle.fsBold];
          LItem.TextSettingsTxtHeader(LTxtStgsHeader);
          LItem.TextHeader(AHeader);

          LItem.BtnStyleLookup('btnTransparent');
          // LItem uses OkCancel buttons only
          LItem.BtnTxtOkCancel('Ok', 'Cancelar');

          // Changes its max sizes
          // LItem.WidthMax(100);

          LItem.CloseDialogProc(ACloseQueryProc);

          // Arrays
          LItem.Prompt(LPrompts);
          LItem.Value(LValues);

          // BtnWidth
          LItem.BtnWidth(70);

          // Last thing before o Show
          LItem.Prepare;

          // Show
          IdeaL.Lib.PopUp.Message.TFmxMessage(FMsgInputQuery).Show;
{$IF  defined(ANDROID) or defined(IOS)}
        end);
    end).Start;
{$IFEND}
end;

class procedure TUtils.OnInitialization;
begin
  FMsgToast := nil;
  FMsgDialog := nil;
  FMsgInputQuery := nil;
end;

class procedure TUtils.ShowMessageError(AParent: TFmxObject; AMsg, AHeader: string; ACloseDialogProc: TInputCloseDialogProc);
begin
  Dialog(AParent, AMsg, AHeader, TMsgTypeButton.mtbOk, TMsgType.mtError)
end;

class procedure TUtils.ShowMessageOk(AParent: TFmxObject; AMsg, AHeader: string; ACloseDialogProc: TInputCloseDialogProc);
begin
  Dialog(AParent, AMsg, AHeader, TMsgTypeButton.mtbOk, TMsgType.mtInformation, ACloseDialogProc);
end;

class procedure TUtils.ShowMessageYesNo(AParent: TFmxObject; AMsg, AHeader: string; ACloseDialogProc: TInputCloseDialogProc);
begin
  Dialog(AParent, AMsg, AHeader, TMsgTypeButton.mtbYesNo, TMsgType.mtQuestion, ACloseDialogProc)
end;

class procedure TUtils.Toast(AParent: TFmxObject; AMsg: string; ADelay: Integer);
begin
{$IF defined(ANDROID) or defined(IOS)}
  {
    Thread was needed just in case that showing other msg straight way
    Shouldn't raise exceptions in other cases
    I'll fix it some day
  }

  TThread.CreateAnonymousThread(
    procedure
    begin
      TThread.Synchronize(nil,
        procedure
        begin
{$IFEND}
          if not Assigned(FMsgToast) then
            FMsgToast :=
              IdeaL.Lib.PopUp.Message.TFmxMessage.GetInstance(AParent);
          var
          LBkg := IdeaL.Lib.PopUp.Message.TFmxMessage(FMsgToast).BackgroundMsg;
          var
          LItem := LBkg.Toast;

          // Backgroung
          // LItem.BackgroundColor(TAlphaColorRec.Pink);
          // LItem.BackgroundColorStroke(TAlphaColorRec.Red);
          // LItem.BackgroundOpacity(0.5);

          // Message Text Settings
          { var
            LTxtStgs := TTextSettings.Create(FMsgToast);
            LTxtStgs.FontColor := TAlphaColorRec.White;
            LTxtStgs.Font.Style := [TFontStyle.fsBold];
            LTxtStgs.HorzAlign := TTextAlign.Center;
            LTxtStgs.WordWrap := True;
            LItem.TextSettingsTxtMessage(LTxtStgs); }
          LItem.TextMessage(AMsg);

          // Changes its max sizes
          // LItem.WidthMax(100);

          // LItem.CloseDialogProc(ACloseDialogProc);

          // Delay
          LItem.Delay(ADelay);

          // Show
          IdeaL.Lib.PopUp.Message.TFmxMessage(FMsgToast).Show;
{$IF  defined(ANDROID) or defined(IOS)}
        end);
    end).Start;
{$IFEND}
end;

initialization

TUtils.OnInitialization;

end.
