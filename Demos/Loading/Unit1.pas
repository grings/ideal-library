unit Unit1;

interface

uses
  IdeaL.Lib.PopUp.Loading,

  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls;

type
  TUtils = class
  private
    class var FFmxLoading: TFmxObject;
    { private declarations }
  protected
    class procedure OnInitialization;
    { protected declarations }
  public
    class procedure FmxLoadingChangeMessage(AMsg: string);
    class procedure FmxLoadingHide;
    class function FmxLoadingShow(
      AParent: TFmxObject;
      AMsg: string;
      ALenght: Single = -1
      ): TFmxLoading; overload;
    class function FmxLoadingShow(
      AMsg: string;
      ALenght: Single = -1
      ): TFmxLoading; overload;
    { public declarations }
  published
    { published declarations }
  end;

  TForm1 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button1Tap(Sender: TObject; const Point: TPointF);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FThread: TThread;
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
  Button1Tap(Sender, TPointF.Create(0, 0));
end;

procedure TForm1.Button1Tap(Sender: TObject; const Point: TPointF);
begin
  {
    TLoading needs to run in a Thread, but since it is not a Thread sample,
    please do not close the APP while the Thread is Running.
    It will not raise TLoading memoryleak, it just that I don't want to manage
    the Thread terminate
  }

  // TUtils.LoadingShow('It is about to start...');

  FThread := TThread.CreateAnonymousThread(
    procedure
    begin
      try
        TThread.Queue(nil,
          procedure
          begin
            TUtils.FmxLoadingShow('It is about to start...' +
              System.StrUtils.DupeString(sLineBreak + 'Test', 20)
              );
          end);
        TThread.Sleep(2000);
        if TThread.CurrentThread.CheckTerminated then
          Exit;
        for var i := 10 downto 0 do
        begin
          if TThread.CurrentThread.CheckTerminated then
            Break;
          TUtils.FmxLoadingChangeMessage(i.ToString + ' sec remaining');
          TThread.Sleep(1000);
        end;
      finally
        FThread := nil;
        if not TThread.CurrentThread.CheckTerminated then
          TThread.Synchronize(nil,
            procedure
            begin
              TUtils.FmxLoadingHide;

              TUtils.FmxLoadingShow('It is about to start... 2');
              // Call it once just to simulate multiple follwed loadings
              FThread := TThread.CreateAnonymousThread(
                procedure
                begin
                  try
                    TThread.Sleep(2000);
                    if TThread.CurrentThread.CheckTerminated then
                      Exit;
                    for var i := 10 downto 0 do
                    begin
                      if TThread.CurrentThread.CheckTerminated then
                        Break;
                      TUtils.FmxLoadingChangeMessage(i.ToString + ' sec remaining 2');
                      TThread.Sleep(1000);
                    end;
                  finally
                    FThread := nil;
                    if not TThread.CurrentThread.CheckTerminated then
                      TThread.Synchronize(nil,
                        procedure
                        begin
                          TUtils.FmxLoadingHide;
                        end);
                  end;
                end);
              FThread.Start;
            end);
      end;
    end);
  FThread.Start;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FThread := nil;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  if Assigned(FThread) then
  begin
    FThread.Terminate;
    while Assigned(FThread) do
      Sleep(200);
  end;
end;

{ TUtils }

class procedure TUtils.FmxLoadingChangeMessage(AMsg: string);
begin
  if not(Assigned(FFmxLoading)) or (Trim(FFmxLoading.Name).IsEmpty) then
    Exit;
  TThread.Synchronize(nil,
    procedure
    begin
      IdeaL.Lib.PopUp.Loading.TFmxLoading(FFmxLoading)
        .BackgroundLoad
        .Loading
        .TextMessage(AMsg);
    end);
end;

class procedure TUtils.FmxLoadingHide;
begin
  if Assigned(FFmxLoading) or (Trim(FFmxLoading.Name).IsEmpty) then
    IdeaL.Lib.PopUp.Loading.TFmxLoading(FFmxLoading).Hide;
end;

class function TUtils.FmxLoadingShow(AMsg: string;
ALenght: Single): TFmxLoading;
begin
  Result := FmxLoadingShow(Application.MainForm, AMsg, ALenght);
end;

class function TUtils.FmxLoadingShow(AParent: TFmxObject; AMsg: string;
ALenght: Single): TFmxLoading;
begin
  if Assigned(FFmxLoading) then
    FreeAndNil(FFmxLoading);
  FFmxLoading :=
    IdeaL.Lib.PopUp.Loading.TFmxLoading.GetInstance(AParent);
  var
  LBkg := IdeaL.Lib.PopUp.Loading.TFmxLoading(FFmxLoading).BackgroundLoad;

  var
  LItem := LBkg.Loading;

  // Message Text Settings
  var
  LTxtStgs := TTextSettings.Create(FFmxLoading);
  LTxtStgs.FontColor := TAlphaColorRec.Black;
  LTxtStgs.Font.Style := [TFontStyle.fsBold];
  LTxtStgs.HorzAlign := TTextAlign.Center;
  LTxtStgs.WordWrap := True;
  LItem.TextSettingsTxtMessage(LTxtStgs);
  LItem.TextMessage(AMsg);

  LItem.AniColor(TAlphaColorRec.Red);

  // LItem.AniLoadingClass(YourNewCustomLoadingClassHere)

  // Show
  IdeaL.Lib.PopUp.Loading.TFmxLoading(FFmxLoading).Show;
end;

class procedure TUtils.OnInitialization;
begin
  FFmxLoading := nil;
end;

initialization

TUtils.OnInitialization;

end.
