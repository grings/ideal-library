unit Onboarding.View;
// {$SCOPEDENUMS ON}

interface

uses
  System.SysUtils, System.Generics.Collections, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, FMX.Layouts, FMX.Objects, FMX.Ani, FMX.Controls.Presentation, FMX.Edit,
  FMX.Effects{, View.Contract};

type

  TOnboardingDirection = (Undefined, ToLeft, ToRight);

  TOnboardingView = class(TFrame{, IView})
    Viewport: TLayout;
    FloatAnimation: TFloatAnimation;
    Layout1: TLayout;
    Layout2: TLayout;
    LayoutBottom: TLayout;
    ButtonBack: TRoundRect;
    Text1: TText;
    ButtonNext: TRoundRect;
    Text2: TText;
    Image1: TImage;
    ImageBackground: TImage;
    FloatAnimationBackground: TFloatAnimation;
    Image2: TImage;
    Layout3: TLayout;
    Image3: TImage;
    ButtonConfirm: TRoundRect;
    Text3: TText;
    FloatAnimationView: TFloatAnimation;
    Layout4: TLayout;
    Layout5: TLayout;
    Button1: TButton;
    procedure FrameResize(Sender: TObject);
    procedure FrameMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure FrameMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure FrameMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure ButtonBackClick(Sender: TObject);
    procedure ButtonNextClick(Sender: TObject);
    procedure ButtonConfirmClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);

  private
    { Private declarations }
    FSteps: TObjectList<TControl>;
    FIsMouseDown: Boolean;
    FLastMouseXPos: Single;
    FLastOnboardingDirection: TOnboardingDirection;
    FIndex: Integer;
    procedure ToIndex(AIndex: Integer);
  protected
    procedure Loaded; override;
  public
    { Public declarations }
    procedure AddStep(AStep: TControl);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoViewShow;
    procedure DoViewHide;
  end;

implementation

uses
  {View.Provider, Cookie.App,} System.JSON;

{$R *.fmx}

procedure TOnboardingView.AddStep(AStep: TControl);
begin
  AStep.Position.X := Self.Width * FSteps.Count;
  AStep.Width := Self.Width;
  AStep.Align := TAlignLayout.Left;
  AStep.Parent := Viewport;

  FSteps.Add(AStep);
end;

procedure TOnboardingView.ButtonNextClick(Sender: TObject);
begin
  ToIndex(FIndex + 1);
end;

procedure TOnboardingView.Button1Click(Sender: TObject);
begin
  ShowMessage('Doing something');
end;

procedure TOnboardingView.ButtonBackClick(Sender: TObject);
begin
  ToIndex(FIndex - 1);
end;

procedure TOnboardingView.ButtonConfirmClick(Sender: TObject);
begin

  TThread.CreateAnonymousThread(
    procedure
    begin
      {TAppCookie.DefaultAppCookie.RemovePair('skip_onboarding').Free;
      TAppCookie.DefaultAppCookie.AddPair('skip_onboarding', TJsonBool.Create(True));
      TViewManager.CloseView('*');
      TViewManager.OpenView('LoginView');}

    end).Start;
end;

constructor TOnboardingView.Create(AOwner: TComponent);
begin

  inherited;
end;

destructor TOnboardingView.Destroy;
begin
  FSteps.Free;
  inherited;

end;

procedure TOnboardingView.DoViewHide;
begin

end;

procedure TOnboardingView.DoViewShow;
begin
  FloatAnimationView.StartValue := Self.Height;
  FloatAnimationView.Start;
end;

procedure TOnboardingView.FrameMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  FloatAnimation.Stop;
  FloatAnimationBackground.Stop;
  FIsMouseDown := True;
  FLastMouseXPos := X;
  FLastOnboardingDirection := TOnboardingDirection.Undefined;
end;

procedure TOnboardingView.FrameMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
var
  LDistance: Single;
begin
  if FIsMouseDown then
  begin
    LDistance := Viewport.Position.X + FIndex * Self.Width;
    if Abs(LDistance) < Self.Width + 100 then
    begin
      Viewport.Position.X := Viewport.Position.X - FLastMouseXPos + X;
      ImageBackground.Position.X := ((Viewport.Position.X - FLastMouseXPos + X) / 5) - (Self.Width * 2);
    end;
    if X > FLastMouseXPos then
      FLastOnboardingDirection := TOnboardingDirection.ToRight
    else if X < FLastMouseXPos then
      FLastOnboardingDirection := TOnboardingDirection.ToLeft
    else
      FLastOnboardingDirection := TOnboardingDirection.Undefined;
    FLastMouseXPos := X;
  end;
end;

procedure TOnboardingView.FrameMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  LDistance: Single;
begin
  FIsMouseDown := False;
  LDistance := Viewport.Position.X + FIndex * Self.Width;
  if (FLastOnboardingDirection = TOnboardingDirection.ToRight) and (LDistance > 100) then
    ToIndex(FIndex - 1)
  else if (FLastOnboardingDirection = TOnboardingDirection.ToLeft) and (LDistance < -100) then
    ToIndex(FIndex + 1)
  else
    ToIndex(FIndex);
end;

procedure TOnboardingView.FrameResize(Sender: TObject);
var
  I: Integer;
begin
  Viewport.Position.X := (FIndex * Self.Width) * -1;
  Viewport.Width := Self.Width * FSteps.Count;
  Viewport.Height := Self.Height;
  FloatAnimation.StopValue := (FIndex * Self.Width) * -1;

  ImageBackground.Position.X := ((FIndex * Self.Width / 5) * -1) - (Self.Width * 2);
  ImageBackground.Width := Self.Width * FSteps.Count * 2;
  ImageBackground.Height := Self.Height;
  FloatAnimationBackground.StopValue := ((FIndex * Self.Width / 5) * -1) - (Self.Width * 2);

  for I := 0 to FSteps.Count - 1 do
    FSteps.Items[I].Width := Self.Width;

end;

procedure TOnboardingView.Loaded;
begin

  FSteps := TObjectList<TControl>.Create;
  FIsMouseDown := False;
  Self.AutoCapture := True;

  AddStep(Layout1);
  AddStep(Layout2);
  AddStep(Layout3);

  FIndex := 0;

  inherited;

end;

procedure TOnboardingView.ToIndex(AIndex: Integer);
begin
  if (AIndex >= 0) and (AIndex < FSteps.Count) then
    FIndex := AIndex;

  FloatAnimation.StartValue := Viewport.Position.X;
  FloatAnimation.StopValue := (FIndex * Self.Width) * -1;
  FloatAnimation.Start;

  FloatAnimationBackground.StartValue := -(Self.Width * 2);
  FloatAnimationBackground.StopValue := ((FIndex * Self.Width / 5) * -1) - (Self.Width * 2);
  FloatAnimationBackground.Start;

  ButtonBack.Visible := FIndex > 0;
  ButtonNext.Visible := FIndex < FSteps.Count - 1;
  ButtonConfirm.Visible := FIndex = FSteps.Count - 1

end;

initialization

//TViewManager.RegisterView('OnboardingView', TOnboardingView);

finalization

//TViewManager.UnregisterView('OnboardingView');

end.
