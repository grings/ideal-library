unit Unit1;

{
Tested on:

Delphi11.1
Android 10 - Xiaomi Redmi Note 7
Android 12 - Samsung Galaxy S21
iPhone 7, iOS 15.4, MAC OS BigSur
}

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, Onboarding.View,
  IdeaL.Lib.View.TabControl;

type
  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    FOnBoarding: TOnboardingView;
    FIlTabControl: TIdeaLTabControl;

    procedure DoTabChange(Sender: TObject);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.DoTabChange(Sender: TObject);
begin
  var
  LBool := FIlTabControl.TabIndex =  Pred(FIlTabControl.TabCount);
  FIlTabControl.lytBottomCenter.Visible := not LBool;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FOnBoarding := TOnboardingView.Create(Self);
  FIlTabControl := TIdeaLTabControl.Create(Self);
  FIlTabControl.Parent := Self;
  FIlTabControl.OnChange := DoTabChange;
  FIlTabControl.AddItem(FOnBoarding.Layout1);
  FIlTabControl.AddItem(FOnBoarding.Layout2);
  FIlTabControl.AddItem(FOnBoarding.Layout3);
  FIlTabControl.Start;
end;

end.
