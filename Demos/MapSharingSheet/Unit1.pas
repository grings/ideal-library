unit Unit1;

(*

  Thanks for Georges for the help

  Tests:

  Delphi 11.1
  Samsung Galaxy S21, Android12
  Samsung Galaxy J7 Pro, Android8.1
  Redmi Note 7, Android10

*)

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.Edit, FMX.Controls.Presentation;

type
  TForm1 = class(TForm)
    Edit1: TEdit;
    ClearEditButton1: TClearEditButton;
    Edit2: TEdit;
    ClearEditButton2: TClearEditButton;
    Edit3: TEdit;
    Button2: TButton;
    Layout1: TLayout;
    Button1: TButton;
    procedure Button2Tap(Sender: TObject; const Point: TPointF);
    procedure Button1Tap(Sender: TObject; const Point: TPointF);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  System.StrUtils,
  IdeaL.Lib.Utils
{$IFDEF ANDROID}
    ,
  Androidapi.Helpers,
  Androidapi.JNI.GraphicsContentViewText
{$ENDIF}
    ;

{$R *.fmx}


procedure TForm1.Button1Tap(Sender: TObject; const Point: TPointF);
begin
  { please, take care yourself of your Decimal Separator pattern }
  FormatSettings.DecimalSeparator := '.';

  TUtils.OpenMapShareSheet(
    StrToFloat(IfThen(Edit1.Text.Trim.IsEmpty, '0', Edit1.Text)),
    StrToFloat(IfThen(Edit2.Text.Trim.IsEmpty, '0', Edit2.Text))
    );
end;

procedure TForm1.Button2Tap(Sender: TObject; const Point: TPointF);
begin
  TUtils.OpenMap(Edit3.Text);
end;

end.
