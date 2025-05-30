unit Unit1;

interface

uses
  IdeaL.Lib.PopUp.DateTimePicker,

  System.SysUtils, System.Types, System.UITypes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Edit,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts, FMX.DateTimeCtrls,
  FMX.ListBox, System.Classes, FMX.Objects;

type
  TUtils = class
  private
    class var FDateTimePicker: TDateTimePicker;

    class procedure OnInitialization;
    { private declarations }
  protected
    { protected declarations }
  public
    class procedure DateTimePickerShow(
      AParent: TFmxObject;
      ADateTime: TDateTime;
      ACallbackOk: TProc<TDateTime>;
      AType: TPickerType);
    class procedure DatePickerShow(
      AParent: TFmxObject;
      ADateTime: TDateTime;
      ACallbackOk: TProc<TDateTime>);
    class procedure TimePickerShow(
      AParent: TFmxObject;
      ADateTime: TDateTime;
      ACallbackOk: TProc<TDateTime>);
    { public declarations }
  published
    { published declarations }
  end;

  TForm1 = class(TForm)
    Layout1: TLayout;
    Button1: TButton;
    Button2: TButton;
    Edit1: TEdit;
    lytBackground: TLayout;
    StyleBook1: TStyleBook;
    Button3: TButton;
    Button4: TButton;
    DateEdit1: TDateEdit;
    TimeEdit1: TTimeEdit;
    procedure FormCreate(Sender: TObject);
    procedure Button4Tap(Sender: TObject; const Point: TPointF);
    procedure Button1Click(Sender: TObject);
    procedure Button1Tap(Sender: TObject; const Point: TPointF);
    procedure Button2Tap(Sender: TObject; const Point: TPointF);
    procedure Button3Tap(Sender: TObject; const Point: TPointF);
  private
    procedure OnTimePickerOk(AValue: TDateTime);
    procedure OnDatePickerOk(AValue: TDateTime);
    { Private declarations }
  public
  constructor Create(AOwner: TComponent); override;
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  System.DateUtils;

{$R *.fmx}

procedure TForm1.Button1Click(Sender: TObject);
begin
{$IFDEF MSWINDOWS}
  if Assigned(TControl(Sender).OnTap) then
    TControl(Sender).OnTap(Sender, TPointF.Create(0, 0));
{$ENDIF}
end;

procedure TForm1.Button1Tap(Sender: TObject; const Point: TPointF);
begin
  TimeEdit1.OpenPicker;
end;

procedure TForm1.Button2Tap(Sender: TObject; const Point: TPointF);
begin
  DateEdit1.OpenPicker;
end;

procedure TForm1.Button3Tap(Sender: TObject; const Point: TPointF);
begin
  TUtils.TimePickerShow(
    lytBackground,
    StrToDateTime(Edit1.Text),
    OnTimePickerOk
    );
end;

procedure TForm1.Button4Tap(Sender: TObject; const Point: TPointF);
begin
  TUtils.DatePickerShow(
    lytBackground,
    StrToDateTime(Edit1.Text),
    OnDatePickerOk
    );
end;

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited;

end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Edit1.Text := DateTimeToStr(Now);
end;

procedure TForm1.OnDatePickerOk(AValue: TDateTime);
begin
  Edit1.Text := DateTimeToStr(AValue);
end;

procedure TForm1.OnTimePickerOk(AValue: TDateTime);
begin
  Edit1.Text := DateTimeToStr(AValue);
end;

{ TUtils }

class procedure TUtils.DatePickerShow(
      AParent: TFmxObject;
      ADateTime: TDateTime;
      ACallbackOk: TProc<TDateTime>);
begin
  DateTimePickerShow(AParent, ADateTime, ACallbackOk, TPickerType.ptDate);
end;

class procedure TUtils.DateTimePickerShow(AParent: TFmxObject;
  ADateTime: TDateTime; ACallbackOk: TProc<TDateTime>; AType: TPickerType);
begin
  if Assigned(FDateTimePicker) and not (Trim(FDateTimePicker.Name).IsEmpty) then
    FreeAndNil(FDateTimePicker);
  FDateTimePicker :=
    TDateTimePicker.GetInstance(AParent)
    .Text('OK', 'Cancelar')
    .BtnStyleLookup('btnTransparent')
    .DefaultBackgroundColor(TAlphaColorRec.Brown)
    .CallbackOk(ACallbackOk)
    .Prepare
    .Show(ADateTime, AType);
end;

class procedure TUtils.OnInitialization;
begin
  FDateTimePicker := nil;
end;

class procedure TUtils.TimePickerShow(AParent: TFmxObject; ADateTime: TDateTime;
  ACallbackOk: TProc<TDateTime>);
begin
  DateTimePickerShow(AParent, ADateTime, ACallbackOk, TPickerType.ptTime);
end;

initialization

TUtils.OnInitialization

end.
