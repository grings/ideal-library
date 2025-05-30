unit Unit1;

{

Tests on:
D11.1
Samsung Galaxy S21, Android12
Redmi Note 7, Android10
Galaxy J7 Pro, Android8.1

}

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Edit,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Memo.Types, FMX.ScrollBox,
  FMX.Memo;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    edtCurrency: TEdit;
    Label2: TLabel;
    edtDate: TEdit;
    edtCep: TEdit;
    Label3: TLabel;
    edtCpfCnpj: TEdit;
    Label4: TLabel;
    edtPhoneNumber: TEdit;
    Label5: TLabel;
    procedure edtCurrencyKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure edtCurrencyPainting(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
    procedure edtCurrencyTap(Sender: TObject; const Point: TPointF);
    procedure FormCreate(Sender: TObject);
    procedure edtCurrencyChangeTracking(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    // Must check if the Backspace is tapped, better when you work with inherited Forms
    FBackspaceTapped: Boolean;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  System.StrUtils,

  IdeaL.Lib.Utils;

{$R *.fmx}


procedure TForm1.edtCurrencyChangeTracking(Sender: TObject);
begin
{$IFDEF ANDROID}
  TThread.CreateAnonymousThread(
    procedure
    begin
{$ENDIF}
      TEdit(Sender).OnChangeTracking := nil;
      try
{$IFDEF ANDROID}
        TThread.Sleep(50);
{$ENDIF}
        TThread.ForceQueue(nil,
          procedure
          var
            LText: string;
          begin
            LText := TEdit(Sender).Text;
            case AnsiIndexText(TEdit(Sender).Name, [
              edtCurrency.Name,
              edtDate.Name,
              edtCep.Name,
              edtCpfCnpj.Name,
              edtPhoneNumber.Name
              ]) of
              0: // Currency
                LText := TUtils.FormatCurrency(LText, 2, FBackspaceTapped);

              1: // Date
                LText := TUtils.FormatDate(TUtils.JustNumber(LText));

              2: // CEP
                LText := TUtils.FormatCep(TUtils.JustNumber(LText));

              3: // CPF / CNPJ
                LText := TUtils.FormatCpfCnpj(TUtils.JustNumber(LText));

              4: // Phone Number
                LText := TUtils.FormatPhoneNumber(TUtils.JustNumber(LText));
            end;
            TEdit(Sender).Text := LText;
            FBackspaceTapped := False;
          end);
      finally
        TEdit(Sender).OnChangeTracking := edtCurrencyChangeTracking;
      end;
{$IFDEF ANDROID}
    end).Start;
{$ENDIF}
end;

procedure TForm1.edtCurrencyKeyDown(Sender: TObject; var Key: Word;
var KeyChar: Char; Shift: TShiftState);
begin
  FBackspaceTapped := Key = vkBack;
end;

procedure TForm1.edtCurrencyPainting(Sender: TObject; Canvas: TCanvas;
const ARect: TRectF);
begin
  if TEdit(Sender).IsFocused then
    TEdit(Sender).SelStart := TEdit(Sender).Text.Length;
end;

procedure TForm1.edtCurrencyTap(Sender: TObject; const Point: TPointF);
begin
  TEdit(Sender).SelStart := Length(TEdit(Sender).Text);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FormatSettings.LongDateFormat := 'dd' + FormatSettings.DateSeparator + 'mm' + FormatSettings.DateSeparator + 'yyyy';
end;


procedure TForm1.FormShow(Sender: TObject);
const
  CFilterChar = '0123456789';
begin
  edtCurrency.FilterChar := CFilterChar + FormatSettings.ThousandSeparator + FormatSettings.DecimalSeparator;
  edtCurrency.Text := '0' + FormatSettings.DecimalSeparator + '00';

  edtDate.FilterChar := CFilterChar + FormatSettings.DateSeparator;
  edtDate.MaxLength := FormatSettings.LongDateFormat.Length;
  edtDate.Text := FormatDateTime(FormatSettings.LongDateFormat, Now);

  edtCep.FilterChar := CFilterChar + '.-';
  edtCep.MaxLength := 10;

  edtCpfCnpj.FilterChar := CFilterChar + '.-/';
  edtCpfCnpj.MaxLength := 18;

  edtPhoneNumber.FilterChar := CFilterChar + '() -';
  edtPhoneNumber.MaxLength := 15;
end;

end.
