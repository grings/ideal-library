unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  FMX.Layouts, FMX.Controls.Presentation, FMX.StdCtrls, FMX.TabControl;

type
  TForm1 = class(TForm)
    rctSignature: TRectangle;
    lytForm: TLayout;
    pthSignature: TPath;
    glytButton: TGridLayout;
    btnClear: TButton;
    tbcMain: TTabControl;
    tbiSign: TTabItem;
    tbiImage: TTabItem;
    imgSign: TImage;
    btnGet: TButton;
    procedure FormCreate(Sender: TObject);
    procedure rctSignatureMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure rctSignatureMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Single);
    procedure btnGetClick(Sender: TObject);
    procedure btnClearTap(Sender: TObject; const Point: TPointF);
    procedure btnGetTap(Sender: TObject; const Point: TPointF);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}


procedure TForm1.btnClearTap(Sender: TObject; const Point: TPointF);
begin
  rctSignature.Fill.Kind := TBrushKind.None;
  pthSignature.Data.Clear;
end;

procedure TForm1.btnGetClick(Sender: TObject);
begin
{$IFDEF MSWINDOWS}
  if (Sender is TControl) and (Assigned(TControl(Sender).OnTap)) then
    TControl(Sender).OnTap(Sender, TPointF.Create(0, 0));
{$ENDIF}
end;

procedure TForm1.btnGetTap(Sender: TObject; const Point: TPointF);
begin
  imgSign.Bitmap := pthSignature.MakeScreenshot;
  tbcMain.ActiveTab := tbiImage;

  {
    Would you like to get the Img base64?
    var
    bm: TBitmap;
    begin
    bm := pathassinatura.makescreenshot;
    Try
    aBs64 := Base64FromBitmap(bm);
    Finally
    bm.DisposeOf;
    End;
    end;

    Obviously Base64FromBitmap dosen't exist, please, use whatever you want to convert it to Base64
  }
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  rctSignature.AutoCapture := True;
  tbcMain.ActiveTab := tbiSign;
end;

procedure TForm1.rctSignatureMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  if ssLeft in Shift then
    pthSignature.Data.MoveTo(TPointF.Create(X, Y));
end;

procedure TForm1.rctSignatureMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Single);
begin
  if ssLeft in Shift then
  begin
    pthSignature.Data.LineTo(TPointF.Create(X, Y));
    rctSignature.Repaint;
  end;
end;

end.
