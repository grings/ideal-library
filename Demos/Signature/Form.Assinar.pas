unit Form.Assinar;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  Form.Base, FMX.Objects, FMX.Controls.Presentation, FMX.Layouts, Form.Principal,
  Form.EmitePedido, Form.Resumo2;

type
  TFormAssinar = class(TFormBase)
    rctClient: TRectangle;
    lytToolBar: TLayout;
    rctToolBar: TRectangle;
    lblTitulo: TLabel;
    btnVoltar: TSpeedButton;
    lytBtSalvar: TLayout;
    imgSalvar: TPath;
    lytForm: TLayout;
    Rect_Assinatura: TRectangle;
    PathAssinatura: TPath;
    lytBtExcluir: TLayout;
    imgExcluir: TPath;
    imgAssinar: TImage;
    procedure btnVoltarClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Rect_AssinaturaMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure Rect_AssinaturaMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Single);
    procedure lytBtExcluirClick(Sender: TObject);
    procedure lytBtSalvarClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormAssinar: TFormAssinar;

implementation

{$R *.fmx}

uses Utils.Lib, DM.Dados, Pedidos.Services, FGX.Toasts;

procedure TFormAssinar.btnVoltarClick(Sender: TObject);
begin
 inherited;
 FormPrincipal.AbrirForm(TFormResumo2);
 TLibrary.MudarAba(FormPrincipal.tbCtrlMain, FormPrincipal.tbItemMain);
end;

procedure TFormAssinar.FormCreate(Sender: TObject);
begin
 inherited;
 rect_assinatura.autocapture := True;
end;

procedure TFormAssinar.lytBtExcluirClick(Sender: TObject);
begin
 inherited;
 rect_assinatura.fill.kind:=TbrushKind.none;
 pathAssinatura.data.clear;
end;

procedure TFormAssinar.lytBtSalvarClick(Sender: TObject);
var
aBs64: String;
bm: TBitmap;
begin
 inherited;

 rect_assinatura.fill.kind := TbrushKind.Bitmap;

 bm := pathassinatura.makescreenshot;
 Try
  rect_assinatura.fill.Bitmap.Bitmap := bm;
  aBs64 := TLibrary.Base64FromBitmap(bm);
 Finally
  bm.DisposeOf;
 End;

 pathAssinatura.data.clear;

 //Salvar na Base
 TPedidos.SalvarAssinatura(TLibrary.Protocolo, aBs64);
 TPedidos.CarregaPedido(TLibrary.Protocolo);

 {$IFNDEF MSWINDOWS}
  TfgToast.Show('Pedido assinado com sucesso', TfgToastDuration(0));
 {$ENDIF}

 //Voltar ao Resumo
 FormPrincipal.AbrirForm(TFormResumo2);
 TLibrary.MudarAba(FormPrincipal.tbCtrlMain, FormPrincipal.tbItemMain);

end;

procedure TFormAssinar.Rect_AssinaturaMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
 inherited;
 if ssleft in shift then
  pathAssinatura.data.moveto(TpointF.create(x,y));
end;

procedure TFormAssinar.Rect_AssinaturaMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Single);
begin
 inherited;
 if ssleft in shift then
  begin
    pathAssinatura.data.LineTo(TPointF.Create(x,y));
    Rect_Assinatura.repaint;
  end;
end;

end.

class function TLibrary.Base64FromBitmap(Bitmap: TBitmap): string;
var
  Input: TBytesStream;
  Output: TStringStream;
begin
  Input := TBytesStream.Create;
  try
    Bitmap.SaveToStream(Input);
    Input.Position := 0;
    Output := TStringStream.Create('', TEncoding.ASCII);
    try
      Soap.EncdDecd.EncodeStream(Input, Output);
      Result := Output.DataString;
    finally
      Output.DisposeOf;
    end;
  finally
    Input.DisposeOf;
  end;
end;

class function TLibrary.BitmapFromBase64(const base64: string): TBitmap;
var
  Input: TStringStream;
  Output: TBytesStream;
begin
  Input := TStringStream.Create(base64, TEncoding.ASCII);
  try
    Output := TBytesStream.Create;
    try
      Soap.EncdDecd.DecodeStream(Input, Output);
      Output.Position := 0;
      Result := TBitmap.Create;
      try
        Result.LoadFromStream(Output);
      except
        Result.DisposeOf;
        raise;
      end;
    finally
      Output.DisposeOf;
    end;
  finally
    Input.DisposeOf;
  end;
end;