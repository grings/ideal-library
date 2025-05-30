unit Api.ReceitaWs.View.FormMain;

{
  Nao precisa de nenhuma permissao para consultar, nem de internet
}

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Memo.Types, FMX.ScrollBox,
  FMX.Memo, FMX.Edit;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    edtCnpj: TEdit;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  IdeaL.Lib.Api.ReceitaWs;

{$R *.fmx}


procedure TForm1.Button1Click(Sender: TObject);
var
  LJson: string;
  LEmp: IdeaL.Lib.Api.ReceitaWs.TEmpresa;
begin
  Memo1.Lines.Clear;
  LJson := TReceitaWs.Consultar(edtCnpj.Text);
  LEmp := TEmpresa.Create(LJson);
  try
    Memo1.Lines.Add(LEmp.Cnpj);
    Memo1.Lines.Add(LEmp.RazaoSocial);
  finally
    FreeAndNil(LEmp);
  end;
end;

end.
