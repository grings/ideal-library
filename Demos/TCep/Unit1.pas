unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts, FMX.Memo.Types,
  FMX.ScrollBox, FMX.Memo, FMX.Edit;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Edit1: TEdit;
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
  IdeaL.Lib.Cep;

{$R *.fmx}

procedure TForm1.Button1Click(Sender: TObject);
var
  LCep: TCepRec; // It's a RECORD
begin
  Memo1.Lines.Clear;
  var LJson := TCep.Get(Edit1.Text);
  Memo1.Lines.Add(LJson);

  LCep.LoadFromJson(LJson);

  Memo1.Lines.Add(LCep.ToJson);
end;

end.
