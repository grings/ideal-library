unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts;

type
  TForm4 = class(TForm)
    GridLayout1: TGridLayout;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form4: TForm4;

implementation

uses
  Utils;

{$R *.fmx}

procedure TForm4.Button1Click(Sender: TObject);
begin
  TUtils.GetPermissionReadWriteExternalStorage(
    procedure
    begin
      ShowMessage('The permission was granted');
    end,
    procedure
    begin
      ShowMessage('The permission was NOT granted');
    end
    );
end;

end.
