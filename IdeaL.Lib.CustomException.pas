unit IdeaL.Lib.CustomException;

interface

uses
  System.SysUtils,

  FMX.Forms;

type

  TCustomException = class
  private
    { private declarations }
  protected
    { protected declarations }
  public
    constructor Create;

    procedure Tryy(Sender: TObject; E: Exception);
    { public declarations }
  published
    { published declarations }
  end;

implementation

{ TCustomException }

constructor TCustomException.Create;
begin
  inherited;
  Application.OnException := Tryy;
end;

procedure TCustomException.Tryy(Sender: TObject; E: Exception);
begin

end;

var
  MyException: TCustomException;

initialization

  ;
MyException := TCustomException.Create;

finalization

  ;
MyException.Free;

end.
