unit IdeaL.Lib.Model.RequestEndPoint;

interface

uses
  System.SysUtils,

  IdeaL.Lib.CustomRtti;

type

  [TLocalObject('RequestEndPoint', ddbNone)]
  TRequestEndPoint = class(TCustomTable)
  private
    [TJsonField('Params', mftString)]
    FParams: string;
    procedure SetParams(const Value: string);
    { private declarations }
  protected
    { protected declarations }
  public
    property Params: string read FParams write SetParams;
    { public declarations }
  published
    { published declarations }
  end;

  [TLocalObject('RequestEndPoint', ddbNone)]
  TRequestEndPointList = class(TCustomTableList<TRequestEndPoint>)
  private
    { private declarations }
  protected
    { protected declarations }
  public
    { public declarations }
  published
    { published declarations }
  end;

implementation

{ TRequestEndPoint }

procedure TRequestEndPoint.SetParams(const Value: string);
begin
  FParams := Value;
  if not(FParams.EndsWith('=')) then
    FParams := FParams + '=';
end;

end.
