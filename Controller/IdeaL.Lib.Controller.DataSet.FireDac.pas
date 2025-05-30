unit IdeaL.Lib.Controller.DataSet.FireDac;

interface

uses
  IdeaL.Lib.Controller.IDataSet,
  IdeaL.Lib.Controller.IConnection,

  System.SysUtils,
  System.Classes,
  System.StrUtils,

  Data.DB;

type
  IDataSet = IdeaL.Lib.Controller.IDataSet.IDataSet;
  TDSFireDAC = class(TInterfacedObject, IDataSet)
  private
    FConn: IConnection;
    { Private declarations }
  public
    constructor Create(AConn: IConnection);
    procedure QryExec(const AScript: string);
    procedure ConnExec(const ASql: string);
    procedure Commit();
    procedure RollBack(AName: string);
    procedure StartTransaction;
    function GetDataSet(const ASql: string): TDataSet;
    function GetDateTime: TDateTime;
    function ApplyUpdates(ADs: TDataSet): Integer;
    { Public declarations }
  protected
    { protected declarations }
  end;

implementation

uses
  IdeaL.Lib.Utils,
  IdeaL.Lib.Controller.Connection.FireDAC,

  FireDAC.Stan.Intf,
  FireDAC.Stan.Option,
  FireDAC.Stan.Param,
  FireDAC.Stan.Error,
  FireDAC.DatS,
  FireDAC.Phys.Intf,
  FireDAC.DApt.Intf,
  FireDAC.Stan.Async,
  FireDAC.DApt,
  FireDAC.Comp.DataSet,
  FireDAC.Comp.Client;

{ TDSFireDAC }

function TDSFireDAC.ApplyUpdates(ADs: TDataSet): Integer;
var
  LErrors: Integer;
  LError: EFDException;
begin
  if (TFDQuery(ADs).State in dsEditModes) and
    (TFDQuery(ADs).CachedUpdates)
  then
    ADs.Post;
  LErrors := TFDQuery(ADs).ApplyUpdates;
  if LErrors > 0 then
  begin
    LError := TFDQuery(ADs).RowError;
    raise Exception.Create(LError.Message);
  end;
end;

procedure TDSFireDAC.Commit;
begin
  FConn.Commit;
end;

procedure TDSFireDAC.ConnExec(const ASql: string);
begin
  FConn.Exec(ASql);
end;

constructor TDSFireDAC.Create(AConn: IConnection);
begin
  if not(AConn.Connection is TFDConnection) then
    raise Exception.Create('TDSFireDAC.Create is expecting a TFDConnection');
  FConn := AConn;
end;

function TDSFireDAC.GetDataSet(const ASql: string): TDataSet;
begin
  if not Assigned(FConn) then
    raise Exception.Create('TDSFireDAC.GetDataSet.FConn=NIL');
  if not Assigned(FConn.Connection) then
    raise Exception.Create('TDSFireDAC.GetDataSet.FConn.Connection=NIL');
  try
    Result := TFDQuery.Create(nil);
    TFDQuery(Result).Close;
    TFDQuery(Result).DisableControls;
    TFDQuery(Result).Connection := TFDConnection(FConn.Connection);
    TFDQuery(Result).CachedUpdates := True;
    TFDQuery(Result).SQL.Clear;
    TFDQuery(Result).SQL.Add(ASql.Trim);
{$IF defined(MSWINDOWS) and defined(DEBUG)}
    try
      TFDQuery(Result).SQL.SaveToFile('ScriptGetDataSet.sql');
    except

    end;
{$IFEND}
    if not TFDQuery(Result).SQL.Text.Trim.IsEmpty then
      TFDQuery(Result).Open;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TDSFireDAC.GetDateTime: TDateTime;
var
  LDs: TDataSet;
  LDriverName: string;
  LSql: string;
begin
  LDriverName := LowerCase(TFDConnection(FConn.Connection).DriverName);

  case AnsiIndexStr(LDriverName, ['fb', 'mysql', 'sqlite']) of
    0:
      LSql := 'select cast(''NOW'' as timestamp) as LDateTime from rdb$database'; // select current_timestamp as LDateTime from rdb$database
    1:
      LSql := 'SELECT NOW() as LDateTime';
    2:
      LSql := 'SELECT datetime(CURRENT_TIMESTAMP, ''localtime'') as LDateTime';
  end;

  LDs := GetDataSet(LSql);
  try
    Result := LDs.FieldByName('LDateTime').AsDateTime;
    LDs.Close;
  finally
    FreeAndNil(LDs);
  end;
end;

procedure TDSFireDAC.QryExec(const AScript: string);
var
  LQry: TFDQuery;
begin
  try
    LQry := TFDQuery.Create(nil);
    LQry.Connection := TFDConnection(FConn.Connection);
    LQry.Close;
    LQry.SQL.Add(AScript);
{$IF defined(MSWINDOWS) and defined(DEBUG)}
    try
      LQry.SQL.SaveToFile('ScriptQryExecSql.sql');
    except

    end;
{$IFEND}
    LQry.ExecSQL;
    LQry.Close;
  finally
    FreeAndNil(LQry);
  end;
end;

procedure TDSFireDAC.RollBack(AName: string);
begin
  TUtils.EventLog('TDSFireDAC.Roll1Back1');
  FConn.RollBack(AName);
  TUtils.EventLog('TDSFireDAC.Roll1Back2');
end;

procedure TDSFireDAC.StartTransaction;
begin
  FConn.StartTransaction;
end;

end.
