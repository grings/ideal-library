unit IdeaL.Lib.Controller.ZeosDataSet;

interface

uses
  System.Classes,
  System.SysUtils,

  Data.DB,

  IdeaL.Lib.Controller.IDataSet,

  ZAbstractRODataset,
  ZAbstractDataset,
  ZDataset,
  ZAbstractConnection,
  ZConnection;

type
  TDMZeosDataSet = class(TComponent, IDataSet)
  private
    FDbConnection: TZConnection;

    procedure SetDbConnection(Value: TComponent);
    function GetDbConnection: TComponent;
    { Private declarations }
  public
    constructor Create(AOwner: TComponent); // override;
    destructor Destroy; override;

    property Connection: TZConnection read FDbConnection;
    procedure ConnClose();
    function DataBasePath(): string;
    procedure QryExec(const ASql: string);
    procedure ConnExec(const ASql: string);
    function GetDataSet(const ASql: string): TDataSet;
    procedure Commit();
    procedure RollBack(AName: string);
    procedure StartTransaction();

    function GetDateTime: TDateTime;
    function GetResultJson(const ASql: string): string;

    function ApplyUpdates(ADs: TDataSet): Integer;
    property DbConnection: TComponent read GetDbConnection write SetDbConnection;
  end;

var
  DMZeosDataSet: TDMZeosDataSet;

implementation

uses
  IdeaL.Lib.Controller.ZeosConnection;

{ TZeosDataSet }

function TDMZeosDataSet.ApplyUpdates(ADs: TDataSet): Integer;
begin
  Result := 0;
  TZQuery(ADs).ApplyUpdates;
end;

procedure TDMZeosDataSet.ConnClose;
begin
  FDbConnection.Connected := False;
end;

procedure TDMZeosDataSet.Commit;
begin
  FDbConnection.Commit;
end;

procedure TDMZeosDataSet.ConnExec(const ASql: string);
begin
  if not(FDbConnection.Connected) then
    FDbConnection.Connected := True;
  FDbConnection.ExecuteDirect(ASql);
end;

procedure TDMZeosDataSet.RollBack(AName: string);
begin
  FDbConnection.Rollback;
end;

procedure TDMZeosDataSet.StartTransaction;
begin
  if not FDbConnection.Connected then
    FDbConnection.Connected := True;
  FDbConnection.StartTransaction;
end;

constructor TDMZeosDataSet.Create(AOwner: TComponent);
begin
  //inherited;
  if Assigned(AOwner) then // so ate ter corrigido tds pontos
    raise Exception.Create(Self.ClassName + ' Create is wrong');
  DMZeosDataSet := Self;
  FDbConnection := nil;
end;

function TDMZeosDataSet.DataBasePath: string;
begin
  Result := FDbConnection.Database;
end;

destructor TDMZeosDataSet.Destroy;
begin
  // DMZeosDataSet := nil;
  FDbConnection := nil;
  inherited;
end;

function TDMZeosDataSet.GetDateTime(): TDateTime;
var
  LQry: TZQuery;
begin
  Result := 0;
  try
    LQry := TZQuery.Create(nil);
    LQry.Connection := FDbConnection;
    if (LowerCase(FDbConnection.Protocol).Equals('firebird')) then
      LQry.SQL.Add
        ('select cast(''Now'' as TIMESTAMP) as LDateTime from rdb$database')
    else if (LowerCase(FDbConnection.Protocol).Equals('mysql')) then
      LQry.SQL.Add('SELECT NOW() as LDateTime');
    LQry.Open;

    Result := LQry.FieldByName('LDateTime').AsDateTime;
    LQry.Close;
  finally
    FreeAndNil(LQry);
  end;
end;

function TDMZeosDataSet.GetDbConnection: TComponent;
begin
  Result := TComponent(FDbConnection);
end;

function TDMZeosDataSet.GetDataSet(const ASql: string): TDataSet;
begin
  try
    Result := TZQuery.Create(nil);
    TZQuery(Result).Close;
    TZQuery(Result).Connection := FDbConnection;
    TZQuery(Result).SQL.Clear;
    TZQuery(Result).SQL.Add(ASql);
  {$IFDEF DEBUG}
    try
      TZQuery(Result).SQL.SaveToFile('ScriptGetDataSet.sql');
    except

    end;
  {$ENDIF}
    TZQuery(Result).Open;
  except
    FreeAndNil(Result);
    raise ;
  end;
end;

function TDMZeosDataSet.GetResultJson(const ASql: string): string;
var
  LDataSet: TDataSet;
begin
  try
    LDataSet := nil;
    Result := EmptyStr;
    LDataSet := GetDataSet(ASql);

    Result := '[{' + '"Result":"OK"' + ',"Values":[' +
      LDataSet.FieldByName('Json').AsString + ']' + '}]';
  finally
    FreeAndNil(LDataSet);
  end;
end;

procedure TDMZeosDataSet.QryExec(const ASql: string);
var
  LQry: TZQuery;
begin
  try
    LQry := TZQuery.Create(nil);
    LQry.Connection := FDbConnection;
    LQry.Close;
    LQry.SQL.Add(ASql);

{$IFDEF DEBUG}
    try
      LQry.SQL.SaveToFile('ScriptQryExec.sql');
    except

    end;
{$ENDIF}
    LQry.ExecSQL;
    LQry.Close;
  finally
    FreeAndNil(LQry);
  end;
end;

procedure TDMZeosDataSet.SetDbConnection(Value: TComponent);
begin
  FDbConnection := TZConnection(Value);
end;

end.
