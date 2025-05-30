unit IdeaL.Lib.Mobile.Database;

interface

uses
  System.SysUtils,

  Data.DB,

  IdeaL.Lib.Controller.IConnection,

  FireDAC.Comp.Client,
  FireDAC.DApt;

type
  TMobileDatabase = class
  private
    { private declarations }
    class var FSql: string;

    class function GetDataSet(const AConnection: IConnection;
      const ASql: string): TDataSet;
  protected
    { protected declarations }
  public
    { public declarations }
    class procedure ExecSql(const AConnection: IConnection; const ASql: string);
    class procedure QryExecSql(const AConnection: IConnection; const ASql: string);
    class function GetTables(const AConnection: IConnection): TDataSet;
    class function GetTableInfo(const AConnection: IConnection; const ATable: string): TDataSet;
    class function GetTriggers(const AConnection: IConnection): TDataSet;
    class function GetForeignKey(const AConnection: IConnection; const ATable: string): TDataSet;
    class procedure AddField(const AConnection: IConnection; const ATable, AField, AType: String; const AOptions: string = '');
    class procedure CreateDatabase(const AFullPath: string);

    class procedure FkEnable(const AConnection: IConnection);
    class procedure FkDisable(const AConnection: IConnection);

    class procedure CheckIntegrity(const AConnection: IConnection);
  published
    { published declarations }
  end;

implementation

{ TMobileDatabase }

class procedure TMobileDatabase.AddField(const AConnection: IConnection;
  const ATable, AField, AType, AOptions: String);
begin
  ExecSql(AConnection, 'alter table ' + ATable + ' add column ' + AField + ' ' + AType + ' ' + AOptions + ';');
end;

class procedure TMobileDatabase.CheckIntegrity(const AConnection: IConnection);
begin
  var
  LDs := GetDataSet(AConnection, 'PRAGMA integrity_check');
  try
    if (not LDs.IsEmpty) and (not LDs.Fields[0].AsString.Trim.ToLower.Equals('ok')) then
      raise Exception.Create('TMobileDatabase.CheckIntegrity integrity error: ' + LDs.Fields[0].AsString);
  finally
    LDs.Free;
  end;
end;

class procedure TMobileDatabase.CreateDatabase;
var
  LArq: TextFile;
begin
  if not(FileExists(AFullPath)) then
  begin
    try
      AssignFile(LArq, AFullPath);
      Rewrite(LArq);
    finally
      CloseFile(LArq);
    end;
  end;
end;

class procedure TMobileDatabase.ExecSql(const AConnection: IConnection;
  const ASql: string);
begin
  // Commit, Rollback etc deve ser feito no codigo que chama
  AConnection.Exec(ASql);
end;

class procedure TMobileDatabase.FkDisable(
  const AConnection: IConnection);
begin
  ExecSql(AConnection, 'PRAGMA foreign_keys = OFF');
end;

class procedure TMobileDatabase.FkEnable(
  const AConnection: IConnection);
begin
  ExecSql(AConnection, 'PRAGMA foreign_keys = ON');
end;

class function TMobileDatabase.GetDataSet(const AConnection
  : IConnection; const ASql: string): TDataSet;
var
  LQry: TFDQuery;
begin
  LQry := TFDQuery.Create(nil);
  LQry.Connection := AConnection.Connection as TFDCustomConnection;
  LQry.SQL.Add(ASql);
  LQry.Open();
  LQry.Last;
  Result := LQry;
end;

class function TMobileDatabase.GetForeignKey(const AConnection
  : IConnection; const ATable: string): TDataSet;
begin
  Result := nil;
  FSql := 'PRAGMA foreign_key_list("' + ATable + '")';
  Result := GetDataSet(AConnection, FSql);
end;

class function TMobileDatabase.GetTableInfo(const AConnection
  : IConnection; const ATable: string): TDataSet;
begin
  Result := nil;
  FSql := 'PRAGMA table_info("' + ATable + '")';
  Result := GetDataSet(AConnection, FSql);
end;

class function TMobileDatabase.GetTables(const AConnection: IConnection)
  : TDataSet;
begin
  Result := nil;
  FSql := 'select name from sqlite_master where type="table"';
  Result := GetDataSet(AConnection, FSql);
end;

class function TMobileDatabase.GetTriggers(const AConnection: IConnection): TDataSet;
begin
  Result := nil;
  FSql := 'select * from sqlite_master where type = "trigger"';
  Result := GetDataSet(AConnection, FSql);
end;

class procedure TMobileDatabase.QryExecSql(
  const AConnection: IConnection; const ASql: string);
var
  LQry: TFDQuery;
begin
  try
    LQry := TFDQuery.Create(nil);
    LQry.Connection := AConnection.Connection as TFDCustomConnection;
    LQry.ExecSQL(ASql);
  finally
    FreeAndNil(LQry);
  end;
end;

end.
