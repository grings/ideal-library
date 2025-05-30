unit IdeaL.Lib.Controller.Connection.FireDac.DM;

interface

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,

  Data.DB,

{$IFDEF MSWINDOWS}
  FireDac.VCLUI.Wait,
{$ENDIF}
  FireDac.Stan.Intf,
  FireDac.Stan.Option,
  FireDac.Stan.Error,
  FireDac.UI.Intf,
  FireDac.Phys.Intf,
  FireDac.Stan.Def,
  FireDac.Stan.Pool,
  FireDac.Stan.Async,
  FireDac.Phys,
  FireDac.FMXUI.Wait,
  FireDac.Comp.Client,
  FireDac.Phys.SQLite,
  FireDac.Phys.SQLiteDef,
  FireDac.Stan.ExprFuncs;

type
  TDmFdConnection = class(TDataModule)
    FdConn: TFDConnection;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    { Private declarations }
    procedure CreateUpdateDatabase();
  public
    { Public declarations }
  end;

var
  DmFdConnection: TDmFdConnection;

implementation

uses
  IdeaL.Lib.Utils,
  IdeaL.Lib.Mobile.Database,
  Libersten.Lib.Database.Constants;

{%CLASSGROUP 'FMX.Controls.TControl'}
{$R *.dfm}

procedure TDmFdConnection.CreateUpdateDatabase;
var
  LDsTables: TDataSet;
begin
  try
    try
      LDsTables := nil;

      TMobileDatabase.ExecSql(FdConn, 'PRAGMA foreign_keys = OFF;');

      FdConn.StartTransaction;

      LDsTables := TMobileDatabase.GetTables(FdConn);

      if not(LDsTables.Locate('name', 'SystemInfo', [])) then
      begin
        TMobileDatabase.ExecSql(FdConn, cCreateTableSystemInfo);
        TMobileDatabase.QryExecSql(FdConn,
          'insert into SystemInfo values ("0")');
      end;

      if not(LDsTables.Locate('name', 'CacheNotification', [])) then
      begin
        TMobileDatabase.ExecSql(FdConn, cCreateTableCacheNotification);
      end;

      if not(LDsTables.Locate('name', 'User', [])) then
      begin
        TMobileDatabase.ExecSql(FdConn, cCreateTableUser);
      end;

      if not(LDsTables.Locate('name', 'Audio', [])) then
      begin
        TMobileDatabase.ExecSql(FdConn, cCreateTableAudio);
        TMobileDatabase.ExecSql(FdConn, 'insert into Audio (Id, Description) values (1, "Dreams_Electric")');
        TMobileDatabase.ExecSql(FdConn, 'insert into Audio (Id, Description) values (2, "Anxious")');
        TMobileDatabase.ExecSql(FdConn, 'insert into Audio (Id, Description) values (3, "Price_Check")');
        TMobileDatabase.ExecSql(FdConn, 'insert into Audio (Id, Description) values (4, "Sky_Skating")');
        TMobileDatabase.ExecSql(FdConn, 'insert into Audio (Id, Description) values (5, "Synergy")');
        TMobileDatabase.ExecSql(FdConn, 'insert into Audio (Id, Description) values (6, "Weak_Knight")');
        TMobileDatabase.ExecSql(FdConn, 'insert into Audio (Id, Description) values (7, "Ação Humana Pedaço 1 de 10")');
      end;

      if not(LDsTables.Locate('name', 'DownloadServerInfo', [])) then
      begin
        TMobileDatabase.ExecSql(FdConn, cCreateTableDownloadServerInfo);
      end;

      if not(LDsTables.Locate('name', 'AudioLocal', [])) then
        TMobileDatabase.ExecSql(FdConn, cCreateTableAudioLocal);

      if not(LDsTables.Locate('name', 'AudioRemote', [])) then
      begin
        TMobileDatabase.ExecSql(FdConn, cCreateTableAudioRemote);
        TMobileDatabase.ExecSql(
          FdConn,
          'insert into AudioRemote (' +
          'Id,' +
          'IdAudio,' +
          'CompactedName,' +
          'CompactedMd5,' +
          'FileName,' +
          'FileMd5,' +
          'IdDownloadServer' +
          ') values (' +
          '1,' +
          '1,' +
          '"Dreams_Electric.rar",' +
          '"AFA71B5341672346A430DE1D653C8C2C",' +
          '"Dreams_Electric.mp3",' +
          '"19B5BCF6EB8D97FFDEE33D452EA95D9B",' +
          '1'+
          ')');
        TMobileDatabase.ExecSql(
          FdConn,
          'insert into AudioRemote (' +
          'Id,' +
          'IdAudio,' +
          'CompactedName,' +
          'CompactedMd5,' +
          'FileName,' +
          'FileMd5,' +
          'IdDownloadServer' +
          ') values (' +
          '2,' +
          '2,' +
          '"Anxious.rar",' +
          '"0AB97A7471E593D338BB09A94BAF23AE",' +
          '"Anxious.mp3",' +
          '"56AAD15BE041222E40ACB5A48021A37F",' +
          '1'+
          ')');
        TMobileDatabase.ExecSql(
          FdConn,
          'insert into AudioRemote (' +
          'Id,' +
          'IdAudio,' +
          'CompactedName,' +
          'CompactedMd5,' +
          'FileName,' +
          'FileMd5,' +
          'IdDownloadServer' +
          ') values (' +
          '3,' +
          '3,' +
          '"Price_Check.rar",' +
          '"8A113E0B248D584C98578317DCD25D4F",' +
          '"Price_Check.mp3",' +
          '"318826923AC6DC3B59D3E74EF96BF042",' +
          '1'+
          ')');
        TMobileDatabase.ExecSql(
          FdConn,
          'insert into AudioRemote (' +
          'Id,' +
          'IdAudio,' +
          'CompactedName,' +
          'CompactedMd5,' +
          'FileName,' +
          'FileMd5,' +
          'IdDownloadServer' +
          ') values (' +
          '4,' +
          '4,' +
          '"Sky_Skating.rar",' +
          '"17231C3E1C5B561CA48727C016DEA779",' +
          '"Sky_Skating.mp3",' +
          '"2B5F7C3005667D0543C268E83CB8209B",' +
          '1'+
          ')');
        TMobileDatabase.ExecSql(
          FdConn,
          'insert into AudioRemote (' +
          'Id,' +
          'IdAudio,' +
          'CompactedName,' +
          'CompactedMd5,' +
          'FileName,' +
          'FileMd5,' +
          'IdDownloadServer' +
          ') values (' +
          '5,' +
          '5,' +
          '"Synergy.rar",' +
          '"35AF2E8516EA213CED146E2F709B548F",' +
          '"Synergy.mp3",' +
          '"9C9FB6C1C35FE91AC0F535A307F9870E",' +
          '1'+
          ')');
        TMobileDatabase.ExecSql(
          FdConn,
          'insert into AudioRemote (' +
          'Id,' +
          'IdAudio,' +
          'CompactedName,' +
          'CompactedMd5,' +
          'FileName,' +
          'FileMd5,' +
          'IdDownloadServer' +
          ') values (' +
          '6,' +
          '6,' +
          '"Weak_Knight.rar",' +
          '"0F42CC785299CACDFB4CC5D670A80DF6",' +
          '"Weak_Knight.mp3",' +
          '"6C5E488A9D48C35A3C22CA52F1630487",' +
          '1'+
          ')');
        TMobileDatabase.ExecSql(
          FdConn,
          'insert into AudioRemote (' +
          'Id,' +
          'IdAudio,' +
          'CompactedName,' +
          'CompactedMd5,' +
          'FileName,' +
          'FileMd5,' +
          'IdDownloadServer' +
          ') values (' +
          '7,' +
          '7,' +
          '"Audio7.rar",' +
          '"487363B88BA490900FDCC46AB7A36125",' +
          '"Ação Humana Pedaço 1 de 10 - Autor Ludwig von Mises.mp3",' +
          '"801056CBB10DE494DCFB5E4AD5F1FC35",' +
          '1'+
          ')');
      end;

      FdConn.Commit;
    except
      FdConn.Rollback;
    end;
  finally
    TMobileDatabase.ExecSql(FdConn, 'PRAGMA foreign_keys = ON;');
    FreeAndNil(LDsTables);
  end;
end;

procedure TDmFdConnection.DataModuleCreate(Sender: TObject);
var
  LFullPath: string;
begin
  FdConn.Close;
{$IFDEF MSWINDOWS}
  LFullPath := TUtils.RemovePathFromDir(TUtils.GetApplicationPath, 2);
  LFullPath := TPath.Combine(LFullPath, 'Database');
  LFullPath := TPath.Combine(LFullPath, 'Libersten.db');
{$ELSE}
  LFullPath := TPath.GetDocumentsPath;
  LFullPath := TPath.Combine(LFullPath, 'Libersten.db');
{$ENDIF}
  FdConn.Params.Values['Database'] := LFullPath;
  FdConn.Params.Values['OpenMode'] := 'ReadWrite';

  TMobileDatabase.CreateDatabase(FdConn.Params.Values['Database']);
  CreateUpdateDatabase();
end;

procedure TDmFdConnection.DataModuleDestroy(Sender: TObject);
begin
  FdConn.Close;
end;

end.
