unit IdeaL.Lib.Controller.ZeosConnection;

interface

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.StrUtils,

  Data.DB,

  ZAbstractConnection,
  ZConnection;

type
  TDMZeosConnection = class(TComponent)
  private
    FZConn: TZConnection;
    FIniFileName: string;
    FSctionDbName: string;
    procedure SetIniFileName(const Value: string);
    function GetZConn: TZConnection;
    { Private declarations }
  public
    constructor Create(AOwner: TComponent); // override;
    destructor Destroy; override;

    procedure ConnectionCommit();
    procedure ConnectionRollBack();
    procedure ConnectionStartTransaction();

    function GetConnection(): TCustomConnection;
    procedure Close;

    property IniFileName: string read FIniFileName write SetIniFileName;
    property SctionDbName: string read FSctionDbName write FSctionDbName;
    property ZConn: TZConnection read GetZConn;

    procedure SetConfig(
      ADriverName: string; //
      ADatabase: string; //
      AUserName: string; //
      APassword: string;
      AHost: string; //
      APort: string //
    );
    { Public declarations }
  end;

var
  DMZeosConnection: TDMZeosConnection;

implementation

uses
  IdeaL.Lib.Utils,
  IdeaL.Lib.ManageIniFile;

{ TDMZeosConnection }

procedure TDMZeosConnection.Close;
begin
  if GetConnection.Connected then
    GetConnection.Connected := False;
end;

procedure TDMZeosConnection.ConnectionCommit;
begin
  FZConn.Commit;
end;

procedure TDMZeosConnection.ConnectionRollBack;
begin
  FZConn.Rollback;
end;

procedure TDMZeosConnection.ConnectionStartTransaction;
begin
  FZConn.StartTransaction;
end;

constructor TDMZeosConnection.Create(AOwner: TComponent);
begin
  // inherited;
  FZConn := TZConnection.Create(nil);
  FSctionDbName := 'ConnectionDb';
end;

destructor TDMZeosConnection.Destroy;
begin
  if Assigned(FZConn) then
    FZConn.Connected := False;
  FreeAndNil(FZConn);
  inherited;
end;

function TDMZeosConnection.GetConnection: TCustomConnection;
begin
  Result := TCustomConnection(FZConn);
end;

function TDMZeosConnection.GetZConn: TZConnection;
begin
  Result := FZConn;
end;

procedure TDMZeosConnection.SetConfig(ADriverName, ADatabase, AUserName,
  APassword, AHost, APort: string);
var
  LTryStrToInt: Integer;
  LDllName: string;
begin
  FZConn.Connected := False;
  FZConn.Protocol := ADriverName;
  FZConn.Database := ADatabase;
  FZConn.User := AUserName;
  FZConn.Password := APassword;
  FZConn.Port := APort.ToInteger;
  FZConn.HostName := AHost;

  case AnsiIndexStr(FZConn.Protocol, [ //
    'mysql', // 0
    'firebird', // 1
    '' //
    ]) of
    0:
      LDllName := 'libmysql.dll';
    1:
      LDllName := 'fbclient.dll';
  else
    raise Exception.Create('ZConn.Protocol: Drivername=[' + FZConn.Protocol + '] not implemented');
  end;

  FZConn.LibraryLocation := System.IOUtils.TPath.Combine(TUtils.GetApplicationPath, LDllName);
end;

procedure TDMZeosConnection.SetIniFileName(const Value: string);
begin
  FIniFileName := Value;

  SetConfig(
    TManageIniFile.ReadIni(TUtils.GetApplicationPath, FIniFileName, SctionDbName, 'DriverName', ''),
    TManageIniFile.ReadIni(TUtils.GetApplicationPath, FIniFileName, SctionDbName, 'Database', ''),
    TManageIniFile.ReadIni(TUtils.GetApplicationPath, FIniFileName, SctionDbName, 'UserName', ''),
    TManageIniFile.ReadIni(TUtils.GetApplicationPath, FIniFileName, SctionDbName, 'Password', ''),
    TManageIniFile.ReadIni(TUtils.GetApplicationPath, FIniFileName, SctionDbName, 'HostName', ''),
    TManageIniFile.ReadIni(TUtils.GetApplicationPath, FIniFileName, SctionDbName, 'Port', '0')
    );
end;

end.
