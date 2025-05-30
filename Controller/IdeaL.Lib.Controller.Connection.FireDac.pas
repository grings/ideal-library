unit IdeaL.Lib.Controller.Connection.FireDac;

// {$I GlobalDefines.inc}

interface

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.StrUtils,

  Data.DB,

  FireDac.Stan.Intf,
  FireDac.Stan.Option,
  FireDac.Stan.Error,
  FireDac.UI.Intf,
  FireDac.Phys.Intf,
  FireDac.Stan.Def,
  FireDac.Stan.Pool,
  FireDac.Stan.Async,
  FireDac.Phys,
  FireDac.Comp.Client,
  FireDac.Stan.ExprFuncs,
  FireDac.Phys.SQLiteWrapper.Stat,
  FireDac.Phys.SQLiteDef,
  FireDac.Phys.SQLite,

{$IF defined(MSWINDOWS) or defined(LINUX)}
  // Firebird
  FireDac.Phys.FBDef,
  FireDac.Phys.IBBase,
  FireDac.Phys.FB,
  // Firebird

  // MySQL
  FireDac.Phys.MySQLDef,
  FireDac.Phys.MySQL,
  // MySQL
{$ENDIF}

{$IFDEF VCL}
  FireDac.ConsoleUI.Wait,
  FireDac.VCLUI.Wait,
{$ELSE}
{$IF defined(FMX) and not defined(ComponentTipScrollBox)}
  FireDac.FMXUI.Wait,
{$ENDIF}
{$ENDIF}
  IdeaL.Lib.Controller.IConnection {,
    IdeaL.Lib.Constants};

type
  IConnection = IdeaL.Lib.Controller.IConnection.IConnection;

  TDMFireDACConnection = class(TInterfacedObject, IConnection)
  private
    FConn: TFDConnection;
    FLockedWaitSec: Integer;
    FPhysDriverLink: TFDPhysDriverLink;
    // FGUIxWaitCursor: TFDGUIxWaitCursor;
    FConnectionDriver: TConnectionDriver;
    function GetPathBin: string;
    procedure SetPathBin(const Value: string);
    { Private declarations }
  protected
    function GetHost: string;
    function GetCharSetStr: string;
    function GetPassword: string;
    function GetPort: Integer;
    function GetUserName: string;
    function GetDataBase: string;
    procedure SetHost(const Value: string);
    procedure SetCharSetStr(const Value: string);
    procedure SetPassword(const Value: string);
    procedure SetPort(const Value: Integer);
    procedure SetUserName(const Value: string);
    procedure SetDataBase(const Value: string);

    function GetConnection: TCustomConnection;
    procedure SetConnection(const Value: TCustomConnection);

    function GetLockedWaitSec: Integer;
    procedure SetLockedWaitSec(const Value: Integer);

    procedure DoBeforeConnect(Sender: TObject);
    { Protected declarations }
  public
    type
    TConnectionDriver = IdeaL.Lib.Controller.IConnection.TConnectionDriver;
    { Public declarations }
  public
    constructor Create(AConnectionDriver: TConnectionDriver);
    destructor Destroy; override;

    procedure Exec(const ASql: string);
    procedure Commit();
    procedure RollBack(AName: string);
    procedure StartTransaction();
    property Connection: TCustomConnection read GetConnection write SetConnection;

    function IsLocked: Boolean;
    property LockedWaitSec: Integer read GetLockedWaitSec write SetLockedWaitSec;

    function IsActive: Boolean;
    function Ping: Boolean;
    procedure Open;
    procedure Close;
    procedure SetConfig(const ADataBase: string);

{$IF defined(MSWINDOWS) or defined(LINUX)}
    procedure SetFirebirdConfig; overload;
    procedure SetFirebirdConfig(const ADataBase, APathBin, AHost, AUserName, APassword, ACharSetStr: string; APort: Integer); overload;
{$ENDIF}

    procedure SetSqLiteConfig;
    function GetConnectionDriver: TConnectionDriver;

    property Host: string read GetHost write SetHost;
    property Port: Integer read GetPort write SetPort;
    property UserName: string read GetUserName write SetUserName;
    property Password: string read GetPassword write SetPassword;
    property CharSetStr: string read GetCharSetStr write SetCharSetStr;
    property DataBase: string read GetDataBase write SetDataBase;
    property PathBin: string read GetPathBin write SetPathBin;
    { Public declarations }
  end;

implementation

uses
  IdeaL.Lib.Utils {,
    IdeaL.Lib.Controller.InstanciedClasses};

{ TDMInterBaseConnection }

procedure TDMFireDACConnection.Close;
begin
  if Assigned(FConn) then
    FConn.Close;
end;

procedure TDMFireDACConnection.Commit;
begin
  if IsActive then
  begin
    TUtils.EventLogIde('TDMFireDACConnection.Commit ' + DateTimeToStr(Now));
    try
      FConn.Commit;
    finally
      FConn.UpdateOptions.LockWait := False;
    end;
  end;
end;

constructor TDMFireDACConnection.Create(AConnectionDriver: TConnectionDriver);
begin
  FLockedWaitSec := -1;
{$IFDEF ComponentTipScrollBox}
  FFDGUIxSilentMode := True;
{$ENDIF}
  FConn := TFDConnection.Create(nil);
  FConn.LoginPrompt := False;
  FConn.BeforeConnect := DoBeforeConnect;
  FConn.UpdateOptions.LockMode := TFDLockMode.lmPessimistic;
  FConnectionDriver := AConnectionDriver;

  // FGUIxWaitCursor := TFDGUIxWaitCursor.Create(FConn);

  FPhysDriverLink := nil;

  case AConnectionDriver of
{$IF defined(MSWINDOWS) or defined(LINUX)}
    cdFirebird:
      SetFirebirdConfig;
{$ENDIF}
    cdSqLite:
      SetSqLiteConfig;
  end;
end;

destructor TDMFireDACConnection.Destroy;
begin
  FreeAndNil(FPhysDriverLink);
  // FreeAndNil(FGUIxWaitCursor);
  if Assigned(FConn) and (IsActive) then
    FConn.Close;

  FreeAndNil(FConn);
  inherited;
end;

procedure TDMFireDACConnection.DoBeforeConnect(Sender: TObject);
begin
  StrToInt('1');
end;

procedure TDMFireDACConnection.Exec(const ASql: string);
begin
  FConn.ExecSQL(ASql);
end;

function TDMFireDACConnection.GetCharSetStr: string;
begin
  Result := FConn.Params.Values['CharacterSet'];
end;

function TDMFireDACConnection.GetConnection: TCustomConnection;
begin
  Result := FConn;
end;

function TDMFireDACConnection.GetConnectionDriver: TConnectionDriver;
begin
  Result := FConnectionDriver;
end;

function TDMFireDACConnection.GetDataBase: string;
begin
  Result := FConn.Params.Values['Database'];
end;

function TDMFireDACConnection.GetHost: string;
begin
  Result := FConn.Params.Values['Server'];
end;

function TDMFireDACConnection.GetLockedWaitSec: Integer;
begin
  Result := FLockedWaitSec;
end;

function TDMFireDACConnection.GetPassword: string;
begin
  Result := FConn.Params.Values['Password'];
end;

function TDMFireDACConnection.GetPathBin: string;
begin
  Result := EmptyStr;
  if Assigned(FPhysDriverLink) then
    Result := FPhysDriverLink.VendorLib;
end;

function TDMFireDACConnection.GetPort: Integer;
begin
  Result := 0;
  TryStrToInt(FConn.Params.Values['Port'], Result);
end;

function TDMFireDACConnection.GetUserName: string;
begin
  Result := FConn.Params.Values['User_Name'];
end;

function TDMFireDACConnection.IsActive: Boolean;
begin
  Result := False;
  if Assigned(FConn) then
    Result := FConn.Connected;
end;

function TDMFireDACConnection.IsLocked: Boolean;
begin
  Result := FConn.UpdateOptions.LockWait;
end;

procedure TDMFireDACConnection.Open;
begin
  if Assigned(FConn) then
    FConn.Connected := True;
end;

function TDMFireDACConnection.Ping: Boolean;
begin
  Result := False;
  if IsActive then
    Result := FConn.Ping;
end;

procedure TDMFireDACConnection.RollBack(AName: string);
begin
  if IsActive then
  begin
    TUtils.EventLogIde(AName + ' TDMFireDACConnection.RollBack ' + DateTimeToStr(Now));
    FConn.UpdateOptions.LockWait := False;
    if FConn.InTransaction then
      FConn.RollBack;
  end;
end;

procedure TDMFireDACConnection.SetCharSetStr(const Value: string);
begin
  FConn.Params.Values['CharacterSet'] := Value;
end;

procedure TDMFireDACConnection.SetConfig(const ADataBase: string);
begin
  FConn.Close;
  DataBase := ADataBase;
  FConn.Params.Values['OpenMode'] := 'ReadWrite';
end;

procedure TDMFireDACConnection.SetConnection(const Value: TCustomConnection);
begin
  TUtils.ThrowExceptionMethodIsNotImplemented('TDMFireDACConnection.SetConnection');
end;

procedure TDMFireDACConnection.SetDataBase(const Value: string);
begin
  FConn.Params.Values['Database'] := Value;
end;

{$IF defined(MSWINDOWS) or defined(LINUX)}
procedure TDMFireDACConnection.SetFirebirdConfig(const ADataBase, APathBin, AHost, AUserName,
  APassword, ACharSetStr: string; APort: Integer);
begin
  FConn.Close;
  FreeAndNil(FPhysDriverLink);
  FPhysDriverLink := TFDPhysFBDriverLink.Create(FConn);
  PathBin := APathBin;
  TFDPhysFBDriverLink(FPhysDriverLink).ThreadSafe := True;
  FConn.DriverName := '';
  FConn.DriverName := 'FB';
  FConn.Params.Values['Protocol'] := 'TCPIP';
  FConn.LoginPrompt := False;
  Host := AHost;
  Port := APort;
  UserName := AUserName;
  Password := APassword;
  CharSetStr := ACharSetStr;
  SetConfig(ADataBase);
end;

procedure TDMFireDACConnection.SetFirebirdConfig;
begin
  SetFirebirdConfig(
    EmptyStr, // TInstanciedClasses.Config.Firebird.DataBase,
    EmptyStr, // TInstanciedClasses.Config.Firebird.PathBin,
    EmptyStr, // TInstanciedClasses.Config.Firebird.Host,
    EmptyStr, // TInstanciedClasses.Config.Firebird.UserName,
    EmptyStr, // TInstanciedClasses.Config.Firebird.Password,
    EmptyStr, // TInstanciedClasses.Config.Firebird.CharSetStr,
    0 // TInstanciedClasses.Config.Firebird.Port
    );
end;
{$ENDIF}

procedure TDMFireDACConnection.SetHost(const Value: string);
begin
  FConn.Params.Values['Server'] := Value;
end;

procedure TDMFireDACConnection.SetLockedWaitSec(const Value: Integer);
begin
  FLockedWaitSec := Value;
end;

procedure TDMFireDACConnection.SetPassword(const Value: string);
begin
  FConn.Params.Values['Password'] := Value;
end;

procedure TDMFireDACConnection.SetPathBin(const Value: string);
begin
  if Assigned(FPhysDriverLink) then
  begin
    if ExtractFileName(Value).Trim.IsEmpty then
      FPhysDriverLink.VendorHome := Value
    else
      FPhysDriverLink.VendorLib := Value;
  end;
end;

procedure TDMFireDACConnection.SetPort(const Value: Integer);
begin
  FConn.Params.Values['Port'] := Value.ToString;
end;

procedure TDMFireDACConnection.SetSqLiteConfig;
begin
  FConn.Close;
  FreeAndNil(FPhysDriverLink);
  FPhysDriverLink := TFDPhysSQLiteDriverLink.Create(FConn);
  FConn.DriverName := 'SQLite';
  SetConfig('');
end;

procedure TDMFireDACConnection.SetUserName(const Value: string);
begin
  FConn.Params.Values['User_Name'] := Value;
end;

procedure TDMFireDACConnection.StartTransaction;
begin
  TUtils.EventLogIde('TDMFireDACConnection.StartTransaction ' + DateTimeToStr(Now));
  if IsActive then
  begin
    if LockedWaitSec > 0 then
    begin
      var
      LCount := 0;
      var
      LIsLocked := False;
      repeat
        LIsLocked := IsLocked;
        if LIsLocked then
        begin
          Sleep(1000);
          Inc(LCount, 1);
        end;
      until (not LIsLocked) or (LCount > LockedWaitSec);

      if IsLocked then
        raise Exception.Create('FDConnection is locked');
    end;

    FConn.UpdateOptions.LockWait := True;
    FConn.StartTransaction;
  end;
end;

end.
