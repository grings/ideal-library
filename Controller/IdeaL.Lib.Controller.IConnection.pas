unit IdeaL.Lib.Controller.IConnection;

interface

uses
  System.Classes,

  Data.DB, System.SysUtils;

type
  TConnectionDriver = (cdFirebird, cdSqLite);

  IConnection = Interface(IInterface)
    ['{616EDA86-67D9-461C-AABC-98625692731F}']
    /// <summary> Connection executing a script
    /// </summary>
    /// <param name="AScript">The script to be executed
    /// </param>
    procedure Exec(const ASql: string);
    /// <summary> Connection commiting the changes
    /// </summary>
    procedure Commit();
    /// <summary> Connection rollback the changes
    /// </summary>
    procedure RollBack(AName: string);
    /// <summary> Connection starting a transaction, to make sure everything is going to be properly saved or rollbacked
    /// </summary>
    procedure StartTransaction();
    /// <summary> Get the Object Connection
    /// </summary>
    /// <returns> The object Connection
    /// </returns>
    function GetConnection: TCustomConnection;
    procedure SetConnection(const Value: TCustomConnection);
    function GetConnectionDriver: TConnectionDriver;
    property Connection: TCustomConnection read GetConnection write SetConnection;

    function GetHost: string;
    procedure SetHost(const Value: string);
    property Host: string read GetHost write SetHost;

    function GetPort: Integer;
    procedure SetPort(const Value: Integer);
    property Port: Integer read GetPort write SetPort;

    function GetUserName: string;
    procedure SetUserName(const Value: string);
    property UserName: string read GetUserName write SetUserName;

    function GetPassword: string;
    procedure SetPassword(const Value: string);
    property Password: string read GetPassword write SetPassword;

    function GetCharSetStr: string;
    procedure SetCharSetStr(const Value: string);
    property CharSetStr: string read GetCharSetStr write SetCharSetStr;

    function GetDataBase: string;
    procedure SetDataBase(const Value: string);
    property DataBase: string read GetDataBase write SetDataBase;

    function GetPathBin: string;
    procedure SetPathBin(const Value: string);
    property PathBin: string read GetPathBin write SetPathBin;

{$IF defined(MSWINDOWS) or defined(LINUX)}
    procedure SetFirebirdConfig; overload;
    procedure SetFirebirdConfig(const ADatabase, APathBin, AHost, AUserName, APassword, ACharSetStr: string; APort: Integer); overload;
{$ENDIF}

    function IsActive: Boolean;
    function Ping: Boolean;
    procedure Open;
    procedure Close;

    function IsLocked: Boolean;
    function GetLockedWaitSec: Integer;
    procedure SetLockedWaitSec(const Value: Integer);
    property LockedWaitSec: Integer read GetLockedWaitSec write SetLockedWaitSec;
  End;

implementation

end.
