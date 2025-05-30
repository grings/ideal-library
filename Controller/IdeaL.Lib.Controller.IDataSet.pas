unit IdeaL.Lib.Controller.IDataSet;

interface

uses
  System.Classes,

  Data.DB;

type
  IDataSet = Interface(IInterface)
    ['{8C6C1DD1-33F9-4B47-BCA4-2F67C7760F8D}']
    /// <summary> Query executing a script
    /// </summary>
    /// <param name="AScript">The script to be executed
    /// </param>
    procedure QryExec(const AScript: string);
    /// <summary> Connection executing a script
    /// </summary>
    /// <param name="AScript">The script to be executed
    /// </param>
    procedure ConnExec(const ASql: string);
    /// <summary> Connection commiting the changes
    /// </summary>
    procedure Commit();
    /// <summary> Connection rollback the changes
    /// </summary>
    procedure RollBack(AName: string);
    /// <summary> Connection starting a transaction, to make sure everything is going to be properly saved or rollbacked
    /// </summary>
    procedure StartTransaction;
    /// <summary> Get a DataSet
    /// </summary>
    /// <param name="ASql">The SQL which will returns the DataSet
    /// </param>
    /// <returns> A DataSet
    /// </returns>
    function GetDataSet(const ASql: string): TDataSet;
    /// <summary> Get the database current timestamp
    /// </summary>
    /// <returns> The database current timestamp
    /// </returns>
    function GetDateTime: TDateTime;
    /// <summary> ApplyUpdates on the DataSet
    /// </summary>
    ///  /// <param name="ADs">The DataSet which will execute the ApplyUpdates
    /// </param>
    /// <returns> Usually the DataSet components return an integer when
    ///  Applying data
    /// </returns>
    function ApplyUpdates(ADs: TDataSet): Integer;
  End;

implementation

end.

