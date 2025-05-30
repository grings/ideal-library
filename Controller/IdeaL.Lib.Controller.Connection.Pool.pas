unit IdeaL.Lib.Controller.Connection.Pool;

// {$I GlobalDefines.inc}

interface

uses
{$IFDEF GrijjyLogger}
  Grijjy.CloudLogging,
{$ENDIF}
  IdeaL.Lib.Controller.IConnection,
  IdeaL.Lib.Controller.IDataSet,
  IdeaL.Lib.Controller.Connection.FireDac,
  IdeaL.Lib.Controller.DataSet.FireDac,

  System.Generics.Collections,
  System.SysUtils,
  System.Classes,
  System.DateUtils,
  System.SyncObjs;

type
  TPoolConnectionItem = class
  private
    FConnection: IConnection;
    FDataSet: IDataSet;
    // Number of current connections
    FCount: Integer;
    FLastUsed: TDateTime;
    FLockedToDestroy: Boolean;
    FId: string;
    FReference: string;
    function GetConnection: IConnection;
    function GetDataSet: IDataSet;
    procedure SetLastUsed(const Value: TDateTime);
    procedure SetLockedToDestroy(const Value: Boolean);

    { private declarations }
  protected
    { protected declarations }
  public
    constructor Create(ADriver: TConnectionDriver);
    destructor Destroy; override;

    property Id: string read FId;
    property Count: Integer read FCount;
    property Connection: IConnection read GetConnection;
    property DataSet: IDataSet read GetDataSet;

    property LastUsed: TDateTime read FLastUsed write SetLastUsed;
    property LockedToDestroy: Boolean read FLockedToDestroy write SetLockedToDestroy;
    /// <summary> Store some reference to check something, e.g.: which Session ID is using it
    /// </summary>
    property Reference: string read FReference write FReference;

    procedure Connect;
    procedure Disconnect;
    { public declarations }
  end;

  TPoolConnection = class
  private
    FCriticalSection: TCriticalSection;
    FConns: TObjectList<TPoolConnectionItem>;
    FMaxConn: Integer;
    FMinutesOnStandBy: Integer;
    // Thread running each minute
    FThreadPerMinute: TThread;
    procedure SetMaxConn(const Value: Integer);
    procedure SetMinutesOnStandBy(const Value: Integer);
    { Private declarations }
  protected
    { Protected declarations }
  public
    constructor Create;
    destructor Destroy; override;

    // time in minutes to free the object when is it not being used
    property MinutesOnStandBy: Integer read FMinutesOnStandBy write SetMinutesOnStandBy;
    property MaxConn: Integer read FMaxConn write SetMaxConn;

    function GetItem(ADrive: TConnectionDriver): TPoolConnectionItem;
    { Public declarations }
  end;

implementation

{ TPoolConnection }

constructor TPoolConnection.Create;
begin
  FCriticalSection := TCriticalSection.Create;
  FConns := TObjectList<TPoolConnectionItem>.Create;
  MaxConn := 1;
  MinutesOnStandBy := 60;

  FThreadPerMinute := TThread.CreateAnonymousThread(
    procedure
    var
      i: Integer;
      LMaxMinuteOnStandBy: TDateTime;
      LNow: TDateTime;
      H, M, S, MS: Word;
      Y, Mo, D: Word;
    begin
      try
        try
          while True do
          begin
            if TThread.CurrentThread.CheckTerminated then
              Break;

            FCriticalSection.Enter;
            try
{$IFDEF GrijjyLogger} var
              LCountCheckToDestroy := 0;
              var
              LCountDeleted := 0;
              var
              LLogOfConns := EmptyStr;
{$ENDIF}
              i := 0;
              while (FConns.Count > 0) and (i < FConns.Count) do
              begin
                if FConns[i].LockedToDestroy then
                begin
                  FConns.Delete(i);
{$IFDEF GrijjyLogger}
                  Inc(LCountDeleted, 1);
{$ENDIF}
                end
                else
                  Inc(i, 1);
              end;

              i := 0;
              for var LItem in FConns do
              begin
                if TThread.CurrentThread.CheckTerminated then
                  Break;

                // Round it to Minutes
                DecodeDateTime(IncMinute(LItem.LastUsed, MinutesOnStandBy), Y, Mo, D, H, M, S, MS);
                LMaxMinuteOnStandBy := EncodeDateTime(Y, Mo, D, H, M, S, 0);
                DecodeDateTime(Now, Y, Mo, D, H, M, S, MS);
                LNow := EncodeDateTime(Y, Mo, D, H, M, S, 0);

{$IFDEF GrijjyLogger}
                Inc(i, 1);
                LLogOfConns := LLogOfConns + sLineBreak +
                  i.ToString + '; ' +
                  LItem.Id + '; ' +
                  LItem.Reference + '; ' +
                  LItem.Count.ToString + '; ' +
                  BoolToStr(LItem.LockedToDestroy, True) + '; ' +
                  DateTimeToStr(LItem.LastUsed) + '; ' +
                  DateTimeToStr(LMaxMinuteOnStandBy) + '; ' +
                  DateTimeToStr(LNow) + '; '
                  ;
{$ENDIF}
                if Assigned(FConns) and Assigned(LItem) and
                  (LMaxMinuteOnStandBy <= LNow) and
                  (LItem.Count = 0)
                then
                begin
                  LItem.LockedToDestroy := True;
{$IFDEF GrijjyLogger}
                  Inc(LCountCheckToDestroy, 1);
{$ENDIF}
                end;

{$IFDEF GrijjyLogger}
                LLogOfConns := LLogOfConns + BoolToStr(LItem.LockedToDestroy, True);
{$ENDIF}
              end;

{$IFDEF GrijjyLogger}
              GrijjyLog.Send('TPoolConnection.FThreadPerMinute',
                DateTimeToStr(Now) + ' ' + FConns.Count.ToString + ' ' + LCountCheckToDestroy.ToString + ' ' + LCountDeleted.ToString + sLineBreak +
                'Index;Id;Reference;Count;IsLockedToDestroy;LastUsed;MaxMinuteOnStandBy;LNow;IsLockedToDestroy' + sLineBreak +
                LLogOfConns.Trim
                );
{$ENDIF}
            finally
              FCriticalSection.Leave;
            end;

            if TThread.CurrentThread.CheckTerminated then
              Break;

            // 1 minute
            i := 0;
            while True do
            begin
              if TThread.CurrentThread.CheckTerminated then
                Break;
              if i = 60 then
                Break;
              TThread.Sleep(1000);
              Inc(i, 1);
            end;
          end;
        except
          on E: Exception do
          begin
{$IFDEF GrijjyLogger}
            GrijjyLog.Send('TPoolConnection.FThreadPerMinute_Error',
              E.Message
              );
{$ENDIF}
          end;
        end
      finally
        FThreadPerMinute := nil;
      end;
    end);
  FThreadPerMinute.Start;
end;

destructor TPoolConnection.Destroy;
var
  LItem: TPoolConnectionItem;
begin
  try
    if Assigned(FThreadPerMinute) then
    begin
      FThreadPerMinute.Terminate;
      while Assigned(FThreadPerMinute) do
        Sleep(200);
    end;
  except

  end;
  if Assigned(FCriticalSection) then
    FCriticalSection.Enter;
  try
    if Assigned(FConns) then
    begin
      while FConns.Count > 0 do
      begin
        LItem := FConns.ExtractAt(0);
        try
          LItem.Disconnect;
          FreeAndNil(LItem);
        except
          // do nothing, it might've being released in another place
        end;
      end;
      FreeAndNil(FConns);
    end;
  finally
    if Assigned(FCriticalSection) then
      FCriticalSection.Leave;
  end;
  FreeAndNil(FCriticalSection);
  inherited;
end;

function TPoolConnection.GetItem(ADrive: TConnectionDriver): TPoolConnectionItem;
begin
  Result := nil;
  FCriticalSection.Enter;
  try
    for var LItem in FConns do
    begin
      if (Assigned(LItem)) and
        not(LItem.LockedToDestroy)
      then
      begin
        var
        LConn := LItem.Connection;
        if (LConn.GetConnectionDriver = ADrive) and
          (LItem.Count < MaxConn)
        then
        begin
          LItem.Connect;
          Result := LItem;
          Break;
        end;
      end;
    end;

    if not(Assigned(Result)) then
    begin
      Result := TPoolConnectionItem.Create(ADrive);
      Result.Connect;
      FConns.Add(Result);
    end;
  finally
    FCriticalSection.Leave;
  end;
end;

procedure TPoolConnection.SetMaxConn(const Value: Integer);
begin
  FMaxConn := Value;
  if FMaxConn <= 0 then
    FMaxConn := 1;
end;

procedure TPoolConnection.SetMinutesOnStandBy(const Value: Integer);
begin
  FMinutesOnStandBy := Value;
end;

{ TPoolConnectionItem }

procedure TPoolConnectionItem.Connect;
begin
  Inc(FCount, 1);
  LastUsed := Now;
  LockedToDestroy := False;
end;

constructor TPoolConnectionItem.Create(ADriver: TConnectionDriver);
begin
  FId := FormatDateTime('yyyymmddhhnnsszzz', Now) + (Random(1000) + 1).ToString;
  FCount := 0;
  LastUsed := Now;
  LockedToDestroy := False;

  if not Assigned(FConnection) then
    // FConnection := TDMInterBaseConnection.Create;
    FConnection := TDMFireDACConnection.Create(ADriver);
  if not Assigned(FDataSet) then
    // FDataSet := TDSInterBase.Create(FConnection);
    FDataSet := TDSFireDAC.Create(FConnection);
end;

destructor TPoolConnectionItem.Destroy;
begin

  inherited;
end;

procedure TPoolConnectionItem.Disconnect;
begin
  if Assigned(FConnection) then
    FConnection.RollBack('TPoolConnectionItem.Disconnect');
  FConnection.Close;
  Reference := EmptyStr;
  Inc(FCount, -1);
  if FCount < 0 then
    FCount := 0;
  LastUsed := Now;
  LockedToDestroy := False;
end;

function TPoolConnectionItem.GetConnection: IConnection;
begin
  Result := FConnection;
end;

function TPoolConnectionItem.GetDataSet: IDataSet;
begin
  Result := FDataSet;
end;

procedure TPoolConnectionItem.SetLastUsed(const Value: TDateTime);
begin
  FLastUsed := Value;
end;

procedure TPoolConnectionItem.SetLockedToDestroy(const Value: Boolean);
begin
  FLockedToDestroy := Value;
end;

end.
