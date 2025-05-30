unit IdeaL.Lib.Thread;

interface

uses
  System.SysUtils,
  System.Classes;

type
  // TProcedureExcept = reference to procedure(const AExceptionMessage: string);

  TThreadCustom = class(System.Classes.TThread)
  private
    function GetIsTerminated: Boolean;
  public

    property IsTerminated: Boolean read GetIsTerminated;

    class function Start(AOnShow, AOnProcess, AOnComplete: TProc;
      AOnFinally: TProc<Boolean>; AOnError: TProc<string> = nil;
      const ADoCompleteWithErro: Boolean = True): TThreadCustom;
  end;

implementation

{ TThreadCustom }

function TThreadCustom.GetIsTerminated: Boolean;
begin
  Result := Terminated;
end;

class function TThreadCustom.Start(AOnShow, AOnProcess, AOnComplete: TProc;
  AOnFinally: TProc<Boolean>; AOnError: TProc<string>;
  const ADoCompleteWithErro: Boolean): TThreadCustom;
begin
  Result := TThreadCustom(TThreadCustom.CreateAnonymousThread(
    procedure
    var
      LDoComplete: Boolean;
      LIsTerminated: Boolean;
    begin
      LIsTerminated := False;
      LDoComplete := True;
      try
        try
          try
            if (Assigned(AOnShow)) then
            begin
              TThread.Synchronize(nil,
                procedure()
                begin
                  AOnShow
                end);
            end;

            if (Assigned(AOnProcess)) then
            begin
              AOnProcess;
            end;
          except
            on E: Exception do
            begin
              if (TThread.CurrentThread.CheckTerminated) then
                Exit;
              LDoComplete := ADoCompleteWithErro;
              if (Assigned(AOnError)) then
              begin
                TThread.Synchronize(nil,
                  procedure()
                  begin
                    AOnError(E.Message);
                  end);
              end;
            end;
          end;
        finally
          if not(TThread.CurrentThread.CheckTerminated) then
            if (LDoComplete) then
              if (Assigned(AOnComplete)) then
              begin
                TThread.Synchronize(nil,
                  procedure()
                  begin
                    AOnComplete
                  end);
              end;
        end;
      finally
        if Assigned(AOnFinally) then
        begin
          LIsTerminated := TThread.CurrentThread.CheckTerminated;
          TThread.Synchronize(nil,
            procedure()
            begin
              AOnFinally(LIsTerminated);
            end);
        end;
      end;
    end)
    );
  Result.FreeOnTerminate := True;
  TThread(Result).Start;

  {
    TThreadCustom.Start(
    procedure
    begin //OnShow
    end,
    procedure
    begin //OnProcess
    end,
    procedure
    begin //OnComplete
    end,
    procedure (AIsTerminated: Boolean)
    begin //OnFinally
    end,
    procedure (AExceptionMessage: string)
    begin //OnError
    end, //
    False //
    );
  }
end;

end.
