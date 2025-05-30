unit IdeaL.Lib.CustomMediaPlayer;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,

  FMX.Media;

type
  TMediaPlayerState = (Unavailable, Playing, Stopped, Paused);

  TCustomMediaPlayer = class
  private
    FPausedTime: TMediaTime;
    FStopedFilePath: string;
    FIndexPlayingNow: Integer;
    FAudioList: TStringList;
    FMediaPlayer: TMediaPlayer;
    FOnProcessPlay: TProc;
    FOnStateChange: TProc;
    FOnAudioInfoUpdate: TProc;
    FState: TMediaPlayerState;

    procedure CreateListToPlay();
    function GetCurrentMediaTime: TMediaTime;
    procedure SetCurrentMediaTime(const Value: TMediaTime);
    function GetDurationMediaTime: TMediaTime;
    procedure DoOnProcessPlay();
    procedure DoStateChange(const AState: TMediaPlayerState);
    procedure DoAudioInfoUpdate();
    function GetAudioPlayingNow: string;
    function GetMediaTimeInSeconds(AValue: TMediaTime): Integer;
    function GetSeconds: Integer; overload;
    procedure SetSeconds(Value: Integer);
    function GetDuration: Integer;

  type
    TProcessThread = class(TThread)
    private
      [weak]
      FMusicPlayer: TCustomMediaPlayer;
      FDoTerminate: Boolean;
      FIsTerminated: Boolean;
      FOnProcessPlay: TProc;
      procedure DoProcessPlay;
    published
      procedure DoTerminate; override;
    public
      constructor Create(CreateSuspended: Boolean;
        AMusicPlayer: TCustomMediaPlayer; processHandler: TProc);
      destructor Destroy; override;
      procedure Execute; override;
      procedure LocalTerminate;

      procedure LocalWaitFor;
    end;

  var
    FThread: TProcessThread;
    { private declarations }
  protected
    { protected declarations }
  public
    constructor Create();
    destructor Destroy; override;

    property MediaPlayer: TMediaPlayer read FMediaPlayer;
    property AudioList: TStringList read FAudioList;
    property CurrentMediaTime: TMediaTime read GetCurrentMediaTime write SetCurrentMediaTime;
    property DurationMediaTime: TMediaTime read GetDurationMediaTime;
    property State: TMediaPlayerState read FState;
    property AudioPlayingNow: string read GetAudioPlayingNow;
    property Seconds: Integer read GetSeconds write SetSeconds;
    property Duration: Integer read GetDuration;

    property OnProcessPlay: TProc read FOnProcessPlay write FOnProcessPlay;
    property OnStateChange: TProc read FOnStateChange write FOnStateChange;
    property OnAudioInfoUpdate: TProc read FOnAudioInfoUpdate write FOnAudioInfoUpdate;

    procedure Stop();
    procedure Play(const AIndexToPlay: Integer = -1);
    procedure Pause();
    procedure Next();
    procedure Previous();
    function IsPlaying(): Boolean;

    procedure Add(const AValue: string); overload;
    procedure Add(const AValue: TStringList); overload;
    procedure Add(const AValue: TArray<string>); overload;
    { public declarations }
  published
    { published declarations }
  end;

implementation

{ TCustomMediaPlayer }

procedure TCustomMediaPlayer.Add(const AValue: string);
begin
  if FileExists(AValue) then
    AudioList.Add(AValue);
end;

procedure TCustomMediaPlayer.Add(const AValue: TStringList);
var
  i: Integer;
begin
  for i := 0 to Pred(AValue.Count) do
    Add(AValue[i]);
end;

procedure TCustomMediaPlayer.Add(const AValue: TArray<string>);
var
  i: Integer;
begin
  for i := 0 to Pred(Length(AValue)) do
    Add(AValue[i]);
end;

constructor TCustomMediaPlayer.Create();
begin
  FIndexPlayingNow := -1;
  FPausedTime := 0;
  FStopedFilePath := EmptyStr;
  FMediaPlayer := TMediaPlayer.Create(nil);
  FAudioList := TStringList.Create;
  FOnProcessPlay := nil;
  FOnAudioInfoUpdate := nil;

  Stop;

  FThread := TProcessThread.Create(True, Self, DoOnProcessPlay);
  FThread.Start;
end;

procedure TCustomMediaPlayer.CreateListToPlay();
var
  i: Integer;
  LCountAudioToPlay: Integer;
begin
  LCountAudioToPlay := 0;

  i := 0;
  while i < AudioList.Count do
  begin
    if not FileExists(AudioList[i]) then
      AudioList.Delete(i)
    else
      Inc(i, 1);
  end;

  if (AudioList.Count = 0) then
    raise Exception.Create('Nenhum áudio marcado para iniciar');
end;

destructor TCustomMediaPlayer.Destroy;
begin
  FOnProcessPlay := nil;
  FOnAudioInfoUpdate := nil;
  FOnStateChange := nil;

  FThread.LocalTerminate;
  FThread.LocalWaitFor;
  Stop;
  FreeAndNil(FAudioList);
  FreeAndNil(FMediaPlayer);
  inherited;
end;

function TCustomMediaPlayer.GetAudioPlayingNow: string;
begin
  Result := EmptyStr;
  if (FIndexPlayingNow < AudioList.Count) and
    (FileExists(AudioList[FIndexPlayingNow]))
  then
    Result := AudioList[FIndexPlayingNow];
end;

function TCustomMediaPlayer.GetCurrentMediaTime: TMediaTime;
begin
  if (State = TMediaPlayerState.Playing) then
    Result := FMediaPlayer.CurrentTime
  else if (FState = TMediaPlayerState.Paused) then
    Result := FPausedTime
  else
    Result := 0;
end;

function TCustomMediaPlayer.GetDurationMediaTime: TMediaTime;
begin
  if (State = TMediaPlayerState.Playing) or (State = TMediaPlayerState.Paused)
  then
    Result := FMediaPlayer.Duration
  else
    Result := 0;
end;

function TCustomMediaPlayer.GetDuration: Integer;
begin
  Result := GetMediaTimeInSeconds(FMediaPlayer.Duration);
end;

function TCustomMediaPlayer.GetMediaTimeInSeconds(AValue: TMediaTime): Integer;
begin
{$IF CompilerVersion >= 34.0}
  Result := Trunc(AValue / MediaTimeScale);
{$ELSE}
{$IFDEF MSWINDOWS}
  Result := Trunc(AValue / MediaTimeScale);
{$ELSE}
  Result := Trunc(AValue / 1000);
{$ENDIF}
{$ENDIF}
end;

function TCustomMediaPlayer.GetSeconds: Integer;
begin
  Result := GetMediaTimeInSeconds(CurrentMediaTime);
end;

function TCustomMediaPlayer.IsPlaying: Boolean;
begin
  Result := State = TMediaPlayerState.Playing;
end;

procedure TCustomMediaPlayer.Next;
var
  LIndexToPlay: Integer;
begin
  LIndexToPlay := FIndexPlayingNow + 1;

  Stop;

  if (LIndexToPlay < AudioList.Count) then
    Play(LIndexToPlay)
  else
    FIndexPlayingNow := -1;
end;

procedure TCustomMediaPlayer.DoAudioInfoUpdate;
begin
  // It's showing problems hahahah
  {if (Assigned(FOnAudioInfoUpdate)) then
    FOnAudioInfoUpdate;}
end;

procedure TCustomMediaPlayer.DoOnProcessPlay;
begin
  if Assigned(FOnProcessPlay) then
    TThread.Queue(TThread.CurrentThread,
      procedure
      begin
        FOnProcessPlay;
      end);
end;

procedure TCustomMediaPlayer.DoStateChange(const AState: TMediaPlayerState);
begin
  FState := AState;

  if (Assigned(FOnStateChange)) then
    FOnStateChange
end;

procedure TCustomMediaPlayer.Pause;
var
  LPausedTime: TMediaTime;
begin
  DoStateChange(Paused);
  LPausedTime := FMediaPlayer.CurrentTime;
  Stop;
  DoStateChange(Paused);
  FPausedTime := LPausedTime;
  DoOnProcessPlay;
end;

procedure TCustomMediaPlayer.Play(const AIndexToPlay: Integer);
var
  LAudioToPlay: string;
begin
  LAudioToPlay := EmptyStr;
  if (AIndexToPlay <> -1) then
  begin
    if (FIndexPlayingNow = AIndexToPlay) and (IsPlaying) then
      Exit;
    FIndexPlayingNow := AIndexToPlay;
  end;

  if (FIndexPlayingNow = -1) then
    FIndexPlayingNow := 0;

  CreateListToPlay();

  while
    (FIndexPlayingNow < 0) or
    (FIndexPlayingNow > Pred(AudioList.Count)) or
    not(FileExists(AudioList[FIndexPlayingNow]))
    do
  begin
    Inc(FIndexPlayingNow, 1);

    if (FIndexPlayingNow > Pred(AudioList.Count)) then
      Break;
  end;

  LAudioToPlay := GetAudioPlayingNow;

  if (LAudioToPlay.Trim.IsEmpty) then
    Exit;

  DoStateChange(Stopped);
  FMediaPlayer.Stop;
  FMediaPlayer.FileName := GetAudioPlayingNow;
  FMediaPlayer.CurrentTime := FPausedTime;
  FMediaPlayer.Play;

  FPausedTime := 0;

  DoStateChange(Playing);
  DoAudioInfoUpdate();
end;

procedure TCustomMediaPlayer.Previous;
var
  LIndexToPlay: Integer;
begin
  LIndexToPlay := FIndexPlayingNow - 1;

  Stop;

  if (LIndexToPlay >= 0) then
    Play(LIndexToPlay)
  else
    FIndexPlayingNow := -1;
end;

procedure TCustomMediaPlayer.SetCurrentMediaTime(const Value: TMediaTime);
begin
  FMediaPlayer.CurrentTime := Value;
end;

procedure TCustomMediaPlayer.SetSeconds(Value: Integer);
begin
{$IF CompilerVersion >= 34.0}
  CurrentMediaTime := Value * MediaTimeScale;
{$ELSE}
{$IFDEF MSWINDOWS}
  CurrentTime := Value * MediaTimeScale;
{$ELSE}
  CurrentTime := Value * 1000;
{$ENDIF}
{$ENDIF}

end;

procedure TCustomMediaPlayer.Stop;
begin
  DoStateChange(Stopped);

  FStopedFilePath := FMediaPlayer.FileName;
  FMediaPlayer.Stop;
  FMediaPlayer.CurrentTime := 0;
  FPausedTime := 0;

  DoOnProcessPlay();
end;

{ TCustomMediaPlayer.TProcessThread }

constructor TCustomMediaPlayer.TProcessThread.Create(CreateSuspended
  : Boolean; AMusicPlayer: TCustomMediaPlayer; processHandler: TProc);
begin
  inherited Create(CreateSuspended);
  FDoTerminate := False;
  FIsTerminated := False;
  FOnProcessPlay := processHandler;
  FMusicPlayer := AMusicPlayer;
  FreeOnTerminate := True;
end;

destructor TCustomMediaPlayer.TProcessThread.Destroy;
begin
  FMusicPlayer := nil;
  inherited;
end;

procedure TCustomMediaPlayer.TProcessThread.DoProcessPlay;
begin
  if (FIsTerminated) then
    Exit;

  if (Assigned(FOnProcessPlay)) then
    FOnProcessPlay;

  if (FIsTerminated) then
    Exit;

  if (Assigned(FMusicPlayer)) then
  begin
    case FMusicPlayer.State of
      Unavailable:
        ;
      Playing:
        begin
          TThread.Sleep(200);
          if (FIsTerminated) then
            Exit;

          if (Assigned(FMusicPlayer)) then
            if (FMusicPlayer.CurrentMediaTime >= Trunc(FMusicPlayer.DurationMediaTime * 0.99))
              and (FMusicPlayer.IsPlaying)
            then
              FMusicPlayer.Next;
        end;
      Stopped:
        ;
      Paused:
        ;
    end;
  end;
end;

procedure TCustomMediaPlayer.TProcessThread.Execute;
begin
  inherited;
  while Assigned(FMusicPlayer) do
  begin
    if (FIsTerminated) then
      Break;
    TThread.Sleep(200);
    if (Assigned(FMusicPlayer)) then
      if (FMusicPlayer.IsPlaying) then
        DoProcessPlay;
  end;
  FDoTerminate := True;
end;

procedure TCustomMediaPlayer.TProcessThread.LocalTerminate;
begin
  FIsTerminated := True;
end;

procedure TCustomMediaPlayer.TProcessThread.LocalWaitFor;
begin
  while True do
  begin
    if (FDoTerminate) then
      Break;
  end;
end;

procedure TCustomMediaPlayer.TProcessThread.DoTerminate;
begin
  FDoTerminate := True;
  inherited;
end;

end.
