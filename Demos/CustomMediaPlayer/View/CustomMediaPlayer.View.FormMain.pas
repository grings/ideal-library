unit CustomMediaPlayer.View.FormMain;

{
  This project/class was tested just with Rad10.4.1, please let me know if you find
  some issue running on another Rad version

  Tested on Windows10 and Android10
}

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.Controls.Presentation, FMX.Layouts,

  System.Math,
  IdeaL.Lib.CustomMediaPlayer, FMX.Media;

const
  C_PATH_PAUSE_SINGLE = 'M12 38h8V10h-8v28zm16-28v28h8V10h-8z';
  C_PATH_PLAY_SINGLE = 'M16 10v28l22-14z';

type
  TFormMain = class(TForm)
    lytMediaPlayerOptions: TLayout;
    lytBottom: TLayout;
    lytBottomBackground: TLayout;
    btnButtonSecondsPlus: TButton;
    lytButtons: TLayout;
    btnPlay: TButton;
    pthPlay: TPath;
    btnStop: TButton;
    pthStop: TPath;
    btnNext: TButton;
    pthNext: TPath;
    btnPrevious: TButton;
    pthPrevious: TPath;
    lytAudioInfo: TLayout;
    lytPthAudioInfo: TLayout;
    pthAudioInfo: TPath;
    lblAudioInfoDescription: TLabel;
    lytProgress: TLayout;
    trkProgress: TTrackBar;
    lblPlayCurrentTime: TLabel;
    lytButtonSecondsPlus: TLayout;
    lytButtonSecondsLess: TLayout;
    btnButtonSecondsLess: TButton;
    Path1: TPath;
    Path2: TPath;
    MediaPlayerControl1: TMediaPlayerControl;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnPlayClick(Sender: TObject);
    procedure btnButtonSecondsLessClick(Sender: TObject);
    procedure trkProgressChange(Sender: TObject);
  private
    FMediaPlayer: TCustomMediaPlayer;

    procedure AudioInfoUpdate();
    procedure StateChange();
    procedure UpdateCurrentTime();

    procedure LoadAudio;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

uses
  System.IOUtils;

{$R *.fmx}


procedure TFormMain.AudioInfoUpdate;
begin
  lblAudioInfoDescription.Text := ExtractFileName(FMediaPlayer.AudioPlayingNow);
end;

procedure TFormMain.btnButtonSecondsLessClick(Sender: TObject);
var
  LMultiple: Integer;
begin
  TButton(Sender).Enabled := False;
  try
    LMultiple := 1;
    if (Sender = btnButtonSecondsLess) then
      LMultiple := -1;
    FMediaPlayer.Seconds := FMediaPlayer.Seconds + (10 * LMultiple);
  finally
    TButton(Sender).Enabled := True;
  end;
end;

procedure TFormMain.btnPlayClick(Sender: TObject);
begin
  try
    if (Sender = btnStop) then
      FMediaPlayer.Stop
    else if (Sender = btnPrevious) then
      FMediaPlayer.Previous
    else if (Sender = btnNext) then
      FMediaPlayer.Next
    else
      case btnPlay.Tag of
        0:
          begin
            FMediaPlayer.Play;
          end;

        1:
          FMediaPlayer.Pause();
      end;
  except
    on E: Exception do
      ShowMessage('ERROR: ' + E.Message);
  end;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  FMediaPlayer := TCustomMediaPlayer.Create;
  FMediaPlayer.OnProcessPlay := UpdateCurrentTime;
  FMediaPlayer.OnStateChange := StateChange;
  FMediaPlayer.OnAudioInfoUpdate := AudioInfoUpdate;
  FMediaPlayer.Stop;

  LoadAudio;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FMediaPlayer);
end;

procedure TFormMain.LoadAudio;
var
  LPath: string;
begin
  {
    It doesn't matter how you will do that, do it your way.
  }
{$IFDEF MSWINDOWS}
  LPath := ExtractFilePath(System.SysUtils.GetCurrentDir);
  LPath := TDirectory.GetParent(ExcludeTrailingPathDelimiter(LPath));
  LPath := TDirectory.GetParent(ExcludeTrailingPathDelimiter(LPath));
  LPath := System.IOUtils.TPath.Combine(LPath, 'File');
{$ENDIF}
{$IFDEF ANDROID}
  LPath := System.IOUtils.TPath.GetHomePath;
{$ENDIF}
  (*
    {$IFDEF IOS}
    Result := System.IOUtils.TPath.GetDocumentsPath;
    {$ENDIF}
  *)

  FMediaPlayer.AudioList.Clear;
  FMediaPlayer.Add(System.IOUtils.TPath.Combine(LPath, 'Falling_Together.mp3'));
  FMediaPlayer.Add(System.IOUtils.TPath.Combine(LPath, 'Grow_KV.mp3'));
  FMediaPlayer.Add(System.IOUtils.TPath.Combine(LPath, 'Last_Nights_Dream_Tryezz.mp3'));
  FMediaPlayer.Add(System.IOUtils.TPath.Combine(LPath, 'Looking_Back_Declan_DP.mp3'));
  FMediaPlayer.Add(System.IOUtils.TPath.Combine(LPath, 'Rainforest_Vendredi.mp3'));
  FMediaPlayer.Add(System.IOUtils.TPath.Combine(LPath, 'Wanderlust_extenz.mp3'));
end;

procedure TFormMain.StateChange;
begin
  case FMediaPlayer.State of
    TMediaPlayerState.Playing:
      begin
        btnPlay.Tag := 1;
        pthPlay.Data.Data := C_PATH_PAUSE_SINGLE;

        btnPlay.Enabled := True;
        btnStop.Enabled := True;
        btnPrevious.Enabled := True;
        btnNext.Enabled := True;
      end;

    TMediaPlayerState.Paused, TMediaPlayerState.Stopped:
      begin
        btnPlay.Tag := 0;
        pthPlay.Data.Data := C_PATH_PLAY_SINGLE;

        if (FMediaPlayer.State = TMediaPlayerState.Stopped) then
        begin
          btnPlay.Enabled := True;
          btnStop.Enabled := False;
          btnPrevious.Enabled := False;
          btnNext.Enabled := False;
        end
        else
        begin
          btnPlay.Enabled := True;
          btnStop.Enabled := True;
          btnPrevious.Enabled := True;
          btnNext.Enabled := True;
        end;
      end;
  end;
end;

procedure TFormMain.trkProgressChange(Sender: TObject);
begin
  // Take a look at UpdateCurrentTime
  FMediaPlayer.CurrentMediaTime := Trunc(FMediaPlayer.DurationMediaTime * (trkProgress.Value / 100));
end;

procedure TFormMain.UpdateCurrentTime;
var
  LTime: Double;
  LSecs: Integer;
  LPerc: Integer;
  LSecsPerDay: Int64;
  LCurrentTime: Int64;
  LDuration: Int64;
begin
  try
    LSecsPerDay := SecsPerDay;
    LCurrentTime := FMediaPlayer.CurrentMediaTime;
    LSecs := FMediaPlayer.Seconds;
    LDuration := FMediaPlayer.DurationMediaTime;
    LPerc := Trunc((LCurrentTime / IfThen(LDuration <= 0, 1, LDuration)) * 100);
    LTime := LSecs / LSecsPerDay;

    {
    If you want to change manually the current time using the progress bar,
    so you have to disable the OnChange
    }
    trkProgress.OnChange := nil;
    trkProgress.Value := LPerc;
    trkProgress.OnChange := trkProgressChange;

    if (FMediaPlayer.Seconds < 3600) then
      lblPlayCurrentTime.Text := FormatDateTime('nn:ss', LTime)
    else
      lblPlayCurrentTime.Text := FormatDateTime('hh:nn:ss', LTime);
  except
    on E: Exception do
    begin
      StrToInt('1');
    end;
  end;
end;

end.
