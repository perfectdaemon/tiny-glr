unit glrSound;

interface

uses
  Windows, Bass;

type
  {Класс для проигрывания музыки}
  TpdSoundSystem = class
  private
    FEnabled: Boolean;
    FMusic: HSTREAM;
    FSoundVolume: Single;
    FMusicVolume: Single;
    procedure SetEnabled(const aEnabled: Boolean);
    procedure SetMusicVolume(const Value: Single);
    procedure SetSoundVolume(const Value: Single);
  public
    constructor Create(aHandle: THandle); virtual;
    destructor Destroy(); override;

    function LoadSample(const aFile: String): HSAMPLE;
    procedure PlaySample(const aSample: HSAMPLE);
    procedure FreeSample(const aSample: HSAMPLE);

    function LoadMusic(const aFile: String; aLoop: Boolean = True): HSTREAM;
    procedure PlayMusic(const aMusic: HSTREAM);
    procedure PauseMusic();
    procedure FreeMusic(const aMusic: HSTREAM);
    procedure SetMusicFade(const aMusic: HSTREAM; const aTime: LongWord);

    property Enabled: Boolean read FEnabled write SetEnabled;
    property SoundVolume: Single read FSoundVolume write SetSoundVolume;
    property MusicVolume: Single read FMusicVolume write SetMusicVolume;
  end;


implementation

uses
  glrMath;

{ TpdSoundSystem }

constructor TpdSoundSystem.Create(aHandle: THandle);
begin
  inherited Create();
	if (HIWORD(BASS_GetVersion) = BASSVERSION) then
	begin
  	BASS_Init(-1, 44100, 0, aHandle, nil);
    BASS_SetConfig(BASS_CONFIG_BUFFER, 4000); //Размер буфера (максимум 5000)
	end;

  FEnabled := True;
  FMusic := 0;
  FSoundVolume := 1.0;
  FMusicVolume := 1.0;
end;

destructor TpdSoundSystem.Destroy;
begin
  BASS_StreamFree(FMusic);
	BASS_Free();
  inherited;
end;

procedure TpdSoundSystem.FreeMusic(const aMusic: HSTREAM);
begin
  BASS_StreamFree(aMusic);
end;

procedure TpdSoundSystem.FreeSample(const aSample: HSAMPLE);
begin
  BASS_SampleFree(aSample);
end;

function TpdSoundSystem.LoadMusic(const aFile: String; aLoop: Boolean = True): HSTREAM;
var
  flags: LongWord;
begin
  if aLoop then
    flags := BASS_SAMPLE_LOOP
  else
    flags := 0;
  Result := BASS_StreamCreateFile(False, PWideChar(aFile), 0, 0, flags{$IFDEF UNICODE} or BASS_UNICODE {$ENDIF});
end;

function TpdSoundSystem.LoadSample(const aFile: String): HSAMPLE;
begin
  Result := BASS_SampleLoad(False, PWideChar(aFile), 0, 0, 3, BASS_SAMPLE_OVER_POS{$IFDEF UNICODE} or BASS_UNICODE {$ENDIF});
  BASS_SampleGetChannel(Result, false);
end;

procedure TpdSoundSystem.PlayMusic(const aMusic: HSTREAM);
begin
  if (FMusic <> 0) then
  begin
    BASS_ChannelStop(FMusic);
    FMusic := aMusic;
    BASS_ChannelPlay(FMusic, True);
    BASS_ChannelSlideAttribute(FMusic, BASS_ATTRIB_VOL, FMusicVolume, 1000);
  end
  else
  begin
    FMusic := aMusic;
    BASS_ChannelPlay(FMusic, True);
    BASS_ChannelSetAttribute(FMusic, BASS_ATTRIB_VOL, FMusicVolume);
  end;
end;

procedure TpdSoundSystem.PlaySample(const aSample: HSAMPLE);
var
  channel: HCHANNEL;
begin
  channel := BASS_SampleGetChannel(aSample, False);
  if channel <> 0 then
  begin
    BASS_ChannelPlay(channel, true);
    BASS_ChannelSetAttribute(channel, BASS_ATTRIB_VOL, FSoundVolume);
  end;
end;

procedure TpdSoundSystem.SetEnabled(const aEnabled: Boolean);
begin
  if FEnabled <> aEnabled then
  begin
    FEnabled := aEnabled;
    if FEnabled then
    begin
      BASS_Start();
      if BASS_ChannelIsActive(FMusic) <> BASS_ACTIVE_PLAYING then
      begin
        BASS_ChannelPlay(FMusic, True);
        BASS_ChannelSlideAttribute(FMusic, BASS_ATTRIB_VOL, FMusicVolume, 1000);
      end;
//      PlayMusic(FMusic);
    end
    else
    begin
      BASS_Pause();
//      PauseMusic();
    end;
  end;
end;

procedure TpdSoundSystem.SetMusicFade(const aMusic: HSTREAM;
  const aTime: LongWord);
begin
  BASS_ChannelSlideAttribute(aMusic, BASS_ATTRIB_VOL, 0.0, aTime);
end;

procedure TpdSoundSystem.SetMusicVolume(const Value: Single);
begin
  FMusicVolume := Clamp(Value, 0, 1);
  BASS_ChannelSetAttribute(FMusic, BASS_ATTRIB_VOL, FMusicVolume);
end;

procedure TpdSoundSystem.SetSoundVolume(const Value: Single);
begin
  FSoundVolume := Clamp(Value, 0, 1);
end;

procedure TpdSoundSystem.PauseMusic;
begin
  BASS_ChannelPause(FMusic);
end;

end.
