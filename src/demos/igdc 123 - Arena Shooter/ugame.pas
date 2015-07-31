unit uGame;

interface

uses
  glr_core,
  glr_render,
  glr_tween,
  glr_gamescreens;

type

  { TGame }

  TGame = class (TglrGame)
  private
  public
    GameScreenManager: TglrGameScreenManager;
    MainMenuScreen, SettingsScreen, GameScreen: TglrGameScreen;
    Tweener: TglrTweener;
    procedure OnFinish; override;
    procedure OnInput(Event: PglrInputEvent); override;
    procedure OnPause; override;
    procedure OnRender; override;
    procedure OnResize(aNewWidth, aNewHeight: Integer); override;
    procedure OnResume; override;
    procedure OnStart; override;
    procedure OnUpdate(const dt: Double); override;
  end;

var
  Game: TGame;

implementation

uses
  uGSMainMenu,
  uGSSettingsMenu,
  uGSGame,
  uAssets;

{ TGame }

procedure TGame.OnStart;
begin
  Assets.LoadBase();
  Texts.LoadMenu();
  Colors.Load();

  Tweener := TglrTweener.Create();

  MainMenuScreen := TglrMainMenu.Create('MainMenu');
  SettingsScreen := TglrSettingsMenu.Create('SettingsMenu');
  GameScreen     := TglrTheGame.Create('Game');

  GameScreenManager := TglrGameScreenManager.Create(3);
  GameScreenManager.Add(MainMenuScreen);
  GameScreenManager.Add(SettingsScreen);
  GameScreenManager.Add(GameScreen);

  GameScreenManager.SwitchTo(MainMenuScreen);
end;

procedure TGame.OnFinish;
begin
  Tweener.Free();
  GameScreenManager.Free(True);
  Assets.UnloadBase();
end;

procedure TGame.OnInput(Event: PglrInputEvent);
begin
  GameScreenManager.Input(Event);
end;

procedure TGame.OnUpdate(const dt: Double);
begin
  GameScreenManager.Update(dt);
  Tweener.Update(dt);
end;

procedure TGame.OnRender;
begin
  GameScreenManager.Render();
end;

procedure TGame.OnPause;
begin
  // Calls when engine receives that app was lost focus
end;

procedure TGame.OnResume;
begin
  // Calls when engine receives that app was focused
end;

procedure TGame.OnResize(aNewWidth, aNewHeight: Integer);
begin
  // Calls when windows has changed size
end;


end.

