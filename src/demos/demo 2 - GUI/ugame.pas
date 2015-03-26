unit uGame;

interface

uses
  glr_core,
  glr_render,
  glr_gamescreens;

type

  { TGame }

  TGame = class (TglrGame)
  private
    GameScreenManager: TglrGameScreenManager;
    MainMenuScreen, SettingsScreen, GameScreen: TglrGameScreen;
  public
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
  uAssets;

{ TGame }

procedure TGame.OnStart;
begin
  Assets.LoadBase();

  MainMenuScreen := TglrMainMenu.Create('MainMenu');

  GameScreenManager := TglrGameScreenManager.Create(3);
  GameScreenManager.Add(MainMenuScreen);


  GameScreenManager.SwitchTo(MainMenuScreen);
end;

procedure TGame.OnFinish;
begin
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

