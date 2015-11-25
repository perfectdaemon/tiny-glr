unit uGame;

interface

uses
  glr_core,
  uPlayer,
  uGUI;

type

  { TGame }

  TGame = class (TglrGame)
  protected
    Gui: TArenaGUIManager;
    Player: TArenaPlayer;
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
  glr_render,
  glr_math,

  uAssets,
  uColors;

{ TGame }

procedure TGame.OnStart;
begin
  Render.SetClearColor(Colors.Background);

  Assets := TArenaAssets.Create();
  Assets.Init();

  Gui := TArenaGUIManager.Create();
  Gui.Init();

  Player := TArenaPlayer.Create();
  Player.Spawn(Vec3f(0, 0, 3));
end;

procedure TGame.OnFinish;
begin
  Player.Free();

  Gui.DeInit();
  Gui.Free();

  Assets.DeInit();
  Assets.Free();
end;

procedure TGame.OnInput(Event: PglrInputEvent);
begin
  if (Player <> nil) then
    Player.OnInputReceived(Event);
  // Calls when engine receives some input info
end;

procedure TGame.OnUpdate(const dt: Double);
begin
  Player.Update(dt);
  Gui.Update(dt);
end;

procedure TGame.OnRender;
begin
  Assets.MainCamera.Update();
  Assets.MainMaterial.Bind();
  Assets.SpriteBatch.Start();
    Player.Render(Assets.SpriteBatch);
  Assets.SpriteBatch.Finish();

  Gui.Render();
end;

procedure TGame.OnPause;
begin
  // Calls when app' window has lost focus
end;

procedure TGame.OnResume;
begin
  // Calls when app' window was focused
end;

procedure TGame.OnResize(aNewWidth, aNewHeight: Integer);
begin
  // Calls when window has changed size
end;


end.

