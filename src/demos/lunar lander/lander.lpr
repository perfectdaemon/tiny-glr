program lander;

uses
  glrMath, ogl, tinyglr, uSpace, uBox2DImport, UPhysics2D, UPhysics2DTypes,
  uMoon;

const
  MAX_FUEL = 100.0;
  FUEL_PER_SEC = 3.0;

type

  { TFuelLevel }

  TFuelLevel = class
    Level: Single;
    sBack, sLevel: TglrSprite;
    sLevelOrigWidth: Single;
    constructor Create();
    destructor Destroy();

    procedure Update(const dt: Double);
  end;


  { TGame }

  TGame = class (TglrGame)
  protected
    //Resources
    Atlas: TglrTextureAtlas;
    Font: TglrFont;
    FontBatch: TglrFontBatch;
    SpriteBatch: TglrSpriteBatch;
    Material, MoonMaterial: TglrMaterial;

    World: Tglrb2World;

    //Sprites
    Ship: TglrSprite;
    Flame: TglrSprite;

    FuelLevel: TFuelLevel;

    Space: TSpace;
    Moon: TMoon;

    b2Ship: Tb2Body;

    //Scenes
    Scene: TglrScene;
  public
    procedure OnFinish; override;
    procedure OnInput(aType: TglrInputType; aKey: TglrKey; X, Y,
      aOtherParam: Integer); override;
    procedure OnPause; override;
    procedure OnRender; override;
    procedure OnResize(aNewWidth, aNewHeight: Integer); override;
    procedure OnResume; override;
    procedure OnStart; override;
    procedure OnUpdate(const dt: Double); override;
  end;

var
  Game: TGame;

{ TFuelLevel }

constructor TFuelLevel.Create;
begin
  inherited;
  sBack := TglrSprite.Create();
  sBack.SetTextureRegion(Game.Atlas.GetRegion('fuel_level_back.png'));
  sLevel := TglrSprite.Create(1, 1, dfVec2f(0, 0.5));
  sLevel.SetTextureRegion(Game.Atlas.GetRegion('fuel_level.png'));
  sLevel.Height := sLevel.Height * 0.9;

  sLevelOrigWidth := sLevel.Width;
  Game.SpriteBatch.Childs.Add(sBack);
  Game.SpriteBatch.Childs.Add(sLevel);

  Level := MAX_FUEL;
end;

destructor TFuelLevel.Destroy;
begin

  inherited;
end;

procedure TFuelLevel.Update(const dt: Double);
begin
  sBack.Position := Game.Ship.Position + dfVec3f(0, -65, 1);
  sLevel.Position := sBack.Position + dfVec3f(-50.5, 5.5, 1);
  sLevel.Width := sLevelOrigWidth * Level / MAX_FUEL;
  sLevel.SetVerticesColor(dfVec4f(1, 0.3 + Level / MAX_FUEL, 0.3 + Level / MAX_FUEL, 1));
end;

{ TGame }


procedure TGame.OnStart;
begin
  Render.SetClearColor(0.1, 0.1, 0.13);

  Atlas := TglrTextureAtlas.Create(
    FileSystem.ReadResource('lander/atlas.tga'),
    FileSystem.ReadResource('lander/atlas.atlas'),
    'tga', 'cheetah');
  Material := TglrMaterial.Create();
  Material.Shader.Free();
  Material.Shader := Default.SpriteShader;
  Material.AddTexture(Atlas, 'uDiffuse');

  MoonMaterial := TglrMaterial.Create();
  MoonMaterial.Shader.Attach(FileSystem.ReadResource('lander/MoonShaderV.txt'), stVertex);
  MoonMaterial.Shader.Attach(FileSystem.ReadResource('lander/MoonShaderF.txt'), stFragment);
  MoonMaterial.Shader.Link();

  World := Tglrb2World.Create(TVector2.From(0, 0.1), false, 1 / 40, 8);

  Ship := TglrSprite.Create();
  Ship.SetTextureRegion(Atlas.GetRegion('lander.png'));
  Ship.Position := dfVec3f(200, 200, 1);

  b2Ship := Box2d.Circle(World, Ship, 0.5, 0.2, 0.0, $0002, $0001, 1);

  Flame := TglrSprite.Create();
  Flame.SetTextureRegion(Atlas.GetRegion('flame.png'));
  Flame.Position := dfVec3f(-50, 0, -5);
  Flame.Parent := Ship;

  SpriteBatch := TglrSpriteBatch.Create();
  SpriteBatch.Material := Material;
  SpriteBatch.Childs.Add(Flame);
  SpriteBatch.Childs.Add(Ship);

  Scene := TglrScene.Create();
  Scene.Camera.ProjectionMode := pmOrtho;
  Scene.Camera.SetCamera(dfVec3f(0, 0, 100), dfVec3f(0, 0, 0), dfVec3f(0, 1, 0));
  Scene.Camera.Viewport(0, 0, Render.Width, Render.Height, 90, -1, 200);
  Scene.Root.Childs.Add(SpriteBatch);

  FuelLevel := TFuelLevel.Create();

  Space := TSpace.Create(dfVec2f(Render.Width, Render.Height), Atlas.GetRegion('particle.png'), Material, 3);
  Space.Camera := Scene.Camera;
  //Scene.Root.Childs.Add(Space.fBatch);

  Moon := TMoon.Create(MoonMaterial, nil);
  Moon.MaxY := Render.Height;
end;

procedure TGame.OnFinish;
begin
  Space.Free();
  Moon.Free();
  Scene.Free();

  FuelLevel.Free();
  Material.Free();
  MoonMaterial.Free();
end;

procedure TGame.OnInput(aType: TglrInputType; aKey: TglrKey; X, Y,
  aOtherParam: Integer);
begin

end;

procedure TGame.OnPause;
begin

end;

procedure TGame.OnRender;
begin
  Scene.RenderScene();
  Render.Params.ModelViewProj := Render.Params.ViewProj;
  Space.RenderSelf();
  Moon.RenderSelf();
end;

procedure TGame.OnResize(aNewWidth, aNewHeight: Integer);
begin

end;

procedure TGame.OnResume;
begin

end;

procedure TGame.OnUpdate(const dt: Double);
begin
  World.Update(dt);

  Box2d.SyncObjects(b2Ship, Ship, True);

  Ship.Rotation := LerpAngles(Ship.Rotation, (Core.Input.MousePos - dfVec2f(Ship.Position)).GetRotationAngle(), 5 * dt);
  FuelLevel.Update(dt);
  Flame.Visible := (Core.Input.Touch[1].IsDown); //left button
  if Flame.Visible then
    FuelLevel.Level -= dt * FUEL_PER_SEC;

  if Core.Input.KeyDown[kUp] then
    Scene.Camera.Translate(-dt * 200, 0, 0)
  else if Core.Input.KeyDown[kDown] then
    Scene.Camera.Translate(dt * 200, 0, 0);

  if Core.Input.KeyDown[kLeft] then
    Scene.Camera.Translate(0, -dt * 200, 0)
  else if Core.Input.KeyDown[kRight] then
    Scene.Camera.Translate(0, dt * 200, 0);
end;

var
  InitParams: TglrInitParams;

begin
  with InitParams do
  begin
    Width := 1000;
    Height := 600;
    X := 100;
    Y := 100;
    Caption := '`Lunar Lander` © remake by perfect.daemon [tiny-glr ' + TINYGLR_VERSION + ']';
    vSync := True;
    PackFilesPath := '';
    UseDefaultAssets := True;
  end;

  Game := TGame.Create();
  Core.Init(Game, InitParams);
  Core.Loop();
  Core.DeInit();
  Game.Free();
end.

