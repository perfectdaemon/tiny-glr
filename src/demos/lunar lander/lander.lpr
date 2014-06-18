program lander;

uses
  glrMath, ogl, tinyglr;

type

  { TFuelLevel }

  TFuelLevel = class
    Level: Single;
    sBack, sLevel: TglrSprite;
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
    Material: TglrMaterial;

    //Sprites
    Ship: TglrSprite;
    Flame: TglrSprite;

    FuelLevel: TFuelLevel;

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
  sLevel := TglrSprite.Create();
  sLevel.SetTextureRegion(Game.Atlas.GetRegion('fuel_level.png'));
  sLevel.Height := sLevel.Height * 0.9;

  Game.SpriteBatch.Childs.Add(sBack);
  Game.SpriteBatch.Childs.Add(sLevel);
end;

destructor TFuelLevel.Destroy;
begin

  inherited;
end;

procedure TFuelLevel.Update(const dt: Double);
begin
  sBack.Position := Game.Ship.Position + dfVec3f(0, -65, 1);
  sLevel.Position := sBack.Position + dfVec3f(0.5, 5.5, 1);
end;

{ TGame }


procedure TGame.OnStart;
begin
  Render.SetClearColor(0.1, 0.1, 0.13);

  Atlas := TglrTextureAtlas.Create(
    FileSystem.ReadResource('lander/lander.tga'),
    FileSystem.ReadResource('lander/lander.atlas'),
    'tga', 'cheetah');
  Material := TglrMaterial.Create();
  Material.Shader.Free();
  Material.Shader := Default.SpriteShader;
  Material.AddTexture(Atlas, 'uDiffuse');

  Ship := TglrSprite.Create();
  Ship.SetTextureRegion(Atlas.GetRegion('lander.png'));
  Ship.Position := dfVec3f(200, 200, 1);

  Flame := TglrSprite.Create();
  Flame.SetTextureRegion(Atlas.GetRegion('flame.png'));
  Flame.Position := dfVec3f(-50, 0, 1);
  Flame.Parent := Ship;

  SpriteBatch := TglrSpriteBatch.Create();
  SpriteBatch.Material := Material;
  SpriteBatch.Childs.Add(Flame);
  SpriteBatch.Childs.Add(Ship);

  Scene := TglrScene.Create();
  Scene.Camera.ProjectionMode := pmOrtho;
  Scene.Camera.SetCamera(dfVec3f(0, 0, 5), dfVec3f(0, 0, 0), dfVec3f(0, 1, 0));
  Scene.Root.Childs.Add(SpriteBatch);

  FuelLevel := TFuelLevel.Create();
end;

procedure TGame.OnFinish;
begin
  Scene.Free();

  FuelLevel.Free();
  Material.Free();
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
end;

procedure TGame.OnResize(aNewWidth, aNewHeight: Integer);
begin

end;

procedure TGame.OnResume;
begin

end;

procedure TGame.OnUpdate(const dt: Double);
begin
  Ship.Rotation := LerpAngles(Ship.Rotation, (Core.Input.MousePos - dfVec2f(Ship.Position)).GetRotationAngle(), 5 * dt);
  FuelLevel.Update(dt);
  Flame.Visible := (Core.Input.Touch[1].IsDown); //left button
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

