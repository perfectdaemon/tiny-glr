unit uMain;

{$mode delphi}

interface

uses
  tinyglr, glrmath, uBox2DImport, uSpace, uMoon, UPhysics2D;

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
    fEditorText: TglrText;
    fMoonVertex: Integer;
    procedure PhysicsAfter(const FixedDeltaTime: Double);
    procedure PhysicsContactBegin(var contact: Tb2Contact);
  public
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

implementation

uses
  UPhysics2DTypes;

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
{
var
  landerV: array[0..14] of TdfVec2f = (
    (x: 0; y: 1.350000023841858),
		(x:0.25;y:1.4250000715255737),
		(x:0.45000001788139343;y:1.4500000476837158),
		(x:0.5250000357627869;y:1.5),
		(x:0.625;y:1.5750000476837158),
		(x:0.675000011920929;y:1.6750000715255737),
		(x:0.625;y:1.774999976158142),
		(x:0.5250000357627869;y:1.850000023841858),
		(x:0.45000001788139343;y:1.899999976158142),
		(x:0.25;y:1.9250000715255737),
    (x:0;y:2),
		(x:0.17499999701976776;y:1.774999976158142),
		(x:0.02500000037252903;y:1.7000000476837158),
		(x:0.02500000037252903;y:1.649999976158142),
		(x:0.17499999701976776;y:1.5750000476837158));

  i: Integer;
  }
begin
  {
  for i := 0 to Length(landerV) - 1 do
  begin
    landerV[i].y -= 1.34;
    landerV[i] *= 64;
  end;
  }

  Render.SetClearColor(0.1, 0.1, 0.13);

  Atlas := TglrTextureAtlas.Create(
    FileSystem.ReadResource('lander/atlas.tga'),
    FileSystem.ReadResource('lander/atlas.atlas'),
    'tga', 'cheetah');
  Material := TglrMaterial.Create();
  Material.Shader.Free();
  Material.Shader := Default.SpriteShader;
  Material.AddTexture(Atlas, 'uDiffuse');

  Font := TglrFont.Create(FileSystem.ReadResource('lander/Hattori Hanzo17b.bmp'));

  MoonMaterial := TglrMaterial.Create();
  MoonMaterial.Shader.Attach(FileSystem.ReadResource('lander/MoonShaderV.txt'), stVertex);
  MoonMaterial.Shader.Attach(FileSystem.ReadResource('lander/MoonShaderF.txt'), stFragment);
  MoonMaterial.Shader.Link();

  World := Tglrb2World.Create(TVector2.From(0, 9.8 * C_COEF), false, 1 / 40, 8);
  World.AddOnBeginContact(Self.PhysicsContactBegin);
  World.OnAfterSimulation := Self.PhysicsAfter;

  Ship := TglrSprite.Create();
  Ship.SetTextureRegion(Atlas.GetRegion('lander.png'));
  Ship.Position := dfVec3f(200, 200, 1);

  //b2Ship := Box2d.{Circle}Polygon(World, dfVec2f(Ship.Position), landerV, 0.5, 0.2, 0.0, $0002, $0001, 1);
  b2Ship := Box2d.Circle(World, Ship.Width * 0.5 * 0.85, dfVec2f(Ship.Position), 0.5, 0.2, 0.0, $0002, $0001, 1);

  Flame := TglrSprite.Create();
  Flame.SetTextureRegion(Atlas.GetRegion('flame.png'));
  Flame.Position := dfVec3f(-50, 0, -5);
  Flame.Parent := Ship;

  SpriteBatch := TglrSpriteBatch.Create();
  SpriteBatch.Material := Material;
  SpriteBatch.Childs.Add(Flame);
  SpriteBatch.Childs.Add(Ship);

  fEditorText := TglrText.Create();
  fEditorText.Position := dfVec3f(Render.Width / 2 - 150, 20, 5);
  fEditorText.LetterSpacing := 1.2;

  FontBatch := TglrFontBatch.Create(Font);
  FontBatch.Childs.Add(fEditorText);

  Scene := TglrScene.Create();
  Scene.Camera.ProjectionMode := pmOrtho;
  Scene.Camera.ProjectionModePivot := pCenter;
  Scene.Camera.SetCamera(
    dfVec3f(Render.Width / 2, Render.Height / 2, 100),
    dfVec3f(Render.Width / 2, Render.Height / 2, 0),
    dfVec3f(0, 1, 0));
  Scene.Camera.Viewport(0, 0, Render.Width, Render.Height, 90, -1, 200);
  Scene.Root.Childs.Add(SpriteBatch);
  Scene.Root.Childs.Add(FontBatch);

  FuelLevel := TFuelLevel.Create();

  Space := TSpace.Create(dfVec2f(Render.Width, Render.Height), Atlas.GetRegion('particle.png'), Material, 3);
  Space.Camera := Scene.Camera;

  Moon := TMoon.Create(MoonMaterial, nil, Atlas.GetRegion('fuel_level.png'), SpriteBatch);
  Moon.MaxY := Render.Height;
  Moon.LoadLevel(FileSystem.ReadResource('lander/level1.bin'));
end;

procedure TGame.PhysicsAfter(const FixedDeltaTime: Double);
begin

end;

procedure TGame.PhysicsContactBegin(var contact: Tb2Contact);
begin

end;

procedure TGame.OnFinish;
begin
  Space.Free();
  Moon.Free();
  Scene.Free();

  FuelLevel.Free();
  Material.Free();
  MoonMaterial.Free();
  Font.Free();
end;

procedure TGame.OnInput(aType: TglrInputType; aKey: TglrKey; X, Y,
  aOtherParam: Integer);
var
  i: Integer;
begin
  if (aType = itKeyUp) and (aKey = kE) then
  begin
    Moon.EditMode := not Moon.EditMode;
    if Moon.EditMode then
      fEditorText.Text := UTF8Decode('В режиме редактора')
    else
      fEditorText.Text := '';
  end;

  if Moon.EditMode then
  begin
    if (aType = itKeyUp) and (aKey = kS) then
    begin
      FileSystem.WriteResource('lander/level1.bin', Moon.SaveLevel())
      fEditorText.Text := UTF8Decode('Успешно сохранено');
    end
    else if (aType = itKeyUp) and (aKey = kL) then
    begin
      Moon.LoadLevel(FileSystem.ReadResource('lander/level1.bin'));
      fEditorText.Text := UTF8Decode('Успешно загружено');
    end;

    if (aType = itTouchDown) and (aKey = kLeftButton) then
    begin
      fMoonVertex := Moon.GetVertexIndexAtPos(Scene.Camera.WindowPosToCameraPos(Core.Input.MousePos));
    end;

    if (aType = itTouchMove) and (aKey = kLeftButton) then
    begin
      if fMoonVertex <> -1 then
      begin
        Moon.Vertices[fMoonVertex] := Scene.Camera.WindowPosToCameraPos(Core.Input.MousePos);
        Moon.VerticesPoints[fMoonVertex].Position := dfVec3f(Moon.Vertices[fMoonVertex], Moon.VerticesPoints[fMoonVertex].Position.z);
        Moon.UpdateData();
      end;
    end;

    if (aType = itTouchUp) and (aKey = kLeftButton) then
    begin
      fMoonVertex := -1;
    end;
  end;


  if aType = itWheel then
    Scene.Camera.Scale := Scene.Camera.Scale + (aOtherParam * 0.1);


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

  //Box2d.SyncObjects(b2Ship, Ship, True);

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

end.

