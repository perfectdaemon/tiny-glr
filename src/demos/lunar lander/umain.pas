unit uMain;

{$mode delphi}

interface

uses
  tinyglr, glrmath, uBox2DImport, uSpace, uMoon, UPhysics2D;

const
  MAX_FUEL = 100.0;
  FUEL_PER_SEC = 3.0;
  SAFE_SPEED = 0.35;

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
    fEditorText, fDebugText: TglrText;
    fMoonVertex: Integer;
    function GetShipDistanceToNearestMoonVertex(): Single;
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

var
  landerV: array[0..3] of TdfVec2f =
    (
      (x: -0.5; y:  0.5),
      (x:  0.5; y:  0.22),
      (x:  0.5; y: -0.22),
      (x: -0.5; y: -0.5)
    );
  i: Integer;
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

  for i := 0 to 3 do
    landerV[i] *= Ship.Width;

  //b2Ship := Box2d.{Circle}Polygon(World, dfVec2f(Ship.Position), landerV, 0.5, 0.2, 0.0, $0002, $0001, 1);
  b2Ship := Box2d.Polygon(World, dfVec2f(Ship.Position), landerV, 0.5, 1.0, 0.0, $0002, $0001, 1);
  b2Ship.AngularDamping := 1.0;

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

  fDebugText := TglrText.Create();
  fDebugText.Position.z := 10;
  fDebugText.LetterSpacing := 1.2;

  FontBatch := TglrFontBatch.Create(Font);
  FontBatch.Childs.Add(fEditorText);
  FontBatch.Childs.Add(fDebugText);

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
  Moon.MaxY := Render.Height * 3;
  Moon.LoadLevel(FileSystem.ReadResource('lander/level1.bin'));
end;

function TGame.GetShipDistanceToNearestMoonVertex: Single;
var
  i: Integer;
  s: Single;
begin
  Result := 1 / 0;
  for i := 0 to Length(Moon.Vertices) - 1 do
  begin
    s := (dfVec2f(Ship.Position) - Moon.Vertices[i]).Length;
    if s < Result then
      Result := s;
  end;
  Result -= (Ship.Width / 2);
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
  absMousePos: TdfVec2f;
  vertexAdded: Boolean;
begin
  if (aType = itKeyUp) and (aKey = kE) then
  begin
    Moon.EditMode := not Moon.EditMode;
    if Moon.EditMode then
      fEditorText.Text := UTF8Decode('В режиме редактора')
    else
      fEditorText.Text := '';
  end;

  if not Moon.EditMode then
  begin

  end
  //Editor mode
  else
  begin
    if (aType = itKeyUp) and (aKey = kS) then
    begin
      FileSystem.WriteResource('lander/level1.bin', Moon.SaveLevel());
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

    if (aType = itTouchDown) and (aKey = kRightButton) then
    begin
      i := Moon.GetVertexIndexAtPos(Scene.Camera.WindowPosToCameraPos(Core.Input.MousePos));
      if i = -1 then
      begin
        absMousePos := Scene.Camera.WindowPosToCameraPos(Core.Input.MousePos);
        vertexAdded := False;
        for i := 0 to Length(Moon.Vertices) - 1 do
          if Moon.Vertices[i].x > absMousePos.x then
          begin
            Moon.AddVertex(absMousePos, i);
            vertexAdded := True;
            break;
          end;
        if not vertexAdded then
          Moon.AddVertex(absMousePos);
      end
      else
      //delete vertex
      begin
        Moon.DeleteVertex(i);
      end;
      Moon.UpdateData();
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

    if (aType = itTouchMove) and (aKey = kMiddleButton) then
    begin
      Ship.Position := dfVec3f(Scene.Camera.WindowPosToCameraPos(Core.Input.MousePos), Ship.Position.z)
    end;

    if (aType = itTouchUp) and (aKey = kLeftButton) then
    begin
      fMoonVertex := -1;
    end;

    if aType = itWheel then
      Scene.Camera.Scale := Scene.Camera.Scale + (aOtherParam * 0.1);

  end;
end;

procedure TGame.OnPause;
begin

end;

procedure TGame.OnRender;
begin
  Scene.Camera.Update();
  Space.RenderSelf();
  Moon.RenderSelf();
  //Render.Params.ModelViewProj := Render.Params.ViewProj;
  Scene.RenderScene();

end;

procedure TGame.OnResize(aNewWidth, aNewHeight: Integer);
begin

end;

procedure TGame.OnResume;
begin

end;

procedure TGame.OnUpdate(const dt: Double);
var
  v: TdfVec2f;
  scale: Single;
  speed: Single;
begin
  if not Moon.EditMode then
  begin
    if Core.Input.KeyDown[kLeft] then
      b2Ship.ApplyAngularImpulse(- Core.DeltaTime * 5)
    else if Core.Input.KeyDown[kRight] then
      b2Ship.ApplyAngularImpulse(  Core.DeltaTime * 5);

    Flame.Visible := (Core.Input.KeyDown[kUp] or Core.Input.KeyDown[kDown] or Core.Input.KeyDown[kSpace])
      and (FuelLevel.Level > 0);
    if Flame.Visible then
    begin
      FuelLevel.Level -= dt * FUEL_PER_SEC;

      v := dfVec2f(Ship.Rotation) * dt;
      b2Ship.ApplyLinearImpulse(TVector2.From(v.x, v.y), b2Ship.GetWorldCenter);
    end;

    Box2d.SyncObjects(b2Ship, Ship);
    World.Update(dt);

    //fEditorText.Text := Convert.ToString(GetShipDistanceToNearestMoonVertex());
    scale := 1.0; // * (1 / Clamp(b2Ship.GetLinearVelocity.SqrLength, 1.0, 1.85)); //no speed clamp
    scale *=  (1 / Clamp(GetShipDistanceToNearestMoonVertex() * 0.01, 0.9, 2.0));
    Scene.Camera.Scale := Lerp(Scene.Camera.Scale, scale, 5 * dt);
    with Scene.Camera do
      Position := dfVec3f(dfVec2f(Ship.Position), Position.z);
//        Position := dfVec3f(dfVec2f(Position.Lerp(Ship.Position, 5 * dt)), Position.z);
    FuelLevel.Update(dt);

    speed := b2Ship.GetLinearVelocity.Length;
    if speed > SAFE_SPEED then
      fDebugText.Color := dfVec4f(1, 0.3, 0.3, 1.0)
    else
      fDebugText.Color := dfVec4f(0.3, 1.0, 0.3, 1.0);
    fDebugText.Position := Ship.Position + dfVec3f(60, -10, 10);
    fDebugText.Text := Convert.ToString(speed, 2);
  end
  else
  begin
    if Core.Input.KeyDown[kUp] then
      Scene.Camera.Translate(-dt * 200, 0, 0)
    else if Core.Input.KeyDown[kDown] then
      Scene.Camera.Translate(dt * 200, 0, 0);

    if Core.Input.KeyDown[kLeft] then
      Scene.Camera.Translate(0, -dt * 200, 0)
    else if Core.Input.KeyDown[kRight] then
      Scene.Camera.Translate(0, dt * 200, 0);
  end;
end;

end.

