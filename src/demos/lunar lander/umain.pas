unit uMain;

{$mode delphi}

interface

uses
  tinyglr, glrmath, uBox2DImport, uSpace, uMoon, UPhysics2D;

const
  MAX_FUEL = 100.0;
  FUEL_PER_SEC = 3.0;
  SAFE_SPEED = 0.75;
  LAND_SPEED = 0.10;

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

  TGameStatus = (sPlay, sPause);
  TGameEndReason = (rWin, rCrash, rAway);
  TEditType = (etVertices, etLandingZones);

  { TGame }

  TGame = class (TglrGame)
  protected
    fIGDCFlag: TglrSprite;
    fMoveFlag: Boolean;

    fGameStatus: TGameStatus;
    fPoint1, fPoint2: Byte;

    fShipEngineDirection: TdfVec2f;
    fCameraScale: Single;
    fShipLinearSpeed: Single;

    fEditorText, fDebugText, fHudMainText: TglrText;
    fShipSpeedVec: TglrSprite;
    fSelectedObjectIndex: Integer;

    fTrigger1, fTrigger2: TglrSprite;
    fb2Trigger1, fb2Trigger2: Tb2Body;
    fEditType: TEditType;

    // debug purposes only
    fEmitter: TglrParticleEmitter2D;
    procedure InitParticles();
    procedure DeinitParticles();

    function GetShipDistanceToNearestMoonVertex(): Single;
    procedure PhysicsAfter(const FixedDeltaTime: Double);
    procedure PhysicsContactBegin(var contact: Tb2Contact);
    procedure PhysicsContactEnd(var contact: Tb2Contact);

    procedure OnGameEnd(aReason: TGameEndReason);
  public
    //Resources
    Atlas: TglrTextureAtlas;
    Font: TglrFont;
    FontBatch, FontBatchHud: TglrFontBatch;
    SpriteBatch: TglrSpriteBatch;
    Material, MoonMaterial: TglrMaterial;
    MoonTexture: TglrTexture;

    World: Tglrb2World;

    //Sprites
    Ship: TglrSprite;
    Flame: TglrSprite;

    FuelLevel: TFuelLevel;

    Space: TSpace;
    Moon: TMoon;

    b2Ship: Tb2Body;

    //Scenes
    Scene, SceneHud: TglrScene;

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
  landerV: array[0..5] of TdfVec2f =
    (
      (x: -0.5; y: 0.48),
      (x:    0; y: 0.35),
      (x: 0.48; y: 0.12),
      (x:    0; y:-0.35),
      (x: 0.48; y:-0.12),
      (x: -0.5; y:-0.48)
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

  MoonTexture := TglrTexture.Create(FileSystem.ReadResource('lander/moon.tga'), 'tga', True);
//  MoonTexture.SetWrapS(wClampToEdge);
  MoonTexture.SetWrapS(wRepeat);
//  MoonTexture.SetWrapR(wRepeat);
//  MoonTexture.SetWrapT(wRepeat);

  MoonMaterial := TglrMaterial.Create();
  MoonMaterial.Shader.Attach(FileSystem.ReadResource('lander/MoonShaderV.txt'), stVertex);
  MoonMaterial.Shader.Attach(FileSystem.ReadResource('lander/MoonShaderF.txt'), stFragment);
  MoonMaterial.Shader.Link();
  MoonMaterial.AddTexture(MoonTexture, 'uDiffuse');
  MoonMaterial.Color := dfVec4f(0.70, 0.70, 0.55, 1.0);

  World := Tglrb2World.Create(TVector2.From(0, 9.8 * C_COEF), false, 1 / 40, 8);
  World.AddOnBeginContact(Self.PhysicsContactBegin);
  World.AddOnEndContact(Self.PhysicsContactEnd);
  World.OnAfterSimulation := Self.PhysicsAfter;

  Ship := TglrSprite.Create();
  Ship.SetTextureRegion(Atlas.GetRegion('lander.png'));
  Ship.Position := dfVec3f(200, 100, 2);

  fTrigger1 := TglrSprite.Create();
  fTrigger1.SetTextureRegion(Atlas.GetRegion('star.png'), False);
  fTrigger1.SetSize(10, 10);
  fTrigger1.SetVerticesColor(dfVec4f(1, 0, 0, 0.5));
  fTrigger1.Parent := Ship;
  fTrigger1.Position := dfVec3f(-0.5 * Ship.Width, 0.5 * Ship.Height, 1);

  fTrigger2 := TglrSprite.Create();
  fTrigger2.SetTextureRegion(Atlas.GetRegion('star.png'), False);
  fTrigger2.SetSize(10, 10);
  fTrigger2.SetVerticesColor(dfVec4f(1, 0, 0, 0.5));
  fTrigger2.Parent := Ship;
  fTrigger2.Position := dfVec3f(-0.5 * Ship.Width, - 0.5 * Ship.Height, 1);

  fb2Trigger1 := Box2D.BoxSensor(World, dfVec2f(fTrigger1.Position), dfVec2f(fTrigger1.Width * 1.2, fTrigger1.Height), 0, $0002, $0001, False);
  fb2Trigger1.UserData := @fPoint1;
  fb2Trigger2 := Box2D.BoxSensor(World, dfVec2f(fTrigger2.Position), dfVec2f(fTrigger2.Width * 1.2, fTrigger2.Height), 0, $0002, $0001, False);
  fb2Trigger2.UserData := @fPoint2;

  for i := 0 to Length(landerV) - 1 do
    landerV[i] *= Ship.Width;

  b2Ship := Box2d.Polygon(World, dfVec2f(Ship.Position), landerV, 0.5, 1.0, 0.0, $0002, $0001, 1);
  b2Ship.AngularDamping := 1.0;
  b2Ship.UserData := Ship;

  Flame := TglrSprite.Create();
  Flame.SetTextureRegion(Atlas.GetRegion('flame.png'));
  Flame.Position := dfVec3f(-50, 0, -1);
  Flame.SetVerticesColor(dfVec4f(0.9, 0.85, 0.1, 1.0));
  Flame.Parent := Ship;

  fShipSpeedVec := TglrSprite.Create();
  fShipSpeedVec.SetTextureRegion(Atlas.GetRegion('arrow.png'));

  SpriteBatch := TglrSpriteBatch.Create();
  SpriteBatch.Material := Material;
  SpriteBatch.Childs.Add(Flame);
  SpriteBatch.Childs.Add(Ship);
  SpriteBatch.Childs.Add(fTrigger1);
  SpriteBatch.Childs.Add(fTrigger2);
  SpriteBatch.Childs.Add(fShipSpeedVec);

  //Text for editor purposes
  fEditorText := TglrText.Create();
  fEditorText.Position := dfVec3f(Render.Width / 2 - 150, 20, 5);
  fEditorText.LetterSpacing := 1.2;

  //Text at spaceship' right
  fDebugText := TglrText.Create();
  fDebugText.Position.z := 10;
  fDebugText.LetterSpacing := 1.2;

  //Text for win/lose
  fHudMainText := TglrText.Create();
  fHudMainText.Position := dfVec3f(50, 50, 15);
  fHudMainText.LetterSpacing := 1.2;
  fHudMainText.LineSpacing := 1.5;

  FontBatch := TglrFontBatch.Create(Font);
  FontBatch.Childs.Add(fDebugText);

  FontBatchHud := TglrFontBatch.Create(Font);
  FontBatchHud.Childs.Add(fHudMainText);
  FontBatchHud.Childs.Add(fEditorText);

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

  SceneHud := TglrScene.Create();
  SceneHud.Camera.ProjectionMode := pmOrtho;
  SceneHud.Camera.ProjectionModePivot := pTopLeft;
  SceneHud.Camera.SetCamera(
    dfVec3f(0, 0, 100),
    dfVec3f(0, 0, 0),
    dfVec3f(0, 1, 0));
  SceneHud.Camera.Viewport(0, 0, Render.Width, Render.Height, 90, -1, 200);
  SceneHud.Root.Childs.Add(FontBatchHud);

  FuelLevel := TFuelLevel.Create();

  Space := TSpace.Create(dfVec2f(-1.5 * Render.Width, -1.5 * Render.Height),
    dfVec2f(3 * Render.Width, 3 * Render.Height),
    Atlas.GetRegion('star.png'), Material, 3);
  Space.Camera := Scene.Camera;

  Moon := TMoon.Create(MoonMaterial, Atlas.GetRegion('blank.png'), SpriteBatch, FontBatch);
  Moon.MaxY := Render.Height * 2;
  Moon.LoadLevel(FileSystem.ReadResource('lander/level1.bin'));

  fIGDCFlag := TglrSprite.Create(1, 1, dfVec2f(0, 1));
  fIGDCFlag.SetTextureRegion(Atlas.GetRegion('flag.png'));
  fIGDCFlag.Visible := False;
  SpriteBatch.Childs.Add(fIGDCFlag);

  fGameStatus := sPlay;
  fPoint1 := 0;
  fPoint2 := 0;
  fMoveFlag := False;

  InitParticles();
end;

procedure TGame.InitParticles;
var
  i: TglrIntKeyFrame;
  s: TglrSingleKeyFrame;
  v: TglrVec2KeyFrame;
  v2: TglrVec22KeyFrame;
  c: TglrVec4KeyFrame;
begin
  fEmitter := TglrParticleEmitter2D.Create();
  fEmitter.EmitterShapeType := peCircle;

  i.t := 0;
  i.value := 50;
  fEmitter.ParticlesCount.Add(i);

  v.t := 0;
  v.value := dfVec2f(2, 5);
  fEmitter.LifetimeMinMax.Add(v);

  v2.t := 0;
  v2.v1 := dfVec2f(15, 15);
  v2.v2 := dfVec2f(25, 25);
  fEmitter.Size.Add(v2);

  c.t := 0;
  c.value := dfVec4f(1, 0.5, 0.5, 1.0);
  fEmitter.Color.Add(c);
  c.t := 1.0;
  c.value := dfVec4f(1, 0.5, 0.5, 0.0);
  fEmitter.Color.Add(c);

  v.t := 0;
  v.value := dfVec2f(0, 5);
  fEmitter.Velocity.Add(v);

  s.t := 0;
  s.value := 20;
  fEmitter.VelocityDispersionAngle.Add(s);
end;

procedure TGame.DeinitParticles;
begin
  fEmitter.Free();
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
var
  b1, b2: Tb2Body;
  //vel1, vel2, imp: TVector2;
  //worldManifold: Tb2WorldManifold;
begin
  b1 := contact.m_fixtureA.GetBody;
  b2 := contact.m_fixtureB.GetBody;
  //contact.GetWorldManifold(worldManifold);


  if ((b1.UserData = Ship) and (b2.UserData = Moon))
    or ((b2.UserData = Ship) and (b1.UserData = Moon)) then
  begin
    //vel1 := b1.GetLinearVelocityFromWorldPoint(worldManifold.points[0]);
    //vel2 := b2.GetLinearVelocityFromWorldPoint(worldManifold.points[0]);
    //imp := vel1 - vel2;

    if {imp.Length} fShipLinearSpeed > SAFE_SPEED then
    begin
      OnGameEnd(rCrash);
    end
    else if fShipLinearSpeed > LAND_SPEED then
    begin
      fHudMainText.Text := UTF8Decode('Осторожно!');
      fHudMainText.Color := dfVec4f(1, 1, 0.1, 1);
    end
    else
    begin
//      OnGameEnd(rWin);
    end;
  end

  else if ( (contact.m_fixtureA.IsSensor) and (b2.UserData = Moon) ) then
  begin
    Byte(b1.UserData^) += 1;
  end
  else if ( (contact.m_fixtureB.IsSensor) and (b1.UserData = Moon) ) then
  begin
    Byte(b2.UserData^) += 1;
  end;

end;

procedure TGame.PhysicsContactEnd(var contact: Tb2Contact);
var
  b1, b2: Tb2Body;
begin
  b1 := contact.m_fixtureA.GetBody;
  b2 := contact.m_fixtureB.GetBody;

  if ( (contact.m_fixtureA.IsSensor) and (b2.UserData = Moon) ) then
  begin
    Byte(b1.UserData^) -= 1;
  end
  else if ( (contact.m_fixtureB.IsSensor) and (b1.UserData = Moon) ) then
  begin
    Byte(b2.UserData^) -= 1;
  end;
end;

procedure TGame.OnGameEnd(aReason: TGameEndReason);
var
  scores: Integer;
  i1, i2: Integer;
begin
  fGameStatus := sPause;
  if aReason = rWin then
  begin
    scores := Ceil((FuelLevel.Level / MAX_FUEL) * 1000);
    i1 := Moon.GetLandingZoneAtPos(dfVec2f(fTrigger1.AbsoluteMatrix.Pos));
    i2 := Moon.GetLandingZoneAtPos(dfVec2f(fTrigger2.AbsoluteMatrix.Pos));
    if (i1 = i2) and (i1 <> -1) then
      scores *= Moon.LandingZones[i1].Multiply;
    fHudMainText.Text := UTF8Decode('Очки: ' + Convert.ToString(scores)
    + #13#10#13#10'Вы успешно прилунились!'
    + #13#10'Пора искать селенитов!'
    + #13#10'Но, может еще разок?'
    + #13#10#13#10'Enter — рестарт');
    fHudMainText.Color := dfVec4f(0.1, 1, 0.1, 1);

    fMoveFlag := True;
    fIGDCFlag.Position := Ship.Position + dfVec3f(0, 40, -1);
    fIGDCFlag.Visible := True;
    FuelLevel.sBack.Visible := False;
    FuelLevel.sLevel.Visible := False;
    fDebugText.Visible := False;
    fShipSpeedVec.Visible := False;
  end
  else if aReason = rCrash then
  begin
    fHudMainText.Text := UTF8Decode('Потрачено!'
    + #13#10#13#10'Аппарат разбит, вы погибли!'
    + #13#10'ЦУП воет и не знает, кого винить!'
    + #13#10'А пока там неразбериха — давайте еще раз...'
    + #13#10#13#10'Enter — рестарт');
    fHudMainText.Color := dfVec4f(1.0, 0.1, 0.1, 1);
    fDebugText.Visible := False;
    fShipSpeedVec.Visible := False;
  end
  else if aReason = rAway then
  begin
    fHudMainText.Text := UTF8Decode('Вы покинули зону посадки!'
    + #13#10#13#10'ЦУП негодует, вы разжалованы, осуждены и'
    + #13#10'будете уничтожены с помощью Большого Боевого Лазера!'
    + #13#10'Пожалуйста, оставайтесь на месте!'
    + #13#10#13#10'Enter — рестарт');
    fHudMainText.Color := dfVec4f(1.0, 0.1, 0.1, 1);
  end;
end;

procedure TGame.OnFinish;
begin
  DeinitParticles();

  Space.Free();
  Moon.Free();
  Scene.Free();
  SceneHud.Free();

  FuelLevel.Free();
  Material.Free();
  MoonMaterial.Free();
  Font.Free();
  World.Free();
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
    begin
      fEditorText.Text := UTF8Decode('В режиме редактора'#13#10'Режим: Поверхность');
      fEditType := etVertices;
      fSelectedObjectIndex := -1;
    end
    else
      fEditorText.Text := '';
  end;

  if not Moon.EditMode then
  begin

    if (aType = itKeyDown) and (aKey = kEscape) then
      Core.Quit();

    if fGameStatus = sPause then
    begin
      if (aType = itKeyDown) and (aKey = kReturn) then
      begin
        Game.OnFinish();
        Game.OnStart();
      end;
    end;

  end
  //Editor mode
  else
  begin
    if aType = itKeyUp then
      case aKey of
        kT: begin
              if fEditType = etVertices then
              begin
                fEditType := etLandingZones;
                fEditorText.Text := UTF8Decode('В режиме редактора'#13#10'Режим: Зоны посадки');
              end
              else if fEditType = etLandingZones then
              begin
                fEditType := etVertices;
                fEditorText.Text := UTF8Decode('В режиме редактора'#13#10'Режим: Поверхность');
              end;
            end;

        kS: begin
              FileSystem.WriteResource('lander/level1.bin', Moon.SaveLevel());
              fEditorText.Text := UTF8Decode('Успешно сохранено');
            end;
        kL: begin
              Moon.LoadLevel(FileSystem.ReadResource('lander/level1.bin'));
              fEditorText.Text := UTF8Decode('Успешно загружено');
            end;

        k2, k3, k4, k5:
          if (fEditType = etLandingZones) and (fSelectedObjectIndex <> -1) then
          begin
            Moon.LandingZones[fSelectedObjectIndex].Multiply := Ord(aKey) - Ord(k0);
            Moon.LandingZones[fSelectedObjectIndex].Update();
          end;

      end;

    absMousePos := Scene.Camera.WindowPosToCameraPos(Core.Input.MousePos);

    if (aType = itTouchDown) and (aKey = kLeftButton) then
      case fEditType of
        etVertices:     fSelectedObjectIndex := Moon.GetVertexIndexAtPos(absMousePos);
        etLandingZones: fSelectedObjectIndex := Moon.GetLandingZoneAtPos(absMousePos);
      end;


    if (aType = itTouchDown) and (aKey = kRightButton) then
      if fEditType = etVertices then
      begin
        i := Moon.GetVertexIndexAtPos(absMousePos);

        if i = -1 then
        begin
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
          Moon.DeleteVertex(i);

        Moon.UpdateData();
      end

      else if fEditType = etLandingZones then
      begin
        i := Moon.GetLandingZoneAtPos(absMousePos);
        if i = -1 then
          Moon.AddLandingZone(absMousePos, dfVec2f(100, 50), 2)
        else
          Moon.DeleteLandingZone(i);
      end;

    if (aType = itTouchMove) and (aKey = kLeftButton) then
      if fSelectedObjectIndex <> -1 then
        if fEditType = etVertices then
        begin
          Moon.Vertices[fSelectedObjectIndex] := absMousePos;
          Moon.VerticesPoints[fSelectedObjectIndex].Position :=
            dfVec3f(Moon.Vertices[fSelectedObjectIndex], Moon.VerticesPoints[fSelectedObjectIndex].Position.z);
          Moon.UpdateData();
        end
        else if fEditType = etLandingZones then
        begin
          Moon.LandingZones[fSelectedObjectIndex].Pos := absMousePos;
          Moon.LandingZones[fSelectedObjectIndex].Update();
        end;


    if (aType = itTouchUp) and (aKey = kLeftButton) then
      fSelectedObjectIndex := -1;

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
  Material.Bind();
  fEmitter.RenderSelf();
  SceneHud.RenderScene();
end;

procedure TGame.OnResize(aNewWidth, aNewHeight: Integer);
begin

end;

procedure TGame.OnResume;
begin

end;

procedure TGame.OnUpdate(const dt: Double);
begin
  if not Moon.EditMode then
  begin

    if fGameStatus = sPlay then
    begin
      if Core.Input.KeyDown[kLeft] then
        b2Ship.ApplyAngularImpulse(- Core.DeltaTime * 3)
      else if Core.Input.KeyDown[kRight] then
        b2Ship.ApplyAngularImpulse(  Core.DeltaTime * 3);

      Flame.Visible := (Core.Input.KeyDown[kUp] or Core.Input.KeyDown[kDown] or Core.Input.KeyDown[kSpace])
        and (FuelLevel.Level > 0);
      if Flame.Visible then
      begin
        FuelLevel.Level -= dt * FUEL_PER_SEC;

        fShipEngineDirection := dfVec2f(Ship.Rotation) * dt;
        b2Ship.ApplyLinearImpulse(TVector2.From(fShipEngineDirection.x, fShipEngineDirection.y), b2Ship.GetWorldCenter);
      end;

      Box2d.SyncObjects(b2Ship, Ship);
      World.Update(dt);

      fCameraScale := 1.0; // * (1 / Clamp(b2Ship.GetLinearVelocity.SqrLength, 1.0, 1.85)); //no fShipLinearSpeed clamp
      fCameraScale *=  (1 / Clamp(GetShipDistanceToNearestMoonVertex() * 0.01, 0.9, 2.0));
      Scene.Camera.Scale := Lerp(Scene.Camera.Scale, fCameraScale, 5 * dt);
      with Scene.Camera do
        Position := dfVec3f(dfVec2f(Ship.Position), Position.z);
  //        Position := dfVec3f(dfVec2f(Position.Lerp(Ship.Position, 5 * dt)), Position.z);
      FuelLevel.Update(dt);

      fShipLinearSpeed := b2Ship.GetLinearVelocity.Length;
      if fShipLinearSpeed > SAFE_SPEED then
        fDebugText.Color := dfVec4f(1, 0.3, 0.3, 1.0)
      else
        fDebugText.Color := dfVec4f(0.3, 1.0, 0.3, 1.0);
      fDebugText.Position := Ship.Position + dfVec3f(60, -10, 10);
      fDebugText.Text := Convert.ToString(fShipLinearSpeed, 2);

      fShipSpeedVec.Position := fDebugText.Position + dfVec3f(20, -20, 0);
      fShipSpeedVec.SetVerticesColor(fDebugText.Color);
      fShipSpeedVec.Rotation := Box2d.ConvertB2ToGL(b2Ship.GetLinearVelocity).GetRotationAngle();

      if (Ship.Position.x > Moon.Vertices[High(Moon.Vertices)].x) or
         (Ship.Position.x < Moon.Vertices[0].x) then
        OnGameEnd(rAway);

      Box2d.ReverseSyncObjects(fTrigger1, fb2Trigger1);
      Box2d.ReverseSyncObjects(fTrigger2, fb2Trigger2);

      if fPoint1 > 0 then
        fTrigger1.SetVerticesColor(dfVec4f(0, 1, 0, 0.5))
      else
        fTrigger1.SetVerticesColor(dfVec4f(1, 0, 0, 0.5));

      if fPoint2 > 0 then
        fTrigger2.SetVerticesColor(dfVec4f(0, 1, 0, 0.5))
      else
        fTrigger2.SetVerticesColor(dfVec4f(1, 0, 0, 0.5));
      if (fShipLinearSpeed < LAND_SPEED) and (fPoint1 > 0) and (fPoint2 > 0) and not Flame.Visible then
        OnGameEnd(rWin);

      //fEmitter.Update(dt);
    end
    else if fGameStatus = sPause then
    begin
      if fMoveFlag then
      begin
        fIGDCFlag.Position.y -= dt * 40;
        if (Ship.Position.y - fIGDCFlag.Position.y) > Ship.Height / 2 - 15 then
          fMoveFlag := False;
      end;
    end;

  end
  else
  begin
    if fSelectedObjectIndex = -1 then
    begin
      if Core.Input.KeyDown[kUp] then
        Scene.Camera.Translate(-dt * 200, 0, 0)
      else if Core.Input.KeyDown[kDown] then
        Scene.Camera.Translate(dt * 200, 0, 0);

      if Core.Input.KeyDown[kLeft] then
        Scene.Camera.Translate(0, -dt * 200, 0)
      else if Core.Input.KeyDown[kRight] then
        Scene.Camera.Translate(0, dt * 200, 0);
    end
    else if (fEditType = etLandingZones) then
    begin
      if Core.Input.KeyDown[kUp] then
        Moon.LandingZones[fSelectedObjectIndex].Size.y += dt * 30
      else if Core.Input.KeyDown[kDown] then
        Moon.LandingZones[fSelectedObjectIndex].Size.y -= dt * 30;

      if Core.Input.KeyDown[kLeft] then
        Moon.LandingZones[fSelectedObjectIndex].Size.x += dt * 30
      else if Core.Input.KeyDown[kRight] then
        Moon.LandingZones[fSelectedObjectIndex].Size.x -= dt * 30;

      Moon.LandingZones[fSelectedObjectIndex].Update();
    end;
  end;
end;

end.

