unit uGame;

interface

uses
  tinyglr, glrMath;

type

  TBulletType = (bSimple);

  TBulletOwner = (bPlayer, bEnemy);

  { TBullet }

  TBullet = class (TglrSprite)
    T, LifeTime: Single;
    Velocity: TdfVec2f;
    BType: TBulletType;
    Owner: TBulletOwner;
    procedure Reset();
  end;

  TBullets = TglrObjectList<TBullet>;

  { TBulletManager }

  TBulletManager = class
  private
    fBullets: TBullets;
    fBatch: TglrSpriteBatch;
    fMaterial: TglrMaterial;
  public
    constructor Create(aBatch: TglrSpriteBatch;
      aMaterial: TglrMaterial);
    destructor Destroy();

    function GetNewBullet(): TBullet;

    procedure Update(const dt: Double);
    procedure RenderSelf();
  end;

  { TPlayer }

  TPlayer = class
  private
    WeaponDir: TdfVec2f;
    fT: Single; // for shooting
  public
    FireThreshold, RotateSpeed, DirectSpeed: Single;
    MainWeaponVelocity, MainWeaponDispersion: Single;
    Hero, Weapon: TglrSprite;

    constructor Create();
    destructor Destroy();

    procedure Update(const dt: Double; axisX, axisY: Integer);

    procedure Fire();
  end;

  { TEnemy }

  TEnemy = class (TglrSprite)
  public
    Health: Single;

    procedure Reset();
    procedure Update(const dt: Double);
  end;

  TEnemies = TglrObjectList<TEnemy>;

  { TEnemyManager }

  TEnemyManager = class
  public
    Enemies: TEnemies;
    constructor Create();
    destructor Destroy();

    procedure Update(const dt: Double);
  end;

  { TGame }

  TGame = class (TglrGame)
  private
    Pause: Boolean;

    SpriteBatch: TglrSpriteBatch;
    FontBatch: TglrFontBatch;

    Font: TglrFont;

    MainMaterial: TglrMaterial;
    ParticleEmitter: TglrCustomParticleEmitter2D;
    BulletManager: TBulletManager;
    EnemyManager: TEnemyManager;

    DebugText: TglrText;
    SceneHud: TglrScene;

    Player: TPlayer;
    procedure InitEnemies();
    procedure ParticleBoom(aPos: TdfVec2f);
    procedure ParticleBigBoom(aPos: TdfVec2f);
    procedure ParticleUpdate(const dt: Double);
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

function PointToSpriteIntersect(aPoint: TdfVec2f; aSprite: TglrSprite): Boolean;

var
  Game: TGame;

implementation

uses
  uFMOD, music;

function PointToSpriteIntersect(aPoint: TdfVec2f; aSprite: TglrSprite): Boolean;
var
  aRad: Single;
  i: Integer;
  p1, p2: TdfVec2f;
begin
  // Get maximum possible bounding sphere
  aRad := sqr(aSprite.Width) + sqr(aSprite.Height);
  if ((dfVec2f(aSprite.Position) - aPoint).LengthQ > aRad) then
    Exit(False);

  // Sum all of angles between sprite vertices and point
  aRad := 0; // Now it will be used for sum
  for i := 0 to 3 do
  begin
    p1 := dfVec2f(aSprite.AbsoluteMatrix * aSprite.Vertices[i].vec) - aPoint;
    p2 := dfVec2f(aSprite.AbsoluteMatrix * aSprite.Vertices[(i + 1) mod 4].vec) - aPoint;
    p1.Normalize();
    p2.Normalize();
    aRad += p1.Dot(p2);
  end;
  Exit(Abs(aRad) < 0.3);
end;

{ TEnemyManager }

constructor TEnemyManager.Create;
begin
  inherited;
  Enemies := TEnemies.Create(40);
end;

destructor TEnemyManager.Destroy;
begin
  Enemies.Free(True);
  inherited;
end;

procedure TEnemyManager.Update(const dt: Double);
var
  i: Integer;
begin
  for i := 0 to Enemies.Count - 1 do
    Enemies[i].Update(dt);
end;

{ TEnemy }


procedure TEnemy.Reset;
begin
  Health := 20;
end;

procedure TEnemy.Update(const dt: Double);
begin

end;

{ TBulletManager }

constructor TBulletManager.Create(aBatch: TglrSpriteBatch; aMaterial: TglrMaterial);
var
  i: Integer;
  b: TBullet;
begin
  inherited Create;
  fBatch := aBatch;
  fMaterial := aMaterial;
  fBullets := TBullets.Create(128);
  for i := 0 to 128 do
  begin
    b := TBullet.Create(6, 2, dfVec2f(0.5, 0.5));
    b.Reset();
    b.Visible := False;
    fBullets.Add(b);
  end;
end;

destructor TBulletManager.Destroy;
begin
  fBullets.Free(True);
  inherited;
end;

function TBulletManager.GetNewBullet: TBullet;
var
  i: Integer;
  b: TBullet;
begin
  for i := 0 to fBullets.Count - 1 do
    if not fBullets[i].Visible then
    begin
      fBullets[i].Reset();
      fBullets[i].SetVerticesColor(dfVec4f(1, 1, 1, 1));
      Exit(fBullets[i]);
    end;

  b := TBullet.Create(6, 2, dfVec2f(0.5, 0.5));
  b.Reset();
  fBullets.Add(b);
  Exit(b);
end;

procedure TBulletManager.Update(const dt: Double);
var
  i, j: Integer;
  e: TEnemy;
begin
  for i := 0 to fBullets.Count - 1 do
    if (fBullets[i].Visible) then
      with fBullets[i] do
      begin
        T += dt;
        if (T > LifeTime) then
          Visible := False
        else
        begin
          Position += dfVec3f(Velocity * dt, 0);

          if (Owner = bEnemy) then
          begin
            if (PointToSpriteIntersect(dfVec2f(Position), Game.Player.Hero)) then
            begin
              Game.DebugText.Text := 'Player hit!';
              Visible := False;
            end;
          end
          else if (Owner = bPlayer) then

            for j := 0 to Game.EnemyManager.Enemies.Count - 1 do
            begin
              e := Game.EnemyManager.Enemies[j];
              if (e.Visible) and (PointToSpriteIntersect(dfVec2f(Position), e)) then
              begin
                Game.ParticleBoom(dfVec2f(Position));
                Visible := False;

                case (BType) of
                  bSimple: e.Health -= 1;
                end;

                if e.Health <= 0 then
                begin
                  e.Visible := False;
                  Game.ParticleBigBoom(dfVec2f(e.Position));
                end;

              end;
            end;


        end;
      end;
end;

procedure TBulletManager.RenderSelf;
var
  i: Integer;
begin
  fMaterial.Bind();
  fBatch.Start();
  for i := 0 to fBullets.Count - 1 do
    fBatch.Draw(fBullets[i]);
  fBatch.Finish();
end;

{ TBullet }

procedure TBullet.Reset;
begin
  T := 0;
  LifeTime := 3;
  Velocity := dfVec2f(0, 0);
  Visible := True;
end;

{ TPlayer }

constructor TPlayer.Create;
begin
  inherited;
  Hero := TglrSprite.Create(45, 25, dfVec2f(0.5, 0.5));
  Hero.Position := dfVec3f(Render.Width / 2, Render.Height / 2, 1);
  Hero.SetVerticesColor(dfVec4f(0.5, 0.7, 0.5, 1.0));

  Weapon := TglrSprite.Create(25, 8, dfVec2f(0.0, 0.5));
  Weapon.SetVerticesColor(dfVec4f(0.3, 0.6, 0.3, 1.0));
  Weapon.Position := Hero.Position + dfVec3f(0, 0, 1);

  RotateSpeed := 130;
  DirectSpeed := 150;
  FireThreshold := 0.1;

  MainWeaponVelocity := 450;
  MainWeaponDispersion := 0.1;

  fT := 0.1;
end;

destructor TPlayer.Destroy;
begin
  Weapon.Free();
  Hero.Free();
  inherited;
end;

procedure TPlayer.Update(const dt: Double; axisX, axisY: Integer);
begin
  Hero.Rotation := Hero.Rotation + (RotateSpeed * dt * axisY);
  Hero.Position += dfVec3f(DirectSpeed * dt * axisX * dfVec2f(Hero.Rotation), 0);

  WeaponDir := (Core.Input.MousePos - dfVec2f(hero.Position)).Normal;
  Weapon.Rotation := WeaponDir.GetRotationAngle();

  Weapon.Position := Hero.Position + dfVec3f(0, 0, 1);

  if (fT > 0) then
    fT -= dt;
end;

procedure TPlayer.Fire;
var
  b: TBullet;
  bulletDir: TdfVec2f;
begin
  if (fT <= 0) then
  begin
    fT := FireThreshold;
    b := Game.BulletManager.GetNewBullet();
    b.BType := bSimple;
    b.Owner := bPlayer;

    bulletDir := WeaponDir +
      dfVec2f(- WeaponDir.y, WeaponDir.x) * MainWeaponDispersion * (0.5 - Random());

    b.Velocity := bulletDir * MainWeaponVelocity;
    b.Rotation := b.Velocity.GetRotationAngle();
    b.Position := Weapon.Position + dfVec3f(WeaponDir * 20, 0);
  end;
end;

{ TGame }

procedure TGame.OnStart;
begin
  // Write here initialization code
  Render.SetClearColor(43 / 255, 99 / 255, 147 / 255);
  Font := TglrFont.Create(FileSystem.ReadResourceLZO('shooter/Hattori Hanzo17b_.bmp', False));

  MainMaterial := TglrMaterial.Create();
  MainMaterial.Shader.Free();
  MainMaterial.Shader := Default.SpriteShader;
  MainMaterial.AddTexture(Default.BlankTexture, 'uDiffuse');

  SpriteBatch := TglrSpriteBatch.Create();
  FontBatch := TglrFontBatch.Create(Font);

  ParticleEmitter := TglrCustomParticleEmitter2D.Create(SpriteBatch, MainMaterial);
  ParticleEmitter.OnUpdate := ParticleUpdate;

  BulletManager := TBulletManager.Create(SpriteBatch, MainMaterial);

  DebugText := TglrText.Create(UTF8Decode('Тест'));
  DebugText.Position := dfVec3f(100, 100, 1);

  SceneHud := TglrScene.Create();
  SceneHud.Camera.ProjectionMode := pmOrtho;
  SceneHud.Camera.ProjectionModePivot := pTopLeft;
  SceneHud.Camera.SetCamera(
    dfVec3f(0, 0, 100),
    dfVec3f(0, 0, 0),
    dfVec3f(0, 1, 0));
  SceneHud.Camera.Viewport(0, 0, Render.Width, Render.Height, 90, -1, 200);

  Player := TPlayer.Create();

  EnemyManager := TEnemyManager.Create();
  InitEnemies();

  uFMOD_PlaySong(@xm1, Length(xm1), XM_MEMORY);

  Pause := False;
end;

procedure TGame.InitEnemies;
var
  e: TEnemy;
begin
  e := TEnemy.Create(50, 50, dfVec2f(0.5, 0.5));
  e.Reset();
  e.Position := dfVec3f(100, 400, 2);
  e.Rotation := 40;
  EnemyManager.Enemies.Add(e);
end;

procedure TGame.ParticleBoom(aPos: TdfVec2f);
var
  i: Integer;
begin
  for i := 0 to 19 do
    with ParticleEmitter.GetNewParticle() do
    begin
      Position := dfVec3f(aPos, 5);
      Position.z += 1;
      Position += dfVec3f(10 - Random(20), 10 - Random(20), Random(3));
      Rotation := Random(180);
      //SetVerticesColor(dfVec4f(Random(), Random(), Random(), 1.0));
      SetVerticesColor(dfVec4f(1.0, 0.5 * Random(), 0.3 * Random(), 1.0));
      Width := 1 + 5 * Random();
      Height := Width;
      LifeTime := 0.5;
      Velocity := dfVec2f(Random(360)) * (90 + Random(40));
    end;
end;

procedure TGame.ParticleBigBoom(aPos: TdfVec2f);
var
  i: Integer;
begin
  for i := 0 to 35 do
    with ParticleEmitter.GetNewParticle() do
    begin
      Position := dfVec3f(aPos, 5);
      Position.z += 1;
      Position += dfVec3f(10 - Random(20), 10 - Random(20), Random(3));
      Rotation := Random(180);
      //SetVerticesColor(dfVec4f(Random(), Random(), Random(), 1.0));
      SetVerticesColor(dfVec4f(1.0, 0.5 * Random(), 0.3 * Random(), 1.0));
      Width := 15 + 10 * Random();
      Height := Width;
      LifeTime := 0.7;
      Velocity := dfVec2f(Random(360)) * (90 + Random(40));
    end;
end;

procedure TGame.ParticleUpdate(const dt: Double);
var
  i, j: Integer;
begin
  for i := 0 to ParticleEmitter.Particles.Count - 1 do
    with ParticleEmitter.Particles[i] do
      if Visible then
        for j := 0 to 3 do
          Vertices[j].col.w := (LifeTime - T) / LifeTime;
end;

procedure TGame.OnFinish;
begin
  // Write here code for destroying all of your objects
  uFMOD_StopSong();

  EnemyManager.Free();
  Player.Free();

  SceneHud.Free();

  DebugText.Free();

  BulletManager.Free();
  ParticleEmitter.Free();

  SpriteBatch.Free();
  FontBatch.Free();

  MainMaterial.Free();
  Font.Free();
end;

procedure TGame.OnInput(aType: TglrInputType; aKey: TglrKey; X, Y,
  aOtherParam: Integer);
begin
  // Calls when engine receives some input info
  if (aType = itKeyUp) and (aKey = kP) then
    Pause := not Pause;
end;

procedure TGame.OnUpdate(const dt: Double);
var
  axisX, axisY: Integer;
begin
  if Pause then
    Exit();

  // Place here game logic code
  if (Core.Input.KeyDown[kA]) then
    axisY := - 1
  else if (Core.Input.KeyDown[kD]) then
    axisY :=  1
  else
    axisY := 0;

  if (Core.Input.KeyDown[kW]) then
    axisX := 1
  else if (Core.Input.KeyDown[kS]) then
    axisX := - 1
  else
    axisX := 0;

  Player.Update(dt, axisX, axisY);
  ParticleEmitter.Update(dt);
  BulletManager.Update(dt);
  EnemyManager.Update(dt);

  if (Core.Input.Touch[1].IsDown) then
    Player.Fire();
end;

procedure TGame.OnRender;
var
  i: Integer;
begin
  // It calls on every draw
  SceneHud.RenderScene();
  FontBatch.Start();
    FontBatch.Draw(DebugText);
  FontBatch.Finish();

  MainMaterial.Bind();
  SpriteBatch.Start();
    SpriteBatch.Draw(Player.Hero);
    SpriteBatch.Draw(Player.Weapon);
    for i := 0 to EnemyManager.Enemies.Count - 1 do
      SpriteBatch.Draw(EnemyManager.Enemies[i]);
  SpriteBatch.Finish();

  BulletManager.RenderSelf();

  // Render ParticleEmitter
  MainMaterial.DepthWrite := False;
  MainMaterial.Bind();
  ParticleEmitter.RenderSelf();
  MainMaterial.DepthWrite := True;
  MainMaterial.Bind();
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
  //Calls when windows has chagned size
end;


end.

