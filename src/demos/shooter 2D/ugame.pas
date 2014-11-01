{
  todo:
    bonuses
      triple shot
      alternative usage
      health
    scores
    weapons
      rockets
      mines
    gameover
}

unit uGame;

interface

uses
  tinyglr, glrMath;

type

  TBulletType = (bSimple, bFragile, bRocket);

  TBulletOwner = (bPlayer, bEnemy);

  { TBullet }

  TBullet = class (TglrSprite)
    T, LifeTime: Single;
    Velocity: TdfVec2f;
    RotationVelocity: Single;
    BType: TBulletType;
    Owner: TBulletOwner;
    Damage: Single;
    procedure Reset();
  end;

  TBullets = TglrObjectList<TBullet>;

  { TBulletManager }

  TBulletManager = class
  private
    fBullets: TBullets;
    fBatch: TglrSpriteBatch;
  public
    constructor Create(aBatch: TglrSpriteBatch);
    destructor Destroy();

    function GetNewBullet(): TBullet;

    procedure Update(const dt: Double);
    procedure RenderSelf();
  end;

  { TUnit }

  TUnit = class (TglrSprite)
  protected
    fBulletOwner: TBulletOwner;
    fWeaponDir: TdfVec2f;
    fT, fSmokeT: Single; // for shooting

    fBonusTriple: Boolean;
  public
    Health, HealthMax: Single;
    Weapon: TglrSprite;
    FireThreshold, RotateSpeed, DirectSpeed: Single;
    MainWeaponVelocity, MainWeaponDispersion, MainWeaponDamage: Single;
    AltWeaponVelocity, AltWeaponDamage: Single;

    constructor Create(); virtual;
    destructor Destroy(); override;

    procedure Update(const dt: Double; axisX, axisY: Integer); virtual;
    procedure Fire();
    procedure FireAlternative();

    procedure GetKilled(); virtual;

    procedure Reset();
  end;

  { TPlayer }

  TPlayer = class (TUnit)
  public
    constructor Create(); override;
    procedure Update(const dt: Double; axisX, axisY: Integer); override;
    procedure GetKilled(); override;
  end;

  { TEnemy }

  TEnemy = class (TUnit)
  public
    constructor Create(); override;
    procedure Update(const dt: Double; axisX, axisY: Integer); override;
  end;

  TEnemies = TglrObjectList<TEnemy>;

  { TEnemyManager }

  TEnemyManager = class
  private
    fT: Single;
    fBatch: TglrSpriteBatch;
  public
    EnemySpawnInterval: Single;
    EnemySpawnCount: Integer;
    Enemies: TEnemies;
    constructor Create(aBatch: TglrSpriteBatch);
    destructor Destroy();

    function GetNewEnemy(): TEnemy;

    procedure Update(const dt: Double);
    procedure RenderSelf();
  end;

  { TGame }

  TGame = class (TglrGame)
  private
    Pause: Boolean;

    SpriteBatch: TglrSpriteBatch;
    FontBatch: TglrFontBatch;

    Font: TglrFont;

    MainMaterial: TglrMaterial;
    ParticleEmitter, ShellEmitter: TglrCustomParticleEmitter2D;
    BulletManager: TBulletManager;
    EnemyManager: TEnemyManager;

    DebugText: TglrText;
    SceneHud: TglrScene;

    Player: TPlayer;
    procedure InitEnemies();
    procedure ParticleBoom(aPos: TdfVec2f);
    procedure ParticleBigBoom(aPos: TdfVec2f);
    procedure ParticleSmoke(aPos: TdfVec2f);

    procedure ParticleShells(aPos, aDir: TdfVec2f);

    procedure ParticleUpdate(const dt: Double);
    procedure ShellUpdate(const dt: Double);

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

{ TEnemy }

constructor TEnemy.Create;
begin
  inherited Create;
  fBulletOwner := bEnemy;
  Width := 45;
  Height := 25;

  SetVerticesColor(dfVec4f(0.7, 0.3, 0.2, 1.0));
  Weapon.SetVerticesColor(dfVec4f(0.6, 0.2, 0.2, 1.0));

  HealthMax := 15;

  RotateSpeed := 2;
  DirectSpeed := 70;
  FireThreshold := 4.0;

  MainWeaponVelocity := 250;
  MainWeaponDispersion := 0.2;
  MainWeaponDamage := 3;
end;

procedure TEnemy.Update(const dt: Double; axisX, axisY: Integer);
begin
  if not Visible then
    Exit();
  fWeaponDir := dfVec2f(Game.Player.Position - Position).Normal;
  Weapon.Rotation := fWeaponDir.GetRotationAngle();


  if (Position - Game.Player.Position).LengthQ > (190 * 190) then
  begin
    Rotation := LerpAngles(Rotation, fWeaponDir.GetRotationAngle(), RotateSpeed * dt);
  end
  else
    Rotation := LerpAngles(Rotation, dfVec2f(-fWeaponDir.y, fWeaponDir.x).GetRotationAngle(), RotateSpeed * dt);

  // Always move!
  axisX := 1;
  axisY := 0;
  Fire();
  inherited Update(dt, axisX, axisY);
end;

{ TPlayer }

constructor TPlayer.Create;
begin
  inherited Create;
  fBulletOwner := bPlayer;
  Width := 45;
  Height := 25;

  HealthMax := 100;
  Health := HealthMax;

  Position := dfVec3f(Render.Width / 2, Render.Height / 2, 1);
  SetVerticesColor(dfVec4f(0.3, 0.7, 0.3, 1.0));
  Weapon.SetVerticesColor(dfVec4f(0.2, 0.6, 0.2, 1.0));

  RotateSpeed := 130;
  DirectSpeed := 150;
  FireThreshold := 0.1;

  MainWeaponVelocity := 650;
  MainWeaponDispersion := 0.1;
  MainWeaponDamage := 5;

  AltWeaponDamage := 30;
  AltWeaponVelocity := 300;

  fBonusTriple := True;
end;

procedure TPlayer.Update(const dt: Double; axisX, axisY: Integer);
begin
  inherited Update(dt, axisX, axisY);
  fWeaponDir := (Core.Input.MousePos - dfVec2f(Position)).Normal;
  Weapon.Rotation := fWeaponDir.GetRotationAngle();
end;

procedure TPlayer.GetKilled;
begin
  inherited GetKilled;
  //todo: gameover
end;

{ TEnemyManager }

constructor TEnemyManager.Create(aBatch: TglrSpriteBatch);
begin
  inherited Create;
  fBatch := aBatch;
  Enemies := TEnemies.Create(40);
  EnemySpawnInterval := 3.0;
  EnemySpawnCount := 4;
end;

destructor TEnemyManager.Destroy;
begin
  Enemies.Free(True);
  inherited;
end;

function TEnemyManager.GetNewEnemy: TEnemy;
var
  i: Integer;
  e: TEnemy;
begin
  for i := 0 to Enemies.Count - 1 do
    if not Enemies[i].Visible then
    begin
      Enemies[i].Reset();
      Exit(Enemies[i]);
    end;

  e := TEnemy.Create();
  e.Reset();
  Enemies.Add(e);
  Exit(e);
end;

procedure TEnemyManager.Update(const dt: Double);
var
  i: Integer;
  e: TEnemy;
begin
  for i := 0 to Enemies.Count - 1 do
    Enemies[i].Update(dt, 0, 0);

  if (fT > 0) then
    fT -= dt
  else if (fT <= 0) then
  begin
    fT := EnemySpawnInterval;
    for i := 0 to EnemySpawnCount - 1 do
    begin
      e := GetNewEnemy();
      e.Position := dfVec3f(dfVec2f(Random(360)) * 700 + dfVec2f(Render.Width / 2, Render.Height / 2), 0);
    end;
  end;
end;

procedure TEnemyManager.RenderSelf();
var
  i: Integer;
begin
  fBatch.Start();
  for i := 0 to Enemies.Count - 1 do
  begin
    fBatch.Draw(Enemies[i]);
    fBatch.Draw(Enemies[i].Weapon);
  end;
  fBatch.Finish();
end;

{ TBulletManager }

constructor TBulletManager.Create(aBatch: TglrSpriteBatch);
var
  i: Integer;
  b: TBullet;
begin
  inherited Create;
  fBatch := aBatch;
  fBullets := TBullets.Create(128);
  for i := 0 to 128 do
  begin
    b := TBullet.Create(12, 2, dfVec2f(0.5, 0.5));
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

  b := TBullet.Create(12, 2, dfVec2f(0.5, 0.5));
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
        for j := 0 to 3 do
          Vertices[j].col.w := (LifeTime - T) / LifeTime;
        if (T > LifeTime) then
          Visible := False
        else
        begin
          Position += dfVec3f(Velocity * dt, 0);
          Rotation := Rotation + RotationVelocity * dt;

          if (Owner = bEnemy) then
          begin
            if (PointToSpriteIntersect(dfVec2f(Position), Game.Player)) then
            begin
              Game.DebugText.Text := 'Player hit!';
              Game.ParticleBoom(dfVec2f(Position));
              Game.Player.Health -= Damage;
              if (Game.Player.Health  <= 0) then
                Game.Player.GetKilled();
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

                e.Health -= Damage;

                if e.Health <= 0 then
                begin
                  e.GetKilled();
                  Game.ParticleBigBoom(dfVec2f(e.Position));
                end;

                break;
              end;
            end;
        end;
      end;
end;

procedure TBulletManager.RenderSelf;
var
  i: Integer;
begin
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
  RotationVelocity := 0;
  Visible := True;
  SetVerticesColor(dfVec4f(1, 1, 1, 1));
end;

{ TPlayer }

procedure TUnit.Reset();
begin
  Health := HealthMax;
  fT := 0.0;
  Visible := True;
  Weapon.Visible := True;
  Position := dfVec3f(-1000, -1000, 0);
  Weapon.Position := Position + dfVec3f(0, 0, 1);
end;

constructor TUnit.Create;
begin
  inherited;
  Weapon := TglrSprite.Create(25, 8, dfVec2f(0.0, 0.5));
  Weapon.Position := Position + dfVec3f(0, 0, 1);

  fBonusTriple := False;
  fT := 0.0;
end;

destructor TUnit.Destroy;
begin
  Weapon.Free();
  inherited;
end;

procedure TUnit.Update(const dt: Double; axisX, axisY: Integer);
begin
  if not Visible then
    Exit();
  Rotation := Rotation + (RotateSpeed * dt * axisY);
  Position += dfVec3f(DirectSpeed * dt * axisX * dfVec2f(Rotation), 0);

  Weapon.Position := Position + dfVec3f(0, 0, 1);

  if (fT > 0) then
    fT -= dt;

  if (fSmokeT > 0) then
    fSmokeT -= dt
  else
  begin
    fSmokeT := 0.01;
    Game.ParticleSmoke(dfVec2f(Position) - dfVec2f(Rotation) * (Width - 15));
  end;
end;

procedure TUnit.Fire;
var
  b: TBullet;
  bulletDir: TdfVec2f;
  i, count: Integer;
begin
  if (fT <= 0) then
  begin
    fT := FireThreshold;

    if fBonusTriple then
      count := 3
    else
      count := 1;

    bulletDir := fWeaponDir +
      dfVec2f(- fWeaponDir.y, fWeaponDir.x) * MainWeaponDispersion * (0.5 - Random());

    for i := 0 to count - 1 do
    begin
      b := Game.BulletManager.GetNewBullet();
      b.Width := 12;
      b.Height := 2;
      b.BType := bSimple;
      b.Owner := fBulletOwner;
      b.Damage := MainWeaponDamage;

      b.Velocity := bulletDir * MainWeaponVelocity;
      b.Rotation := b.Velocity.GetRotationAngle();
      b.Position := Weapon.Position + dfVec3f(fWeaponDir * 20, 0);
      if fBonusTriple then
        b.Position += dfVec3f(dfVec2f(-bulletDir.y, bulletDir.x) * (-1 + i) * 5, 0);
      Game.ParticleShells(dfVec2f(b.Position) + dfVec2f(i * 5, i* 5),
        (dfVec2f(-fWeaponDir.y, fWeaponDir.x) + dfVec2f(10 - Random(20))).Normal);
    end;
  end;
end;

procedure TUnit.FireAlternative;
var
  i: Integer;
begin
  for i := 0 to 35 do
    with Game.BulletManager.GetNewBullet() do
    begin
      Width := 15;
      Height := 5;
      SetVerticesColor(dfVec4f(219 / 255, 104 / 255, 3 / 255, 1.0));
      BType := bFragile;
      Owner := fBulletOwner;
      Damage := AltWeaponDamage;
      Velocity := dfVec2f(i * 10) * AltWeaponVelocity;
      Rotation := Velocity.GetRotationAngle();
      Position := Self.Position;
      RotationVelocity := 150;
      LifeTime := 1.0;
    end;
  Game.ParticleBoom(dfVec2f(Self.Position));
end;

procedure TUnit.GetKilled();
begin
  Visible := False;
  Weapon.Visible := False;
end;

{ TGame }

procedure TGame.OnStart;
begin
  // Write here initialization code
  Randomize();

  //Render.SetClearColor(43 / 255, 99 / 255, 147 / 255);
  Render.SetClearColor(0.15, 0.15, 0.15);
  Font := TglrFont.Create(FileSystem.ReadResourceLZO('shooter/font.bmp', False));

  MainMaterial := TglrMaterial.Create();
  MainMaterial.Shader.Free();
  MainMaterial.Shader := Default.SpriteShader;
  MainMaterial.AddTexture(Default.BlankTexture, 'uDiffuse');

  SpriteBatch := TglrSpriteBatch.Create();
  FontBatch := TglrFontBatch.Create(Font);

  ParticleEmitter := TglrCustomParticleEmitter2D.Create(SpriteBatch, MainMaterial);
  ParticleEmitter.OnUpdate := ParticleUpdate;

  ShellEmitter := TglrCustomParticleEmitter2D.Create(SpriteBatch, MainMaterial);
  ShellEmitter.OnUpdate := ShellUpdate;

  BulletManager := TBulletManager.Create(SpriteBatch);

  DebugText := TglrText.Create(UTF8Decode('Test'));
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

  EnemyManager := TEnemyManager.Create(SpriteBatch);
  InitEnemies();

  uFMOD_PlaySong(@xm1, Length(xm1), XM_MEMORY);

  Pause := False;
end;

procedure TGame.InitEnemies;
begin

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
      //SetVerticesColor(dfVec4f(1.0, 0.5 * Random(), 0.3 * Random(), 1.0));
      SetVerticesColor(dfVec4f(1.0, 0.5, 0.2, 0.3 + Random()));
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
  for i := 0 to 72 do
    with ParticleEmitter.GetNewParticle() do
    begin
      Position := dfVec3f(aPos, 5);
      Position.z += 1;
      Position += dfVec3f(10 - Random(20), 10 - Random(20), Random(3));
      Rotation := Random(180);
      //SetVerticesColor(dfVec4f(Random(), Random(), Random(), 1.0));
      //SetVerticesColor(dfVec4f(1.0, 0.7 * Random(), 0.6 * Random(), 1.0));
      SetVerticesColor(dfVec4f(1.0, 0.5, 0.2, 0.3 + Random()));
      Width := 5 + 10 * Random();
      Height := Width;
      LifeTime := 0.7;
      Velocity := dfVec2f(Random(360)) * (90 + Random(40));
    end;
end;

procedure TGame.ParticleSmoke(aPos: TdfVec2f);
var
  i: Integer;
begin
  for i := 0 to 1 do
    with ParticleEmitter.GetNewParticle() do
    begin
      Position := dfVec3f(aPos, 5);
      Position.z += 1;
      Position += dfVec3f(5 - Random(10), 5 - Random(10), 0);
      Rotation := Random(180);
      SetVerticesColor(dfVec4f(0.3, 0.3, 0.45, 0.7 * Random()));
      Width := 3 + 4 * Random();
      Height := Width;
      LifeTime := 0.5;
      Velocity := dfVec2f(Random(360)) * (30 + Random(10));
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

procedure TGame.ShellUpdate(const dt: Double);
var
  i: Integer;
begin
  for i := 0 to ShellEmitter.Particles.Count - 1 do
    with ShellEmitter.Particles[i] do
    begin
      if Visible then
        Velocity := Velocity * (1 - 2 * dt);
    end;
end;

procedure TGame.ParticleShells(aPos, aDir: TdfVec2f);
begin
  with ShellEmitter.GetNewParticle() do
  begin
    Position := dfVec3f(aPos + aDir* 5, 5);
    Rotation := dfVec2f(- aDir.y, aDir.x).GetRotationAngle() + Random(50);
    //SetVerticesColor(dfVec4f(Random(), Random(), Random(), 1.0));
    SetVerticesColor(dfVec4f(0.7,  0.6, 0.1, 1.0));
    Width := 8;
    Height := 2;
    LifeTime := 1.5;
    Velocity := aDir * (120 + Random(10));
  end;
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
  ShellEmitter.Free();

  SpriteBatch.Free();
  FontBatch.Free();

  MainMaterial.Free();
  Font.Free();
end;

procedure TGame.OnInput(aType: TglrInputType; aKey: TglrKey; X, Y,
  aOtherParam: Integer);
begin
  // Calls when engine receives some input info
  if (aType = itKeyDown) and (aKey = kSpace) then
    Pause := not Pause;

  if (aType = itTouchDown) and (aKey = kRightButton) then
    Player.FireAlternative();
end;

procedure TGame.OnUpdate(const dt: Double);
var
  axisX, axisY: Integer;
begin
  if Pause then
    Exit();

  // Place here game logic code
  if (Core.Input.KeyDown[kA]) then
    axisY := -1
  else if (Core.Input.KeyDown[kD]) then
    axisY := 1
  else
    axisY := 0;

  if (Core.Input.KeyDown[kW]) then
    axisX := 1
  else if (Core.Input.KeyDown[kS]) then
    axisX := -1
  else
    axisX := 0;

  Player.Update(dt, axisX, axisY);
  ParticleEmitter.Update(dt);
  ShellEmitter.Update(dt);
  BulletManager.Update(dt);
  EnemyManager.Update(dt);

  if (Core.Input.Touch[1].IsDown) then
    Player.Fire();
end;

procedure TGame.OnRender;
begin
  // It calls on every draw
  SceneHud.RenderScene();
  FontBatch.Start();
    FontBatch.Draw(DebugText);
  FontBatch.Finish();

  MainMaterial.Bind();
  SpriteBatch.Start();
    SpriteBatch.Draw(Player);
    SpriteBatch.Draw(Player.Weapon);
  SpriteBatch.Finish();

  EnemyManager.RenderSelf();
  BulletManager.RenderSelf();

  // Render ParticleEmitter
  MainMaterial.DepthWrite := False;
  MainMaterial.Bind();
  ParticleEmitter.RenderSelf();
  ShellEmitter.RenderSelf();
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
  // Calls when windows has changed size
end;


end.

