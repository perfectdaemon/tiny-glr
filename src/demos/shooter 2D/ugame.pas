{
  todo:
  + bonuses
    + triple shot
    + alternative usage
    + health
    + ricochet
  + scores
  + weapons
  + gameover

  + hud for bombs
  + hud for health

  + normal pause button
  + OnPause/OnResume

  + enemy spawn progress
  + attention text

  + music volume
}

unit uGame;

interface

uses
  tinyglr, glrMath;

const
  HEALTH_PLAYER = 75;
  HEALTH_ENEMY = 15;

  PLAYER_ROTATE_SPEED = 130;
  PLAYER_DIRECT_SPEED = 150;
  PLAYER_FIRE_INTERVAL = 0.1;
  PLAYER_MAIN_WEAPON_VELOCITY = 650;
  PLAYER_MAIN_WEAPON_DISPERSION = 0.15;
  PLAYER_MAIN_WEAPON_DAMAGE = 5;
  PLAYER_ALT_WEAPON_DAMAGE = 30;
  PLAYER_ALT_WEAPON_VELOCITY = 300;

  SHELL_WIDTH = 7;
  SHELL_HEIGHT = 2;
  SHELL_LIFETIME = 1.5;

  BULLET_WIDTH = 12;
  BULLET_HEIGHT = 2;

  //Bonuses
  HEALTH_BONUS = 10;
  TRIPLESHOT_TIME = 20.0;
  RICOCHET_TIME = 14.0;

  BONUS_LIFETIME = 12.0;
  BONUS_COLLECT_RADIUS = 25;
  BONUS_MAGNET_RADIUS = 100;

  SCORES_FOR_ENEMY = 10;

  ENEMY_SPAWN_INTERVAL = 3.0;
  ENEMY_SPAWN_COUNT = 4;
  ENEMY_ROTATE_SPEED = 2;
  ENEMY_DIRECT_SPEED = 70;
  ENEMY_FIRE_INTERVAL = 4.0;

  ENEMY_MAIN_WEAPON_VELOCITY = 250;
  ENEMY_MAIN_WEAPON_DISPERSION = 0.2;
  ENEMY_MAIN_WEAPON_DAMAGE = 5;

  ENEMY_WAVE_COUNT = 10;

  ATTENTION_POPUP_TIME = 5.0;

type

  { TPopupText }

  TPopupText = class (TglrText)
    T: Single;
    procedure Reset();
    procedure Update(const dt: Double);
  end;

  TPopupTexts = TglrObjectList<TPopupText>;

  { TPopupManager }

  TPopupManager = class
  private
    fBatch: TglrFontBatch;
    fPopups: TPopupTexts;
  public
    constructor Create(aBatch: TglrFontBatch); virtual;
    destructor Destroy(); override;

    procedure Update(const dt: Double);
    procedure RenderSelf();

    function GetNewPopup(): TPopupText;
  end;


  TBonusType = (bTripleShot, bAlternativeShot, bHealth, bRicochet);

  { TBonus }

  TBonus = class (TglrText)
  private
    fT, fLifeTime: Single;
    fBonusType: TBonusType;
    fVelocity: TdfVec3f;
    procedure SetBonusType(aBonusType: TBonusType);
  public
    property BonusType: TBonusType read fBonusType write SetBonusType;
    procedure Reset();
    procedure Update(const dt: Double);
    procedure Use();
  end;

  TBonuses = TglrObjectList<TBonus>;

  { TBonusManager }

  TBonusManager = class
  private
    fBatch: TglrFontBatch;
    fBonuses: TBonuses;
  public
    constructor Create(aBatch: TglrFontBatch); virtual;
    destructor Destroy(); override;

    procedure Update(const dt: Double);
    procedure RenderSelf();

    function GetNewBonus(): TBonus;
  end;

  TBulletType = (bSimple, bFragile, bRocket);

  TBulletOwner = (bPlayer, bEnemy);

  { TBullet }

  TBullet = class (TglrSprite)
    T, LifeTime: Single;
    Velocity: TdfVec2f;
    RotationVelocity: Single;
    BType: TBulletType;
    Owner: TBulletOwner;
    Damage: Integer;
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

    fBonusTriple, fBonusRicochet: Boolean;
    fBonusT: array[TBonusType] of Single;
  public
    HealthMax: Integer;
    FireThreshold, RotateSpeed, DirectSpeed: Single;
    MainWeaponVelocity, MainWeaponDispersion: Single;
    AltWeaponVelocity: Single;
    MainWeaponDamage, AltWeaponDamage: Integer;

    Health: Integer;
    Weapon: TglrSprite;

    AltWeaponCount: Integer;

    constructor Create(); virtual;
    destructor Destroy(); override;

    procedure Update(const dt: Double; axisX, axisY: Integer); virtual;
    procedure Fire();
    procedure FireAlternative();

    procedure GetKilled(); virtual;

    procedure Reset(); virtual;
  end;

  { TPlayer }

  TPlayer = class (TUnit)
  private
    fHealthAnimateT, fGameOverT: Single;
  public
    FrontBumper: TglrSprite;
    BonusInfo, HealthText, AltWeaponText: TglrText;
    constructor Create(); override;
    destructor Destroy(); override;
    procedure Update(const dt: Double; axisX, axisY: Integer); override;
    procedure GetKilled(); override;
  end;

  { TEnemy }

  TEnemy = class (TUnit)
  public
    class var
      _HealthMax: Integer;
      _FireThreshold, _RotateSpeed, _DirectSpeed: Single;
      _MainWeaponVelocity, _MainWeaponDispersion: Single;
      _AltWeaponVelocity: Single;
      _MainWeaponDamage, _AltWeaponDamage: Integer;
    constructor Create(); override;
    procedure Update(const dt: Double; axisX, axisY: Integer); override;
    procedure GetKilled(); override;

    procedure Reset(); override;
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
    WaveCount: Integer;
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

    BonusManager: TBonusManager;
    PopupManager: TPopupManager;

    DebugText, PauseText: TglrText;
    PauseSprite: TglrSprite;
    SceneHud: TglrScene;
    Camera: TglrCamera;

    Scores: Integer;

    Player: TPlayer;

    MusicVolume: LongWord;

    GameOver: Boolean;
    GameOverText: TglrText;

    AttentionText: TglrText;
    fAttT: Single;

    EnemiesKilled, PreviouslyKilled: Integer;

    AutoFire: Boolean;
    procedure SetGameOver();
    procedure ParticleBoom(aPos: TdfVec2f);
    procedure ParticleBigBoom(aPos: TdfVec2f);
    procedure ParticleSmoke(aPos: TdfVec2f);

    procedure ParticleShells(aPos, aDir: TdfVec2f);

    procedure ParticleUpdate(const dt: Double);
    procedure ShellUpdate(const dt: Double);

    function GenerateTexture(aWidth, aHeight, aBorderSize: Integer): TglrTexture;

    procedure SetAttention(aText: WideString);
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

{ TPopupManager }

constructor TPopupManager.Create(aBatch: TglrFontBatch);
var
  i: Integer;
  p: TPopupText;
begin
  inherited Create();
  fBatch := aBatch;
  fPopups := TPopupTexts.Create(30);
  for i := 0 to 29 do
  begin
    p := TPopupText.Create();
    p.Reset();
    p.Visible := False;
    fPopups.Add(p);
  end;
end;

destructor TPopupManager.Destroy;
begin
  fPopups.Free(True);
  inherited Destroy;
end;

procedure TPopupManager.Update(const dt: Double);
var
  i: Integer;
begin
  for i := 0 to fPopups.Count - 1 do
    fPopups[i].Update(dt);
end;

procedure TPopupManager.RenderSelf;
var
  i: Integer;
begin
  fBatch.Start();
    for i := 0 to fPopups.Count - 1 do
      fBatch.Draw(fPopups[i]);
  fBatch.Finish();
end;

function TPopupManager.GetNewPopup: TPopupText;
var
  i: Integer;
  p: TPopupText;
begin
  for i := 0 to fPopups.Count - 1 do
    if not fPopups[i].Visible then
    begin
      fPopups[i].Reset();
      Exit(fPopups[i]);
    end;

  p := TPopupText.Create();
  p.Reset();
  fPopups.Add(p);
  Exit(p);
end;

{ TPopupText }

procedure TPopupText.Reset;
begin
  Text := '';
  Color := dfVec4f(1,1,1,1);
  T := 2.0;
  Visible := True;
  Scale := 0.7;
end;

procedure TPopupText.Update(const dt: Double);
begin
  if not Visible then
    Exit();
  T -= dt;

  Position.y -= 20 * dt;
  Color.w := (T / 2.0);

  if T < 0 then
    Visible := False;
end;

{ TBonusManager }

constructor TBonusManager.Create(aBatch: TglrFontBatch);
var
  b: TBonus;
  i: Integer;
begin
  inherited Create;
  fBatch := aBatch;
  fBonuses := TBonuses.Create(12);
  for i := 0 to 11 do
  begin
    b := TBonus.Create();
    b.Reset();
    b.Visible := False;
    fBonuses.Add(b);
  end;
end;

destructor TBonusManager.Destroy;
begin
  fBonuses.Free(True);
  inherited Destroy;
end;

procedure TBonusManager.Update(const dt: Double);
var
  i: Integer;
begin
  for i := 0 to fBonuses.Count - 1 do
    fBonuses[i].Update(dt);
end;

procedure TBonusManager.RenderSelf;
var
  i: Integer;
begin
  fBatch.Start();
    for i := 0 to fBonuses.Count - 1 do
      fBatch.Draw(fBonuses[i]);
  fBatch.Finish();
end;

function TBonusManager.GetNewBonus: TBonus;
var
  i: Integer;
  b: TBonus;
begin
  for i := 0 to fBonuses.Count - 1 do
    if not fBonuses[i].Visible then
    begin
      fBonuses[i].Reset();
      Exit(fBonuses[i]);
    end;

  b := TBonus.Create();
  b.Reset();
  fBonuses.Add(b);
  Exit(b);
end;

{ TBonus }

procedure TBonus.SetBonusType(aBonusType: TBonusType);
begin
  fBonusType := aBonusType;
  case fBonusType of
    bHealth:          Text := 'H';
    bTripleShot:      Text := 'T';
    bAlternativeShot: Text := 'B';
    bRicochet:        Text := 'R';
  end;
end;

procedure TBonus.Reset();
begin
  fT := 0;
  fLifeTime := BONUS_LIFETIME;
  fVelocity.Reset();
  Color := dfVec4f(1, 1, 1, 1);
  Visible := True;
  Scale := 1.3;
end;

procedure TBonus.Update(const dt: Double);
var
  magnitude: Single;
begin
  if not Visible then
    Exit();

  magnitude := Abs(sin(fT * 2));

  case BonusType of
    bTripleShot:      Color := dfVec4f(0.3 + magnitude, 0.3 + magnitude, 1.0, 1.0);
    bHealth:          Color := dfVec4f(1.0, 0.3 + magnitude, 0.3 + magnitude, 1.0);
    bRicochet:        Color := dfVec4f(0.3 + magnitude, 1.0, 0.3 + magnitude, 1.0);
    bAlternativeShot: Color := dfVec4f(0.6 + magnitude, 0.3 + magnitude, 0.0, 1.0);
  end;

  magnitude := (Position - Game.Player.Position).LengthQ;

  if magnitude < (BONUS_MAGNET_RADIUS * BONUS_MAGNET_RADIUS) then
    fVelocity := (Game.Player.Position - Position).Normal * 180 * dt;

  Position += fVelocity;

  if magnitude < (BONUS_COLLECT_RADIUS * BONUS_COLLECT_RADIUS) then
    Use();

  fT += dt;
  if (fT >= fLifeTime) then
    Visible := False;
end;

procedure TBonus.Use;
begin
  case BonusType of

    bTripleShot:
    begin
      Game.Player.fBonusTriple := True;
      Game.Player.fBonusT[bTripleShot] := TRIPLESHOT_TIME;
    end;

    bHealth:
    begin
      Game.Player.Health := Min(Game.Player.Health + HEALTH_BONUS, Game.Player.HealthMax);
      with Game.PopupManager.GetNewPopup() do
      begin
        if (Game.Player.Health = Game.Player.HealthMax) then
          Text := 'Full health!'
        else
          Text := '+' + Convert.ToString(HEALTH_BONUS) + ' health';
        Color := dfVec4f(0.1, 0.6, 0.1, 1.0);
        Position := Self.Position;
        T := 3;
      end;
    end;

    bAlternativeShot:
    begin
      Game.Player.AltWeaponCount += 1;
      with Game.PopupManager.GetNewPopup() do
      begin
        Text := '+ 1 super bomb';
        Color := dfVec4f(0.1, 0.6, 0.1, 1.0);
        Position := Self.Position;
        T := 3;
      end;
    end;

    bRicochet:
    begin
      Game.Player.fBonusRicochet := True;
      Game.Player.fBonusT[bRicochet] := RICOCHET_TIME;
    end;
  end;

  Visible := False;
end;

{ TEnemy }

constructor TEnemy.Create;
begin
  inherited Create;
  fBulletOwner := bEnemy;
  Width := 45;
  Height := 25;

  SetVerticesColor(dfVec4f(0.7 + 0.4 * Random(), 0.25 + 0.2 * Random(), 0.3, 1.0));
  Weapon.SetVerticesColor(dfVec4f(0.6, 0.2, 0.2, 1.0));

  HealthMax := TEnemy._HealthMax;

  RotateSpeed := TEnemy._RotateSpeed;
  DirectSpeed := TEnemy._DirectSpeed;
  FireThreshold := TEnemy._FireThreshold;

  MainWeaponVelocity := TEnemy._MainWeaponVelocity;
  MainWeaponDispersion := TEnemy._MainWeaponDispersion;
  MainWeaponDamage := TEnemy._MainWeaponDamage;
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

procedure TEnemy.GetKilled;
var
  roll: Single;
begin
  inherited GetKilled;

  Game.Scores += SCORES_FOR_ENEMY + Game.EnemyManager.WaveCount;
  Game.EnemiesKilled += 1;
  with Game.PopupManager.GetNewPopup() do
  begin
    Text := '+' + Convert.ToString(SCORES_FOR_ENEMY + Game.EnemyManager.WaveCount) + ' points';
    Color := dfVec4f(0.6, 0.6, 0.1, 1.0);
    Position := Self.Position;
    T := 3;
  end;

  if Random() < 0.2 then
    with Game.BonusManager.GetNewBonus() do
    begin
      roll := Random();
      if roll < 0.3 then
        BonusType := bHealth
      else if roll < 0.6 then
        BonusType := bTripleShot
      else if roll < 0.8 then
        BonusType := bAlternativeShot
      else
        BonusType := bRicochet;

      Position := Self.Position;
    end;
end;

procedure TEnemy.Reset;
begin
  inherited Reset;
  HealthMax := TEnemy._HealthMax;

  RotateSpeed := TEnemy._RotateSpeed;
  DirectSpeed := TEnemy._DirectSpeed;
  FireThreshold := TEnemy._FireThreshold;

  MainWeaponVelocity := TEnemy._MainWeaponVelocity;
  MainWeaponDispersion := TEnemy._MainWeaponDispersion;
  MainWeaponDamage := TEnemy._MainWeaponDamage;
end;

{ TPlayer }

constructor TPlayer.Create;
begin
  inherited Create;
  BonusInfo := TglrText.Create();
  BonusInfo.Color := dfVec4f(0.3, 0.7, 0.3, 0.5);
  BonusInfo.Scale := 0.7;

  HealthText := TglrText.Create('Low health!');
  HealthText.Scale := 0.7;
  HealthText.Color := dfVec4f(1.0, 0.0, 0.0, 1.0);
  HealthText.Visible := False;
  fHealthAnimateT := 0;

  AltWeaponText := TglrText.Create();
  AltWeaponText.Scale := 0.7;
  AltWeaponText.Color := dfVec4f(0.3, 0.7, 0.3, 0.5);
  AltWeaponText.Visible := True;

  fBulletOwner := bPlayer;
  Width := 46;
  Height := 25;

  HealthMax := HEALTH_PLAYER;
  Health := HealthMax;

  Position := dfVec3f(Render.Width / 2, Render.Height / 2, 1);
  SetVerticesColor(dfVec4f(0.3, 0.7, 0.3, 1.0));
  Weapon.SetVerticesColor(dfVec4f(0.2, 0.6, 0.2, 1.0));

  RotateSpeed := PLAYER_ROTATE_SPEED;
  DirectSpeed := PLAYER_DIRECT_SPEED;
  FireThreshold := PLAYER_FIRE_INTERVAL;

  MainWeaponVelocity := PLAYER_MAIN_WEAPON_VELOCITY;;
  MainWeaponDispersion := PLAYER_MAIN_WEAPON_DISPERSION;
  MainWeaponDamage := PLAYER_MAIN_WEAPON_DAMAGE;

  AltWeaponDamage := PLAYER_ALT_WEAPON_DAMAGE;
  AltWeaponVelocity := PLAYER_ALT_WEAPON_VELOCITY;

  AltWeaponCount := 1;

  FrontBumper := TglrSprite.Create(10, 25, dfVec2f(0, 0.5));
  FrontBumper.SetVerticesColor(dfVec4f(0.2, 0.6, 0.2, 1.0));
  FrontBumper.Position := dfVec3f(23, 0, 1);
  FrontBumper.Parent := Self;
  FrontBumper.Vertices[0].vec.y -= 5;
  FrontBumper.Vertices[1].vec.y += 5;
end;

destructor TPlayer.Destroy;
begin
  AltWeaponText.Free();
  HealthText.Free();
  BonusInfo.Free();
  FrontBumper.Free();
  inherited Destroy;
end;

procedure TPlayer.Update(const dt: Double; axisX, axisY: Integer);
var
  add: Single;
begin
  inherited Update(dt, axisX, axisY);

  if not Visible then
  begin
    fGameOverT -= dt;
    if fGameOverT < 0 then
      Game.SetGameOver();
    Exit();
  end;

  fWeaponDir := (Core.Input.MousePos - dfVec2f(Position)).Normal;
  Weapon.Rotation := fWeaponDir.GetRotationAngle();

  if Position.x < - (Width / 2) then
    Position.x := Render.Width + (Width / 2)
  else if Position.x > Render.Width + (Width / 2) then
    Position.x := - (Width / 2);
  if Position.y < - (Height / 2) then
    Position.y := Render.Height + (Height / 2)
  else if Position.y > Render.Height + (Height / 2) then
    Position.y := - (Height / 2);

  BonusInfo.Text := '';
  add := 0;
  if (fBonusTriple) then
  begin
    BonusInfo.Text := 'Triple shot - ' + Convert.ToString(fBonusT[bTripleShot], 1) + #13#10;
    add += 35;
  end;
  if (fBonusRicochet) then
  begin
    BonusInfo.Text += 'Ricochet - ' + Convert.ToString(fBonusT[bRicochet], 1);
    add += 35;
  end;

  BonusInfo.Position := Position + dfVec3f(-30, -add, 4);
  HealthText.Position := Position + dfVec3f(-30, 30, 4);
  AltWeaponText.Position := Position + dfVec3f(30, 0, 4);

  if Health <= (HealthMax div 2) then
  begin
    HealthText.Visible := True;
    fHealthAnimateT += dt;
    HealthText.Color.w := Abs(sin(fHealthAnimateT * 3));
  end
  else
  begin
    HealthText.Visible := False;
    HealthText.Color.w := 1.0;
    fHealthAnimateT := 0;
  end;

  if AltWeaponCount > 0 then
    AltWeaponText.Text := Convert.ToString(AltWeaponCount) + ' bomb'
  else
    AltWeaponText.Text := '';
end;

procedure TPlayer.GetKilled;
begin
  inherited GetKilled;
  BonusInfo.Visible := False;
  HealthText.Visible := False;
  FrontBumper.Visible := False;
  fGameOverT := 2.0;
end;

{ TEnemyManager }

constructor TEnemyManager.Create(aBatch: TglrSpriteBatch);
begin
  inherited Create;
  fBatch := aBatch;
  Enemies := TEnemies.Create(40);
  EnemySpawnInterval := ENEMY_SPAWN_INTERVAL;
  EnemySpawnCount := ENEMY_SPAWN_COUNT;

  TEnemy._HealthMax := HEALTH_ENEMY;

  TEnemy._RotateSpeed := ENEMY_ROTATE_SPEED;
  TEnemy._DirectSpeed := ENEMY_DIRECT_SPEED;
  TEnemy._FireThreshold := ENEMY_FIRE_INTERVAL;

  TEnemy._MainWeaponVelocity := ENEMY_MAIN_WEAPON_VELOCITY;
  TEnemy._MainWeaponDispersion := ENEMY_MAIN_WEAPON_DISPERSION;
  TEnemy._MainWeaponDamage := ENEMY_MAIN_WEAPON_DAMAGE;
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

  if ((Game.EnemiesKilled - Game.PreviouslyKilled) > ENEMY_WAVE_COUNT * (WaveCount + 1)) and (WaveCount < 5) then
  begin
    WaveCount += 1;
    Game.PreviouslyKilled := Game.EnemiesKilled;
    EnemySpawnCount += 2;
    EnemySpawnInterval := Max(1.0, EnemySpawnInterval - 0.1);
    if (Game.EnemiesKilled > ENEMY_WAVE_COUNT * 5) then
    begin
      TEnemy._HealthMax += 5;
      TEnemy._DirectSpeed += 5;
      TEnemy._FireThreshold := Max(1.0, TEnemy._FireThreshold - 0.2);
      TEnemy._MainWeaponDamage += 2;
      Game.SetAttention('More enemies are coming!'#13#10'And they become stronger!');
    end
    else
      Game.SetAttention('More enemies are coming!');
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
  fBullets := TBullets.Create(256);
  for i := 0 to 127 do
  begin
    b := TBullet.Create(BULLET_WIDTH, BULLET_HEIGHT, dfVec2f(0.5, 0.5));
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
      Exit(fBullets[i]);
    end;

  b := TBullet.Create(BULLET_WIDTH, BULLET_HEIGHT, dfVec2f(0.5, 0.5));
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
            if not Game.Player.Visible then
              continue;

            if (PointToSpriteIntersect(dfVec2f(Position), Game.Player)) then
            begin
              Game.ParticleBoom(dfVec2f(Position));

              if Game.Player.fBonusRicochet then
              begin
                T := 0;
                Velocity := Velocity.Reflect(dfVec2f(Game.Player.Rotation));
                Rotation := Velocity.GetRotationAngle();
                Owner := bPlayer;
                SetVerticesColor(dfVec4f(1, 0.5, 0.5, 1));
              end
              else
              begin

                with Game.PopupManager.GetNewPopup() do
                begin
                  Text := '-' + Convert.ToString(Damage) + ' health';
                  Color := dfVec4f(0.9, 0.1, 0.1, 1.0);
                  Position := fBullets[i].Position;
                  T := 3;
                end;

                Game.Player.Health -= Damage;
                if (Game.Player.Health  <= 0) then
                begin
                  Game.Player.GetKilled();
                  Game.ParticleBigBoom(dfVec2f(e.Position));
                end;

                 Visible := False;
              end;
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
var
  i: TBonusType;
begin
  if not Visible then
    Exit();
  Rotation := Rotation + (RotateSpeed * dt * axisY);
  Position += dfVec3f(DirectSpeed * dt * axisX * dfVec2f(Rotation), 0);

  Weapon.Position := Position + dfVec3f(0, 0, 1);

  for i := Low(TBonusType) to High(TBonusType) do
    if fBonusT[i] > 0 then
      fBonusT[i] -= dt;

  if (fBonusT[bTripleShot] < 0) then
    fBonusTriple := False;
  if (fBonusT[bRicochet] < 0) then
    fBonusRicochet := False;

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
      b.Width := BULLET_WIDTH;
      b.Height := BULLET_HEIGHT;
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
  if AltWeaponCount <= 0 then
    Exit();

  AltWeaponCount -= 1;

  for i := 0 to 35 do
    with Game.BulletManager.GetNewBullet() do
    begin
      Width := 16;
      Height := 4;
      SetVerticesColor(dfVec4f(24 / 255, 124 / 255, 240 / 255, 1.0));
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

  Render.SetClearColor(0.15, 0.15, 0.15);
  Font := TglrFont.Create(FileSystem.ReadResourceLZO('shooter/font.bmp', False));

  MainMaterial := TglrMaterial.Create();
  MainMaterial.Shader.Free();
  MainMaterial.Shader := Default.SpriteShader;
  MainMaterial.AddTexture(GenerateTexture(64, 32, 2), 'uDiffuse');

  SpriteBatch := TglrSpriteBatch.Create();
  FontBatch := TglrFontBatch.Create(Font);

  ParticleEmitter := TglrCustomParticleEmitter2D.Create(SpriteBatch, MainMaterial);
  ParticleEmitter.OnUpdate := ParticleUpdate;

  ShellEmitter := TglrCustomParticleEmitter2D.Create(SpriteBatch, MainMaterial);
  ShellEmitter.OnUpdate := ShellUpdate;

  BulletManager := TBulletManager.Create(SpriteBatch);

  BonusManager := TBonusManager.Create(FontBatch);

  PopupManager := TPopupManager.Create(FontBatch);

  DebugText := TglrText.Create();
  DebugText.Position := dfVec3f(10, 10, 1);

  PauseText := TglrText.Create();
  PauseText.Position := dfVec3f(Render.Width / 2, 100, 30)
    - dfVec3f(300, 0, 0);
  PauseText.Text := '              P A U S E' + #13#10 +
    'Press "Escape" to continue exterminate enemies' + #13#10#13#10 +
    'Control' + #13#10 +
    'WASD - movement' + #13#10 +
    'LMB - fire main weapon' + #13#10 +
    'RMB - fire bomb (if you have ammo)' + #13#10 +
    'Space - on/off autofire' + #13#10#13#10 +
    'Bonuses' + #13#10 +
    'B - + 1 bomb' + #13#10 +
    'R - Projectiles ricochets from you back to your enemies!' + #13#10 +
    'H - ' + Convert.ToString(HEALTH_BONUS) + ' health points' + #13#10+
    'T - Triple shot for your main weapon';
  PauseText.Visible := False;

  GameOverText := TglrText.Create();
  GameOverText.Position := dfVec3f(Render.Width / 2 - 200, 100, 30);
  GameOverText.Visible := False;

  AttentionText := TglrText.Create();
  AttentionText.Scale := 1.5;
  AttentionText.Position := dfVec3f(Render.Width / 2 - 200, 50, 25);
  AttentionText.Visible := False;

  PauseSprite := TglrSprite.Create(Render.Width, Render.Height, dfVec2f(0, 0));
  PauseSprite.SetVerticesColor(dfVec4f(0.1, 0.1, 0.1, 0.5));
  PauseSprite.Position.z := 25;
  PauseSprite.Visible := False;

  Camera := TglrCamera.Create();
  Camera.ProjectionMode := pmOrtho;
  Camera.ProjectionModePivot := pTopLeft;
  Camera.SetCamera(
    dfVec3f(0, 0, 100),
    dfVec3f(0, 0, 0),
    dfVec3f(0, 1, 0));
  Camera.Viewport(0, 0, Render.Width, Render.Height, 90, -1, 200);

  Player := TPlayer.Create();

  EnemyManager := TEnemyManager.Create(SpriteBatch);

  MusicVolume := 18;
  uFMOD_SetVolume(MusicVolume);

  uFMOD_PlaySong(@xm1, Length(xm1), XM_MEMORY);

  Pause := False;
  GameOver := False;
  EnemiesKilled := 0;
  PreviouslyKilled := 0;
  fAttT := 0;

  Scores := 0;
end;

procedure TGame.SetGameOver;
begin
  Pause := True;
  GameOver := True;
  PauseSprite.Visible := True;
  GameOverText.Text :=
    '        SCORES: ' + Convert.ToString(Scores) + #13#10 +
    '        KILLED: ' + Convert.ToString(EnemiesKilled) + ' enemies' + #13#10#13#10 +
    'Press "Enter" to start it all over again';
  GameOverText.Visible := True;
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

function TGame.GenerateTexture(aWidth, aHeight, aBorderSize: Integer): TglrTexture;
var
  m, m_origin: PByte;
  i, j: Integer;
  value: Byte;
begin
  m := GetMemory(aWidth * aHeight * 3);
  m_origin := m;
  for j := 0 to aHeight - 1 do
    for i := 0 to aWidth - 1 do
    begin
      if (i < aBorderSize) or (j < aBorderSize)
        or (i > aWidth - aBorderSize - 1) or (j > aHeight - aBorderSize - 1) then
        value := 196
      else
        if ((i + j) mod 16) >= 8 then
          value := 255
        else
          value := 196;
      m^ := value; m+=1;
      m^ := value; m+=1;
      m^ := value; m+=1;
    end;
  Result := TglrTexture.Create(m_origin, aWidth, aHeight, tfRGB8);
end;

procedure TGame.SetAttention(aText: WideString);
begin
  AttentionText.Text := aText;
  AttentionText.Visible := True;
  fAttT := ATTENTION_POPUP_TIME;
end;

procedure TGame.ParticleShells(aPos, aDir: TdfVec2f);
begin
  with ShellEmitter.GetNewParticle() do
  begin
    Position := dfVec3f(aPos + aDir* 5, 5);
    Rotation := dfVec2f(- aDir.y, aDir.x).GetRotationAngle() + Random(50);
    //SetVerticesColor(dfVec4f(Random(), Random(), Random(), 1.0));
    SetVerticesColor(dfVec4f(0.7,  0.6, 0.1, 1.0));
    Width := SHELL_WIDTH;
    Height := SHELL_HEIGHT;
    LifeTime := SHELL_LIFETIME;
    Velocity := aDir * (120 + Random(10));
  end;
end;

procedure TGame.OnFinish;
begin
  // Write here code for destroying all of your objects
  uFMOD_StopSong();

  EnemyManager.Free();
  Player.Free();

  Camera.Free();

  AttentionText.Free();
  GameOverText.Free();
  DebugText.Free();
  PauseText.Free();
  PauseSprite.Free();

  PopupManager.Free();
  BonusManager.Free();

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
  if (GameOver) then
  begin
    if (aType = itKeyDown) and (aKey = kEscape) then
      Core.Quit()
    else if (aType = itKeyDown) and (aKey = kReturn) then
    begin
      OnFinish();
      OnStart();
    end;
  end
  else
  begin
    if (aType = itKeyDown) and (aKey = kEscape) then
    begin
      Pause := not Pause;
      PauseText.Visible := Pause;
      PauseSprite.Visible := Pause;
    end;

    if (aType = itKeyDown) and (aKey = kSpace) then
      AutoFire := not AutoFire;

    if (aType = itTouchDown) and (aKey = kRightButton) then
      Player.FireAlternative();

    if (aType = itKeyDown) and (aKey = kPlus) then
    begin
      MusicVolume := Clamp(MusicVolume + 1, 0, uFMOD_MAX_VOL);
      uFMOD_SetVolume(MusicVolume);
    end
    else if (aType = itKeyDown) and (aKey = kMinus) then
    begin
      MusicVolume := Clamp(MusicVolume - 1, 0, uFMOD_MAX_VOL);
      uFMOD_SetVolume(MusicVolume);
    end;
  end;
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
  BonusManager.Update(dt);
  PopupManager.Update(dt);

  if (AutoFire) or (Core.Input.Touch[1].IsDown) then
    Player.Fire();

  if (fAttT > 0) then
  begin
    fAttT -= dt;
    AttentionText.Color.w := fAttT / ATTENTION_POPUP_TIME;
  end
  else
    AttentionText.Visible := False;

  DebugText.Text :=
//    'Health: ' + Convert.ToString(Player.Health) + #13#10 +
    'Scores: ' + Convert.ToString(Scores) + #13#10 +
    'Wave: ' + Convert.ToString(EnemyManager.WaveCount);
end;

procedure TGame.OnRender;
begin
  // It calls on every draw
  Camera.Update();
  MainMaterial.Bind();
  SpriteBatch.Start();
    SpriteBatch.Draw(Player);
    SpriteBatch.Draw(Player.Weapon);
    SpriteBatch.Draw(Player.FrontBumper);
  SpriteBatch.Finish();

  EnemyManager.RenderSelf();
  BulletManager.RenderSelf();

  BonusManager.RenderSelf();

  // Render ParticleEmitter
  MainMaterial.DepthWrite := False;
  MainMaterial.Bind();
  ParticleEmitter.RenderSelf();
  ShellEmitter.RenderSelf();
  MainMaterial.DepthWrite := True;
  MainMaterial.Bind();

  FontBatch.Start();
    FontBatch.Draw(DebugText);
    FontBatch.Draw(Player.BonusInfo);
    FontBatch.Draw(Player.HealthText);
    FontBatch.Draw(Player.AltWeaponText);
    FontBatch.Draw(AttentionText);
    FontBatch.Draw(PauseText);
    FontBatch.Draw(GameOverText);
  FontBatch.Finish();

  MainMaterial.Bind();
  SpriteBatch.Start();
    SpriteBatch.Draw(PauseSprite);
  SpriteBatch.Finish();

  PopupManager.RenderSelf();
end;

procedure TGame.OnPause;
begin
  // Calls when app has lost focus
  uFMOD_Pause();
  Pause := True;
  PauseText.Visible := True;
  PauseSprite.Visible := True;
end;

procedure TGame.OnResume;
begin
  // Calls when engine receives that app was focused
  uFMOD_Resume();
end;

procedure TGame.OnResize(aNewWidth, aNewHeight: Integer);
begin
  // Calls when window has changed size
end;


end.

