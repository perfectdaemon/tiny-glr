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

  { TGame }

  TGame = class (TglrGame)
  private
    SpriteBatch: TglrSpriteBatch;
    FontBatch: TglrFontBatch;

    Font: TglrFont;

    MainMaterial: TglrMaterial;
    Particles: TglrCustomParticleEmitter2D;
    Bullets: TBulletManager;

    DebugText: TglrText;
    SceneHud: TglrScene;

    Player: TPlayer;
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

implementation

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
  i: Integer;
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
          // todo: check collisions
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

  Weapon := TglrSprite.Create(25, 8, dfVec2f(0.0, 0.5));
  Weapon.SetVerticesColor(dfVec4f(1, 1, 1, 1.0));
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
    b := Game.Bullets.GetNewBullet();
    b.BType := bSimple;
    b.Owner := bPlayer;

    bulletDir := WeaponDir +
      // Add dispersion
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
  Font := TglrFont.Create(FileSystem.ReadResourceLZO('shooter/Hattori Hanzo17b_.bmp', False));

  MainMaterial := TglrMaterial.Create();
  MainMaterial.Shader.Free();
  MainMaterial.Shader := Default.SpriteShader;
  MainMaterial.AddTexture(Default.BlankTexture, 'uDiffuse');

  SpriteBatch := TglrSpriteBatch.Create();
  FontBatch := TglrFontBatch.Create(Font);

  Particles := TglrCustomParticleEmitter2D.Create(SpriteBatch, MainMaterial);

  Bullets := TBulletManager.Create(SpriteBatch, MainMaterial);

  DebugText := TglrText.Create(UTF8Decode('Wow! Проверка текста!'));
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

  Render.SetClearColor(43 / 255, 99 / 255, 147 / 255);
end;

procedure TGame.OnFinish;
begin
  // Write here code for destroying all of your objects
  Player.Free();

  SceneHud.Free();

  DebugText.Free();

  Bullets.Free();
  Particles.Free();

  SpriteBatch.Free();
  FontBatch.Free();

  MainMaterial.Free();
  Font.Free();
end;

procedure TGame.OnInput(aType: TglrInputType; aKey: TglrKey; X, Y,
  aOtherParam: Integer);
begin
  // Calls when engine receives some input info
end;

procedure TGame.OnUpdate(const dt: Double);
var
  axisX, axisY: Integer;
begin
  // Place here game logic code

  Particles.Update(dt);
  Bullets.Update(dt);


  if Particles.ActiveParticlesCount < 32 then
    with Particles.GetNewParticle() do
    begin
      Position := dfVec3f(300, 300, 5);
      Position.z += 1;
      Position += dfVec3f(10 - Random(20), 10 - Random(20), Random(3));
      Rotation := Random(180);
      //SetVerticesColor(dfVec4f(Random(), Random(), Random(), 1.0));
      SetVerticesColor(dfVec4f(1.0, 0.5 * Random(), 0.3 * Random(), 1.0));
      Width := 1 + 5 * Random();
      Height := Width;
      LifeTime := 1.5;
      Velocity := dfVec2f(Random(360)) * (120 + Random(40));
    end;


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
    SpriteBatch.Draw(Player.Hero);
    SpriteBatch.Draw(Player.Weapon);
  SpriteBatch.Finish();

  Bullets.RenderSelf();

  // Render particles
  MainMaterial.DepthWrite := False;
  MainMaterial.Bind();
  Particles.RenderSelf();
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

