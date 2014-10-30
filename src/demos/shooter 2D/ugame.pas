unit uGame;

interface

uses
  tinyglr, glrMath;

type

  { TPlayer }

  TPlayer = class
    Sprite: TglrSprite;

    constructor Create();
    destructor Destroy();

    procedure Update(const dt: Double);
  end;

  { TGame }

  TGame = class (TglrGame)
  private
    SpriteBatch: TglrSpriteBatch;
    FontBatch: TglrFontBatch;

    Font: TglrFont;

    MainMaterial: TglrMaterial;
    Particles: TglrCustomParticleEmitter2D;

    DebugText: TglrText;
    SceneHud: TglrScene;
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

{ TPlayer }

constructor TPlayer.Create;
begin
  inherited;
  Sprite := TglrSprite.Create(15, 15, pCenter);
  Sprite.
end;

destructor TPlayer.Destroy;
begin
  Sprite.Free();
  inherited;
end;

procedure TPlayer.Update(const dt: Double);
begin

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
end;

procedure TGame.OnFinish;
begin
  // Write here code for destroying all of your objects
  DebugText.Free();
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
begin
  // Place here game logic code

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

  Particles.Update(dt);
end;

procedure TGame.OnRender;
begin
  // It calls on every draw
  SceneHud.RenderScene();
  FontBatch.Start();
    FontBatch.Draw(DebugText);
  FontBatch.Finish();

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

