unit uGame;

interface

uses
  glr_core,
  glr_render,
  glr_render2d,
  glr_scene,
  glr_filesystem,
  glr_gui,
  glr_math,
  glr_gamescreens;

type

  { TGame }

  TGame = class (TglrGame)
  private
    GameScreenManager: TglrGameScreenManager;
    MainMenuScreen, SettingsScreen, GameScreen: TglrGameScreen;
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

uses
  uGSMainMenu,
  uAssets;

{ TGame }

procedure TGame.OnStart;
begin
  // Write here initialization code
  Assets.LoadBase();

  MainMenuScreen := TglrMainMenu.Create('MainMenu');

  GameScreenManager := TglrGameScreenManager.Create(3);
  GameScreenManager.Add(MainMenuScreen);


  GameScreenManager.SwitchTo(MainMenuScreen);
  (*

  Render.SetClearColor(0.1, 0.25, 0.25);

  GuiAtlas := TglrTextureAtlas.Create(
    FileSystem.ReadResource('data/gui.tga'),
    FileSystem.ReadResource('data/gui.atlas'),
    extTga, aextCheetah);
  Material := TglrMaterial.Create(Default.SpriteShader);
  Material.AddTexture(GuiAtlas, 'uDiffuse');

  DefaultMaterial := TglrMaterial.Create(Default.SpriteShader);
  DefaultMaterial.AddTexture(Default.BlankTexture, 'uDiffuse');

  Font := TglrFont.Create(FileSystem.ReadResource('data/Arial14b.bmp'));

  FontBatch := TglrFontBatch.Create(Font);
  SpriteBatch := TglrSpriteBatch.Create();

  Button1 := TglrGuiButton.Create();
  Button1.NormalTextureRegion := GuiAtlas.GetRegion('button.png');
  Button1.OverTextureRegion := GuiAtlas.GetRegion('button_over.png');
  Button1.SetTextureRegion(GuiAtlas.GetRegion('button.png'));
  Button1.SetVerticesColor(Vec4f(0.4, 0.75, 0.6, 1.0));
  Button1.Position := Vec3f(300, 200, 5);
  Button1.Rotation := -15;
  Button1.OnMouseOver := ButtonMouseOver;
  Button1.OnMouseOut := ButtonMouseOut;
  Button1.OnClick := ButtonClicked;
  Button1.Text.Text := 'Click me';
  Button1.Text.Position.z := 1;

  Layout1 := TglrGuiLayout.Create(100, 100, Vec2f(0.5, 0.5));
  Layout1.SetNinePatchBorders(0.4, 0.6, 0.4, 0.6);
  Layout1.SetTextureRegion(GuiAtlas.GetRegion('layout_test.png'));
  Layout1.Position := Vec3f(500, 150, 4);

  DebugText := TglrText.Create(UTF8Decode('Проверка очень длинного и малосвязного текста с оченьдлинныминеразрывными словами и прочим'));
  DebugText.Position := Vec3f(350, 250, 10);
  DebugText.TextWidth := 200;

  DebugTextPivotPointSprite := TglrSprite.Create(5, 5, Vec2f(0.5, 0.5));
  DebugTextPivotPointSprite.Position := Vec3f(0, 0, 10);
  DebugTextPivotPointSprite.SetVerticesColor(Vec4f(1.0, 0.0, 0.0, 1.0));
  DebugTextPivotPointSprite.Parent := Layout1;

  DebugTextWidthSprite := TglrSprite.Create(20, 5, Vec2f(0.5, 0.5));
  DebugTextWidthSprite.Position := Vec3f(0, 0, -1);
  DebugTextWidthSprite.SetVerticesColor(Vec4f(1.0, 1.0, 1.0, 0.5));
  DebugTextWidthSprite.Parent := DebugText;

  Gui := TglrGuiManager.Create();
  Gui.Elements.Add(Button1);
  Gui.Elements.Add(Layout1);

  Camera := TglrCamera.Create();
  Camera.ProjectionMode := pmOrtho;
  Camera.ProjectionModePivot := pTopLeft;
  Camera.SetCamera(
    Vec3f(0, 0, 100),
    Vec3f(0, 0, 0),
    Vec3f(0, 1, 0));
  Camera.Viewport(0, 0, Render.Width, Render.Height, 90, -1, 200);
  *)
end;

procedure TGame.OnFinish;
begin
  // Write here code for destroying all of your objects
  GameScreenManager.Free(True);
  Assets.UnloadBase();
end;

procedure TGame.OnInput(aType: TglrInputType; aKey: TglrKey; X, Y,
  aOtherParam: Integer);
begin
  // Calls when engine receives some input info
  GameScreenManager.Input(aType, aKey, X, Y, aOtherParam);
end;

procedure TGame.OnUpdate(const dt: Double);
begin
  GameScreenManager.Update(dt);
end;

procedure TGame.OnRender;
begin
  GameScreenManager.Render();
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

