unit uGame;

interface

uses
  tinyglr, glrMath;

type

  { TGame }

  TGame = class (TglrGame)
  private
    // Resources
    Material: TglrMaterial;
    DefaultMaterial: TglrMaterial;
    GuiAtlas: TglrTextureAtlas;
    Font: TglrFont;

    // Gui
    Gui: TglrGuiManager;
    Button1: TglrGuiButton;
    Layout1: TglrGuiLayout;

    // Text checks
    DebugText: TglrText;
    DebugTextPivotPointSprite, DebugTextWidthSprite: TglrSprite;

    // Camera
    Camera: TglrCamera;

    // Batches
    SpriteBatch: TglrSpriteBatch;
    FontBatch: TglrFontBatch;

    procedure ButtonMouseOver(Sender: TglrGuiElement;
      aType: TglrInputType;
      aKey: TglrKey;
      X, Y: Single; aOtherParam: Integer);
    procedure ButtonMouseOut(Sender: TglrGuiElement;
      aType: TglrInputType;
      aKey: TglrKey;
      X, Y: Single; aOtherParam: Integer);
    procedure ButtonClicked(Sender: TglrGuiElement;
      aType: TglrInputType;
      aKey: TglrKey;
      X, Y: Single; aOtherParam: Integer);
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

{ TGame }

procedure TGame.OnStart;
begin
  // Write here initialization code
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
end;

procedure TGame.ButtonMouseOver(Sender: TglrGuiElement; aType: TglrInputType;
  aKey: TglrKey; X, Y: Single; aOtherParam: Integer);
begin
  Sender.SetVerticesColor(Vec4f(1.0, 0.7, 0.6, 1.0));
end;

procedure TGame.ButtonMouseOut(Sender: TglrGuiElement; aType: TglrInputType;
  aKey: TglrKey; X, Y: Single; aOtherParam: Integer);
begin
  Sender.SetVerticesColor(Vec4f(0.4, 0.75, 0.6, 1.0));
end;

procedure TGame.ButtonClicked(Sender: TglrGuiElement; aType: TglrInputType;
  aKey: TglrKey; X, Y: Single; aOtherParam: Integer);
begin
  //DebugText.Text := UTF8Decode('Clicked!');
end;

procedure TGame.OnFinish;
begin
  // Write here code for destroying all of your objects
  Gui.Free();
  Camera.Free();

  DebugText.Free();
  DebugTextPivotPointSprite.Free();
  DebugTextWidthSprite.Free();

  SpriteBatch.Free();
  FontBatch.Free();

  Material.Free();
  DefaultMaterial.Free();
  GuiAtlas.Free();
  Font.Free();
end;

procedure TGame.OnInput(aType: TglrInputType; aKey: TglrKey; X, Y,
  aOtherParam: Integer);
var
  touchVec: TglrVec3f;
begin
  // Calls when engine receives some input info
  Gui.ProcessInput(aType, aKey, X, Y, aOtherParam, Camera);

  if aType = itKeyDown then
    case aKey of
      kLeft:
        DebugText.PivotPoint.x := Min(DebugText.PivotPoint.x + 0.5, 1.0);
      kRight:
        DebugText.PivotPoint.x := Max(DebugText.PivotPoint.x - 0.5, 0);
      kUp:
        DebugText.PivotPoint.y := Min(DebugText.PivotPoint.y + 0.5, 1.0);
      kDown:
        DebugText.PivotPoint.y := Max(DebugText.PivotPoint.y - 0.5, 0);

      kPlus:
        DebugText.TextWidth := DebugText.TextWidth + 5.0;
      kMinus:
        DebugText.TextWidth := DebugText.TextWidth - 5.0;

      kA:
        Layout1.Width := Layout1.Width - 5.0;
      kD:
        Layout1.Width := Layout1.Width + 5.0;
      kW:
        Layout1.Height := Layout1.Height + 5.0;
      ks:
        Layout1.Height := Layout1.Height - 5.0;
    end
end;

procedure TGame.OnUpdate(const dt: Double);
var
  moveVec: TglrVec2f;
begin
  // Place here game logic code
  {moveVec.Reset();
  if (Core.Input.KeyDown[kLeft]) then
    moveVec.x -= 1
  else if (Core.Input.KeyDown[kRight]) then
    moveVec.x += 1;

  if (Core.Input.KeyDown[kUp]) then
    moveVec.y -= 1
  else if (Core.Input.KeyDown[kDown]) then
    moveVec.y += 1;


  Camera.Position += Vec3f(moveVec.Normal * 250 * dt, 0);
  }
  Button1.Rotation := Button1.Rotation + dt * 10;
  DebugTextWidthSprite.Width := DebugText.TextWidth;
  DebugTextWidthSprite.PivotPoint := DebugText.PivotPoint;
end;

procedure TGame.OnRender;
begin
  Camera.Update();
  Material.Bind();
  SpriteBatch.Start();
    SpriteBatch.Draw(Layout1.Patches);
    SpriteBatch.Draw(Layout1);
    SpriteBatch.Draw(Button1);
  SpriteBatch.Finish();

  DefaultMaterial.Bind();
  SpriteBatch.Start();
    SpriteBatch.Draw(DebugTextPivotPointSprite);
    SpriteBatch.Draw(DebugTextWidthSprite);
  SpriteBatch.Finish();

  FontBatch.Start();
    FontBatch.Draw(Button1.Text);
    FontBatch.Draw(DebugText);
  FontBatch.Finish();
  // It calls on every draw
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

