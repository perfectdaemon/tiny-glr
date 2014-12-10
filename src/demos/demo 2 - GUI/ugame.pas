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
    GuiAtlas: TglrTextureAtlas;
    Font: TglrFont;

    // Gui
    Gui: TglrGuiManager;
    Button1: TglrGuiButton;

    DebugText: TglrText;

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
  GuiAtlas := TglrTextureAtlas.Create(
    FileSystem.ReadResource('data/gui.tga'),
    FileSystem.ReadResource('data/gui.atlas'),
    'tga', 'cheetah');
  Material := TglrMaterial.Create(Default.SpriteShader);
  Material.AddTexture(GuiAtlas, 'uDiffuse');

  Font := TglrFont.Create(FileSystem.ReadResource('data/Arial14b.bmp'));

  FontBatch := TglrFontBatch.Create(Font);
  SpriteBatch := TglrSpriteBatch.Create();

  Button1 := TglrGuiButton.Create();
  Button1.SetTextureRegion(GuiAtlas.GetRegion('button.png'));
  Button1.Position := Vec3f(300, 200, 5);
  Button1.Rotation := -15;
  Button1.OnMouseOver := ButtonMouseOver;
  Button1.OnMouseOut := ButtonMouseOut;
  Button1.OnClick := ButtonClicked;

  DebugText := TglrText.Create(UTF8Decode('Проверка'));
  DebugText.Position := Vec3f(10, 10, 10);

  Gui := TglrGuiManager.Create();
  Gui.Elements.Add(Button1);

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
  Sender.SetVerticesColor(Vec4f(1.0, 0.5, 0.5, 1.0));
end;

procedure TGame.ButtonMouseOut(Sender: TglrGuiElement; aType: TglrInputType;
  aKey: TglrKey; X, Y: Single; aOtherParam: Integer);
begin
  Sender.SetVerticesColor(Vec4f(1.0, 1.0, 1.0, 1.0));
end;

procedure TGame.ButtonClicked(Sender: TglrGuiElement; aType: TglrInputType;
  aKey: TglrKey; X, Y: Single; aOtherParam: Integer);
begin
  DebugText.Text := UTF8Decode('Clicked!');
end;

procedure TGame.OnFinish;
begin
  // Write here code for destroying all of your objects
  Gui.Free();
  Camera.Free();

  SpriteBatch.Free();
  FontBatch.Free();

  Material.Free();
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

  if (aType = itTouchDown) and (aKey = kLeftButton) then
  begin
    touchVec := Vec3f(X, Y, 0);
    DebugText.Text := 'Mouse coords: '#13#10 + Convert.ToString(touchVec) + #13#10
      + 'Proj coords: '#13#10 + Convert.ToString(Camera.Matrix * touchVec);
  end;
end;

procedure TGame.OnUpdate(const dt: Double);
var
  moveVec: TglrVec2f;
begin
  // Place here game logic code
  moveVec.Reset();
  if (Core.Input.KeyDown[kLeft]) then
    moveVec.x -= 1
  else if (Core.Input.KeyDown[kRight]) then
    moveVec.x += 1;

  if (Core.Input.KeyDown[kUp]) then
    moveVec.y -= 1
  else if (Core.Input.KeyDown[kDown]) then
    moveVec.y += 1;


  Camera.Position += Vec3f(moveVec.Normal * 250 * dt, 0);
end;

procedure TGame.OnRender;
begin
  Camera.Update();
  Material.Bind();
  SpriteBatch.Start();
    SpriteBatch.Draw(Button1);
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

