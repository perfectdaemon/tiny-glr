program demo1;

uses
  tinyglr, glrMath, ogl, sys_win;

type

  { TGame }

  TGame = class(TglrGame)
  protected
    dx, dy: Integer;
    Scene, SceneHud: TglrScene;
    Material: TglrMaterial;

    meshData: array of TglrVertexP3T2N3;
    meshBuffer: TglrVertexBuffer;
    meshIBuffer: TglrIndexBuffer;

    Sprites: array of TglrSprite;
    Batch: TglrSpriteBatch;

    Font: TglrFont;
    Text: TglrText;
    FontBatch: TglrFontBatch;

    procedure PrepareMesh();
    procedure RenderMesh();
    procedure CreateSprites();
    procedure CreateFont();
  public
    procedure OnFinish; override;
    procedure OnInput(aType: TglrInputType; aKey: TglrKey; X, Y,
      aOtherParam: Integer); override;
    procedure OnPause; override;
    procedure OnRender; override;
    procedure OnResume; override;
    procedure OnResize(aNewWidth, aNewHeight: Integer); override;
    procedure OnStart; override;
    procedure OnUpdate(const dt: Double); override;
  end;

{ TGame }

procedure TGame.PrepareMesh;
const
  CubeSize = 4;

  indices: array[0..35] of byte = (0, 1, 3, //back
                                   0, 3, 2,

                                   4, 6, 7, //front
                                   7, 5, 4,

                                   9, 8, 10, //right
                                   8, 11, 10,

                                   15, 12, 14, //left
                                   15, 13, 12,

                                   19, 17, 16, //top
                                   19, 16, 18,

                                   23, 20, 21, //bottom
                                   23, 22, 20);
begin
  SetLength(meshData, 24);
  meshData[0].vec := dfVec3f(CubeSize/2, CubeSize/2, CubeSize/2);
  meshData[1].vec := dfVec3f(-CubeSize/2, CubeSize/2, CubeSize/2);
  meshData[2].vec := dfVec3f(CubeSize/2, -CubeSize/2, CubeSize/2);
  meshData[3].vec := dfVec3f(-CubeSize/2, -CubeSize/2, CubeSize/2);

  meshData[4].vec := dfVec3f(CubeSize/2, CubeSize/2, -CubeSize/2);
  meshData[5].vec := dfVec3f(-CubeSize/2, CubeSize/2, -CubeSize/2);
  meshData[6].vec := dfVec3f(CubeSize/2, -CubeSize/2, -CubeSize/2);
  meshData[7].vec := dfVec3f(-CubeSize/2, -CubeSize/2, -CubeSize/2);

  meshData[8].vec := dfVec3f(CubeSize/2, CubeSize/2, CubeSize/2); //0
  meshData[9].vec := dfVec3f(CubeSize/2, CubeSize/2, -CubeSize/2); //4
  meshData[10].vec := dfVec3f(CubeSize/2, -CubeSize/2, -CubeSize/2); //6
  meshData[11].vec := dfVec3f(CubeSize/2, -CubeSize/2, CubeSize/2); //2

  meshData[12].vec := dfVec3f(-CubeSize/2, CubeSize/2, CubeSize/2); //1
  meshData[13].vec := dfVec3f(-CubeSize/2, -CubeSize/2, CubeSize/2); //3
  meshData[14].vec := dfVec3f(-CubeSize/2, CubeSize/2, -CubeSize/2); //5
  meshData[15].vec := dfVec3f(-CubeSize/2, -CubeSize/2, -CubeSize/2); //7

  meshData[16].vec := dfVec3f(CubeSize/2, CubeSize/2, CubeSize/2); //0
  meshData[17].vec := dfVec3f(-CubeSize/2, CubeSize/2, CubeSize/2); //1
  meshData[18].vec := dfVec3f(CubeSize/2, CubeSize/2, -CubeSize/2); //4
  meshData[19].vec := dfVec3f(-CubeSize/2, CubeSize/2, -CubeSize/2); //5

  meshData[20].vec := dfVec3f(CubeSize/2, -CubeSize/2, CubeSize/2); //2
  meshData[21].vec := dfVec3f(-CubeSize/2, -CubeSize/2, CubeSize/2); //3
  meshData[22].vec := dfVec3f(CubeSize/2, -CubeSize/2, -CubeSize/2); //6
  meshData[23].vec := dfVec3f(-CubeSize/2, -CubeSize/2, -CubeSize/2); //7

  meshData[0].tex := dfVec2f(1, 1);
  meshData[1].tex := dfVec2f(0, 1);
  meshData[2].tex := dfVec2f(1, 0);
  meshData[3].tex := dfVec2f(0, 0);

  meshData[4].tex := dfVec2f(0, 1);
  meshData[5].tex := dfVec2f(1, 1);
  meshData[6].tex := dfVec2f(0, 0);
  meshData[7].tex := dfVec2f(1, 0);

  meshData[8].tex := dfVec2f(0, 1);
  meshData[9].tex := dfVec2f(1, 1);
  meshData[10].tex := dfVec2f(1, 0);
  meshData[11].tex := dfVec2f(0, 0);

  meshData[12].tex := dfVec2f(1, 1);
  meshData[13].tex := dfVec2f(1, 0);
  meshData[14].tex := dfVec2f(0, 1);
  meshData[15].tex := dfVec2f(0, 0);

  meshData[16].tex := dfVec2f(1, 0);
  meshData[17].tex := dfVec2f(0, 0);
  meshData[18].tex := dfVec2f(1, 1);
  meshData[19].tex := dfVec2f(0, 1);

  meshData[20].tex := dfVec2f(1, 1);
  meshData[21].tex := dfVec2f(0, 1);
  meshData[22].tex := dfVec2f(1, 0);
  meshData[23].tex := dfVec2f(0, 0);

  meshBuffer := TglrVertexBuffer.Create(@meshData[0], 24, vfPos3Tex2Nor3);
  meshIBuffer := TglrIndexBuffer.Create(@indices[0], 36, ifByte);
end;

procedure TGame.RenderMesh;
begin
  Render.DrawTriangles(meshBuffer, meshIBuffer, 0, 36);
end;

procedure TGame.CreateSprites;
const
  count = 300;
var
  i: Integer;
begin
  Batch := TglrSpriteBatch.Create();
  Batch.Material := Material;
  SceneHud.Root.Childs.Add(Batch);
  SetLength(Sprites, count);
  for i := 0 to count - 1 do
  begin
    Sprites[i] := TglrSprite.Create(30, 30, dfVec2f(0.5, 0.5));
    Sprites[i].Position := dfVec3f(Random(800), Random(600), Random(5));
    Sprites[i].SetVerticesColor(dfVec4f(Random(), Random(), Random, 1));
    Batch.Childs.Add(Sprites[i]);
  end;
end;

procedure TGame.CreateFont;
begin
  Font := TglrFont.Create(FileSystem.ReadResource('data/Arial14b.bmp'));
  Text := TglrText.Create('Hello, world!');
  Text.LetterSpacing := 2;
  Text.Position := dfVec3f(200, 150, 90);
  FontBatch := TglrFontBatch.Create(Font);
  FontBatch.Childs.Add(Text);
  SceneHud.Root.Childs.Add(FontBatch);
end;

procedure TGame.OnFinish;
begin
  WriteLn('End');
  Scene.Free();
end;

procedure TGame.OnInput(aType: TglrInputType; aKey: TglrKey; X, Y,
  aOtherParam: Integer);
begin
  if (aType = itTouchDown) and (aKey = kLeftButton) then
  begin
    dx := X;
    dy := Y;
  end;

  if (aType = itTouchMove) and (aKey = kLeftButton) then
  begin
    Scene.Camera.Rotate((x - dx) * deg2rad, dfVec3f(0, 1, 0));
    Scene.Camera.Rotate((y - dy) * deg2rad, Scene.Camera.Right);
    dx := X;
    dy := Y;
  end;

  if (aType = itTouchMove) and (aKey = kNoInput) then
  begin
    Sprites[1].Up := (Sprites[1].Position - dfVec3f(Core.Input.Touch[0].Pos, 0)).Normal;
    Sprites[1].Direction := dfVec3f(0, 0.0, 1.0);
  end;

  if (aType = itWheel) then
    Scene.Camera.Translate(0, 0, -Sign(aOtherParam));
  if (aType = itKeyUp) and (aKey = kU) then
    log.Write(lInformation, 'Camera.Up: ' + Convert.ToString(Scene.Camera.Up));
end;

procedure TGame.OnPause;
begin
  WriteLn('Pause');
end;

procedure TGame.OnRender;
begin
  Scene.RenderScene();

  Material.Bind();
  RenderMesh();
  Material.Unbind();

  SceneHud.RenderScene();
end;

procedure TGame.OnResume;
begin
  WriteLn('Resume');
end;

procedure TGame.OnResize(aNewWidth, aNewHeight: Integer);
begin
  WriteLn('Resize');
end;

procedure TGame.OnStart;
begin
  WriteLn('Start');

  Render.SetCullMode(cmBack);

  Scene := TglrScene.Create(True);
  Scene.Camera.SetCamera(dfVec3f(5, 0, 5), dfVec3f(0, 0, 0), dfVec3f(0, 1, 0));
  Scene.Camera.ProjectionMode := pmPerspective;

  SceneHud := TglrScene.Create(True);
  //SceneHud.Camera := Scene.Camera;
  SceneHud.Camera.ProjectionMode := pmOrtho;
  SceneHud.Camera.SetCamera(dfVec3f(0, 0, 100), dfVec3f(0, 0, 0), dfVec3f(0, 1, 0));

  Material := TglrMaterial.Create();
  Material.Shader.Free();
  Material.Shader := Default.SpriteShader;
//  Material.AddTexture(TglrTexture.Create(FileSystem.ReadResource('Arial12b.bmp'), 'bmp'), 'uDiffuse');
  Material.AddTexture(TglrTexture.Create(FileSystem.ReadResource('data/box.tga'), 'tga'), 'uDiffuse');
//  Material.Color := dfVec4f(0.7, 0.2, 0.1, 1);

  PrepareMesh();
  CreateSprites();
  CreateFont();
end;

procedure TGame.OnUpdate(const dt: Double);
var
  i: Integer;
begin
  for i := 0 to Length(Sprites) - 1 do
    Sprites[i].Rotation := Sprites[i].Rotation + dt * Sprites[i].Position.y / 10;
end;

var
  Game: TGame;
  InitParams: TglrInitParams;

begin
  with InitParams do
  begin
    Width := 800;
    Height := 600;
    X := 100;
    Y := 100;
    Caption := 'tiny glr [' + TINYGLR_VERSION + ']';
    vSync := True;
    PackFilesPath := '';
    UseDefaultAssets := True;
  end;

  Game := TGame.Create();
  Core.Init(Game, InitParams);
  Core.Loop();
  Core.DeInit();
  Game.Free();
end.

