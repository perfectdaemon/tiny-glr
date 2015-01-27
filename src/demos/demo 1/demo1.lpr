program demo1;

uses
  tinyglr, glrMath, ogl, sys_win;

type

  { TGame }

  TGame = class(TglrGame)
  protected
    dx, dy: Integer;
    Camera, CameraHud: TglrCamera;
    Material: TglrMaterial;

    meshData: array of TglrVertexP3T2N3;
    meshBuffer: TglrVertexBuffer;
    meshIBuffer: TglrIndexBuffer;

    Sprites: array of TglrSprite;
    Batch: TglrSpriteBatch;

    Font: TglrFont;
    Text: TglrText;
    FontBatch: TglrFontBatch;
    atlas: TglrTextureAtlas;

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
  meshData[0].vec := Vec3f(CubeSize/2, CubeSize/2, CubeSize/2);
  meshData[1].vec := Vec3f(-CubeSize/2, CubeSize/2, CubeSize/2);
  meshData[2].vec := Vec3f(CubeSize/2, -CubeSize/2, CubeSize/2);
  meshData[3].vec := Vec3f(-CubeSize/2, -CubeSize/2, CubeSize/2);

  meshData[4].vec := Vec3f(CubeSize/2, CubeSize/2, -CubeSize/2);
  meshData[5].vec := Vec3f(-CubeSize/2, CubeSize/2, -CubeSize/2);
  meshData[6].vec := Vec3f(CubeSize/2, -CubeSize/2, -CubeSize/2);
  meshData[7].vec := Vec3f(-CubeSize/2, -CubeSize/2, -CubeSize/2);

  meshData[8].vec := Vec3f(CubeSize/2, CubeSize/2, CubeSize/2); //0
  meshData[9].vec := Vec3f(CubeSize/2, CubeSize/2, -CubeSize/2); //4
  meshData[10].vec := Vec3f(CubeSize/2, -CubeSize/2, -CubeSize/2); //6
  meshData[11].vec := Vec3f(CubeSize/2, -CubeSize/2, CubeSize/2); //2

  meshData[12].vec := Vec3f(-CubeSize/2, CubeSize/2, CubeSize/2); //1
  meshData[13].vec := Vec3f(-CubeSize/2, -CubeSize/2, CubeSize/2); //3
  meshData[14].vec := Vec3f(-CubeSize/2, CubeSize/2, -CubeSize/2); //5
  meshData[15].vec := Vec3f(-CubeSize/2, -CubeSize/2, -CubeSize/2); //7

  meshData[16].vec := Vec3f(CubeSize/2, CubeSize/2, CubeSize/2); //0
  meshData[17].vec := Vec3f(-CubeSize/2, CubeSize/2, CubeSize/2); //1
  meshData[18].vec := Vec3f(CubeSize/2, CubeSize/2, -CubeSize/2); //4
  meshData[19].vec := Vec3f(-CubeSize/2, CubeSize/2, -CubeSize/2); //5

  meshData[20].vec := Vec3f(CubeSize/2, -CubeSize/2, CubeSize/2); //2
  meshData[21].vec := Vec3f(-CubeSize/2, -CubeSize/2, CubeSize/2); //3
  meshData[22].vec := Vec3f(CubeSize/2, -CubeSize/2, -CubeSize/2); //6
  meshData[23].vec := Vec3f(-CubeSize/2, -CubeSize/2, -CubeSize/2); //7

  meshData[0].tex := Vec2f(1, 1);
  meshData[1].tex := Vec2f(0, 1);
  meshData[2].tex := Vec2f(1, 0);
  meshData[3].tex := Vec2f(0, 0);

  meshData[4].tex := Vec2f(0, 1);
  meshData[5].tex := Vec2f(1, 1);
  meshData[6].tex := Vec2f(0, 0);
  meshData[7].tex := Vec2f(1, 0);

  meshData[8].tex := Vec2f(0, 1);
  meshData[9].tex := Vec2f(1, 1);
  meshData[10].tex := Vec2f(1, 0);
  meshData[11].tex := Vec2f(0, 0);

  meshData[12].tex := Vec2f(1, 1);
  meshData[13].tex := Vec2f(1, 0);
  meshData[14].tex := Vec2f(0, 1);
  meshData[15].tex := Vec2f(0, 0);

  meshData[16].tex := Vec2f(1, 0);
  meshData[17].tex := Vec2f(0, 0);
  meshData[18].tex := Vec2f(1, 1);
  meshData[19].tex := Vec2f(0, 1);

  meshData[20].tex := Vec2f(1, 1);
  meshData[21].tex := Vec2f(0, 1);
  meshData[22].tex := Vec2f(1, 0);
  meshData[23].tex := Vec2f(0, 0);

  meshBuffer := TglrVertexBuffer.Create(@meshData[0], 24, vfPos3Tex2Nor3, uStaticDraw);
  meshIBuffer := TglrIndexBuffer.Create(@indices[0], 36, ifByte);
end;

procedure TGame.RenderMesh;
begin
  Render.DrawTriangles(meshBuffer, meshIBuffer, 0, 36);
end;

procedure TGame.CreateSprites;
const
  count = 30;
var
  i: Integer;
begin
  atlas := TglrTextureAtlas.Create(FileSystem.ReadResource('data/atlas.tga'),
  FileSystem.ReadResource('data/atlas.atlas'), extTga, aextCheetah);

  Batch := TglrSpriteBatch.Create();
  SetLength(Sprites, count);
  for i := 0 to count - 1 do
  begin
    Sprites[i] := TglrSprite.Create(30, 30, Vec2f(0.5, 0.5));
    Sprites[i].Position := Vec3f(Random(800), Random(600), Random(15));
    Sprites[i].SetVerticesColor(Vec4f(Random(), Random(), Random, 1));
    Sprites[i].SetTextureRegion(atlas.GetRegion('goodline.png'));
  end;
end;

procedure TGame.CreateFont;
begin
  Font := TglrFont.Create(FileSystem.ReadResource('data/Arial14b.bmp'));
  Text := TglrText.Create(UTF8Decode('Hello, world! / Привет, мир! ' + #13#10 +
    'This time it will go over platforms... / На этот раз все будет кроссплатформенно...'));
  Text.LetterSpacing := 1;
  Text.Position := Vec3f(10, 150, 90);
  FontBatch := TglrFontBatch.Create(Font);
end;

procedure TGame.OnFinish;
var
  i: Integer;
begin
  for i := 0 to Length(Sprites) - 1 do
    Sprites[i].Free();
  Font.Free();
  Text.Free();
  FontBatch.Free();
  Batch.Free();
  atlas.Free();
  Material.Free();
  Camera.Free();
  CameraHud.Free();
  WriteLn('End');
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
    Camera.Rotate((x - dx) * deg2rad, Vec3f(0, 1, 0));
    Camera.Rotate((y - dy) * deg2rad, Camera.Right);
    dx := X;
    dy := Y;
  end;

  if (aType = itTouchMove) and (aKey = kNoInput) then
  begin
    Sprites[1].Up := (Sprites[1].Position - Vec3f(Core.Input.Touch[0].Pos, 0)).Normal;
    Sprites[1].Direction := Vec3f(0, 0.0, 1.0);
  end;

  if (aType = itWheel) then
    Camera.Translate(0, 0, -Sign(aOtherParam));
  if (aType = itKeyUp) and (aKey = kU) then
    log.Write(lInformation, 'Camera.Mat: '#13#10 + Convert.ToString(Camera.Matrix, 2));
end;

procedure TGame.OnPause;
begin
  WriteLn('Pause');
end;

procedure TGame.OnRender;
begin
  Material.Bind();
  Camera.Update();
  RenderMesh();

  CameraHud.Update();
  Batch.Start();
  Batch.Draw(Sprites);
  Batch.Finish();
  FontBatch.Start();
  FontBatch.Draw(Text);
  FontBatch.Finish();
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

  Camera := TglrCamera.Create();
  Camera.SetCamera(Vec3f(5, 0, 5), Vec3f(0, 0, 0), Vec3f(0, 1, 0));
  Camera.ProjectionMode := pmPerspective;

  CameraHud := TglrCamera.Create();
  CameraHud.SetCamera(Vec3f(0, 0, 100), Vec3f(0, 0, 0), Vec3f(0, 1, 0));
  CameraHud.ProjectionMode := pmOrtho;

  Material := TglrMaterial.Create(Default.SpriteShader);
//  Material.AddTexture(TglrTexture.Create(FileSystem.ReadResource('Arial12b.bmp'), 'bmp'), 'uDiffuse');
//  Material.AddTexture(TglrTexture.Create(FileSystem.ReadResource('data/box.tga'), 'tga'), 'uDiffuse');
//  Material.Color := dfVec4f(0.7, 0.2, 0.1, 1);

  PrepareMesh();
  CreateSprites();
  CreateFont();
  Material.AddTexture(atlas, 'uDiffuse');
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

