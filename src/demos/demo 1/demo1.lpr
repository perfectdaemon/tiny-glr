program demo1;

uses
  tinyglr, glrMath, ogl, sys_win;

type

  { TGame }

  TGame = class(TglrGame)
  protected
    dx, dy: Integer;
    Scene: TglrScene;
    Points: array of TdfVec3f;
    Tex: TglrTexture;

    meshData: array of TglrVertexP3T2;
    meshBuffer: TglrVertexBuffer;
    meshIBuffer: TglrIndexBuffer;
    procedure PrepareMesh();
    procedure RenderMesh();
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

  meshBuffer := TglrVertexBuffer.Create(@meshData[0], 24, vfPos3Tex2);
  meshIBuffer := TglrIndexBuffer.Create(@indices[0], 36, ifByte);

  Default.SpriteMaterial.Color := dfVec4f(1, 1, 1, 1.0);
  Default.SpriteMaterial.AddTexture(Tex, 'uTexture');
end;

procedure TGame.RenderMesh;
begin
  Default.SpriteMaterial.Shader.SetUniform(utVec4, 1, @Default.SpriteMaterial.Color, 'uColor', -1);
  Render.DrawTriangles(meshBuffer, meshIBuffer, 0, 36);
end;

procedure TGame.OnFinish;
begin
  WriteLn('End');
  //Tex.Free();
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

  if (aType = itWheel) then
    Scene.Camera.Translate(0, 0, -Sign(aOtherParam));

  if (aType = itKeyDown) and (aKey = kL) then
    Log.Write(lInformation, Convert.ToString(Render.Params.ModelViewProj));
end;

procedure TGame.OnPause;
begin
  WriteLn('Pause');
end;

procedure TGame.OnRender;
var
  i: Integer;
begin
  Scene.RenderScene();
  gl.Color3f(1, 1, 1);

  gl.Beginp(GL_POINTS);
    for i := 0 to Length(Points) - 1 do
      gl.Vertex3fv(Points[i]);
  gl.Endp();


    gl.Beginp(GL_LINES);
      gl.Color4ub(255, 0, 0, 255);
      gl.Vertex3f(0, 0, 0);
      gl.Vertex3f(100, 0, 0);

      gl.Color4ub(0, 255, 0, 255);
      gl.Vertex3f(0, 0, 0);
      gl.Vertex3f(0, 100, 0);

      gl.Color4ub(0, 0, 255, 255);
      gl.Vertex3f(0, 0, 0);
      gl.Vertex3f(0, 0, 100);
    gl.Endp();

  Default.SpriteMaterial.Bind();
  RenderMesh();
  Default.SpriteMaterial.Unbind();
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
var
  i: Integer;
begin
  WriteLn('Start');

  SetLength(Points, 1024);
  Randomize();
  Render.SetCullMode(cmBack);
  for i := 0 to Length(Points) - 1 do
    Points[i] := dfVec3f(15 - Random(30), 10 - Random(20), 15 - Random(30));

  Scene := TglrScene.Create(True);
  Scene.Camera.SetCamera(dfVec3f(5, 5, 5), dfVec3f(0, 0, 0), dfVec3f(0, 1, 0));
  Scene.Camera.ProjectionMode := pmPerspective;

  Tex := TglrTexture.Create(FileSystem.ReadResource('data/box.tga'), 'tga');

  PrepareMesh();
end;

procedure TGame.OnUpdate(const dt: Double);
begin

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
    Caption := 'tiny glr ляля';
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

