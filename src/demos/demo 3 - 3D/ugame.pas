unit uGame;

interface

uses
  tinyglr, glrMath, resload;

type

  { TGame }

  TGame = class (TglrGame)
  protected
    dx, dy: Integer;
    Camera: TglrCamera;
    Material: TglrMaterial;
    Shader: TglrShaderProgram;

    mesh: TglrMesh;
    procedure PrepareMesh();
    procedure PrepareMaterial();
    procedure SaveMeshDataAsGlr();
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

procedure TGame.PrepareMesh();
begin
//  mesh := TglrMesh.Create(FileSystem.ReadResource('data/Lara_Croft_default.obj'), mfObj);
  mesh := TglrMesh.Create(FileSystem.ReadResource('data/mesh.raw'), mfRawGlr);
end;

procedure TGame.PrepareMaterial;
begin
  Shader := TglrShaderProgram.Create();
  Shader.Attach(FileSystem.ReadResource('data/MeshShaderV.txt'), stVertex);
  Shader.Attach(FileSystem.ReadResource('data/MeshShaderF.txt'), stFragment);
  Shader.Link();

  Material := TglrMaterial.Create(Shader);
  Material.AddTexture(Default.BlankTexture, 'uDiffuse');
end;

procedure TGame.SaveMeshDataAsGlr;
var
  f: TglrStream;
begin
  f := resload.SaveMesh(mesh.Data, mfRawGlr);
  FileSystem.WriteResource('data/mesh.raw', f);
  f.Free();
end;

procedure TGame.OnStart;
begin
  PrepareMesh();
  PrepareMaterial();

  Camera := TglrCamera.Create();
  Camera.ProjectionMode := pmPerspective;
  Camera.SetCamera(Vec3f(5, 7, 5), Vec3f(0, 2, 0), Vec3f(0, 1, 0));
  Camera.Viewport(0, 0, Render.Width, Render.Height, 45, 0.1, 500);
end;

procedure TGame.OnFinish;
begin
  mesh.Free();
  Shader.Free();
  Material.Free();
  Camera.Free();
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
    Camera.Rotate((x - dx) * deg2rad * 0.2, Vec3f(0, 1, 0));
    Camera.Rotate((y - dy) * deg2rad * 0.2, Camera.Right);
    dx := X;
    dy := Y;
  end;

  if (aType = itWheel) then
    Camera.Translate(0, 0, -Sign(10 * aOtherParam));
end;

procedure TGame.OnUpdate(const dt: Double);
var
  v: TglrVec2f;
begin
  v.Reset();
  if (Core.Input.KeyDown[kW]) then
    v.x := -1
  else if (Core.Input.KeyDown[kS]) then
    v.x := 1;

  if (Core.Input.KeyDown[kA]) then
    v.y := -1
  else if (Core.Input.KeyDown[kD]) then
    v.y := 1;

  Camera.Translate(0, v.y * 40 * dt, v.x * 40 * dt);
end;

procedure TGame.OnRender;
begin
  Camera.Update();
  Material.Bind();
  with mesh.Data do
    Render.DrawTriangles(vBuffer, iBuffer, 0, iBuffer.Count);
end;

procedure TGame.OnPause;
begin
  // Calls when app' window has lost focus
end;

procedure TGame.OnResume;
begin
  // Calls when app' window was focused
end;

procedure TGame.OnResize(aNewWidth, aNewHeight: Integer);
begin
  // Calls when window has changed size
end;


end.

