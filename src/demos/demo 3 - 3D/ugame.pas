unit uGame;

interface

uses
  glr_math, glr_core, glr_scene, glr_render, glr_mesh, glr_filesystem,
  glr_utils, glr_resload;

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
    procedure OnInput(Event: PglrInputEvent); override;
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
  Material.PolygonMode := pmLines;
end;

procedure TGame.SaveMeshDataAsGlr;
var
  f: TglrStream;
begin
  f := glr_resload.SaveMesh(mesh.Data, mfRawGlr);
  FileSystem.WriteResource('data/mesh.raw', f);
  f.Free();
end;

procedure TGame.OnStart;
begin
  PrepareMesh();
  PrepareMaterial();

  Camera := TglrCamera.Create();
  Camera.SetProjParams(0, 0, Render.Width, Render.Height,
    45, 0.1, 500,
    pmPerspective, pTopLeft);
  Camera.SetViewParams(Vec3f(5, 7, 5), Vec3f(0, 2, 0), Vec3f(0, 1, 0));
end;

procedure TGame.OnFinish;
begin
  mesh.Free();
  Shader.Free();
  Material.Free();
  Camera.Free();
end;

procedure TGame.OnInput(Event: PglrInputEvent);
begin
  if (Event.InputType = itTouchDown) and (Event.Key = kLeftButton) then
  begin
    dx := Event.X;
    dy := Event.Y;
  end;

  if (Event.InputType = itTouchMove) and (Event.Key = kLeftButton) then
  begin
    Camera.Rotate((Event.X - dx) * deg2rad * 0.2, Vec3f(0, 1, 0));
    Camera.Rotate((Event.Y - dy) * deg2rad * 0.2, Camera.Right);
    dx := Event.X;
    dy := Event.Y;
  end;

  if (Event.InputType = itWheel) then
    Camera.Translate(0, 0, -Sign(2 * Event.W));
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

  Camera.Translate(0, v.y * 15 * dt, v.x * 15 * dt);
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

