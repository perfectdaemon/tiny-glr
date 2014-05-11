program demo1;
  //{$mode delphi}
//  {$apptype console}

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
    Shader: TglrShaderProgram;
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

procedure TGame.OnFinish;
begin
  WriteLn('End');
  Tex.Free();
  Shader.Free();
  Scene.Free();
end;

procedure TGame.OnInput(aType: TglrInputType; aKey: TglrKey; X, Y,
  aOtherParam: Integer);
begin
//  if (aKey <> kNoInput) then
//    WriteLn('Input : ' + Core.Input.GetInputTypeName(aType) + ' : ' + Core.Input.GetKeyName(aKey));

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

//  Shader.Bind();
  Render.SetTexture(Tex.Id, 0);
  gl.Color4f(0.5, 1, 1, 1);
  gl.Beginp(GL_TRIANGLES);
    gl.TexCoord2f(0, 0); gl.Vertex3f(1, 0, 1);
    gl.TexCoord2f(0, 1); gl.Vertex3f(1, 0, 2);
    gl.TexCoord2f(1, 1); gl.Vertex3f(2, 0, 2);
  gl.Endp();
  Render.SetTexture(0, 0);
  Shader.Unbind();
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
  Render.SetCullMode(cmNone);
  for i := 0 to Length(Points) - 1 do
    Points[i] := dfVec3f(15 - Random(30), 10 - Random(20), 15 - Random(30));

  Scene := TglrScene.Create(True);
  //Scene.Camera.Viewport(0, 0, Render.Width, Render.Height, 90, 0.1, 100);
  Scene.Camera.SetCamera(dfVec3f(5, 5, 5), dfVec3f(0, 0, 0), dfVec3f(0, 1, 0));
  Scene.Camera.ProjectionMode := pmPerspective;

  Tex := TglrTexture.Create(FileSystem.ReadResource('data/box.tga'), 'tga');

  Shader := TglrShaderProgram.Create();
  Shader.LoadAndAttachShader(FileSystem.ReadResource('data/simple.vs'), stVertex);
  Shader.LoadAndAttachShader(FileSystem.ReadResource('data/simple.fs'), stFragment);
  Shader.Link();
end;

procedure TGame.OnUpdate(const dt: Double);
begin

end;

var
  Game: TGame;
  InitParams: TglrInitParams;

begin
//  SetHeapTraceOutput('heaptrace.log');
  with InitParams do
  begin
    Width := 800;
    Height := 600;
    X := 100;
    Y := 100;
    Caption := 'tiny glr ляля';
    vSync := True;
    PackFilesPath := 'data/';
  end;

  Game := TGame.Create();
  Core.Init(Game, InitParams);
  Core.Loop();
  Core.DeInit();
  Game.Free();
end.

