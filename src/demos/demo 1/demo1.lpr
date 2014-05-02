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
end;

procedure TGame.OnInput(aType: TglrInputType; aKey: TglrKey; X, Y,
  aOtherParam: Integer);
begin
  if (aKey <> kNoInput) then
    WriteLn('Input : ' + Core.Input.GetInputTypeName(aType) + ' : ' + Core.Input.GetKeyName(aKey));

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
    for i := 0 to Length(Points) do
      gl.Vertex3fv(Points[i]);
  gl.Endp();

  gl.Beginp(GL_TRIANGLES);
    gl.Vertex3f(100, 100, 5);
    gl.Vertex3f(100, 200, 5);
    gl.Vertex3f(200, 200, 5);
  gl.Endp();
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
  for i := 0 to Length(Points) do
    Points[i] := dfVec3f(300 - Random(600), 300 - Random(600), 300 - Random(600));
  Scene := TglrScene.Create(True);
//  Scene.Camera.Viewport(0, 0, Render.Width, Render.Height, 90, -1, 10);
  Scene.Camera.ProjectionMode := pmOrtho;
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
  end;

  Game := TGame.Create();
  Core.Init(Game, InitParams);
  Core.Loop();
  Core.DeInit();
end.

