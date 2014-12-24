unit uGame;

interface

uses
  tinyglr, glrMath,
  uLevel;

const
  cRed: TglrVec4f = (x: 0.9; y: 0.2; z: 0.2; w: 1.0);
  cGreen: TglrVec4f = (x: 0.2; y: 0.9; z: 0.2; w: 1.0);
  cBlue: TglrVec4f = (x: 0.15; y: 0.18; z: 0.9; w: 1.0);
  cWhite: TglrVec4f = (x: 1; y: 1; z: 1; w: 1.0);

type

  { TGame }

  TGame = class (TglrGame)
  private
    // Resources and systems
    sb: TglrSpriteBatch;
    fb: TglrFontBatch;
    font: TglrFont;
    mat: TglrMaterial;
    cam: TglrCamera;

    // Game objects
    blender: TBlender;
    points: TColorPoints;

    dragged: TColorPoint;

    function GetColorPointAt(X, Y: Single): TColorPoint;
    function GetBlenderAt(X, Y: Single): TBlender;
  public
    procedure LoadLevel();

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

  // Resources
  font := TglrFont.Create(FileSystem.ReadResource('puzzle/Hattori Hanzo17b.bmp'));
  sb := TglrSpriteBatch.Create();
  fb := TglrFontBatch.Create(font);
  mat := TglrMaterial.Create(Default.SpriteShader);
  mat.AddTexture(Default.BlankTexture, 'uDiffuse');
  cam := TglrCamera.Create();
  cam.ProjectionMode := pmOrtho;
  cam.ProjectionModePivot := pTopLeft;
  cam.SetCamera(
    Vec3f(0, 0, 100),
    Vec3f(0, 0, 0),
    Vec3f(0, 1, 0));
  cam.Viewport(0, 0, Render.Width, Render.Height, 90, -1, 200);

  LoadLevel();

  dragged := nil;
end;

function TGame.GetColorPointAt(X, Y: Single): TColorPoint;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to points.Count - 1 do
    with points[i].sprite do
    if (IsPointInBox(Vec2f(X, Y), Vec2f(Position + Vertices[2].vec), Vec2f(Position + Vertices[0].vec))) then
      Result := points[i];
end;

function TGame.GetBlenderAt(X, Y: Single): TBlender;
begin
  Result := nil;
  with blender.sprite do
    if (IsPointInBox(Vec2f(X, Y), Vec2f(Position + Vertices[2].vec), Vec2f(Position + Vertices[0].vec))) then
      Result := blender;
end;

procedure TGame.LoadLevel;
var
  p: TColorPoint;
begin
  blender := TBlender.Create();
  blender.Setup(Vec2f(500, 500), btAdd);

  points := TColorPoints.Create();

  p := TColorPoint.Create();
  p.Setup(Vec2f(200, 150), cRed);
  points.add(p);

  p := TColorPoint.Create();
  p.Setup(Vec2f(300, 120), cGreen);
  points.add(p);

  p := TColorPoint.Create();
  p.Setup(Vec2f(150, 400), cBlue);
  points.add(p);

  p := TColorPoint.Create();
  p.Setup(Vec2f(600, 700), cWhite);
  points.add(p);
end;

procedure TGame.OnFinish;
begin
  // Write here code for destroying all of your objects

  blender.Free();
  points.Free(True);

  cam.Free();
  mat.Free();
  sb.Free();
  fb.Free();
  font.Free();
end;

procedure TGame.OnInput(aType: TglrInputType; aKey: TglrKey; X, Y,
  aOtherParam: Integer);
var
  b: TBlender;
  p: TColorPoint;
  col: TglrVec4f;
begin
  // Calls when engine receives some input info
  if aKey = kLeftButton then
    case aType of
      itTouchDown:
        begin
          dragged := GetColorPointAt(X, Y);
          if Assigned(dragged) then
            if blender.colorPoints.IndexOf(dragged) <> -1 then
              blender.colorPoints.Delete(dragged);
        end;
      itTouchUp:
        begin
          b := GetBlenderAt(X, Y);
          if Assigned(b) then
            if b.colorPoints.IndexOf(dragged) = -1 then
              b.colorPoints.Add(dragged);

          dragged := nil;
        end;
    end;

  if aType = itKeyDown then
    case aKey of
      kE:
      begin
        if (blender.CanUse()) then
        begin
          p := TColorPoint.Create();
          p.Setup(Vec2f(blender.sprite.Position) + Vec2f(0, 20), blender.ResultColor);
          blender.colorPoints.Clear();
          points.Add(p);
        end;
      end;
    end;
end;

procedure TGame.OnUpdate(const dt: Double);
begin
  // Place here game logic code
  if (dragged <> nil) then
    with dragged.sprite.Position do
    begin
      x := Core.Input.MousePos.x;
      y := Core.Input.MousePos.y;
    end;
end;

procedure TGame.OnRender;
var
  i: Integer;
begin
  cam.Update();
  mat.Bind();
  sb.Start();
    for i := 0 to points.Count - 1 do
      sb.Draw(points[i].sprite);
    sb.Draw(blender.sprite);
  sb.Finish();
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

