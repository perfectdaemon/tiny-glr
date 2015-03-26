unit glr_scene;

{$i defines.inc}

interface

uses
  glr_math;

type

  { TglrNode }

  TglrNode = class;

  TglrNode = class
  protected
    fDir, fRight, fUp: TglrVec3f;
    fParent: TglrNode;
    fAbsMatrix: TglrMat4f;
    fVisible: Boolean;

    procedure SetParent(AValue: TglrNode);
    function GetAbsMatrix: TglrMat4f;
    procedure SetDir(aDir: TglrVec3f);
    procedure SetRight(aRight: TglrVec3f);
    procedure SetUp(aUp: TglrVec3f);
    procedure UpdateModelMatrix(aNewDir, aNewUp, aNewRight: TglrVec3f); virtual;
    procedure UpdateVectorsFromMatrix(); virtual;
    procedure DoRender(); virtual;
    procedure SetVisible(const aVisible: Boolean); virtual;
  public
    Matrix: TglrMat4f;
    Position: TglrVec3f;

    constructor Create; virtual;
    destructor Destroy; override;

    property AbsoluteMatrix: TglrMat4f read GetAbsMatrix write fAbsMatrix;
    property Parent: TglrNode read fParent write SetParent;

    property Up: TglrVec3f read fUp write SetUp;
    property Direction: TglrVec3f read fDir write SetDir;
    property Right: TglrVec3f read fRight write SetRight;

    property Visible: Boolean read fVisible write SetVisible;

    procedure RenderSelf(); virtual;
  end;

  { TglrCamera }

  TglrCameraProjectionMode = (pmOrtho, pmPerspective);

  TglrCameraPivot = (pTopLeft, pCenter, pBottomRight);

  TglrCamera = class (TglrNode)
  protected
    fProjMode: TglrCameraProjectionMode;
    fPivotMode: TglrCameraPivot;

    fScale, fFOV, fZNear, fZFar: Single;
    fX, fY, fW, fH: Integer;
    procedure SetScale(aValue: Single);
    procedure RebuildProjMatrix();
  public
    ProjMatrix: TglrMat4f;

    constructor Create(); override;

    procedure SetProjParams(X, Y, W, H: Integer); overload;
    procedure SetProjParams(X, Y, W, H: Integer;
      FOV, ZNear, ZFar: Single;
      ProjMode: TglrCameraProjectionMode;
      PivotMode: TglrCameraPivot); overload;

    procedure SetViewParams(SelfPos, TargetPos, Up: TglrVec3f);

    procedure Translate(alongUpVector, alongRightVector, alongDirVector: Single);
    procedure Rotate(delta: Single; Axis: TglrVec3f);
    property Scale: Single read fScale write SetScale;

    function ScreenToWorld(screenPosition: TglrVec2f): TglrVec2f;

    procedure RenderSelf(); override;
    procedure Update();
  end;

  { TglrScene }

  TglrScene = class
  protected
    fOwnCamera: Boolean;
    fCameraAssignedLogError: Boolean;
  public
    Root: TglrNode;
    Camera: TglrCamera;

    constructor Create(aCreateCamera: Boolean = True); virtual;
    destructor Destroy(); override;

    procedure RenderScene(); virtual;
  end;

implementation

uses
  glr_render, glr_utils;

{ TglrNode }

procedure TglrNode.SetParent(AValue: TglrNode);
begin
  if (fParent = AValue) then
    Exit();
  fParent := AValue;
  //GetAbsMatrix();
end;

function TglrNode.GetAbsMatrix: TglrMat4f;
begin
  Matrix.Pos := Position;
  if Assigned(fParent) then
    fAbsMatrix := fParent.AbsoluteMatrix * Matrix
  else
    fAbsMatrix := Matrix;
  Exit(fAbsMatrix);
end;

procedure TglrNode.SetDir(aDir: TglrVec3f);
var
  NewUp, NewRight: TglrVec3f;
begin
  if (fDir = aDir) then
    Exit;
  aDir.Normalize;
  NewRight := fUp.Cross(aDir);
  NewRight.Negate;
  NewRight.Normalize;
  NewUp := aDir.Cross(NewRight);
  NewUp.Normalize;
  UpdateModelMatrix(aDir, NewUp, NewRight);
end;

procedure TglrNode.SetRight(aRight: TglrVec3f);
var
  NewDir, NewUp: TglrVec3f;
begin
  if (fRight = aRight) then
    Exit();
  aRight.Normalize;
  NewDir := aRight.Cross(fUp);
  NewDir.Normalize;
  NewUp := NewDir.Cross(aRight);
  NewUp.Normalize;
  UpdateModelMatrix(NewDir, NewUp, aRight);
end;

procedure TglrNode.SetUp(aUp: TglrVec3f);
var
  NewDir, NewRight: TglrVec3f;
begin
  if (fUp = aUp) then
    Exit();
  aUp.Normalize;
  NewRight := aUp.Cross(fDir);
  NewRight.Negate;
  NewRight.Normalize;
  NewDir := NewRight.Cross(aUp);
  NewDir.Normalize;
  UpdateModelMatrix(NewDir, aUp, NewRight);
end;

procedure TglrNode.UpdateModelMatrix(aNewDir, aNewUp, aNewRight: TglrVec3f);
begin
  with Matrix do
  begin
    e00 := aNewRight.x; e01 := aNewRight.y; e02 := aNewRight.z; e03 := Position.Dot(aNewRight);
    e10 := aNewUp.x;    e11 := aNewUp.y;    e12 := aNewUp.z;    e13 := Position.Dot(aNewUp);
    e20 := aNewDir.x;   e21 := aNewDir.y;   e22 := aNewDir.z;   e23 := Position.Dot(aNewDir);
    e30 := 0;           e31 := 0;           e32 := 0;           e33 := 1;
  end;
  fRight := aNewRight;
  fUp   := aNewUp;
  fDir  := aNewDir;
end;

procedure TglrNode.UpdateVectorsFromMatrix;
begin
  with Matrix do
  begin
    fDir.x := e20; fDir.y := e21; fDir.z := e22;
    fUp.x := e10; fUp.y := e11; fUp.z := e12;
    fRight.x := e00; fRight.y := e01; fRight.z := e02;
  end;
end;

procedure TglrNode.DoRender;
begin
  //nothing
end;

procedure TglrNode.SetVisible(const aVisible: Boolean);
begin
  fVisible := aVisible;
end;

constructor TglrNode.Create;
begin
  inherited Create();
  Matrix.Identity;
  fVisible := True;
  Parent := nil;
  UpdateVectorsFromMatrix();
end;

destructor TglrNode.Destroy;
begin
  inherited Destroy;
end;

procedure TglrNode.RenderSelf();
begin
  Render.Params.Model := AbsoluteMatrix;
  Render.Params.CalculateMVP();

  UpdateVectorsFromMatrix();

  if (not Visible) then
    Exit();
  DoRender();
end;

{ TglrCamera }

procedure TglrCamera.SetScale(aValue: Single);
begin
  if fScale = aValue then
    Exit;
  fScale := aValue;
  RebuildProjMatrix();
end;

procedure TglrCamera.RebuildProjMatrix;
begin
  ProjMatrix.Identity();
  case fProjMode of
    pmPerspective: ProjMatrix.Perspective(fFOV, fW / fH, fZNear, fZFar);
    pmOrtho:
      case fPivotMode of
        pTopLeft:     ProjMatrix.Ortho(0,                   fW / fScale,       fH / fScale,       0,                   fZNear, fZFar);
        pCenter:      ProjMatrix.Ortho(- fW / (2 * fScale), fW / (2 * fScale), fH / (2 * fScale), - fH / (2 * fScale), fZNear, fZFar);
        pBottomRight: ProjMatrix.Ortho(- fW / (2 * fScale), 0,                 0,                 - fH / (2 * fScale), fZNear, fZFar);
      end;

  end;
end;

constructor TglrCamera.Create;
begin
  inherited Create;
	fScale := 1.0;
  SetProjParams(0, 0, Render.Width, Render.Height, 45, 0.01, 100, pmOrtho, pTopLeft);
  SetViewParams(Vec3f(0, 0, 100), Vec3f(0, 0, 0), Vec3f(0, 1, 0));
end;

procedure TglrCamera.SetProjParams(X, Y, W, H: Integer);
begin
  if (W < 0) then
    W := 1;
  if (H < 0) then
    H := 1;

  fX := X;
  fY := Y;
  fW := W;
  fH := H;
  RebuildProjMatrix();
end;

procedure TglrCamera.SetProjParams(X, Y, W, H: Integer; FOV, ZNear,
  ZFar: Single; ProjMode: TglrCameraProjectionMode; PivotMode: TglrCameraPivot);
begin
  fFOV := FOV;
  fZNear := ZNear;
  fZFar := ZFar;
  fProjMode := ProjMode;
  fPivotMode := PivotMode;
  setProjParams(X, Y, W, H);
end;

procedure TglrCamera.SetViewParams(SelfPos, TargetPos, Up: TglrVec3f);
begin
  Matrix.Identity();
  fUp := Up.Normal();
  fDir := (TargetPos - SelfPos).Normal();
  fRight := fDir.Cross(fUp).Normal();
  fUp := fRight.Cross(fDir).Normal(); // wtf?
  Position := SelfPos;
  fDir := -fDir;
  with Matrix do
  begin
    e00 := fRight.x; e10 := fRight.y; e20 := fRight.z; e30 := -fRight.Dot(Position);
    e01 := fUp.x;    e11 := fUp.y;    e21 := fUp.z;    e31 := -fUp.Dot(Position);
    e02 := fDir.x;   e12 := fDir.y;   e22 := fDir.z;   e32 := -fDir.Dot(position);
    e03 := 0;        e13 := 0;        e23 := 0;        e33 := 1;
  end;

  Matrix := Matrix.Transpose();
  UpdateVectorsFromMatrix();
end;

procedure TglrCamera.Translate(alongUpVector, alongRightVector,
  alongDirVector: Single);
var
  v: TglrVec3f;
begin
  v := Up * alongUpVector + Right * alongRightVector + Direction * alongDirVector;
  Position += v;
end;

procedure TglrCamera.Rotate(delta: Single; Axis: TglrVec3f);
begin
  Matrix.Rotate(delta, Axis);
  UpdateVectorsFromMatrix();
end;

function TglrCamera.ScreenToWorld(screenPosition: TglrVec2f): TglrVec2f;
begin
  case fProjMode of
    pmOrtho:
    begin
      Result := Vec2f(Position) + screenPosition / fScale;
      case fPivotMode of
        pCenter:      Result -= Vec2f(fW / 2, fH / 2) / fScale;
        pBottomRight: Result -= Vec2f(fW,     fH)     / fScale;
        pTopLeft: ;
      end;
    end;
    pmPerspective:
      Log.Write(lCritical, 'Camera.ScreenToWorld() with perpective camera is not implemented');
  end;
end;

procedure TglrCamera.Update;
begin
  Matrix.Pos := Vec3f(0, 0, 0);
  Matrix.Pos := Matrix * Position.NegateVector;
  Render.Params.ViewProj := ProjMatrix * Matrix;
  Render.Params.ModelViewProj := Render.Params.ViewProj;
  UpdateVectorsFromMatrix();

  if (Render.Width <> fW) or (Render.Height <> fH) then
    SetProjParams(0, 0, Render.Width, Render.Height);
end;

procedure TglrCamera.RenderSelf;
begin
  //nothing!!!!
end;

{ TglrScene }

constructor TglrScene.Create(aCreateCamera: Boolean);
begin
  inherited Create();
  fOwnCamera := aCreateCamera;
  if (fOwnCamera) then
    Camera := TglrCamera.Create();
  Root := TglrNode.Create();
  fCameraAssignedLogError := False;
end;

destructor TglrScene.Destroy;
begin
  if (fOwnCamera) then
    Camera.Free();
  Root.Free();
  inherited Destroy;
end;

procedure TglrScene.RenderScene;
begin
  if not Assigned(Camera) and not fCameraAssignedLogError then
  begin
    Log.Write(lError, 'No camera assigned to scene, render is impossible');
    fCameraAssignedLogError := True;
    Exit();
  end
  else
    fCameraAssignedLogError := False;

  Camera.Update();
  Root.RenderSelf();
end;

end.

