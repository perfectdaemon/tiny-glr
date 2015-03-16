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
  TglrCameraTargetMode = (mPoint, mTarget, mFree);

  TglrCameraPivot = (pTopLeft, pCenter, pBottomRight);

  TglrViewportParams = record
    X, Y, W, H: Integer;
    FOV, ZNear, ZFar: Single;
  end;

  TglrCamera = class (TglrNode)
  private
    procedure SetProjModePivot(aValue: TglrCameraPivot);
    procedure SetScale(aValue: Single);
  protected
    fProjMode: TglrCameraProjectionMode;

    fMode: TglrCameraTargetMode;
    fTargetPoint: TglrVec3f;
    fTarget: TglrNode;
    fScale, fFOV, fZNear, fZFar: Single;
    fX, fY, fW, fH: Integer;
    fProjectionPivot: TglrCameraPivot;
    procedure SetProjMode(aMode: TglrCameraProjectionMode);
    procedure UpdateVectorsFromMatrix(); override;
  public
    fProjMatrix: TglrMat4f;

    constructor Create(); override;

    procedure Viewport(x, y, w, h: Integer; FOV, ZNear, ZFar: Single);
    procedure ViewportOnly(x, y, w, h: Integer);

    procedure Translate(alongUpVector, alongRightVector, alongDirVector: Single);
    procedure Rotate(delta: Single; Axis: TglrVec3f);

    function GetViewport(): TglrViewportParams;
    function WindowPosToCameraPos(aPos: TglrVec2f): TglrVec2f;

    procedure Update;

    procedure SetCamera(aPos, aTargetPos, aUp: TglrVec3f);

    procedure RenderSelf(); override;

    property Scale: Single read fScale write SetScale;

  //    procedure SetTarget(aPoint: TglrVec3f); overload;
  //    procedure SetTarget(aTarget: IglrNode); overload;

    property ProjectionMode: TglrCameraProjectionMode read fProjMode write SetProjMode;
    property ProjectionModePivot: TglrCameraPivot read fProjectionPivot write SetProjModePivot;
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

procedure TglrCamera.SetProjModePivot(aValue: TglrCameraPivot);
begin
  if fProjectionPivot = aValue then
    Exit;
  fProjectionPivot := aValue;
  SetProjMode(ProjectionMode); //update projection
end;

procedure TglrCamera.SetScale(aValue: Single);
begin
  if fScale = aValue then
    Exit;
  fScale := aValue;
  SetProjMode(ProjectionMode); //update projection
end;

procedure TglrCamera.SetProjMode(aMode: TglrCameraProjectionMode);
begin
  fProjMatrix.Identity;
  case aMode of
    pmPerspective:
      fProjMatrix.Perspective(fFOV, fW / fH, fZNear, fZFar);
    pmOrtho:
      case fProjectionPivot of
        pTopLeft:
          fProjMatrix.Ortho(0, fW / fScale, fH / fScale, 0, fZNear, fZFar); //replaced fX, fY with zero
        pCenter:
          fProjMatrix.Ortho(-fW / (2 * fScale), fW / (2 * fScale), fH /(2 * fScale), -fH / (2 * fScale), fZNear, fZFar);
        pBottomRight:
          fProjMatrix.Ortho(- fW / (2 * fScale), 0, - fH /(2 * fScale), 0, fZNear, fZFar);
      end;
  end;
  fProjMode := aMode;
end;

procedure TglrCamera.UpdateVectorsFromMatrix;
begin
  inherited;
  exit();
  (*
  with Matrix do
  begin
    fRight.x := e00;  fRight.y := e10;  fRight.z := e20;
    fUp.x := e01; fUp.y := e11; fUp.z := e21;
    fDir.x := e02;   fDir.y := e12;   fDir.z := e22;
  end;
  *)
end;

constructor TglrCamera.Create;
begin
  inherited Create;
  fFOV := 90;
  fZNear := 0.01;
  fZFar := 100;
  fX := 0;
  fY := 0;
  fW := Render.Width;
  fH := Render.Height;
  fScale := 1.0;
  fProjectionPivot := pTopLeft;
  fProjMatrix.Identity;
  SetCamera(Vec3f(0, 0, 10), Vec3f(0, 0, 0), Vec3f(0, 1, 0));
end;

procedure TglrCamera.Viewport(x, y, w, h: Integer; FOV, ZNear, ZFar: Single);
begin
  fFOV := FOV;
  fZNear := ZNear;
  fZFar := ZFar;
  fX := x;
  fY := y;
  if w > 0 then
    fW := w
  else
    fW := 1;
  if h > 0 then
    fH := h
  else
    fH := 1;

  ProjectionMode := fProjMode; //Обновляем матрицу проекции

  Render.SetViewPort(fX, fY, fW, fH);
end;

procedure TglrCamera.ViewportOnly(x, y, w, h: Integer);
begin
  Viewport(x, y, w, h, fFOV, fZNear, fZFar);
end;

procedure TglrCamera.Translate(alongUpVector, alongRightVector,
  alongDirVector: Single);
var
  v: TglrVec3f;
begin
  v := Up * alongUpVector + Right * alongRightVector + Direction * alongDirVector;
  Position += v;
  UpdateVectorsFromMatrix();
end;

procedure TglrCamera.Rotate(delta: Single; Axis: TglrVec3f);
begin
  Matrix.Rotate(delta, Axis);
  UpdateVectorsFromMatrix();
end;

function TglrCamera.GetViewport: TglrViewportParams;
begin
  with Result do
  begin
    X := fX;
    Y := fY;
    W := fW;
    H := fH;
    FOV := fFOV;
    ZNear := fZNear;
    ZFar := fZFar;
  end;
end;

function TglrCamera.WindowPosToCameraPos(aPos: TglrVec2f): TglrVec2f;
begin
  Result := Vec2f(Position) + aPos * (1 / fScale);
  case fProjectionPivot of
    pTopLeft: ;
    pCenter:      Result -= Vec2f(fW / 2, fH / 2) * (1 / fScale);
    pBottomRight: Result -= Vec2f(fW,     fH)     * (1 / fScale);
  end;
end;

procedure TglrCamera.Update;
begin
  Matrix.Pos := Vec3f(0, 0, 0);
  Matrix.Pos := Matrix * Position.NegateVector;
  Render.Params.ViewProj := fProjMatrix * Matrix;
  Render.Params.ModelViewProj := Render.Params.ViewProj;
  UpdateVectorsFromMatrix();

  if (Render.Width <> fW) or (Render.Height <> fH) then
    ViewportOnly(0, 0, Render.Width, Render.Height);
end;

procedure TglrCamera.SetCamera(aPos, aTargetPos, aUp: TglrVec3f);
begin
  Matrix.Identity;
  fUp := aUp.Normal();
  fDir := (aTargetPos - aPos).Normal();
  fRight := fDir.Cross(fUp).Normal();
  fUp := fRight.Cross(fDir).Normal();
  Position := aPos;
  fDir.Negate;

  with Matrix do
  begin
    e00 := fRight.x;  e10 := fRight.y;  e20 := fRight.z;  e30 := -fRight.Dot(Position);
    e01 := fUp.x;    e11 := fUp.y;    e21 := fUp.z;    e31 := -fUp.Dot(Position);
    e02 := fDir.x;   e12 := fDir.y;   e22 := fDir.z;   e32 := -fDir.Dot(Position);
    e03 := 0;        e13 := 0;        e23 := 0;        e33 := 1;
  end;

  Matrix := Matrix.Transpose();

  fTargetPoint := aTargetPos;
  fMode := mPoint;

  UpdateVectorsFromMatrix();
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

