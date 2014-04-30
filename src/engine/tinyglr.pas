{
  TODO:
    - Окно
    - Начинка рендера
    - Логгер ?
}

unit tinyglr;

interface

uses
  ogl, glrMath;

type
  {$REGION 'Utils'}
  TglrStream = class
    class function Init(Memory: Pointer; MemSize: LongInt): TglrStream; overload;
    class function Init(const FileName: AnsiString; RW: Boolean = False): TglrStream; overload;
    destructor Destroy; override;
  private
    SType  : (stMemory, stFile);
    FSize  : LongInt;
    FPos   : LongInt;
    FBPos  : LongInt;
    F      : File;
    Mem    : Pointer;
    procedure SetPos(Value: LongInt);
    procedure SetBlock(BPos, BSize: LongInt);
  public
    procedure CopyFrom(const Stream: TglrStream);
    function Read(out Buf; BufSize: LongInt): LongInt;
    function Write(const Buf; BufSize: LongInt): LongInt;
    function ReadAnsi: AnsiString;
    procedure WriteAnsi(const Value: AnsiString);
    function ReadUnicode: WideString;
    procedure WriteUnicode(const Value: WideString);
    property Size: LongInt read FSize;
    property Pos: LongInt read FPos write SetPos;
  end;

  TglrListCompareFunc = function (Item1, Item2: Pointer): LongInt;

  { TglrList }

  TglrList = class
    procedure Init(Capacity: LongInt = 1);
    procedure Free(FreeItems: Boolean = False);
  private
    FItems    : array of Pointer;
    FCount    : LongInt;
    FCapacity : LongInt;
    procedure BoundsCheck(Index: LongInt);
    function GetItem(Index: LongInt): Pointer; inline;
    procedure SetItem(Index: LongInt; Value: Pointer); inline;
  public
    function IndexOf(Item: Pointer): LongInt;
    function Add(Item: Pointer): LongInt;
    procedure Delete(Index: LongInt; FreeItem: Boolean = False); overload;
    procedure Delete(Item: Pointer; FreeItem: Boolean = False); overload;
    procedure Insert(Index: LongInt; Item: Pointer);
    procedure Sort(CompareFunc: TglrListCompareFunc);
    property Count: LongInt read FCount;
    property Items[Index: LongInt]: Pointer read GetItem write SetItem; default;
  end;

  { FileSystem }

  FileSystem = class
  protected
  public
    class function GetResource(aFileName: AnsiString): TglrStream;
  end;

  { Convert }

  Convert = class
  public
    class function ToStringA(aVal: Integer): AnsiString; overload;
    class function ToStringA(aVal: Single): AnsiString; overload;
    class function ToStringW(aVal: Integer): UnicodeString; overload;
    class function ToStringW(aVal: Single): UnicodeString; overload;
    class function ToInt(aStr: AnsiString): Integer; overload;
    class function ToInt(aStr: UnicodeString): Integer; overload;
    class function ToFloat(aStr: AnsiString): Single; overload;
    class function ToFloat(aStr: UnicodeString): Single; overload;
  end;

  {$ENDREGION}

  {$REGION 'Render'}

  TglrTextureId = type LongWord;
  TglrShaderId = type LongWord;
  TglrIndexBufferId = type LongWord;
  TglrVertexBufferId = type LongWord;
  TglrFrameBufferId = type LongWord;
  TglrIndex = type Word;

  TglrTextureFormat = (tfFuck);
  TglrVertexFormat = (vfPos2Tex2, vfPos3Tex2);
  TglrIndexFormat = (ifByte, ifShort, ifInt);

  TglrVertexP2T2 = record
    vec, tex: TdfVec2f;
  end;

  TglrVertexP3T2 = record
    vec: TdfVec3f;
    tex: TdfVec2f;
  end;


  { TglrTexture }

  TglrTexWrap = (wClamp, wRepeat, wClampToEdge, wClampToBorder, wMirrorRepeat);
  TglrTexCombineMode = (cmDecal, cmModulate, cmBlend, cmReplace, cmAdd);

  TglrTexture = class
  protected
    Target: TGLConst;
    WrapS, WrapT, WrapR: TglrTexWrap;
    CombineMode: TglrTexCombineMode;
  public
    Id: TglrTextureId;

    X, Y, RegionWidth, RegionHeight,
    Width, Height: Integer;

    procedure SetWrapS(aWrap: TglrTexWrap);
    procedure SetWrapT(aWrap: TglrTexWrap);
    procedure SetWrapR(aWrap: TglrTexWrap);

    procedure SetCombineMode(aCombineMode: TglrTexCombineMode);

    //constructor Create(aData: Pointer; aCount: Integer; aFormat: TglrTextureFormat); virtual; overload;
    constructor CreateFromFileStream(aStream: TglrStream; aExt: AnsiString); virtual;
    //constructor CreateEmpty2D(aWidth, aHeight, aFormat: TGLConst);
    //todo 1d, 3d
    destructor Destroy(); override;
  end;


  { TglrVertexBuffer }

  TglrVertexBuffer = class
    Id: TglrVertexBufferId;
    procedure Bind();
    class procedure Unbind();
    constructor Create(aData: Pointer; aCount: Integer; aFormat: TglrVertexFormat); virtual;
    destructor Destroy(); override;
  end;

  { TglrIndexBuffer }

  TglrIndexBuffer = class
    Id: TglrIndexBufferId;
    procedure Bind();
    class procedure Unbind();
    constructor Create(aData: Pointer; aCount: Integer; aFormat: TglrIndexFormat); virtual;
    destructor Destroy(); override;
  end;

  { TglrFrameBuffer }

  TglrFrameBuffer = class
    Id: TglrFrameBufferId;
    procedure Bind();
    class procedure Unbind();
    constructor Create(); virtual;
    destructor Destroy(); override;

    //procedure AttachTexture(aTextureId: TglrTextureId);
  end;


  TglrBlendingMode = ( bmNone, bmAlpha, bmAdditive, bmMultiply, bmScreen);
  TglrCullMode = (cmNone, cmBack, cmFront);
  TglrFuncComparison = (fcNever, fcLess, fcEqual, fcLessOrEqual,
    fcGreater, fcNotEqual, fcGreaterOrEqual, fcAlways);
  TglrClearMask = (cmAll, cmColor, cmDepth);

  { Render }

  TglrRenderParams = record
    ViewProj, Model: TdfMat4f;
    Color: TdfVec4f;
  end;

  Render = class
  private
  protected
    class var fBlendingMode: TglrBlendingMode;
    class var fCullMode: TglrCullMode;
    class var fDepthWrite, fDepthTest, fLight: Boolean;
    class var fDepthFunc, fAlphaFunc: TglrFuncComparison;
    class var fAlphaTest: Single;
    class var fShader: TglrShaderId;
    class var fTexture: TglrTextureId;
    class var fVB: TglrVertexBufferId;
    class var fIB: TglrIndexBufferId;
    class var fFB: TglrFrameBufferId;

    class var fStatTextureBind: Integer;
    class var fWidth, fHeight: Integer;
  public
    class var Params: TglrRenderParams;

    class procedure Init();
    class procedure DeInit();

    class procedure Resize(aWidth, aHeight: Integer);
    class procedure ResetStates();
    class procedure Clear(aClearMask: TglrClearMask);
    class procedure SetClearColor(R, G, B: Single);

    class procedure SetViewPort(aLeft, aTop, aWidth, aHeight: Integer);
    class procedure SetCullMode(aCullMode: TglrCullMode);
    class procedure SetBlendingMode(aBlendingMode: TglrBlendingMode);
    class procedure SetLighting(aEnabled: Boolean); deprecated;
    class procedure SetDepthWrite(aEnabled: Boolean);
    class procedure SetDepthTest(aEnabled: Boolean);
    class procedure SetDepthFunc(aComparison: TglrFuncComparison);
    class procedure SetAlphaTest(aComparison: TglrFuncComparison; aValue: Single);
    class procedure SetVerticalSync(aEnabled: Boolean);
    class procedure SetShader(aShader: TglrShaderId);
    class procedure SetTexture(aTexture: TglrTextureId; aSampler: Integer);

    class procedure DrawTriangles(vBuffer: TglrVertexBuffer; iBuffer: TglrIndexBuffer;
      aStart, aVertCount: Integer);
    class procedure DrawPoints(vBuffer: TglrVertexBuffer; aStart, aVertCount: Integer);
//    class procedure DrawPointSprites();

    //stat
    class property TextureBinds: Integer read fStatTextureBind;
    class property Width: Integer read fWidth;
  end;

{$ENDREGION}

  {$REGION 'App view'}
  TglrAppView = class abstract
  public
    constructor Create(aData: Pointer); virtual; abstract;
    destructor Destroy(); override; abstract;

    procedure Loop(); virtual; abstract;
  end;

  {$ENDREGION}

  {$REGION 'Input'}

  TglrInputType = ( itTouchDown, itTouchUp, itTouchMove, itKeyDown, itKeyUp, itWheel );

  TglrTouch = record
    IsDown: Boolean;
    Start, Pos: TdfVec2f;
  end;

  TglrKey = (
  {$IFDEF WINDOWS}
    kNoInput = $00,

    kLeftButton = $01,
    kRightButton, kCancel, kMiddleButton, kXButton1, kXButton2,

    kBack = $08, kTab,

    kClear = $0C, kReturn,

    kShift = $10, kCtrl, kAlt, kPause, kCapsLock,

    kEscape = $1B,

    kSpace = $20, kPageUp, kPageDown,
    kEnd, kHome, kLeft, kUp, kRight, kDown,

    kPrintScreen = $2C, kInsert, kDelete,

    k0 = $30, k1, k2, k3, k4, k5, k6, k7, k8, k9,
    kA = $41, kB, kC, kD, kE, kF, kG, kH, kI, kJ, kK, kL, kM, kN, kO, kP, kQ, kR, kS, kT, kU, kV, kW, kX, kY, kZ,

    kLeftWin = $5B, kRightWin,

    kNumPad0 = $60, kNumPad1, kNumPad2, kNumPad3, kNumPad4, kNumPad5, kNumPad6, kNumPad7, kNumPad8, kNumPad9,
    kNumPadMul, kNumPadAdd, kNumPadSeparator, kNumPadSub, kNumPadDecimal, kNumPadDiv,

    kF1, kF2, kF3, kF4, kF5, kF6, kF7, kF8, kF9, kF10, kF11, kF12, kF13, kF14, kF15, kF16, kF17, kF18, kF19, kF20, kF21, kF22, kF23, kF24,

    kNumLock = $90, kScrollLock,

    kWheelUp = $97, kWheelDown //engine defined, according to win api reference these codes are not assigned
  {$ENDIF}
  );

  { TglrInput }

  TglrInput = class
    Touch: array[0..9] of TglrTouch;
    KeyDown: array[Low(TglrKey)..High(TglrKey)] of Boolean;
    lastWheelDelta: Integer;

    procedure Process(aType: TglrInputType; aKey: TglrKey; X, Y, aOtherParam: Integer);
    function GetKeyName(aKey: TglrKey): AnsiString;
    function GetInputTypeName(aType: TglrInputType): AnsiString;
  end;

  {$ENDREGION}

  {$REGION 'Game'}

  TglrGame = class abstract
  public
    procedure OnStart(); virtual; abstract;
    procedure OnFinish(); virtual; abstract;

    procedure OnPause(); virtual; abstract;
    procedure OnResume(); virtual; abstract;
    procedure OnResize(aNewWidth, aNewHeight: Integer); virtual; abstract;

    procedure OnUpdate(const dt: Double); virtual; abstract;
    procedure OnRender(); virtual; abstract;

    procedure OnInput(aType: TglrInputType; aKey: TglrKey; X, Y, aOtherParam: Integer); virtual; abstract;
  end;

  {$ENDREGION}

  {$REGION 'Core'}

  TglrInitParams = record
    X, Y, Width, Height: Integer;
    Caption: AnsiString;
    vSync: Boolean;
  {$IFDEF WINDOWS}
  {$ENDIF}
  end;

  //PglrInitParams = ^TglrInitParams;

  Core = class
  protected
    class var fGame: TglrGame;
    class var fAppView: TglrAppView;
  public
    class var Input: TglrInput;

    class procedure Init(aGame: TglrGame; aInitParams: TglrInitParams);

    class procedure Resize(aNewWidth, aNewHeight: Integer);
    class procedure InputReceived(aType: TglrInputType; aKey: TglrKey; X, Y, aOtherParam: Integer);

    class procedure Loop();
    class procedure Pause();
    class procedure Resume();

    class procedure Update(const dt: Double);
    class procedure RenderAll();

    class procedure DeInit();
  end;

  {$ENDREGION}

  {$REGION 'Scene'}

  { TglrNode }

  TglrNode = class
  protected
    fDir, fRight, fUp, fPos,
    fLastDir, fLastRight, fLastUp: TdfVec3f;
    fParent: TglrNode;
    fAbsMatrix: TdfMat4f;

    procedure SetParent(AValue: TglrNode);
    function GetAbsMatrix: TdfMat4f;
    procedure SetDir(aDir: TdfVec3f);
    procedure SetRight(aRight: TdfVec3f);
    procedure SetUp(aUp: TdfVec3f);
    procedure UpdateModelMatrix(aNewDir, aNewUp, aNewRight: TdfVec3f); virtual;
    procedure RenderChilds(); virtual;
    procedure DoRender(); virtual;
  public
    Visible: Boolean;
    Matrix: TdfMat4f;
    Childs: TglrList;

    constructor Create; virtual;
    destructor Destroy; override;

    property AbsoluteMatrix: TdfMat4f read GetAbsMatrix write fAbsMatrix;
    property Parent: TglrNode read fParent write SetParent;

    property Position: TdfVec3f read fPos write fPos;
    property Up: TdfVec3f read fUp write SetUp;
    property Direction: TdfVec3f read fDir write SetDir;
    property Right: TdfVec3f read fRight write SetRight;

    procedure RenderSelf(); virtual;
  end;

  { TglrCamera }

  TglrCameraProjectionMode = (pmOrtho, pmPerpective);
  TglrCameraTargetMode = (mPoint, mTarget, mFree);

  TglrViewportParams = record
    X, Y, W, H: Integer;
    FOV, ZNear, ZFar: Single;
  end;

  TglrCamera = class (TglrNode)
  protected
    fProjMode: TglrCameraProjectionMode;
    fProjMatrix: TdfMat4f;
    fMode: TglrCameraTargetMode;
    fTargetPoint: TdfVec3f;
    fTarget: TglrNode;
    fFOV, fZNear, fZFar: Single;
    fX, fY, fW, fH: Integer;
    procedure SetProjMode(aMode: TglrCameraProjectionMode);
  public
    procedure Viewport(x, y, w, h: Integer; FOV, ZNear, ZFar: Single);
    procedure ViewportOnly(x, y, w, h: Integer);

    procedure Translate(alongUpVector, alongRightVector: Single);
    procedure Scale(aScale: Single);
    procedure Rotate(delta: Single; Axis: TdfVec3f);

    function GetViewport(): TglrViewportParams;

    procedure Update;

    procedure SetCamera(aPos, aTargetPos, aUp: TdfVec3f);
//    procedure SetTarget(aPoint: TdfVec3f); overload;
//    procedure SetTarget(aTarget: IglrNode); overload;

    property ProjectionMode: TglrCameraProjectionMode read fProjMode write SetProjMode;
  end;

  { TglrScene }

  TglrScene = class
  protected
    fOwnCamera: Boolean;
  public
    Root: TglrNode;
    Camera: TglrCamera;

    constructor Create(aCreateCamera: Boolean = True); virtual;
    destructor Destroy(); override;

    procedure Render(); virtual;
  end;

  {$ENDREGION}
implementation

uses
{$IFDEF WINDOWS}
  sys_win,
{$ENDIF}
  resload;

const
  VF_STRIDE: array[Low(TglrVertexFormat)..High(TglrVertexFormat)] of Integer =
    (SizeOf(TglrVertexP2T2), SizeOf(TglrVertexP3T2));
  IF_STRIDE: array[Low(TglrIndexFormat)..High(TglrIndexFormat)] of Integer =
    (SizeOf(Byte), SizeOf(ShortInt), SizeOf(Integer));

  comparison: array[Low(TglrFuncComparison)..High(TglrFuncComparison)] of TGLConst =
    (GL_NEVER, GL_LESS, GL_EQUAL, GL_LEQUAL, GL_GREATER, GL_NOTEQUAL, GL_GEQUAL, GL_ALWAYS);

  aWraps: array[Low(TglrTexWrap)..High(TglrTexWrap)] of TGLConst =
    (GL_CLAMP, GL_REPEAT, GL_CLAMP_TO_EDGE, GL_CLAMP_TO_BORDER, GL_MIRRORED_REPEAT);
  aTextureMode: array[Low(TglrTexCombineMode)..High(TglrTexCombineMode)] of TGLConst =
    (GL_DECAL, GL_MODULATE, GL_BLEND, GL_REPLACE, GL_ADD);

{ TglrCamera }

procedure TglrCamera.SetProjMode(aMode: TglrCameraProjectionMode);
begin
  fProjMatrix.Identity;
  case aMode of
    pmPerpective:
      fProjMatrix.Perspective(FFOV, FW / FH, FZNear, FZFar);
    pmOrtho:
      fProjMatrix.Ortho(FX, FW, FH, FY, FZNear, FZFar);
  end;
  fProjMode := aMode;
  gl.Viewport(fX, fY, fW, fH);
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

  ProjectionMode := fProjMode; //Обновляем
end;

procedure TglrCamera.ViewportOnly(x, y, w, h: Integer);
begin
  Viewport(x, y, w, h, fFOV, fZNear, fZFar);
end;

procedure TglrCamera.Translate(alongUpVector, alongRightVector: Single);
var
  v: TdfVec3f;
begin
  v := Up * alongUpVector;
  v := v + (Right * alongRightVector);
  Matrix.Translate(v);
  fPos := Matrix.Pos.NegateVector;
end;

procedure TglrCamera.Scale(aScale: Single);
begin
  Matrix.Scale(dfVec3f(aScale, aScale, aScale));
end;

procedure TglrCamera.Rotate(delta: Single; Axis: TdfVec3f);
begin
  Matrix.Rotate(delta, Axis);
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

procedure TglrCamera.Update;
begin
  Matrix.Pos := dfVec3f(0, 0, 0);
  Matrix.Pos := Matrix * fPos.NegateVector;
  Render.Params.ViewProj := fProjMatrix * Matrix;
end;

procedure TglrCamera.SetCamera(aPos, aTargetPos, aUp: TdfVec3f);
var
  vDir, vUp, vLeft: TdfVec3f;
begin
  Matrix.Identity;
  vUp := aUp;
  vUp.Normalize;
  vDir := aTargetPos - aPos;
  vDir.Normalize;
  vLeft := vDir.Cross(vUp);
  vLeft.Normalize;
  vUp := vLeft.Cross(vDir);
  vUp.Normalize;

  fPos := aPos;
  fRight := vLeft;
  fUp   := vUp;
  fDir  := vDir;

  vDir.Negate;

  with Matrix do
  begin
    e00 := vLeft.x;  e10 := vLeft.y;  e20 := vLeft.z;  e30 := 0;
    e01 := vUp.x;    e11 := vUp.y;    e21 := vUp.z;    e31 := 0;
    e02 := vDir.x;   e12 := vDir.y;   e22 := vDir.z;   e32 := 0;
    e03 := 0;        e13 := 0;        e23 := 0;        e33 := 1;
  end;

  Matrix := Matrix.Transpose();
  aPos.Negate;
  aPos := Matrix * aPos;
  with Matrix do
  begin
    e03 := aPos.x;        e13 := aPos.y;        e23 := aPos.z;        e33 := 1;
  end;

  fTargetPoint := aTargetPos;
  fMode := mPoint;
end;

{ TglrScene }

constructor TglrScene.Create(aCreateCamera: Boolean);
begin
  inherited Create();
  fOwnCamera := aCreateCamera;
  if (fOwnCamera) then
    Camera := TglrCamera.Create();
  Root := TglrNode.Create();
end;

destructor TglrScene.Destroy;
begin
  if (fOwnCamera) then
    Camera.Free();
  Root.Free();
  inherited Destroy;
end;

procedure TglrScene.Render;
begin
  Assert(Assigned(Camera), 'No camera assigned to scene');

  Camera.Update();
  Root.RenderSelf();
end;

{ Convert }

class function Convert.ToStringA(aVal: Integer): AnsiString;
begin
  Assert(False, 'Not implemented');
end;

class function Convert.ToStringA(aVal: Single): AnsiString;
begin
  Assert(False, 'Not implemented');
end;

class function Convert.ToStringW(aVal: Integer): UnicodeString;
begin
  Assert(False, 'Not implemented');
end;

class function Convert.ToStringW(aVal: Single): UnicodeString;
begin
  Assert(False, 'Not implemented');
end;

class function Convert.ToInt(aStr: AnsiString): Integer;
begin
  Assert(False, 'Not implemented');
end;

class function Convert.ToInt(aStr: UnicodeString): Integer;
begin
  Assert(False, 'Not implemented');
end;

class function Convert.ToFloat(aStr: AnsiString): Single;
begin
  Assert(False, 'Not implemented');
end;

class function Convert.ToFloat(aStr: UnicodeString): Single;
begin
  Assert(False, 'Not implemented');
end;

{ TglrNode }

procedure TglrNode.SetParent(AValue: TglrNode);
begin
  if (fParent = AValue) then
    Exit();
  fParent := AValue;
  GetAbsMatrix();
end;

function TglrNode.GetAbsMatrix: TdfMat4f;
begin
  if Assigned(fParent) then
    fAbsMatrix := fParent.AbsoluteMatrix * Matrix
  else
    fAbsMatrix := Matrix;
  Exit(fAbsMatrix);
end;

procedure TglrNode.SetDir(aDir: TdfVec3f);
var
  NewUp, NewRight: TdfVec3f;
begin
  if (fDir = aDir) then
    Exit;
  NewRight := fUp.Cross(aDir);
  NewRight.Negate;
  NewRight.Normalize;
  NewUp := aDir.Cross(NewRight);
  NewUp.Normalize;
  UpdateModelMatrix(aDir, NewUp, NewRight);
end;

procedure TglrNode.SetRight(aRight: TdfVec3f);
var
  NewDir, NewUp: TdfVec3f;
begin
  if (fRight = aRight) then
    Exit();
  NewDir := aRight.Cross(fUp);
  NewDir.Normalize;
  NewUp := NewDir.Cross(aRight);
  NewUp.Normalize;
  UpdateModelMatrix(NewDir, NewUp, aRight);
end;

procedure TglrNode.SetUp(aUp: TdfVec3f);
var
  NewDir, NewRight: TdfVec3f;
begin
  if (fUp = aUp) then
    Exit();
  NewRight := aUp.Cross(fDir);
  NewRight.Negate;
  NewRight.Normalize;
  NewDir := NewRight.Cross(aUp);
  NewDir.Normalize;
  UpdateModelMatrix(NewDir, aUp, NewRight);
end;

procedure TglrNode.UpdateModelMatrix(aNewDir, aNewUp, aNewRight: TdfVec3f);
begin
  with Matrix do
  begin
    e00 := aNewRight.x; e01 := aNewRight.y; e02 := aNewRight.z; e03 := FPos.Dot(aNewRight);
    e10 := aNewUp.x;    e11 := aNewUp.y;    e12 := aNewUp.z;    e13 := FPos.Dot(aNewUp);
    e20 := aNewDir.x;   e21 := aNewDir.y;   e22 := aNewDir.z;   e23 := FPos.Dot(aNewDir);
    e30 := 0;           e31 := 0;           e32 := 0;           e33 := 1;
  end;
  fRight := aNewRight;
  fUp   := aNewUp;
  fDir  := aNewDir;
  fLastDir := fDir;
  fLastRight := fRight;
  fLastUp := fUp;

  GetAbsMatrix();
end;

procedure TglrNode.RenderChilds;
var
  i: Integer;
begin
  for i := 0 to Childs.Count - 1 do
    TglrNode(Childs[i]).RenderSelf;
end;

procedure TglrNode.DoRender;
begin
  //nothing
end;

constructor TglrNode.Create;
begin
  inherited Create();
  Childs := TglrList.Create();
  Matrix.Identity;
  Visible := True;
  Parent := nil;
  Right := dfVec3f(1, 0, 0);
  Up := dfVec3f(0, 1, 0);
  Direction := dfVec3f(0, 0, 1);
end;

destructor TglrNode.Destroy;
begin
  if (Parent <> nil) then
    Parent.Childs.Delete(Self, False);
  Childs.Free(True);
  inherited Destroy;
end;

procedure TglrNode.RenderSelf();
begin
  if (Direction <> fLastDir) or (Up <> fLastUp) or (Right <> fLastRight) then
    UpdateModelMatrix(fDir, fUp, fRight);
  Matrix.Pos := fPos;

  if (not Visible) then
    Exit();
  DoRender();
  RenderChilds();
end;

{ FileSystem }

class function FileSystem.GetResource(aFileName: AnsiString): TglrStream;
begin
  {$IFDEF WINDOWS}
  if (FileExists(aFileName)) then
  begin
    Result := TglrStream.Init(aFileName);
  end;
  {$ENDIF}
end;

{ Core }

class procedure Core.Resize(aNewWidth, aNewHeight: Integer);
begin
  Render.Resize(aNewWidth, aNewHeight);
  fGame.OnResize(aNewWidth, aNewHeight);
end;

class procedure Core.InputReceived(aType: TglrInputType; aKey: TglrKey; X, Y,
  aOtherParam: Integer);
begin
  Input.Process(aType, aKey, X, Y, aOtherParam);
  fGame.OnInput(aType, aKey, X, Y, aOtherParam);
end;

class procedure Core.Init(aGame: TglrGame; aInitParams: TglrInitParams);
begin
  fGame := aGame;
  Input := TglrInput.Create();

  fAppView :=
  {$IFDEF WINDOWS}TglrWindow{$ENDIF}
    .Create(@aInitParams);
  Render.Init();
  Render.SetClearColor(0.2, 0.21, 0.25);
  Render.SetViewPort(0, 0, aInitParams.Width, aInitParams.Height);
end;

class procedure Core.Loop();
begin
  fGame.OnStart();
  fAppView.Loop(); //main loop is here
  fGame.OnFinish();
end;

class procedure Core.Pause();
begin
  fGame.OnPause();
end;

class procedure Core.Resume();
begin
  fGame.OnResume();
end;

class procedure Core.Update(const dt: Double);
begin
  fGame.OnUpdate(dt);
end;

class procedure Core.RenderAll();
begin
  Render.Clear(cmAll);
  Render.ResetStates();
  Render.fStatTextureBind := 0;
  fGame.OnRender();
end;

class procedure Core.DeInit();
begin
  fAppView.Free();
  Render.DeInit();
  Input.Free();
end;

{ TglrInput }

procedure TglrInput.Process(aType: TglrInputType; aKey: TglrKey; X, Y,
  aOtherParam: Integer);
var
  t: ^TglrTouch;
  k: ^Boolean;
begin
  t := @Touch[Ord(aKey)];
  k := @KeyDown[aKey];
  case aType of
    itTouchDown:
      with t^ do
      begin
        IsDown := True;
        Start := dfVec2f(X, Y);
        Pos := Start;
      end;
    itTouchUp:
      t^.IsDown := False;
    itTouchMove:
      t^.Pos := dfVec2f(X, Y);
    itKeyDown:
      k^ := True;
    itKeyUp:
      k^ := False;
    itWheel:
      lastWheelDelta := aOtherParam;
  end;
end;

function TglrInput.GetKeyName(aKey: TglrKey): AnsiString;
var
  l: Integer;
begin
  WriteStr(Result, aKey);
  l := Length(Result) - 1;
  Move(Result[2], Result[1], l);
  SetLength(Result, l);
end;

function TglrInput.GetInputTypeName(aType: TglrInputType): AnsiString;
var
  l: Integer;
begin
  WriteStr(Result, aType);
  l := Length(Result) - 2;
  Move(Result[3], Result[1], l);
  SetLength(Result, l);
end;

{ Render }

class procedure Render.Init;
begin
  gl.Init();
end;

class procedure Render.DeInit;
begin
  gl.Free();
end;

class procedure Render.Resize(aWidth, aHeight: Integer);
begin
  SetViewPort(0, 0, aWidth, aHeight);
  fWidth := aWidth;
  fHeight := aHeight;
end;

class procedure Render.ResetStates;
begin
  SetCullMode(cmBack);
  SetBlendingMode(bmAlpha);
  SetDepthFunc(fcLessOrEqual);
  SetDepthWrite(True);
  SetDepthTest(True);
  SetAlphaTest(fcGreaterOrEqual, 0.0);

  Params.Color := dfVec4f(1, 1, 1, 1);
  Params.ViewProj.Identity;
  Params.Model.Identity;

  gl.Enable(GL_COLOR_MATERIAL);
end;

class procedure Render.Clear(aClearMask: TglrClearMask);
begin
  case aClearMask of
    cmAll:
      gl.Clear(TGLConst(Ord(GL_COLOR_BUFFER_BIT) or Ord(GL_DEPTH_BUFFER_BIT)));
    cmColor:
      gl.Clear(GL_COLOR_BUFFER_BIT);
    cmDepth:
      gl.Clear(GL_DEPTH_BUFFER_BIT);
  end;
end;

class procedure Render.SetClearColor(R, G, B: Single);
begin
  gl.ClearColor(R, G, B, 1.0);
end;

class procedure Render.SetViewPort(aLeft, aTop, aWidth, aHeight: Integer);
begin
  gl.Viewport(aLeft, aTop, aWidth, aHeight);
end;

class procedure Render.SetCullMode(aCullMode: TglrCullMode);
begin
  if (fCullMode = aCullMode) then
    Exit();

  case aCullMode of
    cmNone:
      gl.Disable(GL_CULL_FACE);
    cmFront:
      gl.CullFace(GL_FRONT);
    cmBack:
      gl.CullFace(GL_BACK);
  end;

  if (fCullMode = cmNone) then
    gl.Enable(GL_CULL_FACE);
  fCullMode := aCullMode;
end;

class procedure Render.SetBlendingMode(aBlendingMode: TglrBlendingMode);
begin
  if (fBlendingMode = aBlendingMode) then
    Exit();
  case aBlendingMode of
    bmNone:
      gl.Disable(GL_BLEND);
    bmAlpha:
      gl.BlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    bmAdditive:
      gl.BlendFunc(GL_ONE, GL_ONE);
    bmMultiply:
      gl.BlendFunc(GL_DST_COLOR, GL_ZERO);
    bmScreen:
      gl.BlendFunc(GL_ONE, GL_ONE_MINUS_SRC_COLOR);
  end;

  if (fBlendingMode = bmNone) then
    gl.Enable(GL_BLEND);

  fBlendingMode := aBlendingMode;
end;

class procedure Render.SetLighting(aEnabled: Boolean);
begin
  if (fLight = aEnabled) then
    Exit();
  if (aEnabled) then
    gl.Enable(GL_LIGHTING)
  else
    gl.Disable(GL_LIGHTING);
end;

class procedure Render.SetDepthWrite(aEnabled: Boolean);
begin
  if (aEnabled = fDepthWrite) then
    Exit();
  gl.DepthMask(aEnabled);
  fDepthWrite := aEnabled;
end;

class procedure Render.SetDepthTest(aEnabled: Boolean);
begin
  if (aEnabled = fDepthTest) then
    Exit();
  if (aEnabled) then
    gl.Enable(GL_DEPTH_TEST)
  else
    gl.Disable(GL_DEPTH_TEST);
  fDepthTest := aEnabled;
end;

class procedure Render.SetDepthFunc(aComparison: TglrFuncComparison);
begin
  if (fDepthFunc = aComparison) then
    Exit();
  gl.DepthFunc(comparison[aComparison]);
  fDepthFunc := aComparison;
end;

class procedure Render.SetAlphaTest(aComparison: TglrFuncComparison;
  aValue: Single);
begin
  if (fAlphaFunc = aComparison) or (Abs(fAlphaTest - aValue) < cEPS) then
    Exit();
  gl.AlphaFunc(comparison[aComparison], aValue);
  fAlphaFunc := aComparison;
  fAlphaTest := aValue;
end;

class procedure Render.SetVerticalSync(aEnabled: Boolean);
begin
  gl.SwapInterval(LongInt(aEnabled));
end;

class procedure Render.SetShader(aShader: TglrShaderId);
begin
  Assert(False, 'Not implemented');
end;

class procedure Render.SetTexture(aTexture: TglrTextureId; aSampler: Integer);
begin
  Assert(False, 'Not implemented');
end;

class procedure Render.DrawTriangles(vBuffer: TglrVertexBuffer;
  iBuffer: TglrIndexBuffer; aStart, aVertCount: Integer);
begin
  Assert(False, 'Not implemented');
end;

class procedure Render.DrawPoints(vBuffer: TglrVertexBuffer; aStart,
  aVertCount: Integer);
begin
  Assert(False, 'Not implemented');
end;

{ TglrFrameBuffer }

procedure TglrFrameBuffer.Bind();
begin
  gl.BindFramebuffer(GL_FRAMEBUFFER, Self.Id);
end;

class procedure TglrFrameBuffer.Unbind();
begin
  gl.BindFramebuffer(GL_FRAMEBUFFER, 0);
end;

constructor TglrFrameBuffer.Create;
begin
  gl.GenFramebuffers(1, @Self.Id);
end;

destructor TglrFrameBuffer.Destroy;
begin
  gl.DeleteFramebuffers(1, @Self.Id);
  inherited Destroy;
end;

{ TglrIndexBuffer }

procedure TglrIndexBuffer.Bind;
begin
  gl.BindBuffer(GL_ELEMENT_ARRAY_BUFFER, Self.Id);
end;

class procedure TglrIndexBuffer.Unbind;
begin
  gl.BindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
end;

constructor TglrIndexBuffer.Create(aData: Pointer; aCount: Integer;
  aFormat: TglrIndexFormat);
begin
  gl.GenBuffers(1, @Self.Id);
  Self.Bind();
  gl.BufferData(GL_ELEMENT_ARRAY_BUFFER, IF_STRIDE[aFormat] * aCount, aData, GL_STATIC_DRAW);
  Self.Unbind();
end;

destructor TglrIndexBuffer.Destroy;
begin
  gl.DeleteBuffers(1, @Self.Id);
  inherited Destroy;
end;

{ TglrVertexBuffer }

procedure TglrVertexBuffer.Bind;
begin
  gl.BindBuffer(GL_ARRAY_BUFFER, Self.Id);
end;

class procedure TglrVertexBuffer.Unbind;
begin
  gl.BindBuffer(GL_ARRAY_BUFFER, 0);
end;

constructor TglrVertexBuffer.Create(aData: Pointer; aCount: Integer;
  aFormat: TglrVertexFormat);
begin
  gl.GenBuffers(1, @Self.Id);
  Self.Bind();
  gl.BufferData(GL_ARRAY_BUFFER, VF_STRIDE[aFormat] * aCount, aData, GL_STATIC_DRAW);
  Self.Unbind();
end;

destructor TglrVertexBuffer.Destroy;
begin
  gl.DeleteBuffers(1, @Self.Id);
  inherited Destroy;
end;

{ TglrStream }
class function TglrStream.Init(Memory: Pointer; MemSize: LongInt): TglrStream;
begin
  Result := TglrStream.Create;
  with Result do
  begin
    SType := stMemory;
    Mem   := Memory;
    FSize := MemSize;
    FPos  := 0;
    FBPos := 0;
  end;
end;

class function TglrStream.Init(const FileName: AnsiString; RW: Boolean): TglrStream;
var
  io: Integer;
begin
  Result := TglrStream.Create();
  AssignFile(Result.F, FileName);
  if RW then
    Rewrite(Result.F, 1)
  else
    Reset(Result.F, 1);
  io := IOResult;
  if io = 0 then
  begin
    Result.SType := stFile;
    Result.FSize := FileSize(Result.F);
    Result.FPos  := 0;
    Result.FBPos := 0;
  end
  else
  begin
    Result.Free;
    Result := nil;
  end;
end;

destructor TglrStream.Destroy;
begin
  if SType = stFile then
    CloseFile(F);
end;

procedure TglrStream.SetPos(Value: LongInt);
begin
  FPos := Value;
  if SType = stFile then
    Seek(F, FBPos + FPos);
end;

procedure TglrStream.SetBlock(BPos, BSize: LongInt);
begin
  FSize := BSize;
  FBPos := BPos;
  Pos := 0;
end;

procedure TglrStream.CopyFrom(const Stream: TglrStream);
var
  p : Pointer;
  CPos : LongInt;
begin
  p := GetMemory(Stream.Size);
  CPos := Stream.Pos;
  Stream.Pos := 0;
  Stream.Read(p^, Stream.Size);
  Stream.Pos := CPos;
  Write(p^, Stream.Size);
  FreeMemory(p);
end;

function TglrStream.Read(out Buf; BufSize: LongInt): LongInt;
begin
  if SType = stMemory then
  begin
    Result := Min(FPos + BufSize, FSize) - FPos;
    Move(Mem^, Buf, Result);
  end else
    BlockRead(F, Buf, BufSize, Result);
  Inc(FPos, Result);
end;

function TglrStream.Write(const Buf; BufSize: LongInt): LongInt;
begin
  if SType = stMemory then
  begin
    Result := Min(FPos + BufSize, FSize) - FPos;
    Move(Buf, Mem^, Result);
  end else
    BlockWrite(F, Buf, BufSize, Result);
  Inc(FPos, Result);
  Inc(FSize, Max(0, FPos - FSize));
end;

function TglrStream.ReadAnsi: AnsiString;
var
  Len : Word;
begin
  Read(Len, SizeOf(Len));
  if Len > 0 then
  begin
    SetLength(Result, Len);
    Read(Result[1], Len);
  end else
    Result := '';
end;

procedure TglrStream.WriteAnsi(const Value: AnsiString);
var
  Len : Word;
begin
  Len := Length(Value);
  Write(Len, SizeOf(Len));
  if Len > 0 then
    Write(Value[1], Len);
end;

function TglrStream.ReadUnicode: WideString;
var
  Len : Word;
begin
  Read(Len, SizeOf(Len));
  SetLength(Result, Len);
  Read(Result[1], Len * 2);
end;

procedure TglrStream.WriteUnicode(const Value: WideString);
var
  Len : Word;
begin
  Len := Length(Value);
  Write(Len, SizeOf(Len));
  Write(Value[1], Len * 2);
end;


{ TglrTexture }

procedure TglrTexture.SetWrapS(aWrap: TglrTexWrap);
begin
  gl.BindTexture(Target, Self.Id);
  gl.TexParameteri(Target, GL_TEXTURE_WRAP_S, Ord(aWraps[aWrap]));
  gl.BindTexture(Target, 0);
end;

procedure TglrTexture.SetWrapT(aWrap: TglrTexWrap);
begin
  gl.BindTexture(Target, Self.Id);
  gl.TexParameteri(Target, GL_TEXTURE_WRAP_T, Ord(aWraps[aWrap]));
  gl.BindTexture(Target, 0);
end;

procedure TglrTexture.SetWrapR(aWrap: TglrTexWrap);
begin
  gl.BindTexture(Target, Self.Id);
  gl.TexParameteri(Target, GL_TEXTURE_WRAP_R, Ord(aWraps[aWrap]));
  gl.BindTexture(Target, 0);
end;

procedure TglrTexture.SetCombineMode(aCombineMode: TglrTexCombineMode);
begin
  gl.BindTexture(Target, Self.Id);
  gl.TexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, aTextureMode[aCombineMode]);
  gl.BindTexture(Target, 0);
end;

constructor TglrTexture.CreateFromFileStream(aStream: TglrStream;
  aExt: AnsiString);
var
  data: Pointer;
  iFormat, cFormat, dType: TGLConst;
  pSize, anisotropy: Integer;

begin
  gl.GenTextures(1, @Self.Id);
  Target := GL_TEXTURE_2D;
  gl.BindTexture(Target, Self.Id);

  gl.TexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, Ord(GL_LINEAR));
  gl.TexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, Ord(GL_LINEAR));

  gl.TexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, Ord(GL_REPEAT));
  gl.TexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, Ord(GL_REPEAT));

  gl.GetIntegerv(GL_MAX_TEXTURE_MAX_ANISOTROPY, @anisotropy);
  gl.TexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAX_ANISOTROPY, anisotropy);

  //todo: combine mode ?

  New(data);
  data := LoadTexture(aStream, aExt, iFormat, cFormat, dType, pSize, Self.Width, Self.Height); //TexLoad.LoadTexture(aFileName, Format, W, H);
  //FTex.FullSize := SizeOfP(Data);
  X := 0;
  Y := 0;
  RegionWidth := Width;
  RegionHeight := Height;
  gl.TexImage2D(GL_TEXTURE_2D, 0, iFormat, Width, Height, 0, cFormat, dType, data);

  gl.BindTexture(GL_TEXTURE_2D, 0);
//  logWriteMessage('Загрузка текстуры завершена. ID = ' + IntToStr(FTex.Id) +
//    ' Размер текстуры: ' + IntToStr(FTex.Width) + 'x' + IntToStr(FTex.Height) + '; ' + IntToStr(FTex.FullSize) + ' байт');
  Dispose(data);
end;

destructor TglrTexture.Destroy();
begin
  gl.DeleteTextures(1, @Self.Id);
  inherited Destroy;
end;

procedure TglrList.Init(Capacity: LongInt);
begin
  FItems := nil;
  FCount := 0;
  FCapacity := Capacity;
end;

procedure TglrList.Free(FreeItems: Boolean);
var
  i : LongInt;
begin
  if FreeItems then
    for i := 0 to Count - 1 do
      TObject(FItems[i]).Free;
  FItems := nil;
  FCount := 0;
end;

procedure TglrList.BoundsCheck(Index: LongInt);
begin
  Assert((Index > 0) or (Index <= FCount), 'List index out of bounds (' + Convert.ToStringA(Index) + ')');
end;

function TglrList.GetItem(Index: LongInt): Pointer;
begin
  BoundsCheck(Index);
  Result := FItems[Index];
end;

procedure TglrList.SetItem(Index: LongInt; Value: Pointer);
begin
  BoundsCheck(Index);
  FItems[Index] := Value;
end;

function TglrList.IndexOf(Item: Pointer): LongInt;
var
  i : LongInt;
begin
  for i := 0 to FCount - 1 do
    if FItems[i] = Item then
    begin
      Result := i;
      Exit;
    end;
  Result := -1;
end;

function TglrList.Add(Item: Pointer): LongInt;
begin
  if FCount mod FCapacity = 0 then
    SetLength(FItems, Length(FItems) + FCapacity);
  FItems[FCount] := Item;
  Result := FCount;
  Inc(FCount);
end;

procedure TglrList.Delete(Index: LongInt; FreeItem: Boolean);
begin
  BoundsCheck(Index);
  if FreeItem then
    TObject(FItems[Index]).Free;
  Move(FItems[Index + 1], FItems[Index], (FCount - Index - 1) * SizeOf(FItems[0]));
  Dec(FCount);
  if Length(FItems) - FCount + 1 > FCapacity then
    SetLength(FItems, Length(FItems) - FCapacity);
end;

procedure TglrList.Delete(Item: Pointer; FreeItem: Boolean);
begin
  Delete(IndexOf(Item), FreeItem);
end;

procedure TglrList.Insert(Index: LongInt; Item: Pointer);
begin
  BoundsCheck(Index);
  Add(nil);
  Move(FItems[Index], FItems[Index + 1], (FCount - Index - 1) * SizeOf(FItems[0]));
  FItems[Index] := Item;
end;

procedure TglrList.Sort(CompareFunc: TglrListCompareFunc);

  procedure SortFragment(L, R: LongInt);
  var
    i, j : Integer;
    P, T : Pointer;
  begin
    repeat
      i := L;
      j := R;
      P := FItems[(L + R) div 2];
      repeat
        while CompareFunc(FItems[i], P) < 0 do
          Inc(i);
        while CompareFunc(FItems[j], P) > 0 do
          Dec(j);
        if i <= j then
        begin
          T := FItems[i];
          FItems[i] := FItems[j];
          FItems[j] := T;
          Inc(i);
          Dec(j);
        end;
      until i > j;
      if L < j then
        SortFragment(L, j);
      L := i;
    until i >= R;
  end;

begin
  if FCount > 1 then
    SortFragment(0, FCount - 1);
end;

end.
