{
  TODO:
    - Базовый шейдер
    - Класс батча?
    - Спрайт, меш
}

unit tinyglr;

{$define log}
{$Assertions on}

interface

uses
  ogl, glrMath;

const
  TINYGLR_VERSION = '0.1';
  LOG_FILE = 'tinyglr.log';

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

  TglrList<T> = class
    procedure Init(Capacity: LongInt);
    procedure Free(FreeItems: Boolean = False);
  private
    FItems    : array of T;
    FCount    : LongInt;
    FCapacity : LongInt;
    procedure BoundsCheck(Index: LongInt);
    function GetItem(Index: LongInt): T; inline;
    procedure SetItem(Index: LongInt; Value: T); inline;
    procedure SortFragment(CompareFunc: TglrListCompareFunc; L, R: LongInt);
  public
    constructor Create(aCapacity: LongInt = 1); virtual;

    function IndexOf(Item: T): LongInt;
    function Add(Item: T): LongInt;
    procedure DeleteByIndex(Index: LongInt; FreeItem: Boolean = False);
    procedure Delete(Item: T; FreeItem: Boolean = False);
    procedure DeleteSafe(Item: T; FreeItem: Boolean = False);
    procedure DeleteSafeByIndex(Index: LongInt; FreeItem: Boolean = False);
    procedure Insert(Index: LongInt; Item: T);
    procedure Sort(CompareFunc: TglrListCompareFunc);
    property Count: LongInt read FCount;
    property Items[Index: LongInt]: T read GetItem write SetItem; default;
  end;

  TglrStringList = TglrList<AnsiString>;

  { FileSystem }

const
  PACK_FILE_MAGIC: Word = $0F86;
type
  NameString = String[255];

  TglrPackFileResource = packed record
    fFileName: NameString;
    fStride, fSize: LongWord;
  end;

  FileSystem = class
  protected
    type
      TglrPackFile = packed record
        fPackName: AnsiString;
        fFiles: array of TglrPackFileResource;
        fLoaded: Boolean;
        fPackData: TglrStream;
        fPackDataPointer: Pointer;
      end;

    var
      class var fPackFilesPath: AnsiString;
      class var fPackFiles: array of TglrPackFile;

    class procedure Init(const aPackFilesPath: AnsiString);
    class procedure DeInit();

    class function IsPackContainsFile(packIndex: Integer; aFileName: AnsiString): Boolean;
    class function GetPackIndexByPackName(const aPackName: AnsiString): Integer;
  public
    class procedure LoadPack(const aPackFileName: AnsiString); //loads entire pack file into memory
    class procedure UnloadPack(const aPackFileName: AnsiString);
    class function ReadResource(const aFileName: AnsiString; aSearchInPackFiles: Boolean = True): TglrStream;
    class procedure WriteResource(const aFileName: AnsiString; const aStream: TglrStream); overload;
    class procedure WriteResource(const aFileName: AnsiString; const aContent: AnsiString); overload;
  end;

  TglrLogMessageType = (lInformation, lWarning, lError, lCritical);

  { Log }

  Log = class
  protected
    class var f: Text;
  public
    class procedure Init(const aFileName: AnsiString);
    class procedure Deinit();
    class procedure Write(aType: TglrLogMessageType; aMessage: AnsiString); inline;
  end;

  { Convert }

  Convert = class
  public
    class function ToStringA(aVal: Integer): AnsiString; overload;
    class function ToStringA(aVal: Single; Digits: Integer = 5): AnsiString; overload;
    class function ToStringA(aVal: TdfMat4f): AnsiString; overload;
//    class function ToStringW(aVal: Integer): UnicodeString; overload;
//    class function ToStringW(aVal: Single): UnicodeString; overload;
    class function ToInt(aStr: AnsiString; aDefault: Integer = -1): Integer; overload;
//    class function ToInt(aStr: WideString): Integer; overload;
    class function ToFloat(aStr: AnsiString; aDefault: Single = -1.0): Single; overload;
//    class function ToFloat(aStr: WideString): Single; overload;
  end;

  {$ENDREGION}

  {$REGION 'Buffers, Shader, Texture, Render'}

  TglrTextureId = type LongWord;
  TglrShaderProgramId = type LongWord;
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

  TglrVertexAtrib = (vaCoord = 0, vaNormal = 1, vaTexCoord0 = 2, vaTexCoord1 = 3{, ...});

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
    constructor Create(aStream: TglrStream; aExt: AnsiString;
      aFreeStreamOnFinish: Boolean = True); virtual;
    //constructor CreateEmpty2D(aWidth, aHeight, aFormat: TGLConst);
    //todo 1d, 3d
    destructor Destroy(); override;

    procedure Bind(const aSampler: Integer = 0);
    class procedure Unbind();
  end;


  { TglrVertexBuffer }

  TglrVertexBuffer = class
    Id: TglrVertexBufferId;
    vbFormat: TglrVertexFormat;
    procedure Bind();
    class procedure Unbind();
    constructor Create(aData: Pointer; aCount: Integer; aFormat: TglrVertexFormat); virtual;
    destructor Destroy(); override;
  end;

  { TglrIndexBuffer }

  TglrIndexBuffer = class
    Id: TglrIndexBufferId;
    iFormat: TglrIndexFormat;
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

  TglrShaderType = (stVertex, stFragment);

  { TglrShaderProgram }

  TglrShaderProgram = class
  protected
    fLinkStatus: Integer;
    class function GetVertexAtribName(const aAtrib: TglrVertexAtrib): AnsiString;
  public
    Id: TglrShaderProgramId;
    ShadersId: array of TglrShaderId;

    procedure Bind();
    class procedure Unbind();

    procedure LoadAndAttachShader(aStream: TglrStream; aShaderType: TglrShaderType;
      aFreeStreamOnFinish: Boolean = True);
    procedure Link();

    constructor Create(); virtual;
    destructor Destroy(); override;
  end;


  TglrBlendingMode = ( bmNone, bmAlpha, bmAdditive, bmMultiply, bmScreen);
  TglrCullMode = (cmNone, cmBack, cmFront);
  TglrFuncComparison = (fcNever, fcLess, fcEqual, fcLessOrEqual,
    fcGreater, fcNotEqual, fcGreaterOrEqual, fcAlways);
  TglrClearMask = (cmAll, cmColor, cmDepth);

  { Render }

  TglrRenderParams = record
    ViewProj, Model, ModelViewProj: TdfMat4f;
    Color: TdfVec4f;
  end;

const
  TEXURE_SAMPLERS_MAX = 8;

type
  Render = class
  private
  protected
    class var fBlendingMode: TglrBlendingMode;
    class var fCullMode: TglrCullMode;
    class var fDepthWrite, fDepthTest, fLight: Boolean;
    class var fDepthFunc, fAlphaFunc: TglrFuncComparison;
    class var fAlphaTest: Single;
    class var fShader: TglrShaderId;
    class var fTextureSampler: array[0..TEXURE_SAMPLERS_MAX - 1] of TglrTextureId;
    class var fActiveSampler: Integer;
    class var fVB: TglrVertexBufferId;
    class var fIB: TglrIndexBufferId;
    class var fFB: TglrFrameBufferId;

    class var fStatTextureBind, fTriCount, fDIPCount: Integer;
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
    class procedure SetShader(aShader: TglrShaderProgramId);
    class procedure SetTexture(aTexture: TglrTextureId; aSampler: Integer);

    class procedure DrawTriangles(vBuffer: TglrVertexBuffer; iBuffer: TglrIndexBuffer;
      aStart, aVertCount: Integer);
    class procedure DrawPoints(vBuffer: TglrVertexBuffer; aStart, aVertCount: Integer);

    class property TextureBinds: Integer read fStatTextureBind;
    class property TriCount: Integer read fTriCount;
    class property DipCount: Integer read fDIPCount;
    class property Width: Integer read fWidth;
    class property Height: Integer read fHeight;
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
    PackFilesPath: AnsiString;
    UseDefaultAssets: Boolean;
  {$IFDEF WINDOWS}
  {$ENDIF}
  end;

  //PglrInitParams = ^TglrInitParams;

  { Core }

  Core = class
  protected
    class var fGame: TglrGame;
    class var fAppView: TglrAppView;
    class var fFPS: Single;
    class function GetFPSText(): AnsiString; static;
  public
    class var Input: TglrInput;

    class procedure Init(aGame: TglrGame; aInitParams: TglrInitParams);
    class procedure DeInit();

    class procedure Resize(aNewWidth, aNewHeight: Integer);
    class procedure InputReceived(aType: TglrInputType; aKey: TglrKey; X, Y, aOtherParam: Integer);

    class procedure Loop();
    class procedure Pause();
    class procedure Resume();

    class procedure Update(const dt: Double);
    class procedure RenderAll();

    class property FPS: Single read fFPS;
    class property FPSText: AnsiString read GetFPSText; //preformatted with 1 digit after delimiter
  end;

  {$ENDREGION}

  {$REGION 'Node, Camera, Scene'}

  { TglrNode }

  TglrNode = class;
  TglrNodeList = TglrList<TglrNode>;

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
    procedure UpdateVectorsFromMatrix(); virtual;
    procedure RenderChilds(); virtual;
    procedure DoRender(); virtual;
  public
    Visible: Boolean;
    Matrix: TdfMat4f;
    Childs: TglrNodeList;

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

  TglrCameraProjectionMode = (pmOrtho, pmPerspective);
  TglrCameraTargetMode = (mPoint, mTarget, mFree);

  TglrViewportParams = record
    X, Y, W, H: Integer;
    FOV, ZNear, ZFar: Single;
  end;

  TglrCamera = class (TglrNode)
  protected
    fProjMode: TglrCameraProjectionMode;

    fMode: TglrCameraTargetMode;
    fTargetPoint: TdfVec3f;
    fTarget: TglrNode;
    fFOV, fZNear, fZFar: Single;
    fX, fY, fW, fH: Integer;
    procedure SetProjMode(aMode: TglrCameraProjectionMode);
    procedure UpdateVectorsFromMatrix(); override;
  public
    fProjMatrix: TdfMat4f;

    constructor Create(); override;

    procedure Viewport(x, y, w, h: Integer; FOV, ZNear, ZFar: Single);
    procedure ViewportOnly(x, y, w, h: Integer);

    procedure Translate(alongUpVector, alongRightVector, alongDirVector: Single);
    procedure Scale(aScale: Single);
    procedure Rotate(delta: Single; Axis: TdfVec3f);

    function GetViewport(): TglrViewportParams;

    procedure Update;

    procedure SetCamera(aPos, aTargetPos, aUp: TdfVec3f);

    procedure RenderSelf(); override;

//    procedure SetTarget(aPoint: TdfVec3f); overload;
//    procedure SetTarget(aTarget: IglrNode); overload;

    property ProjectionMode: TglrCameraProjectionMode read fProjMode write SetProjMode;
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

  {$ENDREGION}

  {$REGION 'Material, Sprite, Mesh'}

  { TglrMaterial }

  TglrMaterial = class
    Shader: TglrShaderProgram;
    Textures: array of TglrTexture;
    Color: TdfVec4f;
  	Blend: TglrBlendingMode;
  	DepthWrite: Boolean;
    DepthTest: Boolean;
    AlphaTest: TglrFuncComparison;
    AlphaTestValue: Single;
  	Cull: TglrCullMode;

    constructor Create(); virtual; overload;
    constructor Create(aStream: TglrStream;
      aFreeStreamOnFinish: Boolean = True); virtual; overload;
    destructor Destroy(); override;

    procedure Bind();
    class procedure Unbind();
  end;

  {$ENDREGION}

  {$REGION 'Default'}

  { Default }

  Default = class
  protected
    class var fInited: Boolean;
    class procedure Init();
    class procedure Deinit();
  public
    class var SpriteMaterial: TglrMaterial;
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
    (SizeOf(Byte), SizeOf(Word), SizeOf(LongWord));
  IF_FORMAT: array[Low(TglrIndexFormat)..High(TglrIndexFormat)] of TGLConst =
  (GL_UNSIGNED_BYTE, GL_UNSIGNED_SHORT, GL_UNSIGNED_INT);

  comparison: array[Low(TglrFuncComparison)..High(TglrFuncComparison)] of TGLConst =
    (GL_NEVER, GL_LESS, GL_EQUAL, GL_LEQUAL, GL_GREATER, GL_NOTEQUAL, GL_GEQUAL, GL_ALWAYS);

  aWraps: array[Low(TglrTexWrap)..High(TglrTexWrap)] of TGLConst =
    (GL_CLAMP, GL_REPEAT, GL_CLAMP_TO_EDGE, GL_CLAMP_TO_BORDER, GL_MIRRORED_REPEAT);
  aTextureMode: array[Low(TglrTexCombineMode)..High(TglrTexCombineMode)] of TGLConst =
    (GL_DECAL, GL_MODULATE, GL_BLEND, GL_REPLACE, GL_ADD);

{ Log }

class procedure Log.Init(const aFileName: AnsiString);
begin
  {$ifdef log}
  AssignFile(f, aFileName);
  Rewrite(f);
  CloseFile(f);
  Self.Write(lInformation, 'Start. Tiny glr version: ' + TINYGLR_VERSION);
  {$endif}
end;

class procedure Log.Deinit;
begin
  {$ifdef log}
  Self.Write(lInformation, 'End');
  {$endif}
end;

const
  cLOG_MESSAGE_TYPES: array[TglrLogMessageType] of AnsiString =
  ('    ', ' !  ', ' !! ', '!!!!');

class procedure Log.Write(aType: TglrLogMessageType; aMessage: AnsiString);
begin
  {$ifdef log}
  Append(f);
  WriteLn(f, cLOG_MESSAGE_TYPES[aType] + '::'#9 + aMessage);
  CloseFile(f);
  if (aType = lCritical) then
    Assert(False, 'Critical error detected: ' + aMessage);
  {$endif}
end;

{ Default }

class procedure Default.Init;
begin
  SpriteMaterial := TglrMaterial.Create();
  SpriteMaterial.Shader := TglrShaderProgram.Create();
  SpriteMaterial.Shader.LoadAndAttachShader(FileSystem.ReadResource('default assets/SpriteShaderV.txt'), stVertex);
  SpriteMaterial.Shader.LoadAndAttachShader(FileSystem.ReadResource('default assets/SpriteShaderF.txt'), stFragment);
  SpriteMaterial.Shader.Link();
  //todo: shader and everything
  //do it via pack file (default.glrpack for example)
  fInited := True;
end;

class procedure Default.Deinit;
begin
  if (not fInited) then
    Exit();

  SpriteMaterial.Free();
end;

{ TglrMaterial }

constructor TglrMaterial.Create;
begin
  inherited;
  Log.Write(lWarning, 'Material create is not implemented');
end;

constructor TglrMaterial.Create(aStream: TglrStream;
  aFreeStreamOnFinish: Boolean);
begin
  Create();
  Log.Write(lWarning, 'Material create is not implemented');
end;

destructor TglrMaterial.Destroy;
begin
  Log.Write(lWarning, 'Material destroy is not implemented');
  inherited Destroy;
end;

procedure TglrMaterial.Bind;
begin
  Log.Write(lWarning, 'Material bind is not implemented');
end;

class procedure TglrMaterial.Unbind;
begin
  Log.Write(lWarning, 'Material unbind is not implemented');
end;

{ TglrShaderProgram }

class function TglrShaderProgram.GetVertexAtribName(
  const aAtrib: TglrVertexAtrib): AnsiString;
begin
  WriteStr(Result, aAtrib);
end;

procedure TglrShaderProgram.Bind;
var
  param: TGLConst;
  len: LongInt;
  infoLog: PAnsiChar;
begin
  Render.SetShader(Self.Id);

  //gl.GetProgramiv(Self.Id, GL_VALIDATE_STATUS, @param);
  //if (param = GL_FALSE) then
  //begin
  //  gl.GetProgramiv(Self.Id, GL_INFO_LOG_LENGTH, @len);
  //  GetMem(infoLog, len);
  //  gl.GetProgramInfoLog(Self.Id, len, len, infoLog);
  //  Log.Write(lCritical, 'Shader validate failed. Log: #13#10' + InfoLog);
  //  FreeMem(infoLog, len);
  //end;

  with Render.Params do
    ModelViewProj := ViewProj * Model;
  gl.UniformMatrix4fv(gl.GetUniformLocation(Self.Id, 'uModelViewProj'), 1, false, @(Render.Params.ModelViewProj));
end;

class procedure TglrShaderProgram.Unbind;
begin
  Render.SetShader(0);
end;

procedure TglrShaderProgram.LoadAndAttachShader(aStream: TglrStream;
  aShaderType: TglrShaderType; aFreeStreamOnFinish: Boolean);
var
  aType: TGLConst;
  i: Integer;
  param, len: Integer;
  ErrorLog: PAnsiChar;
  data: Pointer;
  v: TglrVertexAtrib;
begin
  case aShaderType of
    stVertex: aType := GL_VERTEX_SHADER;
    stFragment: aType := GL_FRAGMENT_SHADER;
  end;
  i := Length(ShadersId);
  SetLength(ShadersId, i + 1);
  ShadersId[i] := gl.CreateShader(aType);
  data := LoadShader(aStream);
  gl.ShaderSource(ShadersId[i], 1, @data, nil);
  FreeMem(data);
  gl.CompileShader(ShadersId[i]);
  gl.GetShaderiv(ShadersId[i], GL_COMPILE_STATUS, @param);
  if (param = Ord(GL_FALSE)) then
  begin
    gl.GetShaderiv(ShadersId[i], GL_INFO_LOG_LENGTH, @param);
    GetMem(ErrorLog, param);
    gl.GetShaderInfoLog(ShadersId[i], param, len, ErrorLog);
    Log.Write(lCritical, 'Shader compilation failed. Log: #13#10' + ErrorLog);
    FreeMem(ErrorLog, param);
  end;
  if (aFreeStreamOnFinish) then
    aStream.Free();

  gl.AttachShader(Self.Id, ShadersId[i]);

  //Quick fix of nvidia bug - they don't like unused / unprovided attribs enabled and binded

  (*for v := Low(TglrVertexAtrib) to High(TglrVertexAtrib) do
  begin
    gl.EnableVertexAttribArray(Ord(v));
    gl.BindAttribLocation(Self.Id, Ord(v), PAnsiChar(GetVertexAtribName(v)));
  end; *)
  gl.EnableVertexAttribArray(Ord(vaCoord));
  gl.BindAttribLocation(Self.Id, Ord(vaCoord), 'vaCoord');

  //todo: gl.Uniform
end;

procedure TglrShaderProgram.Link;
var
  param: TGLConst;
  len: LongInt;
  infoLog: PAnsiChar;
begin
  gl.LinkProgram(Self.Id);
  gl.GetProgramiv(Self.Id, GL_LINK_STATUS, @param);
  if (param = GL_FALSE) then
  begin
    gl.GetProgramiv(Self.Id, GL_INFO_LOG_LENGTH, @len);
    GetMem(infoLog, len);
    gl.GetProgramInfoLog(Self.Id, len, len, infoLog);
    Log.Write(lCritical, 'Shader link failed. Log: #13#10' + InfoLog);
    FreeMem(infoLog, len);
  end;
end;

constructor TglrShaderProgram.Create;
begin
  inherited;
  Self.Id := gl.CreateProgram();
end;

destructor TglrShaderProgram.Destroy;
var
  i: Integer;
begin
  for i := 0 to High(ShadersId) do
    gl.DeleteShader(ShadersId[i]);
  gl.DeleteProgram(Self.Id);
  inherited Destroy;
end;

{ TglrCamera }

procedure TglrCamera.SetProjMode(aMode: TglrCameraProjectionMode);
begin
  fProjMatrix.Identity;
  case aMode of
    pmPerspective:
      fProjMatrix.Perspective(fFOV, fW / fH, fZNear, fZFar);
    pmOrtho:
      //todo
      //предусмотреть аспект
      fProjMatrix.Ortho(fX, fW, fH, fY, fZNear, fZFar);
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
  fProjMatrix.Identity;
  SetCamera(dfVec3f(0, 0, 10), dfVec3f(0, 0, 0), dfVec3f(0, 1, 0));
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
  v: TdfVec3f;
begin
  v := Up * alongUpVector + Right * alongRightVector + Direction * alongDirVector;
  fPos += v;
  UpdateVectorsFromMatrix();
end;

procedure TglrCamera.Scale(aScale: Single);
begin
  Matrix.Scale(dfVec3f(aScale, aScale, aScale));
end;

procedure TglrCamera.Rotate(delta: Single; Axis: TdfVec3f);
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

procedure TglrCamera.Update;
begin
  //UpdateVectorsFromMatrix();
  Matrix.Pos := dfVec3f(0, 0, 0);
  Matrix.Pos := Matrix * fPos.NegateVector;
  Render.Params.ViewProj := fProjMatrix * Matrix;
  if (Render.Width <> fW) or (Render.Height <> fH) then
    ViewportOnly(0, 0, Render.Width, Render.Height);

  //no shaders, bad, bad, bad
  gl.MatrixMode(GL_PROJECTION);
  gl.LoadMatrixf(fProjMatrix);
  gl.MatrixMode(GL_MODELVIEW);
  gl.LoadMatrixf(Matrix);
end;

procedure TglrCamera.SetCamera(aPos, aTargetPos, aUp: TdfVec3f);
begin
  Matrix.Identity;
  fUp := aUp.Normal();
  fDir := (aTargetPos - aPos).Normal();
  fRight := fDir.Cross(fUp).Normal();
  fUp := fRight.Cross(fDir).Normal();
  fPos := aPos;
  fDir.Negate;

  with Matrix do
  begin
    e00 := fRight.x;  e10 := fRight.y;  e20 := fRight.z;  e30 := -fRight.Dot(fPos);
    e01 := fUp.x;    e11 := fUp.y;    e21 := fUp.z;    e31 := -fUp.Dot(fPos);
    e02 := fDir.x;   e12 := fDir.y;   e22 := fDir.z;   e32 := -fDir.Dot(fPos);
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
    Exit();
    fCameraAssignedLogError := True;
  end
  else
    fCameraAssignedLogError := False;

  Camera.Update();
  Root.RenderSelf();
end;

{ Convert }

class function Convert.ToStringA(aVal: Integer): AnsiString;
begin
  Str(aVal, Result);
end;

class function Convert.ToStringA(aVal: Single; Digits: Integer = 5): AnsiString;
begin
  Str(aVal:0:Digits, Result);
end;

class function Convert.ToStringA(aVal: TdfMat4f): AnsiString;
var
  f: PSingle;
begin
  f := @aVal;
  log.Write(lCritical, 'Convert mat to string is not implemented');
end;

class function Convert.ToInt(aStr: AnsiString; aDefault: Integer): Integer;
var
  Code: Integer;
begin
  Val(aStr, Result, Code);
  if (Code <> 0) then
    Result := aDefault;
end;

class function Convert.ToFloat(aStr: AnsiString; aDefault: Single): Single;
var
  Code: Integer;
begin
  Val(aStr, Result, Code);
  if (Code <> 0) then
    Result := aDefault;
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

procedure TglrNode.UpdateVectorsFromMatrix;
begin
  with Matrix do
  begin
    fDir.x := e20; fDir.y := e21; fDir.z := e22;
    fUp.x := e10; fUp.y := e11; fUp.z := e12;
    fRight.x := e00; fRight.y := e01; fRight.z := e02;
  end;
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
  Childs := TglrNodeList.Create();
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
  Matrix.Pos := fPos;
  Render.Params.Model := GetAbsMatrix;

  UpdateVectorsFromMatrix();

  if (not Visible) then
    Exit();
  DoRender();
  RenderChilds();
end;

{ FileSystem }

class procedure FileSystem.Init(const aPackFilesPath: AnsiString);
var
  packFilesList: TglrStringList;
  i, l, j: Integer;
  stream: TglrStream;
  WordBuf, bytesRead: Word;
begin
  fPackFilesPath := aPackFilesPath;
//  if (fPackFilesPath[Length(fPackFilesPath)] in ['/', '\']) then
//    fPackFilesPath := Copy(fPackFilesPath, 0, Length(fPackFilesPath) - 1);
  packFilesList := TglrStringList.Create();
  FindFiles(fPackFilesPath, '.glrpack', packFilesList);
  Log.Write(lInformation, 'FileSystem: pack files found at "' + fPackFilesPath + '": ' + Convert.ToStringA(packFilesList.Count));
  SetLength(fPackFiles, packFilesList.Count);
  l := 0;
  for i := 0 to packFilesList.Count - 1 do
  begin
    stream := FileSystem.ReadResource(packFilesList[i], False);
    bytesRead := stream.Read(WordBuf, SizeOf(Word));
    if (WordBuf <> PACK_FILE_MAGIC) or (bytesRead < SizeOf(Word)) then
    begin
      Log.Write(lError, #9 + packFilesList[i] + ': not a correct pack file');
      SetLength(fPackFiles, Length(fPackFiles) - 1);
      stream.Free();
      continue;
    end;
    bytesRead := stream.Read(WordBuf, SizeOf(Word));
    if (bytesRead <> SizeOf(Word)) then
    begin
      log.Write(lError, #9 + packFilesList[i] + ': error while read file count');
      SetLength(fPackFiles, Length(fPackFiles) - 1);
      stream.Free();
      continue;
    end;
    fPackFiles[l].fPackName := packFilesList[i];
    fPackFiles[l].fLoaded := False;

    SetLength(fPackFiles[l].fFiles, WordBuf);
    for j := 0 to WordBuf - 1 do
    begin
      fPackFiles[l].fFiles[j].fFileName := stream.ReadAnsi();
      stream.Read(fPackFiles[l].fFiles[j].fStride, SizeOf(LongWord));
      stream.Read(fPackFiles[l].fFiles[j].fSize, SizeOf(LongWord));
    end;
    (*
    bytesRead := stream.Read(fPackFiles[l].fFiles[0], SizeOf(TglrPackFileResource) * WordBuf);
    if (bytesRead <> SizeOf(TglrPackFileResource) * WordBuf) then
    begin
      log.Write(lError, #9 + packFilesList[i] + ': error while read file headers');
      SetLength(fPackFiles, Length(fPackFiles) - 1);
      stream.Free();
      continue;
    end;
    *)
    Log.Write(lInformation, #9 + packFilesList[i] + ': header loaded. Files inside: ' + Convert.ToStringA(WordBuf));
    for j := 0 to Length(fPackFiles[l].fFiles) - 1 do
      Log.Write(lInformation, #9#9 + fPackFiles[l].fFiles[j].fFileName + ' - ' + Convert.ToStringA(Integer(fPackFiles[l].fFiles[j].fSize)));
    stream.Free();
    l += 1;
  end;
  packFilesList.Free();

  //Log.Write(lWarning, 'FileSystem.Init has no full implementation for pack load');
end;

class procedure FileSystem.DeInit;
var
  i: Integer;
begin
  for i := 0 to High(fPackFiles) do
    if (fPackFiles[i].fLoaded) then
      UnloadPack(fPackFiles[i].fPackName);
  SetLength(fPackFiles, 0);
//  Log.Write(lWarning, 'FileSystem.DeInit has no implementation for pack unload');
end;

class function FileSystem.IsPackContainsFile(packIndex: Integer;
  aFileName: AnsiString): Boolean;
var
  i: Integer;
begin
  Result := False;
  if (packIndex < 0) or (packIndex > High(fPackFiles)) then
  begin
    Log.Write(lError, 'Wrong pack index provided: ' + Convert.ToStringA(packIndex)
      + '. Bounds: 0..' + Convert.ToStringA(High(fPackFiles)));
    Exit();
  end;

  for i := 0 to High(fPackFiles[packIndex].fFiles) do
    if (fPackFiles[packIndex].fFiles[i].fFileName = aFileName) then
      Exit(True);
end;

class function FileSystem.GetPackIndexByPackName(const aPackName: AnsiString): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Length(fPackFiles) - 1 do
    if (fPackFiles[i].fPackName = aPackName) then
      Exit(i);
  //Log.Write(lError, 'FileSystem: Pack "' + aPackName + '" was not found');
end;

class procedure FileSystem.LoadPack(const aPackFileName: AnsiString);
var
  i: Integer;
  FileStream: TglrStream;
begin
  i := GetPackIndexByPackName(aPackFileName);
  if (i = -1) then
    Log.Write(lError, 'FileSystem: Unable to load pack "' + aPackFileName + '". No pack was found.')
  else
  begin
    FileStream := TglrStream.Init(aPackFileName);
    GetMem(fPackFiles[i].fPackDataPointer, FileStream.Size);
    fPackFiles[i].fPackData := TglrStream.Init(fPackFiles[i].fPackDataPointer, FileStream.Size);
    fPackFiles[i].fPackData.CopyFrom(FileStream);
    fPackFiles[i].fLoaded := True;
    FileStream.Free();
    Log.Write(lInformation, 'FileSystem: Load pack "' + aPackFileName + '" is completed');
  end;
end;

class procedure FileSystem.UnloadPack(const aPackFileName: AnsiString);
var
  i: Integer;
  FileStream: TglrStream;
begin
  i := GetPackIndexByPackName(aPackFileName);
  if (i = -1) then
    Log.Write(lError, 'FileSystem: Unable to unload pack "' + aPackFileName + '". No pack was found.')
  else
  begin
    FreeMem(fPackFiles[i].fPackDataPointer, fPackFiles[i].fPackData.Size);
    fPackFiles[i].fPackData.Free();
    fPackFiles[i].fLoaded := False;
    Log.Write(lInformation, 'FileSystem: Unload pack "' + aPackFileName + '" is completed');
  end;
end;

class function FileSystem.ReadResource(const aFileName: AnsiString;
  aSearchInPackFiles: Boolean): TglrStream;
var
  i: Integer;
  PackFile: TglrStream;
begin
  if (FileExists(aFileName)) then
    Exit(TglrStream.Init(aFileName))
  else if (aSearchInPackFiles) then;
    for i := 0 to Length(fPackFiles) - 1 do
      if IsPackContainsFile(i, aFileName) then
      begin
        if (not fPackFiles[i].fLoaded) then
        begin
          PackFile := TglrStream.Init(aFileName);
          //load pack file from file directly
          Log.Write(lError, 'FileSystem.ReadResource from pack file directly is not implemented');
          PackFile.Free();
        end;
        Log.Write(lError, 'FileSystem.ReadResource from pack file memory is not implemented');
        //load pack file from fPackData (memory)
      end;
  Log.Write(lError, 'FileSystem: requested resource "' + aFileName + '" was not found');
end;

class procedure FileSystem.WriteResource(const aFileName: AnsiString;
  const aStream: TglrStream);
var
  FileStream: TglrStream;
begin
  FileStream := TglrStream.Init(aFileName, True);
  FileStream.CopyFrom(aStream);
  FileStream.Free();
end;

class procedure FileSystem.WriteResource(const aFileName: AnsiString;
  const aContent: AnsiString);
var
  t: Text;
begin
  AssignFile(t, aFileName);
  Rewrite(t);
  Write(t, aContent);
  CloseFile(t);
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

type
  EAssertationFailed = class(TObject)
  private
    fMessage: AnsiString;
  public
    constructor Create(const aMsg: Ansistring);
  end;

  constructor EAssertationFailed.Create(const aMsg: Ansistring);
  begin
    inherited Create();
    fMessage := aMsg;
  end;

procedure AssertErrorHandler(const aMessage, aFileName: ShortString; aLineNo: LongInt; aAddr: Pointer);
begin
  raise EAssertationFailed.Create(
    #13#10 + aMessage +
    #13#10 + 'file: ' + aFileName +
    #13#10 + 'line: ' + Convert.ToStringA(aLineNo) +
    #13#10 + 'addr: ' + Convert.ToStringA(Integer(aAddr)));
end;

class function Core.GetFPSText: AnsiString;
begin
  Result := Convert.ToStringA(fFPS, 1);
end;

class procedure Core.Init(aGame: TglrGame; aInitParams: TglrInitParams);
begin
  AssertErrorProc := @AssertErrorHandler;
  fGame := aGame;
  Log.Init(LOG_FILE);
  Input := TglrInput.Create();
  FileSystem.Init(aInitParams.PackFilesPath);

  fAppView :=
  {$IFDEF WINDOWS}TglrWindow{$ENDIF}
    .Create(@aInitParams);
  Render.Init();
  Render.SetVerticalSync(aInitParams.vSync);
  Render.SetClearColor(0.2, 0.21, 0.25);
  Render.SetViewPort(0, 0, aInitParams.Width, aInitParams.Height);
  if (aInitParams.UseDefaultAssets) then
    Default.Init();
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
  fFPS := 1 / dt;
  fGame.OnUpdate(dt);
end;

class procedure Core.RenderAll();
begin
  Render.Clear(cmAll);
  Render.ResetStates();
  Render.fStatTextureBind := 0;
  Render.fTriCount := 0;
  Render.fDIPCount := 0;
  fGame.OnRender();
end;

class procedure Core.DeInit();
begin
  Default.Deinit();
  fAppView.Free();
  Render.DeInit();
  FileSystem.DeInit();
  Input.Free();
  Log.Deinit();
end;

{ TglrInput }

procedure TglrInput.Process(aType: TglrInputType; aKey: TglrKey; X, Y,
  aOtherParam: Integer);
var
  k: ^Boolean;
begin
  k := @KeyDown[aKey];
  case aType of
    itTouchDown:
      with Touch[Ord(aKey)] do
      begin
        IsDown := True;
        Start := dfVec2f(X, Y);
        Pos := Start;
      end;
    itTouchUp:
      Touch[Ord(aKey)].IsDown := False;
    itTouchMove:
      Touch[Ord(aKey)].Pos := dfVec2f(X, Y);
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
var
  aStr: AnsiString;
begin
  gl.Init();
  {$ifdef log}
  aStr := 'Graphics information' + #13#10#9#9 +
    'Vendor: ' + gl.GetString(TGLConst.GL_VENDOR) + #13#10#9#9 +
    'Renderer: ' + gl.GetString(TGLConst.GL_RENDERER) + #13#10#9#9 +
    'OpenGL: ' + gl.GetString(TGLConst.GL_VERSION) + #13#10#9#9 +
    'GLSL: ' + gl.GetString(TGLConst.GL_SHADING_LANGUAGE_VERSION);
  Log.Write(lInformation, aStr);
  {$endif}
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
var
  i: Integer;
begin
  SetCullMode(cmBack);
  SetBlendingMode(bmAlpha);
  SetDepthFunc(fcLessOrEqual);
  SetDepthWrite(True);
  SetDepthTest(True);
  SetAlphaTest(fcGreater, 0.0);

  fActiveSampler := -1;
  fShader := 0;
  for i := 0 to TEXURE_SAMPLERS_MAX - 1 do
    fTextureSampler[i] := 0;
  fVB := 0;
  fIB := 0;
  fFB := 0;

  Params.Color := dfVec4f(1, 1, 1, 1);
  Params.ViewProj.Identity;
  Params.Model.Identity;

  gl.Enable(GL_COLOR_MATERIAL);
  gl.Enable(GL_TEXTURE_2D);
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
  fWidth := aWidth;
  fHeight := aHeight;
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

class procedure Render.SetShader(aShader: TglrShaderProgramId);
begin
  if (fShader <> aShader) then
  begin
    fShader := aShader;
    gl.UseProgram(fShader);
  end;
end;

class procedure Render.SetTexture(aTexture: TglrTextureId; aSampler: Integer);
begin
  if (fTextureSampler[aSampler] <> aTexture) then
  begin
    fStatTextureBind += 1;
    fTextureSampler[aSampler] := aTexture;
    if (fActiveSampler <> aSampler) then
    begin
      gl.ActiveTexture(Ord(GL_TEXTURE0) + aSampler);
      fActiveSampler := aSampler;
    end;
    gl.BindTexture(GL_TEXTURE_2D, aTexture);
  end;
end;

class procedure Render.DrawTriangles(vBuffer: TglrVertexBuffer;
  iBuffer: TglrIndexBuffer; aStart, aVertCount: Integer);
begin
  if (fVB <> vBuffer.Id) then
  begin
    fVB := vBuffer.Id;
    vBuffer.Bind();
  end;
  if (fIB <> iBuffer.Id) then
  begin
    fIB := iBuffer.Id;
    iBuffer.Bind();
  end;

  gl.DrawElements(GL_TRIANGLES, aVertCount, IF_FORMAT[iBuffer.iFormat], Pointer(aStart * IF_STRIDE[iBuffer.iFormat]));

  fDipCount += 1;
  fTriCount += aVertCount div 3;

  //Log.Write(lCritical, 'Render.DrawTriangles not implemented fully');
end;

class procedure Render.DrawPoints(vBuffer: TglrVertexBuffer; aStart,
  aVertCount: Integer);
begin
  Log.Write(lCritical, 'Render.DrawPoints not implemented');
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
  iFormat := aFormat;
  Self.Bind();
  gl.BufferData(GL_ELEMENT_ARRAY_BUFFER, IF_STRIDE[iFormat] * aCount, aData, GL_STATIC_DRAW);
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

  //gl.EnableClientState(GL_VERTEX_ARRAY);
  //gl.BindBuffer(GL_ARRAY_BUFFER, Self.Id);
  //gl.VertexPointer(3, GL_FLOAT, SizeOf(TdfVec2f), nil);
  //Log.Write(lCritical, 'VertexBuffer.Bind has no vertex attrib calls');

  //todo: gl.VertexAtribPointer в зависимости от формата
  case vbFormat of
    vfPos3Tex2:
    begin
      //gl.VertexAttribPointer(Ord(vaTexCoord0), 2, GL_FLOAT, False, VF_STRIDE[vbFormat], Pointer(SizeOf(TdfVec3f)));
			gl.VertexAttribPointer(Ord(vaCoord), 3, GL_FLOAT, False, VF_STRIDE[vbFormat], 0);
    end;
    vfPos2Tex2:
    begin
			gl.VertexAttribPointer(Ord(vaTexCoord0), 2, GL_FLOAT, False, VF_STRIDE[vbFormat], Pointer(SizeOf(TdfVec2f)));
			gl.VertexAttribPointer(Ord(vaCoord), 2, GL_FLOAT, False, VF_STRIDE[vbFormat], 0);
    end;
  end;
end;

class procedure TglrVertexBuffer.Unbind;
begin
  gl.DisableClientState(GL_VERTEX_ARRAY);
  gl.BindBuffer(GL_ARRAY_BUFFER, 0);
end;

constructor TglrVertexBuffer.Create(aData: Pointer; aCount: Integer;
  aFormat: TglrVertexFormat);
begin
  gl.GenBuffers(1, @Self.Id);
  Self.vbFormat := aFormat;
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

constructor TglrTexture.Create(aStream: TglrStream; aExt: AnsiString;
  aFreeStreamOnFinish: Boolean);
var
  data: Pointer;
  iFormat, cFormat, dType: TGLConst;
  pSize, anisotropy: Integer;

begin
  gl.GenTextures(1, @Self.Id);
  Target := GL_TEXTURE_2D;

  Log.Write(lInformation, 'Texture (ID = ' + Convert.ToStringA(Integer(Self.Id)) + ') load started');

  gl.BindTexture(Target, Self.Id);

  gl.TexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, Ord(GL_LINEAR));
  gl.TexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, Ord(GL_LINEAR));
  gl.TexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, Ord(GL_REPEAT));
  gl.TexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, Ord(GL_REPEAT));
  gl.GetIntegerv(GL_MAX_TEXTURE_MAX_ANISOTROPY, @anisotropy);
  gl.TexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAX_ANISOTROPY, anisotropy);
  gl.TexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);

  data := LoadTexture(aStream, aExt, iFormat, cFormat, dType, pSize, Self.Width, Self.Height);
  gl.TexImage2D(GL_TEXTURE_2D, 0, iFormat, Width, Height, 0, cFormat, dType, data);

  gl.BindTexture(GL_TEXTURE_2D, 0);

  Log.Write(lInformation, 'Texture (ID = ' + Convert.ToStringA(Integer(Self.Id)) + ') load completed');

  X := 0;
  Y := 0;
  RegionWidth := Width;
  RegionHeight := Height;

  Freemem(data);
  if (aFreeStreamOnFinish) then
    aStream.Free();
end;

destructor TglrTexture.Destroy();
begin
  gl.DeleteTextures(1, @Self.Id);
  inherited Destroy;
end;

procedure TglrTexture.Bind(const aSampler: Integer);
begin
  Render.SetTexture(Self.Id, aSampler);
end;

class procedure TglrTexture.Unbind;
begin
  Render.SetTexture(0, 0);
end;

procedure TglrList<T>.Init(Capacity: LongInt);
begin
  FItems := nil;
  FCount := 0;
  FCapacity := Capacity;
end;

procedure TglrList<T>.Free(FreeItems: Boolean);
var
  i : LongInt;
begin
  if FreeItems then
    for i := 0 to Count - 1 do
      TObject(FItems[i]).Free;
  FItems := nil;
  FCount := 0;
end;

procedure TglrList<T>.BoundsCheck(Index: LongInt);
begin
  if (Index < 0) or (Index >= FCount) then
    Log.Write(lCritical, 'List index out of bounds (' + Convert.ToStringA(Index) + ')');
end;

function TglrList<T>.GetItem(Index: LongInt): T;
begin
  BoundsCheck(Index);
  Result := FItems[Index];
end;

procedure TglrList<T>.SetItem(Index: LongInt; Value: T);
begin
  BoundsCheck(Index);
  FItems[Index] := Value;
end;

procedure TglrList<T>.SortFragment(CompareFunc: TglrListCompareFunc; L, R: LongInt);
var
  i, j : Integer;
  P, tm : T;
begin
  repeat
    i := L;
    j := R;
    P := FItems[(L + R) div 2];
    repeat
      while CompareFunc(@FItems[i], @P) < 0 do
        Inc(i);
      while CompareFunc(@FItems[j], @P) > 0 do
        Dec(j);
      if i <= j then
      begin
        tm := FItems[i];
        FItems[i] := FItems[j];
        FItems[j] := tm;
        Inc(i);
        Dec(j);
      end;
    until i > j;
    if L < j then
      SortFragment(CompareFunc, L, j);
    L := i;
  until i >= R;
end;

constructor TglrList<T>.Create(aCapacity: LongInt);
begin
  inherited Create();
  Init(aCapacity);
end;

function TglrList<T>.IndexOf(Item: T): LongInt;
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

function TglrList<T>.Add(Item: T): LongInt;
begin
  if FCount mod FCapacity = 0 then
    SetLength(FItems, Length(FItems) + FCapacity);
  FItems[FCount] := Item;
  Result := FCount;
  Inc(FCount);
end;

procedure TglrList<T>.DeleteByIndex(Index: LongInt; FreeItem: Boolean);
begin
  BoundsCheck(Index);
  if FreeItem then
    TObject(FItems[Index]).Free;
  Move(FItems[Index + 1], FItems[Index], (FCount - Index - 1) * SizeOf(T));
  Dec(FCount);
  if Length(FItems) - FCount + 1 > FCapacity then
    SetLength(FItems, Length(FItems) - FCapacity);
end;

procedure TglrList<T>.Delete(Item: T; FreeItem: Boolean);
begin
  DeleteByIndex(IndexOf(Item), FreeItem);
end;

procedure TglrList<T>.DeleteSafe(Item: T; FreeItem: Boolean);
begin
  DeleteSafeByIndex(IndexOf(Item), FreeItem);
end;

procedure TglrList<T>.DeleteSafeByIndex(Index: LongInt; FreeItem: Boolean);
var
  i: Integer;
begin
  BoundsCheck(Index);
  if FreeItem then
    TObject(FItems[Index]).Free;
  for i := Index to FCount - 2 do
    FItems[i] := FItems[i + 1];

  Dec(FCount);
  if Length(FItems) - FCount + 1 > FCapacity then
    SetLength(FItems, Length(FItems) - FCapacity);
end;

procedure TglrList<T>.Insert(Index: LongInt; Item: T);
begin
  BoundsCheck(Index);
  Add(Item); //can't add nil
  Move(FItems[Index], FItems[Index + 1], (FCount - Index - 1) * SizeOf(FItems[0]));
  FItems[Index] := Item;
end;

procedure TglrList<T>.Sort(CompareFunc: TglrListCompareFunc);
begin
  if FCount > 1 then
    SortFragment(CompareFunc, 0, FCount - 1);
end;

end.
