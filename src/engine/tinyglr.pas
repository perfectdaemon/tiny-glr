{
  tiny glr

  Dmitry Orlov a.k.a. perfect.daemon
  http://perfect-daemon.ru

  2014
}

unit tinyglr;

{$define log}
{$Assertions on}

interface

uses
  ogl, glrMath;

const
  TINYGLR_VERSION = '0.1 :: stable';
  LOG_FILE = 'tinyglr.log';

type
  {$REGION 'Utils'}

  { TglrStream }

  TglrStream = class
    class function Init(Memory: Pointer; MemSize: LongInt; MemoryOwner: Boolean = False): TglrStream; overload;
    class function Init(const FileName: AnsiString; RW: Boolean = False): TglrStream; overload;
    destructor Destroy; override;
  private
    fMemoryOwner: Boolean;
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
  private
    FItems    : array of T;
    FCount    : LongInt;
    FCapacity : LongInt;
    procedure BoundsCheck(Index: LongInt);
    function GetItem(Index: LongInt): T; inline;
    procedure SetItem(Index: LongInt; Value: T); inline;
    procedure SortFragment(CompareFunc: TglrListCompareFunc; L, R: LongInt);
  public
    constructor Create(aCapacity: LongInt = 4); virtual;
    destructor Destroy(); override;

    function IndexOf(Item: T): LongInt;
    function Add(Item: T): LongInt;
    procedure DeleteByIndex(Index: LongInt);
    procedure Delete(Item: T);
    procedure DeleteSafe(Item: T);
    procedure DeleteSafeByIndex(Index: LongInt);
    procedure Insert(Index: LongInt; Item: T);
    procedure Sort(CompareFunc: TglrListCompareFunc);
    property Count: LongInt read FCount;
    property Items[Index: LongInt]: T read GetItem write SetItem; default;
  end;

  { TglrObjectList }

  TglrObjectList<T> = class (TglrList<T>)
    procedure Free(aFreeObjects: Boolean = False);
  public
    procedure DeleteByIndex(Index: LongInt; FreeItem: Boolean = False); reintroduce;
    procedure Delete(Item: T; FreeItem: Boolean = False); reintroduce;
    procedure DeleteSafe(Item: T; FreeItem: Boolean = False); reintroduce;
    procedure DeleteSafeByIndex(Index: LongInt; FreeItem: Boolean = False); reintroduce;
  end;

  TglrStringList = TglrList<AnsiString>;
  TglrWordList = TglrList<Word>;

  { TglrDictionary }

  TglrDictionary<Key, Value> = class
  private
    fSorted  : Boolean;
    fKeys    : array of Key;
    fValues  : array of Value;
    fCount   : LongInt;
    fCapacity: LongInt;
    procedure BoundsCheck(Index: LongInt);
    function GetItem(aKey: Key): Value; inline;
    procedure SetItem(aKey: Key; aValue: Value); inline;
    function GetKey(aIndex: Integer): Key; inline;
    procedure SetKey(aIndex: Integer; aKey: Key); inline;
    function GetValue(aIndex: Integer): Value; inline;
    procedure SetValue(aIndex: Integer; aValue: Value); inline;
  public
    constructor Create(aCapacity: LongInt = 4); virtual;
    destructor Destroy(); override;

    function IndexOfKey(aKey: Key): LongInt;
    function IndexOfValue(aValue: Value): LongInt;
    function Add(aKey: Key; aValue: Value): LongInt;
    procedure DeleteByIndex(aIndex: LongInt);
    procedure Delete(aKey: Key);
    procedure DeleteSafe(aKey: Key);
    procedure DeleteSafeByIndex(aIndex: LongInt);

    property Count: LongInt read fCount;
    property Items[aKey: Key]: Value read GetItem write SetItem; default;
    property Keys[aIndex: Integer]: Key read GetKey write SetKey;
    property Values[aIndex: Integer]: Value read GetValue write SetValue;

    procedure SortByKey(aAscending: Boolean = True);
    function GetLerpValue(aKey: Key): Value;
  end;


  { FileSystem }

const
  PACK_FILE_MAGIC: Word = $0F86;
  PACK_FILE_MAGIC_LZO: Word = $0F87;
  PACK_FILE_EXT = '.glrpack';
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
        fLoaded, fLZO: Boolean;
        fPackData: TglrStream;
        fPackDataPointer: Pointer;
      end;

    var
      class var fPackFilesPath: AnsiString;
      class var fPackFiles: array of TglrPackFile;

    class procedure Init(const aPackFilesPath: AnsiString);
    class procedure DeInit();

    class function GetFileIndexInPackFile(packIndex: Integer; aFileName: AnsiString): Integer;
    class function GetPackIndexByPackName(const aPackName: AnsiString): Integer;
  public
    class procedure LoadPack(const aPackFileName: AnsiString); //loads entire pack file into memory
    class procedure UnloadPack(const aPackFileName: AnsiString);
    class function ReadResource(const aFileName: AnsiString; aSearchInPackFiles: Boolean = True): TglrStream;
    class function ReadResourceLZO(const aFileName: AnsiString; aSearchInPackFiles: Boolean = True): TglrStream;
    class procedure WriteResource(const aFileName: AnsiString; const aStream: TglrStream); overload;
    class procedure WriteResource(const aFileName: AnsiString; const aContent: AnsiString); overload;
  end;

  procedure CompressData(const InData: Pointer; InSize: LongInt; out OutData: Pointer; out OutSize: LongInt);
  procedure DecompressData(const InData: Pointer; InSize: LongInt; const OutData: Pointer; var OutSize: LongInt);

  { Log }

type
  TglrLogMessageType = (lInformation, lWarning, lError, lCritical);

  Log = class
  protected
    class var f: Text;
    class var fTotalErrors, fTotalWarnings: Word;
  public
    class procedure Init(const aFileName: AnsiString);
    class procedure Deinit();
    class procedure Write(aType: TglrLogMessageType; aMessage: AnsiString);
  end;

  { Convert }

  Convert = class
  public
    class function ToString(aVal: Integer): AnsiString; overload;
    class function ToString(aVal: Single; Digits: Integer = 5): AnsiString; overload;
    class function ToString(aVal: TdfMat4f; aDigits: Integer = 5): AnsiString; overload;
    class function ToString(aVal: TdfVec2f): AnsiString; overload;
    class function ToString(aVal: TdfVec3f): AnsiString; overload;
    class function ToString(aVal: TdfVec4f): AnsiString; overload;
    class function ToInt(aStr: AnsiString; aDefault: Integer = -1): Integer; overload;
    class function ToFloat(aStr: AnsiString; aDefault: Single = -1.0): Single; overload;
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

  TglrTextureFormat = (tfRGB8, tfRGBA8, tfBGR8, tfBGRA8);
  TglrVertexFormat = (vfPos2Tex2, vfPos3Tex2, vfPos3Tex2Nor3, vfPos3Tex2Col4);
  TglrIndexFormat = (ifByte, ifShort, ifInt);

  TglrVertexP2T2 = packed record
    vec, tex: TdfVec2f;
  end;

  TglrVertexP3T2 = packed record
    vec: TdfVec3f;
    tex: TdfVec2f;
  end;

  TglrVertexP3T2N3 = packed record
    vec: TdfVec3f;
    tex: TdfVec2f;
    nor: TdfVec3f;
  end;

  TglrVertexP3T2C4 = packed record
    vec: TdfVec3f;
    tex: TdfVec2f;
    col: TdfVec4f;
  end;

  TglrQuadP3T2C4 = array[0..3] of TglrVertexP3T2C4;

  TglrVertexAtrib = (vaCoord = 0, vaNormal = 1, vaTexCoord0 = 2, vaTexCoord1 = 3, vaColor = 4{, ...});

  { TglrTexture }

  TglrTexWrap = (wClamp, wRepeat, wClampToEdge, wClampToBorder, wMirrorRepeat);

  TglrTexture = class
  protected
    Target: TGLConst;
    WrapS, WrapT, WrapR: TglrTexWrap;
  public
    Id: TglrTextureId;

    Width, Height: Integer;

    procedure SetWrapS(aWrap: TglrTexWrap);
    procedure SetWrapT(aWrap: TglrTexWrap);
    procedure SetWrapR(aWrap: TglrTexWrap);

    //constructor Create(aData: Pointer; aCount: Integer; aFormat: TglrTextureFormat); virtual; overload;
    constructor Create(aData: Pointer; aWidth, aHeight: Integer; aFormat: TglrTextureFormat); virtual; overload;
    constructor Create(aStream: TglrStream; aExt: AnsiString;
      aFreeStreamOnFinish: Boolean = True); virtual; overload;
    //constructor CreateEmpty2D(aWidth, aHeight, aFormat: TGLConst);
    //todo 1d, 3d
    destructor Destroy(); override;

    procedure Bind(const aSampler: Integer = 0);
    class procedure Unbind();
  end;

  PglrTextureRegion = ^TglrTextureRegion;
  TglrTextureRegion = record
    Texture: TglrTexture;
    Name: AnsiString;
    tx, ty, tw, th: Single;
    Rotated: Boolean;
  end;


  { TglrTextureAtlas }

  TglrTextureAtlas = class (TglrTexture)
  protected
    type
      TglrTextureRegionsList = TglrList<PglrTextureRegion>;
    var
      fRegions: TglrTextureRegionsList;
  public
    constructor Create(aImageStream, aInfoStream: TglrStream;
      aImageExt, aInfoExt: AnsiString;
      aFreeStreamsOnFinish: Boolean = True); virtual;
    destructor Destroy(); override;

    function GetRegion(aName: AnsiString): PglrTextureRegion;
  end;


  TglrVertexBufferMapAccess = (maRead, maWrite, maReadWrite);

  { TglrVertexBuffer }

  TglrVertexBuffer = class
    Id: TglrVertexBufferId;
    Format: TglrVertexFormat;
    Count: Integer;
    procedure Bind();
    class procedure Unbind();
    constructor Create(aData: Pointer; aCount: Integer; aFormat: TglrVertexFormat); virtual;
    destructor Destroy(); override;

    procedure Update(aData: Pointer; aStart, aCount: Integer); virtual;
    function Map(aAccess: TglrVertexBufferMapAccess = maReadWrite): Pointer;
    procedure Unmap();
  end;

  { TglrIndexBuffer }

  TglrIndexBuffer = class
    Id: TglrIndexBufferId;
    Format: TglrIndexFormat;
    procedure Bind();
    class procedure Unbind();
    constructor Create(aData: Pointer; aCount: Integer; aFormat: TglrIndexFormat); virtual;
    destructor Destroy(); override;

    procedure Update(aData: Pointer; aStart, aCount: Integer); virtual;
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
  TglrUniformType = (utVec1, utVec2, utVec3, utVec4, utMat4, utSampler);

  TglrUniformInfo = record
  	fType: TglrUniformType;
	  fName: AnsiString;
	  fCount, fIndex: Integer;
    fData: Pointer;
  end;

  { TglrShaderProgram }

  TglrShaderProgram = class
  protected
    fLinkStatus: Integer;
    function GetVertexAtribName(const aAtrib: TglrVertexAtrib): AnsiString;
  public
    Id: TglrShaderProgramId;
    ShadersId: array of TglrShaderId;
    Uniforms: array of TglrUniformInfo;

    procedure Bind();
    class procedure Unbind();

    procedure Attach(aStream: TglrStream; aShaderType: TglrShaderType;
      aFreeStreamOnFinish: Boolean = True);
    procedure Link();

    function AddUniform(aUniformType: TglrUniformType; aCount: Integer;
      aName: AnsiString; aData: Pointer = nil): Integer;
    function GetUniformIndexByName(aName: AnsiString): Integer;

    procedure SetUniform(aUniformType: TglrUniformType; aCount: Integer;
      aValue: Pointer; aName: PAnsiChar; aIndex: Integer = -1); overload;
    procedure SetUniform(aInternalIndex: Integer; aValue: Pointer); overload;

    constructor Create(); virtual;
    destructor Destroy(); override;
  end;


  TglrBlendingMode = ( bmNone, bmAlpha, bmAdditive, bmMultiply, bmScreen);
  TglrCullMode = (cmNone, cmBack, cmFront);
  TglrFuncComparison = (fcNever, fcLess, fcEqual, fcLessOrEqual,
    fcGreater, fcNotEqual, fcGreaterOrEqual, fcAlways);
  TglrClearMask = (cmAll, cmColor, cmDepth);

  { Render }

  { TglrRenderParams }

  TglrRenderParams = record
    ViewProj, Model, ModelViewProj: TdfMat4f;
    Color: TdfVec4f;
    procedure CalculateMVP();
  end;

const
  TEXURE_SAMPLERS_MAX = 8;

type
  Render = class
  private
  protected
    class var fBlendingMode: TglrBlendingMode;
    class var fCullMode: TglrCullMode;
    class var fDepthWrite, fDepthTest: Boolean;
    class var fDepthFunc: TglrFuncComparison;
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
    class procedure SetDepthWrite(aEnabled: Boolean);
    class procedure SetDepthTest(aEnabled: Boolean);
    class procedure SetDepthFunc(aComparison: TglrFuncComparison);
    class procedure SetVerticalSync(aEnabled: Boolean);
    class procedure SetShader(aShader: TglrShaderProgramId);
    class procedure SetTexture(aTexture: TglrTextureId; aSampler: Integer);

    class procedure DrawTriangles(vBuffer: TglrVertexBuffer; iBuffer: TglrIndexBuffer;
      aStartIndex, aIndicesCount: Integer);
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
    procedure Quit(); virtual; abstract;
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
    MousePos: TdfVec2f;
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

  { Core }

  Core = class
  protected
    class var fGame: TglrGame;
    class var fAppView: TglrAppView;
    class var fFPS: Single;
    class var fDT: Double;
    class var fReady: Boolean;
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

    class procedure Quit();

    class procedure Update(const dt: Double);
    class procedure RenderAll();

    class property FPS: Single read fFPS;
    class property FPSText: AnsiString read GetFPSText; //preformatted with 1 digit after delimiter
    class property DeltaTime: Double read fDT;
    class property IsReady: Boolean read fReady;
  end;

  {$ENDREGION}

  {$REGION 'Node, Camera, Scene'}

  { TglrNode }

  TglrNode = class;

  TglrNode = class
  protected
    fDir, fRight, fUp: TdfVec3f;
    fParent: TglrNode;
    fAbsMatrix: TdfMat4f;

    procedure SetParent(AValue: TglrNode);
    function GetAbsMatrix: TdfMat4f;
    procedure SetDir(aDir: TdfVec3f);
    procedure SetRight(aRight: TdfVec3f);
    procedure SetUp(aUp: TdfVec3f);
    procedure UpdateModelMatrix(aNewDir, aNewUp, aNewRight: TdfVec3f); virtual;
    procedure UpdateVectorsFromMatrix(); virtual;
    procedure DoRender(); virtual;
  public
    Visible: Boolean;
    Matrix: TdfMat4f;
    Position: TdfVec3f;

    constructor Create; virtual;
    destructor Destroy; override;

    property AbsoluteMatrix: TdfMat4f read GetAbsMatrix write fAbsMatrix;
    property Parent: TglrNode read fParent write SetParent;

    property Up: TdfVec3f read fUp write SetUp;
    property Direction: TdfVec3f read fDir write SetDir;
    property Right: TdfVec3f read fRight write SetRight;

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
    fTargetPoint: TdfVec3f;
    fTarget: TglrNode;
    fScale, fFOV, fZNear, fZFar: Single;
    fX, fY, fW, fH: Integer;
    fProjectionPivot: TglrCameraPivot;
    procedure SetProjMode(aMode: TglrCameraProjectionMode);
    procedure UpdateVectorsFromMatrix(); override;
  public
    fProjMatrix: TdfMat4f;

    constructor Create(); override;

    procedure Viewport(x, y, w, h: Integer; FOV, ZNear, ZFar: Single);
    procedure ViewportOnly(x, y, w, h: Integer);

    procedure Translate(alongUpVector, alongRightVector, alongDirVector: Single);
    procedure Rotate(delta: Single; Axis: TdfVec3f);

    function GetViewport(): TglrViewportParams;
    function WindowPosToCameraPos(aPos: TdfVec2f): TdfVec2f;

    procedure Update;

    procedure SetCamera(aPos, aTargetPos, aUp: TdfVec3f);

    procedure RenderSelf(); override;

    property Scale: Single read fScale write SetScale;

//    procedure SetTarget(aPoint: TdfVec3f); overload;
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

  {$ENDREGION}

  {$REGION 'Material, Sprite, Font'}

  { TglrMaterial }

  TglrTextureMaterialInfo = record
    Texture: TglrTexture;
    UniformName: AnsiString;
    ShaderInternalIndex: Integer;
  end;

  TglrMaterial = class
    Shader: TglrShaderProgram;
    Textures: array of TglrTextureMaterialInfo;
    Color: TdfVec4f;
  	Blend: TglrBlendingMode;
  	DepthWrite: Boolean;
    DepthTest: Boolean;
    DepthTestFunc: TglrFuncComparison;
  	Cull: TglrCullMode;

    constructor Create(); virtual; overload;
    constructor Create(aStream: TglrStream;
      aFreeStreamOnFinish: Boolean = True); virtual; overload;
    destructor Destroy(); override;

    procedure AddTexture(aTexture: TglrTexture; aUniformName: AnsiString);

    procedure Bind();
    procedure Unbind();
  end;

  { TglrSprite }
const
  SpriteIndices: array[0..5] of Word = (0, 1, 2, 2, 3, 0);

type
  TglrSprite = class (TglrNode)
  protected
    fRot, fWidth, fHeight: Single;
    fPP: TdfVec2f;

    procedure SetRot(const aRot: Single);
    procedure SetWidth(const aWidth: Single);
    procedure SetHeight(const aHeight: Single);
    procedure SetPP(const aPP: TdfVec2f);
  public
    Vertices: array[0..3] of TglrVertexP3T2C4;

    constructor Create(); override; overload;
    constructor Create(aWidth, aHeight: Single; aPivotPoint: TdfVec2f); overload;
    destructor Destroy(); override;

    property Rotation: Single read fRot write SetRot;
    property Width: Single read fWidth write SetWidth;
    property Height: Single read fHeight write SetHeight;
    property PivotPoint: TdfVec2f read fPP write SetPP;

    procedure SetDefaultVertices(); virtual;//Sets vertices due to width, height, pivot point and rotation
    procedure SetDefaultTexCoords(); //Sets default texture coords
    procedure SetVerticesColor(aColor: TdfVec4f);
    procedure SetSize(aWidth, aHeight: Single); overload;
    procedure SetSize(aSize: TdfVec2f); overload;

    procedure SetTextureRegion(aRegion: PglrTextureRegion; aAdjustSpriteSize: Boolean = True);

    procedure RenderSelf(); override;
  end;

  TglrSpriteList = TglrObjectList<TglrSprite>;

  { TglrSpriteBatch }

  TglrSpriteBatch = class
  protected
    fVData: array[0..65535] of TglrVertexP3T2C4;
    fIData: array[0..65535] of Word;
    fCount: Word;
    fVB: TglrVertexBuffer;
    fIB: TglrIndexBuffer;
  public
    constructor Create(); virtual;
    destructor Destroy(); override;

    procedure Start();
    procedure Draw(aSprite: TglrSprite); overload;
    procedure Draw(aSprites: array of TglrSprite); overload;
    procedure Draw(aSprites: TglrSpriteList); overload;
    procedure Finish();
  end;

  { TglrFont }

  TglrText = class;

  TglrFont = class
  protected
    type
      TglrCharData = record
        ID: WideChar;
        py: Word;
        w, h: Word;
        tx, ty, tw, th: Single;
      end;
      PglrCharData = ^TglrCharData;

    var
      Material: TglrMaterial;
      Texture: TglrTexture;
      Table: array [WideChar] of PglrCharData;
      CharData: array of TglrCharData;

    function GetCharQuad(aChar: WideChar): TglrQuadP3T2C4;
  public
    MaxCharHeight: Word;

    constructor Create(); virtual; overload;
    constructor Create(aStream: TglrStream;
      aFreeStreamOnFinish: Boolean = True); virtual; overload;
    destructor Destroy(); override;
  end;

  TglrTextHorAlign = (haLeft, haCenter, haRight);
  TglrTextVerAlign = (vaTop, vaCenter, vaBottom);

  TglrText = class (TglrNode)
  protected
    fHorAlign: TglrTextHorAlign;
    fTextWidth: Single;
    fVerAlign: TglrTextVerAlign;
    procedure SetHorAlign(aValue: TglrTextHorAlign);
    procedure SetTextWidth(aValue: Single);
    procedure SetVerAlign(aValue: TglrTextVerAlign);
  public
    Text: WideString;
    LetterSpacing, LineSpacing: Single;
    Color: TdfVec4f;
    constructor Create(const aText: WideString = ''); virtual;
    destructor Destroy(); override;

    property TextWidth: Single read fTextWidth write SetTextWidth;
    property HorAlign: TglrTextHorAlign read fHorAlign write SetHorAlign;
    property VerAlign: TglrTextVerAlign read fVerAlign write SetVerAlign;

    procedure RenderSelf(); override;
  end;

  { TglrFontBatch }

  TglrFontBatch = class
  protected
    fVData: array[0..65535] of TglrVertexP3T2C4;
    fIData: array[0..65535] of Word;
    fCount: Word;
    fVB: TglrVertexBuffer;
    fIB: TglrIndexBuffer;
    fFont: TglrFont;
  public
    constructor Create(aFont: TglrFont); virtual;
    destructor Destroy(); override;

    procedure Start();
    procedure Draw(aText: TglrText);
    procedure Finish();
  end;

  {$ENDREGION}

  TglrMesh = class (TglrNode)

  end;

  {$REGION 'Particles'}

  { TglrParticle2D }

  TglrParticle2D = class (TglrSprite)
    T: Single;
    LifeTime: Single;
    Velocity: TdfVec2f;
    procedure Reset();
  end;
  TglrParticles2D = TglrObjectList<TglrParticle2D>;

  TglrUpdateCallback = procedure(const dt: Double) of object;

  { TglrCustomParticleEmitter2D }

  TglrCustomParticleEmitter2D = class
  protected
    fBatch: TglrSpriteBatch;
    fMaterial: TglrMaterial;
    fTextureRegion: PglrTextureRegion;
    fActiveParticles: Integer;
  public
    Visible, Enabled: Boolean;
    Duration, Time: Single;

    Particles: TglrParticles2D;

    OnUpdate: TglrUpdateCallback;

    constructor Create(aBatch: TglrSpriteBatch;
      aMaterial: TglrMaterial;
      aTextureRegion: PglrTextureRegion = nil); virtual;
    destructor Destroy(); override;

    function GetNewParticle(): TglrParticle2D;

    procedure Update(const dt: Double);
    procedure RenderSelf();

    property ActiveParticlesCount: Integer read fActiveParticles;
  end;

  //Range of byte is 0..100 (means percent of emitter animation duration)
  TglrSingleDic = TglrDictionary<Byte, Single>;
  TglrIntDic = TglrDictionary<Byte, Integer>;
  TglrVec2fDic = TglrDictionary<Byte, TdfVec2f>;
  TglrVec4fDic = TglrDictionary<Byte, TdfVec4f>;

  { TglrParticleEmitter2D }

  TglrParticleEmitter2D = class
  protected
    fBatch: TglrSpriteBatch;
    fMaterial: TglrMaterial;
    fTextureRegion: PglrTextureRegion;
    fParticles: TglrParticles2D;

    function GetFreeParticleIndex(): Integer;
  public
    //Dynamics
    OriginBoxMinMax: TglrVec4fDic;
    VelocityMinMax: TglrVec4fDic;
    VelocityDispersionAngle: TglrSingleDic;
    VelocityAngularMinMax: TglrVec2fDic;
    Color: TglrVec4fDic;
    ParticlesPerSecond: TglrIntDic;
    LifetimeMinMax: TglrVec2fDic;
    ParticleSizeMinMax: TglrVec4fDic;

    //Params
    Duration, Time: Single;

    constructor Create(aBatch: TglrSpriteBatch;
      aMaterial: TglrMaterial; aTextureRegion: PglrTextureRegion = nil); virtual; overload;
    constructor Create(aBatch: TglrSpriteBatch;
      aMaterial: TglrMaterial;
      const aStream: TglrStream;
      aTextureRegion: PglrTextureRegion = nil;
      aFreeStreamOnFinish: Boolean = True); virtual; overload;
    destructor Destroy(); override;

    function SaveToStream(): TglrStream;

    procedure Update(const dt: Double);
  end;

  {$ENDREGION}

  {$REGION 'GUI'}

  TglrGuiElement = class;

  TglrGuiBooleanCallback = procedure (Sender: TglrGuiElement; aValue: Boolean) of object;

  TglrGuiInputCallback = procedure(Sender: TglrGuiElement;
    aType: TglrInputType;
    aKey: TglrKey;
    X, Y, aOtherParam: Integer) of object;

  { TglrGuiElement }

  TglrGuiElement = class (TglrSprite)
  protected
    fIsMouseOver: Boolean;
    fEnabled, fFocused: Boolean;
    procedure SetEnabled(const aValue: Boolean);
    procedure SetFocused(const aValue: Boolean);
    procedure ProcessInput(aType: TglrInputType; aKey: TglrKey; X, Y,
      aOtherParam: Integer);
    function IsHit(X, Y: Integer): Boolean;
  public
    // Input events
    OnClick, OnTouchDown, OnTouchUp, OnTouchMove, OnMouseOver, OnMouseOut: TglrGuiInputCallback;

    // Other events
    OnEnable, OnFocus: TglrGuiBooleanCallback;

    ZIndex: Integer;

    HitBox: TdfBB;

    property Enabled: Boolean read fEnabled write SetEnabled;
    property Focused: Boolean read fFocused write SetFocused;

    procedure UpdateHitBox();
    procedure SetDefaultVertices(); override;

    constructor Create(); virtual;
  end;

  TglrGuiElementsList = TglrObjectList<TglrGuiElement>;

  { TglrGuiManager }

  TglrGuiManager = class
  protected
  public
    Elements: TglrGuiElementsList;

    constructor Create(); virtual;
    destructor Destroy(); override;

    procedure ProcessInput(aType: TglrInputType; aKey: TglrKey; X, Y,
      aOtherParam: Integer); overload;
    procedure ProcessInput(aType: TglrInputType; aKey: TglrKey; X, Y,
      aOtherParam: Integer; aGuiCamera: TglrCamera); overload;
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
    class var SpriteShader: TglrShaderProgram;
    class var BlankTexture: TglrTexture;
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
    (SizeOf(TglrVertexP2T2), SizeOf(TglrVertexP3T2), SizeOf(TglrVertexP3T2N3), SizeOf(TglrVertexP3T2C4));
  IF_STRIDE: array[Low(TglrIndexFormat)..High(TglrIndexFormat)] of Integer =
    (SizeOf(Byte), SizeOf(Word), SizeOf(LongWord));
  IF_FORMAT: array[Low(TglrIndexFormat)..High(TglrIndexFormat)] of TGLConst =
  (GL_UNSIGNED_BYTE, GL_UNSIGNED_SHORT, GL_UNSIGNED_INT);

  comparison: array[Low(TglrFuncComparison)..High(TglrFuncComparison)] of TGLConst =
    (GL_NEVER, GL_LESS, GL_EQUAL, GL_LEQUAL, GL_GREATER, GL_NOTEQUAL, GL_GEQUAL, GL_ALWAYS);

  aWraps: array[Low(TglrTexWrap)..High(TglrTexWrap)] of TGLConst =
    (GL_CLAMP, GL_REPEAT, GL_CLAMP_TO_EDGE, GL_CLAMP_TO_BORDER, GL_MIRRORED_REPEAT);

{ TglrTextureAtlas }

function ParseLine(const aLine: AnsiString): TglrStringList;
var
  start, i: Integer;
begin
  Result := TglrStringList.Create(8);
  start := 1;
  for i := start to Length(aLine) do
    if aLine[i] = #9 then
    begin
      Result.Add(Copy(aLine, start, i - start));
      start := i + 1;
    end;
  Result.Add(Copy(aLine, start, i - start)); // add last
end;

{ TglrClassList<T> }

procedure TglrObjectList<T>.Free(aFreeObjects: Boolean);
var
  i: Integer;
begin
  if aFreeObjects then
    for i := 0 to Count - 1 do
      TObject(FItems[i]).Free();
  inherited Free();
end;

procedure TglrObjectList<T>.DeleteByIndex(Index: LongInt; FreeItem: Boolean);
begin
  BoundsCheck(Index);
  if FreeItem then
    TObject(FItems[Index]).Free;
  if Index <> fCount - 1 then
    Move(FItems[Index + 1], FItems[Index], (FCount - Index - 1) * SizeOf(T));
  Dec(FCount);
  if Length(FItems) - FCount + 1 > FCapacity then
    SetLength(FItems, Length(FItems) - FCapacity);
end;

procedure TglrObjectList<T>.Delete(Item: T; FreeItem: Boolean);
var
  i: Integer;
begin
  i := IndexOf(Item);
  if i <> -1 then
    DeleteByIndex(i, FreeItem)
  else
    Log.Write(lError, 'List: No item found at list, delete is impossible');
end;

procedure TglrObjectList<T>.DeleteSafe(Item: T; FreeItem: Boolean);
var
  i: Integer;
begin
  i := IndexOf(Item);
  if i <> -1 then
    DeleteSafeByIndex(i, FreeItem)
  else
    Log.Write(lError, 'List: No item found at list, delete is impossible');
end;

procedure TglrObjectList<T>.DeleteSafeByIndex(Index: LongInt; FreeItem: Boolean);
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

{ TglrDictionary<Key, Value> }

procedure TglrDictionary<Key, Value>.BoundsCheck(Index: LongInt);
begin
  if (Index < 0) or (Index >= fCount) then
    Log.Write(lCritical, 'Dictionary index out of bounds (' + Convert.ToString(Index) + ')');
end;

function TglrDictionary<Key, Value>.GetItem(aKey: Key): Value;
var
  i: Integer;
begin
  for i := 0 to fCount - 1 do
    if fKeys[i] = aKey then
      Exit(fValues[i]);
end;

procedure TglrDictionary<Key, Value>.SetItem(aKey: Key; aValue: Value);
var
  i: Integer;
begin
  for i := 0 to fCount - 1 do
    if fKeys[i] = aKey then
      fValues[i] := aValue;
end;

function TglrDictionary<Key, Value>.GetKey(aIndex: Integer): Key;
begin
  BoundsCheck(aIndex);
  Exit(fKeys[aIndex]);
end;

procedure TglrDictionary<Key, Value>.SetKey(aIndex: Integer; aKey: Key);
begin
  BoundsCheck(aIndex);
  fKeys[aIndex] := aKey;
  fSorted := False;
end;

function TglrDictionary<Key, Value>.GetValue(aIndex: Integer): Value;
begin
  BoundsCheck(aIndex);
  Exit(fValues[aIndex]);
end;

function TglrDictionary<Key, Value>.GetLerpValue(aKey: Key): Value;
var
  i: Integer;
  t: single;
begin
  if not fSorted then
    Log.Write(lCritical, 'Dictionary: can not return lerp value, dictionary is not sorted!');

  i := IndexOfKey(aKey);
  if i <> -1 then
    Exit(fValues[i]);

  for i := 0 to fCount - 1 do
    if fKeys[i] > aKey then
      break;

  if (i = 0) or (i = fCount - 1) then
    Exit(fValues[i])
  else
  begin
    t := (aKey - fKeys[i - 1]) / (fKeys[i] - fKeys[i - 1]);
    Result := Value(fValues[i - 1] + (fValues[i] - fValues[i - 1]) * t);
  end;
end;

procedure TglrDictionary<Key, Value>.SetValue(aIndex: Integer; aValue: Value);
begin
  BoundsCheck(aIndex);
  fValues[aIndex] := aValue;
end;

constructor TglrDictionary<Key, Value>.Create(aCapacity: LongInt);
begin
  inherited Create();
  fCount := 0;
  SetLength(fKeys, fCount);
  SetLength(fValues, fCount);
  fCapacity := aCapacity;
  fSorted := False;
end;

destructor TglrDictionary<Key, Value>.Destroy;
begin
  fCount := 0;
  SetLength(fKeys, fCount);
  SetLength(fValues, fCount);
  inherited Destroy;
end;

function TglrDictionary<Key, Value>.IndexOfKey(aKey: Key): LongInt;
var
  i: Integer;
begin
  for i := 0 to fCount - 1 do
    if fKeys[i] = aKey then
      Exit(i);
  Exit(-1);
end;

function TglrDictionary<Key, Value>.IndexOfValue(aValue: Value): LongInt;
var
  i: Integer;
begin
  for i := 0 to fCount - 1 do
    if fValues[i] = aValue then
      Exit(i);
  Exit(-1);
end;

function TglrDictionary<Key, Value>.Add(aKey: Key; aValue: Value): LongInt;
begin
  if fCount mod fCapacity = 0 then
  begin
    SetLength(fKeys, Length(fKeys) + fCapacity);
    SetLength(fValues, Length(fValues) + fCapacity);
  end;
  fKeys[fCount] := aKey;
  fValues[fCount] := aValue;
  Result := fCount;
  Inc(fCount);
  fSorted := False;
end;

procedure TglrDictionary<Key, Value>.SortByKey(aAscending: Boolean);
var
  i, j, max: LongInt;
  k: Key;
  v: Value;
begin
  for i := 0 to fCount - 2 do
  begin
    max := i;
    for j := i to fCount - 1 do
      if (aAscending and (fKeys[j] < fKeys[max])) or
        (not aAscending and (fKeys[j] > fKeys[max])) then
        max := j;

    if max <> i then
    begin
      k := fKeys[i];
      v := fValues[i];
      fKeys[i] := fKeys[max];
      fValues[i] := fValues[max];
      fKeys[max] := k;
      fValues[max] := v;
    end;
  end;
  fSorted := True;
end;

procedure TglrDictionary<Key, Value>.DeleteByIndex(aIndex: LongInt);
begin
  BoundsCheck(aIndex);
  if aIndex <> fCount - 1 then
  begin
    Move(fKeys[aIndex + 1], fKeys[aIndex], (fCount - aIndex - 1) * SizeOf(Key));
    Move(fValues[aIndex + 1], fValues[aIndex], (fCount - aIndex - 1) * SizeOf(Value));
  end;
  Dec(fCount);
  if Length(fKeys) - fCount + 1 > fCapacity then
  begin
    SetLength(fKeys, Length(fKeys) - fCapacity);
    SetLength(fValues, Length(fValues) - fCapacity);
  end;
end;

procedure TglrDictionary<Key, Value>.Delete(aKey: Key);
var
  i: Integer;
begin
  i := IndexOfKey(aKey);
  if i <> -1 then
    DeleteByIndex(i)
  else
    Log.Write(lError, 'Dictionary: No item found at list, delete is impossible');
end;

procedure TglrDictionary<Key, Value>.DeleteSafe(aKey: Key);
var
  i: Integer;
begin
  i := IndexOfKey(aKey);
  if i <> -1 then
    DeleteSafeByIndex(i)
  else
    Log.Write(lError, 'Dictionary: No item found at list, delete is impossible');
end;

procedure TglrDictionary<Key, Value>.DeleteSafeByIndex(aIndex: LongInt);
var
  i: Integer;
begin
  BoundsCheck(aIndex);
  for i := aIndex to fCount - 2 do
  begin
    fKeys[i] := fKeys[i + 1];
    fValues[i] := fValues[i + 1];
  end;

  Dec(fCount);
  if Length(fKeys) - fCount + 1 > fCapacity then
  begin
    SetLength(fKeys, Length(fKeys) - fCapacity);
    SetLength(fValues, Length(fValues) - fCapacity);
  end;
end;

{ TglrGuiManager }

constructor TglrGuiManager.Create;
begin
  inherited;
  Elements := TglrGuiElementsList.Create();
end;

destructor TglrGuiManager.Destroy;
begin
  Elements.Free(True);
  inherited Destroy;
end;

procedure TglrGuiManager.ProcessInput(aType: TglrInputType; aKey: TglrKey; X,
  Y, aOtherParam: Integer);
var
  i: Integer;
begin
  for i := 0 to Elements.Count - 1 do
    if Elements[i].Enabled then
      Elements[i].ProcessInput(aType, aKey, X, Y, aOtherParam);
end;

procedure TglrGuiManager.ProcessInput(aType: TglrInputType; aKey: TglrKey; X,
  Y, aOtherParam: Integer; aGuiCamera: TglrCamera);
begin
  Log.Write(lCritical, 'Not implemented');
end;

{ TglrGuiElement }

procedure TglrGuiElement.SetEnabled(const aValue: Boolean);
begin
  if fEnabled = aValue then
    Exit();
  fEnabled := aValue;
  if Assigned(OnEnable) then
    OnEnable(Self, aValue);
end;

procedure TglrGuiElement.SetFocused(const aValue: Boolean);
begin
  if fFocused = aValue then
    Exit();
  fFocused := aValue;
  if Assigned(OnFocus) then
    OnFocus(Self, aValue);
end;

procedure TglrGuiElement.ProcessInput(aType: TglrInputType; aKey: TglrKey; X,
  Y, aOtherParam: Integer);
begin
  case aType of
    itTouchDown:
    begin
      if Assigned(OnTouchDown) then
        OnTouchDown(Self, aType, aKey, x, y, aOtherParam);
      Focused := True;
    end;
    itTouchUp:
    begin
      if Assigned(OnTouchUp) then
        OnTouchUp(Self, aType, aKey, x, y, aOtherParam);
    end;

  end;

  Log.Write(lCritical, 'Not implemented');
end;

function TglrGuiElement.IsHit(X, Y: Integer): Boolean;
begin
  x += Floor(Position.x);
  y += Floor(Position.y);
  with HitBox do
    Result := (x >= Left) and (x <= Right) and (y >= Top) and (y <= Bottom);
end;

procedure TglrGuiElement.UpdateHitBox;
begin
  HitBox.Bottom := Max(Vertices[0].vec.y, Vertices[1].vec.y);
  HitBox.Top := Min(Vertices[2].vec.y, Vertices[3].vec.y);
  HitBox.Right := Max(Vertices[0].vec.x, Vertices[3].vec.x);
  HitBox.Left := Min(Vertices[1].vec.x, Vertices[2].vec.x);
end;

procedure TglrGuiElement.SetDefaultVertices;
begin
  inherited SetDefaultVertices;
  UpdateHitBox();
end;

constructor TglrGuiElement.Create;
begin
  inherited;
  UpdateHitBox();
  fFocused := False;
  fEnabled := True;
  fIsMouseOver := False;

  ZIndex := 0;
end;

{ TglrParticle2D }

procedure TglrParticle2D.Reset;
begin
  T := 0;
  Velocity.Reset();
  LifeTime := 0.0;
  fRot := 0;
  Visible := True;
end;

{ TglrCustomParticleEmitter2D }

constructor TglrCustomParticleEmitter2D.Create(aBatch: TglrSpriteBatch;
  aMaterial: TglrMaterial; aTextureRegion: PglrTextureRegion);
var
  s: TglrParticle2D;
  i: Integer;
begin
  inherited Create();
  fBatch := aBatch;
  fMaterial := aMaterial;
  fTextureRegion := aTextureRegion;
  Particles := TglrParticles2D.Create(128);

  for i := 0 to Particles.Count - 1 do
  begin
    s := TglrParticle2D.Create();
    if fTextureRegion <> nil then
      s.SetTextureRegion(fTextureRegion)
    else
    begin
      s.Width := fMaterial.Textures[0].Texture.Width;
      s.Height := fMaterial.Textures[0].Texture.Height;
    end;
    s.Reset();
    s.Visible := False;
    Particles.Add(s);
  end;

  Duration := 1.0;
  Time := 0;
  Enabled := True;
  Visible := True;
end;

destructor TglrCustomParticleEmitter2D.Destroy;
begin
  Particles.Free(True);
  inherited Destroy;
end;

function TglrCustomParticleEmitter2D.GetNewParticle: TglrParticle2D;
var
  i: Integer;
  p: TglrParticle2D;
begin
  fActiveParticles += 1;

  for i := 0 to Particles.Count - 1 do
    if not Particles[i].Visible then
    begin
      Particles[i].Reset();
      if (fTextureRegion <> nil) then
        Particles[i].SetTextureRegion(fTextureRegion);
      Particles[i].SetVerticesColor(dfVec4f(1, 1, 1, 1));
      Exit(Particles[i]);
    end;

  p := TglrParticle2D.Create();
  p.Reset();
  if fTextureRegion <> nil then
    p.SetTextureRegion(fTextureRegion);
  Particles.Add(p);
  Exit(p);
end;

procedure TglrCustomParticleEmitter2D.Update(const dt: Double);
var
  i: Integer;
begin
  if not Visible then
    Exit();

  Time += dt;

  for i := 0 to Particles.Count - 1 do
    if Particles[i].Visible then
      with Particles[i] do
      begin
        T := T + dt;
        if T >= LifeTime then
        begin
          Visible := False;
          fActiveParticles -= 1;
        end
        else
          Position += dfVec3f(Velocity * dt, 0);
      end;

  if Assigned(OnUpdate) then
    OnUpdate(dt);
end;

procedure TglrCustomParticleEmitter2D.RenderSelf;
var
  i: Integer;
begin
  fMaterial.Bind();
  fBatch.Start();
  for i := 0 to Particles.Count - 1 do
    fBatch.Draw(Particles[i]);
  fBatch.Finish();
end;

{ TglrParticleEmitter2D }

function TglrParticleEmitter2D.GetFreeParticleIndex: Integer;
var
  i: Integer;
  p: TglrParticle2D;
begin
  for i := 0 to fParticles.Count - 1 do
    if not fParticles[i].Visible then
      Exit(i);

  p := TglrParticle2D.Create();
  if fTextureRegion <> nil then
    p.SetTextureRegion(fTextureRegion);
  p.Velocity.Reset();
  p.T := 0.0;
  p.Visible := False;
  p.LifeTime := 0;
  Exit(fParticles.Add(p));
end;

constructor TglrParticleEmitter2D.Create(aBatch: TglrSpriteBatch;
  aMaterial: TglrMaterial; aTextureRegion: PglrTextureRegion);
var
  s: TglrParticle2D;
  i: Integer;
begin
  inherited Create();
  fBatch := aBatch;
  fMaterial := aMaterial;
  fTextureRegion := aTextureRegion;
  fParticles := TglrParticles2D.Create(128);
  for i := 0 to 127 do
  begin
    s := TglrParticle2D.Create();
    if fTextureRegion <> nil then
      s.SetTextureRegion(fTextureRegion);
    s.Velocity.Reset();
    s.T := 0.0;
    s.Visible := False;
    s.LifeTime := 0;
    fParticles.Add(s);
  end;

  Duration := 1.0;
  Time := 0;

  OriginBoxMinMax         := TglrVec4fDic.Create (100);
  VelocityMinMax          := TglrVec4fDic.Create (100);
  VelocityDispersionAngle := TglrSingleDic.Create(100);
  VelocityAngularMinMax   := TglrVec2fDic.Create(100);
  Color                   := TglrVec4fDic.Create (100);
  ParticlesPerSecond      := TglrIntDic.Create   (100);
  LifetimeMinMax          := TglrVec2fDic.Create (100);
  ParticleSizeMinMax      := TglrVec4fDic.Create (100);
end;

constructor TglrParticleEmitter2D.Create(aBatch: TglrSpriteBatch;
  aMaterial: TglrMaterial; const aStream: TglrStream;
  aTextureRegion: PglrTextureRegion; aFreeStreamOnFinish: Boolean);
begin
  Create(aBatch, aMaterial, aTextureRegion);
end;

destructor TglrParticleEmitter2D.Destroy;
begin
  fParticles.Free(True);
  OriginBoxMinMax.Free();
  VelocityMinMax.Free();
  VelocityDispersionAngle.Free();
  VelocityAngularMinMax.Free();
  Color.Free();
  ParticlesPerSecond.Free();
  LifetimeMinMax.Free();
  ParticleSizeMinMax.Free();
  inherited Destroy;
end;

function TglrParticleEmitter2D.SaveToStream: TglrStream;
begin
  Log.Write(lCritical, 'ParticleEmitter2D: SaveToStream is not implemented');
end;

procedure TglrParticleEmitter2D.Update(const dt: Double);
var
  i: Integer;
begin
  Time += dt;

  for i := 0 to fParticles.Count - 1 do
    if fParticles[i].Visible then
      with fParticles[i] do
      begin
        Position += dfVec3f(Velocity * dt, 0);
        T := T + dt;

        if T >= LifeTime then
          Visible := False
        else
        begin
          //todo: Update other stuff

        end;
      end;

  //todo: add new particles

end;

constructor TglrTextureAtlas.Create(aImageStream, aInfoStream: TglrStream;
  aImageExt, aInfoExt: AnsiString; aFreeStreamsOnFinish: Boolean);
var
  lines, list: TglrStringList;
  i: Integer;
  p: PglrTextureRegion;
begin
  if aInfoExt = 'cheetah' then
  begin
    inherited Create(aImageStream, aImageExt, aFreeStreamsOnFinish);
    fRegions := TglrTextureRegionsList.Create(8);
    lines := LoadStringList(aInfoStream);
    for i := 1 to lines.Count - 1 do
    begin
      list := ParseLine(lines[i]);
      if list.Count > 0 then
      begin
        New(p);
        p^.Name := list[0];
        p^.Texture := Self;
        p^.tx := Convert.ToInt(list[1]) / Self.Width;
        p^.ty := Convert.ToInt(list[2]) / Self.Height;
        p^.tw := Convert.ToInt(list[3]) / Self.Width;
        p^.th := Convert.ToInt(list[4]) / Self.Height;
        p^.Rotated := ((list.Count > 9) and (list[9] = 'r'));
        fRegions.Add(p);
      end
      else
        Log.Write(lError, 'TextureAtlas: Cheetah atlas - can not parse line `' + lines[i] + '`');
      list.Free();
    end;
    lines.Free();

    if aFreeStreamsOnFinish then
      aInfoStream.Free();
  end
  else
    Log.Write(lCritical, 'TextureAtlas: unrecognizable info file extension: `' + aInfoExt + '`');
end;

destructor TglrTextureAtlas.Destroy;
var
  i: Integer;
begin
  for i := 0 to fRegions.Count - 1 do
    Dispose(fRegions[i]);
  fRegions.Free();
  inherited Destroy;
end;

function TglrTextureAtlas.GetRegion(aName: AnsiString): PglrTextureRegion;
var
  i: Integer;
begin
  for i := 0 to fRegions.Count - 1 do
    if fRegions[i]^.Name = aName then
      Exit(fRegions[i]);
  Log.Write(lCritical, 'TextureAtlas: Can not find requested region "' + aName + '" at atlas');
end;

{ TglrFontBatch }

constructor TglrFontBatch.Create(aFont: TglrFont);
begin
  inherited Create;
  if (aFont = nil) then
    Log.Write(lCritical, 'FontBatch: Null pointer provided, Font object expected');
  fFont := aFont;
  fVB := TglrVertexBuffer.Create(nil, 65536, vfPos3Tex2Col4);
  fIB := TglrIndexBuffer.Create(nil, 65536, ifShort);
end;

destructor TglrFontBatch.Destroy;
begin
  fVB.Free();
  fIB.Free();
  inherited Destroy;
end;

procedure TglrFontBatch.Start;
begin
  fCount := 0;
end;

procedure TglrFontBatch.Draw(aText: TglrText);
var
  x, y: Single;
  quad: TglrQuadP3T2C4;
  j, k: Integer;
begin
  x := 0;
  y := 0;
  if (not aText.Visible) or (aText.Text = '') then
    Exit();

  for j := 1 to Length(aText.Text) do
  begin
    if (aText.Text[j] = #10) then
    begin
      x := 0;
      y += aText.LineSpacing + fFont.MaxCharHeight;
      continue;
    end;

    if fFont.Table[aText.Text[j]] = nil then
      continue;

    quad := fFont.GetCharQuad(aText.Text[j]);
    //Do not need it anymore - included in AbsoluteMatrix computing
    //child.Matrix.Pos := child.Position;
    for k := 0 to 3 do
    begin
      fVData[fCount * 4 + k] := quad[k];
      fVData[fCount * 4 + k].vec += dfVec3f(x, y, 0);
      fVData[fCount * 4 + k].vec := aText.AbsoluteMatrix * fVData[fCount * 4 + k].vec;
      fVData[fCount * 4 + k].col := aText.Color;
    end;

    for k := 0 to 5 do
      fIData[fCount * 6 + k] := SpriteIndices[k] + fCount * 4;

    x += quad[0].vec.x + aText.LetterSpacing;
    fCount += 1;
  end;
end;

procedure TglrFontBatch.Finish;
begin
  if fCount = 0 then
    Exit();
  fVB.Update(@fVData[0], 0, fCount * 4);
  fIB.Update(@fIData[0], 0, fCount * 6);

  Render.Params.ModelViewProj := Render.Params.ViewProj;
  fFont.Material.Bind();
  Render.DrawTriangles(fVB, fIB, 0, 6 * fCount);
  //Font.Material.Unbind();
end;

{ TglrSpriteBatch }

constructor TglrSpriteBatch.Create;
begin
  inherited Create;
  fVB := TglrVertexBuffer.Create(nil, 65536, vfPos3Tex2Col4);
  fIB := TglrIndexBuffer.Create(nil, 65536, ifShort);
end;

destructor TglrSpriteBatch.Destroy;
begin
  fVB.Free();
  fIB.Free();
  inherited Destroy;
end;

procedure TglrSpriteBatch.Start;
begin
  fCount := 0;
end;

procedure TglrSpriteBatch.Draw(aSprite: TglrSprite);
var
  i: Integer;
begin
  if (aSprite.Visible) then
  begin
    for i := 0 to 3 do
    begin
      fVData[fCount * 4 + i] := aSprite.Vertices[i];
      fVData[fCount * 4 + i].vec := aSprite.AbsoluteMatrix * fVData[fCount * 4 + i].vec;
    end;

    for i := 0 to 5 do
      fIData[fCount * 6 + i] := SpriteIndices[i] + fCount * 4;
    fCount += 1;
  end;
end;

procedure TglrSpriteBatch.Draw(aSprites: array of TglrSprite);
var
  i: Integer;
begin
  for i := 0 to Length(aSprites) - 1 do
    Draw(aSprites[i]);
end;

procedure TglrSpriteBatch.Draw(aSprites: TglrSpriteList);
var
  i: Integer;
begin
  for i := 0 to aSprites.Count - 1 do
    Draw(aSprites[i]);
end;

procedure TglrSpriteBatch.Finish;
begin
  if fCount = 0 then
    Exit();
  fVB.Update(@fVData[0], 0, fCount * 4);
  fIB.Update(@fIData[0], 0, fCount * 6);
  Render.Params.ModelViewProj := Render.Params.ViewProj;
  Render.DrawTriangles(fVB, fIB, 0, fCount * 6);
end;

{ TglrText }

procedure TglrText.SetHorAlign(aValue: TglrTextHorAlign);
begin
  if fHorAlign = aValue then
    Exit();
  fHorAlign := aValue;
  Log.Write(lCritical, 'Text.SetHorAlign is not implemented');
end;

procedure TglrText.SetTextWidth(aValue: Single);
begin
  if fTextWidth = aValue then
    Exit();
  fTextWidth := aValue;
  Log.Write(lCritical, 'Text.SetTexWidth is not implemented');
end;

procedure TglrText.SetVerAlign(aValue: TglrTextVerAlign);
begin
  if fVerAlign = aValue then
    Exit();
  fVerAlign := aValue;
  Log.Write(lCritical, 'Text.SetVerAlign is not implemented');
end;

constructor TglrText.Create(const aText: WideString);
begin
  inherited Create();
  Text := aText;
  LineSpacing := 2.0;
  LetterSpacing := 1.0;
  Color := dfVec4f(1, 1, 1, 1);
end;

destructor TglrText.Destroy;
begin
  inherited Destroy;
end;

procedure TglrText.RenderSelf;
begin

end;

{ TglrFont }

function TglrFont.GetCharQuad(aChar: WideChar): TglrQuadP3T2C4;
begin
  FillChar(Result[0], SizeOf(TglrVertexP3T2C4) * 4, 0);
  if Table[aChar] = nil then
    Exit();
  with Table[aChar]^ do
  begin
    Result[0].vec := dfVec3f(w, py + h, 0);
    Result[1].vec := dfVec3f(w, py, 0);
    Result[2].vec := dfVec3f(0, py, 0);
    Result[3].vec := dfVec3f(0, py + h, 0);

    Result[0].tex := dfVec2f(tx + tw, ty + th);
    Result[1].tex := dfVec2f(tx + tw, ty);
    Result[2].tex := dfVec2f(tx, ty);
    Result[3].tex := dfVec2f(tx, ty + th);
  end;
end;

constructor TglrFont.Create;
begin
  inherited Create();
  if not Default.fInited then
    Log.Write(lCritical, 'Font: Can not create default font - default assets are disabled');
  Create(FileSystem.ReadResource('default assets/default.fnt'));
end;

constructor TglrFont.Create(aStream: TglrStream; aFreeStreamOnFinish: Boolean);
var
  data: Pointer;
  charCount, i: LongWord;
begin
  inherited Create();

  Material := TglrMaterial.Create();
  if Default.fInited then;
    Material.Shader := Default.SpriteShader;
  Texture := TglrTexture.Create(aStream, 'bmp', False);
  Material.AddTexture(Texture, 'uDiffuse');

  data := LoadFontData(aStream, charCount);
  SetLength(Self.CharData, charCount);
  Move(data^, CharData[0], charCount * SizeOf(TglrCharData));
  FreeMem(data);
  MaxCharHeight := 0;
  for i := 0 to charCount - 1 do
  begin
    Table[CharData[i].ID] := @CharData[i];
    if CharData[i].h > MaxCharHeight then
      MaxCharHeight := CharData[i].h;
  end;

  if aFreeStreamOnFinish then
    aStream.Free();
end;

destructor TglrFont.Destroy;
begin
  Material.Free();
  Texture.Free();
  SetLength(CharData, 0);
  inherited Destroy;
end;

{ TglrRenderParams }

procedure TglrRenderParams.CalculateMVP;
begin
  ModelViewProj := ViewProj * Model;
end;

{ TglrSprite }

procedure TglrSprite.SetRot(const aRot: Single);
begin
  if (not Equalf(aRot, fRot)) then
  begin
    Matrix.Identity();
    Matrix.Rotate(aRot * deg2rad, dfVec3f(0, 0, 1));
    fRot := aRot;
    if fRot > 360 then
      fRot -= 360
    else if fRot < -360 then
      fRot += 360;
  end;
end;

procedure TglrSprite.SetWidth(const aWidth: Single);
begin
  if (not Equalf(aWidth, fWidth)) then
  begin
    fWidth := aWidth;
    SetDefaultVertices();
  end;
end;

procedure TglrSprite.SetHeight(const aHeight: Single);
begin
  if (not Equalf(aHeight, fHeight)) then
  begin
    fHeight := aHeight;
    SetDefaultVertices();
  end;
end;

procedure TglrSprite.SetPP(const aPP: TdfVec2f);
begin
  if (aPP <> fPP) then
  begin
    fPP := aPP;
    SetDefaultVertices();
  end;
end;

constructor TglrSprite.Create;
begin
  Create(1, 1, dfVec2f(0.5, 0.5));
end;

constructor TglrSprite.Create(aWidth, aHeight: Single; aPivotPoint: TdfVec2f);
begin
  inherited Create();
  fWidth := aWidth;
  fHeight := aHeight;
  fPP := aPivotPoint;
  SetDefaultVertices();
  SetDefaultTexCoords();
  SetVerticesColor(dfVec4f(1, 1, 1, 1));
end;

destructor TglrSprite.Destroy;
begin
  inherited Destroy;
end;

procedure TglrSprite.SetDefaultVertices;
begin
  Vertices[0].vec := dfVec3f((dfVec2f(1, 1) - fPP) * dfVec2f(fWidth, fHeight), 0);
  Vertices[1].vec := dfVec3f((dfVec2f(1, 0) - fPP) * dfVec2f(fWidth, fHeight), 0);
  Vertices[2].vec := dfVec3f((fPP.NegateVector)    * dfVec2f(fWidth, fHeight), 0);
  Vertices[3].vec := dfVec3f((dfVec2f(0, 1) - fPP) * dfVec2f(fWidth, fHeight), 0);
end;

procedure TglrSprite.SetDefaultTexCoords;
begin
  Vertices[0].tex := dfVec2f(1, 1);
  Vertices[1].tex := dfVec2f(1, 0);
  Vertices[2].tex := dfVec2f(0, 0);
  Vertices[3].tex := dfVec2f(0, 1);
end;

procedure TglrSprite.SetVerticesColor(aColor: TdfVec4f);
begin
  Vertices[0].col := aColor;
  Vertices[1].col := aColor;
  Vertices[2].col := aColor;
  Vertices[3].col := aColor;
end;

procedure TglrSprite.SetSize(aWidth, aHeight: Single);
begin
  fWidth := aWidth;
  fHeight := aHeight;
  SetDefaultVertices();
end;

procedure TglrSprite.SetSize(aSize: TdfVec2f);
begin
  SetSize(aSize.x, aSize.y);
end;

procedure TglrSprite.SetTextureRegion(aRegion: PglrTextureRegion;
  aAdjustSpriteSize: Boolean);
begin
  with aRegion^ do
    if not Rotated then
    begin
      Vertices[0].tex := dfVec2f(tx + tw, ty + th);
      Vertices[1].tex := dfVec2f(tx + tw, ty);
      Vertices[2].tex := dfVec2f(tx, ty);
      Vertices[3].tex := dfVec2f(tx, ty + th);
      if aAdjustSpriteSize then
      begin
        Width := tw * Texture.Width;
        Height := th * Texture.Height;
      end;
    end
    else
    begin
      Vertices[0].tex := dfVec2f(tx, ty + th);
      Vertices[1].tex := dfVec2f(tx + tw, ty + th);
      Vertices[2].tex := dfVec2f(tx + tw, ty);
      Vertices[3].tex := dfVec2f(tx, ty);
      if aAdjustSpriteSize then
      begin
        Width := th * Texture.Height;
        Height := tw * Texture.Width;
      end;
    end;
end;

procedure TglrSprite.RenderSelf;
begin

end;

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
  Self.Write(lInformation, 'End. Errors: ' + Convert.ToString(fTotalErrors) + ', warnings: ' + Convert.ToString(fTotalWarnings));
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
  if (aType = lWarning) then
    fTotalWarnings += 1
  else if (aType = lError) then
    fTotalErrors += 1
  else if (aType = lCritical) then
    Assert(False, 'Critical error detected: ' + aMessage);
  {$endif}
end;

{ Default }

class procedure Default.Init;
var
  blankData: Pointer;
begin
  SpriteShader := TglrShaderProgram.Create();
  SpriteShader.Attach(FileSystem.ReadResource('default assets/SpriteShaderV.txt'), stVertex);
  SpriteShader.Attach(FileSystem.ReadResource('default assets/SpriteShaderF.txt'), stFragment);
  SpriteShader.Link();

  GetMem(blankData, 1 * 1 * 3(*RGB*));
  FillChar(blankData^, 3, $FF);
  BlankTexture := TglrTexture.Create(blankData, 1, 1, tfRGB8);
  FreeMem(blankData);

  fInited := True;
end;

class procedure Default.Deinit;
begin
  if (not fInited) then
    Exit();

  SpriteShader.Free();
  BlankTexture.Free();
end;

{ TglrMaterial }

constructor TglrMaterial.Create;
begin
  inherited;
  Shader := TglrShaderProgram.Create();
  SetLength(Textures, 0);

  Blend := bmAlpha;
  Color := dfVec4f(1, 1, 1, 1);
  Cull := cmBack;
  DepthTest := True;
  DepthWrite := True;
  DepthTestFunc := fcLess;
end;

constructor TglrMaterial.Create(aStream: TglrStream;
  aFreeStreamOnFinish: Boolean);
begin
  Create();
  Log.Write(lCritical, 'Material create from stream is not implemented');
end;

destructor TglrMaterial.Destroy;
var
  i: Integer;
begin
  //for i := 0 to Length(Textures) - 1 do
  //  Textures[i].Texture.Free();
  SetLength(Textures, 0);
  //Wow, such a hack...
  if (Shader <> Default.SpriteShader) then
    Shader.Free();
  inherited Destroy;
end;

procedure TglrMaterial.AddTexture(aTexture: TglrTexture;
  aUniformName: AnsiString);
var
  l, ind: Integer;
begin
  l := Length(Textures);
  ind := Shader.AddUniform(utSampler, 1, aUniformName, Pointer(l));
  if ind = -1 then
    Log.Write(lError, 'Can not add texture to material - no such uniform name ("' + aUniformName + '") at shader')
  else
  begin
    SetLength(Textures, l + 1);
    with Textures[l] do
    begin
      Texture := aTexture;
      UniformName := aUniformName;
      ShaderInternalIndex := ind;
    end;
  end;
end;

procedure TglrMaterial.Bind;
var
  i: Integer;
begin
  Render.SetBlendingMode(Blend);
  Render.SetCullMode(Cull);
  Render.SetDepthWrite(DepthWrite);
  Render.SetDepthTest(DepthTest);
  Render.SetDepthFunc(DepthTestFunc);

  Render.Params.Color := Color;
  for i := 0 to Length(Textures) - 1 do
    Textures[i].Texture.Bind(i);

  Shader.Bind();
end;

procedure TglrMaterial.Unbind;
begin
  Shader.Unbind();
end;

{ TglrShaderProgram }

function TglrShaderProgram.GetVertexAtribName(const aAtrib: TglrVertexAtrib
  ): AnsiString;
begin
  WriteStr(Result, aAtrib);
end;

procedure TglrShaderProgram.Bind;
var
  i: Integer;
begin
  Render.SetShader(Self.Id);

  for i := 0 to Length(Uniforms) - 1 do
    if Uniforms[i].fData <> nil then
      SetUniform(i, Uniforms[i].fData);
end;

class procedure TglrShaderProgram.Unbind;
begin
  Render.SetShader(0);
end;

procedure TglrShaderProgram.Attach(aStream: TglrStream;
  aShaderType: TglrShaderType; aFreeStreamOnFinish: Boolean);
var
  aType: TGLConst;
  i: Integer;
  param, len: Integer;
  ErrorLog: PAnsiChar;
  data: PAnsiChar;
  v: TglrVertexAtrib;
begin
  case aShaderType of
    stVertex: aType := GL_VERTEX_SHADER;
    stFragment: aType := GL_FRAGMENT_SHADER;
  end;
  i := Length(ShadersId);
  SetLength(ShadersId, i + 1);
  ShadersId[i] := gl.CreateShader(aType);
  data := LoadText(aStream);
  gl.ShaderSource(ShadersId[i], 1, @data, nil);
  FreeMem(data);
  gl.CompileShader(ShadersId[i]);
  gl.GetShaderiv(ShadersId[i], GL_COMPILE_STATUS, @param);
  if (param = Ord(GL_FALSE)) then
  begin
    gl.GetShaderiv(ShadersId[i], GL_INFO_LOG_LENGTH, @param);
    GetMem(ErrorLog, param);
    gl.GetShaderInfoLog(ShadersId[i], param, len, ErrorLog);
    Log.Write(lCritical, 'Shader compilation failed. Log:'#13#10#9 + ErrorLog);
    FreeMem(ErrorLog, param);
  end;
  if (aFreeStreamOnFinish) then
    aStream.Free();

  if (aShaderType = stVertex) then
    for v := Low(TglrVertexAtrib) to High(TglrVertexAtrib) do
      gl.BindAttribLocation(Self.Id, Ord(v), PAnsiChar(GetVertexAtribName(v)));

  gl.AttachShader(Self.Id, ShadersId[i]);
end;

procedure TglrShaderProgram.Link;
var
  param: TGLConst;
  len: LongInt;
  infoLog: PAnsiChar;
begin
  //Link
  gl.LinkProgram(Self.Id);
  gl.GetProgramiv(Self.Id, GL_LINK_STATUS, @param);
  if (param = GL_FALSE) then
  begin
    gl.GetProgramiv(Self.Id, GL_INFO_LOG_LENGTH, @len);
    GetMem(infoLog, len);
    gl.GetProgramInfoLog(Self.Id, len, len, infoLog);
    Log.Write(lCritical, 'Shader link failed. Log:'#13#10#9 + InfoLog);
    FreeMem(infoLog, len);
  end;

  //Validate
  gl.ValidateProgram(Id);
  gl.GetProgramiv(Self.Id, GL_VALIDATE_STATUS, @param);
  if (param = GL_FALSE) then
  begin
    gl.GetProgramiv(Self.Id, GL_INFO_LOG_LENGTH, @len);
    GetMem(infoLog, len);
    gl.GetProgramInfoLog(Self.Id, len, len, infoLog);
    Log.Write(lError, 'Shader validate failed. Log:'#13#10#9 + InfoLog); //sometimes it is not critical
    FreeMem(infoLog, len);
  end;

  //Set shared uniforms
  AddUniform(utMat4, 1, 'uModelViewProj', @Render.Params.ModelViewProj);
  AddUniform(utVec4, 1, 'uColor', @Render.Params.Color);
end;

function TglrShaderProgram.AddUniform(aUniformType: TglrUniformType;
  aCount: Integer; aName: AnsiString; aData: Pointer = nil): Integer;
var
  l: Integer;
  index: Integer;
begin
  index := gl.GetUniformLocation(Id, PAnsiChar(aName));
  if (index = -1) then
    Log.Write(lError, 'Uniform "' + aName + '" was not found in shader')
  else
  begin
    l := Length(Uniforms);
    SetLength(Uniforms, l + 1);
    with Uniforms[l] do
    begin
      fType := aUniformType;
      fName := aName;
      fCount := aCount;
      fIndex := index;
      fData := aData;
    end;
    Result := l;
  end;
end;

function TglrShaderProgram.GetUniformIndexByName(aName: AnsiString): Integer;
var
  i: Integer;
begin
  for i := 0 to Length(Uniforms) - 1 do
    if Uniforms[i].fName = aName then
      Exit(i);
  Exit(-1);
end;

procedure TglrShaderProgram.SetUniform(aUniformType: TglrUniformType;
  aCount: Integer; aValue: Pointer; aName: PAnsiChar; aIndex: Integer);
begin
  if (aIndex = -1) then
    aIndex := gl.GetUniformLocation(Id, aName);
  if (aIndex = -1) then
    Log.Write(lError, 'Uniform "' + aName + '" was not found in shader')
  else
    case aUniformType of
      utVec1: gl.Uniform1fv(aIndex, aCount, aValue);
      utVec2: gl.Uniform2fv(aIndex, aCount, aValue);
      utVec3: gl.Uniform3fv(aIndex, aCount, aValue);
      utVec4: gl.Uniform4fv(aIndex, aCount, aValue);
      utMat4: gl.UniformMatrix4fv(aIndex, aCount, False, aValue);
      utSampler: gl.Uniform1iv(aIndex, aCount, aValue);
    end;
end;

procedure TglrShaderProgram.SetUniform(aInternalIndex: Integer; aValue: Pointer);
begin
  if (aInternalIndex < 0) or (aInternalIndex > High(Uniforms)) then
    Log.Write(lError,
      'Internal index of uniform ('+ Convert.ToString(aInternalIndex) +
      ') is out of range (0-' + Convert.ToString(High(Uniforms)))
  else
    with Uniforms[aInternalIndex] do
    begin
      fData := aValue;
      SetUniform(fType, fCount, fData, PAnsiChar(fName), fIndex);
    end;
end;

constructor TglrShaderProgram.Create;
begin
  inherited;
  Self.Id := gl.CreateProgram();
  SetLength(ShadersId, 0);
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
  Position += v;
  UpdateVectorsFromMatrix();
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

function TglrCamera.WindowPosToCameraPos(aPos: TdfVec2f): TdfVec2f;
begin
  Result := dfVec2f(Position) + aPos * (1 / fScale);
  case fProjectionPivot of
    pTopLeft: ;
    pCenter:      Result -= dfVec2f(fW / 2, fH / 2) * (1 / fScale);
    pBottomRight: Result -= dfVec2f(fW,     fH)     * (1 / fScale);
  end;
end;

procedure TglrCamera.Update;
begin
  Matrix.Pos := dfVec3f(0, 0, 0);
  Matrix.Pos := Matrix * Position.NegateVector;
  Render.Params.ViewProj := fProjMatrix * Matrix;
  Render.Params.ModelViewProj := Render.Params.ViewProj;
  UpdateVectorsFromMatrix();

  if (Render.Width <> fW) or (Render.Height <> fH) then
    ViewportOnly(0, 0, Render.Width, Render.Height);
end;

procedure TglrCamera.SetCamera(aPos, aTargetPos, aUp: TdfVec3f);
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

{ Convert }

class function Convert.ToString(aVal: Integer): AnsiString;
begin
  Str(aVal, Result);
end;

class function Convert.ToString(aVal: Single; Digits: Integer = 5): AnsiString;
begin
  Str(aVal:0:Digits, Result);
end;

class function Convert.ToString(aVal: TdfMat4f; aDigits: Integer): AnsiString;
var
  p: PSingle;
  i: Integer;
begin
  p := @aVal;
  Result := '';
  for i := 0 to 15 do
  begin
    Result += ToString(p[i], aDigits) + ' ';
    if i in [3, 7, 11] then
      Result += #13#10;
  end;
  //log.Write(lCritical, 'Convert mat to string is not implemented');
end;

class function Convert.ToString(aVal: TdfVec2f): AnsiString;
begin
  Result := ToString(aVal.x) + '|' + ToString(aVal.y);
end;

class function Convert.ToString(aVal: TdfVec3f): AnsiString;
begin
  Result := ToString(aVal.x) + '|' + ToString(aVal.y) + '|' + ToString(aVal.z);
end;

class function Convert.ToString(aVal: TdfVec4f): AnsiString;
begin
  Result := ToString(aVal.x) + '|' + ToString(aVal.y) + '|' + ToString(aVal.z)
   + '|' + ToString(aVal.w);
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
  //GetAbsMatrix();
end;

function TglrNode.GetAbsMatrix: TdfMat4f;
begin
  Matrix.Pos := Position;
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
  aDir.Normalize;
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
  aRight.Normalize;
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
  aUp.Normalize;
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

constructor TglrNode.Create;
begin
  inherited Create();
  Matrix.Identity;
  Visible := True;
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

{ FileSystem }

function lzo_compress(const Data; Size: LongInt; var CData; var CSize: LongInt; var WorkBuf): LongInt; cdecl;
asm
//{$IFDEF WIN32}
//jmp lzo_compress+$2F0+8+3
///{$ELSE}
  pop ebp
  lea eax, @dest + $2F0
  jmp eax
@dest:
//{$ENDIF}
  DD $83EC8B55,$5653E8C4,$C458B57,$308558B,$FC5589D0,$89F3C283,$458BF855,$F4458918,$8B10558B
  DD $F08B0845,$3304C083,$8ADB33C9,$588A0348,$6E1C102,$DB33CB33,$8A05E1C1,$CB330158,$E1C1DB33
  DD $33188A05,$C1D98BCB,$CB0305E1,$8105E9C1,$3FFFE1,$EC4D8900,$8BF44D8B,$C8BEC5D,$3BD98B99
  DD $7E72085D,$FB2BF88B,$85F07D89,$817376FF,$BFFFF07D,$6A770000,$F07D81,$76000008,$3598A51
  DD $7403583A,$EC4D8B49,$7FFE181,$F1810000,$201F,$8BEC4D89,$5D8BF44D,$990C8BEC,$5D3BD98B
  DD $8B377208,$89FB2BF8,$FF85F07D,$7D812C76,$BFFFF0,$81237700,$800F07D,$A760000,$3A03598A
  DD $2740358,$8B6610EB,$183B6619,$598A0875,$2583A02,$4D8B1874,$EC5D8BF4,$40990489,$FF8453B
  DD $1D883,$FF25E900,$5D8BFFFF,$EC7D8BF4,$8BBB0489,$85DE2BD8,$89567EDB,$7D83E85D,$87703E8
  DD $8E85D8A,$3AEBFE5A,$12E87D83,$5D8A0B77,$3EB80E8,$EB421A88,$E87D8B29,$420002C6,$8112EF83
  DD $FFFF,$81127600,$FFEF,$2C600,$FFFF8142,$77000000,$88DF8BEE,$1E8A421A,$421A8846,$75E84DFF
  DD $3C083F5,$4003598A,$75FF583A,$4598A2D,$FF583A40,$598A2475,$583A4005,$8A1B75FF,$3A400659
  DD $1275FF58,$4007598A,$75FF583A,$8598A09,$FF583A40,$8B487674,$81CE2BC8,$800F07D,$F18B0000
  DD $4DFF2577,$49CE8BF0,$8A05E1C1,$E380F05D,$2E3C107,$A88CB0A,$F04D8B42,$8803E9C1,$F2E9420A
  DD $81000000,$4000F07D,$13770000,$8BF04DFF,$2E980CE,$8820C980,$C1E9420A,$81000000,$4000F06D
  DD $4D8B0000,$81DE8BF0,$4000E1,$2EB8000,$800BE9C1,$CB0A10C9,$E9420A88,$9C,$83FC7D8B,$2EB09C1
  DD $F83B4041,$198A0676,$F474183A,$CE2BC88B,$7D81F18B,$4000F0,$FF1E7700,$FE83F04D,$8B0D7721
  DD $2E980CE,$8820C980,$64EB420A,$C621EE83,$EB422002,$F06D813C,$4000,$7709FE83,$F04D8B1B
  DD $E181DE8B,$4000,$C102EB80,$C9800BE9,$88CB0A10,$34EB420A,$8B09EE83,$E181F04D,$4000
  DD $800BE9C1,$A8810C9,$FFFE8142,$76000000,$FFEE8112,$C6000000,$81420002,$FFFE,$8BEE7700
  DD $420A88CE,$80F04D8A,$E1C13FE1,$420A8802,$C1F04D8B,$A8806E9,$3BF08B42,$573F845,$FFFD52E9
  DD $10552BFF,$8914458B,$FC458B10,$5E5FC62B,$5DE58B5B,$909090C3,$53EC8B55,$758B5756,$C7D8B10
  DD $FF83DE8B,$8B04770D,$8B1BEBC7,$8B521855,$5351144D,$8458B57,$FCE6E850,$C483FFFF,$14558B14
  DD $C0851A03,$4D8B6676,$2BCF0308,$8BF33BC8,$3D1175F9,$EE,$D08B0A77,$8811C280,$3FEB4313
  DD $7703F883,$FE430805,$F88335EB,$8B0A7712,$3E980C8,$EB430B88,$C6D08B26,$83430003,$FA8112EA
  DD $FF,$EA811276,$FF,$430003C6,$FFFA81,$EE770000,$8A431388,$B88470F,$F7754843,$431103C6,$430003C6
  DD $430003C6,$458BDE2B,$33188914,$5B5E5FC0,$9090C35D
end;

function lzo_decompress(const CData; CSize: LongInt; var Data; var Size: LongInt): LongInt; cdecl;
asm
  DB $51
  DD $458B5653,$C558B08,$F08BD003,$33FC5589,$144D8BD2,$68A1189,$3C10558B,$331C7611,$83C88AC9
  DD $8346EFC1,$820F04F9,$1C9,$8846068A,$75494202,$3366EBF7,$460E8AC9,$F10F983,$8D83,$75C98500,$8107EB18
  DD $FFC1,$3E804600,$33F47400,$83068AC0,$C8030FC0,$83068B46,$28904C6,$4904C283,$F9832F74,$8B217204,$83028906
  DD $C68304C2,$4E98304,$7304F983,$76C985EE,$46068A14,$49420288,$9EBF775,$8846068A,$75494202,$8AC933F7
  DD $F983460E,$C12B7310,$828D02E9,$FFFFF7FF,$C933C12B,$C1460E8A,$C12B02E1,$8840088A,$88A420A,$420A8840
  DD $288008A,$113E942,$F9830000,$8B207240,$FF428DD9,$8302EBC1,$C32B07E3,$1E8ADB33,$3E3C146,$2B05E9C1
  DD $D9E949C3,$83000000,$2F7220F9,$851FE183,$EB1875C9,$FFC18107,$46000000,$74003E80,$8AC033F4,$1FC08306
  DD $F46C803,$FBC11EB7,$FF428D02,$C683C32B,$8369EB02,$457210F9,$D98BC28B,$C108E383,$C32B0BE3,$8507E183
  DD $EB1875C9,$FFC18107,$46000000,$74003E80,$8ADB33F4,$7C3831E,$F46CB03,$FBC11EB7,$83C32B02,$D03B02C6
  DD $9A840F,$2D0000,$EB000040,$2E9C11F,$2BFF428D,$8AC933C1,$E1C1460E,$8AC12B02,$A884008,$88008A42
  DD $51EB4202,$7206F983,$2BDA8B37,$4FB83D8,$188B2E7C,$8904C083,$4C2831A,$8B02E983,$831A8918,$C08304C2
  DD $4E98304,$7304F983,$76C985EE,$40188A20,$49421A88,$15EBF775,$8840188A,$188A421A,$421A8840,$8840188A
  DD $7549421A,$8AC933F7,$E183FE4E,$FC98503,$FFFE4284,$46068AFF,$49420288,$C933F775,$E9460E8A,$FFFFFECA
  DD $8B10552B,$10891445,$75FC753B,$EBC03304,$FFF8B80D,$753BFFFF,$830372FC,$5B5E04C0,$90C35D59
end;

procedure CompressData(const InData: Pointer; InSize: LongInt; out OutData: Pointer; out OutSize: LongInt);
var
  WorkBuf : array [Word] of Byte;
begin
// в случае брутфорс сжатия нужно менять данные в WorkBuf
  FillChar(WorkBuf, SizeOf(WorkBuf), 0);
  OutSize := InSize + ((InSize div 1024) + 1) * 16;
  OutData := GetMemory(OutSize);
  lzo_compress(InData^, InSize, OutData^, OutSize, WorkBuf);
end;

procedure DecompressData(const InData: Pointer; InSize: LongInt; const OutData: Pointer; var OutSize: LongInt);
begin
  lzo_decompress(InData^, InSize, OutData^, OutSize);
end;


class procedure FileSystem.Init(const aPackFilesPath: AnsiString);
var
  packFilesList: TglrStringList;
  i, l, j: Integer;
  stream: TglrStream;
  WordBuf, bytesRead: Word;
begin
  fPackFilesPath := aPackFilesPath;
  packFilesList := TglrStringList.Create();
  FindFiles(fPackFilesPath, PACK_FILE_EXT, packFilesList);
  Log.Write(lInformation, 'FileSystem: pack files found at "' + fPackFilesPath + '": ' + Convert.ToString(packFilesList.Count));
  SetLength(fPackFiles, packFilesList.Count);

  l := 0;
  for i := 0 to packFilesList.Count - 1 do
  begin
    stream := FileSystem.ReadResource(packFilesList[i], False);

    //Read magic header
    bytesRead := stream.Read(WordBuf, SizeOf(Word));
    if ((WordBuf <> PACK_FILE_MAGIC) and (WordBuf <> PACK_FILE_MAGIC_LZO)) or (bytesRead < SizeOf(Word)) then
    begin
      Log.Write(lError, #9 + packFilesList[i] + ': not a correct pack file');
      SetLength(fPackFiles, Length(fPackFiles) - 1);
      stream.Free();
      continue;
    end;

    //Set main params for record
    fPackFiles[l].fPackName := packFilesList[i];
    fPackFiles[l].fLoaded := False;
    fPackFiles[l].fLZO := (WordBuf = PACK_FILE_MAGIC_LZO);

    //Read files count
    bytesRead := stream.Read(WordBuf, SizeOf(Word));
    if (bytesRead <> SizeOf(Word)) then
    begin
      log.Write(lError, #9 + packFilesList[i] + ': error while read file count');
      SetLength(fPackFiles, Length(fPackFiles) - 1);
      stream.Free();
      continue;
    end;

    //Read file headers: name, stride and size
    SetLength(fPackFiles[l].fFiles, WordBuf);
    for j := 0 to WordBuf - 1 do
    begin
      fPackFiles[l].fFiles[j].fFileName := stream.ReadAnsi();
      stream.Read(fPackFiles[l].fFiles[j].fStride, SizeOf(LongWord));
      stream.Read(fPackFiles[l].fFiles[j].fSize, SizeOf(LongWord));
    end;

    Log.Write(lInformation, #9 + packFilesList[i] + ': header loaded. Files inside: ' + Convert.ToString(WordBuf));
    for j := 0 to Length(fPackFiles[l].fFiles) - 1 do
      Log.Write(lInformation, #9#9 + fPackFiles[l].fFiles[j].fFileName + ' - ' + Convert.ToString(Integer(fPackFiles[l].fFiles[j].fSize)) + ' bytes');
    stream.Free();
    l += 1;
  end;
  packFilesList.Free();
end;

class procedure FileSystem.DeInit;
var
  i: Integer;
begin
  for i := 0 to High(fPackFiles) do
    if (fPackFiles[i].fLoaded) then
      UnloadPack(fPackFiles[i].fPackName);
  SetLength(fPackFiles, 0);
end;

class function FileSystem.GetFileIndexInPackFile(packIndex: Integer;
  aFileName: AnsiString): Integer;
var
  i: Integer;
begin
  Result := -1;
  if (packIndex < 0) or (packIndex > High(fPackFiles)) then
  begin
    Log.Write(lError, 'Wrong pack index provided: ' + Convert.ToString(packIndex)
      + '. Bounds: 0..' + Convert.ToString(High(fPackFiles)));
    Exit();
  end;

  for i := 0 to High(fPackFiles[packIndex].fFiles) do
    if (fPackFiles[packIndex].fFiles[i].fFileName = aFileName) then
      Exit(i);
end;

class function FileSystem.GetPackIndexByPackName(const aPackName: AnsiString): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Length(fPackFiles) - 1 do
    if (fPackFiles[i].fPackName = aPackName) then
      Exit(i);
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
  i, f: Integer;
  PackFile: TglrStream;
  m: Pointer;
  bytesRead: LongInt;
begin
  if (FileExists(aFileName)) then
  begin
    //todo: load directly into memory?
    Log.Write(lInformation, 'FileSystem: start reading resource "' + aFileName + '" directly from file');
    Result := TglrStream.Init(aFileName);
    Log.Write(lInformation, 'FileSystem: read successfully');
    Exit();
  end

  //Try read from pack files
  else if (aSearchInPackFiles) then
    for i := 0 to Length(fPackFiles) - 1 do
    begin
      f := GetFileIndexInPackFile(i, aFileName);
      if (f <> -1) then //we have found requested file at pack with index 'f'
      begin
        Log.Write(lInformation, 'FileSystem: start reading resource "' + aFileName + '" from pack file "' + fPackFiles[i].fPackName + '"');
        if (not fPackFiles[i].fLoaded) then
        begin
          //Read pack file, seek to requested file's stride, read it into new Stream
          if (fPackFiles[i].fLZO) then
          begin
            Log.Write(lCritical, 'LZO is not supported yet');
          end;
          PackFile := TglrStream.Init(fPackFiles[i].fPackName);
          PackFile.Pos := fPackFiles[i].fFiles[f].fStride;
          GetMem(m, fPackFiles[i].fFiles[f].fSize);
          Result := TglrStream.Init(m, fPackFiles[i].fFiles[f].fSize, True); //True means that FreeMem will be executed at Stream.Free()
          bytesRead := PackFile.Read(m^, Result.FSize); //write directly, no need of Write (Read);
          PackFile.Free();

          if (bytesRead <> Result.FSize) then
            Log.Write(lCritical, 'FileSystem: resource "' + aFileName + '" read from packfile "' + fPackFiles[i].fPackName + '" failed');
        end
        else
        begin
          //load pack file from fPackData (memory)
          if (fPackFiles[i].fLZO) then
          begin
            Log.Write(lCritical, 'LZO is not supported yet');
          end;
          Result := TglrStream.Init(fPackFiles[i].fPackDataPointer + fPackFiles[i].fFiles[f].fStride, fPackFiles[i].fFiles[f].fSize);
        end;
        Log.Write(lInformation, 'FileSystem: read successfully');
        Exit();
      end;
    end;

  Log.Write(lError, 'FileSystem: requested resource "' + aFileName + '" was not found');
end;

class function FileSystem.ReadResourceLZO(const aFileName: AnsiString;
  aSearchInPackFiles: Boolean): TglrStream;
var
  fileStream: TglrStream;
  mIn, mOut: Pointer;
  outSize: LongInt;
begin
  if (FileExists(aFileName)) then
  begin
    Log.Write(lInformation, 'FileSystem: start reading LZO resource "' + aFileName + '" directly from file');
    fileStream := TglrStream.Init(aFileName);

    mIn := GetMemory(fileStream.Size);
    mOut := GetMemory(fileStream.Size * 10);

    fileStream.Read(mIn^, fileStream.Size);

    DecompressData(mIn, fileStream.Size, mOut, outSize);

    Result := TglrStream.Init(mOut, outSize, True);

    fileStream.Free();
    FreeMemory(mIn);
    Log.Write(lInformation, 'FileSystem: read successfully');
    Exit();
  end
  else if (aSearchInPackFiles) then
    Log.Write(lCritical, 'FileSystem.ReadResourceLZO from pack files is not implemented');

  Log.Write(lError, 'FileSystem: requested resource "' + aFileName + '" was not found');
end;

class procedure FileSystem.WriteResource(const aFileName: AnsiString;
  const aStream: TglrStream);
var
  FileStream: TglrStream;
begin
  if PathExists(ExtractFilePath(aFileName)) then
  begin
    FileStream := TglrStream.Init(aFileName, True);
    FileStream.CopyFrom(aStream);
    FileStream.Free();
  end
  else
    Log.Write(lError, 'FileSystem: Unable to write resource "' + aFileName + '", path is not exists');
end;

class procedure FileSystem.WriteResource(const aFileName: AnsiString;
  const aContent: AnsiString);
var
  t: Text;
begin
  if PathExists(ExtractFilePath(aFileName)) then
  begin
    AssignFile(t, aFileName);
    Rewrite(t);
    Write(t, aContent);
    CloseFile(t);
  end
  else
    Log.Write(lError, 'FileSystem: Unable to write resource "' + aFileName + '", path is not exists');
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
    #13#10 + 'line: ' + Convert.ToString(aLineNo) +
    #13#10 + 'addr: ' + Convert.ToString(Integer(aAddr)));
end;

class function Core.GetFPSText: AnsiString;
begin
  Result := Convert.ToString(fFPS, 1);
end;

class procedure Core.Init(aGame: TglrGame; aInitParams: TglrInitParams);
begin
  AssertErrorProc := @AssertErrorHandler;
  fGame := aGame;
  Log.Init(LOG_FILE);
  Input := TglrInput.Create();
  FileSystem.Init(aInitParams.PackFilesPath);
  fReady := False;

  fAppView :=
  {$IFDEF WINDOWS}TglrWindow{$ENDIF}
    .Create(@aInitParams);
  Render.Init();
  Render.SetVerticalSync(aInitParams.vSync);
  Render.SetClearColor(0.2, 0.21, 0.25);
  Render.SetViewPort(0, 0, aInitParams.Width, aInitParams.Height);
  if (aInitParams.UseDefaultAssets) then
    Default.Init();

  Log.Write(lInformation, 'Initialize completed'#13#10);
end;

class procedure Core.Loop();
begin
  fGame.OnStart();
  fReady := True;
  Log.Write(lInformation, 'Appication loop started');
  fAppView.Loop();
  fReady := False;
  fGame.OnFinish();
  Log.Write(lInformation, 'Appication loop finished');
end;

class procedure Core.Pause();
begin
  fGame.OnPause();
end;

class procedure Core.Resume();
begin
  fGame.OnResume();
end;

class procedure Core.Quit;
begin
  fAppView.Quit();
end;

class procedure Core.Update(const dt: Double);
begin
  fDT := dt;
  fFPS := 1 / fDT;
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
    begin
      Touch[Ord(aKey)].Pos := dfVec2f(X, Y);
      MousePos := dfVec2f(X, Y);
    end;
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
  iBuffer: TglrIndexBuffer; aStartIndex, aIndicesCount: Integer);
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

  gl.DrawElements(GL_TRIANGLES, aIndicesCount, IF_FORMAT[iBuffer.Format], Pointer(aStartIndex * IF_STRIDE[iBuffer.Format]));

  fDipCount += 1;
  fTriCount += aIndicesCount div 3;
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
  Format := aFormat;
  Self.Bind();
  gl.BufferData(GL_ELEMENT_ARRAY_BUFFER, IF_STRIDE[Format] * aCount, aData, GL_STATIC_DRAW);
  Self.Unbind();
end;

destructor TglrIndexBuffer.Destroy;
begin
  gl.DeleteBuffers(1, @Self.Id);
  inherited Destroy;
end;

procedure TglrIndexBuffer.Update(aData: Pointer; aStart, aCount: Integer);
begin
  gl.BindBuffer(GL_ELEMENT_ARRAY_BUFFER, Id);
  gl.BufferSubData(GL_ELEMENT_ARRAY_BUFFER, aStart, aCount * IF_STRIDE[Format], aData);
  gl.BindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
  Render.fIB := 0;
end;

{ TglrVertexBuffer }

procedure TglrVertexBuffer.Bind;
begin
  gl.BindBuffer(GL_ARRAY_BUFFER, Self.Id);
  case Format of
    vfPos3Tex2:
    begin
      gl.EnableVertexAttribArray(Ord(vaCoord));
      gl.EnableVertexAttribArray(Ord(vaTexCoord0));
			gl.VertexAttribPointer(Ord(vaCoord), 3, GL_FLOAT, False, VF_STRIDE[Format], nil);
      gl.VertexAttribPointer(Ord(vaTexCoord0), 2, GL_FLOAT, False, VF_STRIDE[Format], Pointer(SizeOf(TdfVec3f)));
    end;
    vfPos2Tex2:
    begin
      gl.EnableVertexAttribArray(Ord(vaCoord));
      gl.EnableVertexAttribArray(Ord(vaTexCoord0));
			gl.VertexAttribPointer(Ord(vaCoord), 2, GL_FLOAT, False, VF_STRIDE[Format], nil);
			gl.VertexAttribPointer(Ord(vaTexCoord0), 2, GL_FLOAT, False, VF_STRIDE[Format], Pointer(SizeOf(TdfVec2f)));
    end;
    vfPos3Tex2Nor3:
    begin
      gl.EnableVertexAttribArray(Ord(vaCoord));
      gl.EnableVertexAttribArray(Ord(vaTexCoord0));
      gl.EnableVertexAttribArray(Ord(vaNormal));
			gl.VertexAttribPointer(Ord(vaCoord), 3, GL_FLOAT, False, VF_STRIDE[Format], nil);
      gl.VertexAttribPointer(Ord(vaTexCoord0), 2, GL_FLOAT, False, VF_STRIDE[Format], Pointer(SizeOf(TdfVec3f)));
      gl.VertexAttribPointer(Ord(vaNormal), 3, GL_FLOAT, False, VF_STRIDE[Format], Pointer(SizeOf(TdfVec3f) + SizeOf(TdfVec2f)));
    end;
    vfPos3Tex2Col4:
    begin
      gl.EnableVertexAttribArray(Ord(vaCoord));
      gl.EnableVertexAttribArray(Ord(vaTexCoord0));
      gl.EnableVertexAttribArray(Ord(vaColor));
			gl.VertexAttribPointer(Ord(vaCoord), 3, GL_FLOAT, False, VF_STRIDE[Format], nil);
			gl.VertexAttribPointer(Ord(vaTexCoord0), 2, GL_FLOAT, False, VF_STRIDE[Format], Pointer(SizeOf(TdfVec3f)));
      gl.VertexAttribPointer(Ord(vaColor), 4, GL_FLOAT, False, VF_STRIDE[Format], Pointer(SizeOf(TdfVec3f) + SizeOf(TdfVec2f)));
    end
    else
      Log.Write(lCritical, 'Unsupported type of vertexbuffer format. Tinyglr developer is an asshole');
  end

end;

class procedure TglrVertexBuffer.Unbind;
begin
  gl.BindBuffer(GL_ARRAY_BUFFER, 0);
end;

constructor TglrVertexBuffer.Create(aData: Pointer; aCount: Integer;
  aFormat: TglrVertexFormat);
begin
  gl.GenBuffers(1, @Self.Id);
  Self.Format := aFormat;
  Self.Count := aCount;
  gl.BindBuffer(GL_ARRAY_BUFFER, Id);
  gl.BufferData(GL_ARRAY_BUFFER, VF_STRIDE[aFormat] * aCount, aData, GL_STATIC_DRAW);
  gl.BindBuffer(GL_ARRAY_BUFFER, 0);
end;

procedure TglrVertexBuffer.Update(aData: Pointer; aStart, aCount: Integer);
begin
  gl.BindBuffer(GL_ARRAY_BUFFER, Id);
  gl.BufferSubData(GL_ARRAY_BUFFER, aStart, aCount * VF_STRIDE[Format], aData);
  gl.BindBuffer(GL_ARRAY_BUFFER, 0);
  Render.fVB := 0;
end;

function TglrVertexBuffer.Map(aAccess: TglrVertexBufferMapAccess): Pointer;
var
  glAccess: TGLConst;
begin
  gl.BindBuffer(GL_ARRAY_BUFFER, Id);
  case aAccess of
    maReadWrite: glAccess := GL_READ_WRITE;
    maRead: glAccess := GL_READ_ONLY;
    maWrite: glAccess := GL_WRITE_ONLY;
  end;
  Result := gl.MapBuffer(GL_ARRAY_BUFFER, glAccess);
  gl.BindBuffer(GL_ARRAY_BUFFER, 0);
end;

procedure TglrVertexBuffer.Unmap;
begin
  gl.BindBuffer(GL_ARRAY_BUFFER, Id);
  gl.UnmapBuffer(GL_ARRAY_BUFFER);
  gl.BindBuffer(GL_ARRAY_BUFFER, 0);
end;

destructor TglrVertexBuffer.Destroy;
begin
  gl.DeleteBuffers(1, @Self.Id);
  inherited Destroy;
end;

{ TglrStream }
class function TglrStream.Init(Memory: Pointer; MemSize: LongInt;
  MemoryOwner: Boolean): TglrStream;
begin
  Result := TglrStream.Create;
  with Result do
  begin
    SType := stMemory;
    Mem   := Memory;
    FSize := MemSize;
    FPos  := 0;
    FBPos := 0;
    fMemoryOwner := MemoryOwner;
  end;
end;

class function TglrStream.Init(const FileName: AnsiString; RW: Boolean
  ): TglrStream;
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
    CloseFile(F)
  else if (SType = stMemory) and fMemoryOwner then
    FreeMem(Mem);
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
    Move((Mem + FPos)^, Buf, Result);
  end else if SType = stFile then
    BlockRead(F, Buf, BufSize, Result);
  Inc(FPos, Result);
end;

function TglrStream.Write(const Buf; BufSize: LongInt): LongInt;
begin
  if SType = stMemory then
  begin
    Result := Min(FPos + BufSize, FSize) - FPos;
    Move(Buf, (Mem + FPos)^, Result);
  end else if SType = stFile then
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

constructor TglrTexture.Create(aData: Pointer; aWidth, aHeight: Integer;
  aFormat: TglrTextureFormat);
var
  iFormat, cFormat, dType: TGLConst;
  anisotropy: Integer;
begin
  case aFormat of
    tfBGR8:
    begin
      iFormat := GL_RGB8;
      cFormat := GL_BGR;
      dType := GL_UNSIGNED_BYTE;
    end;
    tfBGRA8:
    begin
      iFormat := GL_RGBA8;
      cFormat := GL_BGRA;
      dType := GL_UNSIGNED_BYTE;
    end;
    tfRGB8:
    begin
      iFormat := GL_RGB8;
      cFormat := GL_RGB;
      dType := GL_UNSIGNED_BYTE;
    end;
    tfRGBA8:
    begin
      iFormat := GL_RGBA8;
      cFormat := GL_RGBA;
      dType := GL_UNSIGNED_BYTE;
    end;
  end;

  Width := aWidth;
  Height := aHeight;

  gl.GenTextures(1, @Id);
  Target := GL_TEXTURE_2D;
  Log.Write(lInformation, 'Texture (ID = ' + Convert.ToString(Integer(Self.Id)) + ') load started');

  gl.BindTexture(Target, Self.Id);

  gl.TexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, Ord(GL_LINEAR));
  gl.TexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, Ord(GL_LINEAR));
  gl.TexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, Ord(GL_REPEAT));
  gl.TexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, Ord(GL_REPEAT));
  gl.GetIntegerv(GL_MAX_TEXTURE_MAX_ANISOTROPY, @anisotropy);
  gl.TexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAX_ANISOTROPY, anisotropy);

  gl.TexImage2D(GL_TEXTURE_2D, 0, iFormat, Width, Height, 0, cFormat, dType, aData);

  gl.BindTexture(GL_TEXTURE_2D, 0);

  Log.Write(lInformation, 'Texture (ID = ' + Convert.ToString(Integer(Self.Id)) + ') load completed');
end;

constructor TglrTexture.Create(aStream: TglrStream; aExt: AnsiString;
  aFreeStreamOnFinish: Boolean);
var
  data: Pointer;
  iFormat, cFormat, dType: TGLConst;
  pSize: Integer;

  aFormat: TglrTextureFormat;
begin
  data := LoadTexture(aStream, aExt, iFormat, cFormat, dType, pSize, Self.Width, Self.Height);
  case cFormat of
    GL_BGR:  aFormat := tfBGR8;
    GL_BGRA: aFormat := tfBGRA8;
    GL_RGB:  aFormat := tfRGB8;
    GL_RGBA: aFormat := tfRGBA8;
  end;

  Create(data, Width, Height, aFormat);
(*
  gl.GenTextures(1, @Id);
  Target := GL_TEXTURE_2D;

  Log.Write(lInformation, 'Texture (ID = ' + Convert.ToString(Integer(Self.Id)) + ') load started');

  gl.BindTexture(Target, Self.Id);

  gl.TexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, Ord(GL_LINEAR));
  gl.TexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, Ord(GL_LINEAR));
  gl.TexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, Ord(GL_REPEAT));
  gl.TexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, Ord(GL_REPEAT));
  gl.GetIntegerv(GL_MAX_TEXTURE_MAX_ANISOTROPY, @anisotropy);
  gl.TexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAX_ANISOTROPY, anisotropy);

  data := LoadTexture(aStream, aExt, iFormat, cFormat, dType, pSize, Self.Width, Self.Height);
  gl.TexImage2D(GL_TEXTURE_2D, 0, iFormat, Width, Height, 0, cFormat, dType, data);

  gl.BindTexture(GL_TEXTURE_2D, 0);

  Log.Write(lInformation, 'Texture (ID = ' + Convert.ToString(Integer(Self.Id)) + ') load completed');

*)
  Freemem(data, Width * Height * pSize);
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

procedure TglrList<T>.BoundsCheck(Index: LongInt);
begin
  if (Index < 0) or (Index >= FCount) then
    Log.Write(lCritical, 'List index out of bounds (' + Convert.ToString(Index) + ')');
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

destructor TglrList<T>.Destroy;
begin
  SetLength(FItems, 0);
  FCount := 0;
  inherited Destroy;
end;

function TglrList<T>.IndexOf(Item: T): LongInt;
var
  i : LongInt;
begin
  for i := 0 to FCount - 1 do
    if @FItems[i] = @Item then
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

procedure TglrList<T>.DeleteByIndex(Index: LongInt);
begin
  BoundsCheck(Index);
  if Index <> fCount - 1 then
    Move(FItems[Index + 1], FItems[Index], (FCount - Index - 1) * SizeOf(T));
  Dec(FCount);
  if Length(FItems) - FCount + 1 > FCapacity then
    SetLength(FItems, Length(FItems) - FCapacity);
end;

procedure TglrList<T>.Delete(Item: T);
var
  i: Integer;
begin
  i := IndexOf(Item);
  if i <> -1 then
    DeleteByIndex(i)
  else
    Log.Write(lError, 'List: No item found at list, delete is impossible');
end;

procedure TglrList<T>.DeleteSafe(Item: T);
var
  i: Integer;
begin
  i := IndexOf(Item);
  if i <> -1 then
    DeleteSafeByIndex(i)
  else
    Log.Write(lError, 'List: No item found at list, delete is impossible');
end;

procedure TglrList<T>.DeleteSafeByIndex(Index: LongInt);
var
  i: Integer;
begin
  BoundsCheck(Index);
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
