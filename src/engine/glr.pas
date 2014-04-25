unit glr;

interface

uses
  Windows, Graphics, ogl,
  glrMath;

const
  dllName = 'glrenderer.dll';


function PCharToPWide(AChar: PAnsiChar): PWideChar;
function PWideToPChar(pw: PWideChar): PAnsiChar;

//Размер памяти под указателем
function SizeOfP(const P: Pointer): Integer;

type
  TglrOnUpdateProc = procedure(const dt: Double);

  TglrMouseShiftState = set of (ssLeft, ssRight, ssMiddle, ssDouble);
  TglrMouseButton = (mbNone, mbLeft, mbRight, mbMiddle);

  //TODO: А нужнен ли ShiftState для Up?
  TglrOnMouseDownProc   = procedure(X, Y: Integer; MouseButton: TglrMouseButton; Shift: TglrMouseShiftState);
  TglrOnMouseUpProc     = procedure(X, Y: Integer; MouseButton: TglrMouseButton; Shift: TglrMouseShiftState);
  TglrOnMouseMoveProc   = procedure(X, Y: Integer; Shift: TglrMouseShiftState);
  TglrOnMouseWheelProc  = procedure(X, Y: Integer; Shift: TglrMouseShiftState; WheelDelta: Integer);

  {$REGION ' Utility types '}

  TglrStream = class
    class function Init(Memory: Pointer; MemSize: LongInt): TglrStream; overload;
    class function Init(const FileName: string; RW: Boolean = False): TglrStream; overload;
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

  {$ENDREGION

  {$REGION ' Input '}

const
  VK_0 = $30;
  VK_1 = $31;
  VK_2 = $32;
  VK_3 = $33;
  VK_4 = $34;
  VK_5 = $35;
  VK_6 = $36;
  VK_7 = $37;
  VK_8 = $38;
  VK_9 = $39;
  VK_A = $41;
  VK_B = $42;
  VK_C = $43;
  VK_D = $44;
  VK_E = $45;
  VK_F = $46;
  VK_G = $47;
  VK_H = $48;
  VK_I = $49;
  VK_J = $4A;
  VK_K = $4B;
  VK_L = $4C;
  VK_M = $4D;
  VK_N = $4E;
  VK_O = $4F;
  VK_P = $50;
  VK_Q = $51;
  VK_R = $52;
  VK_S = $53;
  VK_T = $54;
  VK_U = $55;
  VK_V = $56;
  VK_W = $57;
  VK_X = $58;
  VK_Y = $59;
  VK_Z = $5A;
  VK_MOUSEWHEELUP   = VK_F23;
  VK_MOUSEWHEELDOWN = VK_F24;

type
  IglrInput = interface
    ['{5552ED21-B3E8-4F3D-9551-AD7A9EF82CF4}']
    {$REGION '[private]'}
    function GetAllow(): Boolean;
    procedure SetAllow(aAllow: Boolean);
    {$ENDREGION}
    function IsKeyDown(const vk: Integer): Boolean; overload;
    function IsKeyDown(const c: Char): Boolean; overload;

    function IsKeyPressed(aCode: Integer; aPressed: PBoolean): Boolean; overload; deprecated;
    function IsKeyPressed(aChar: Char; aPressed: PBoolean): Boolean; overload; deprecated;
    function IsKeyPressed(aCode: Integer): Boolean; overload;
    function IsKeyPressed(aChar: Char): Boolean; overload;

    procedure KeyboardNotifyWheelMoved(wheelDelta : Integer);
    //Разрешить захват клавиш.
    //Автоматически меняется в зависимости от того, активно окно или нет
    property AllowKeyCapture: Boolean read GetAllow write SetAllow;
  end;
  {$ENDREGION}

  {$REGION ' Resource manager '}

  TglrResType = type of Byte;

const
  RES_UNKNOWN      : TglrResType = $FF;
  RES_TEXTURE      : TglrResType = $01;
  RES_TEXTURE_ATLAS: TglrResType = $02;

type
  { IdfResource - ресурс(изображение, звук, текстовый файл, бинарный файл) }
  IglrResource = interface
    ['{A95929A4-C8B6-4EE3-844F-E5C9B5E1249A}']
    {$REGION '[private]'}
    function GetStream(): TglrStream;
    procedure SetStream(aStream: TglrStream);
    function GetResType(): TglrResType;
    procedure SetResType(aResType: TglrResType);
    function GetExtData(): Pointer;
    procedure SetExtData(aData: Pointer);
    function GetName(): String;
    procedure SetName(aName: String);
    {$ENDREGION}
    property Name: String read GetName write SetName;
    property Stream: TglrStream read GetStream write SetStream;
    property ResType: TglrResType read GetResType write SetResType;
    property ExtData: Pointer read GetExtData write SetExtData;
  end;

  { IdfResourceManager - менеджер по загрузке и использованию ресурсов }
  IglrResourceManager = interface
    ['{BF733D21-0F1B-4907-98B0-F03F2B0FFCCB}']
    {$REGION '[private]'}
    function GetResource(aIndex: String): IglrResource;
    procedure SetResource(aIndex: String; const aRes: IglrResource);
    {$ENDREGION}
    function AddResource(): IglrResource;
    function LoadResourceFromFile(aFileName: String; aResName: String = ''): IglrResource;
    function LoadResourceFromPack(aPackName, aFileName: String; aResName: String = ''): IglrResource;

    property Resource[Index: String]: IglrResource read GetResource write SetResource;
  end;

  {$ENDREGION}

  {$REGION ' Texture, shaders and material '}

  //Вид текстуры
  TglrTextureTarget = (ttTexture1D, ttTexture2D, ttTexture3D{, ttTextureRectangle,
                ttTextureRectangleNV,
                ttCubemap, ttCubemapPX, ttCubemapPY, ttCubemapNX, ttCubemapNY,
                ttCubemapPZ, ttCubemapNZ, tt1DArray, tt2DArray, ttCubeMapArray});
  //Режим враппинга (повторения и рамок)
  TglrTextureWrap = (twClamp, twRepeat, twClampToEdge, twClampToBorder, twMirrorRepeat);
//  TdfTexGens = (tgDisable,tgObjectLinear,tgEyeLinear,tgSphereMap,tgNormalMap,tgReflectionMap);
  //маг и мин фильтры
  TglrTextureMagFilter = (tmgNearest, tmgLinear);
  TglrTextureMinFilter = (tmnNearest, tmnLinear, tmnNearestMipmapNearest, tmnNearestMipmapLinear,
                tmnLinearMipmapNearest, tmnLinearMipmapLinear);
  //Режимы прозрачности
  TglrTextureBlendingMode = (tbmOpaque, tbmTransparency, tbmAdditive, tbmAlphaTest50,
                    tbmAlphaTest100, tbmModulate, tbmCustom1);
  //Режимы смешивания с цветом
  TglrTextureCombineMode = (tcmDecal, tcmModulate, tcmBlend, tcmReplace, tcmAdd);

  TglrTextureDecription = record
     InternalFormat: TGLConst; //число компонентов
     ColorFormat: TGLConst; //GL_BGR, GL_RGB, GL_RGBA....
     DataType: TGLConst;
     WrapS, WrapT, WrapR: TGLConst;
     Target: TGLConst;
     minFilter: TGLConst;
     magFilter: TGLConst;
//     Data: pointer;
     Id: LongInt;
     FullSize: Integer;
     X, Y, Width, Height, Depth, RegionWidth, RegionHeight: Integer;
  end;
  PglrTextureDecription = ^TglrTextureDecription;

  IglrTexture = interface
    ['{3D75E1EB-E4C8-4856-BA55-B98020407605}']
    {$REGION '[private]'}
    function GetWidth(): Integer;
    function GetHeight(): Integer;

    function GetTexTarget(): TglrTextureTarget;
    function GetTexWrapS(): TglrTextureWrap;
    function GetTexWrapT(): TglrTextureWrap;
    function GetTexWrapR(): TglrTextureWrap;
    function GetTexMinFilter(): TglrTextureMinFilter;
    function GetTexMagFilter(): TglrTextureMagFilter;
    function GetTexBlendingMode(): TglrTextureBlendingMode;
    function GetTexCombineMode(): TglrTextureCombineMode;

    procedure SetTexWrapS(aWrap: TglrTextureWrap);
    procedure SetTexWrapT(aWrap: TglrTextureWrap);
    procedure SetTexWrapR(aWrap: TglrTextureWrap);
    procedure SetTexMinFilter(aFilter: TglrTextureMinFilter);
    procedure SetTexMagFilter(aFilter: TglrTextureMagFilter);
    procedure SetTexBlendingMode(aMode: TglrTextureBlendingMode);
    procedure SetTexCombineMode(aMode: TglrTextureCombineMode);
    {$ENDREGION}
    procedure Bind;
    procedure Unbind;

    {debug procedure
     Переделать на загрузку из Stream через ResourceManager}
    procedure Load2D(const aFileName: String); overload;
    procedure Load2D(const aStream: TglrStream; aFormatExtension: String); overload;
    {Прообраз загрузки из атласа}
    procedure Load2DRegion(const aTex: IglrTexture; aX, aY, aWidth, aHeight: Integer);

    function GetTexDesc(): TglrTextureDecription;

    property Target: TglrTextureTarget read GetTexTarget;
    property WrapS: TglrTextureWrap read GetTexWrapS write SetTexWrapS;
    property WrapT: TglrTextureWrap read GetTexWrapT write SetTexWrapT;
    property WrapR: TglrTextureWrap read GetTexWrapR write SetTexWrapR;
    property MinFilter: TglrTextureMinFilter read GetTexMinFilter write SetTexMinFilter;
    property MagFilter: TglrTextureMagFilter read GetTexMagFilter write SetTexMagFilter;
    property BlendingMode: TglrTextureBlendingMode read GetTexBlendingMode write SetTexBlendingMode;
    property CombineMode: TglrTextureCombineMode read GetTexCombineMode write SetTexCombineMode;

    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
  end;

  (*

  TdfTextureAtlasInfo = record
    aTexCoords: TdfVec4f;
    aTexture: IdfTexture;
  end;

  IdfTextureAtlas = interface
    ['{36F427A0-A11F-4B80-B40B-F7904C1179BE}']
    {$REGION '[private]'}
    function GetTex(aIndex: String): TdfTextureAtlasInfo;
    procedure SetTex(aIndex: String; aTex: TdfTextureAtlasInfo);
    {$ENDREGION}

    procedure LoadFromFile(aFileName: String);
    procedure SaveToFile(aFileName: String);

    procedure LoadFromStream(const aStream: TdfStream);
    procedure SaveToStream(var aStream: TdfStream);

    function AddTexture(aTexture: IdfTexture): TdfTextureAtlasInfo;

    property Texture[Index: String]: TdfTextureAtlasInfo read GetTex write SetTex;
  end;

    *)

  (*
  IdfShader = interface
    ['{5C020C83-273C-4351-A41E-3AE8D12C8A90}']
  end;

  *)

  IglrShaderProgram = interface
    ['{B31B84F3-D71D-4117-B5D7-3BEAD6E5D5E2}']
    procedure Use;
    procedure Unuse;
  end;

  IglrMaterial = interface
    ['{DE277592-0C48-4DA0-971F-780470FCCA04}']
    {$REGION '[private]'}
    function GetTexture: IglrTexture;
    procedure SetTexture(const aTexture: IglrTexture);
    function GetShader(): IglrShaderProgram;
    procedure SetShader(const aShader: IglrShaderProgram);
    function GetDif(): TdfVec4f;
    procedure SetDif(const aDif: TdfVec4f);
    function GetPDif(): PdfVec4f;
    procedure SetPDif(const aDif: PdfVec4f);
    {$ENDREGION}

    property Texture: IglrTexture read GetTexture write SetTexture;
    property ShaderProgram: IglrShaderProgram read GetShader write SetShader;


    property Diffuse: TdfVec4f read GetDif write SetDif;
    property PDiffuse: PdfVec4f read GetPDif write SetPDif;

    procedure Apply();
    procedure Unapply();
  end;

  {$ENDREGION}

  {$REGION ' RenderNodes and scenes '}


  { IdfNode - рендер-узел, обладает структурой Родитель-Дети, имеет матрицу,
    позиционирующую его в пространстве}
  IglrNode = interface
    ['{3D31C699-4B5F-4FC3-8F08-2E91BA918135}']
    {$REGION '[private]'}
    function GetPos(): TdfVec3f;
    procedure SetPos(const aPos: TdfVec3f);
    function GetPPos(): PdfVec3f;
    procedure SetPPos(const aPos: PdfVec3f);
    function GetUp(): TdfVec3f;
    procedure SetUp(const aUp: TdfVec3f);
    function GetDir(): TdfVec3f;
    procedure SetDir(const aDir: TdfVec3f);
    function GetRight(): TdfVec3f;
    procedure SetRight(const aRight: TdfVec3f);
    function GetModel(): TdfMat4f;
    procedure SetModel(const aModel: TdfMat4f);
    function GetVis(): Boolean;
    procedure SetVis(const aVis: Boolean);
    function GetChild(Index: Integer): IglrNode;
    procedure SetChild(Index: Integer; aChild: IglrNode);
    function GetParent(): IglrNode;
    procedure SetParent(aParent: IglrNode);
    function GetChildsCount(): Integer;

    function GetAbsMatrix(): TdfMat4f;
    procedure SetAbsMatrix(const aMat: TdfMat4f);
    {$ENDREGION}

    property Position: TdfVec3f read GetPos write SetPos;
    property PPosition: PdfVec3f read GetPPos write SetPPos;
    property Up: TdfVec3f read GetUp write SetUp;
    property Direction: TdfVec3f read GetDir write SetDir;
    property Right: TdfVec3f read GetRight write SetRight;
    property ModelMatrix: TdfMat4f read GetModel write SetModel;
    property Parent: IglrNode read GetParent write SetParent;
    property Visible: Boolean read GetVis write SetVis;
    property AbsoluteMatrix: TdfMat4f read GetAbsMatrix write SetAbsMatrix;

    property Childs[Index: Integer]: IglrNode read GetChild write SetChild;
    property ChildsCount: Integer read GetChildsCount;

    function AddChild(aChild: IglrNode): Integer;
    procedure RemoveChild(Index: Integer); overload;
    procedure RemoveChild(aChild: IglrNode); overload;
    procedure RemoveAllChilds();

    procedure Render();
  end;

  { IdfRenderable - базовый класс чего-то, способного отобразиться на экране.
    Имеется материал и метод рендера, который переопределяется в потомках
    данного класса }
  IglrRenderable = interface (IglrNode)
    ['{A2DD3046-3FDE-43DD-93AE-83C7A29A2196}']
    {$REGION '[private]'}
    function GetMaterial(): IglrMaterial;
    procedure SetMaterial(const aMat: IglrMaterial);

    {$ENDREGION}
    procedure DoRender;

    property Material: IglrMaterial read GetMaterial write SetMaterial;
  end;


  IglrBaseScene = interface
    ['{5285C5A6-11A1-4F53-8327-71CBBD20E010}']
    {$REGION '[private]'}
    function GetRoot: IglrNode;
    procedure SetRoot(aRoot: IglrNode);
    function GetUpdateProc(): TglrOnUpdateProc;
    procedure SetUpdateProc(aProc: TglrOnUpdateProc);
    {$ENDREGION}
    property RootNode: IglrNode read GetRoot write SetRoot;
    property OnUpdate: TglrOnUpdateProc read GetUpdateProc write SetUpdateProc;
    procedure Render();
    procedure Update(const deltaTime: Double);
  end;

{ Idf2DScene - класс, организующий все Idf2DRenderable-сущности}
  Iglr2DScene = interface (IglrBaseScene)
    ['{3D0DB66F-077A-406B-88A4-882972D8077A}']
    {$REGION '[private]'}
    function GetNear(): Single;
    function GetFar(): Single;
    function GetCamInd(): Boolean;
    procedure SetNear(const aNear: Single);
    procedure SetFar(const aFar: Single);
    procedure SetCamInd(const aValue: Boolean);
    {$ENDREGION}
    procedure SortFarthestFirst();
    property ZNear: Single read GetNear write SetNear;
    property ZFar: Single read GetFar write SetFar;

    property IsCameraIndependent: Boolean read GetCamInd write SetCamInd;
  end;

  { Iglr3DScene - идентифицирует игровую сцену, иерархию рендер-узлов с привязанными
    к ним графическими объектами }
  Iglr3DScene = interface(IglrBaseScene)
    ['{5E52434E-3A00-478E-AE73-BA45C77BD2AC}']

  end;

  {$ENDREGION}

  TglrViewportParams = record
    X,Y,W,H: Integer;
    FOV, ZNear, ZFar: Single;
  end;

  TglrCameraProjectionMode = (pmPerpective, pmOrtho);

  { IdfCamera - идентифицирует камеру с возможностями установки вьюпорта,
    панорамирования, масштабирования и прочим }
  IglrCamera = interface (IglrNode)
    ['{D6E97126-FF5F-4CE7-9687-4F358A90B34E}']
    {$REGION '[private]'}
    function GetProjMode(): TglrCameraProjectionMode;
    procedure SetProjMode(aMode: TglrCameraProjectionMode);
    {$ENDREGION}
    procedure Viewport(x, y, w, h: Integer; FOV, ZNear, ZFar: Single);
    procedure ViewportOnly(x, y, w, h: Integer);
    procedure Translate(alongUpVector, alongRightVector: Single);
    procedure Scale(aScale: Single);
    procedure Rotate(delta: Single; Axis: TdfVec3f);
    procedure SetCamera(Pos, TargetPos, Up: TdfVec3f);
//    procedure SetTarget(Point: TdfVec3f); overload;
//    procedure SetTarget(Target: IglrNode); overload;

    function GetViewport(): TglrViewportParams;

    property ProjectionMode: TglrCameraProjectionMode read GetProjMode write SetProjMode;

    procedure Update();
  end;

  { IdfLight - источник света }
  IglrLight = interface (IglrNode)
    ['{2F9B9229-7A8D-4517-9E5D-DB135E1A6929}']
    {$REGION '[private]'}
    function GetAmb(): TdfVec4f;
    procedure SetAmb(const aAmb: TdfVec4f);
    function GetDif(): TdfVec4f;
    procedure SetDif(const aDif: TdfVec4f);
    function GetSpec(): TdfVec4f;
    procedure SetSpec(const aSpec: TdfVec4f);
    function GetConstAtten(): Single;
    procedure SetConstAtten(const aAtten: Single);
    function GetLinAtten(): Single;
    procedure SetLinAtten(const aAtten: Single);
    function GetQuadroAtten(): Single;
    procedure SetQuadroAtten(const aAtten: Single);
    function GetDR(): Boolean;
    procedure SetDR(aDR: Boolean);
    {$ENDREGION}

    property Ambient: TdfVec4f read GetAmb write SetAmb;
    property Diffuse: TdfVec4f read GetDif write SetDif;
    property Specular: TdfVec4f read GetSpec write SetSpec;

    property ConstAtten: Single read GetConstAtten write SetConstAtten;
    property LinearAtten: Single read GetLinAtten write SetLinAtten;
    property QuadraticAtten: Single read GetQuadroAtten write SetQuadroAtten;

    property DebugRender: Boolean read GetDR write SetDR;
  end;

  TglrParticleDynamics = set of (pdColor, pdSize, pdVelocity, pdSpread);

  IglrEmitter = interface
    ['{31622C02-C603-4AF8-9F72-9A2614E34DE5}']
    {$REGION '[private]'}
    function GetTexture(): IglrTexture;
    procedure SetTexture(aTex: IglrTexture);
    {$ENDREGION}
    property Texture: IglrTexture read GetTexture write SetTexture;
    procedure SetColorBorders(aStart, aEnd: TdfVec4f);
    procedure SetSizeBorders(aMin, aMax: Single);
    procedure SetVelocityBorders(aMin, aMax: Single);
  end;

  IglrParticleSystem = interface
    ['{750E2517-93F9-498B-B760-4F4BE3047CBD}']
  end;

  IglrMesh = interface (IglrRenderable)
    ['{90223F0B-7F8F-4EBF-9752-DF84CE75B7E7}']

  end;


  {$REGION ' 2D-рендер '}

  {Точка отсчета для рендера 2Д вещей}
  Tglr2DPivotPoint = (ppTopLeft, ppTopRight, ppBottomLeft, ppBottomRight,
    ppCenter, ppTopCenter, ppBottomCenter, ppCustom);

  {Отличительные особенности - не использует матрицу Node, а собственные свойства}
  Iglr2DRenderable = interface(IglrRenderable)
    ['{EC48E06A-778E-45B7-A239-3DE1897A7C06}']
    {$REGION '[private]'}
    function GetWidth(): Single;
    procedure SetWidth(const aWidth: Single);
    function GetHeight(): Single;
    procedure SetHeight(const aHeight: Single);
    function GetPos2D(): TdfVec2f;
    procedure SetPos2D(const aPos: TdfVec2f);
    function GetScale(): TdfVec2f;
    procedure SetScale(const aScale: TdfVec2f);
    function GetRot(): Single;
    procedure SetRot(const aRot: Single);
    function GetPRot(): System.PSingle;
    procedure SetPRot(const aRot: System.PSingle);
    function GetPivot(): Tglr2DPivotPoint;
    procedure SetPivot(const aPivot: Tglr2DPivotPoint);
    function GetCoord(aIndex: Integer): TdfVec2f;
    procedure SetCoord(aIndex: Integer; aCoord: TdfVec2f);
    function GetTexCoord(aIndex: Integer): TdfVec2f;
    procedure SetTexCoord(aIndex: Integer; aCoord: TdfVec2f);
//    function GetAbsPosition(): Boolean;
//    procedure SetAbsPosition(const Value: Boolean);

    function GetBB: TdfBB;

//    function GetParentScene(): Iglr2DScene;
//    procedure SetParentScene(const aScene: Iglr2DScene);
    {$ENDREGION}
    property Position2D: TdfVec2f read GetPos2D write SetPos2D;
    property Scale: TdfVec2f read GetScale write SetScale;
    procedure ScaleMult(const aScale: TdfVec2f); overload;
    procedure ScaleMult(const aScale: Single); overload;
    property Rotation: Single read GetRot write SetRot;
    property PRotation: System.PSingle read GetPRot write SetPRot;
    property PivotPoint: Tglr2DPivotPoint read GetPivot write SetPivot;
    //Задаем собственную точку, в координатах 0..1, отсчет от верхнего левого угла
    procedure SetCustomPivotPoint(pX, pY: Single);

    property Width: Single read GetWidth write SetWidth;
    property Height: Single read GetHeight write SetHeight;

    property Coords[Index: Integer]: TdfVec2f read GetCoord write SetCoord;
    property TexCoords[Index: Integer]: TdfVec2f read GetTexCoord write SetTexCoord;

    //Debug. Необходимо вызывать, когда поменялась/загрузилась текстура
    procedure UpdateTexCoords();

    //True - объект располагается независимо от вхождения в Scene2D или RenderNode
    //Рекомендуется использовать True при использовании RenderNode.Renderable
    //и False при Scene2D-рендере (устанавливается автоматически)
//    property AbsolutePosition: Boolean read GetAbsPosition write SetAbsPosition;

    procedure SetSizeToTextureSize();

    property BoundingBox: TdfBB read GetBB;
  end;

  { IdfSprite - двумерный спрайт, отображающийся на экране (HUD-sprite) без искажений }
  IglrSprite = interface (Iglr2DRenderable)
    ['{C8048F34-9F3D-4E58-BC71-633F2413A9A5}']
  end;

  IglrText = interface;

  { IdfFont отвечает за хранение шрифта, которым может быть отрендерен текст }
  IglrFont = interface
    ['{C05DAC6F-ABC0-41BF-9752-6064395741D2}']
    {$REGION '[private]'}
    function GetTexture(): IglrTexture;
//    procedure SetTexture(aTexture: IdfTexture);
    function GetFontSize(): Integer;
    procedure SetFontSize(aSize: Integer);
    function GetFontStyle(): TFontStyles;
    procedure SetFontStyle(aStyle: TFontStyles);
    {$ENDREGION}
    procedure AddRange(aStart, aStop: Word); overload;
    procedure AddRange(aStart, aStop: WideChar); overload;
    procedure AddSymbols(aText: WideString);

    property FontSize: Integer read GetFontSize write SetFontSize;
    property FontStyle: TFontStyles read GetFontStyle write SetFontStyle;

    procedure GenerateFromTTF(aFile: WideString; aFontName: WideString = '');
    procedure GenerateFromFont(aFontName: WideString);
    property Texture: IglrTexture read GetTexture;

    procedure PrintText(aText: IglrText); //overload;

    function GetTextLength(aText: WideString): Single;
    function GetTextSize(aText: IglrText): TdfVec2f;
    function IsSymbolExist(aSymbol: WideChar): Boolean;
  end;

  { IdfText - текст, отображающийся на экране без искажений и вне зависимости
    от положения камеры (HUD-элемент)}
  IglrText = interface (Iglr2DRenderable)
    ['{C0E53D75-7C6B-4218-AA3E-B6FE6076EA68}']
    {$REGION '[private]'}
    function GetFont(): IglrFont;
    procedure SetFont(aFont: IglrFont);
    function GetText(): WideString;
    procedure SetText(aText: WideString);

//    function GetWidth(): Single;
//    procedure SetWidth(const aWidth: Single);
//    function GetHeight(): Single;
//    procedure SetHeight(const aHeight: Single);
    {$ENDREGION}

    property Font: IglrFont read GetFont write SetFont;
    property Text: WideString read GetText write SetText;
  end;

  {$ENDREGION}

  {$REGION ' GUI '}

  IglrGUIElement = interface;

  //Виды проверок:
  // - hmBox - проверка по всей площади кнопки
  // - hmCircle - проверка по радиусу, равному Width
  // - hmAlpha0 - за кнопку считается все, у чего альфа больше 0
  // - hmAlpha50 - за кнопку считается все, у чего альфа больше 50%
  TglrGUIHitMode = (hmBox, hmCircle, hmAlpha0, hmAlpha50);

  TglrMousePos = (mpOut = 0, mpOver);

  TglrMouseEvent = procedure(Sender: IglrGUIElement; X, Y: Integer; Button: TglrMouseButton; Shift: TglrMouseShiftState);
  TglrWheelEvent = procedure(Sender: IglrGUIElement; X, Y: Integer; Shift: TglrMouseShiftState; WheelDelta: Integer);
  TglrFocusEvent = procedure(Sender: IglrGUIElement; IsFocused: Boolean);
  TglrValueChangedEvent = procedure(Sender: IglrGUIElement; aNewValue: Integer);

  {
    ОБщий предок для всех элементов GUI
  }
  IglrGUIElement = interface(Iglr2DRenderable)
    ['{68635C44-C704-438B-8D98-C741C325F3CA}']
    {$REGION '[private]'}
    function GetEnabled(): Boolean;
    procedure SetEnabled(const aEnabled: Boolean);

    function GetHitMode(): TglrGUIHitMode;
    procedure SetHitMode(aMode: TglrGUIHitMode);

    function GetOnClick(): TglrMouseEvent;
    function GetOnOver(): TglrMouseEvent;
    function GetOnOut(): TglrMouseEvent;
    function GetOnDown(): TglrMouseEvent;
    function GetOnUp(): TglrMouseEvent;
    function GetOnWheel(): TglrWheelEvent;
    function GetOnFocus(): TglrFocusEvent;

    procedure SetOnClick(aProc: TglrMouseEvent);
    procedure SetOnOver(aProc: TglrMouseEvent);
    procedure SetOnOut(aProc: TglrMouseEvent);
    procedure SetOnDown(aProc: TglrMouseEvent);
    procedure SetOnUp(aProc: TglrMouseEvent);
    procedure SetOnWheel(aProc: TglrWheelEvent);
    procedure SetOnFocus(aProc: TglrFocusEvent);

    function GetMousePos(): TglrMousePos;

    //Для внутреннего использования. Либо для принудительного вызова события
    procedure _MouseMove (X, Y: Integer; Shift: TglrMouseShiftState);
    procedure _MouseOver (X, Y: Integer; Shift: TglrMouseShiftState);
    procedure _MouseOut (X, Y: Integer; Shift: TglrMouseShiftState);
    procedure _MouseDown (X, Y: Integer; MouseButton: TglrMouseButton; Shift: TglrMouseShiftState);
    procedure _MouseUp   (X, Y: Integer; MouseButton: TglrMouseButton; Shift: TglrMouseShiftState);
    procedure _MouseWheel(X, Y: Integer; Shift: TglrMouseShiftState; WheelDelta: Integer);
    procedure _MouseClick(X, Y: Integer; MouseButton: TglrMouseButton; Shift: TglrMouseShiftState);
    procedure _Focused();
    procedure _Unfocused();
    procedure _KeyDown(KeyCode: Word; KeyData: Integer);
    {$ENDREGION}

    property Enabled: Boolean read GetEnabled write SetEnabled;
    //Режим проверки попадания по элементу.
    property HitMode: TglrGUIHitMode read GetHitMode write SetHitMode;
    //Проверка на попадание по элементу
    function CheckHit(X, Y: Integer): Boolean;
    //Коллбэки для пользователя
    property OnMouseClick: TglrMouseEvent read GetOnClick write SetOnClick;
    property OnMouseOver: TglrMouseEvent read GetOnOver write SetOnOver;
    property OnMouseOut: TglrMouseEvent read GetOnOut write SetOnOut;
    property OnMouseDown: TglrMouseEvent read GetOnDown write SetOnDown;
    property OnMouseUp: TglrMouseEvent read GetOnUp write SetOnUp;
    property OnMouseWheel: TglrWheelEvent read GetOnWheel write SetOnWheel;
    property OnFocus: TglrFocusEvent read GetOnFocus write SetOnFocus;

    property MousePos: TglrMousePos read GetMousePos;

    //Порядок сортировки при обработке ввода.
    // При конфликте двух GUI-элементов обработка
    // перейдет к тому, чей ZIndex МЕНЬШЕ

    //Возвращает элемент к некоторму первоначальному состоянию
    procedure Reset();
  end;

  IglrGUIButton = interface(IglrGUIElement)
    ['{D8A90E06-F07C-48B4-9F9A-8E7C31BDFA1F}']
    {$REGION '[private]'}
    function GetTextureNormal(): IglrTexture;
    function GetTextureOver(): IglrTexture;
    function GetTextureClick(): IglrTexture;

    procedure SetTextureNormal(aTexture: IglrTexture);
    procedure SetTextureOver(aTexture: IglrTexture);
    procedure SetTextureClick(aTexture: IglrTexture);

    function GetAutoChange: Boolean;
    procedure SetAutoChange(aChange: Boolean);

    {$ENDREGION}
    property TextureNormal: IglrTexture read GetTextureNormal write SetTextureNormal;
    property TextureOver: IglrTexture read GetTextureOver write SetTextureOver;
    property TextureClick: IglrTexture read GetTextureClick write SetTextureClick;

    //Текстуры будут меняться автоматически при наведении, клие и уходе мыши
    property TextureAutoChange: Boolean read GetAutoChange write SetAutoChange;
  end;

  IglrGUITextButton = interface (IglrGUIButton)
    ['{4B86B915-17A1-4D5C-BDBB-063DCE087F6C}']
    {$REGION '[private]'}
    function GetText(): IglrText;
    procedure SetText(const aText: IglrText);
    function GetTextOffset(): TdfVec2f;
    procedure SetTextOffset(aOffset: TdfVec2f);
    {$ENDREGION}
    property TextObject: IglrText read GetText write SetText;
    property TextOffset: TdfVec2f read GetTextOffset write SetTextOffset;
  end;

  IglrGUICheckBox = interface;

  TglrCheckEvent = procedure (Sender: IglrGUICheckBox; Checked: Boolean);

  IglrGUICheckBox = interface (IglrGUIElement)
    ['{ABBBF404-EBD6-4E89-869C-DA25AD5D1E17}']
    {$REGION '[private]'}
    function GetChecked: Boolean;
    procedure SetChecked(const aChecked: Boolean);

    function GetTextureOn(): IglrTexture;
    function GetTextureOnOver(): IglrTexture;
    function GetTextureOff(): IglrTexture;
    function GetTextureOffOver(): IglrTexture;

    procedure SetTextureOn(aTexture: IglrTexture);
    procedure SetTextureOnOver(aTexture: IglrTexture);
    procedure SetTextureOff(aTexture: IglrTexture);
    procedure SetTextureOffOver(aTexture: IglrTexture);

    function GetAutoChange: Boolean;
    procedure SetAutoChange(aChange: Boolean);

    function GetOnCheck: TglrCheckEvent;
    procedure SetOnCheck(const aOnCheck: TglrCheckEvent);
    {$ENDREGION}
    property Checked: Boolean read GetChecked write SetChecked;

    property TextureOn:      IglrTexture read GetTextureOn      write SetTextureOn;
    property TextureOnOver:  IglrTexture read GetTextureOnOver  write SetTextureOnOver;
    property TextureOff:     IglrTexture read GetTextureOff     write SetTextureOff;
    property TextureOffOver: IglrTexture read GetTextureOffOver write SetTextureOffOver;

    //Текстуры будут меняться автоматически при наведении, клие и уходе мыши
    property TextureAutoChange: Boolean read GetAutoChange write SetAutoChange;
    //Событие при смене статуса checked
    property OnCheck: TglrCheckEvent read GetOnCheck write SetOnCheck;
  end;

  IglrGUITextBox = interface (IglrGUIElement)
    ['{E646CA2F-3A0D-484E-9CD7-53E978F8F98C}']
    {$REGION '[private]'}
    function GetTextObject(): IglrText;
    procedure SetTextObject(const aTextObject: IglrText);
    function GetMaxTextLength(): Integer;
    procedure SetMaxTextLength(aLength: Integer);
    function GetTextOffset(): TdfVec2f;
    procedure SetTextOffset(aOffset: TdfVec2f);
    function GetCurOffset(): TdfVec2f;
    procedure SetCurOffset(aOffset: TdfVec2f);
    function GetCursor: IglrSprite;
    procedure SetCursor(const aCursor: IglrSprite);
    {$ENDREGION}
    property TextObject: IglrText read GetTextObject write SetTextObject;
    property CursorObject: IglrSprite read GetCursor write SetCursor;
    property TextOffset: TdfVec2f read GetTextOffset write SetTextOffset;
    property CursorOffset: TdfVec2f read GetCurOffset write SetCurOffset;
    property MaxTextLength: Integer read GetMaxTextLength write SetMaxTextLength;
  end;

  IglrGUISlider = interface (IglrGUIElement)
    ['{7922F9A6-82D0-4A64-AED6-806CE1ED72FD}']
    {$REGION '[private]'}
    function GetMaxValue: Integer;
    function GetMinValue: Integer;
    function GetSliderBtn: IglrSprite;
    function GetSliderOver: IglrSprite;
    function GetValue: Integer;
    function GetOnValueChanged(): TglrValueChangedEvent;
    procedure SetMaxValue(const Value: Integer);
    procedure SetMinValue(const Value: Integer);
    procedure SetSliderBtn(const Value: IglrSprite);
    procedure SetSliderOver(const Value: IglrSprite);
    procedure SetValue(const Value: Integer);
    procedure SetOnValueChanged(const aOnValueChanged: TglrValueChangedEvent);
    {$ENDREGION}
    property Value:    Integer read GetValue    write SetValue;
    property MinValue: Integer read GetMinValue write SetMinValue;
    property MaxValue: Integer read GetMaxValue write SetMaxValue;
    property SliderButton: IglrSprite read GetSliderBtn  write SetSliderBtn;
    property SliderOver:   IglrSprite read GetSliderOver write SetSliderOver;

    property OnValueChanged: TglrValueChangedEvent read GetOnValueChanged write SetOnValueChanged;
  end;

  IglrGUIManager = interface
    ['{E29C453A-E98E-4881-A444-397AEC9007A8}']
    {$REGION '[private]'}
    function GetFocused(): IglrGUIElement;
    procedure SetFocused(aElement: IglrGUIElement);
    {$ENDREGION}
    //Зарегистрировать/разрегистрировать элемент
    procedure RegisterElement(aElement: IglrGUIElement);
    procedure UnregisterElement(aElement: IglrGUIElement);

    //Элемент, находящийся в фокусе
    property Focused: IglrGUIElement read GetFocused write SetFocused;

    //для внутреннего использования IdfRenderer-ом.
    procedure MouseMove (X, Y: Integer; Shift: TglrMouseShiftState);
    procedure MouseDown (X, Y: Integer; MouseButton: TglrMouseButton; Shift: TglrMouseShiftState);
    procedure MouseUp   (X, Y: Integer; MouseButton: TglrMouseButton; Shift: TglrMouseShiftState);
    procedure MouseWheel(X, Y: Integer; Shift: TglrMouseShiftState; WheelDelta: Integer);
    procedure KeyDown   (KeyCode: Word; KeyData: Integer);
  end;

  {$ENDREGION}

  TglrUserRenderableCallback = procedure(); stdcall;

  IglrUserRenderable = interface(IglrRenderable)
    ['{1315E4FF-F4EF-4049-A4FD-18FEE4FA0A8E}']
    {$REGION '[private]'}
    function GetUserCallback: TglrUserRenderableCallback;
    procedure SetUserCallback(urc: TglrUserRenderableCallback);
    {$ENDREGION}
    property OnRender: TglrUserRenderableCallback read GetUserCallback write SetUserCallback;
  end;

  IglrRenderer = interface
    ['{BFB518E7-A55A-48E2-B0C4-ED7BE8D23796}']
    {$REGION '[private]'}
    function GetWindowHandle(): Integer;
    function GetWindowCaption(): WideString;
    procedure SetWindowCaption(aCaption: WideString);
    function GetRenderReady(): Boolean;
    function GetFPS(): Single;
    function GetCamera(): IglrCamera;
    procedure SetCamera(const aCamera: IglrCamera);
    function GetRootNode: IglrNode;
    procedure SetRootNode(const aRoot: IglrNode);

    procedure SetOnMouseDown(aProc: TglrOnMouseDownProc);
    procedure SetOnMouseUp(aProc: TglrOnMouseUpProc);
    procedure SetOnMouseMove(aProc: TglrOnMouseMoveProc);
    procedure SetOnMouseWheel(aProc: TglrOnMouseWheelProc);

    function GetOnMouseDown(): TglrOnMouseDownProc;
    function GetOnMouseUp(): TglrOnMouseUpProc;
    function GetOnMouseMove(): TglrOnMouseMoveProc;
    function GetOnMouseWheel() : TglrOnMouseWheelProc;

    function GetOnUpdate(): TglrOnUpdateProc;
    procedure SetOnUpdate(aProc: TglrOnUpdateProc);

    function GetEnabled(): Boolean;
    procedure SetEnabled(aEnabled: Boolean);

    function GetSelfVersion(): PWideChar;

    function GetDC(): hDC;
    function GetRC(): hglRC;

    function GetWidth(): Integer;
    function GetHeight(): Integer;

    function GetInput(): IglrInput;
    procedure SetInput(const aInput: IglrInput);

    function GetManager(): IglrGUIManager;
    procedure SetManager(const aManager: IglrGUIManager);

    procedure Dispatch(var Message);

    function GetTexSwitches(): Integer;
    {$ENDREGION}

    //Инициализация с параметрами из файла
    procedure Init(FileName: PAnsiChar); overload;
    //Инициализация в определенный хэндл
    procedure Init(Handle: THandle; FileName: PAnsiChar); overload; deprecated;
    procedure Step(deltaTime: Double);
    procedure Start();
    procedure Stop();
    procedure DeInit();

    property Enabled: Boolean read GetEnabled write SetEnabled;

    property WindowHandle: Integer read GetWindowHandle;
    property WindowCaption: WideString read GetWindowCaption write SetWindowCaption;
    property WindowWidth: Integer read GetWidth;
    property WindowHeight: Integer read GetHeight;

    property DC: hDC read GetDC;
    property RC: hglRC read GetRC;

    property RenderReady: Boolean read GetRenderReady;
    property FPS: Single read GetFPS;

    property VersionText: PWideChar read GetSelfVersion;

    {Вероятно, вынести в класс TdfWindow?}
    property OnMouseDown: TglrOnMouseDownProc read GetOnMouseDown write SetOnMouseDown;
    property OnMouseUp: TglrOnMouseUpProc read GetOnMouseUp write SetOnMouseUp;
    property OnMouseMove: TglrOnMouseMoveProc read GetOnMouseMove write SetOnMouseMove;
    property OnMouseWheel: TglrOnMouseWheelProc read GetOnMouseWheel write SetOnMouseWheel;

    property OnUpdate: TglrOnUpdateProc read GetOnUpdate write SetOnUpdate;

    property Camera: IglrCamera read GetCamera write SetCamera;

    property RootNode: IglrNode read GetRootNode write SetRootNode;

    property Input: IglrInput read GetInput write SetInput;

    property GUIManager: IglrGUIManager read GetManager write SetManager;

    {debug - количество переключений текстур}
    property TextureSwitches: Integer read GetTexSwitches;

    {Функционал для работы со сценами}
    function RegisterScene(const aScene: IglrBaseScene): Integer;
    procedure UnregisterScene(const aScene: IglrBaseScene);
    procedure UnregisterScenes();
  end;

  IglrObjectFactory = interface
    ['{18D6C31A-3ECC-4F13-B5D0-5F4BF6FB466B}']
    function NewNode(): IglrNode;
    function NewUserRender(): IglrUserRenderable;
    function NewHudSprite(): IglrSprite;
    function NewSprite(): IglrSprite; //for future uses
    function NewMaterial(): IglrMaterial;
    function NewTexture(): IglrTexture;
    function NewFont(): IglrFont;
    function NewText(): IglrText;
    function NewGUIButton(): IglrGUIButton;
    function NewGUITextButton(): IglrGUITextButton;
    function NewGUICheckBox(): IglrGUICheckBox;
    function NewGUITextBox(): IglrGUITextBox;
    function NewGUISlider(): IglrGUISlider;
    function New2DScene(): Iglr2DScene;
    function New3DScene(): Iglr3DScene;
  end;

  procedure LoadRendererLib();
  procedure UnLoadRendererLib();

var
  glrGetRenderer: function(): IglrRenderer; stdcall;
  glrGetObjectFactory: function(): IglrObjectFactory; stdcall;

  dllHandle: THandle;

implementation

procedure LoadRendererLib();
begin
  dllHandle := LoadLibrary(dllname);
  Assert(dllHandle <> 0, 'Ошибка загрузки библиотеки: вероятно библиотека не найдена');

  glrGetRenderer := GetProcAddress(dllHandle, 'GetRenderer');
  glrGetObjectFactory := GetProcAddress(dllHandle, 'GetObjectFactory');
end;

procedure UnLoadRendererLib();
begin
//  FreeLibrary(dllHandle);
end;

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

class function TglrStream.Init(const FileName: String; RW: Boolean): TglrStream;
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


function PCharToPWide(AChar: PAnsiChar): PWideChar;
var
  pw: PWideChar;
  iSize: integer;
begin
  iSize := Length(AChar) + 1;
  pw := AllocMem(iSize * 2);
  MultiByteToWideChar(CP_ACP, 0, AChar, iSize, pw, iSize * 2);

  Result := pw;
end;

function PWideToPChar(pw: PWideChar): PAnsiChar;
var
  p: PAnsiChar;
  iLen: integer;
begin
  iLen := lstrlenw(pw) + 1;
  GetMem(p, iLen);

  WideCharToMultiByte(CP_ACP, 0, pw, iLen, p, iLen * 2, nil, nil);

  Result := p;
  FreeMem(p, iLen);
end;

function SizeOfP(const P: Pointer): Integer;
begin
  if P = nil then
    Result := -1
  else
    Result := Integer(Pointer((Integer(p) - 4))^) and $7FFFFFFC - 4;
end;

initialization
  ReportMemoryLeaksOnShutDown := True;

end.
