unit glr_render;

{$i defines.inc}

interface

uses
  glr_utils,
  glr_ogl,
  glr_math;

type
  TglrTextureId = type LongWord;
  TglrShaderProgramId = type LongWord;
  TglrShaderId = type LongWord;
  TglrIndexBufferId = type LongWord;
  TglrVertexBufferId = type LongWord;
  TglrFrameBufferId = type LongWord;
  TglrIndex = type Word;

  TglrTextureFormat = (tfRGB8, tfRGBA8, tfBGR8, tfBGRA8);
  TglrVertexFormat = (vfPos2Tex2 = 0, vfPos3Tex2, vfPos3Tex2Nor3, vfPos3Tex2Col4);
  TglrIndexFormat = (ifByte = 0, ifShort, ifInt);

  TglrVertexP2T2 = record
    vec, tex: TglrVec2f;
  end;

  TglrVertexP3T2 = record
    vec: TglrVec3f;
    tex: TglrVec2f;
  end;

  TglrVertexP3T2N3 = record
    vec: TglrVec3f;
    tex: TglrVec2f;
    nor: TglrVec3f;
  end;
  TglrVertexP3T2N3List = TglrList<TglrVertexP3T2N3>;

  TglrVertexP3T2C4 = record
    vec: TglrVec3f;
    tex: TglrVec2f;
    col: TglrVec4f;
  end;

  TglrQuadP3T2C4 = array[0..3] of TglrVertexP3T2C4;

  TglrVertexAtrib = (vaCoord = 0, vaNormal = 1, vaTexCoord0 = 2, vaTexCoord1 = 3, vaColor = 4{, ...});

const
  VF_STRIDE: array[Low(TglrVertexFormat)..High(TglrVertexFormat)] of Integer =
    (SizeOf(TglrVertexP2T2), SizeOf(TglrVertexP3T2), SizeOf(TglrVertexP3T2N3), SizeOf(TglrVertexP3T2C4));

  IF_STRIDE: array[Low(TglrIndexFormat)..High(TglrIndexFormat)] of Integer =
    (SizeOf(Byte), SizeOf(Word), SizeOf(LongWord));

type
  TglrVertexBufferMapAccess = (maRead, maWrite, maReadWrite);
  TglrVertexBufferUsage = (
    uStreamDraw, uStreamRead, uStreamCopy,
    uStaticDraw, uStaticRead, uStaticCopy,
    uDynamicDraw, uDynamicRead, uDynamicCopy
    );

  { TglrVertexBuffer }

  TglrVertexBuffer = class
    Id: TglrVertexBufferId;
    Format: TglrVertexFormat;
    Count: Integer;
    procedure Bind();
    constructor Create(aData: Pointer; aCount: Integer;
      aFormat: TglrVertexFormat; aUsage: TglrVertexBufferUsage); virtual;
    destructor Destroy(); override;

    procedure Update(aData: Pointer; aStart, aCount: Integer); virtual;
    function Map(aAccess: TglrVertexBufferMapAccess = maReadWrite): Pointer;
    procedure Unmap();
  end;

  { TglrIndexBuffer }

  TglrIndexBuffer = class
    Id: TglrIndexBufferId;
    Format: TglrIndexFormat;
    Count: LongWord;
    procedure Bind();
    constructor Create(aData: Pointer; aCount: LongWord; aFormat: TglrIndexFormat); virtual;
    destructor Destroy(); override;

    procedure Update(aData: Pointer; aStart, aCount: LongWord); virtual;
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

  TglrTexWrap = (wClamp, wRepeat, wClampToEdge, wClampToBorder, wMirrorRepeat);

  TglrTextureExt = (extBmp, extTga);

  { TglrTexture }

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
    constructor Create(aStream: TglrStream; aExt: TglrTextureExt;
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


  TglrTextureAtlasExt = (aextCheetah);

  TglrTextureRegionsList = TglrList<PglrTextureRegion>;

  { TglrTextureAtlas }

  TglrTextureAtlas = class (TglrTexture)
  protected
    fRegions: TglrTextureRegionsList;
  public
    constructor Create(aImageStream, aInfoStream: TglrStream;
      aImageExt: TglrTextureExt; aInfoExt: TglrTextureAtlasExt;
      aFreeStreamsOnFinish: Boolean = True); virtual;
    destructor Destroy(); override;

    function GetRegion(aName: AnsiString): PglrTextureRegion;
  end;


  TglrBlendingMode = ( bmNone, bmAlpha, bmAdditive, bmMultiply, bmScreen);
  TglrCullMode = (cmNone, cmBack, cmFront);
  TglrFuncComparison = (fcNever, fcLess, fcEqual, fcLessOrEqual,
    fcGreater, fcNotEqual, fcGreaterOrEqual, fcAlways);
  TglrClearMask = (cmAll, cmColor, cmDepth);
  TglrPolygonMode = (pmFill, pmLines, pmPoints);

{ TglrMaterial }

  TglrTextureMaterialInfo = record
    Texture: TglrTexture;
    UniformName: AnsiString;
    ShaderInternalIndex: Integer;
  end;

  TglrMaterial = class
    Shader: TglrShaderProgram;
    Textures: array of TglrTextureMaterialInfo;
    Color: TglrVec4f;
  	Blend: TglrBlendingMode;
  	DepthWrite: Boolean;
    DepthTest: Boolean;
    DepthTestFunc: TglrFuncComparison;
  	Cull: TglrCullMode;
    PolygonMode: TglrPolygonMode;

    constructor Create(aShaderProgram: TglrShaderProgram); virtual; overload;
    constructor Create(aStream: TglrStream;
      aFreeStreamOnFinish: Boolean = True); virtual; overload;
    destructor Destroy(); override;

    procedure AddTexture(aTexture: TglrTexture; aUniformName: AnsiString);

    procedure Bind();
    procedure Unbind();
  end;

  { TglrRenderParams }

  TglrRenderParams = record
    ViewProj, Model, ModelViewProj: TglrMat4f;
    Color: TglrVec4f;
    procedure CalculateMVP();
  end;

const
  TEXURE_SAMPLERS_MAX = 8;

type

  { Render }

  Render = class
  protected
    class var fBlendingMode: TglrBlendingMode;
    class var fCullMode: TglrCullMode;
    class var fPolygonMode: TglrPolygonMode;
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
    class procedure SetPolygonMode(aPolygonMode: TglrPolygonMode);
    class procedure SetBlendingMode(aBlendingMode: TglrBlendingMode);
    class procedure SetDepthWrite(aEnabled: Boolean);
    class procedure SetDepthTest(aEnabled: Boolean);
    class procedure SetDepthFunc(aComparison: TglrFuncComparison);
    class procedure SetVerticalSync(aEnabled: Boolean);
    class procedure SetShader(aShader: TglrShaderProgramId);
    class procedure SetTexture(aTexture: TglrTextureId; aSampler: Integer);
    class procedure SetVertexBuffer(vBuffer: TglrVertexBuffer);
    class procedure SetIndexBuffer(iBuffer: TglrIndexBuffer);

    class procedure DrawTriangles(vBuffer: TglrVertexBuffer; iBuffer: TglrIndexBuffer;
      aStartIndex, aIndicesCount: Integer);
    class procedure DrawPoints(vBuffer: TglrVertexBuffer; aStart, aVertCount: Integer);

    class property TextureBinds: Integer read fStatTextureBind;
    class property TriCount: Integer read fTriCount;
    class property DipCount: Integer read fDIPCount;
    class property Width: Integer read fWidth;
    class property Height: Integer read fHeight;
  end;

implementation

uses
  glr_resload;

const
  // Vertex format usage
  VF_USAGE: array[Low(TglrVertexBufferUsage)..High(TglrVertexBufferUsage)] of TGLConst =
    (GL_STREAM_DRAW, GL_STREAM_READ, GL_STREAM_COPY,
     GL_STATIC_DRAW, GL_STATIC_READ, GL_STATIC_COPY,
     GL_DYNAMIC_DRAW, GL_DYNAMIC_READ, GL_DYNAMIC_COPY);

  // Index format converter
  IF_FORMAT: array[Low(TglrIndexFormat)..High(TglrIndexFormat)] of TGLConst =
  (GL_UNSIGNED_BYTE, GL_UNSIGNED_SHORT, GL_UNSIGNED_INT);

  // Depth func comparison converter
  comparison: array[Low(TglrFuncComparison)..High(TglrFuncComparison)] of TGLConst =
    (GL_NEVER, GL_LESS, GL_EQUAL, GL_LEQUAL, GL_GREATER, GL_NOTEQUAL, GL_GEQUAL, GL_ALWAYS);

  // Texture wraps converter
  aWraps: array[Low(TglrTexWrap)..High(TglrTexWrap)] of TGLConst =
    (GL_CLAMP, GL_REPEAT, GL_CLAMP_TO_EDGE, GL_CLAMP_TO_BORDER, GL_MIRRORED_REPEAT);

  // Texture extensions converter
  TEXTURE_EXT: array[Low(TglrTextureExt)..High(TglrTextureExt)] of AnsiString =
    ('bmp', 'tga');

  // Texture atlas extensions converter
  TEXTURE_ATLAS_EXT: array[Low(TglrTextureAtlasExt)..High(TglrTextureAtlasExt)] of AnsiString =
    ('cheetah');

{ TglrVertexBuffer }

procedure TglrVertexBuffer.Bind;
begin
  Render.SetVertexBuffer(Self);
end;

constructor TglrVertexBuffer.Create(aData: Pointer; aCount: Integer;
  aFormat: TglrVertexFormat; aUsage: TglrVertexBufferUsage);
begin
  gl.GenBuffers(1, @Self.Id);
  Self.Format := aFormat;
  Self.Count := aCount;
  Bind();
  gl.BufferData(GL_ARRAY_BUFFER, VF_STRIDE[aFormat] * aCount, aData, VF_USAGE[aUsage]);
end;

procedure TglrVertexBuffer.Update(aData: Pointer; aStart, aCount: Integer);
begin
  Bind();
  gl.BufferSubData(GL_ARRAY_BUFFER, aStart, aCount * VF_STRIDE[Format], aData);
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

{ TglrIndexBuffer }

procedure TglrIndexBuffer.Bind;
begin
  Render.SetIndexBuffer(Self);
end;

constructor TglrIndexBuffer.Create(aData: Pointer; aCount: LongWord;
  aFormat: TglrIndexFormat);
begin
  gl.GenBuffers(1, @Self.Id);
  Format := aFormat;
  Count := aCount;
  Bind();
  gl.BufferData(GL_ELEMENT_ARRAY_BUFFER, IF_STRIDE[Format] * aCount, aData, GL_STATIC_DRAW);
end;

destructor TglrIndexBuffer.Destroy;
begin
  gl.DeleteBuffers(1, @Self.Id);
  inherited Destroy;
end;

procedure TglrIndexBuffer.Update(aData: Pointer; aStart, aCount: LongWord);
begin
  Bind();
  gl.BufferSubData(GL_ELEMENT_ARRAY_BUFFER, aStart, aCount * IF_STRIDE[Format], aData);
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

constructor TglrTexture.Create(aStream: TglrStream; aExt: TglrTextureExt;
  aFreeStreamOnFinish: Boolean);
var
  data: Pointer;
  iFormat, cFormat, dType: TGLConst;
  pSize: Integer;

  aFormat: TglrTextureFormat;
begin
  data := LoadTexture(aStream, TEXTURE_EXT[aExt], iFormat, cFormat, dType, pSize, Self.Width, Self.Height);
  case cFormat of
    GL_BGR:  aFormat := tfBGR8;
    GL_BGRA: aFormat := tfBGRA8;
    GL_RGB:  aFormat := tfRGB8;
    GL_RGBA: aFormat := tfRGBA8;
  end;

  Create(data, Width, Height, aFormat);
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

constructor TglrTextureAtlas.Create(aImageStream, aInfoStream: TglrStream;
  aImageExt: TglrTextureExt; aInfoExt: TglrTextureAtlasExt;
  aFreeStreamsOnFinish: Boolean);
var
  lines, list: TglrStringList;
  i: Integer;
  p: PglrTextureRegion;
begin
  if aInfoExt = aextCheetah then
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
    Log.Write(lCritical, 'TextureAtlas: unrecognizable info file extension: `' + TEXTURE_ATLAS_EXT[aInfoExt] + '`');
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

{ TglrMaterial }

constructor TglrMaterial.Create(aShaderProgram: TglrShaderProgram);
begin
  inherited Create;
  Shader := aShaderProgram;
  SetLength(Textures, 0);

  Blend := bmAlpha;
  Color := Vec4f(1, 1, 1, 1);
  Cull := cmBack;
  PolygonMode := pmFill;
  DepthTest := True;
  DepthWrite := True;
  DepthTestFunc := fcLess;
end;

constructor TglrMaterial.Create(aStream: TglrStream;
  aFreeStreamOnFinish: Boolean);
begin
  Create(TglrShaderProgram(nil));
  Log.Write(lCritical, 'Material create from stream is not implemented');
end;

destructor TglrMaterial.Destroy;
begin
  SetLength(Textures, 0);
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
  Render.SetPolygonMode(PolygonMode);
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

{ TglrRenderParams }

procedure TglrRenderParams.CalculateMVP;
begin
  ModelViewProj := ViewProj * Model;
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
  SetPolygonMode(pmFill);
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

  Params.Color := Vec4f(1, 1, 1, 1);
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

class procedure Render.SetPolygonMode(aPolygonMode: TglrPolygonMode);
begin
  if (fPolygonMode = aPolygonMode) then
    Exit();

  case aPolygonMode of
    pmFill: gl.PolygonMode(GL_FRONT_AND_BACK, GL_FILL);
    pmLines: gl.PolygonMode(GL_FRONT_AND_BACK, GL_LINE);
    pmPoints: gl.PolygonMode(GL_FRONT_AND_BACK, GL_POINT);
  end;

  fPolygonMode := aPolygonMode;
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

class procedure Render.SetVertexBuffer(vBuffer: TglrVertexBuffer);
begin
  if (vBuffer = nil) then
    gl.BindBuffer(GL_ARRAY_BUFFER, 0)
  else
  begin
    gl.BindBuffer(GL_ARRAY_BUFFER, vBuffer.Id);
    case vBuffer.Format of
      vfPos3Tex2:
      begin
        gl.EnableVertexAttribArray(Ord(vaCoord));
        gl.EnableVertexAttribArray(Ord(vaTexCoord0));
			  gl.VertexAttribPointer(Ord(vaCoord), 3, GL_FLOAT, False, VF_STRIDE[vBuffer.Format], nil);
        gl.VertexAttribPointer(Ord(vaTexCoord0), 2, GL_FLOAT, False, VF_STRIDE[vBuffer.Format], Pointer(SizeOf(TglrVec3f)));
      end;
      vfPos2Tex2:
      begin
        gl.EnableVertexAttribArray(Ord(vaCoord));
        gl.EnableVertexAttribArray(Ord(vaTexCoord0));
			  gl.VertexAttribPointer(Ord(vaCoord), 2, GL_FLOAT, False, VF_STRIDE[vBuffer.Format], nil);
			  gl.VertexAttribPointer(Ord(vaTexCoord0), 2, GL_FLOAT, False, VF_STRIDE[vBuffer.Format], Pointer(SizeOf(TglrVec2f)));
      end;
      vfPos3Tex2Nor3:
      begin
        gl.EnableVertexAttribArray(Ord(vaCoord));
        gl.EnableVertexAttribArray(Ord(vaTexCoord0));
        gl.EnableVertexAttribArray(Ord(vaNormal));
			  gl.VertexAttribPointer(Ord(vaCoord), 3, GL_FLOAT, False, VF_STRIDE[vBuffer.Format], nil);
        gl.VertexAttribPointer(Ord(vaTexCoord0), 2, GL_FLOAT, False, VF_STRIDE[vBuffer.Format], Pointer(SizeOf(TglrVec3f)));
        gl.VertexAttribPointer(Ord(vaNormal), 3, GL_FLOAT, False, VF_STRIDE[vBuffer.Format], Pointer(SizeOf(TglrVec3f) + SizeOf(TglrVec2f)));
      end;
      vfPos3Tex2Col4:
      begin
        gl.EnableVertexAttribArray(Ord(vaCoord));
        gl.EnableVertexAttribArray(Ord(vaTexCoord0));
        gl.EnableVertexAttribArray(Ord(vaColor));
			  gl.VertexAttribPointer(Ord(vaCoord), 3, GL_FLOAT, False, VF_STRIDE[vBuffer.Format], nil);
			  gl.VertexAttribPointer(Ord(vaTexCoord0), 2, GL_FLOAT, False, VF_STRIDE[vBuffer.Format], Pointer(SizeOf(TglrVec3f)));
        gl.VertexAttribPointer(Ord(vaColor), 4, GL_FLOAT, False, VF_STRIDE[vBuffer.Format], Pointer(SizeOf(TglrVec3f) + SizeOf(TglrVec2f)));
      end
      else
        Log.Write(lCritical, 'Unsupported type of vertexbuffer format. Tinyglr developer is an asshole');
    end
  end;
end;

class procedure Render.SetIndexBuffer(iBuffer: TglrIndexBuffer);
begin
  if iBuffer = nil then
    gl.BindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0)
  else
    gl.BindBuffer(GL_ELEMENT_ARRAY_BUFFER, iBuffer.Id);
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

end.

