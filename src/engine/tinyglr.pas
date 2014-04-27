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
  {$ENDREGION}

  {$REGION 'Render'}

  TglrTextureId = type LongWord;
  TglrShaderId = type LongWord;
  TglrIndexBufferId = type LongWord;
  TglrVertexBufferId = type LongWord;
  TglrFrameBufferId = type LongWord;
  TglrIndex = type Word;

  TglrTextureFormat = (tfFuck);
  TglrVertexFormat = (vfPos2Tex2, vfPos3Tex2{, ...}, vfLIMIT);
  TglrIndexFormat = (ifByte, ifShort, ifInt{, ...}, ifLIMIT);

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


  TglrBlendingMode = (bmOpaque, bmTransparency, bmAdditive, bmAlphaTest50,
    bmAlphaTest100, bmModulate);
  TglrCullMode = (cmNone, cmBack, cmFront);
  TglrFuncComparison = (fcNever, fcLess, fcEqual, fcLessOrEqual,
    fcGreater, fcNotEqual, fcGreaterOrEqual, fcAlways);

  { GLRender }

  GLRender = class
  private
  protected
    class var fBlendingMode: TglrBlendingMode;
    class var fCullMode: TglrCullMode;
    class var fDepthWrite, depthTest: Boolean;
    class var fShader: TglrShaderId;
    class var fTexture: TglrTextureId;
    class var fVB: TglrVertexBufferId;
    class var fIB: TglrIndexBufferId;
    class var fFB: TglrFrameBufferId;
  public
    class procedure Init();
    class procedure DeInit();

    class procedure SetCullMode(aCullMode: TglrCullMode);
    class procedure SetBlendingMode(aBlendingMode: TglrBlendingMode);
    class procedure SetLighting(aEnabled: Boolean);
    class procedure SetDepthWrite(aEnabled: Boolean);
    class procedure SetDepthTest(aEnabled: Boolean);
    class procedure SetDepthFunc(aComparison: TglrFuncComparison);
    class procedure SetAlphaTest(aComparison: TglrFuncComparison; aValue: Single);

    class procedure SetShader(aShader: TglrShaderId);
    class procedure SetTexture(aTexture: TglrTextureId; aSampler: Integer);

    //todo:
    class procedure DrawTriangles(vBuffer: TglrVertexBuffer; iBuffer: TglrIndexBuffer;
      aStart, aVertCount: Integer);
    class procedure DrawPoints(vBuffer: TglrVertexBuffer; aStart, aVertCount: Integer);
//    class procedure DrawPointSprites();

  end;

{$ENDREGION}

  {$REGION 'Window'}
  TglrAppView = class abstract
  public
    constructor Create(); virtual; abstract;
    destructor Destroy(); override; abstract;
  end;

  {$ENDREGION}

  {$REGION 'Core and Game'}

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

		kNumLock = $90, kScrollLock
  {$ENDIF}
  );

  { TglrInput }

  TglrInput = class
    Touch: array[0..9] of TglrTouch;
		KeyDown: array[0..255] of Boolean;

		procedure Process(aType: TglrInputType; aKey: TglrKey; X, Y, aOtherParam: Integer);
  end;

  TglrGame = class abstract
  public
    procedure OnStart(); virtual; abstract;
    procedure OnFinish(); virtual; abstract;

    procedure OnUpdate(const dt: Double); virtual; abstract;
    procedure OnRender(); virtual; abstract;

    procedure OnInput(aType: TglrInputType; aKey: TglrKey; X, Y, aOtherParam: Integer); virtual; abstract;
  end;

  Glr = class
  protected
    class var fGame: TglrGame;

    class procedure Resize(aNewWidth, aNewHeight: Integer);
    class procedure InputReceived(aType: TglrInputType; aKey: TglrKey; X, Y, aOtherParam: Integer);
  public
    class procedure Init(aGame: TglrGame);

    class procedure Start();
    class procedure Pause();
    class procedure Resume();
    class procedure Stop();

    class procedure DeInit();
  end;

  {$ENDREGION}

implementation

uses
{$IFDEF WINDOWS}
  sys_win,
{$ENDIF}
  resload;

const
  VF_STRIDE: array[0..TglrVertexFormat.vfLIMIT - 1] of Integer =
    (SizeOf(TglrVertexP2T2), SizeOf(TglrVertexP3T2));
  IF_STRIDE: array[0..ifLimit - 1] of Integer =
    (SizeOf(Byte), SizeOf(ShortInt), SizeOf(Integer));

{ Glr }

class procedure Glr.Resize(aNewWidth, aNewHeight: Integer);
begin

end;

class procedure Glr.InputReceived(aType: TglrInputType; aKey: TglrKey; X, Y,
  aOtherParam: Integer);
begin

end;

class procedure Glr.Init(aGame: TglrGame);
begin

end;

class procedure Glr.Start;
begin

end;

class procedure Glr.Pause;
begin

end;

class procedure Glr.Resume;
begin

end;

class procedure Glr.Stop;
begin

end;

class procedure Glr.DeInit;
begin

end;

{ TglrInput }

procedure TglrInput.Process(aType: TglrInputType; aKey: TglrKey; X, Y,
  aOtherParam: Integer);
var
  t: ^TglrTouch;
  k: ^Boolean;
begin
  t := @Touch[Ord(aKey)];
  k := @KeyDown[Ord(aKey)];
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
      ;
  end;
end;

{ GLRender }

class procedure GLRender.Init;
begin
  gl.Init();
end;

class procedure GLRender.DeInit;
begin
  gl.Free();
end;

class procedure GLRender.SetCullMode(aCullMode: TglrCullMode);
begin

end;

class procedure GLRender.SetBlendingMode(aBlendingMode: TglrBlendingMode);
begin

end;

class procedure GLRender.SetLighting(aEnabled: Boolean);
begin

end;

class procedure GLRender.SetDepthWrite(aEnabled: Boolean);
begin

end;

class procedure GLRender.SetDepthTest(aEnabled: Boolean);
begin

end;

class procedure GLRender.SetDepthFunc(aComparison: TglrFuncComparison);
begin

end;

class procedure GLRender.SetAlphaTest(aComparison: TglrFuncComparison;
  aValue: Single);
begin

end;

class procedure GLRender.SetShader(aShader: TglrShaderId);
begin

end;

class procedure GLRender.SetTexture(aTexture: TglrTextureId; aSampler: Integer);
begin

end;

class procedure GLRender.DrawTriangles(vBuffer: TglrVertexBuffer;
  iBuffer: TglrIndexBuffer; aStart, aVertCount: Integer);
begin

end;

class procedure GLRender.DrawPoints(vBuffer: TglrVertexBuffer; aStart,
  aVertCount: Integer);
begin

end;

{ TglrFrameBuffer }

procedure TglrFrameBuffer.Bind;
begin
  gl.BindBuffer(GL_FRAMEBUFFER, Self.Id);
end;

class procedure TglrFrameBuffer.Unbind;
begin
  gl.BindBuffer(GL_FRAMEBUFFER, 0);
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
  gl.BufferData(GL_ELEMENT_ARRAY_BUFFER, IF_STRIDE[Ord(aFormat)] * aCount, aData, GL_STATIC_DRAW);
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
  gl.BufferData(GL_ARRAY_BUFFER, VF_STRIDE[Ord(aFormat)] * aCount, aData, GL_STATIC_DRAW);
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


{ TglrTexture }

const
  aWraps: array[Low(TglrTexWrap)..High(TglrTexWrap)] of TGLConst =
    (GL_CLAMP, GL_REPEAT, GL_CLAMP_TO_EDGE, GL_CLAMP_TO_BORDER, GL_MIRRORED_REPEAT);
  aTextureMode: array[Low(TglrTexCombineMode)..High(TglrTexCombineMode)] of TGLConst =
    (GL_DECAL, GL_MODULATE, GL_BLEND, GL_REPLACE, GL_ADD);

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

end.
