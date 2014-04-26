unit tinyglr;

//{$mode delphi}

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

  TglrTextureId = type longword;
  TglrShaderId = type longword;
  TglrIndexBufferId = type longword;
  TglrVertexBufferId = type longword;
  TglrFrameBufferId = type longword;
  TglrIndex = type word;

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

  TglrTexture = class
    Id: TglrTextureId;
    procedure Bind();
    constructor Create(aData: Pointer; aCount: Integer; aFormat: TglrTextureFormat); virtual; overload;
    constructor Create(aStream: TglrStream); virtual; overload;
    destructor Destroy(); override;
  end;


  TglrVertexBuffer = class
    Id: TglrVertexBufferId;
    procedure Bind();
    constructor Create(aData: Pointer; aCount: Integer; aFormat: TglrVertexFormat); virtual;
    destructor Destroy(); override;
  end;

  TglrIndexBuffer = class
    Id: TglrIndexBufferId;
    procedure Bind();
    constructor Create(aData: Pointer; aCount: Integer; aFaormat: TglrIndexFormat); virtual;
    destructor Destroy(); override;
  end;

  TglrFrameBuffer = class
    Id: TglrFrameBufferId;
    procedure Bind();
    constructor Create(); virtual; //todo params
    destructor Destroy(); override;
  end;


  TglrBlendingMode = (bmOpaque, bmTransparency, bmAdditive, bmAlphaTest50,
    bmAlphaTest100, bmModulate);
  TglrCullMode = (cmNone, cmBack, cmFront);
  TglrFuncComparison = (fcNever, fcLess, fcEqual, fcLessOrEqual,
    fcGreater, fcNotEqual, fcGreaterOrEqual, fcAlways);

  GLRender = class
  private
  protected
    class var fBlendingMode: TglrBlendingMode;
    class var fCullMode: TglrCullMode;
    class var fDepthWrite, depthTest: boolean;
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
    class procedure SetLighting(aEnabled: boolean);
    class procedure SetDepthWrite(aEnabled: boolean);
    class procedure SetDepthTest(aEnabled: boolean);
    class procedure SetDepthFunc(aComparison: TglrFuncComparison);
    class procedure SetAlphaTest(aComparison: TglrFuncComparison; aValue: single);

    class procedure SetShader(aShader: TglrShaderId);
    class procedure SetTexture(aTexture: TglrTextureId);

    //todo:
    class procedure DrawTriangles();
    class procedure DrawPoints();
    class procedure DrawPointSprites();

  end;

{$ENDREGION}

  {$REGION 'Window'}
  TglrAppView = class abstract
  public
    constructor Create(); virtual; abstract;
    destructor Destroy(); override; abstract;
  end;

  {$ENDREGION}
implementation

const
  VB_STRIDE: array[0..TglrVertexFormat.vfLIMIT - 1] of Integer =
    (SizeOf(TglrVertexP2T2), SizeOf(TglrVertexP3T2));
  IF_STRIDE: array[0..ifLimit - 1] of Integer =
    (SizeOf(Byte), SizeOf(ShortInt), SizeOf(Integer));

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

procedure TglrTexture.Bind();
begin
//  gl.BindTexture();
end;

constructor TglrTexture.Create(aData: Pointer; aCount: Integer; aFormat: TglrTextureFormat);
begin

end;

constructor TglrTexture.Create(aStream: TglrStream);
begin

end;

destructor TglrTexture.Destroy();
begin
  inherited Destroy;
end;

end.
