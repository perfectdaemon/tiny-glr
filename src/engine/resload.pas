{
  Todo: поправить логи
}

unit resload;

interface

uses
  ogl, tinyglr, glrMath;

type

  TglrObjFace = record
    v, vt, vn: array[0..2] of LongWord;
  end;

  TglrRawMeshData_Obj = class
  public
    v, vn: array of TglrVec3f;
    vt: array of TglrVec2f;
    f: array of TglrObjFace;
  end;

  { TglrMeshData }

  TglrMeshData = record
    vfFormat: TglrVertexFormat;
    ifFormat: TglrIndexFormat;
    vCount, iCount: LongWord;
    vData, iData: Pointer;
    procedure FreeMemory();
  end;

function LoadTexture(const Stream: TglrStream; ext: String;
  out iFormat,cFormat,dType: TGLConst;
  out pSize: Integer;
  out Width, Height: Integer): Pointer;

function LoadFontData(const Stream: TglrStream; out CharCount: LongWord): Pointer;

function LoadText(const Stream: TglrStream): PAnsiChar;
function LoadStringList(const Stream: TglrStream): TglrStringList;
function LoadMesh(const Stream: TglrStream; aFormat: TglrMeshFormat): TglrMeshData;

implementation

procedure flipSurface(chgData: Pbyte; w, h, pSize: integer);
var
  lineSize: integer;
  sliceSize: integer;
  tempBuf: Pbyte;
  j: integer;
  top, bottom: Pbyte;
begin
  lineSize := pSize * w;
  sliceSize := lineSize * h;
  GetMem(tempBuf, lineSize);

  top := chgData;
  bottom := top;
  Inc(bottom, sliceSize - lineSize);

  for j := 0 to (h div 2) - 1 do begin
    Move(top^, tempBuf^, lineSize);
    Move(bottom^, top^, lineSize);
    Move(tempBuf^, bottom^, lineSize);
    Inc(top, lineSize);
    Dec(bottom, lineSize);
  end;
  FreeMem(tempBuf);
end;

type
  BITMAPFILEHEADER = packed record
    bfType : Word;
    bfSize : LongWord;
    bfReserved1 : Word;
    bfReserved2 : Word;
    bfOffBits : LongWord;
  end;

  BITMAPINFOHEADER = record
    biSize : LongWord;
    biWidth : LongInt;
    biHeight : LongInt;
    biPlanes : Word;
    biBitCount : Word;
    biCompression : LongWord;
    biSizeImage : LongWord;
    biXPelsPerMeter : LongInt;
    biYPelsPerMeter : LongInt;
    biClrUsed : LongWord;
    biClrImportant : LongWord;
  end;

function myLoadBMPTexture(Stream: TglrStream; out Format : TGLConst; out Width, Height: Integer): Pointer;
var
  FileHeader: BITMAPFILEHEADER;
  InfoHeader: BITMAPINFOHEADER;
  BytesPP: Integer;
  imageSize: Integer;
  image: Pointer;
  sLength, fLength, tmp: Integer;
  absHeight: Integer;
  i: Integer;
begin
  Result := nil;

  Stream.Read(FileHeader, SizeOf(FileHeader));
  Stream.Read(InfoHeader, SizeOf(InfoHeader));

  if InfoHeader.biClrUsed <> 0 then
  begin
    Log.Write(lError, 'BMP load: Color map is not supported');
    Exit(nil);
  end;

  Width := InfoHeader.biWidth;
  Height := InfoHeader.biHeight;
  //Если высота отрицательная, то битмап перевернут
  absHeight := Abs(Height);
  BytesPP := InfoHeader.biBitCount div 8;
  case BytesPP of
    3: Format := GL_BGR;
    4: Format := GL_BGRA;
  end;
  imageSize := Width * absHeight * BytesPP;

  sLength := Width * BytesPP;
  fLength := 0;
  if frac(sLength / 4) > 0 then
    fLength := ((sLength div 4) + 1) * 4 - sLength;
  GetMem(image, imageSize);
  Result := image;
  for i := 0 to absHeight - 1 do
  begin
    Stream.Read(image^, sLength);
    Stream.Read(tmp, fLength);
    Inc(image, sLength);
  end;
end;

{------------------------------------------------------------------}
{  Loads 24 and 32bpp (alpha channel) TGA textures                 }
{------------------------------------------------------------------}

type
   TTGAHeader = packed record
     IDLength          : Byte;
     ColorMapType      : Byte;
     ImageType         : Byte;
     ColorMapOrigin    : Word;
     ColorMapLength    : Word;
     ColorMapEntrySize : Byte;
     XOrigin           : Word;
     YOrigin           : Word;
     Width             : Word;
     Height            : Word;
     PixelSize         : Byte;
     ImageDescriptor   : Byte;
  end;

   PByteArray = ^TByteArray;
   TByteArray = Array[0..32767] of Byte;

function myLoadTGATexture(Stream: TglrStream; out Format: TGLConst; out Width, Height: Integer): Pointer;
var
  tgaHeader: TTGAHeader;
  colorDepth: Integer; //число бит на пиксель
  bytesPP: Integer; //число байт на пиксель
  imageSize: Integer; //размер изображения в байтах

  image: Pointer; //само изображение

  procedure ReadUncompressedTGA();
  var
    bytesRead: Integer;
    i: Integer;
    Blue, Red: ^Byte;
    Tmp: Byte;
  begin
    bytesRead := Stream.Read(image^, imageSize);
    //Считано меньше, чем необходимо
    if (bytesRead <> imageSize) then
    begin
      Log.Write(lError, 'TGA load: uncompressed data: count of bytes read not equal to size of stream');
      Exit();
    end;
    //Флипаем bgr(a) в rgb(a)
    for i :=0 to Width * Height - 1 do
    begin
      Blue := Pointer(image + i * bytesPP + 0);
      Red  := Pointer(image + i * bytesPP + 2);
      Tmp := Blue^;
      Blue^ := Red^;
      Red^ := Tmp;
    end;
  end;
  (*
  procedure CopySwapPixel(const Source, Destination: Pointer);
  asm
    push ebx
    mov bl,[eax+0]
    mov bh,[eax+1]
    mov [edx+2],bl
    mov [edx+1],bh
    mov bl,[eax+2]
    mov bh,[eax+3]
    mov [edx+0],bl
    mov [edx+3],bh
    pop ebx
  end;         *)

  procedure CopySwapPixelPascal(const Source, Destination: Pointer);
  var
    s, d: PByteArray;
  begin
    s := PByteArray(Source);
    d := PByteArray(Destination);
    d[0] := s[2];
    d[1] := s[1];
    d[2] := s[0];
    d[3] := s[3];
  end;

  procedure ReadCompressedTGA();
  var
    bufferIndex, currentByte, currentPixel: Integer;
    compressedImage: Pointer;

    bytesRead: Integer;

    i: Integer;

    First: ^Byte;
  begin
    currentByte := 0;
    currentPixel := 0;
    bufferIndex := 0;

    GetMem(compressedImage, Stream.Size - SizeOf(tgaHeader));
    bytesRead := Stream.Read(compressedImage^, Stream.Size - SizeOf(tgaHeader));
    if bytesRead <> Stream.Size - SizeOf(tgaHeader) then
    begin
      Log.Write(lError, 'TGA load: compressed data: count of bytes read not equal to size of stream');
      Exit();
    end;

    //Извлекаем данные о пикселях, сжатых по RLE
    repeat
      First := compressedImage + BufferIndex;
      Inc(BufferIndex);
      if First^ < 128 then //Незапакованные данные
      begin
        for i := 0 to First^ do
        begin
          CopySwapPixelPascal(compressedImage+ BufferIndex + i * bytesPP, image + CurrentByte);
          CurrentByte := CurrentByte + bytesPP;
          inc(CurrentPixel);
        end;
        BufferIndex := BufferIndex + (First^ + 1) * bytesPP
      end
      else  //Запакованные данные
      begin
        for i := 0 to First^ - 128 do
        begin
          CopySwapPixelPascal(compressedImage + BufferIndex, image + CurrentByte);
          CurrentByte := CurrentByte + bytesPP;
          inc(CurrentPixel);
        end;
        BufferIndex := BufferIndex + bytesPP;
      end;
    until CurrentPixel >= Width * Height;

    FreeMem(compressedImage, Stream.Size - SizeOf(tgaHeader));
  end;

begin
  Result := nil;
  //Читаем заголовок
  Stream.Read(tgaHeader, SizeOf(TTGAHeader));

  Width := tgaHeader.Width;
  Height := tgaHeader.Height;
  colorDepth := tgaHeader.PixelSize;
  bytesPP := ColorDepth div 8;
  imageSize := Width * Height * bytesPP;

  GetMem(image, ImageSize);

  case bytesPP of
    3: Format := GL_RGB;
    4: Format := GL_RGBA;
  end;

  {$REGION ' Неподдерживаемые типы tga '}

  if (colorDepth <> 24) and (colorDepth <> 32) then
  begin
    Log.Write(lError, 'TGA load: Color depth differs from 24 or 32');
    Exit(nil);
  end;

  if tgaHeader.ColorMapType <> 0 then
  begin
    Log.Write(lError, 'TGA load: ColorMap is not supported');
    Exit(nil);
  end;

  {$ENDREGION}

  case tgaHeader.ImageType of
    2: ReadUncompressedTGA();
    10: ReadCompressedTGA();
    else
    begin
      Log.Write(lError, 'TGA load: Uncompressed and RLE-compressed files are only supported');
      Exit(nil);
    end;
  end;
  Result := image;
end;


function LoadTexture(const Stream: TglrStream; ext: String; out iFormat, cFormat, dType: TGLConst;
  out pSize: Integer; out Width, Height: Integer): Pointer;
begin
  Result := nil;
  if ext = 'bmp' then
  begin
    Result := myLoadBMPTexture(Stream, cFormat, Width, Height);
    if cFormat = GL_BGRA then
    begin
      iFormat := GL_RGBA8;
      dType := GL_UNSIGNED_BYTE;
      pSize := 4;
    end
    else
    begin
      iFormat := GL_RGB8;
      dType := GL_UNSIGNED_BYTE;
      pSize := 3;
    end;
    if Height > 0 then
      flipSurface(Result, Width, Height, pSize)
    else
      Height := -Height;
  end
  else if ext = 'tga' then
  begin
    Result := myLoadTGATexture(Stream, cFormat, Width, Height);
    if cFormat = GL_RGB then
    begin
      iFormat := GL_RGB8;
      pSize := 3;
    end
    else
    begin
      iFormat := GL_RGBA8;
      pSize := 4;
    end;
    dType := GL_UNSIGNED_BYTE;

    flipSurface(Result, Width, Height, pSize);
  end
  else
  begin
    Log.Write(lError, '"' + ext + '" is not supported');
  end;
end;

function LoadFontData(const Stream: TglrStream; out CharCount: LongWord): Pointer;
var
  fh: BITMAPFILEHEADER;
  ih: BITMAPINFOHEADER;
begin
  Stream.Pos := 0;
  Stream.Read(fh, SizeOf(BITMAPFILEHEADER));
  Stream.Read(ih, SizeOf(BITMAPINFOHEADER));
  if (fh.bfReserved1 <> $0F86) then
    Log.Write(lCritical, 'Font load failed, invalid bmpf file');
  Stream.Pos := SizeOf(BITMAPFILEHEADER) + SizeOf(BITMAPINFOHEADER) + ih.biSizeImage;
  Stream.Read(CharCount, SizeOf(LongWord));
  GetMem(Result, Stream.Size - Stream.Pos);
  Stream.Read(Result^, Stream.Size - Stream.Pos);
end;

function LoadText(const Stream: TglrStream): PAnsiChar;
var
  bytesRead: Integer;
  data: PAnsiChar;
begin
  GetMem(data, Stream.Size + 1);
  bytesRead := Stream.Read(data^, Stream.Size);
  data[Stream.Size] := #0;
  if (bytesRead <> Stream.Size) then
    Log.Write(lCritical, 'Text file load: Count of bytes read not equal to stream size');

  Result := data;
end;

function LoadStringList(const Stream: TglrStream): TglrStringList;
var
  p: PAnsiChar;
  line: AnsiString;
  i, start: Integer;
begin
  p := LoadText(Stream);
  line := p;
  FreeMem(p);
  Result := TglrStringList.Create(32);
  start := 0;
  for i := 1 to Length(line) do
    if line[i] = #10 then
    begin
      if (i > 1) and (line[i - 1] = #13) then
        Result.Add(Copy(line, start + 1, i - start - 1))
      else
        Result.Add(Copy(line, start + 1, i - start));
      start := i;
    end;
end;

function LoadRawMeshData_Obj(const Stream: TglrStream): TglrRawMeshData_Obj;

  function GetVec3(s: TglrStringList): TglrVec3f;
  begin
    Result := Vec3f(Convert.ToFloat(s[1]),
      Convert.ToFloat(s[2]),
      Convert.ToFloat(s[3]));
  end;

  function GetVec2(s: TglrStringList): TglrVec2f;
  begin
    Result := Vec2f(Convert.ToFloat(s[1]),
      Convert.ToFloat(s[2]));
  end;

  function GetFace(s: TglrStringList): TglrObjFace;
  var
    s1: TglrStringList;
    i: Integer;
  begin
    for i := 0 to 2 do
    begin
      s1 := StrSplit(s[1 + i], '/');
      Result.v[i]  := Convert.ToInt(s1[1], 0);
      Result.vt[i] := Convert.ToInt(s1[2], 0);
      Result.vn[i] := Convert.ToInt(s1[3], 0);
      s1.Free();
    end;
  end;

var
  stringList, splitStr: TglrStringList;
  firstChar, secondChar: AnsiChar;
  i: Integer;
  vCount, vtCount, vnCount, fCount: Integer;
begin
  stringList := LoadStringList(Stream);
  Result := TglrRawMeshData_Obj.Create();

  vCount := 0;
  vtCount := 0;
  vnCount := 0;
  fCount := 0;
  // first run - count array length
  for i := 0 to stringList.Count - 1 do
  begin
    firstChar := StrTrim(stringList[i])[1];
    secondChar := StrTrim(stringList[i])[2];

    case firstChar of
      '#': continue;
      'f': fCount += 1;
      'v':
        case secondChar of
          ' ': vCount += 1;
          'n': vnCount += 1;
          't': vtCount += 1;
        end;
    end;
  end;

  SetLength(Result.v, vCount + 1);
  SetLength(Result.vn, vnCount + 1);
  SetLength(Result.vt, vtCount + 1);
  SetLength(Result.f, fCount + 1);

  // second run - read information
  vCount := 1;
  vtCount := 1;
  vnCount := 1;
  fCount := 1;
  for i := 0 to stringList.Count - 1 do
  begin
    firstChar := StrTrim(stringList[i])[1];
    if (firstChar = '#') then
      continue;

    splitStr := StrSplit(StrTrim(stringList[i]), ' ');
    if (splitStr[0] = 'v') then
    begin
      Result.v[vCount] := GetVec3(splitStr);
      vCount += 1;
    end
    else if (splitStr[0] = 'vn') then
    begin
      Result.vn[vnCount] := GetVec3(splitStr);
      vnCount += 1;
    end
    else if (splitStr[0] = 'vt') then
    begin
      Result.vt[vtCount] := GetVec2(splitStr);
      vtCount += 1;
    end
    else if (splitStr[0] = 'f') then
    begin
      Result.f[fCount] := GetFace(splitStr);
      fCount += 1;
    end;

    splitStr.Free();
  end;
end;

function MeshDataFromRawMeshData_Obj(Raw: TglrRawMeshData_Obj): TglrMeshData;
var
  i: Integer;
begin
  Log.Write(lCritical, 'MeshDataFromRawMeshData_Obj is not implemented');
  for i := 0 to Length(Raw.f) - 1 do
  begin

  end;
end;

function LoadMesh(const Stream: TglrStream; aFormat: TglrMeshFormat): TglrMeshData;
var
  raw: TglrRawMeshData_Obj;
begin
  case aFormat of
    mfObj:
    begin
      Log.Write(lInformation, 'Mesh load: start loading .obj stream ' + Convert.ToString(Pointer(Stream)));
      raw := LoadRawMeshData_Obj(Stream);
      Log.Write(lInformation, 'Mesh load: raw data loaded successfully');
      Log.Write(lInformation, 'Mesh load: start converting raw data to mesh data');
      Result := MeshDataFromRawMeshData_Obj(raw);
      Log.Write(lInformation, 'Mesh load: converted successfully');
      raw.Free();
      Log.Write(lInformation, 'Mesh load: successfully loaded ' + Convert.ToString(Pointer(Stream)));
    end;
    mfRawGlr:
    begin
      Log.Write(lCritical, 'LoadMesh(RawGlr) is not implemented');
    end;
  end;
end;

{ TglrMeshData }

procedure TglrMeshData.FreeMemory;
begin
  FreeMem(vData);
  FreeMem(iData);
end;

end.
