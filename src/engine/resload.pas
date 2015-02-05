{
  Todo: поправить логи
}

unit resload;

interface

uses
  ogl, tinyglr, glrMath;

function LoadTexture(const Stream: TglrStream; ext: String;
  out iFormat,cFormat,dType: TGLConst;
  out pSize: Integer;
  out Width, Height: Integer): Pointer;

function LoadFontData(const Stream: TglrStream; out CharCount: LongWord): Pointer;

function LoadText(const Stream: TglrStream): PAnsiChar;
function LoadStringList(const Stream: TglrStream): TglrStringList;
function LoadMesh(const Stream: TglrStream; aFormat: TglrMeshFormat): TglrMeshData;

function SaveMesh(const meshData: TglrMeshData; aFormat: TglrMeshFormat): TglrStream;

implementation

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

function LoadRaw_Obj(const Stream: TglrStream): TglrObjRawMeshData;

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
      Result.v[i]  := Convert.ToInt(s1[0], 0);
      if s1.Count > 1 then
        Result.vt[i] := Convert.ToInt(s1[1], 0)
      else
        Result.vt[i] := 0;

      if s1.Count > 2 then
        Result.vn[i] := Convert.ToInt(s1[2], 0)
      else
        Result.vn[i] := 0;

      s1.Free();
    end;
  end;

var
  stringList, splitStr: TglrStringList;
  i, j: Integer;
begin
  stringList := LoadStringList(Stream);
  Result := TglrObjRawMeshData.Create();

  j := Result.subs.Add(TglrObjRawSubMeshData.Create());
  for i := 0 to stringList.Count - 1 do
  begin
    splitStr := StrSplit(StrTrim(stringList[i]), ' '#9);

    if (splitStr[0] = '#') then
      continue
    else if (splitStr[0] = 'f') then
      Result.subs[j].f.Add(GetFace(splitStr))
    else if (splitStr[0] = 'v') then
      Result.v.Add(GetVec3(splitStr))
    else if (splitStr[0] = 'vn') then
      Result.vn.Add(GetVec3(splitStr))
    else if (splitStr[0] = 'vt') then
      Result.vt.Add(GetVec2(splitStr))
    else if (splitStr[0] = 'o') then
      if (Result.subs[j].f.Count = 0) then
        Result.subs[j].Name := splitStr[1]
      else
      begin
        j := Result.subs.Add(TglrObjRawSubMeshData.Create());
        Result.subs[j].Name := splitStr[1];
      end;
    splitStr.Free();
  end;
  stringList.Free();
end;

function GetMeshDataFromRaw(Raw: TglrObjRawMeshData): TglrMeshData; overload;
var
  i, j: Integer;

  verts: TglrObjIndexList;
  vertices: TglrVertexP3T2N3List;
  indices: TglrLongWordList;

  function GetVertIndex(V, VT, VN: LongWord): LongWord;
  var
    k: Integer;
    nPTN: ^TglrVertexP3T2N3;
    nVert: ^TglrObjIndex;
  begin
    for k := verts.Count - 1 downto 0 do
      if (verts[k].v = V) and (verts[k].vn = VN) and (verts[k].vt = VT) then
        Exit(k);

    New(nVert);
    nVert^.v := V;
    nVert^.vt := VT;
    nVert^.vn := VN;
    Result := verts.Add(nVert^);
    Dispose(nVert);

    New(nPTN);
    nPTN^.vec := Raw.v[v - 1];
    if (vt > 0) then
      nPTN^.tex := Raw.vt[vt - 1]
    else
      nPTN^.tex := Vec2f(0, 0);
    if (vn > 0) then
      nPTN^.nor := Raw.vn[vn - 1]
    else
      //TODO: auto calculate normal
      nPTN^.nor := Vec3f(0, 0, 0);
    vertices.Add(nPTN^);
    Dispose(nPTN);
  end;

begin
  vertices := TglrVertexP3T2N3List.Create(Raw.v.Count);
  indices := TglrLongWordList.Create(Raw.v.Count);
  verts := TglrObjIndexList.Create(Raw.v.Count);

  SetLength(Result.subMeshes, Raw.subs.Count);
  for i := 0 to Raw.subs.Count - 1 do
  begin
    Result.subMeshes[i].name := Raw.subs[i].Name;
    Result.subMeshes[i].start := indices.Count;
    Result.subMeshes[i].count := Raw.subs[i].f.Count * 3;
    for j := 0 to Raw.subs[i].f.Count - 1 do
    begin
      indices.Add(GetVertIndex(Raw.subs[i].f[j].v[0], Raw.subs[i].f[j].vt[0], Raw.subs[i].f[j].vn[0]));
      indices.Add(GetVertIndex(Raw.subs[i].f[j].v[1], Raw.subs[i].f[j].vt[1], Raw.subs[i].f[j].vn[1]));
      indices.Add(GetVertIndex(Raw.subs[i].f[j].v[2], Raw.subs[i].f[j].vt[2], Raw.subs[i].f[j].vn[2]));
    end;
  end;

  Result.vBuffer := TglrVertexBuffer.Create(vertices.FirstElementAddr, vertices.Count, vfPos3Tex2Nor3, uStaticDraw);
  Result.iBuffer := TglrIndexBuffer.Create(indices.FirstElementAddr, indices.Count, ifInt);

  Result.vLength := vertices.Count * SizeOf(TglrVertexP3T2N3);
  Result.iLength := indices.Count * SizeOf(LongWord);
  Result.vData := GetMem(Result.vLength);
  Result.iData := GetMem(Result.iLength);
  Move(vertices.FirstElementAddr^, Result.vData^, vertices.Count * SizeOf(TglrVertexP3T2N3));
  Move(indices.FirstElementAddr^, Result.iData^, indices.Count * SizeOf(LongWord));

  verts.Free();
  vertices.Free();
  indices.Free();
end;

// Load .raw files
// Structure
//  4 bytes     LongWord    Magic number 0xA505FF75
//  1 bytes     Byte        Version
//  1 byte      Byte        Vertex format [0 - vfPos2Tex2, 1 - vfPos3Tex2, 2 - vfPos3Tex2Nor3, 3 - vfPos3Tex2Col4]
//  1 byte      Byte        Index format [0 - ifByte, 1 - ifShort, 2 - ifInt]
//  4 bytes     LongWord    Vertex data V size in bytes
//  4 bytes     LongWord    Index data I size in bytes
//  V bytes     ---         Vertex data
//  I bytes     ---         Index data
//  2 bytes     Word        Submesh count N
//  N records with structure:
//      2 bytes     Word        Submesh name' length
//      L bytes     AnsiString  Submesh name
//      4 bytes     LongWord    Submesh start index
//      4 bytes     LongWord    Submesh index count

const
  RAWMESH_MAGIC: LongWord = $A505FF75;
function GetMeshDataFromRaw(const Stream: TglrStream): TglrMeshData; overload;
var
  // Buffers
  w: Word;
  lw: LongWord;
  b: Byte;

  vertexFormat: TglrVertexFormat;
  indexFormat: TglrIndexFormat;
  i: Integer;
begin
  FillChar(Result, SizeOf(TglrMeshData), 0);

  //Check magic number
  Stream.Read(lw, SizeOf(LongWord));
  if (lw <> RAWMESH_MAGIC) then
    Log.Write(lCritical, 'Mesh load: .raw stream has wrong format');

  // Check version
  Stream.Read(b, SizeOf(Byte));
  // No action required

  // Read vertex format
  Stream.Read(b, SizeOf(Byte));
  vertexFormat := TglrVertexFormat(b);

  // Read index format
  Stream.Read(b, SizeOf(Byte));
  indexFormat := TglrIndexFormat(b);

  // Read vertex and index data length in bytes
  Stream.Read(Result.vLength, SizeOf(LongWord));
  Stream.Read(Result.iLength, SizeOf(LongWord));

  // Allocate memory for vertex and index data pointers
  GetMem(Result.vData, Result.vLength);
  GetMem(Result.iData, Result.iLength);

  // Read vertex and index data
  Stream.Read(Result.vData^, Result.vLength);
  Stream.Read(Result.iData^, Result.iLength);

  // Read submesh count
  Stream.Read(w, SizeOf(Word));
  SetLength(Result.subMeshes, w);

  // Read submeshes data
  for i := 0 to w - 1 do
  begin
    // Read name
    Result.subMeshes[i].name := Stream.ReadAnsi();

    // Read start and count data
    Stream.Read(Result.subMeshes[i].start, SizeOf(LongWord));
    Stream.Read(Result.subMeshes[i].count, SizeOf(LongWord));
  end;

  // Build bufer objects
  with Result do
  begin
    vBuffer := TglrVertexBuffer.Create(vData, vLength div VF_STRIDE[vertexFormat], vertexFormat, uStaticDraw);
    iBuffer := TglrIndexBuffer.Create(iData, iLength div IF_STRIDE[indexFormat], indexFormat);
  end;
end;

function LoadMesh(const Stream: TglrStream; aFormat: TglrMeshFormat): TglrMeshData;
var
  raw: TglrObjRawMeshData;
begin
  case aFormat of
    mfObj:
    begin
      Log.Write(lInformation, 'Mesh load: start loading .obj stream ' + Convert.ToString(Pointer(Stream)));
      raw := LoadRaw_Obj(Stream);
      Log.Write(lInformation, 'Mesh load: .obj data loaded successfully. Total objects: ' + Convert.ToString(raw.subs.Count));
      Log.Write(lInformation, 'Mesh load: start converting .obj data to mesh data');
      Result := GetMeshDataFromRaw(raw);
      Log.Write(lInformation, 'Mesh load: converted successfully');
      raw.Free();
      Log.Write(lInformation, 'Mesh load: successfully loaded ' + Convert.ToString(Pointer(Stream)));
    end;

    mfRawGlr:
    begin
      Log.Write(lInformation, 'Mesh load: start loading .raw stream ' + Convert.ToString(Pointer(Stream)));
      Result := GetMeshDataFromRaw(Stream);
      Log.Write(lInformation, 'Mesh load: .raw data loaded successfully. Total objects: ' + Convert.ToString(Length(Result.subMeshes)));
      Log.Write(lInformation, 'Mesh load: successfully loaded ' + Convert.ToString(Pointer(Stream)));
    end;
  end;
end;

const
  RAWGLR_VERSION: Byte = $01;

function SaveMesh(const meshData: TglrMeshData; aFormat: TglrMeshFormat): TglrStream;

  function CalculateMemSize(): LongWord;
  var
    i: Integer;
  begin
    Result := SizeOf(LongWord) {Magic}
      + SizeOf(Byte) {Version}
      + SizeOf(Byte) {Vertex format}
      + SizeOf(Byte) {Index format}
      + SizeOf(LongWord) {Vertex data size}
      + SizeOf(LongWord) {Index data size}
      + meshData.vLength
      + meshData.iLength
      + SizeOf(Word) {Submesh count}
      + (SizeOf(Word) + SizeOf(LongWord) + SizeOf(LongWord)) * Length(meshData.subMeshes); {submesh data excluding name string}
    for i := 0 to Length(meshData.subMeshes) - 1 do
      Result += Length(meshData.subMeshes[i].name);
  end;

var
  size: LongWord;
  i: Integer;
  // Word buffer
  w: Word;
begin
  case aFormat of
    mfRawGlr:
    begin
      size := CalculateMemSize();
      Result := TglrStream.Init(GetMem(size), size, True);
      Log.Write(lInformation, 'Mesh save: start saving mesh data to stream ' + Convert.ToString(@Result));
      Log.Write(lInformation, 'Mesh save: calculated memory size in bytes: ' + Convert.ToString(size));

      // Write magic
      Result.Write(RAWMESH_MAGIC, SizeOf(LongWord));

      // Write version
      Result.Write(RAWGLR_VERSION, SizeOf(Byte));

      // Write vertex format
      Result.Write(meshData.vBuffer.Format, SizeOf(Byte));

      // Write index format
      Result.Write(meshData.iBuffer.Format, SizeOf(Byte));

      // Write vertex and index data length in bytes
      Result.Write(meshData.vLength, SizeOf(LongWord));
      Result.Write(meshData.iLength, SizeOf(LongWord));

      // Write vertex and index data
      Result.Write(meshData.vData^, meshData.vLength);
      Result.Write(meshData.iData^, meshData.iLength);

      // Write submesh count
      w := Word(Length(meshData.subMeshes));
      Result.Write(w, SizeOf(Word));

      // Write submeshes data
      for i := 0 to Length(meshData.subMeshes) - 1 do
      begin
        // Write name' length
        Result.WriteAnsi(meshData.subMeshes[i].name);

        // Write start and count data
        Result.Write(meshData.subMeshes[i].start, SizeOf(LongWord));
        Result.Write(meshData.subMeshes[i].count, SizeOf(LongWord));
      end;

      Log.Write(lInformation, 'Mesh save: successfully saved to stream ' + Convert.ToString(@Result));
    end;

    mfObj:
      Log.Write(lCritical, 'Mesh save: Save to .obj is not implemented');
  end;
end;

end.
