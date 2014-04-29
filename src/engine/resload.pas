{
  Todo: поправить логи
}

unit resload;

interface

uses
  ogl, tinyglr;

function LoadTexture(const Stream: TglrStream; ext: String;
  out iFormat,cFormat,dType: TGLConst;
  out pSize: Integer;
  out Width, Height: Integer): Pointer;

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
//    logWriteError('TexLoad: Ошибка загрузки BMP из потока. ColorMaps не поддерживаются');
    Exit(nil);
  end;

  Width := InfoHeader.biWidth;
  Height := InfoHeader.biHeight;
  //Если высота отрицательная, то битмап читается сверху вниз
  //flip не нужен
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
    Inc(integer(image), sLength);
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

function myLoadTGATexture(Stream: TglrStream; out Format: TGLConst; out Width, Height: Integer): Pointer;
var
  //tgaFile: File;
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
//      logWriteError('TexLoad: Ошибка загрузки TGA из потока. Ошибка при чтении несжатых данных');
      Exit();
    end;
    //Флипаем bgr(a) в rgb(a)
    for i :=0 to Width * Height - 1 do
    begin
      Blue := Pointer(Integer(image) + i * bytesPP + 0);
      Red  := Pointer(Integer(image) + i * bytesPP + 2);
      Tmp := Blue^;
      Blue^ := Red^;
      Red^ := Tmp;
    end;
  end;

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
//      logWriteError('TexLoad: Ошибка загрузки TGA из потока. Ошибка при чтении сжатых данных');
      Exit();
    end;

    //Извлекаем данные о пикселях, сжатых по RLE
    repeat
      First := Pointer(Integer(compressedImage) + BufferIndex);
      Inc(BufferIndex);
      if First^ < 128 then //Незапакованные данные
      begin
        for i := 0 to First^ do
        begin
          CopySwapPixel(Pointer(Integer(compressedImage)+ BufferIndex + i * bytesPP), Pointer(Integer(image) + CurrentByte));
          CurrentByte := CurrentByte + bytesPP;
          inc(CurrentPixel);
        end;
        BufferIndex := BufferIndex + (First^ + 1) * bytesPP
      end
      else  //Запакованные данные
      begin
        for i := 0 to First^ - 128 do
        begin
          CopySwapPixel(Pointer(Integer(compressedImage) + BufferIndex), Pointer(Integer(image) + CurrentByte));
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
//    logWriteError('TexLoad: Ошибка загрузки TGA из потока. BPP отлично от 24 и 32');
    Exit(nil);
  end;

  if tgaHeader.ColorMapType <> 0 then
  begin
//    logWriteError('TexLoad: Ошибка загрузки TGA из потока. ColorMap не поддерживаются');
    Exit(nil);
  end;

  {$ENDREGION}

  case tgaHeader.ImageType of
    2: {Несжатый tga} ReadUncompressedTGA();
    10: {Сжатый tga} ReadCompressedTGA();
    else
    begin
//      logWriteError('TexLoad: Ошибка загрузки TGA из потока. Поддерживаются только несжатые и RLE-сжатые tga');
      Exit(nil);
    end;
  end;
  Result := image;
end;


function LoadTexture(const Stream: TglrStream; ext: String; out iFormat, cFormat, dType: TGLConst;
  out pSize: Integer; out Width, Height: Integer): Pointer;
begin
  Result := nil;
  if ext = 'BMP' then
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
      flipSurface(Result, Width, Height, pSize);
  end;
  if ext = 'TGA' then
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
  end;

  if ext = 'PNG' then
  begin
    Assert(ext <> 'PNG', 'PNG is not supported yet');
    //LoadPNG(result,PWideChar(FileName),iFormat,cFormat,dType,pSize,Width,Height);
  end;
end;


end.
