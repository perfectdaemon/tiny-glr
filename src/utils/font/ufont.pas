{
  Based on XProger' corexgine tool for font generation
}

unit uFont;

{$mode delphi}

interface

uses
  windows,
  classes,
  sysutils;

type
  TFontChar = class
    ID : WideChar;
    PosX, PosY : LongInt;
    OffsetY, Width, Height : LongInt;
    Data : PByteArray;
  end;

  TFontNode = class
    constructor Create(const Rect: TRect);
    destructor Destroy; override;
  public
    Block : Boolean;
    Node  : array [0..1] of TFontNode;
    Rect  : TRect;
    function Insert(Width, Height: LongInt; out X, Y: LongInt): Boolean;
  end;

  { TFontGenerator }

  TFontGenerator = class
    constructor Create; virtual;
    destructor Destroy; override;
  private
    FNT : HFONT;
    MDC : HDC;
    BMP : HBITMAP;
    BI  : TBitmapInfo;
    CharData : PByteArray;
    MaxWidth, MaxHeight : LongInt;
  public
    TexWidth, TexHeight: LongInt;
    TexData  : PByteArray;
    FontChar : array of TFontChar;
    FontData : array [WideChar] of TFontChar;
    Text     : WideString;
    procedure GenInit(const Face: WideString; Size: LongInt; Bold, Italic: Boolean);
    procedure GenFree;
    function GenChar(c: WideChar): TFontChar;
    procedure AddChar(c: WideChar);
    procedure AddChars(str: WideString);
    procedure ClearChars;
    function PackChars: LongInt;
//    procedure Save(const FileName: string);
    procedure SaveBmpToFile(const FileName: AnsiString);
    procedure SaveFntToFile(const FileName: AnsiString);
    function SaveBmpToStream(): TStream;
  end;

implementation

//uses
//  tinyglr in '..\..\engine\tinyglr.pas';

function Recti(Left, Top, Right, Bottom: LongInt): TRect;
begin
  Result.Left   := Left;
  Result.Top    := Top;
  Result.Right  := Right;
  Result.Bottom := Bottom;
end;

function ToPow2(x: LongInt): LongInt;
begin
  Result := x - 1;
  Result := Result or (Result shr 1);
  Result := Result or (Result shr 2);
  Result := Result or (Result shr 4);
  Result := Result or (Result shr 8);
  Result := Result or (Result shr 16);
  Result := Result + 1;
end;

{$REGION 'TFontNode'}
constructor TFontNode.Create(const Rect: TRect);
begin
  Self.Rect := Rect;
end;

destructor TFontNode.Destroy;
begin
  if Node[0] <> nil then Node[0].Free;
  if Node[1] <> nil then Node[1].Free;
end;

function TFontNode.Insert(Width, Height: LongInt; out X, Y: LongInt): Boolean;
var
  dw, dh : LongInt;
begin
  if (Node[0] <> nil) and (Node[1] <> nil) then
  begin
    Result := Node[0].Insert(Width, Height, X, Y);
    if not Result then
      Result := Node[1].Insert(Width, Height, X, Y);
  end else
  begin
    dw := Rect.Right - Rect.Left;
    dh := Rect.Bottom - Rect.Top;
    if (dw < Width) or (dh < Height) or Block then
    begin
      Result := False;
      Exit;
    end else
      if (dw = Width) and (dh = Height) then
      begin
        X := Rect.Left;
        Y := Rect.Top;
        Block := True;
        Result := True;
        Exit;
      end else
        with Rect do
          if dw - Width > dh - Height then
          begin
            Node[0] := TFontNode.Create(Recti(Left, Top, Left + Width, Bottom));
            Node[1] := TFontNode.Create(Recti(Left + Width, Top, Right, Bottom));
          end else
          begin
            Node[0] := TFontNode.Create(Recti(Left, Top, Right, Top + Height));
            Node[1] := TFontNode.Create(Recti(Left, Top + Height, Right, Bottom));
          end;
    Result := Node[0].Insert(Width, Height, X, Y);
  end;
end;
{$ENDREGION}

{$REGION 'TFontDisplay'}
constructor TFontGenerator.Create;
begin
  inherited Create();
  //GenInit('Arial', 10, True, False);
  //AddChars('0123456789');
  //AddChars(' !@#$%^&*()_+|{}:"<>?-=\[];'',./~`');
  //AddChars(UTF8Decode('abcdefghijklmnopqrstuvwxyz'));
  //AddChars(UTF8Decode('ABCDEFGHIJKLMNOPQRSTUVWXYZ'));
  //AddChars(UTF8Decode('абвгдеёжзийклмнопрстуфхцчшщъыьэюя'));
  //AddChars(UTF8Decode('АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ'));
  //PackChars;
  //SaveBmpToFile('Arial_10b');
end;

destructor TFontGenerator.Destroy;
begin
  GenFree;
  ClearChars;
  inherited;
end;

procedure TFontGenerator.GenInit(const Face: WideString; Size: LongInt; Bold, Italic: Boolean);
var
  Weight : LongInt;
  DC : LongWord;
  TM : TTextMetricW;
begin
  DC := GetDC(0);

  if Bold then
    Weight := FW_BOLD
  else
    Weight := FW_NORMAL;

  FNT := CreateFontW(-MulDiv(Size, GetDeviceCaps(DC, LOGPIXELSY), 72), 0,
                      0, 0, Weight, Byte(Italic), 0, 0, DEFAULT_CHARSET, 0, 0,
                      ANTIALIASED_QUALITY, 0, PWideChar(Face));
  if FNT = 0 then
    Exit;

  MDC := CreateCompatibleDC(DC);
  SelectObject(MDC, FNT);
  GetTextMetricsW(MDC, @TM);
  MaxWidth  := TM.tmMaxCharWidth;
  MaxHeight := TM.tmHeight;
  CharData  := GetMemory(MaxWidth * MaxHeight * 4);

  BMP := CreateCompatibleBitmap(DC, MaxWidth, MaxHeight);
// Fill Black
  SelectObject(MDC, BMP);
  SetBkMode(MDC, TRANSPARENT);
  SetTextColor(MDC, $FFFFFF);

  ZeroMemory(@BI, SizeOf(BI));
  with BI.bmiHeader do
  begin
    biSize      := SizeOf(BI.bmiHeader);
    biWidth     := MaxWidth;
    biHeight    := MaxHeight;
    biPlanes    := 1;
    biBitCount  := 32;
    biSizeImage := biWidth * biHeight * biBitCount div 8;
  end;

  ReleaseDC(0, DC);
end;

procedure TFontGenerator.GenFree;
begin
  if TexData <> nil then
  begin
    FreeMemory(TexData);
    TexData := nil;;
  end;
  FreeMemory(CharData);
  DeleteObject(BMP);
  DeleteObject(FNT);
  DeleteDC(MDC);
end;

function TFontGenerator.GenChar(c: WideChar): TFontChar;

  function ScanLine(Offset: LongInt; Vert: Boolean): Boolean;
  var
    i, c, d : LongInt;
    p : ^Byte;
  begin
    if Vert then
    begin
      p := @CharData[Offset * 4];
      c := MaxWidth * 4;
      d := MaxHeight;
    end else
    begin
      p := @CharData[Offset * MaxWidth * 4];
      c := 4;
      d := MaxWidth;
    end;

    for i := 0 to d - 1 do
      if p^ <> 0 then
      begin
        Result := True;
        Exit;
      end else
        Inc(Integer(p), c);

    Result := False;
  end;

var
  i, x, y : LongInt;
  CharRect : TRect;
  Size : Windows.TSize;
begin
  CharRect := Recti(0, 0, MaxWidth, MaxHeight);
  FillRect(MDC, Windows.TRect(CharRect), GetStockObject(BLACK_BRUSH));
  TextOutW(MDC, 0, 0, @c, 1);

// get char bitmap data
  GDIFlush;
  GetDIBits(MDC, BMP, 0, MaxHeight, CharData, BI, DIB_RGB_COLORS);

  GetTextExtentPoint32W(MDC, @c, 1, Size);

  CharRect := Recti(0, 0, Size.cx, 1);
// Top
  for y := 0 to MaxHeight - 1 do
    if ScanLine(MaxHeight - y - 1, False) then
    begin
      CharRect.Top := y - 1;
      break;
    end;
// Bottom
  for y := MaxHeight - 1 downto 0 do
    if ScanLine(MaxHeight - y - 1, False) then
    begin
      CharRect.Bottom := y + 1;
      break;
    end;
// Left
  for x := 0 to MaxWidth - 1 do
    if ScanLine(x, True) then
    begin
      CharRect.Left := x - 1;
      break;
    end;
// Right
  for x := MaxWidth - 1 downto 0 do
    if ScanLine(x, True) then
    begin
      CharRect.Right := x + 1;
      break;
    end;

// get char trimmed bitmap data
  Result := TFontChar.Create;
  Result.ID      := c;
  Result.OffsetY := CharRect.Top;
  Result.Width   := CharRect.Right - CharRect.Left + 1;
  Result.Height  := CharRect.Bottom - CharRect.Top + 1;
  Result.Data    := GetMemory(Result.Width * Result.Height);
  i := 0;
  for y := CharRect.Top to CharRect.Bottom do
    for x := CharRect.Left to CharRect.Right do
    begin
      Result.Data[i] := CharData[((MaxHeight - y - 1) * MaxWidth + x) * 4];
      Inc(i);
    end;
end;

procedure TFontGenerator.AddChar(c: WideChar);
var
  f : TFontChar;
  i, j : LongInt;
begin
  f := GenChar(c);
  SetLength(FontChar, Length(FontChar) + 1);
  j := Length(FontChar) - 1;
  for i := 0 to Length(FontChar) - 2 do
    with FontChar[i] do
      if Width * Height < f.Width * f.Height then
      begin
        for j := Length(FontChar) - 1 downto i + 1 do
          FontChar[j] := FontChar[j - 1];
        j := i;
        break;
      end;
  FontChar[j] := f;
  FontData[f.ID] := f;
end;

procedure TFontGenerator.AddChars(str: WideString);
var
  i : LongInt;
begin
  for i := 1 to Length(str) do
    AddChar(str[i]);
end;

procedure TFontGenerator.ClearChars;
var
  i : LongInt;
begin
  for i := 0 to Length(FontChar) - 1 do
  begin
    FreeMemory(FontChar[i].Data);
    FontChar[i].Free;
  end;
  SetLength(FontChar, 0);
end;

function TFontGenerator.PackChars: LongInt;
var
  i, j : LongInt;
  Node : TFontNode;

  function Pack(Idx: LongInt): Boolean;
  var
    i, ix, iy : LongInt;
  begin
    i := 0;
    with FontChar[Idx] do
      if Node.Insert(Width + 1, Height + 1, PosX, PosY) then
      begin
        for iy := PosY to PosY + Height - 1 do
          for ix := PosX to PosX + Width - 1 do
          begin
            TexData[(iy * TexWidth + ix) * 2 + 1] := Data[i]; // write alpha value
            Inc(i);
          end;
        Result := True;
      end else
        Result := False;
  end;

begin
// Get minimum texture area
  j := 0;
  for i := 0 to Length(FontChar) - 1 do
    Inc(j, FontChar[i].Width * FontChar[i].Height);
  j := ToPow2(Round(sqrt(j)));

  TexWidth  := j;
  TexHeight := j div 2;

  Result := Length(FontChar);

  while TexHeight < 4096 do
  begin
    if TexData <> nil then
      FreeMemory(TexData);

    TexData := GetMemory(TexWidth * TexHeight * 2);

  // fill texture data with default values
    for i := 0 to TexWidth * TexHeight - 1 do
    begin
      TexData[i * 2 + 0] := 255; // luminance
      TexData[i * 2 + 1] := 0;   // alpha
    end;

    Result := Length(FontChar);
    Node := TFontNode.Create(Recti(1, 1, TexWidth, TexHeight));
    for i := 0 to Length(FontChar) - 1 do
      if Pack(i) then
        Dec(Result)
      else
        break;
    Node.Free;

    if Result = 0 then
      break;

    if TexHeight < TexWidth then
      TexHeight := TexHeight * 2
    else
      TexWidth := TexWidth * 2;
  end;

  if Result > 0 then
    Writeln('Can''t pack ', Result, ' chars');
end;
(*
procedure TFontGenerator.Save(const FileName: string);
const
  DDSD_CAPS        = $0001;
  DDSD_HEIGHT      = $0002;
  DDSD_WIDTH       = $0004;
  DDSD_PIXELFORMAT = $1000;
  DDSCAPS_TEXTURE  = $1000;
  DDPF_ALPHAPIXELS = $0001;
  DDPF_LUMINANCE   = $20000;
var
  i : LongInt;
  FileChar : record
    ID   : WideChar;
    py   : Word;
    w, h : Word;
    tx, ty, tw, th : Single;
  end;

  DDS : record
    dwMagic       : LongWord;
    dwSize        : LongInt;
    dwFlags       : LongWord;
    dwHeight      : LongWord;
    dwWidth       : LongWord;
    dwPOLSize     : LongWord;
    dwDepth       : LongWord;
    dwMipMapCount : LongInt;
    FontMagic   : LongWord;
    FontOffset  : LongWord;
    SomeData1   : array [0..8] of LongWord;
    pfSize      : LongWord;
    pfFlags     : LongWord;
    pfFourCC    : LongWord;
    pfRGBbpp    : LongWord;
    pfRMask     : LongWord;
    pfGMask     : LongWord;
    pfBMask     : LongWord;
    pfAMask     : LongWord;
    dwCaps1     : LongWord;
    dwCaps2     : LongWord;
    SomeData3   : array [0..2] of LongWord;
  end;

  Stream : TglrStream;
begin
  Stream := TglrStream.Init(FileName + '.dds', True);
// save dds data
  FillChar(DDS, SizeOf(DDS), 0);
  with DDS do
  begin
    dwMagic    := $20534444;
    dwSize     := SizeOf(DDS) - 4; // -4 = Magic size
    dwFlags    := DDSD_CAPS or DDSD_HEIGHT or DDSD_WIDTH or DDSD_PIXELFORMAT;
    dwHeight   := TexHeight;
    dwWidth    := TexWidth;
    dwPOLSize  := dwWidth * 2;
    dwCaps1    := DDSCAPS_TEXTURE;
    FontMagic  := $2B6E6678;
    FontOffset := SizeOf(DDS) + dwWidth * dwHeight * 2;
    pfSize     := 32;
    pfFlags    := DDPF_LUMINANCE or DDPF_ALPHAPIXELS;
    pfRGBbpp   := 16;
    pfRMask    := $00FF;
    pfAMask    := $FF00;
  end;
  Stream.Write(DDS, SizeOf(DDS));
  Stream.Write(TexData^, DDS.dwWidth * DDS.dwHeight * 2);

  if Stream <> nil then
  begin
    i := Length(FontChar);
    Stream.Write(i, SizeOf(i));

    for i := 0 to Length(FontChar) - 1 do
    begin
      with FontChar[i], FileChar do
      begin
        py := OffsetY;
        tx := PosX / TexWidth;
        ty := PosY / TexHeight;
        tw := Width / TexWidth;
        th := Height / TexHeight;
        w  := Width;
        h  := Height;
      end;
      FileChar.ID := FontChar[i].ID;
      Stream.Write(FileChar, SizeOf(FileChar));
    end;

    Stream.Free;
  end;
end;
*)
procedure TFontGenerator.SaveBmpToFile(const FileName: AnsiString);
var
  Stream: TFileStream;
  fh: BITMAPFILEHEADER;
  ih: BITMAPINFOHEADER;
  texdata2: PByteArray;
  i: LongWord;

  FileChar : record
    ID   : WideChar;
    py   : Word;
    w, h : Word;
    tx, ty, tw, th : Single;
  end;
begin
  Stream := TFileStream.Create(FileName + '.bmp', fmCreate);
  FillChar(fh, SizeOf(BITMAPFILEHEADER), 0);
  FillChar(ih, SizeOf(BITMAPINFOHEADER), 0);

  fh.bfType := $4D42;
  fh.bfReserved1 := $0F86;
  ih.biSize := SizeOf(BITMAPINFOHEADER);
  ih.biWidth := TexWidth;
  ih.biHeight := -TexHeight;
  ih.biPlanes := 1;
  ih.biBitCount := 32;
  ih.biSizeImage := TexWidth * TexHeight * 4;

  Stream.Write(fh, SizeOf(BITMAPFILEHEADER));
  Stream.Write(ih, SizeOf(BITMAPINFOHEADER));

  texdata2 := GetMemory(TexWidth * TexHeight * 4);
  FillChar(texdata2^, TexWidth * TexHeight * 4, 255);
  for i := 0 to (TexWidth * TexHeight - 1) do
    texdata2^[i * 4 + 3] := TexData^[i * 2 + 1];
  Stream.Write(texdata2^, TexWidth * TexHeight * 4);
  FreeMemory(texdata2);

  i := Length(FontChar);
  Stream.Write(i, SizeOf(i));
  for i := 0 to Length(FontChar) - 1 do
  begin
    with FontChar[i], FileChar do
    begin
      py := OffsetY;
      tx := PosX / TexWidth;
      ty := PosY / TexHeight;
      tw := Width / TexWidth;
      th := Height / TexHeight;
      w  := Width;
      h  := Height;
    end;
    FileChar.ID := FontChar[i].ID;
    Stream.Write(FileChar, SizeOf(FileChar));
  end;

  Stream.Free();
end;

procedure TFontGenerator.SaveFntToFile(const FileName: AnsiString);
var
  Stream: TFileStream;
  texdata2: PByteArray;
  i: LongWord;

  FileChar : record
    ID   : WideChar;
    py   : Word;
    w, h : Word;
    tx, ty, tw, th : Single;
  end;

  Header : record
    Width  : Word;
    Height : Word;
    Size   : LongWord;
  end;

begin
  Stream := TFileStream.Create(FileName + '.fnt', fmCreate);

  Header.Width  := TexWidth;
  Header.Height := TexHeight;
  Header.Size   := TexWidth * TexHeight;

  texdata2 := GetMemory(Header.Size);
  FillChar(texdata2^, Header.Size, 0);
  for i := 0 to (TexWidth * TexHeight - 1) do
    texdata2^[i] := TexData^[i * 2 + 1];

  Stream.Write(Header, SizeOf(Header));
  Stream.Write(texdata2^, Header.Size);

  i := Length(FontChar);
  Stream.Write(i, SizeOf(i));
  for i := 0 to Length(FontChar) - 1 do
  begin
    with FontChar[i], FileChar do
    begin
      py := OffsetY;
      tx := PosX / TexWidth;
      ty := PosY / TexHeight;
      tw := Width / TexWidth;
      th := Height / TexHeight;
      w  := Width;
      h  := Height;
    end;
    FileChar.ID := FontChar[i].ID;
    Stream.Write(FileChar, SizeOf(FileChar));
  end;

  FreeMemory(texdata2);
  Stream.Free();
end;

function TFontGenerator.SaveBmpToStream: TStream;
var
  fh: BITMAPFILEHEADER;
  ih: BITMAPINFOHEADER;
  texdata2: PByteArray;
  i: Integer;
  size: LongInt;
begin
  Result := TMemoryStream.Create();
  size := SizeOf(BITMAPFILEHEADER) + SizeOf(BITMAPINFOHEADER) + TexWidth * TexHeight * 4;
  (Result as TMemoryStream).SetSize(size);
  FillChar(fh, SizeOf(BITMAPFILEHEADER), 0);
  FillChar(ih, SizeOf(BITMAPINFOHEADER), 0);

  fh.bfType := $4D42;
  fh.bfReserved1 := $0F86;
  ih.biSize := SizeOf(BITMAPINFOHEADER);
  ih.biWidth := TexWidth;
  ih.biHeight := -TexHeight;
  ih.biPlanes := 1;
  ih.biBitCount := 32;
  ih.biSizeImage := TexWidth * TexHeight * 4;

  Result.Write(fh, SizeOf(BITMAPFILEHEADER));
  Result.Write(ih, SizeOf(BITMAPINFOHEADER));

  texdata2 := GetMemory(TexWidth * TexHeight * 4);
  FillChar(texdata2^, TexWidth * TexHeight * 4, 255);
  for i := 0 to (TexWidth * TexHeight - 1) do
    texdata2^[i * 4 + 3] := TexData^[i * 2 + 1];
  Result.Write(texdata2^, TexWidth * TexHeight * 4);
  FreeMemory(texdata2);
end;

{$ENDREGION}

end.

