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
    function SaveBmpToStream(): TStream;
  end;

procedure CompressData(const InData: Pointer; InSize: LongInt; out OutData: Pointer; out OutSize: LongInt);
procedure DecompressData(const InData: Pointer; InSize: LongInt; const OutData: Pointer; var OutSize: LongInt);

implementation

uses
  tinyglr in '..\..\engine\tinyglr.pas';

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
      CharRect.Top := y;
      break;
    end;
// Bottom
  for y := MaxHeight - 1 downto 0 do
    if ScanLine(MaxHeight - y - 1, False) then
    begin
      CharRect.Bottom := y;
      break;
    end;
// Left
  for x := 0 to MaxWidth - 1 do
    if ScanLine(x, True) then
    begin
      CharRect.Left := x;
      break;
    end;
// Right
  for x := MaxWidth - 1 downto 0 do
    if ScanLine(x, True) then
    begin
      CharRect.Right := x;
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
    Node := TFontNode.Create(Recti(0, 0, TexWidth, TexHeight));
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
  Stream: TglrStream;
  fh: BITMAPFILEHEADER;
  ih: BITMAPINFOHEADER;
  texdata2: PByteArray;
  i: LongInt;

  FileChar : record
    ID   : WideChar;
    py   : Word;
    w, h : Word;
    tx, ty, tw, th : Single;
  end;
begin
  Stream := TglrStream.Init(FileName + '.bmp', True);
  FillChar(fh, SizeOf(BITMAPFILEHEADER), 0);
  FillChar(ih, SizeOf(BITMAPINFOHEADER), 0);

  fh.bfType := $4D42;
  fh.bfReserved1 := $0F86;
  ih.biSize := SizeOf(BITMAPINFOHEADER);
  ih.biWidth := TexWidth;
  ih.biHeight := -TexHeight;
  ih.biPlanes := 1;
  ih.biBitCount := 32;

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

function lzo_compress(const Data; Size: LongInt; var CData; var CSize: LongInt; var WorkBuf): LongInt; cdecl;
asm
//{$IFDEF WIN32}
//  jmp lzo_compress + $2F0 + 8 + 3
//{$ELSE}
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

end.

