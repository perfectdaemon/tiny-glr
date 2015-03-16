unit glr_filesystem;

{$i defines.inc}

interface

uses
  glr_utils;

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

    class function GetFileIndexInPackFile(packIndex: Integer; aFileName: AnsiString): Integer;
    class function GetPackIndexByPackName(const aPackName: AnsiString): Integer;
  public
    class procedure Init(const aPackFilesPath: AnsiString);
    class procedure DeInit();

    class procedure LoadPack(const aPackFileName: AnsiString); //loads entire pack file into memory
    class procedure UnloadPack(const aPackFileName: AnsiString);
    class function ReadResource(const aFileName: AnsiString; aSearchInPackFiles: Boolean = True): TglrStream;
    class function ReadResourceLZO(const aFileName: AnsiString; aSearchInPackFiles: Boolean = True): TglrStream;
    class procedure WriteResource(const aFileName: AnsiString; const aStream: TglrStream); overload;
    class procedure WriteResource(const aFileName: AnsiString; const aContent: AnsiString); overload;
  end;

  procedure CompressData(const InData: Pointer; InSize: LongInt; out OutData: Pointer; out OutSize: LongInt);
  procedure DecompressData(const InData: Pointer; InSize: LongInt; const OutData: Pointer; var OutSize: LongInt);

implementation

{$IFDEF WIN32}
uses
  glr_os_win;
{$ENDIF}

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
    fPackFiles[i].fPackDataPointer := nil;
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
          bytesRead := PackFile.Read(m^, Result.Size); //write directly, no need of Write (Read);
          PackFile.Free();

          if (bytesRead <> Result.Size) then
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

end.

