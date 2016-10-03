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

  TglrPackFileResource = record
    FileName: NameString;
    Stride, CompressedSize, OriginalSize: LongInt;
  end;

  FileSystem = class
  protected
    type
      { TglrPackFile }
      TglrPackFile = record
        PackName: AnsiString;
        Files: array of TglrPackFileResource;
        Loaded, LZO: Boolean;
        PackData: TglrStream;
        PackDataPointer: Pointer;

        procedure Load();
        procedure Unload();

        function GetFileIndex(FileName: AnsiString): Integer;
        function ReadResource(FileIndex: Integer): TglrStream;
      end;

    var
      class var fPackFilesPath: AnsiString;
      class var fPackFiles: array of TglrPackFile;

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

uses
  SynLZO
{$IFDEF WIN64}
  ,glr_os_win;
{$ELSEIF}
{$ENDIF}

{ FileSystem }

function lzo_compress(const Data; Size: LongInt; var CData; var CSize: LongInt; var WorkBuf): LongInt; cdecl;

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

{ FileSystem.TglrPackFile }

procedure FileSystem.TglrPackFile.Load;
var
  FileStream: TglrStream;
begin
  FileStream := TglrStream.Init(PackName);
  PackDataPointer := GetMem(FileStream.Size);
  PackData := TglrStream.Init(PackDataPointer, FileStream.Size, True);
  PackData.CopyFrom(FileStream);

  Loaded := True;

  FileStream.Free();
end;

procedure FileSystem.TglrPackFile.Unload;
begin
  PackData.Free();
  PackData := nil;
  PackDataPointer := nil;
  Loaded := False;
end;

function FileSystem.TglrPackFile.GetFileIndex(FileName: AnsiString): Integer;
var
  i: Integer;
begin
  Result := -1;

  for i := 0 to High(Files) do
    if (Files[i].FileName = FileName) then
      Exit(i);
end;

function FileSystem.TglrPackFile.ReadResource(FileIndex: Integer): TglrStream;
var
  PackFile: TglrStream;
  compressedBuffer, uncompressedBuffer: Pointer;
  bytesRead: LongInt;
begin
  // Load pack file from PackData (memory), uncompress if necessary
  if (Loaded) then
  begin
    if (LZO) then
    begin
      // Read compressed data from pack
      compressedBuffer := GetMem(Files[FileIndex].CompressedSize);
      PackData.Pos := Files[FileIndex].Stride;
      PackData.Read(compressedBuffer^, Files[FileIndex].CompressedSize);

      uncompressedBuffer := GetMem(Files[FileIndex].OriginalSize);

      DecompressData(compressedBuffer, Files[FileIndex].CompressedSize,
        uncompressedBuffer, bytesRead);

      if (bytesRead <> Files[FileIndex].OriginalSize) then
        Log.Write(lError, 'FileSystem: error occured while decompressing LZO compressed resource "'
          + Files[FileIndex].FileName + '" from pack file "' + PackName + '"');

      FreeMem(compressedBuffer);
      Result := TglrStream.Init(uncompressedBuffer, bytesRead, True);
    end
    else
      Result := TglrStream.Init(PackDataPointer + Files[FileIndex].Stride,
        Files[FileIndex].CompressedSize, False);
  end

  // Read pack file, seek to requested file's stride, read it into new Stream,
  // uncompress if necessary
  else
  begin
    PackFile := TglrStream.Init(PackName);
    PackFile.Pos := Files[FileIndex].Stride;
    compressedBuffer := GetMem(Files[FileIndex].CompressedSize);
    bytesRead := PackFile.Read(compressedBuffer^, Files[FileIndex].CompressedSize);
    PackFile.Free();

    if (bytesRead <> Files[FileIndex].CompressedSize) then
      Log.Write(lError, 'FileSystem: error occured while reading resource "'
          + Files[FileIndex].FileName + '" from pack file "' + PackName + '"');

    if (LZO) then
    begin
      uncompressedBuffer := GetMem(Files[FileIndex].OriginalSize);

      DecompressData(compressedBuffer, Files[FileIndex].CompressedSize,
        uncompressedBuffer, bytesRead);

      if (bytesRead <> Files[FileIndex].OriginalSize) then
        Log.Write(lError, 'FileSystem: error occured while decompressing LZO compressed resource "'
          + Files[FileIndex].FileName + '" from pack file "' + PackName + '"');

      FreeMem(compressedBuffer);
    end
    else
      uncompressedBuffer := compressedBuffer;

    Result := TglrStream.Init(uncompressedBuffer, bytesRead, True);
  end;
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

    // Read magic header
    bytesRead := stream.Read(WordBuf, SizeOf(Word));
    if ((WordBuf <> PACK_FILE_MAGIC) and (WordBuf <> PACK_FILE_MAGIC_LZO)) or (bytesRead < SizeOf(Word)) then
    begin
      Log.Write(lError, #9 + packFilesList[i] + ' is not a correct pack file');
      SetLength(fPackFiles, Length(fPackFiles) - 1);
      stream.Free();
      continue;
    end;

    // Set main params for record
    fPackFiles[l].PackName := packFilesList[i];
    fPackFiles[l].Loaded := False;
    fPackFiles[l].LZO := (WordBuf = PACK_FILE_MAGIC_LZO);

    // Read files count
    bytesRead := stream.Read(WordBuf, SizeOf(Word));
    if (bytesRead <> SizeOf(Word)) then
    begin
      log.Write(lError, #9 + packFilesList[i] + ': error occured while reading files count');
      SetLength(fPackFiles, Length(fPackFiles) - 1);
      stream.Free();
      continue;
    end;

    // Read file headers: name, stride and sizes (compressed and original)
    SetLength(fPackFiles[l].Files, WordBuf);
    for j := 0 to WordBuf - 1 do
    begin
      fPackFiles[l].Files[j].FileName := stream.ReadAnsi();
      stream.Read(fPackFiles[l].Files[j].Stride, SizeOf(LongInt));
      stream.Read(fPackFiles[l].Files[j].CompressedSize, SizeOf(LongInt));
      stream.Read(fPackFiles[l].Files[j].OriginalSize, SizeOf(LongInt));
    end;

    Log.Write(lInformation, #9 + packFilesList[i] + ': header loaded. Files inside: ' + Convert.ToString(WordBuf));
    for j := 0 to Length(fPackFiles[l].Files) - 1 do
      Log.Write(lInformation,
      #9#9 + fPackFiles[l].Files[j].FileName
        + #9#9' - c ' + Convert.ToString(Integer(fPackFiles[l].Files[j].CompressedSize)) + ' bytes'
        + #9#9' - o ' + Convert.ToString(Integer(fPackFiles[l].Files[j].OriginalSize)) + ' bytes');
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
    if (fPackFiles[i].Loaded) then
      fPackFiles[i].Unload();
  SetLength(fPackFiles, 0);
end;

class function FileSystem.GetPackIndexByPackName(const aPackName: AnsiString): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Length(fPackFiles) - 1 do
    if (fPackFiles[i].PackName = aPackName) then
      Exit(i);
end;

class procedure FileSystem.LoadPack(const aPackFileName: AnsiString);
var
  i: Integer;
begin
  i := GetPackIndexByPackName(aPackFileName);
  if (i = -1) then
    Log.Write(lError, 'FileSystem: Unable to load pack "' + aPackFileName + '". No pack was found.')
  else
  begin
    fPackFiles[i].Load();
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
    fPackFiles[i].Unload();
    Log.Write(lInformation, 'FileSystem: Pack unload "' + aPackFileName + '" is completed');
  end;
end;

class function FileSystem.ReadResource(const aFileName: AnsiString;
  aSearchInPackFiles: Boolean): TglrStream;
var
  i, fileIndex: Integer;
begin
  if (FileExists(aFileName)) then
  begin
    // ToDo: load directly into memory?
    Log.Write(lInformation, 'FileSystem: start reading resource "' + aFileName + '" directly from file');
    Result := TglrStream.Init(aFileName);
    Log.Write(lInformation, 'FileSystem: read successfully');
    Exit();
  end

  // Try read from pack files
  else if (aSearchInPackFiles) then
    for i := 0 to Length(fPackFiles) - 1 do
    begin
      fileIndex := fPackFiles[i].GetFileIndex(aFileName);
      if (fileIndex <> -1) then
      begin
        Log.Write(lInformation, 'FileSystem: start reading resource "' + aFileName + '" from pack file "' + fPackFiles[i].PackName + '"');
        Result := fPackFiles[i].ReadResource(fileIndex);
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
  originalSize, outSize: LongInt;
  wordBuf: Word;
  i, fileIndex: Integer;
begin
  if (FileExists(aFileName)) then
  begin
    Log.Write(lInformation, 'FileSystem: start reading LZO resource "' + aFileName + '" directly from file');
    fileStream := TglrStream.Init(aFileName);

    // Read magic
    fileStream.Read(wordBuf, SizeOf(Word));
    if (wordBuf <> PACK_FILE_MAGIC_LZO) then
      Log.Write(lCritical, 'FileSystem: resource "' + aFileName + '" is not LZO, no magic found');

    // Read original size
    fileStream.Read(originalSize, SizeOf(LongInt));

    // Prepare buffers
    mIn  := GetMem(fileStream.Size - fileStream.Pos);
    mOut := GetMem(originalSize);

    // Read compressed data
    fileStream.Read(mIn^, fileStream.Size);

    DecompressData(mIn, fileStream.Size, mOut, outSize);

    if (outSize <> originalSize) then
      Log.Write(lError, 'FileSystem: error occured while decompressing LZO compressed resource "'
          + aFileName);

    Result := TglrStream.Init(mOut, outSize, True);

    fileStream.Free();
    FreeMem(mIn);
    Log.Write(lInformation, 'FileSystem: read successfully');
    Exit();
  end

  // Try read from pack files
  else if (aSearchInPackFiles) then
    for i := 0 to Length(fPackFiles) - 1 do
    begin
      fileIndex := fPackFiles[i].GetFileIndex(aFileName);
      if (fileIndex <> -1) then
      begin
        Log.Write(lInformation, 'FileSystem: start reading resource "' + aFileName + '" from pack file "' + fPackFiles[i].PackName + '"');
        Result := fPackFiles[i].ReadResource(fileIndex);
        Log.Write(lInformation, 'FileSystem: read successfully');
        Exit();
      end;
    end;

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

