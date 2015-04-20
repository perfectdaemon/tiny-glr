program pack;

uses
  glr_utils,
  glr_filesystem,
  glr_os_win;

  procedure WriteInfo(info: AnsiString);
  begin
    WriteLn(info);
    Log.Write(lInformation, info);
  end;

  procedure WriteError(error: AnsiString);
  begin
    WriteLn(error);
    Log.Write(lCritical, error);
    Halt(1);
  end;

  procedure PackData(const inputDir, outputFileName: AnsiString; useLZO: Boolean);
  var
    files: TglrStringList;
    PackStream, FileStream: TglrStream;

    buffer, compressedBuffer: Pointer;
    headers: array of TglrPackFileResource;

    bytesRead, stride: LongInt;
    count: Word;
    wordBuf: Word;
    compressedSize: LongInt;

    info: String;

    i: Integer;
  begin
    // Logging start
    info := 'Start packing dir "' + inputDir + '" into pack file "' + outputFileName + '"';
    if (useLZO) then
      info += ' using LZO compression';
    WriteInfo(info);

    // Get files from input dir excluding .glrpack-files
    files := TglrStringList.Create();
    FindFiles(inputDir, '', files);
    i := 0;
    while (i < files.Count) do
    begin
      if ExtractFileExt(files[i]) = PACK_FILE_EXT then
        files.DeleteSafeByIndex(i)
      else
        i += 1;
    end;
    count := files.Count;
    WriteInfo('Files found: ' + Convert.ToString(count));

    SetLength(headers, count);

    // Create output stream and write magic header and file count
    PackStream := TglrStream.Init(outputFileName, True);
    if (useLZO) then
      wordBuf := PACK_FILE_MAGIC_LZO
    else
      wordBuf := PACK_FILE_MAGIC;
    PackStream.Write(wordBuf, SizeOf(Word));
    PackStream.Write(count, SizeOf(Word));

    // Skip header data - we will write it later
    PackStream.Pos := 2 * SizeOf(Word) + SizeOf(TglrPackFileResource) * count;
    stride := PackStream.Pos;

    // Write file data itself
    for i := 0 to count - 1 do
    begin
      WriteInfo('Start packing "' + files[i] + '"...');
      FileStream := TglrStream.Init(files[i]);

      // Read file into buffer
      buffer := GetMem(FileStream.Size);
      bytesRead := FileStream.Read(buffer^, FileStream.Size);
      if (bytesRead <> FileStream.Size) then
        WriteError('Error occured while reading file "' + files[i] + '". Number of total bytes read is not equal to file size');

      // if lzo compression is not used just copy pointer
      if (not useLZO) then
      begin
        compressedBuffer := buffer;
        compressedSize := FileStream.Size;
      end
      else
      begin
        compressedBuffer := GetMem(FileStream.Size);
        CompressData(buffer, FileStream.Size, compressedBuffer, compressedSize);
        FreeMem(buffer);
      end;

      // Write data (compressed or not) info output stream
      PackStream.Write(compressedBuffer^, compressedSize);
      FreeMem(compressedBuffer);

      headers[i].FileName := files[i];
      headers[i].CompressedSize := compressedSize;
      headers[i].OriginalSize := FileStream.Size;
      headers[i].Stride := stride;

      stride += headers[i].CompressedSize;
      FileStream.Free();
      WriteInfo('... success!');
    end;

    // Write header data
    WriteInfo('Start writing headers...');
    PackStream.Pos := 2 * SizeOf(Word);
    for i := 0 to count - 1 do
    begin
      PackStream.WriteAnsi(headers[i].FileName);
      PackStream.Write(headers[i].Stride, SizeOf(LongInt));
      PackStream.Write(headers[i].CompressedSize, SizeOf(LongInt));
      PackStream.Write(headers[i].OriginalSize, SizeOf(LongInt));
    end;
    WriteInfo('... success!');
    PackStream.Free();
  end;

  procedure PackFileLZO(const inputFileName, outputFileName: AnsiString);
  var
    inputStream, outputStream: TglrStream;
    mIn, mOut: Pointer;
    compressedSize: LongInt;
  begin
    WriteInfo('Start packing file "' + inputFileName + '" into LZO file "' + outputFileName + '"');
    if (not FileExists(inputFileName)) then
    begin
      WriteError('File "' + inputFileName + '" not found');
      Exit();
    end;

    inputStream := TglrStream.Init(inputFileName);
    outputStream := TglrStream.Init(outputFileName, True);

    mIn := GetMem(inputStream.Size);
    mOut := GetMem(inputStream.Size);

    inputStream.Read(mIn^, inputStream.Size);

    CompressData(mIn, inputStream.Size, mOut, compressedSize);

    // Structure:
    // - 2 bytes magic
    // - 4 bytes original size
    // - file itself
    outputStream.Write(PACK_FILE_MAGIC_LZO, SizeOf(Word));
    outputStream.Write(inputStream.Size, SizeOf(LongInt));
    outputStream.Write(mOut^, compressedSize);

    inputStream.Free();
    outputStream.Free();
    FreeMemory(mIn);
    FreeMemory(mOut);
    WriteInfo('... success!');
  end;


type
  TCommandLineFlag = (kLZO);

const
  FLAG_NAMES: array[Low(TCommandLineFlag)..High(TCommandLineFlag)] of String =
    ('-lzo');

var
  command : String; // First param, command name
  input   : String; // Second param, usually original file/folder
  output  : String; // Third param, new pack name / file name

  flags: array[Low(TCommandLineFlag)..High(TCommandLineFlag)] of Boolean =
    (False);

  // Read and parse param string array
  procedure ReadParams();
  var
    i: Integer;
    j: TCommandLineFlag;
  begin
    if (Paramcount > 0) then
      command := ParamStr(1)
    else
      WriteError('Command line params not found');

    if (Paramcount > 1) then
      input := ParamStr(2);
    if (Paramcount > 2) then
      output := ParamStr(3);

    // looking for flags
    if (Paramcount > 3) then
      for i := 4 to Paramcount do
        for j := Low(TCommandLineFlag) to High(TCommandLineFlag) do
          if ParamStr(i) = FLAG_NAMES[j] then
          begin
            flags[j] := True;
            break;
          end;
  end;

begin
  Log.Init('pack.log');

  ReadParams();

  if (command = '-pack') then
    PackData(input, output, flags[kLZO])

  else if (command = '-file') then
    PackFileLZO(input, output);

  Log.Deinit();
end.

