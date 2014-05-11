program pack;

uses
  sys_win,
  tinyglr;

  procedure PackData(const aOutputFileName, aPackDir: AnsiString);
  var
    files: TglrStringList;
    i: Integer;
    PackStream, FileStream: TglrStream;
    buffer: Pointer;
    headers: array of TglrPackFileResource;
    bytesRead, stride: LongWord;
    count: Word;
  begin
    Log.Write(lInformation, 'Start packing dir "' + aPackDir + '" into pack file "' + aOutputFileName + '"');
    files := TglrStringList.Create(1);
    FindFiles(aPackDir, '', files);
    i := 0;
    while (i < files.Count) do
    begin
      if ExtractFileExt(files[i]) = '.glrpack' then
        files.DeleteSafeByIndex(i)
      else
        i += 1;
    end;
    count := files.Count;
    Log.Write(lInformation, 'Files found: ' + Convert.ToStringA(count));

    PackStream := TglrStream.Init(aOutputFileName, True);
    PackStream.Write(PACK_FILE_MAGIC, SizeOf(Word));
    PackStream.Write(count, SizeOf(Word));

    SetLength(headers, count);
    //Skip header data - we will write it later
    PackStream.Pos := 2 * SizeOf(Word) + SizeOf(TglrPackFileResource) * count;
    stride := 0;

    for i := 0 to count - 1 do
    begin
      Log.Write(lInformation, 'Start packing "' + files[i] + '"...');
      FileStream := TglrStream.Init(files[i]);

      GetMem(buffer, FileStream.Size);
      bytesRead := FileStream.Read(buffer^, FileStream.Size);
      if (bytesRead <> LongWord(FileStream.Size)) then
        Log.Write(lCritical, 'Error while read file "' + files[i] + '". Number of total bytes read not equal to file size');
      PackStream.Write(buffer, FileStream.Size);
      FreeMem(buffer, FileStream.Size);

      headers[i].fFileName := files[i];
      headers[i].fSize := LongWord(FileStream.Size);
      headers[i].fStride := stride;

      stride += LongWord(FileStream.Size);
      FileStream.Free();
      Log.Write(lInformation, '... success!');
    end;

    Log.Write(lInformation, 'Start writing headers...');
    PackStream.Pos := 2 * SizeOf(Word);
    for i := 0 to count - 1 do
    begin
      PackStream.WriteAnsi(headers[i].fFileName);
      PackStream.Write(headers[i].fStride, SizeOf(LongWord));
      PackStream.Write(headers[i].fSize, SizeOf(LongWord));
    end;
    Log.Write(lInformation, '... success!');
//    PackStream.Write(headers, SizeOf(TglrPackFileResource) * count);
    PackStream.Free();
  end;

begin
  Log.Init('pack.log');

//  PackData('data/p1.glrpack', 'data');
  if ParamStr(1) = '-pack' then
    PackData(ParamStr(2), ParamStr(3));

  Log.Deinit();
end.

