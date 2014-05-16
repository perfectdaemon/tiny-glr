program sumcode;

uses
  sysutils;

  function IsInFilter(const aFileName: AnsiString): Boolean;
  var
    i: Integer;
  begin
    for i := 0 to Paramcount - 1 do
      if LowerCase(aFileName) = LowerCase(ParamStr(i)) then
        Exit(True);
    Exit(False);
  end;

  var
  Size, Lines: Integer;

function GetLinesTotal(aPath: AnsiString): Integer;
var
  sr: TSearchRec;
  F: TextFile;
  i: Integer;
  ext: AnsiString;
begin
  Result := 0;
  if (aPath <> '') then
    aPath += '\';
  if FindFirst(aPath + '*', faAnyFile, sr) = 0 then
    repeat
      if (sr.Name <> '.') and (sr.Name <> '..') and DirectoryExists(aPath + sr.Name) then
        Result := Result + GetLinesTotal(aPath + sr.Name)
      else
      begin
        ext := LowerCase(ExtractFileExt(sr.Name));
        if ((ext = '.pas') or (ext = '.dpr') or (ext = '.inc') or (ext = '.lpr') or (ext = '.pp'))
          and not (IsInFilter(ExtractFileName(sr.Name))) then
        begin
          Size += sr.Size;
          AssignFile(F, aPath + sr.Name);
          Reset(F);
          i := 0;
          while not eof(F) do
          begin
            Readln(F);
            i += 1;
          end;
          Writeln('- ', sr.Name, #9#9#9, i, ' lines', #9, sr.Size, ' bytes');
          Result += i;
          CloseFile(F);
        end;
      end;
    until (FindNext(sr) <> 0);
end;

begin
  Lines := GetLinesTotal('');
  Writeln('Lines : ', Lines);
  Writeln('Size  : ', Size, ' bytes');
  ReadLn();
end.

