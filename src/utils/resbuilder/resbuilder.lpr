program resbuilder;
{$mode delphi}
{$H+}
var
  input, output: string;
  debug: boolean;
begin
if (not paramcount = 3) then
begin
println("Usage: resbuilder [input.file] [output.file] [dbg/rls]");
exit;
end;
input  := paramstr(1);
output := paramstr(2);
end.

