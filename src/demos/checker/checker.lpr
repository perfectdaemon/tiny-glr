program checker;

{$APPTYPE CONSOLE}

uses
  glr_utils;

type
  TStack = TglrStack<Integer>;

var
  stack: TStack;
  p: Integer;

begin
  Log.Init('checker.log');
  stack := TStack.Create();
  stack.Push(1);
  WriteLn('Pushed 1');
  stack.Push(2);
  WriteLn('Pushed 2');
  stack.Push(3);
  WriteLn('Pushed 3');
  stack.Push(4);
  WriteLn('Pushed 4');
  p := stack.Pop();
  WriteLn('Popped ', p, ' awaiting 4');
  p := stack.Pop();
  WriteLn('Popped ', p, ' awaiting 3');
  stack.Push(5);
  WriteLn('Pushed 5');
  p := stack.Pop();
  WriteLn('Popped ', p, ' awaiting 5');
  p := stack.Pop();
  WriteLn('Popped ', p, ' awaiting 2');
  p := stack.Pop();
  WriteLn('Popped ', p, ' awaiting 1');
  p := stack.Pop();
  WriteLn('Popped ', p, ' awaiting error');
  stack.Free();

  Log.Deinit();

  ReadLn();
end.

