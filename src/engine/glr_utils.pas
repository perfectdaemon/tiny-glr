unit glr_utils;

{$i defines.inc}

interface

uses
  glr_math;

const
  TINYGLR_VERSION = '0.2 :: unstable';
  LOG_FILE = 'tinyglr.log';

type

  { TglrStream }

  TglrStream = class
    class function Init(Memory: Pointer; MemSize: LongInt; MemoryOwner: Boolean = False): TglrStream; overload;
    class function Init(const FileName: AnsiString; RW: Boolean = False): TglrStream; overload;
    destructor Destroy; override;
  private
    fMemoryOwner: Boolean;
    SType  : (stMemory, stFile);
    FSize  : LongInt;
    FPos   : LongInt;
    FBPos  : LongInt;
    F      : File;
    Mem    : Pointer;
    procedure SetPos(Value: LongInt);
    procedure SetBlock(BPos, BSize: LongInt);
  public
    procedure CopyFrom(const Stream: TglrStream);
    function Read(out Buf; BufSize: LongInt): LongInt;
    function Write(const Buf; BufSize: LongInt): LongInt;
    function ReadAnsi: AnsiString;
    procedure WriteAnsi(const Value: AnsiString);
    function ReadUnicode: WideString;
    procedure WriteUnicode(const Value: WideString);
    property Size: LongInt read FSize;
    property Pos: LongInt read FPos write SetPos;
  end;

  TglrListCompareFunc = function (Item1, Item2: Pointer): LongInt;

  { TglrList }

  TglrList<T> = class
    procedure Init(Capacity: LongInt);
  protected
    FItems    : array of T;
    FCount    : LongInt;
    FCapacity : LongInt;
    procedure BoundsCheck(Index: LongInt);
    function GetItem(Index: LongInt): T; inline;
    procedure SetItem(Index: LongInt; Value: T); inline;
    procedure SortFragment(CompareFunc: TglrListCompareFunc; L, R: LongInt);
    function GetFirstElementAddr(): Pointer;
  public
    constructor Create(aCapacity: LongInt = 4); virtual;
    destructor Destroy(); override;

    function IndexOf(Item: T): LongInt;
    function Add(Item: T): LongInt;
    procedure DeleteByIndex(Index: LongInt);
    procedure Delete(Item: T);
    procedure DeleteSafe(Item: T);
    procedure DeleteSafeByIndex(Index: LongInt);
    procedure Insert(Index: LongInt; Item: T);
    procedure Sort(CompareFunc: TglrListCompareFunc);
    property Count: LongInt read FCount;
    property Items[Index: LongInt]: T read GetItem write SetItem; default;
    procedure Clear();
    property FirstElementAddr: Pointer read GetFirstElementAddr;
  end;

  TglrStringList = TglrList<AnsiString>;
  TglrWordList = TglrList<Word>;
  TglrLongWordList = TglrList<LongWord>;
  TglrVec3fList = TglrList<TglrVec3f>;
  TglrVec2fList = TglrList<TglrVec2f>;

  { TglrObjectList }

  TglrObjectList<T> = class (TglrList<T>)
    procedure Free(aFreeObjects: Boolean = False);
  public
    procedure DeleteByIndex(Index: LongInt; FreeItem: Boolean = False); reintroduce;
    procedure Delete(Item: T; FreeItem: Boolean = False); reintroduce;
    procedure DeleteSafe(Item: T; FreeItem: Boolean = False); reintroduce;
    procedure DeleteSafeByIndex(Index: LongInt; FreeItem: Boolean = False); reintroduce;
    procedure Clear(FreeItems: Boolean = False);  reintroduce;
  end;

  { TglrPool }

  TglrPool<T> = class (TglrObjectList<T>)
  public
    constructor Create(const aPoolSize: Integer); virtual;
    function Get(): T;
    procedure Release(aItem: T);
  end;

  { TglrDictionary }

  TglrDictionary<Key, Value> = class
  protected
    fSorted  : Boolean;
    fKeys    : array of Key;
    fValues  : array of Value;
    fCount   : LongInt;
    fCapacity: LongInt;
    procedure BoundsCheck(Index: LongInt);
    function GetItem(aKey: Key): Value; inline;
    procedure SetItem(aKey: Key; aValue: Value); inline;
    function GetKey(aIndex: Integer): Key; inline;
    procedure SetKey(aIndex: Integer; aKey: Key); inline;
    function GetValue(aIndex: Integer): Value; inline;
    procedure SetValue(aIndex: Integer; aValue: Value); inline;
  public
    constructor Create(aCapacity: LongInt = 4); virtual;
    destructor Destroy(); override;

    function IndexOfKey(aKey: Key): LongInt;
    function IndexOfValue(aValue: Value): LongInt;
    function Add(aKey: Key; aValue: Value): LongInt;
    procedure DeleteByIndex(aIndex: LongInt);
    procedure Delete(aKey: Key);
    procedure DeleteSafe(aKey: Key);
    procedure DeleteSafeByIndex(aIndex: LongInt);

    property Count: LongInt read fCount;
    property Items[aKey: Key]: Value read GetItem write SetItem; default;
    property Keys[aIndex: Integer]: Key read GetKey write SetKey;
    property Values[aIndex: Integer]: Value read GetValue write SetValue;

    procedure SortByKey(aAscending: Boolean = True);
    function GetLerpValue(aKey: Key): Value;
  end;

  { TglrStack }

  TglrStack<T> = class (TglrList<T>)
  protected
    fCurrentIndex: Integer;
  public
    constructor Create(aCapacity: LongInt = 4); override;
    procedure Push(aItem: T);
    function Pop(): T;
    // Get head with no pop
    function Head(): T;
    function IsEmpty(): Boolean;
  end;

  TglrSimpleAction = procedure of object;
  TglrContinuousAction = procedure(const DeltaTime: Double) of object;

  { TglrActionManager }

  TglrActionManager = class
  protected
    type
      TglrVirtualActionInfo = class
        Done: Boolean;
      end;

      TglrSimpleActionInfo = class (TglrVirtualActionInfo)
        Action: TglrSimpleAction;
        StartAfter: Single;
        constructor Create(aAction: TglrSimpleAction; aStartAfter: Single);
      end;

      TglrContinuousActionInfo = class (TglrVirtualActionInfo)
        Action: TglrContinuousAction;
        StartAfter, Period: Single;
        constructor Create(aAction: TglrContinuousAction; aStartAfter, aPeriod: Single);
      end;

    TglrSimpleActionList     = TglrObjectList<TglrSimpleActionInfo>;
    TglrContinousActionList  = TglrObjectList<TglrContinuousActionInfo>;
    TglrSharedActionsList   = TglrObjectList<TglrVirtualActionInfo>;

    var
      fSimpleList: TglrSimpleActionList;
      fContinuousList: TglrContinousActionList;
      fQueue: TglrSharedActionsList;

    procedure PerformSimpleAction(Action: TglrSimpleActionInfo;
      const DeltaTime: Double);
    procedure PerformContinuousAction(Action: TglrContinuousActionInfo;
      const DeltaTime: Double);
  public
    constructor Create();
    destructor Destroy(); override;

    procedure AddIndependent(Action: TglrSimpleAction;                    StartAfter: Single = 0); overload;
    procedure AddIndependent(Action: TglrContinuousAction; Period: Single; StartAfter: Single = 0); overload;

    procedure AddToQueue(Action: TglrSimpleAction;                     StartAfter: Single = 0); overload;
    procedure AddToQueue(Action: TglrContinuousAction; Period: Single; StartAfter: Single = 0); overload;

    procedure Update(const DeltaTime: Double);
  end;

  { Convert }

  Convert = class
  public
    class function ToString(aVal: Integer): AnsiString; overload;
    class function ToString(aVal: LongWord): AnsiString; overload;
    class function ToString(aVal: Single; Digits: Integer = 5): AnsiString; overload;
    class function ToString(aVal: Boolean): AnsiString; overload;
    class function ToString(aVal: TglrMat4f; aDigits: Integer = 5): AnsiString; overload;
    class function ToString(aVal: TglrVec2f): AnsiString; overload;
    class function ToString(aVal: TglrVec3f): AnsiString; overload;
    class function ToString(aVal: TglrVec4f): AnsiString; overload;
    class function ToString(aVal: Pointer): AnsiString; overload;
    class function ToInt(aStr: AnsiString; aDefault: Integer = -1): Integer; overload;
    class function ToFloat(aStr: AnsiString; aDefault: Single = -1.0): Single; overload;
  end;

  { Log }

  TglrLogMessageType = (lInformation, lWarning, lError, lCritical);

  Log = class
  protected
    class var f: Text;
    class var fTotalErrors, fTotalWarnings: Word;
  public
    class procedure Init(const aFileName: AnsiString);
    class procedure Deinit();
    class procedure Write(aType: TglrLogMessageType; aMessage: AnsiString);
  end;



function StrTrim(const s: AnsiString): AnsiString;
function StrSplit(aString: AnsiString; aSeparator: AnsiChar): TglrStringList; overload;
function StrSplit(aString: AnsiString; aSeparators: AnsiString): TglrStringList; overload;

implementation

//uses
//  glr_core;

function StrTrim(const s: AnsiString): AnsiString;
var
  Ofs, Len: sizeint;
begin
  Len := Length(S);
  while (Len > 0) and (S[Len] <= ' ') do
    Dec(Len);
  Ofs := 1;
  while (Ofs <= Len) and (S[Ofs] <= ' ') do
    Inc(Ofs);
  Result := Copy(S, Ofs, 1 + Len - Ofs);
end;

function StrSplit(aString: AnsiString; aSeparator: AnsiChar): TglrStringList; overload;
begin
  Result := StrSplit(aString, AnsiString(aSeparator));
end;

function StrSplit(aString: AnsiString; aSeparators: AnsiString): TglrStringList; overload;

  function CheckSeparators(Char: AnsiChar): Boolean;
  var
    i: Integer;
  begin
    Result := False;
    for i := 1 to Length(aSeparators) do
      if (Char = aSeparators[i]) then
        Exit(True);
  end;

var
  i, start: Integer;
begin
  Result := TglrStringList.Create();
  start := 0;
  for i := 1 to Length(aString) do
    if CheckSeparators(aString[i]) then
    begin
      Result.Add(Copy(aString, start + 1, i - start - 1));
      start := i;
    end;
  Result.Add(Copy(aString, start + 1, Length(aString)));
end;

{ TglrStream }

class function TglrStream.Init(Memory: Pointer; MemSize: LongInt;
  MemoryOwner: Boolean): TglrStream;
begin
  Result := TglrStream.Create;
  with Result do
  begin
    SType := stMemory;
    Mem   := Memory;
    FSize := MemSize;
    FPos  := 0;
    FBPos := 0;
    fMemoryOwner := MemoryOwner;
  end;
end;

class function TglrStream.Init(const FileName: AnsiString; RW: Boolean
  ): TglrStream;
var
  io: Integer;
begin
  Result := TglrStream.Create();
  AssignFile(Result.F, FileName);
  if RW then
    Rewrite(Result.F, 1)
  else
    Reset(Result.F, 1);
  io := IOResult;
  if io = 0 then
  begin
    Result.SType := stFile;
    Result.FSize := FileSize(Result.F);
    Result.FPos  := 0;
    Result.FBPos := 0;
  end
  else
  begin
    Result.Free;
    Result := nil;
  end;
end;

destructor TglrStream.Destroy;
begin
  if SType = stFile then
    CloseFile(F)
  else if (SType = stMemory) and fMemoryOwner then
    FreeMem(Mem);
end;

procedure TglrStream.SetPos(Value: LongInt);
begin
  FPos := Value;
  if SType = stFile then
    Seek(F, FBPos + FPos);
end;

procedure TglrStream.SetBlock(BPos, BSize: LongInt);
begin
  FSize := BSize;
  FBPos := BPos;
  Pos := 0;
end;

procedure TglrStream.CopyFrom(const Stream: TglrStream);
var
  p : Pointer;
  CPos : LongInt;
begin
  p := GetMemory(Stream.Size);
  CPos := Stream.Pos;
  Stream.Pos := 0;
  Stream.Read(p^, Stream.Size);
  Stream.Pos := CPos;
  Write(p^, Stream.Size);
  FreeMemory(p);
end;


function TglrStream.Read(out Buf; BufSize: LongInt): LongInt;
begin
  if SType = stMemory then
  begin
    Result := Min(FPos + BufSize, FSize) - FPos;
    Move((Mem + FPos)^, Buf, Result);
  end else if SType = stFile then
    BlockRead(F, Buf, BufSize, Result);
  Inc(FPos, Result);
end;

function TglrStream.Write(const Buf; BufSize: LongInt): LongInt;
begin
  if SType = stMemory then
  begin
    Result := Min(FPos + BufSize, FSize) - FPos;
    Move(Buf, (Mem + FPos)^, Result);
  end else if SType = stFile then
    BlockWrite(F, Buf, BufSize, Result);

  Inc(FPos, Result);
  Inc(FSize, Max(0, FPos - FSize));
end;

function TglrStream.ReadAnsi: AnsiString;
var
  Len : Word;
begin
  Read(Len, SizeOf(Len));
  if Len > 0 then
  begin
    SetLength(Result, Len);
    Read(Result[1], Len);
  end else
    Result := '';
end;

procedure TglrStream.WriteAnsi(const Value: AnsiString);
var
  Len : Word;
begin
  Len := Length(Value);
  Write(Len, SizeOf(Len));
  if Len > 0 then
    Write(Value[1], Len);
end;

function TglrStream.ReadUnicode: WideString;
var
  Len : Word;
begin
  Read(Len, SizeOf(Len));
  SetLength(Result, Len);
  Read(Result[1], Len * 2);
end;

procedure TglrStream.WriteUnicode(const Value: WideString);
var
  Len : Word;
begin
  Len := Length(Value);
  Write(Len, SizeOf(Len));
  Write(Value[1], Len * 2);
end;

{ TglrList<T> }

procedure TglrList<T>.Init(Capacity: LongInt);
begin
  FItems := nil;
  FCount := 0;
  FCapacity := Capacity;
end;

procedure TglrList<T>.BoundsCheck(Index: LongInt);
begin
  if (Index < 0) or (Index >= FCount) then
    Log.Write(lCritical, 'List index out of bounds (' + Convert.ToString(Index) + ')');
end;

function TglrList<T>.GetItem(Index: LongInt): T;
begin
  BoundsCheck(Index);
  Result := FItems[Index];
end;

procedure TglrList<T>.SetItem(Index: LongInt; Value: T);
begin
  BoundsCheck(Index);
  FItems[Index] := Value;
end;

procedure TglrList<T>.SortFragment(CompareFunc: TglrListCompareFunc; L, R: LongInt);
var
  i, j : Integer;
  P, tm : T;
begin
  repeat
    i := L;
    j := R;
    P := FItems[(L + R) div 2];
    repeat
      while CompareFunc(@FItems[i], @P) < 0 do
        Inc(i);
      while CompareFunc(@FItems[j], @P) > 0 do
        Dec(j);
      if i <= j then
      begin
        tm := FItems[i];
        FItems[i] := FItems[j];
        FItems[j] := tm;
        Inc(i);
        Dec(j);
      end;
    until i > j;
    if L < j then
      SortFragment(CompareFunc, L, j);
    L := i;
  until i >= R;
end;

function TglrList<T>.GetFirstElementAddr: Pointer;
begin
  if (Count > 0) then
    Result := @FItems[0]
  else
    Result := nil;
end;

constructor TglrList<T>.Create(aCapacity: LongInt);
begin
  inherited Create();
  Init(aCapacity);
end;

destructor TglrList<T>.Destroy;
begin
  SetLength(FItems, 0);
  FCount := 0;
  inherited Destroy;
end;

function TglrList<T>.IndexOf(Item: T): LongInt;
var
  i : LongInt;
begin
  for i := 0 to FCount - 1 do
    if @FItems[i] = @Item then
    begin
      Result := i;
      Exit;
    end;
  Result := -1;
end;

function TglrList<T>.Add(Item: T): LongInt;
begin
  if FCount mod FCapacity = 0 then
    SetLength(FItems, Length(FItems) + FCapacity);
  FItems[FCount] := Item;
  Result := FCount;
  Inc(FCount);
end;

procedure TglrList<T>.DeleteByIndex(Index: LongInt);
begin
  BoundsCheck(Index);
  if Index <> fCount - 1 then
    Move(FItems[Index + 1], FItems[Index], (FCount - Index - 1) * SizeOf(T));
  Dec(FCount);
  if Length(FItems) - FCount + 1 > FCapacity then
    SetLength(FItems, Length(FItems) - FCapacity);
end;

procedure TglrList<T>.Delete(Item: T);
var
  i: Integer;
begin
  i := IndexOf(Item);
  if i <> -1 then
    DeleteByIndex(i)
  else
    Log.Write(lError, 'List: No item found at list, delete is impossible');
end;

procedure TglrList<T>.DeleteSafe(Item: T);
var
  i: Integer;
begin
  i := IndexOf(Item);
  if i <> -1 then
    DeleteSafeByIndex(i)
  else
    Log.Write(lError, 'List: No item found at list, delete is impossible');
end;

procedure TglrList<T>.DeleteSafeByIndex(Index: LongInt);
var
  i: Integer;
begin
  BoundsCheck(Index);
  for i := Index to FCount - 2 do
    FItems[i] := FItems[i + 1];

  Dec(FCount);
  if Length(FItems) - FCount + 1 > FCapacity then
    SetLength(FItems, Length(FItems) - FCapacity);
end;

procedure TglrList<T>.Insert(Index: LongInt; Item: T);
begin
  BoundsCheck(Index);
  Add(Item); //can't add nil
  Move(FItems[Index], FItems[Index + 1], (FCount - Index - 1) * SizeOf(FItems[0]));
  FItems[Index] := Item;
end;

procedure TglrList<T>.Sort(CompareFunc: TglrListCompareFunc);
begin
  if FCount > 1 then
    SortFragment(CompareFunc, 0, FCount - 1);
end;

procedure TglrList<T>.Clear;
begin
  FCount := 0;
end;


{ TglrObjectList<T> }

procedure TglrObjectList<T>.Free(aFreeObjects: Boolean);
var
  i: Integer;
begin
  if aFreeObjects then
    for i := 0 to Count - 1 do
      TObject(FItems[i]).Free();
  inherited Free();
end;

procedure TglrObjectList<T>.DeleteByIndex(Index: LongInt; FreeItem: Boolean);
begin
  BoundsCheck(Index);
  if FreeItem then
    TObject(FItems[Index]).Free;
  if Index <> fCount - 1 then
    Move(FItems[Index + 1], FItems[Index], (FCount - Index - 1) * SizeOf(T));
  Dec(FCount);
  if Length(FItems) - FCount + 1 > FCapacity then
    SetLength(FItems, Length(FItems) - FCapacity);
end;

procedure TglrObjectList<T>.Delete(Item: T; FreeItem: Boolean);
var
  i: Integer;
begin
  i := IndexOf(Item);
  if i <> -1 then
    DeleteByIndex(i, FreeItem)
  else
    Log.Write(lError, 'List: No item found at list, delete is impossible');
end;

procedure TglrObjectList<T>.DeleteSafe(Item: T; FreeItem: Boolean);
var
  i: Integer;
begin
  i := IndexOf(Item);
  if i <> -1 then
    DeleteSafeByIndex(i, FreeItem)
  else
    Log.Write(lError, 'List: No item found at list, delete is impossible');
end;

procedure TglrObjectList<T>.DeleteSafeByIndex(Index: LongInt; FreeItem: Boolean);
var
  i: Integer;
begin
  BoundsCheck(Index);
  if FreeItem then
    TObject(FItems[Index]).Free;
  for i := Index to FCount - 2 do
    FItems[i] := FItems[i + 1];

  Dec(FCount);
  if Length(FItems) - FCount + 1 > FCapacity then
    SetLength(FItems, Length(FItems) - FCapacity);
end;

procedure TglrObjectList<T>.Clear(FreeItems: Boolean);
var
  i: Integer;
begin
  if FreeItems then
    for i := 0 to FCount - 1 do
      TObject(FItems[i]).Free();
  inherited Clear();
end;

{ TglrPool<T> }

constructor TglrPool<T>.Create(const aPoolSize: Integer);
var
  i: Integer;
  new: T;
begin
  inherited Create(aPoolSize);
  for i := 0 to aPoolSize - 1 do
  begin
    new := T.Create;
    new.Enabled := False;
    Add(new);
  end;
end;

function TglrPool<T>.Get: T;
var
  i: Integer;
  new: T;
begin
  for i := 0 to Count - 1 do
    if (not Items[i].Enabled) then
    begin
      Items[i].Enabled := True;
      Exit(Items[i]);
    end;

  new := T.Create;
  Add(new);
  new.Enabled := True;
  Exit(new);
end;

procedure TglrPool<T>.Release(aItem: T);
begin
  aItem.Enabled := False;
end;

{ TglrDictionary<Key, Value> }

procedure TglrDictionary<Key, Value>.BoundsCheck(Index: LongInt);
begin
  if (Index < 0) or (Index >= fCount) then
    Log.Write(lCritical, 'Dictionary index out of bounds (' + Convert.ToString(Index) + ')');
end;

function TglrDictionary<Key, Value>.GetItem(aKey: Key): Value;
var
  i: Integer;
begin
  for i := 0 to fCount - 1 do
    if fKeys[i] = aKey then
      Exit(fValues[i]);
end;

procedure TglrDictionary<Key, Value>.SetItem(aKey: Key; aValue: Value);
var
  i: Integer;
begin
  for i := 0 to fCount - 1 do
    if fKeys[i] = aKey then
      fValues[i] := aValue;
end;

function TglrDictionary<Key, Value>.GetKey(aIndex: Integer): Key;
begin
  BoundsCheck(aIndex);
  Exit(fKeys[aIndex]);
end;

procedure TglrDictionary<Key, Value>.SetKey(aIndex: Integer; aKey: Key);
begin
  BoundsCheck(aIndex);
  fKeys[aIndex] := aKey;
  fSorted := False;
end;

function TglrDictionary<Key, Value>.GetValue(aIndex: Integer): Value;
begin
  BoundsCheck(aIndex);
  Exit(fValues[aIndex]);
end;

function TglrDictionary<Key, Value>.GetLerpValue(aKey: Key): Value;
var
  i: Integer;
  t: single;
begin
  if not fSorted then
    Log.Write(lCritical, 'Dictionary: can not return lerp value, dictionary is not sorted!');

  i := IndexOfKey(aKey);
  if i <> -1 then
    Exit(fValues[i]);

  for i := 0 to fCount - 1 do
    if fKeys[i] > aKey then
      break;

  if (i = 0) or (i = fCount - 1) then
    Exit(fValues[i])
  else
  begin
    t := (aKey - fKeys[i - 1]) / (fKeys[i] - fKeys[i - 1]);
    Result := Value(fValues[i - 1] + (fValues[i] - fValues[i - 1]) * t);
  end;
end;

procedure TglrDictionary<Key, Value>.SetValue(aIndex: Integer; aValue: Value);
begin
  BoundsCheck(aIndex);
  fValues[aIndex] := aValue;
end;

constructor TglrDictionary<Key, Value>.Create(aCapacity: LongInt);
begin
  inherited Create();
  fCount := 0;
  SetLength(fKeys, fCount);
  SetLength(fValues, fCount);
  fCapacity := aCapacity;
  fSorted := False;
end;

destructor TglrDictionary<Key, Value>.Destroy;
begin
  fCount := 0;
  SetLength(fKeys, fCount);
  SetLength(fValues, fCount);
  inherited Destroy;
end;

function TglrDictionary<Key, Value>.IndexOfKey(aKey: Key): LongInt;
var
  i: Integer;
begin
  for i := 0 to fCount - 1 do
    if fKeys[i] = aKey then
      Exit(i);
  Exit(-1);
end;

function TglrDictionary<Key, Value>.IndexOfValue(aValue: Value): LongInt;
var
  i: Integer;
begin
  for i := 0 to fCount - 1 do
    if fValues[i] = aValue then
      Exit(i);
  Exit(-1);
end;

function TglrDictionary<Key, Value>.Add(aKey: Key; aValue: Value): LongInt;
begin
  if fCount mod fCapacity = 0 then
  begin
    SetLength(fKeys, Length(fKeys) + fCapacity);
    SetLength(fValues, Length(fValues) + fCapacity);
  end;
  fKeys[fCount] := aKey;
  fValues[fCount] := aValue;
  Result := fCount;
  Inc(fCount);
  fSorted := False;
end;

procedure TglrDictionary<Key, Value>.SortByKey(aAscending: Boolean);
var
  i, j, max: LongInt;
  k: Key;
  v: Value;
begin
  for i := 0 to fCount - 2 do
  begin
    max := i;
    for j := i to fCount - 1 do
      if (aAscending and (fKeys[j] < fKeys[max])) or
        (not aAscending and (fKeys[j] > fKeys[max])) then
        max := j;

    if max <> i then
    begin
      k := fKeys[i];
      v := fValues[i];
      fKeys[i] := fKeys[max];
      fValues[i] := fValues[max];
      fKeys[max] := k;
      fValues[max] := v;
    end;
  end;
  fSorted := True;
end;

procedure TglrDictionary<Key, Value>.DeleteByIndex(aIndex: LongInt);
begin
  BoundsCheck(aIndex);
  if aIndex <> fCount - 1 then
  begin
    Move(fKeys[aIndex + 1], fKeys[aIndex], (fCount - aIndex - 1) * SizeOf(Key));
    Move(fValues[aIndex + 1], fValues[aIndex], (fCount - aIndex - 1) * SizeOf(Value));
  end;
  Dec(fCount);
  if Length(fKeys) - fCount + 1 > fCapacity then
  begin
    SetLength(fKeys, Length(fKeys) - fCapacity);
    SetLength(fValues, Length(fValues) - fCapacity);
  end;
end;

procedure TglrDictionary<Key, Value>.Delete(aKey: Key);
var
  i: Integer;
begin
  i := IndexOfKey(aKey);
  if i <> -1 then
    DeleteByIndex(i)
  else
    Log.Write(lError, 'Dictionary: No item found at list, delete is impossible');
end;

procedure TglrDictionary<Key, Value>.DeleteSafe(aKey: Key);
var
  i: Integer;
begin
  i := IndexOfKey(aKey);
  if i <> -1 then
    DeleteSafeByIndex(i)
  else
    Log.Write(lError, 'Dictionary: No item found at list, delete is impossible');
end;

procedure TglrDictionary<Key, Value>.DeleteSafeByIndex(aIndex: LongInt);
var
  i: Integer;
begin
  BoundsCheck(aIndex);
  for i := aIndex to fCount - 2 do
  begin
    fKeys[i] := fKeys[i + 1];
    fValues[i] := fValues[i + 1];
  end;

  Dec(fCount);
  if Length(fKeys) - fCount + 1 > fCapacity then
  begin
    SetLength(fKeys, Length(fKeys) - fCapacity);
    SetLength(fValues, Length(fValues) - fCapacity);
  end;
end;

{ TglrStack<T> }

constructor TglrStack<T>.Create(aCapacity: LongInt);
begin
  inherited Create(aCapacity);
  fCurrentIndex := -1;
end;

procedure TglrStack<T>.Push(aItem: T);
begin
  Inc(fCurrentIndex);
  if (fCurrentIndex < fCount) then
    fItems[fCurrentIndex] := aItem
  else
    fCurrentIndex := Add(aItem);
end;

function TglrStack<T>.Pop(): T;
begin
  if (fCurrentIndex >= 0) and (fCurrentIndex < fCount) then
  begin
    Result := fItems[fCurrentIndex];
    Dec(fCurrentIndex);
    Exit();
  end
  else
    Log.Write(lError,
      'Stack: pop failed, Current Index is ' + Convert.ToString(fCurrentIndex) +
      ', Count is ' + Convert.ToString(fCount));
end;

function TglrStack<T>.Head: T;
begin
  if (fCurrentIndex >= 0) and (fCurrentIndex < fCount) then
    Exit(fItems[fCurrentIndex])
  else
    Log.Write(lError,
        'Stack: head() failed, Current Index is ' + Convert.ToString(fCurrentIndex) +
        ', Count is ' + Convert.ToString(fCount));
end;

function TglrStack<T>.IsEmpty(): Boolean;
begin
  Result := fCurrentIndex < 0;
end;

{ TglrActionManager.TglrContinuousActionInfo }

constructor TglrActionManager.TglrContinuousActionInfo.Create(
  aAction: TglrContinuousAction; aStartAfter, aPeriod: Single);
begin
  Action := aAction;
  StartAfter := aStartAfter;
  Period := aPeriod;
  Done := False;
end;

{ TglrActionManager.TglrSimpleActionInfo }

constructor TglrActionManager.TglrSimpleActionInfo.Create(
  aAction: TglrSimpleAction; aStartAfter: Single);
begin
  Action := aAction;
  StartAfter := aStartAfter;
  Done := False;
end;

{ TglrActionManager }

procedure TglrActionManager.PerformSimpleAction(Action: TglrSimpleActionInfo;
  const DeltaTime: Double);
begin
  with Action do
  begin
    if (StartAfter > 0) then
      StartAfter -= DeltaTime
    else
    begin
      Action();
      Done := True;
    end;
  end;
end;

procedure TglrActionManager.PerformContinuousAction(
  Action: TglrContinuousActionInfo; const DeltaTime: Double);
begin
  with Action do
  begin
    if (StartAfter > 0) then
      StartAfter -= DeltaTime
    else if (Period >= 0) then
    begin
      Action(DeltaTime);
      Period -= DeltaTime;
    end
    else
      Done := True;
  end;
end;

constructor TglrActionManager.Create;
begin
  inherited;
  fSimpleList     := TglrSimpleActionList.Create();
  fContinuousList := TglrContinousActionList.Create();
  fQueue          := TglrSharedActionsList.Create();
end;

destructor TglrActionManager.Destroy;
begin
  fSimpleList.Free(True);
  fContinuousList.Free(True);
  fQueue.Free(True);
  inherited Destroy;
end;

procedure TglrActionManager.AddIndependent(Action: TglrSimpleAction;
  StartAfter: Single);
begin
  fSimpleList.Add(TglrSimpleActionInfo.Create(Action, StartAfter));
end;

procedure TglrActionManager.AddIndependent(Action: TglrContinuousAction;
  Period: Single; StartAfter: Single);
begin
  fContinuousList.Add(TglrContinuousActionInfo.Create(Action, StartAfter, Period));
end;

procedure TglrActionManager.AddToQueue(Action: TglrSimpleAction;
  StartAfter: Single);
begin
  fQueue.Add(TglrSimpleActionInfo.Create(Action, StartAfter));
end;

procedure TglrActionManager.AddToQueue(Action: TglrContinuousAction;
  Period: Single; StartAfter: Single);
begin
  fQueue.Add(TglrContinuousActionInfo.Create(Action, StartAfter, Period));
end;

procedure TglrActionManager.Update(const DeltaTime: Double);
var
  i: Integer;
  isAnyActionInQueue: Boolean;
begin
  // Perform simple independent actions
  for i := 0 to fSimpleList.Count - 1 do
    PerformSimpleAction(fSimpleList[i], DeltaTime);

  // Perform continuoues independent actions
  for i := 0 to fContinuousList.Count - 1 do
    PerformContinuousAction(fContinuousList[i], DeltaTime);

  // Perform queue actions
  isAnyActionInQueue := False;
  for i := 0 to fQueue.Count - 1 do
    if fQueue[i].Done then
      continue
    else
    begin
      isAnyActionInQueue := True;
      if (fQueue[i] is TglrSimpleActionInfo) then
        PerformSimpleAction(fQueue[i] as TglrSimpleActionInfo, DeltaTime)
      else if (fQueue[i] is TglrContinuousActionInfo) then
        PerformContinuousAction(fQueue[i] as TglrContinuousActionInfo, DeltaTime);
      break;
    end;

  // Clear list if there is only done actions
  if not isAnyActionInQueue then
    fQueue.Clear();

  // Clean done actions
  for i := 0 to fSimpleList.Count - 1 do
    if (fSimpleList[i].Done) then
      fSimpleList.DeleteByIndex(i, True);

  for i := 0 to fContinuousList.Count - 1 do
    if (fContinuousList[i].Done) then
      fContinuousList.DeleteByIndex(i, True);

end;

{ Convert }

class function Convert.ToString(aVal: Integer): AnsiString;
begin
  Str(aVal, Result);
end;

class function Convert.ToString(aVal: LongWord): AnsiString;
begin
  Str(aVal, Result);
end;

class function Convert.ToString(aVal: Single; Digits: Integer = 5): AnsiString;
begin
  Str(aVal:0:Digits, Result);
end;

class function Convert.ToString(aVal: Boolean): AnsiString;
begin
  if (aVal) then
    Result := 'True'
  else
    Result := 'False';
end;

class function Convert.ToString(aVal: TglrMat4f; aDigits: Integer): AnsiString;
var
  p: PSingle;
  i: Integer;
begin
  p := @aVal;
  Result := '';
  for i := 0 to 15 do
  begin
    Result += ToString(p[i], aDigits) + ' ';
    if i in [3, 7, 11] then
      Result += #13#10;
  end;
end;

class function Convert.ToString(aVal: TglrVec2f): AnsiString;
begin
  Result := ToString(aVal.x) + '|' + ToString(aVal.y);
end;

class function Convert.ToString(aVal: TglrVec3f): AnsiString;
begin
  Result := ToString(aVal.x) + '|' + ToString(aVal.y) + '|' + ToString(aVal.z);
end;

class function Convert.ToString(aVal: TglrVec4f): AnsiString;
begin
  Result := ToString(aVal.x) + '|' + ToString(aVal.y) + '|' + ToString(aVal.z)
   + '|' + ToString(aVal.w);
end;

class function Convert.ToString(aVal: Pointer): AnsiString;
begin
  Result := '#' + Convert.ToString(Integer(aVal));
end;

class function Convert.ToInt(aStr: AnsiString; aDefault: Integer): Integer;
var
  Code: Integer;
begin
  Val(aStr, Result, Code);
  if (Code <> 0) then
    Result := aDefault;
end;

class function Convert.ToFloat(aStr: AnsiString; aDefault: Single): Single;
var
  Code: Integer;
begin
  Val(aStr, Result, Code);
  if (Code <> 0) then
    Result := aDefault;
end;

{ Log }

class procedure Log.Init(const aFileName: AnsiString);
begin
  {$ifdef log}
  AssignFile(f, aFileName);
  Rewrite(f);
  CloseFile(f);
  Self.Write(lInformation, 'Start. Tiny glr version: ' + TINYGLR_VERSION);
  {$endif}
end;

class procedure Log.Deinit;
begin
  {$ifdef log}
  Self.Write(lInformation, 'End. Errors: ' + Convert.ToString(fTotalErrors) + ', warnings: ' + Convert.ToString(fTotalWarnings));
  {$endif}
end;

const
  cLOG_MESSAGE_TYPES: array[TglrLogMessageType] of AnsiString =
  ('    ', ' !  ', ' !! ', '!!!!');

class procedure Log.Write(aType: TglrLogMessageType; aMessage: AnsiString);
begin
  {$ifdef log}
  Append(f);
  WriteLn(f, cLOG_MESSAGE_TYPES[aType] + '::'#9 + aMessage);
  CloseFile(f);
  if (aType = lWarning) then
    fTotalWarnings += 1
  else if (aType = lError) then
    fTotalErrors += 1
  else if (aType = lCritical) then
    Assert(False, 'Critical error detected: ' + aMessage);
  {$endif}
end;


end.

