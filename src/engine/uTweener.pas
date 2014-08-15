unit uTweener;

interface

uses
  tinyglr,
  glrMath;

type
  TglrVarType = (vtInteger, vtSingle, vtVector);
  PglrInteger = ^Integer;
  PglrSingle = PSingle;
  PglrVec3f = ^TdfVec3f;

  TglrTweenObject = TObject;

  TglrEasingFunc = function(aStartValue, aDiffValue, aUnitValue: Single): Single of object;
  TglrTweenStyle = (tsElasticEaseIn, tsElasticEaseOut, tsExpoEaseIn, tsBounce, tsSimple);
  TglrSetSingle = procedure(aObject: TglrTweenObject; aValue: Single);
  TglrDoneCallback = procedure of object;

  TglrBaseTweenItem = class
  protected
    FPaused: Boolean;
    FTime: Single;
    FPauseOnStart: Single;
    FDone: Boolean;
    FOnDone: TglrDoneCallback;
    FDuration: Single;
  protected
    FEasingFunc: TglrEasingFunc;
    function GetUnitValue(): Single; virtual;
    function ShouldChange(): Boolean;
  public
    property Done: Boolean read FDone;
    property OnDone: TglrDoneCallback read FOnDone write FOnDone;
    procedure Play; virtual;
    procedure Pause; virtual;
    procedure SetPause(const aPause: Boolean); virtual;
    procedure Update(const aDeltaTime: Single); virtual;

    constructor Create(aDuration: Single; aPauseOnStart: Single);
  end;

  TglrBaseSingleTweenItem = class (TglrBaseTweenItem)
  protected
    FStartValue, FFinishValue: Single;
  public
    constructor Create(aStartValue, aFinishValue, aDuration: Single;
      aPauseOnStart: Single);
  end;

  TglrPSingleTweenItem = class (TglrBaseSingleTweenItem)
  protected
    FValue: PglrSingle;
  public
    procedure Update(const aDeltaTime: Single); override;
    constructor Create(aValue: PglrSingle; aStartValue, aFinishValue,
      aDuration: Single; aPauseOnStart: Single);
  end;

  TglrSingleTweenItem = class (TglrBaseSingleTweenItem)
  protected
    FSetSingleEvent: TglrSetSingle;
    FObject: TglrTweenObject;
  public
    property SetSingleEvent: TglrSetSingle read FSetSingleEvent write FSetSingleEvent;
    procedure Update(const aDeltaTime: Single); override;

    constructor Create(aObject: TglrTweenObject; aEvent: TglrSetSingle;
      aStartValue, aFinishValue, aDuration: Single; aPauseOnStart: Single);
  end;

  TglrBaseVectorTweenItem = class (TglrBaseTweenItem)
  protected
    FStartValue, FFinishValue: TdfVec3f;
  public
    constructor Create(aStartValue, aFinishValue: TdfVec3f; aDuration: Single;
      aPauseOnStart: Single);
  end;

  TglrPVectorTweenItem = class (TglrBaseVectorTweenItem)
  protected
    FValue: PglrVec3f;
  public
    procedure Update(const aDeltaTime: Single); override;
    constructor Create(aValue: PglrVec3f; aStartValue, aFinishValue: TdfVec3f;
      aDuration: Single; aPauseOnStart: Single);
  end;

  TglrBaseEasingFunctions = class
  public
    class function ExpoEaseIn(aStartValue, aDiffValue, aUnitValue: Single): Single;
    class function QuintEaseOut(aStartValue, aDiffValue, aUnitValue: Single): Single;
    class function ElasticEaseIn(aStartValue, aDiffValue, aUnitValue: Single): Single;
    class function Simple(aStartValue, aDiffValue, aUnitValue: Single): Single;
  end;

  TglrTweener = class
  protected
    FTweenItems: TglrList<Pointer>;
    FEasingFunctions: TglrBaseEasingFunctions;
    function GetTweenCount: Integer;
  public
    property TweenCount: Integer read GetTweenCount;

    // манипуляция с элементами списка
    function GetItemByIndex(const aIndex: integer): TglrBaseTweenItem;
    procedure FreeByIndex(const aIndex: integer);
    procedure FreeAll;
    function AddTweenItem(aTweenItem: TglrBaseTweenItem; aTweenStyle: TglrTweenStyle): Integer; virtual;

    // добавление типовых элементов
    function AddTweenPSingle(aVariable: PglrSingle; aTweenStyle: TglrTweenStyle;
       const aStartValue, aFinishValue, aDuration: Single;
       const aPauseOnStart: Single = 0): TglrPSingleTweenItem;

    function AddTweenPVector(aVariable: PglrVec3f; aTweenStyle: TglrTweenStyle;
      const aStartValue, aFinishValue: TdfVec3f; aDuration: Single;
      const aPauseOnStart: Single = 0): TglrPVectorTweenItem;

    function AddTweenSingle(aObject: TglrTweenObject; aSetValue: TglrSetSingle; aTweenStyle: TglrTweenStyle;
      const aStartValue, aFinishValue, aDuration: Single;
      const aPauseOnStart: Single = 0): TglrSingleTweenItem;

    procedure Update(const aDeltaTime: Single);

    constructor Create;
    destructor Destroy; override;
  end;

var
  Tweener: TglrTweener;

implementation

uses

  SysUtils;

function TglrBaseTweenItem.GetUnitValue: Single;
begin
  if FTime <= FPauseOnStart then
    Result := 0
  else if FTime = FDuration + FPauseOnStart then
    Result := 1
  else
    Result := (FTime - FPauseOnStart) / FDuration;
end;

function TglrBaseTweenItem.ShouldChange: Boolean;
begin
  Result := FTime >= FPauseOnStart;
end;

Procedure TglrBaseTweenItem.Play;
begin
  SetPause(False);
end;

procedure TglrBaseTweenItem.Pause;
begin
  SetPause(True);
end;

procedure TglrBaseTweenItem.SetPause(const aPause: Boolean);
begin
  FPaused := aPause;
end;

procedure TglrBaseTweenItem.Update(const aDeltaTime: Single);
begin
  if FPaused then
    Exit;

  FTime := FTime + aDeltaTime;
  if FTime - FPauseOnStart >= FDuration then
  begin
    FTime := FDuration + FPauseOnStart;
    FDone := True;
    Pause;
    if Assigned(FOnDone) then
      FOnDone()
  end;  
end;

constructor TglrBaseTweenItem.Create(aDuration: Single; aPauseOnStart: Single);
begin
  inherited Create;
  FDuration := aDuration;
  FPauseOnStart := aPauseOnStart;
  FTime := 0;
  FDone := False;
end;

constructor TglrBaseSingleTweenItem.Create(aStartValue, aFinishValue,
  aDuration: Single; aPauseOnStart: Single);
begin
  inherited Create(aDuration, aPauseOnStart);
  FStartValue := aStartValue;
  FFinishValue := aFinishValue;
end;

procedure TglrPSingleTweenItem.Update(const aDeltaTime: Single);
begin
  inherited Update(aDeltaTime);
  if Assigned(FEasingFunc) then
    FValue^ := FEasingFunc(FStartValue, FFinishValue - FStartValue, GetUnitValue);
end;

constructor TglrPSingleTweenItem.Create(aValue: PglrSingle; aStartValue,
  aFinishValue, aDuration: Single; aPauseOnStart: Single);
begin
  inherited Create(aStartValue, aFinishValue, aDuration, aPauseOnStart);
  FValue := aValue;
end;

procedure TglrSingleTweenItem.Update(const aDeltaTime: Single);
begin
  inherited Update(aDeltaTime);
  if Assigned(FSetSingleEvent) and Assigned(FEasingFunc) then
    FSetSingleEvent(FObject, FEasingFunc(FStartValue, FFinishValue - FStartValue, GetUnitValue));
end;

constructor TglrSingleTweenItem.Create(aObject: TglrTweenObject; aEvent: TglrSetSingle;
  aStartValue, aFinishValue, aDuration: Single; aPauseOnStart: Single);
begin
  inherited Create(aStartValue, aFinishValue, aDuration, aPauseOnStart);
  FObject := aObject;
  FSetSingleEvent := aEvent;
end;

constructor TglrBaseVectorTweenItem.Create(aStartValue, aFinishValue: TdfVec3f;
  aDuration: Single; aPauseOnStart: Single);
begin
  inherited Create(aDuration, aPauseOnStart);
  FStartValue := aStartValue;
  FFinishValue := aFinishValue;
end;

procedure TglrPVectorTweenItem.Update(const aDeltaTime: Single);
begin
  inherited Update(aDeltaTime);
  if Assigned(FEasingFunc) and ShouldChange then
  begin
    FValue^.x := FEasingFunc(FStartValue.x, FFinishValue.x - FStartValue.x, GetUnitValue);
    FValue^.y := FEasingFunc(FStartValue.y, FFinishValue.y - FStartValue.y, GetUnitValue);
    FValue^.z := FEasingFunc(FStartValue.z, FFinishValue.z - FStartValue.z, GetUnitValue);
  end;
end;

constructor TglrPVectorTweenItem.Create(aValue: PglrVec3f; aStartValue, aFinishValue: TdfVec3f;
  aDuration: Single; aPauseOnStart: Single);
begin
  inherited Create(aStartValue, aFinishValue, aDuration, aPauseOnStart);
  FValue := aValue;
end;

class function TglrBaseEasingFunctions.ExpoEaseIn(aStartValue, aDiffValue, aUnitValue: Single): Single;
begin
  if (aUnitValue = 1) then
    Result := aStartValue + aDiffValue
  else
    Result := aDiffValue * (-Pow(2, -15*aUnitValue) + 1) + aStartValue;
end;

class function TglrBaseEasingFunctions.QuintEaseOut(aStartValue, aDiffValue, aUnitValue: Single): Single;
begin
  Result := aStartValue;
end;

class function TglrBaseEasingFunctions.Simple(aStartValue, aDiffValue,
  aUnitValue: Single): Single;
begin
  if aUnitValue = 1 then
    Result := aStartValue + aDiffValue
  else
    Result := aStartValue + aDiffValue * aUnitValue;
end;

class function TglrBaseEasingFunctions.ElasticEaseIn(aStartValue, aDiffValue, aUnitValue: Single): Single;
begin
  if (aDiffValue = 0) or (aUnitValue = 0) or (aUnitValue = 1) then
  begin
    if (aUnitValue = 1) then
      Result := aStartValue + aDiffValue
    else
      Result := aStartValue;
    Exit;
  end;
  Result := (aDiffValue * Pow(2, -10 * aUnitValue) * Sin((aUnitValue - 0.25/4)*(2*3.14)/0.25)) + aDiffValue + aStartValue;
end;

function TglrTweener.GetTweenCount: Integer;
begin
  Result := FTweenItems.Count;
end;

function TglrTweener.GetItemByIndex(const aIndex: integer): TglrBaseTweenItem;
begin
  Result := TglrBaseTweenItem(FTweenItems[aIndex]);
end;

procedure TglrTweener.FreeByIndex(const aIndex: integer);
var
  TweenItem: TglrBaseTweenItem;
begin
  TweenItem := GetItemByIndex(aIndex);
  FreeAndNil(TweenItem);
  FTweenItems.DeleteByIndex(aIndex);
end;

procedure TglrTweener.FreeAll;
begin
  {TODO: rewrite}
  while FTweenItems.Count > 0 do
    FreeByIndex(0);  
end;

function TglrTweener.AddTweenItem(aTweenItem: TglrBaseTweenItem; aTweenStyle: TglrTweenStyle): Integer;
begin
  case aTweenStyle of
    tsElasticEaseIn:  aTweenItem.FEasingFunc := FEasingFunctions.ElasticEaseIn;
    tsElasticEaseOut: aTweenItem.FEasingFunc := FEasingFunctions.ElasticEaseIn;
    tsExpoEaseIn:     aTweenItem.FEasingFunc := FEasingFunctions.ExpoEaseIn;
    tsBounce:         aTweenItem.FEasingFunc := FEasingFunctions.ElasticEaseIn;
    tsSimple:         aTweenItem.FEasingFunc := FEasingFunctions.Simple;
  end;
  Result := FTweenItems.Add(aTweenItem);
end;

function TglrTweener.AddTweenPSingle(aVariable: PglrSingle; aTweenStyle: TglrTweenStyle;
  const aStartValue, aFinishValue, aDuration: Single;
  const aPauseOnStart: Single = 0): TglrPSingleTweenItem;
begin
  Result := TglrPSingleTweenItem.Create(aVariable, aStartValue, aFinishValue, aDuration, aPauseOnStart);
  AddTweenItem(Result, aTweenStyle);
end;

function TglrTweener.AddTweenPVector(aVariable: PglrVec3f; aTweenStyle: TglrTweenStyle;
  const aStartValue, aFinishValue: TdfVec3f; aDuration: Single;
  const aPauseOnStart: Single = 0): TglrPVectorTweenItem;
begin
  Result := TglrPVectorTweenItem.Create(aVariable, aStartValue, aFinishValue, aDuration, aPauseOnStart);
  AddTweenItem(Result, aTweenStyle);
end;

function TglrTweener.AddTweenSingle(aObject: TglrTweenObject; aSetValue: TglrSetSingle; aTweenStyle: TglrTweenStyle;
  const aStartValue, aFinishValue, aDuration: Single;
  const aPauseOnStart: Single = 0): TglrSingleTweenItem;
begin
  Result := TglrSingleTweenItem.Create(aObject, aSetValue, aStartValue, aFinishValue, aDuration, aPauseOnStart);
  AddTweenItem(Result, aTweenStyle);
end;
//------------------------------------------------------------------------------
Procedure TglrTweener.Update(const aDeltaTime: Single);
var
  i: integer;
  item: TglrBaseTweenItem;
begin
  {TODO: do not delete, sent to accum}
  i := 0;
  while (i < TweenCount)do
  begin
    item := GetItemByIndex(i);
    if item.Done then
      FreeByIndex(i)
    else
    begin
      item.Update(aDeltaTime);
      inc(i);
    end;
  end;
end;

constructor TglrTweener.Create;
begin
  inherited Create;
  FTweenItems := TglrList<Pointer>.Create;
  FEasingFunctions := TglrBaseEasingFunctions.Create;
end;

destructor TglrTweener.Destroy;
begin
  FreeAll;
  FreeAndNil(FEasingFunctions);
  FreeAndNil(FTweenItems);
  inherited Destroy;
end;


initialization
  Tweener := TglrTweener.Create;

finalization
  Tweener.Free;
end.
