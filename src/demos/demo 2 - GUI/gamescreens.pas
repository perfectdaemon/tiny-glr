unit gamescreens;

interface

uses
  glr_utils;

type
  // Usual lifecycle:
  // Hidden -> Loading -> Ready -> Paused/Unpaused -> Unloading -> Hidden
  TglrGameScreenState = ( gssHidden = 0, gssLoading, gssReady, gssPaused, gssUnloading );

  // GameScreen is something like form, internal window.
  // Main menu is GameScreen, pause menu is GameScreen, even a game itself
  // is a GameScreen. Game level could be descendant of GameScreen

  { TglrGameScreen }

  TglrGameScreen = class abstract
  protected
    fState: TglrGameScreenState;
    procedure SetState(const NewState: TglrGameScreenState); virtual;
    procedure InternalUpdate(const DeltaTime: Double);
  public
    Name: UnicodeString;
    constructor Create(ScreenName: UnicodeString); virtual;

    property State: TglrGameScreenState read fState write SetState;

    procedure OnUpdate    (const DeltaTime: Double); virtual; abstract;
    procedure OnLoading   (const DeltaTime: Double); virtual; abstract;
    procedure OnUnloading (const DeltaTime: Double); virtual; abstract;
  end;

  TglrGameScreenList = TglrObjectList<TglrGameScreen>;
  TglrGameScreenStack = TglrStack<TglrGameScreen>;

  { TglrGameScreenManager }

  TglrGameScreenManager = class (TglrGameScreenList)
  protected
    function GetScreenIndex(ScreenName: UnicodeString): Integer; overload;
    function GetScreenIndex(Screen: TglrGameScreen): Integer; overload;
  public
    ScreenStack: TglrGameScreenStack;

    constructor Create(aCapacity: LongInt = 4); override;
    destructor Destroy(); override;

    procedure ShowModal(const Index : Integer);        overload;
    procedure ShowModal(const Name  : UnicodeString);  overload;
    procedure ShowModal(const Screen: TglrGameScreen); overload;

    procedure SwitchTo(const Index : Integer;        Immediately: Boolean = False); overload;
    procedure SwitchTo(const Name  : UnicodeString;  Immediately: Boolean = False); overload;
    procedure SwitchTo(const Screen: TglrGameScreen; Immediately: Boolean = False); overload;

    procedure Update(const DeltaTime: Double);
  end;

implementation

{ TglrGameScreen }

procedure TglrGameScreen.SetState(const NewState: TglrGameScreenState);
begin
  fState := NewState;
end;

procedure TglrGameScreen.InternalUpdate(const DeltaTime: Double);
begin
  case fState of
    gssHidden:    ;
    gssLoading:   OnLoading(DeltaTime);
    gssReady:     OnUpdate(DeltaTime);
    gssPaused:    ;
    gssUnloading: OnUnloading(DeltaTime);
  end;
end;

constructor TglrGameScreen.Create(ScreenName: UnicodeString = '');
begin
  inherited Create();

  Name := ScreenName;
end;

{ TglrGameScreenManager }

function TglrGameScreenManager.GetScreenIndex(ScreenName: UnicodeString
  ): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to FCount - 1 do
    if (FItems[i].Name = ScreenName) then
      Exit(i);
end;

function TglrGameScreenManager.GetScreenIndex(Screen: TglrGameScreen): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to FCount - 1 do
    if (FItems[i] = Screen) then
      Exit(i);
end;

constructor TglrGameScreenManager.Create(aCapacity: LongInt);
begin
  inherited Create(aCapacity);
  ScreenStack := TglrGameScreenStack.Create(aCapacity);
end;

destructor TglrGameScreenManager.Destroy;
begin
  ScreenStack.Free();
  inherited Destroy;
end;

procedure TglrGameScreenManager.ShowModal(const Index: Integer);
begin
  Log.Write(lCritical, 'GameScreenManager.ShowModal(index) is not implemented');
end;

procedure TglrGameScreenManager.ShowModal(const Name: UnicodeString);
var
  index: Integer;
begin
  index := GetScreenIndex(Name);
  if (index = -1) then
    Log.Write(lCritical, 'GameScreenManager: can not find screen with name "' + Name + '"');
  ShowModal(index);
end;

procedure TglrGameScreenManager.ShowModal(const Screen: TglrGameScreen);
var
  index: Integer;
begin
  index := GetScreenIndex(Screen);
  if (index = -1) then
    Log.Write(lCritical, 'GameScreenManager: can not find screen by ref');
  ShowModal(index);
end;

procedure TglrGameScreenManager.SwitchTo(const Index: Integer;
  Immediately: Boolean);
begin
  Log.Write(lCritical, 'GameScreenManager.SwitchTo(index) is not implemented');
end;

procedure TglrGameScreenManager.SwitchTo(const Name: UnicodeString;
  Immediately: Boolean);
var
  index: Integer;
begin
  index := GetScreenIndex(Name);
  if (index = -1) then
    Log.Write(lCritical, 'GameScreenManager: can not find screen with name "' + Name + '"');
  SwitchTo(index, Immediately);
end;

procedure TglrGameScreenManager.SwitchTo(const Screen: TglrGameScreen;
  Immediately: Boolean);
var
  index: Integer;
begin
  index := GetScreenIndex(Screen);
  if (index = -1) then
    Log.Write(lCritical, 'GameScreenManager: can not find screen by ref');
  SwitchTo(index, Immediately);
end;

procedure TglrGameScreenManager.Update(const DeltaTime: Double);
var
  i: Integer;
begin
  for i := 0 to FCount - 1 do
    FItems[i].InternalUpdate(DeltaTime);
end;

end.

