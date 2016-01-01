unit glr_gamescreens;

interface

uses
  glr_core,
  glr_utils;

type
  // Usual lifecycle:
  // Hidden -> Loading -> Ready -> Paused/Unpaused -> Unloading -> Hidden
  TglrGameScreenState = ( gssHidden = 0, gssLoading, gssReady, gssPaused, gssUnloading );

  // GameScreen is something like a form, internal window.
  // Main menu is a GameScreen, pause menu is a GameScreen, even a game itself
  // is a GameScreen. Game level could be descendant of GameScreen

  { TglrGameScreen }

  TglrGameScreen = class abstract
  private
    procedure InternalUpdate(const DeltaTime: Double);
  protected
    fState: TglrGameScreenState;
    procedure SetState(NewState: TglrGameScreenState); virtual;
    procedure LoadCompleted(); virtual;
    procedure UnloadCompleted(); virtual;
  public
    Name: UnicodeString;
    constructor Create(ScreenName: UnicodeString); virtual;

    property State: TglrGameScreenState read fState write SetState;

    procedure OnUpdate        (const DeltaTime: Double); virtual; abstract;
    procedure OnLoadStarted   (); virtual;
    procedure OnUnloadStarted (); virtual;
    procedure OnRender        (); virtual; abstract;

    procedure OnInput(Event: PglrInputEvent); virtual; abstract;
  end;

  TglrGameScreenList = TglrObjectList<TglrGameScreen>;
  TglrGameScreenStack = TglrStack<TglrGameScreen>;

  { TglrGameScreenManager }

  TglrGameScreenManager = class (TglrGameScreenList)
  protected
    fUnloadingScreen: TglrGameScreen;
    function GetScreenIndex(ScreenName: UnicodeString): Integer; overload;
    function GetScreenIndex(Screen: TglrGameScreen):    Integer; overload;
  public
    ScreenStack: TglrGameScreenStack;

    constructor Create(aCapacity: LongInt = 4); override;
    destructor Destroy(); override;

    procedure ShowModal(const Index : Integer);        overload;
    procedure ShowModal(const Name  : UnicodeString);  overload;
    procedure ShowModal(const Screen: TglrGameScreen); overload;

    procedure SwitchTo(const Index : Integer);        overload;
    procedure SwitchTo(const Name  : UnicodeString);  overload;
    procedure SwitchTo(const Screen: TglrGameScreen); overload;

    procedure Back();

    procedure Update(const DeltaTime: Double);
    procedure Input(Event: PglrInputEvent);
    procedure Render();
  end;

implementation

{ TglrGameScreen }

procedure TglrGameScreen.SetState(NewState: TglrGameScreenState);
begin
  case fState of
    gssHidden:
      case NewState of
        gssUnloading: Exit();
        gssReady: NewState := gssLoading;
      end;

    gssLoading:
      case NewState of
        gssHidden: NewState := gssUnloading;
        gssReady, gssPaused: Exit();
      end;

    gssReady, gssPaused:
      case NewState of
        gssHidden: NewState := gssUnloading;
        gssLoading: NewState := gssReady;
      end;

    gssUnloading:
      case NewState of
        gssHidden, gssPaused: Exit();
        gssReady: NewState := gssLoading;
      end;
  end;


  if (fState <> NewState) then
  begin
    fState := NewState;
    case fState of
      gssLoading:   OnLoadStarted();
      gssUnloading: OnUnloadStarted();
    end;
  end;
end;

procedure TglrGameScreen.LoadCompleted;
begin
  // Load ended
  fState := gssReady;
end;

procedure TglrGameScreen.UnloadCompleted;
begin
  // Unload ended
  fState := gssHidden;
end;

procedure TglrGameScreen.InternalUpdate(const DeltaTime: Double);
begin
  case fState of
    gssHidden, gssPaused: ;
    gssLoading, gssReady, gssUnloading: OnUpdate(DeltaTime);
  end;
end;

constructor TglrGameScreen.Create(ScreenName: UnicodeString);
begin
  ScreenName := '';
  inherited Create();
  Name := ScreenName;
end;

procedure TglrGameScreen.OnLoadStarted;
begin
  LoadCompleted();
end;

procedure TglrGameScreen.OnUnloadStarted;
begin
  UnloadCompleted();
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
  fUnloadingScreen := nil;
end;

destructor TglrGameScreenManager.Destroy;
begin
  ScreenStack.Free();
  inherited Destroy;
end;

procedure TglrGameScreenManager.ShowModal(const Index: Integer);
begin
  if (index < 0) or (index >= FCount) then
    Log.Write(lCritical, 'GameScreenManager.ShowModal(index): index is out of range');

  // Pause current game screen
  if (not ScreenStack.IsEmpty()) then
    ScreenStack.Head().State := gssPaused;

  // Push new screen as current
  ScreenStack.Push(FItems[index]);

  // Start loading
  FItems[index].State := gssLoading;
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

procedure TglrGameScreenManager.SwitchTo(const Index: Integer);
begin
  if (index < 0) or (index >= FCount) then
    Log.Write(lCritical, 'GameScreenManager.ShowModal(index): index is out of range');

  // Unload current game screen
  if (not ScreenStack.IsEmpty()) then
  begin
    fUnloadingScreen := ScreenStack.Head();
    fUnloadingScreen.State := gssUnloading;
  end
  // If there is no current screen - start loading
  else
    FItems[index].State := gssLoading;

  // Push new screen as current
  ScreenStack.Push(FItems[index]);
end;

procedure TglrGameScreenManager.SwitchTo(const Name: UnicodeString);
var
  index: Integer;
begin
  index := GetScreenIndex(Name);
  if (index = -1) then
    Log.Write(lCritical, 'GameScreenManager: can not find screen with name "' + Name + '"');
  SwitchTo(index);
end;

procedure TglrGameScreenManager.SwitchTo(const Screen: TglrGameScreen);
var
  index: Integer;
begin
  index := GetScreenIndex(Screen);
  if (index = -1) then
    Log.Write(lCritical, 'GameScreenManager: can not find screen by ref');
  SwitchTo(index);
end;

procedure TglrGameScreenManager.Back();
begin
  // Pop current and unload it
  if (not ScreenStack.IsEmpty()) then
  begin
    fUnloadingScreen := ScreenStack.Pop();
    fUnloadingScreen.State := gssUnloading;
  end;
end;

procedure TglrGameScreenManager.Update(const DeltaTime: Double);
var
  i: Integer;
begin
  for i := 0 to FCount - 1 do
    FItems[i].InternalUpdate(DeltaTime);

  // if previous screen is unloaded (state = hidden)
  if (fUnloadingScreen <> nil) and (fUnloadingScreen.State = gssHidden) then
  begin
    fUnloadingScreen := nil;
    // Make current screen loading
    // If it is already loaded and paused, state will be automatically set to Ready
    // ( check GameScreen.SetState() )
    if (not ScreenStack.IsEmpty()) then
      ScreenStack.Head().State := gssLoading;
  end;
end;

procedure TglrGameScreenManager.Input(Event: PglrInputEvent);
begin
  // Process input only for current screen if it is ready or paused
  if (not ScreenStack.IsEmpty()) and (ScreenStack.Head().State in [gssReady, gssPaused]) then
    ScreenStack.Head().OnInput(Event);
end;

procedure TglrGameScreenManager.Render;
var
  i: Integer;
begin
  // Render all screens except of hidden
  for i := 0 to FCount - 1 do
    if (FItems[i].State <> gssHidden) then
      FItems[i].OnRender();
end;

end.

