unit glr_core;

{$i defines.inc}

interface

uses
  glr_math, glr_render, glr_render2d;

type

  { TglrInput }

  TglrInputType = ( itTouchDown, itTouchUp, itTouchMove, itKeyDown, itKeyUp, itWheel );

  TglrTouch = record
    IsDown: Boolean;
    Start, Pos: TglrVec2f;
  end;

  TglrKey = (
  {$IFDEF WINDOWS}
    kNoInput = $00,

    kLeftButton = $01,
    kRightButton, kCancel, kMiddleButton, kXButton1, kXButton2,

    kBack = $08, kTab,

    kClear = $0C, kReturn,

    kShift = $10, kCtrl, kAlt, kPause, kCapsLock,

    kEscape = $1B,

    kSpace = $20, kPageUp, kPageDown,
    kEnd, kHome, kLeft, kUp, kRight, kDown,

    kPrintScreen = $2C, kInsert, kDelete,

    k0 = $30, k1, k2, k3, k4, k5, k6, k7, k8, k9,
    kA = $41, kB, kC, kD, kE, kF, kG, kH, kI, kJ, kK, kL, kM, kN, kO, kP, kQ, kR, kS, kT, kU, kV, kW, kX, kY, kZ,

    kLeftWin = $5B, kRightWin,

    kNumPad0 = $60, kNumPad1, kNumPad2, kNumPad3, kNumPad4, kNumPad5, kNumPad6, kNumPad7, kNumPad8, kNumPad9,
    kNumPadMul, kNumPadAdd, kNumPadSeparator, kNumPadSub, kNumPadDecimal, kNumPadDiv,

    kF1, kF2, kF3, kF4, kF5, kF6, kF7, kF8, kF9, kF10, kF11, kF12, kF13, kF14, kF15, kF16, kF17, kF18, kF19, kF20, kF21, kF22, kF23, kF24,

    kNumLock = $90, kScrollLock,

    kWheelUp = $97, kWheelDown, //engine defined, according to win api reference these codes are not assigned
    kPlus = $BB, kMinus = $BD
  {$ENDIF}
  );

  { TglrInputEvent }

  TglrInputEvent = record
    InputType: TglrInputType;
    Key: TglrKey;
    X, Y, W: Integer;
    _inited: Boolean;     // Internal field
    procedure Init(aType: TglrInputType; aKey: TglrKey; aX, aY, aW: Integer);
  end;

  PglrInputEvent = ^TglrInputEvent;

  TglrInput = class
    Touch: array[0..9] of TglrTouch;
    MousePos: TglrVec2f;
    KeyDown: array[Low(TglrKey)..High(TglrKey)] of Boolean;
    lastWheelDelta: Integer;

    procedure Process(Event: PglrInputEvent);
    function GetKeyName(aKey: TglrKey): AnsiString;
    function GetInputTypeName(aType: TglrInputType): AnsiString;
  end;

  { TglrAppView }

  TglrAppView = class abstract
  public
    constructor Create(aData: Pointer); virtual; abstract;
    destructor Destroy(); override; abstract;

    procedure Loop(); virtual; abstract;
    procedure Quit(); virtual; abstract;
  end;

  { TglrGame }

  TglrGame = class abstract
  public
    procedure OnStart(); virtual; abstract;
    procedure OnFinish(); virtual; abstract;

    procedure OnPause(); virtual; abstract;
    procedure OnResume(); virtual; abstract;
    procedure OnResize(aNewWidth, aNewHeight: Integer); virtual; abstract;

    procedure OnUpdate(const dt: Double); virtual; abstract;
    procedure OnRender(); virtual; abstract;

    procedure OnInput(Event: PglrInputEvent); virtual; abstract;
  end;

  { Core }

  TglrInitParams = record
    X, Y, Width, Height: Integer;
    Caption: AnsiString;
    vSync: Boolean;
    PackFilesPath: AnsiString;
    UseDefaultAssets: Boolean;
  {$IFDEF WINDOWS}
  {$ENDIF}
  end;

  Core = class
  protected
    class var fGame: TglrGame;
    class var fAppView: TglrAppView;
    class var fFPS: Single;
    class var fDT: Double;
    class var fReady: Boolean;
    class function GetFPSText(): AnsiString; static;
  public
    class var Input: TglrInput;

    class procedure Init(aGame: TglrGame; aInitParams: TglrInitParams);
    class procedure DeInit();

    class procedure Resize(aNewWidth, aNewHeight: Integer);
    class procedure InputReceived(Event: PglrInputEvent);

    class procedure Loop();
    class procedure Pause();
    class procedure Resume();

    class procedure Quit();

    class procedure Update(const dt: Double);
    class procedure RenderAll();

    class property FPS: Single read fFPS;
    class property FPSText: AnsiString read GetFPSText; //preformatted with 1 digit after delimiter
    class property DeltaTime: Double read fDT;
    class property IsReady: Boolean read fReady;
  end;

  { Default }

  Default = class
  protected
    class procedure Init();
    class procedure Deinit();
  public
    class var fInited: Boolean;
    class var SpriteShader: TglrShaderProgram;
    class var BlankTexture: TglrTexture;
    class var Font: TglrFont;
  end;

implementation

uses
  {$IFDEF WIN32}
  glr_os_win,
  {$ENDIF}
  glr_utils, glr_filesystem;

{ TglrInputEvent }

procedure TglrInputEvent.Init(aType: TglrInputType; aKey: TglrKey; aX,
  aY, aW: Integer);
begin
  InputType := aType;
  Key := aKey;
  X := aX;
  Y := aY;
  W := aW;
  _inited := True;
end;

{ TglrInput }

procedure TglrInput.Process(Event: PglrInputEvent);
var
  k: ^Boolean;
begin
  k := @KeyDown[Event.Key];
  case Event.InputType of
    itTouchDown:
      with Touch[Ord(Event.Key)] do
      begin
        IsDown := True;
        Start := Vec2f(Event.X, Event.Y);
        Pos := Start;
      end;
    itTouchUp:
      Touch[Ord(Event.Key)].IsDown := False;
    itTouchMove:
    begin
      Touch[Ord(Event.Key)].Pos := Vec2f(Event.X, Event.Y);
      MousePos := Vec2f(Event.X, Event.Y);
    end;
    itKeyDown:
      k^ := True;
    itKeyUp:
      k^ := False;
    itWheel:
      lastWheelDelta := Event.W;
  end;
end;

function TglrInput.GetKeyName(aKey: TglrKey): AnsiString;
var
  l: Integer;
begin
  WriteStr(Result, aKey);
  l := Length(Result) - 1;
  Move(Result[2], Result[1], l);
  SetLength(Result, l);
end;

function TglrInput.GetInputTypeName(aType: TglrInputType): AnsiString;
var
  l: Integer;
begin
  WriteStr(Result, aType);
  l := Length(Result) - 2;
  Move(Result[3], Result[1], l);
  SetLength(Result, l);
end;


{ Core }

class procedure Core.Resize(aNewWidth, aNewHeight: Integer);
begin
  Render.Resize(aNewWidth, aNewHeight);
  fGame.OnResize(aNewWidth, aNewHeight);
end;

class procedure Core.InputReceived(Event: PglrInputEvent);
begin
  Input.Process(Event);
  fGame.OnInput(Event);
end;

type
  EAssertationFailed = class(TObject)
  private
    fMessage: AnsiString;
  public
    constructor Create(const aMsg: Ansistring);
  end;

  constructor EAssertationFailed.Create(const aMsg: Ansistring);
  begin
    inherited Create();
    fMessage := aMsg;
  end;

procedure AssertErrorHandler(const aMessage, aFileName: ShortString; aLineNo: LongInt; aAddr: Pointer);
begin
  raise EAssertationFailed.Create(
    #13#10 + aMessage +
    #13#10 + 'file: ' + aFileName +
    #13#10 + 'line: ' + Convert.ToString(aLineNo) +
    #13#10 + 'addr: ' + Convert.ToString(aAddr));
end;

class function Core.GetFPSText: AnsiString;
begin
  Result := Convert.ToString(fFPS, 1);
end;

class procedure Core.Init(aGame: TglrGame; aInitParams: TglrInitParams);
begin
  AssertErrorProc := @AssertErrorHandler;
  fGame := aGame;
  Log.Init(LOG_FILE);
  Input := TglrInput.Create();
  FileSystem.Init(aInitParams.PackFilesPath);
  fReady := False;

  fAppView :=
  {$IFDEF WINDOWS}TglrWindow{$ENDIF}
    .Create(@aInitParams);
  Render.Init();
  Render.SetVerticalSync(aInitParams.vSync);
  Render.SetClearColor(0.2, 0.21, 0.25);
  Render.SetViewPort(0, 0, aInitParams.Width, aInitParams.Height);
  if (aInitParams.UseDefaultAssets) then
    Default.Init();

  Log.Write(lInformation, 'Initialize completed'#13#10);
end;

class procedure Core.Loop();
begin
  fGame.OnStart();
  fReady := True;
  Log.Write(lInformation, 'Appication loop started');
  fAppView.Loop();
  fReady := False;
  fGame.OnFinish();
  Log.Write(lInformation, 'Appication loop finished');
end;

class procedure Core.Pause();
begin
  fGame.OnPause();
end;

class procedure Core.Resume();
begin
  fGame.OnResume();
end;

class procedure Core.Quit;
begin
  fAppView.Quit();
end;

class procedure Core.Update(const dt: Double);
begin
  fDT := dt;
  fFPS := 1 / fDT;
  fGame.OnUpdate(dt);
end;

class procedure Core.RenderAll();
begin
  Render.Clear(cmAll);
  Render.ResetStates();
  Render.fStatTextureBind := 0;
  Render.fTriCount := 0;
  Render.fDIPCount := 0;
  fGame.OnRender();
end;

class procedure Core.DeInit();
begin
  Default.Deinit();
  fAppView.Free();
  Render.DeInit();
  FileSystem.DeInit();
  Input.Free();
  Log.Deinit();
end;

{ Default }

class procedure Default.Init;
var
  blankData: Pointer;
begin
  fInited := True;
  SpriteShader := TglrShaderProgram.Create();
  SpriteShader.Attach(FileSystem.ReadResource('default assets/SpriteShaderV.txt'), stVertex);
  SpriteShader.Attach(FileSystem.ReadResource('default assets/SpriteShaderF.txt'), stFragment);
  SpriteShader.Link();

  GetMem(blankData, 1 * 1 * 3(*RGB*));
  FillChar(blankData^, 3, $FF);
  BlankTexture := TglrTexture.Create(blankData, 1, 1, tfRGB8);
  FreeMem(blankData);

  Font := TglrFont.Create(FileSystem.ReadResource('default assets/HelveticaLight13.fnt'), True);
end;

class procedure Default.Deinit;
begin
  if (not fInited) then
    Exit();

  SpriteShader.Free();
  BlankTexture.Free();
  Font.Free();

  fInited := False;
end;

end.

