unit uGame;

interface

uses
  glr_core;

type

  { TGame }

  TGame = class (TglrGame)

  public
    procedure OnFinish; override;
    procedure OnInput(aType: TglrInputType; aKey: TglrKey; X, Y,
      aOtherParam: Integer); override;
    procedure OnPause; override;
    procedure OnRender; override;
    procedure OnResize(aNewWidth, aNewHeight: Integer); override;
    procedure OnResume; override;
    procedure OnStart; override;
    procedure OnUpdate(const dt: Double); override;
  end;

var
  Game: TGame;

implementation

{ TGame }

procedure TGame.OnStart;
begin
  // Write here initialization code
end;

procedure TGame.OnFinish;
begin
  // Write here code for destroying all of your objects
end;

procedure TGame.OnInput(aType: TglrInputType; aKey: TglrKey; X, Y,
  aOtherParam: Integer);
begin
  // Calls when engine receives some input info
end;

procedure TGame.OnUpdate(const dt: Double);
begin
  // Place here game logic code
end;

procedure TGame.OnRender;
begin
  // It calls on every draw
end;

procedure TGame.OnPause;
begin
  // Calls when app' window has lost focus
end;

procedure TGame.OnResume;
begin
  // Calls when app' window was focused
end;

procedure TGame.OnResize(aNewWidth, aNewHeight: Integer);
begin
  // Calls when window has changed size
end;


end.

