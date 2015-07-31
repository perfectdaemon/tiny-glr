unit uGSGame;

interface

uses
  glr_core,
  glr_gamescreens;

type

  { TglrTheGame }

  TglrTheGame = class (TglrGameScreen)
  public
    //Player: TglrPlayer;
    constructor Create(ScreenName: UnicodeString); override;
    destructor Destroy; override;

    procedure OnInput(Event: PglrInputEvent); override;
    procedure OnRender; override;
    procedure OnUpdate    (const DeltaTime: Double); override;

    procedure OnLoadStarted(); override;
    procedure OnUnloadStarted(); override;
  end;

implementation

{ TglrTheGame }

constructor TglrTheGame.Create(ScreenName: UnicodeString);
begin
  inherited Create(ScreenName);
end;

destructor TglrTheGame.Destroy;
begin
  inherited Destroy;
end;

procedure TglrTheGame.OnInput(Event: PglrInputEvent);
begin

end;

procedure TglrTheGame.OnRender;
begin

end;

procedure TglrTheGame.OnUpdate(const DeltaTime: Double);
begin

end;

procedure TglrTheGame.OnLoadStarted;
begin
  inherited OnLoadStarted;
end;

procedure TglrTheGame.OnUnloadStarted;
begin
  inherited OnUnloadStarted;
end;

end.

