unit uGSMainMenu;

interface

uses
  glr_core,
  glr_render2d,
  glr_gui,
  glr_gamescreens;

type

  { TglrMainMenu }

  TglrMainMenu = class (TglrGameScreen)
  protected
    NewGame, Settings, Exit: TglrGuiButton;
  public
    procedure OnInput(aType: TglrInputType; aKey: TglrKey; X, Y,
      aOtherParam: Integer); override;
    procedure OnRender; override;
    procedure OnUpdate    (const DeltaTime: Double); override;
    procedure OnLoading   (const DeltaTime: Double); override;
    procedure OnUnloading (const DeltaTime: Double); override;
  end;

implementation

{ TglrMainMenu }

procedure TglrMainMenu.OnInput(aType: TglrInputType; aKey: TglrKey; X, Y,
  aOtherParam: Integer);
begin

end;

procedure TglrMainMenu.OnRender;
begin

end;

procedure TglrMainMenu.OnUpdate(const DeltaTime: Double);
begin

end;

procedure TglrMainMenu.OnLoading(const DeltaTime: Double);
begin
  inherited OnLoading(DeltaTime);
end;

procedure TglrMainMenu.OnUnloading(const DeltaTime: Double);
begin
  inherited OnUnloading(DeltaTime);
end;

end.

