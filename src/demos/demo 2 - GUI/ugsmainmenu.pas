unit uGSMainMenu;

interface

uses
  glr_core,
  glr_gui,
  glr_gamescreens;

type

  { TglrMainMenu }

  TglrMainMenu = class (TglrGameScreen)
  protected
    NewGameBtn, SettingsBtn, ExitBtn: TglrGuiButton;
    GuiManager: TglrGuiManager;

    procedure ButtonInit(var button: TglrGuiButton);

    //procedure ButtonMouseOver(Sender: TglrGuiElement; aType: TglrInputType; aKey: TglrKey; X, Y: Single; aOtherParam: Integer);
    //procedure ButtonMouseOut(Sender: TglrGuiElement;  aType: TglrInputType; aKey: TglrKey; X, Y: Single; aOtherParam: Integer);
    //procedure ButtonClicked(Sender: TglrGuiElement;   aType: TglrInputType; aKey: TglrKey; X, Y: Single; aOtherParam: Integer);
  public
    procedure OnInput(Event: PglrInputEvent); override;
    procedure OnRender; override;
    procedure OnUpdate    (const DeltaTime: Double); override;
    procedure OnLoading   (const DeltaTime: Double); override;
    procedure OnUnloading (const DeltaTime: Double); override;
  end;

implementation

uses
  uAssets,
  glr_render,
  glr_math;

{ TglrMainMenu }

procedure TglrMainMenu.ButtonInit(var button: TglrGuiButton);
begin
  button := TglrGuiButton.Create();
  with button do
  begin
    SetVerticesColor(Vec4f(0.5, 0.7, 0.5, 1.0));
    NormalTextureRegion := Assets.GuiAtlas.GetRegion(R_GUI_ATLAS_BUTTON);
    OverTextureRegion := Assets.GuiAtlas.GetRegion(R_GUI_ATLAS_BUTTON_OVER);
    TextLabel.Text := 'Button';
    TextLabel.Position := Vec3f(-100, -10, 0);
    Position := Vec3f(Render.Width div 2, 200, 1);
  end;
end;

procedure TglrMainMenu.OnInput(Event: PglrInputEvent);
begin
  GuiManager.ProcessInput(Event, Assets.GuiCamera);
end;

procedure TglrMainMenu.OnRender;
begin
  if not (State in [gssReady, gssPaused]) then
    Exit();

  Assets.GuiCamera.Update();
  GuiManager.Render();
end;

procedure TglrMainMenu.OnUpdate(const DeltaTime: Double);
begin
  GuiManager.Update(DeltaTime);
end;

procedure TglrMainMenu.OnLoading(const DeltaTime: Double);
begin
  Render.SetClearColor(0.1, 0.25, 0.25);

  ButtonInit(NewGameBtn);
  ButtonInit(SettingsBtn);
  ButtonInit(ExitBtn);

  NewGameBtn.TextLabel.Text := 'New Game';
  SettingsBtn.TextLabel.Text := 'Settings';
  ExitBtn.TextLabel.Text := 'Exit';

  SettingsBtn.Position.y += 70;
  ExitBtn.Position.y += 140;

  GuiManager := TglrGuiManager.Create(Assets.GuiMaterial, Assets.FontMain);
  GuiManager.Add(NewGameBtn);
  GuiManager.Add(SettingsBtn);
  GuiManager.Add(ExitBtn);

  inherited OnLoading(DeltaTime); // load completed
end;

procedure TglrMainMenu.OnUnloading(const DeltaTime: Double);
begin
  GuiManager.Free(True);
  inherited OnUnloading(DeltaTime);
end;

end.

