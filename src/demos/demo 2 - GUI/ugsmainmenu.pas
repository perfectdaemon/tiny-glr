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
    NewGameBtn, SettingsBtn, ExitBtn: TglrGuiButton;
    GuiManager: TglrGuiManager;

    //procedure ButtonMouseOver(Sender: TglrGuiElement; aType: TglrInputType; aKey: TglrKey; X, Y: Single; aOtherParam: Integer);
    //procedure ButtonMouseOut(Sender: TglrGuiElement;  aType: TglrInputType; aKey: TglrKey; X, Y: Single; aOtherParam: Integer);
    //procedure ButtonClicked(Sender: TglrGuiElement;   aType: TglrInputType; aKey: TglrKey; X, Y: Single; aOtherParam: Integer);
  public
    procedure OnInput(aType: TglrInputType; aKey: TglrKey; X, Y,
      aOtherParam: Integer); override;
    procedure OnRender; override;
    procedure OnUpdate    (const DeltaTime: Double); override;
    procedure OnLoading   (const DeltaTime: Double); override;
    procedure OnUnloading (const DeltaTime: Double); override;
  end;

implementation

uses
  uAssets,
  glr_math;

{ TglrMainMenu }

procedure TglrMainMenu.OnInput(aType: TglrInputType; aKey: TglrKey; X, Y,
  aOtherParam: Integer);
begin
  GuiManager.ProcessInput(aType, aKey, X, Y, aOtherParam, Assets.GuiCamera);
end;

procedure TglrMainMenu.OnRender;
begin
  if not (State in [gssReady, gssPaused]) then
    Exit();
  with Assets do
  begin
    GuiCamera.Update();
    GuiMaterial.Bind();
    GuiSpriteBatch.Start();
      GuiSpriteBatch.Draw(NewGameBtn);
      GuiSpriteBatch.Draw(SettingsBtn);
      GuiSpriteBatch.Draw(ExitBtn);
    GuiSpriteBatch.Finish();

    FontMainBatch.Start();
      FontMainBatch.Draw(NewGameBtn.Text);
      FontMainBatch.Draw(SettingsBtn.Text);
      FontMainBatch.Draw(ExitBtn.Text);
    FontMainBatch.Finish();
  end;
end;

procedure TglrMainMenu.OnUpdate(const DeltaTime: Double);
begin
  GuiManager.Update(DeltaTime);
end;

procedure TglrMainMenu.OnLoading(const DeltaTime: Double);
begin
  NewGameBtn := TglrGuiButton.Create();
  NewGameBtn.NormalTextureRegion := Assets.GuiAtlas.GetRegion(R_GUI_ATLAS_BUTTON);
  NewGameBtn.OverTextureRegion := Assets.GuiAtlas.GetRegion(R_GUI_ATLAS_BUTTON_OVER);
  NewGameBtn.SetTextureRegion(NewGameBtn.NormalTextureRegion);
  NewGameBtn.Text.Text := 'New Game';
  NewGameBtn.Text.Position.z := 2;
  NewGameBtn.Position := Vec3f(200, 100, 1);

  SettingsBtn := TglrGuiButton.Create();
  SettingsBtn.NormalTextureRegion := Assets.GuiAtlas.GetRegion(R_GUI_ATLAS_BUTTON);
  SettingsBtn.OverTextureRegion := Assets.GuiAtlas.GetRegion(R_GUI_ATLAS_BUTTON_OVER);
  SettingsBtn.SetTextureRegion(SettingsBtn.NormalTextureRegion);
  SettingsBtn.Text.Text := 'Settings';
  SettingsBtn.Text.Position.z := 2;
  SettingsBtn.Position := Vec3f(200, 200, 1);

  ExitBtn := TglrGuiButton.Create();
  ExitBtn.NormalTextureRegion := Assets.GuiAtlas.GetRegion(R_GUI_ATLAS_BUTTON);
  ExitBtn.OverTextureRegion := Assets.GuiAtlas.GetRegion(R_GUI_ATLAS_BUTTON_OVER);
  ExitBtn.SetTextureRegion(ExitBtn.NormalTextureRegion);
  ExitBtn.Text.Position.z := 2;
  ExitBtn.Text.Text := 'Exit';
  ExitBtn.Position := Vec3f(200, 300, 1);

  GuiManager := TglrGuiManager.Create();
  GuiManager.Add(NewGameBtn);
  GuiManager.Add(SettingsBtn);
  GuiManager.Add(ExitBtn);

  inherited OnLoading(DeltaTime);
end;

procedure TglrMainMenu.OnUnloading(const DeltaTime: Double);
begin
  GuiManager.Free(True);
  inherited OnUnloading(DeltaTime);
end;

end.

