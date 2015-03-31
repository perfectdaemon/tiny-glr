unit uGSMainMenu;

interface

uses
  glr_core,
  glr_gui,
  glr_gamescreens,
  glr_utils,
  glr_tween;

type

  { TglrMainMenu }

  TglrMainMenu = class (TglrGameScreen)
  protected
    NewGameBtn, SettingsBtn, ExitBtn: TglrGuiButton;
    GuiManager: TglrGuiManager;
    ActionManager: TglrActionManager;

    procedure ButtonInit(var Button: TglrGuiButton);
    procedure ButtonTween(aObject: TglrTweenObject; aValue: Single);
    procedure ButtonClicked(Sender: TglrGuiElement;   Event: PglrInputEvent);

    procedure MenuTween(aObject: TglrTweenObject; aValue: Single);

    procedure DoExit();
    procedure ToSettings();
    procedure Up(const DeltaTime: Double);
    procedure Right(const DeltaTime: Double);
    procedure Down(const DeltaTime: Double);
    procedure Left(const DeltaTime: Double);
  public
    constructor Create(ScreenName: UnicodeString); override;
    destructor Destroy; override;

    procedure OnInput(Event: PglrInputEvent); override;
    procedure OnRender; override;
    procedure OnUpdate    (const DeltaTime: Double); override;
    procedure OnLoadStarted(); override;
    procedure OnUnloadStarted(); override;
  end;

implementation

uses
  uAssets,
  uGame,
  glr_render,
  glr_math;

{ TglrMainMenu }

procedure TglrMainMenu.ButtonInit(var Button: TglrGuiButton);
begin
  Button := TglrGuiButton.Create();
  with Button do
  begin
    SetVerticesColor(Vec4f(0.5, 0.7, 0.5, 1.0));
    NormalTextureRegion := Assets.GuiAtlas.GetRegion(R_GUI_ATLAS_BUTTON);
    OverTextureRegion := Assets.GuiAtlas.GetRegion(R_GUI_ATLAS_BUTTON_OVER);
    TextLabel.Text := 'Button';
    TextLabel.Position := Vec3f(-100, -12, 0);
    //TextLabel.Color := Vec4f(0.7, 0.9, 0.9, 1.0);
    TextLabel.Scale := 1.12;
    Position := Vec3f(Render.Width div 2, 200, 1);
    OnClick := ButtonClicked;
  end;
end;

procedure TglrMainMenu.ButtonTween(aObject: TglrTweenObject; aValue: Single);
var
  v: TglrVec3f;
begin
  v := Vec3f(1,1,1);
  (aObject as TglrGuiButton).SetVerticesColor(Vec4f(v.Lerp(Vec3f(0.5, 0.7, 0.5), aValue), 1.0));
end;

procedure TglrMainMenu.ButtonClicked(Sender: TglrGuiElement;
  Event: PglrInputEvent);
begin
  Game.Tweener.AddTweenSingle(Sender, ButtonTween, tsExpoEaseIn, 0.0, 1.0, 2.0, 0.1);

  if (Sender = SettingsBtn) then
    ActionManager.AddIndependent(ToSettings, 0.2)
  else if (Sender = ExitBtn) then
    ActionManager.AddIndependent(DoExit, 0.2)
  else if (Sender = NewGameBtn) then
  begin
    ActionManager.AddToQueue(Up, 1.0);
    ActionManager.AddToQueue(Right, 1.0);
    ActionManager.AddToQueue(Down, 1.0);
    ActionManager.AddToQueue(Left, 1.0);
  end;
end;

procedure TglrMainMenu.MenuTween(aObject: TglrTweenObject; aValue: Single);
begin
  NewGameBtn.SetVerticesAlpha(aValue);
  SettingsBtn.SetVerticesAlpha(aValue);
  ExitBtn.SetVerticesAlpha(aValue);

  NewGameBtn.TextLabel.Color.w := aValue;
  SettingsBtn.TextLabel.Color.w := aValue;
  ExitBtn.TextLabel.Color.w := aValue;
end;

procedure TglrMainMenu.DoExit;
begin
  Core.Quit();
end;

procedure TglrMainMenu.ToSettings;
begin
  Game.GameScreenManager.SwitchTo('SettingsMenu');
end;

procedure TglrMainMenu.Up(const DeltaTime: Double);
begin
  NewGameBtn.Position.y -= 60 * DeltaTime;
end;

procedure TglrMainMenu.Right(const DeltaTime: Double);
begin
  NewGameBtn.Position.x += 60 * DeltaTime;
end;

procedure TglrMainMenu.Down(const DeltaTime: Double);
begin
  NewGameBtn.Position.y += 60 * DeltaTime;
end;

procedure TglrMainMenu.Left(const DeltaTime: Double);
begin
  NewGameBtn.Position.x -= 60 * DeltaTime;
end;

procedure TglrMainMenu.OnInput(Event: PglrInputEvent);
begin
  GuiManager.ProcessInput(Event, Assets.GuiCamera);
end;

procedure TglrMainMenu.OnRender;
begin
  Assets.GuiCamera.Update();
  GuiManager.Render();
end;

procedure TglrMainMenu.OnUpdate(const DeltaTime: Double);
begin
  GuiManager.Update(DeltaTime);
  ActionManager.Update(DeltaTime);
end;

procedure TglrMainMenu.OnLoadStarted;
begin
  Render.SetClearColor(0.1, 0.25, 0.25);
  Game.Tweener.AddTweenSingle(Self, MenuTween, tsExpoEaseIn, 0.0, 1.0, 1.5, 0.0);
  inherited OnLoadStarted;
end;

procedure TglrMainMenu.OnUnloadStarted;
begin
  Game.Tweener.AddTweenSingle(Self, MenuTween, tsExpoEaseIn, 1.0, 0.0, 1.5, 0.0);
  inherited OnUnloadStarted;
end;

constructor TglrMainMenu.Create(ScreenName: UnicodeString);
begin
  inherited Create(ScreenName);
  ActionManager := TglrActionManager.Create();

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
end;

destructor TglrMainMenu.Destroy();
begin
  ActionManager.Free();
  GuiManager.Free(True);
  inherited;
end;

end.

