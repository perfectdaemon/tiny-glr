unit uGSMainMenu;

interface

uses
  glr_core,
  glr_scene,
  glr_gui,
  glr_gamescreens,
  glr_utils,
  glr_tween;

type

  { TglrMainMenu }

  TglrMainMenu = class (TglrGameScreen)
  protected
    Container: TglrNode;

    GameName, AuthorName: TglrGuiLabel;

    NewGameBtn, SettingsBtn, ExitBtn: TglrGuiButton;
    GuiManager: TglrGuiManager;
    ActionManager: TglrActionManager;

    procedure ButtonInit(var Button: TglrGuiButton);
    procedure ButtonTween(aObject: TglrTweenObject; aValue: Single);
    procedure ButtonClicked(Sender: TglrGuiElement;   Event: PglrInputEvent);

    procedure DoExit();
    procedure ToSettings();
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
    SetVerticesColor(Colors.MenuButton);
    NormalTextureRegion := Assets.GuiAtlas.GetRegion(R_GUI_ATLAS_BUTTON);
    OverTextureRegion := Assets.GuiAtlas.GetRegion(R_GUI_ATLAS_BUTTON_OVER);
    TextLabel.Text := 'Button';
    TextLabel.Position := Vec3f(-115, -15, 0);
    TextLabel.Color := Colors.MenuButtonText;
    Position := Vec3f(Render.Width div 2, 240, 1);
    OnClick := ButtonClicked;
    Parent := Container;
  end;
end;

procedure TglrMainMenu.ButtonTween(aObject: TglrTweenObject; aValue: Single);
var
  v: TglrVec3f;
begin
  v := Vec3f(1,1,1);
  (aObject as TglrGuiButton).SetVerticesColor(Vec4f(v.Lerp(Vec3f(Colors.MenuButton), aValue), 1.0));
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

  end;
end;

procedure TglrMainMenu.DoExit;
begin
  Core.Quit();
end;

procedure TglrMainMenu.ToSettings;
begin
  Game.GameScreenManager.SwitchTo('SettingsMenu');
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
  Render.SetClearColor(Colors.MenuBackground);
  Game.Tweener.AddTweenPSingle(@Container.Position.y, tsExpoEaseIn, -Render.Height, 0, 1.5);
  inherited OnLoadStarted;
end;

procedure TglrMainMenu.OnUnloadStarted;
begin
  Game.Tweener.AddTweenPSingle(@Container.Position.y, tsExpoEaseIn, 0, -Render.Height, 1.5);
  ActionManager.AddIndependent(UnloadCompleted, 0.5);
end;

constructor TglrMainMenu.Create(ScreenName: UnicodeString);
begin
  inherited Create(ScreenName);
  ActionManager := TglrActionManager.Create();

  Container := TglrNode.Create();

  ButtonInit(NewGameBtn);
  ButtonInit(SettingsBtn);
  ButtonInit(ExitBtn);

  NewGameBtn.TextLabel.Text := Texts.MenuNewGame;
  SettingsBtn.TextLabel.Text := Texts.MenuSettings;
  ExitBtn.TextLabel.Text := Texts.MenuExit;

  SettingsBtn.Position += Vec3f(0, 70, 0);
  ExitBtn.Position += Vec3f(0, 140, 0);

  GameName := TglrGuiLabel.Create();
  GameName.Position := Vec3f(Render.Width div 2, 70, 5);
  GameName.TextLabel.Text := Texts.MenuTitle;
  GameName.TextLabel.Scale := 1.2;
  GameName.TextLabel.PivotPoint := Vec2f(0.5, 0.0);
  GameName.TextLabel.ShadowEnabled := True;
  GameName.TextLabel.ShadowOffset := Vec2f(4,4);
  GameName.TextLabel.Color := Colors.MenuText;
  GameName.Parent := Container;

  AuthorName := TglrGuiLabel.Create();
  AuthorName.Position := Vec3f(Render.Width div 2, Render.Height - 50, 5);
  AuthorName.TextLabel.Text := Texts.MenuAuthorName;
  AuthorName.TextLabel.Scale := 0.7;
  AuthorName.TextLabel.PivotPoint := Vec2f(0.5, 0.5);
  AuthorName.TextLabel.ShadowEnabled := True;
  AuthorName.TextLabel.ShadowOffset := Vec2f(3,3);
  AuthorName.TextLabel.Color := Colors.MenuText;
  AuthorName.Parent := Container;

  GuiManager := TglrGuiManager.Create(Assets.GuiMaterial, Assets.FontMain);
  GuiManager.Add(NewGameBtn);
  GuiManager.Add(SettingsBtn);
  GuiManager.Add(ExitBtn);
  GuiManager.Add(GameName);
  GuiManager.Add(AuthorName);
end;

destructor TglrMainMenu.Destroy();
begin
  Container.Free();
  ActionManager.Free();
  GuiManager.Free(True);
  inherited;
end;

end.

