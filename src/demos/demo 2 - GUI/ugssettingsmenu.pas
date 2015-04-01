unit uGSSettingsMenu;

interface

uses
  glr_gamescreens,
  glr_utils,
  glr_core,
  glr_scene,
  glr_render2d,
  glr_gui,
  glr_tween;

type

  { TglrSettingsMenu }

  TglrSettingsMenu = class (TglrGameScreen)
  protected
    ActionManager: TglrActionManager;
    GuiManager: TglrGuiManager;

    Container: TglrNode;
    ApplyBtn, BackBtn: TglrGuiButton;

    MusicSlider, SoundSlider: TglrGuiSlider;

    procedure ButtonInit(var Button: TglrGuiButton);
    procedure ButtonTween(aObject: TglrTweenObject; aValue: Single);
    procedure ButtonClicked(Sender: TglrGuiElement; Event: PglrInputEvent);

    procedure SliderInit(var Slider: TglrGuiSlider);
    procedure SliderOver(Sender: TglrGuiElement; Event: PglrInputEvent);
    procedure SliderValueChanged(Sender: TglrGuiElement; NewValue: Integer);

    procedure MenuTween(aObject: TglrTweenObject; aValue: Single);

    procedure BackToMainMenu();
    procedure Apply();
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
  uGame,
  uAssets,
  glr_math,
  glr_render;

{ TglrSettingsMenu }

procedure TglrSettingsMenu.ButtonInit(var Button: TglrGuiButton);
begin
  Button := TglrGuiButton.Create();
  with Button do
  begin
    SetVerticesColor(Vec4f(0.5, 0.7, 0.5, 1.0));
    NormalTextureRegion := Assets.GuiAtlas.GetRegion(R_GUI_ATLAS_BUTTON);
    OverTextureRegion := Assets.GuiAtlas.GetRegion(R_GUI_ATLAS_BUTTON_OVER);
    TextLabel.Text := 'Button';
    TextLabel.Position := Vec3f(-115, -12, 0);
    TextLabel.Color := Color4f(1,1,1);
    Position := Vec3f(Render.Width - Width / 2 - 50, Render.Height - Height - 80, 1);
    OnClick := ButtonClicked;
    Parent := Container;
  end;
end;

procedure TglrSettingsMenu.ButtonTween(aObject: TglrTweenObject; aValue: Single);
var
  v: TglrVec3f;
begin
  v := Vec3f(1,1,1);
  (aObject as TglrGuiButton).SetVerticesColor(Vec4f(v.Lerp(Vec3f(0.5, 0.7, 0.5), aValue), 1.0));
end;

procedure TglrSettingsMenu.ButtonClicked(Sender: TglrGuiElement;
  Event: PglrInputEvent);
begin
  Game.Tweener.AddTweenSingle(Sender, ButtonTween, tsExpoEaseIn, 0.0, 1.0, 2.0, 0.1);

  if (Sender = BackBtn) then
    ActionManager.AddIndependent(BackToMainMenu, 0.2);

  if (Sender = ApplyBtn) then
  begin
    ActionManager.AddIndependent(Apply, 0.2);
  end;
end;

procedure TglrSettingsMenu.SliderInit(var Slider: TglrGuiSlider);
begin
  Slider := TglrGuiSlider.Create();
  with Slider do
  begin
    Position := Vec3f(Render.Width div 2, 150, 5);
    SetVerticesColor(Vec4f(0.5, 0.7, 0.5, 1.0));
    NormalTextureRegion := Assets.GuiAtlas.GetRegion(R_GUI_ATLAS_SLIDER_BACK);
    Fill.SetTextureRegion(Assets.GuiAtlas.GetRegion(R_GUI_ATLAS_SLIDER_FILL));
    Fill.SetVerticesColor(Vec4f(0.5, 0.7, 0.5, 1.0));
    Button.NormalTextureRegion := Assets.GuiAtlas.GetRegion(R_GUI_ATLAS_SLIDER_BTN);
    Button.SetVerticesColor(Vec4f(0.5, 0.7, 0.5, 1.0));
    Parent := Container;
    OnValueChanged := SliderValueChanged;
    OnMouseOver := SliderOver;
    OnMouseOut := SliderOver;
    ChangeTexCoords := True;
  end;
end;

procedure TglrSettingsMenu.SliderOver(Sender: TglrGuiElement;
  Event: PglrInputEvent);
begin
  if (Sender.IsMouseOver) then
  begin
    TglrGuiSlider(Sender).Button.SetVerticesColor(Vec4f(0.7, 0.5, 0.5, 1.0));
    TglrGuiSlider(Sender).Fill.SetVerticesColor(Vec4f(0.7, 0.5, 0.5, 1.0));
  end
  else
  begin
    TglrGuiSlider(Sender).Button.SetVerticesColor(Vec4f(0.5, 0.7, 0.5, 1.0));
    TglrGuiSlider(Sender).Fill.SetVerticesColor(Vec4f(0.5, 0.7, 0.5, 1.0));
  end;
end;

procedure TglrSettingsMenu.SliderValueChanged(Sender: TglrGuiElement;
  NewValue: Integer);
begin

end;

procedure TglrSettingsMenu.MenuTween(aObject: TglrTweenObject; aValue: Single);
begin
  ApplyBtn.SetVerticesAlpha(aValue);
  BackBtn.SetVerticesAlpha(aValue);
  MusicSlider.SetVerticesAlpha(aValue);
end;

procedure TglrSettingsMenu.BackToMainMenu;
begin
  Game.GameScreenManager.Back();
end;

procedure TglrSettingsMenu.Apply;
begin
  // Do some stuff
end;

constructor TglrSettingsMenu.Create(ScreenName: UnicodeString);
begin
  inherited Create(ScreenName);

  Container := TglrNode.Create();

  ActionManager := TglrActionManager.Create();

  ButtonInit(ApplyBtn);
  ButtonInit(BackBtn);

  SliderInit(MusicSlider);
  MusicSlider.Value := 50;

  ApplyBtn.TextLabel.Text := 'Apply';
  BackBtn.TextLabel.Text := 'Back';

  BackBtn.Position.y += 70;

  GuiManager := TglrGuiManager.Create(Assets.GuiMaterial, Assets.FontMain);
  GuiManager.Add(ApplyBtn);
  GuiManager.Add(BackBtn);
  GuiManager.Add(MusicSlider);
end;

destructor TglrSettingsMenu.Destroy;
begin
  Container.Free();
  ActionManager.Free();
  GuiManager.Free(True);
  inherited Destroy;
end;

procedure TglrSettingsMenu.OnInput(Event: PglrInputEvent);
begin
  GuiManager.ProcessInput(Event, Assets.GuiCamera);
  case Event.InputType of
    itKeyDown:
      case Event.Key of
        kLeft: MusicSlider.Value := MusicSlider.Value - 5;
        kRight: MusicSlider.Value := MusicSlider.Value + 5;
      end;
  end;
end;

procedure TglrSettingsMenu.OnRender;
begin
  Assets.GuiCamera.Update();
  GuiManager.Render();
end;

procedure TglrSettingsMenu.OnUpdate(const DeltaTime: Double);
begin
  GuiManager.Update(DeltaTime);
  ActionManager.Update(DeltaTime);
end;

procedure TglrSettingsMenu.OnLoadStarted;
begin
  Game.Tweener.AddTweenSingle(Self, MenuTween, tsExpoEaseIn, 0.0, 1.0, 1.5, 0.4);
  Game.Tweener.AddTweenPSingle(@Container.Position.x, tsExpoEaseIn, -800, 0, 2.5);
  inherited OnLoadStarted;
end;

procedure TglrSettingsMenu.OnUnloadStarted;
begin
  Game.Tweener.AddTweenSingle(Self, MenuTween, tsExpoEaseIn, 1.0, 0.0, 1.5, 0.4);
  Game.Tweener.AddTweenPSingle(@Container.Position.x, tsExpoEaseIn, 0, -800, 1.0, 0, Self.UnloadCompleted);
end;

end.

