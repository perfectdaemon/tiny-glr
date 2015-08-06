unit uGSSettingsMenu;

interface

uses
  glr_gamescreens,
  glr_utils,
  glr_core,
  glr_scene,
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
    MusicSliderLabel, SoundSliderLabel: TglrGuiLabel;

    ImportantCheckBox1, ImportantCheckBox2: TglrGuiCheckBox;
    ICBLabel1, ICBLabel2: TglrGuiLabel;

    procedure ButtonInit(var Button: TglrGuiButton);
    procedure ButtonTween(aObject: TglrTweenObject; aValue: Single);
    procedure ButtonClicked(Sender: TglrGuiElement; Event: PglrInputEvent);
    procedure ButtonOver(Sender: TglrGuiElement; Event: PglrInputEvent);

    procedure SliderAndLabelInit(var Slider: TglrGuiSlider;
      var SLabel: TglrGuiLabel);
    procedure SliderOver(Sender: TglrGuiElement; Event: PglrInputEvent);
    procedure SliderValueChanged(Sender: TglrGuiElement; NewValue: Integer);

    procedure CheckBoxAndLabelInit(var CheckBox: TglrGuiCheckBox;
      var CBLabel: TglrGuiLabel);

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
    SetVerticesColor(Colors.MenuButton);
    SetSize(Sizes.ButtonSize);
    TextLabel.Text := 'Button';
    TextLabel.Position := Vec3f(Sizes.ButtonTextOffset, 0);
    TextLabel.Color := Colors.MenuButtonText;
    Position := Vec3f(Render.Width - Width / 2 - 50, Render.Height - Height - 80, 1);
    OnClick := ButtonClicked;
    OnMouseOver := ButtonOver;
    OnMouseOut := ButtonOver;
    Parent := Container;
  end;
end;

procedure TglrSettingsMenu.ButtonTween(aObject: TglrTweenObject; aValue: Single);
var
  v: TglrVec3f;
begin
  v := Vec3f(1,1,1);
  (aObject as TglrGuiButton).SetVerticesColor(Vec4f(v.Lerp(Vec3f(Colors.MenuButton), aValue), 1.0));
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

procedure TglrSettingsMenu.ButtonOver(Sender: TglrGuiElement;
  Event: PglrInputEvent);
begin
  if (Sender.IsMouseOver) then
    Sender.SetSize(Sizes.ButtonSizeOver)
  else
    Sender.SetSize(Sizes.ButtonSize);
end;

procedure TglrSettingsMenu.SliderAndLabelInit(var Slider: TglrGuiSlider;
  var SLabel: TglrGuiLabel);
begin
  Slider := TglrGuiSlider.Create();
  with Slider do
  begin
    Position := Vec3f(Render.Width div 2, 150, 5);
    SetVerticesColor(Colors.MenuSlider);
    SetSize(200, 20);
    Fill.SetSize(195, 15);
    Fill.SetVerticesColor(Colors.MenuSlider);
    Button.SetSize(25, 25);
    Button.SetVerticesColor(Colors.MenuSlider);
    Parent := Container;
    OnValueChanged := SliderValueChanged;
    OnMouseOver := SliderOver;
    OnMouseOut := SliderOver;
    ChangeTexCoords := True;

    ValueLabel.Visible := False;
  end;

  SLabel := TglrGuiLabel.Create();
  SLabel.SetFor(Slider, lpLeft, Vec2f(-15, 0));
  SLabel.TextLabel.Color := Colors.MenuText;
end;

procedure TglrSettingsMenu.SliderOver(Sender: TglrGuiElement;
  Event: PglrInputEvent);
begin
  if (Sender.IsMouseOver) then
  begin
    TglrGuiSlider(Sender).Button.SetVerticesColor(Colors.MenuSliderOver);
    TglrGuiSlider(Sender).Fill.SetVerticesColor(Colors.MenuSliderOver);
  end
  else
  begin
    TglrGuiSlider(Sender).Button.SetVerticesColor(Colors.MenuSlider);
    TglrGuiSlider(Sender).Fill.SetVerticesColor(Colors.MenuSlider);
  end;
end;

procedure TglrSettingsMenu.SliderValueChanged(Sender: TglrGuiElement;
  NewValue: Integer);
begin

end;

procedure TglrSettingsMenu.CheckBoxAndLabelInit(var CheckBox: TglrGuiCheckBox;
  var CBLabel: TglrGuiLabel);
begin
  CheckBox := TglrGuiCheckBox.Create();
  with CheckBox do
  begin
    SetSize(45, 45);
    Check.SetSize(35, 35);
    Position := Vec3f(Render.Width div 2, 250, 5);
    SetVerticesColor(Colors.MenuSlider);
    Check.SetVerticesColor(Colors.MenuSliderOver);
    Parent := Container;
  end;

  CBLabel := TglrGuiLabel.Create();
  CBLabel.SetFor(CheckBox, lpLeft, Vec2f(-145, 0));
  CBLabel.TextLabel.Color := Colors.MenuText;
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

  SliderAndLabelInit(MusicSlider, MusicSliderLabel);
  SliderAndLabelInit(SoundSlider, SoundSliderLabel);
  SoundSlider.Position.y += 40;

  MusicSlider.Value := 50;
  SoundSlider.Value := 50;

  MusicSliderLabel.TextLabel.Text := Texts.SettingsMusicVolume;
  SoundSliderLabel.TextLabel.Text := Texts.SettingsSoundVolume;

  ApplyBtn.TextLabel.Text := Texts.MenuApply;
  BackBtn.TextLabel.Text := Texts.MenuBack;

  BackBtn.Position.y += 70;

  CheckBoxAndLabelInit(ImportantCheckBox1, ICBLabel1);
  CheckBoxAndLabelInit(ImportantCheckBox2, ICBLabel2);
  ICBLabel1.TextLabel.Text := 'Option 1';
  ICBLabel2.TextLabel.Text := 'Option 2';

  ImportantCheckBox2.Position.y += 50;

  GuiManager := TglrGuiManager.Create(Assets.GuiMaterial, Assets.FontMain);
  GuiManager.Add(ApplyBtn);
  GuiManager.Add(BackBtn);
  GuiManager.Add(MusicSlider);
  GuiManager.Add(MusicSliderLabel);
  GuiManager.Add(SoundSlider);
  GuiManager.Add(SoundSliderLabel);
  GuiManager.Add(ImportantCheckBox1);
  GuiManager.Add(ImportantCheckBox2);
  GuiManager.Add(ICBLabel1);
  GuiManager.Add(ICBLabel2);
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
  Game.Tweener.AddTweenPSingle(@Container.Position.x, tsExpoEaseIn, -Render.Width, 0, 2.5);
  inherited OnLoadStarted;
end;

procedure TglrSettingsMenu.OnUnloadStarted;
begin
  Game.Tweener.AddTweenPSingle(@Container.Position.x, tsExpoEaseIn, 0, -Render.Width, 1.0);
  ActionManager.AddIndependent(UnloadCompleted, 0.5);
end;

end.

