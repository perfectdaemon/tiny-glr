unit uGUI;

interface

uses
  glr_gui;

type

  { TArenaGUIManager }

  TArenaGUIManager = class
    GlrGuiManager: TglrGuiManager;
    DebugText: TglrGuiLabel;

    procedure Init();
    procedure DeInit();
    procedure Update(const dt: Double);
    procedure Render();
  end;

implementation

uses
  glr_core,
  glr_math,
  uAssets;

{ TArenaGUIManager }

procedure TArenaGUIManager.Init();
begin
  GlrGuiManager := TglrGuiManager.Create(Assets.GuiMaterial, Default.Font);
  DebugText := TglrGuiLabel.Create();
  DebugText.TextLabel.Text := 'Debug';
  DebugText.Position := Vec3f(10, 10, 10);

  GlrGuiManager.Add(DebugText);
end;

procedure TArenaGUIManager.DeInit();
begin
  GlrGuiManager.Free(True);
end;

procedure TArenaGUIManager.Update(const dt: Double);
begin
  GlrGuiManager.Update(dt);
end;

procedure TArenaGUIManager.Render();
begin
  Assets.GuiCamera.Update();
  GlrGuiManager.Render();
end;

end.

