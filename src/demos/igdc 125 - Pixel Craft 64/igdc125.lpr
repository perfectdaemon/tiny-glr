program igdc125;

uses
  uGame, glr_core, glr_utils, uGSMainMenu, uAssets, 
  uGSSettingsMenu, uGSGame, uUnit, uObjects;

var
  InitParams: TglrInitParams;

begin
  with InitParams do
  begin
    Width := W_WIDTH;
    Height := W_HEIGHT;
    X := 100;
    Y := 100;
    Caption := 'igdc #125 [tiny-glr ' + TINYGLR_VERSION + ']';
    vSync := True;
    PackFilesPath := '';
    UseDefaultAssets := True;
  end;

  Game := TGame.Create();
  Core.Init(Game, InitParams);
  Core.Loop();
  Core.DeInit();
  Game.Free();
end.

