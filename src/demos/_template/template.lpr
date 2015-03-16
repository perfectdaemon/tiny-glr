program template;

uses
  uGame, glr_core, glr_utils;

var
  InitParams: TglrInitParams;

begin
  with InitParams do
  begin
    Width := 1000;
    Height := 600;
    X := 100;
    Y := 100;
    Caption := 'Template project [tiny-glr ' + TINYGLR_VERSION + ']';
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

