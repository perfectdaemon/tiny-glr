program demo3simple3D;

uses
  tinyglr,
  uGame,
  glr_utils, glr_filesystem, glr_render, glr_render2d, glr_scene,
  glr_core, glr_gui, glr_mesh, glr_particles2d;

var
  InitParams: TglrInitParams;

begin
  with InitParams do
  begin
    Width := 1000;
    Height := 600;
    X := 100;
    Y := 100;
    Caption := 'demo 3 simple3D project [tiny-glr ' + TINYGLR_VERSION + ']';
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

