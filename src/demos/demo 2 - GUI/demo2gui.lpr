program demo2gui;

uses
  glrMath, tinyglr,
  uGame;

var
  InitParams: TglrInitParams;

begin
  with InitParams do
  begin
    Width := 1000;
    Height := 600;
    X := 100;
    Y := 100;
    Caption := 'Demo 2 — GUI [tiny-glr ' + TINYGLR_VERSION + ']';
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

