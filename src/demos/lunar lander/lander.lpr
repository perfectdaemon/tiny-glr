program lander;

uses
  tinyglr,
  uMain;

var
  InitParams: TglrInitParams;

begin
  with InitParams do
  begin
    Width := 1000;
    Height := 600;
    X := 100;
    Y := 100;
    Caption := '`Lunar Lander` © remake by perfect.daemon [tiny-glr ' + TINYGLR_VERSION + ']';
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

