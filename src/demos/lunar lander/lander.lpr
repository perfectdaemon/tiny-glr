{
  TODO:
    Редактор - добавление новых точек
    Редактор - добавление точек посадки с множителем

    GUI: Отображение точек посадки

    Обработка столкновения корабля с луной:
      В случае большой скорости или неправильного угла - перезапуск игры
      В случае малой скорости и верного угла - "вы победили" + очки

    Расчет очков на основе оставшегося топлива и множителя
}

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

