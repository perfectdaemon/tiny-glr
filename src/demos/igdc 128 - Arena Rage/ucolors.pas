unit uColors;

interface

uses
  glr_math;

type
  TArenaColors = record
  private
    procedure Init();
  public
    Red, Green: TglrVec4f;

    Background: TglrVec4f;
  end;

var
  Colors: TArenaColors;

implementation

procedure TArenaColors.Init();
begin
  Red := Color4ub(185, 20, 20);
  Green := Color4ub(74, 244, 8);
  Background := Color4ub(71, 97, 101);
end;


initialization
  Colors.Init();

end.

