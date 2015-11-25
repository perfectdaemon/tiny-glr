unit uUnit;

interface

uses
  glr_math,
  uObject;

type

  { TArenaUnit }

  TArenaUnit = class (TArenaObject)
  protected
    fControlVector: TglrVec2f;
    fDirection: TglrVec2f;
    procedure Control(const dt: Double); virtual;
    procedure DoUpdate(const dt: Double); override;
  public
    HealthMax, DirectSpeed: Single;

    Health: Single;

    constructor Create(); virtual;

    procedure Spawn(Position: TglrVec3f); override;
    procedure Die(); virtual;
  end;

implementation

{ TArenaUnit }

procedure TArenaUnit.Spawn(Position: TglrVec3f);
begin
  inherited Spawn(Position);
  Health := HealthMax;
  DirectSpeed := 0;
end;

procedure TArenaUnit.Control(const dt: Double);
begin

end;

procedure TArenaUnit.DoUpdate(const dt: Double);
begin
  inherited DoUpdate(dt);
  Control(dt);

  Sprite.Position += Vec3f(fControlVector * dt * DirectSpeed, 0);
  Sprite.Rotation := fDirection.GetRotationAngle();

  if (Health <= 0) then
    Die();
end;

constructor TArenaUnit.Create;
begin
  inherited;
  fControlVector.Reset();
  fDirection := Vec2f(0, 1);
end;

procedure TArenaUnit.Die;
begin

end;

end.

