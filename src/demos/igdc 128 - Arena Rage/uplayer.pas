unit uPlayer;

interface

uses
  glr_core,
  glr_render2d,
  glr_math,
  glr_utils,
  uUnit;

type

  { TArenaPlayer }

  TArenaPlayer = class (TArenaUnit)
  private
    fMeleeAttackInProgress: Boolean;
//    procedure MeleeAttack1(const dt: Double);
//    procedure MeleeAttack2(const dt: Double);
//    procedure MeleeAttack3();
  protected
    fAnimator: TglrActionManager;
    procedure DoRender(SpriteBatch: TglrSpriteBatch; FontBatch: TglrFontBatch=nil); override;
    procedure Control(const dt: Double); override;

    procedure MeleeAttack();
  public
    Sword: TglrSprite;
    Sword2: TglrSprite;

    constructor Create; override;
    destructor Destroy; override;

    procedure Spawn(Position: TglrVec3f); override;

    procedure OnInputReceived(Event: PglrInputEvent);
  end;

implementation

uses
  uColors,
  uAssets;

{ TArenaPlayer }

(*
procedure TArenaPlayer.MeleeAttack1(const dt: Double);
begin
  fMeleeAttackInProgress := true;
  Sword.Rotation := Sword.Rotation + dt * 720;
end;

procedure TArenaPlayer.MeleeAttack2(const dt: Double);
begin
  Sword.Rotation := Sword.Rotation + dt * 720;
  Sprite.Rotation := Sprite.Rotation + dt * 720;
end;

procedure TArenaPlayer.MeleeAttack3;
begin
  fMeleeAttackInProgress := False;
  Sword.Rotation := -45;
end;

*)
procedure TArenaPlayer.DoRender(SpriteBatch: TglrSpriteBatch;
  FontBatch: TglrFontBatch);
begin
  inherited DoRender(SpriteBatch, FontBatch);
  SpriteBatch.Draw(Sword);
end;

procedure TArenaPlayer.Control(const dt: Double);
begin
  inherited Control(dt);

  // Moving
  fControlVector.Reset();
  if (Core.Input.KeyDown[kW] or Core.Input.KeyDown[kUp]) then
    fControlVector.y := -1
  else if (Core.Input.KeyDown[kS] or Core.Input.KeyDown[kDown]) then
    fControlVector.y := 1;
  if (Core.Input.KeyDown[kA] or Core.Input.KeyDown[kLeft]) then
    fControlVector.x := -1
  else if (Core.Input.KeyDown[kD] or Core.Input.KeyDown[kRight]) then
    fControlVector.x := 1;

  if (fControlVector.LengthQ > cEPS) then
    fControlVector.Normalize();

  // Rotating
  if not fMeleeAttackInProgress then
    fDirection := (Assets.MainCamera.ScreenToWorld(Core.Input.MousePos) - Vec2f(Sprite.Position)).Normal;

  fAnimator.Update(dt);
end;

procedure TArenaPlayer.MeleeAttack;
begin
  (*
  if fMeleeAttackInProgress then
    Exit();
  fAnimator.AddToQueue(MeleeAttack1, 0.25);
  fAnimator.AddToQueue(MeleeAttack2, 0.5);
  fAnimator.AddToQueue(MeleeAttack3);
  *)
end;

constructor TArenaPlayer.Create;
begin
  inherited Create;

  Sprite.SetVerticesColor(Color4f(1, 1, 1, 1));

  Sword := TglrSprite.Create(50, 5, Vec2f(0, 0.5));
  Sword.Parent := Sprite;
  Sword.SetVerticesColor(Colors.Green);

  fAnimator := TglrActionManager.Create();
end;

destructor TArenaPlayer.Destroy;
begin
  fAnimator.Free();
  Sword.Free();
  inherited Destroy;
end;

procedure TArenaPlayer.Spawn(Position: TglrVec3f);
begin
  inherited Spawn(Position);
  Sprite.SetSize(25, 45);
  DirectSpeed := 190;
  Sword.Position := Vec3f(10, 10, 4);
  Sword.Rotation := -45;
end;

procedure TArenaPlayer.OnInputReceived(Event: PglrInputEvent);
begin
  if (Event^.InputType = itTouchDown) then
    MeleeAttack();
end;

end.

