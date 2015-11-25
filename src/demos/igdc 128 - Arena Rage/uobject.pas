unit uObject;

interface

uses
  glr_math,
  glr_render2d;

type

  { TArenaObject }

  TArenaObject = class
  protected
    fVisible, FEnabled: Boolean;
    procedure SetVisible(const Value: Boolean); virtual;
    procedure SetEnabled(const Value: Boolean); virtual;
    procedure DoUpdate(const dt: Double); virtual;
    procedure DoRender(SpriteBatch: TglrSpriteBatch; FontBatch: TglrFontBatch = nil); virtual;
  public
    Sprite: TglrSprite;

    constructor Create(); virtual;
    destructor Destroy(); override;

    procedure Spawn(Position: TglrVec3f); virtual;

    procedure Update(const dt: Double);
    procedure Render(SpriteBatch: TglrSpriteBatch; FontBatch: TglrFontBatch = nil);

    property Visible: Boolean read fVisible write SetVisible;
    property Enabled: Boolean read fEnabled write SetEnabled;
  end;

implementation

uses
  uColors;

{ TArenaObject }

procedure TArenaObject.SetVisible(const Value: Boolean);
begin
  fVisible := Value;
  Sprite.Visible := Value;
end;

procedure TArenaObject.SetEnabled(const Value: Boolean);
begin
  fEnabled := Value;
end;

procedure TArenaObject.DoUpdate(const dt: Double);
begin

end;

procedure TArenaObject.DoRender(SpriteBatch: TglrSpriteBatch; FontBatch: TglrFontBatch = nil);
begin
  SpriteBatch.Draw(Sprite);
end;

constructor TArenaObject.Create;
begin
  inherited Create();
  Sprite := TglrSprite.Create(10, 10, Vec2f(0.5, 0.5));
  Sprite.SetVerticesColor(Colors.Red);
end;

destructor TArenaObject.Destroy;
begin
  inherited Destroy;
end;

procedure TArenaObject.Spawn(Position: TglrVec3f);
begin
  Visible := True;
  Enabled := True;
  Sprite.Position := Position;
end;

procedure TArenaObject.Update(const dt: Double);
begin
  if (fEnabled) then
    DoUpdate(dt);
end;

procedure TArenaObject.Render(SpriteBatch: TglrSpriteBatch;
  FontBatch: TglrFontBatch);
begin
  if (Visible) then
    DoRender(SpriteBatch, FontBatch);
end;

end.

