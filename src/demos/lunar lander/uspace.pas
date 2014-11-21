unit uSpace;

{$mode delphi}

interface

uses
  tinyglr, glrmath;

const
  STARS_PER_LAYER = 100;

type
  TSpacePatch = record
    Position: TglrVec2f;
    Stars: array of TglrSprite;
    Initials: array of TglrVec2f;
  end;

  { TSpace }

  TSpace = class
  protected
    fCount: Integer;
    fBatch: TglrSpriteBatch;
    fMaterial: TglrMaterial;
    fPatch: TSpacePatch;
  public
    Camera: TglrCamera;
    constructor Create(aPatchStart, aPatchSize: TglrVec2f; StarTR: PglrTextureRegion;
      aMaterial: TglrMaterial; aParallaxLevels: Integer = 3); virtual;
    destructor Destroy(); override;

    procedure RenderSelf();
  end;

implementation

const
  colorWhite: TglrVec4f = (x: 1.0; y: 1.0;  z: 1.0; w: 1.0);
  colorBlue:  TglrVec4f = (x: 74/255; y: 151/255;  z: 215/255; w: 1.0);
  colorRed:   TglrVec4f = (x: 215/255; y: 109/255;  z: 74/255; w: 1.0);

constructor TSpace.Create(aPatchStart, aPatchSize: TglrVec2f; StarTR: PglrTextureRegion;
  aMaterial: TglrMaterial; aParallaxLevels: Integer);
var
  i: Integer;
  z: Single;
  pos: TglrVec3f;
  col: TglrVec4f;
begin
  inherited Create();
  fBatch := TglrSpriteBatch.Create();
  fMaterial := aMaterial;
  fPatch.Position := Vec2f(0, 0);
  fCount := STARS_PER_LAYER * aParallaxLevels;
  SetLength(fPatch.Stars, fCount);
  SetLength(fPatch.Initials, fCount);
  Randomize();
  for i := 0 to fCount - 1 do
  begin
    z := - (i div STARS_PER_LAYER) / aParallaxLevels;
    pos := Vec3f(aPatchStart.x + Random(Round(aPatchSize.x)),
      aPatchStart.y + Random(Round(aPatchSize.y)), z);
    fPatch.Initials[i] := Vec2f(pos);
    z := Random();
    if z < 0.3 then
      col := colorBlue
    else if z < 0.6 then
      col := colorRed
    else
      col := colorWhite;
    col.w := pos.z + 0.8;

    fPatch.Stars[i] := TglrSprite.Create();
    with fPatch.Stars[i] do
    begin
      Position := pos;
      SetTextureRegion(StarTR);
      SetSize(6 * (1 + pos.z), 6  * (1 + pos.z));
      SetVerticesColor(col);
    end;
    //fBatch.Childs.Add(fPatch.Stars[i]);
  end;
end;

destructor TSpace.Destroy;
var
  i: Integer;
begin
  fBatch.Free();
  for i := 0 to fCount - 1 do
    fPatch.Stars[i].Free();
  inherited Destroy;
end;

procedure TSpace.RenderSelf;
var
  i: Integer;
begin
  fMaterial.Bind();
  fBatch.Start();
  for i := 0 to fCount - 1 do
    with fPatch.Stars[i] do
    begin
      Position := Vec3f(fPatch.Initials[i] - Position.z * Vec2f(Camera.Position), Position.z);
      fBatch.Draw(fPatch.Stars[i]);
    end;
  fBatch.Finish();
end;

end.

