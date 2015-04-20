unit uMoon;

{$mode delphi}

interface

uses
  glr_render,
  glr_render2d,
  glr_utils,
  glr_math, uPhysics2d;

type

  { TLandingZone }

  TLandingZone = packed record
    Pos, Size: TglrVec2f;
    Multiply: Byte;
    Sprite: TglrSprite;
    MultText: TglrText;
    procedure Update();
  end;

  { TMoon }

  TMoon = class
  private
    fEditMode: Boolean;
    procedure SetEditMode(AValue: Boolean);
  protected
    fVB: TglrVertexBuffer;
    fIB: TglrIndexBuffer;
    fMoonMaterial, fMaterial: TglrMaterial;
    fPointTR: PglrTextureRegion;
    fVecCount, fIndCount: Integer;
    fBatch: TglrSpriteBatch;
    fFontBatch: TglrFontBatch;
  public
    MaxY: Single;
    Vertices: array of TglrVec2f;
    VerticesPoints: array of TglrSprite;
    LandingZones: array of TLandingZone;
    b2Body: Tb2Body;
    constructor Create(aMoonMaterial, aMaterial: TglrMaterial; aPointTexRegion: PglrTextureRegion;
      aBatch: TglrSpriteBatch; aFontBatch: TglrFontBatch); virtual;
    destructor Destroy(); override;

    procedure UpdateData();

    procedure AddVertex(aPos: TglrVec2f; aIndex: Integer = -1);
    procedure DeleteVertex(aIndex: Integer);
    function GetVertexIndexAtPos(aPos: TglrVec2f): Integer;

    procedure AddLandingZone(aPos, aSize: TglrVec2f; aMultiply: Byte);
    function GetLandingZoneAtPos(aPos: TglrVec2f): Integer;
    procedure DeleteLandingZone(aIndex: Integer);

    procedure RenderSelf();
    procedure RenderLandingZones();

    procedure LoadLevel(const aStream: TglrStream; aFreeStreamOnFinish: Boolean = True);
    function SaveLevel(): TglrStream;

    property EditMode: Boolean read fEditMode write SetEditMode;
  end;

implementation

uses
  uMain,
  uBox2DImport;

{ TLandingZone }

procedure TLandingZone.Update;
begin
  Sprite.SetSize(Size);
  Sprite.Position := Vec3f(Pos, 4);
  MultText.Text := Convert.ToString(Multiply) + 'x';
  MultText.Position := Vec3f(Pos.x - 20, Pos.y - Size.y / 2 + 10, 5);
end;

{ TMoon }

procedure TMoon.SetEditMode(AValue: Boolean);
var
  i: Integer;
begin
  if fEditMode = AValue then
    Exit;
  fEditMode := AValue;
  for i := 0 to Length(VerticesPoints) - 1 do
    VerticesPoints[i].Visible := AValue;
end;

constructor TMoon.Create(aMoonMaterial, aMaterial: TglrMaterial;
  aPointTexRegion: PglrTextureRegion; aBatch: TglrSpriteBatch;
  aFontBatch: TglrFontBatch);
begin
  inherited Create();
  fVB := TglrVertexBuffer.Create(nil, 65536, vfPos3Tex2, uStaticDraw);
  fIB := TglrIndexBuffer.Create(nil, 65536, ifShort);
  fMoonMaterial := aMoonMaterial;
  fMaterial := aMaterial;
  fPointTR := aPointTexRegion;
  fBatch := aBatch;
  fFontBatch := aFontBatch;

  b2Body := nil;

  SetLength(LandingZones, 0);
  SetLength(Vertices, 0);
  SetLength(VerticesPoints, 0);
end;

destructor TMoon.Destroy;
var
  i: Integer;
begin
  fVB.Free();
  fIB.Free();
  for i := 0 to Length(LandingZones) - 1 do
  begin
    LandingZones[i].MultText.Free();
    LandingZones[i].Sprite.Free();
  end;
  for i := 0 to Length(VerticesPoints) - 1 do
    VerticesPoints[i].Free();

  inherited Destroy;
end;

procedure TMoon.UpdateData;
var
  i: Integer;
  data: array of TglrVertexP3T2;
  idata: array of Word;
begin
  fVecCount := 0;
  fIndCount := 0;
  SetLength(data, Length(Vertices) * 2);
  SetLength(idata, (Length(Vertices) - 1) * 6);
  for i := Length(Vertices) - 1 downto 0 do
  begin
    if i > 0 then
    begin
      idata[fIndCount + 0] := 0 + fVecCount;
      idata[fIndCount + 1] := 1 + fVecCount;
      idata[fIndCount + 2] := 3 + fVecCount;
      idata[fIndCount + 3] := 3 + fVecCount;
      idata[fIndCount + 4] := 2 + fVecCount;
      idata[fIndCount + 5] := 0 + fVecCount;
      fIndCount += 6;
    end;
    data[fVecCount].vec     := Vec3f(Vertices[i].x, MaxY, 5);
    data[fVecCount].tex     := Vec2f((data[0].vec.x - data[fVecCount].vec.x) / (fMoonMaterial.Textures[0].Texture.Width), 0);
    data[fVecCount + 1].vec := Vec3f(Vertices[i], 5);
    data[fVecCount + 1].tex := Vec2f(data[fVecCount].tex.x, (data[fVecCount + 1].vec.y - MaxY) / fMoonMaterial.Textures[0].Texture.Width);
    fVecCount += 2;
  end;

  fVb.Update(@data[0], 0, fVecCount);
  fIB.Update(@idata[0], 0, fIndCount);

  //box2d
  if Assigned(b2Body) then
    b2Body.Free();
  b2Body := Box2d.ChainStatic(Game.World, Vec2f(0, 0), Vertices, 1.0, 1.5, 0.0, $0001, $0002, 2);
  b2Body.UserData := Self;
end;

procedure TMoon.AddVertex(aPos: TglrVec2f; aIndex: Integer);
var
  i: Integer;
begin
  i := Length(Vertices);
  SetLength(Vertices, i + 1);
  SetLength(VerticesPoints, i + 1);

  if aIndex = -1 then
    aIndex := i
  else
  begin
    Move(VerticesPoints[aIndex], VerticesPoints[aIndex + 1], SizeOf(TglrSprite) * (i - aIndex));
    Move(Vertices[aIndex], Vertices[aIndex + 1], SizeOf(TglrVec2f) * (i - aIndex));
  end;

  Vertices[aIndex] := aPos;
  VerticesPoints[aIndex] := TglrSprite.Create(15, 15, Vec2f(0.5, 0.5));
  VerticesPoints[aIndex].Position := Vec3f(Vertices[aIndex], 10);
  VerticesPoints[aIndex].SetTextureRegion(fPointTR, False);
end;

procedure TMoon.DeleteVertex(aIndex: Integer);
begin
  if (aIndex < 0) or (aIndex > High(Vertices)) then
  begin
    Log.Write(lError, 'Moon.DeleteVertex: Index '
      + Convert.ToString(aIndex)
      + ' is out of bounds [0; '
      + Convert.ToString(High(Vertices)) + ']');
    Exit();
  end;

  if aIndex <> High(Vertices) then
  begin
    Move(Vertices[aIndex + 1], Vertices[aIndex], (High(Vertices) - aIndex) * SizeOf(TglrVec2f));
    Move(VerticesPoints[aIndex + 1], VerticesPoints[aIndex], (High(VerticesPoints) - aIndex) * SizeOf(TglrSprite));
  end;
  SetLength(Vertices, Length(Vertices) - 1);
  SetLength(VerticesPoints, Length(VerticesPoints) - 1);
end;

function TMoon.GetVertexIndexAtPos(aPos: TglrVec2f): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Length(Vertices) - 1 do
    if (Vertices[i] - aPos).LengthQ < 49 then
    Exit(i);
end;

procedure TMoon.AddLandingZone(aPos, aSize: TglrVec2f; aMultiply: Byte);
var
  i: Integer;
begin
  i := Length(LandingZones);
  SetLength(LandingZones, i + 1);
  with LandingZones[i] do
  begin
    Multiply := aMultiply;
    Pos := aPos;
    Size := aSize;

    Sprite := TglrSprite.Create();
    Sprite.SetTextureRegion(fPointTR, False);
    Sprite.SetVerticesColor(Vec4f(0, 1, 0, 0.1));

    MultText := TglrText.Create();

    Update();
  end;
end;

function TMoon.GetLandingZoneAtPos(aPos: TglrVec2f): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Length(LandingZones) - 1 do
    if (aPos.x > LandingZones[i].Pos.x - LandingZones[i].Size.x / 2)
      and (aPos.x < LandingZones[i].Pos.x + LandingZones[i].Size.x / 2)
      and (aPos.y > LandingZones[i].Pos.y - LandingZones[i].Size.y / 2)
      and (aPos.y < LandingZones[i].Pos.y + LandingZones[i].Size.y / 2) then
    Exit(i);
end;

procedure TMoon.DeleteLandingZone(aIndex: Integer);
begin
  if (aIndex < 0) or (aIndex > High(LandingZones)) then
  begin
    Log.Write(lError, 'Moon.DeleteLandingZoe: Index '
      + Convert.ToString(aIndex)
      + ' is out of bounds [0; '
      + Convert.ToString(High(LandingZones)) + ']');
    Exit();
  end;

  if aIndex <> High(LandingZones) then
    Move(LandingZones[aIndex + 1], LandingZones[aIndex], (High(LandingZones) - aIndex) * SizeOf(TLandingZone));
  SetLength(LandingZones, Length(LandingZones) - 1);
end;

procedure TMoon.RenderSelf;
begin
  fMoonMaterial.Bind();
  Render.DrawTriangles(fVB, fIB, 0, fIndCount);
//  fMoonMaterial.Unbind();
end;

procedure TMoon.RenderLandingZones;
var
  i: Integer;
begin
  fMaterial.Bind();
  fBatch.Start();
  fFontBatch.Start();
  if fEditMode then
    fBatch.Draw(VerticesPoints);

  for i := 0 to Length(LandingZones) - 1 do
  begin
    fBatch.Draw(LandingZones[i].Sprite);
    fFontBatch.Draw(LandingZones[i].MultText);
  end;
  fBatch.Finish();
  fFontBatch.Finish();
  //fMaterial.Unbind();
end;

procedure TMoon.LoadLevel(const aStream: TglrStream;
  aFreeStreamOnFinish: Boolean);
var
  count: Word;
  i: Integer;
begin
  aStream.Read(count, SizeOf(Word));
  SetLength(Vertices, count);
  SetLength(VerticesPoints, count);
  aStream.Read(Vertices[0], count * SizeOf(TglrVec2f));
  for i := 0 to count - 1 do
  begin
    VerticesPoints[i] := TglrSprite.Create(15, 15, Vec2f(0.5, 0.5));
    VerticesPoints[i].Position := Vec3f(Vertices[i], 10);
    VerticesPoints[i].SetTextureRegion(fPointTR, False);
    VerticesPoints[i].Visible := False;
  end;
  UpdateData();

  count := 0;
  aStream.Read(count, SizeOf(Word));
  if count <> 0 then
  begin
    SetLength(LandingZones, count);
    aStream.Read(LandingZones[0], count * SizeOf(TLandingZone));
    for i := 0 to count - 1 do
      with LandingZones[i] do
      begin
        Sprite := TglrSprite.Create();
        Sprite.SetTextureRegion(fPointTR, False);
        Sprite.SetVerticesColor(Vec4f(0, 1, 0, 0.1));

        MultText := TglrText.Create();

        Update();
      end;
  end;

  if aFreeStreamOnFinish then
    aStream.Free();
end;

function TMoon.SaveLevel: TglrStream;
var
  count, count2: Word;
  size: LongInt;
  p: Pointer;
begin
  count := Length(Vertices);
  count2 := Length(LandingZones);
  size := 2 * SizeOf(Word) + count * SizeOf(TglrVec2f) + count2 * SizeOf(TLandingZone);
  GetMem(p, size);
  Result := TglrStream.Init(p, size, True);
  Result.Write(count, SizeOf(Word));
  Result.Write(Vertices[0], count * SizeOf(TglrVec2f));
  Result.Write(count2, SizeOf(Word));
  if count2 <> 0 then
    Result.Write(LandingZones[0], count2 * SizeOf(TLandingZone));
end;

end.

