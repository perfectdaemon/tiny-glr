unit uMoon;

{$mode delphi}

interface

uses
  tinyglr, glrMath, uPhysics2d ;

type

  { TMoon }

  TMoon = class
  private
    fEditMode: Boolean;
    procedure SetEditMode(AValue: Boolean);
  protected
    fVB: TglrVertexBuffer;
    fIB: TglrIndexBuffer;
    fMaterial: TglrMaterial;
    fTR, fPointTR: PglrTextureRegion;
    fVecCount, fIndCount: Integer;
    fBatch: TglrSpriteBatch;
  public
    MaxY: Single;
    Vertices: array of TdfVec2f;
    VerticesPoints: array of TglrSprite;
    b2Body: Tb2Body;
    constructor Create(aMaterial: TglrMaterial; aTexRegion, aPointTexRegion: PglrTextureRegion;
      aBatch: TglrSpriteBatch); virtual;
    destructor Destroy(); override;

    procedure UpdateData();

    procedure AddVertex(aPos: TdfVec2f; aIndex: Integer = -1);
    procedure DeleteVertex(aIndex: Integer);
    function GetVertexIndexAtPos(aPos: TdfVec2f): Integer;

    procedure RenderSelf();

    procedure LoadLevel(const aStream: TglrStream; aFreeStreamOnFinish: Boolean = True);
    function SaveLevel(): TglrStream;

    property EditMode: Boolean read fEditMode write SetEditMode;
  end;

implementation

uses
  uMain,
  uBox2DImport;

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

constructor TMoon.Create(aMaterial: TglrMaterial; aTexRegion,
  aPointTexRegion: PglrTextureRegion; aBatch: TglrSpriteBatch);
begin
  inherited Create();
  fVB := TglrVertexBuffer.Create(nil, 65536, vfPos3Tex2);
  fIB := TglrIndexBuffer.Create(nil, 65536, ifShort);
  fMaterial := aMaterial;
  fTR := aTexRegion;
  fPointTR := aPointTexRegion;
  fBatch := aBatch;

  b2Body := nil;
end;

destructor TMoon.Destroy;
var
  i: Integer;
begin
  fVB.Free();
  fIB.Free();
  for i := 0 to Length(VerticesPoints) - 1 do
    fBatch.Childs.Delete(VerticesPoints[i], True);
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
    data[fVecCount].vec     := dfVec3f(Vertices[i].x, MaxY, 5);
    data[fVecCount + 1].vec := dfVec3f(Vertices[i], 5);
    fVecCount += 2;
  end;

  fVb.Update(@data[0], 0, fVecCount);
  fIB.Update(@idata[0], 0, fIndCount);

  //box2d
  if Assigned(b2Body) then
    b2Body.Free();
  b2Body := Box2d.ChainStatic(Game.World, dfVec2f(0, 0), Vertices, 1.0, 1.5, 0.0, $0001, $0002, 2);
end;

procedure TMoon.AddVertex(aPos: TdfVec2f; aIndex: Integer);
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
    Move(Vertices[aIndex], Vertices[aIndex + 1], SizeOf(TdfVec2f) * (i - aIndex));
  end;

  Vertices[aIndex] := aPos;
  VerticesPoints[aIndex] := TglrSprite.Create(15, 15, dfVec2f(0.5, 0.5));
  VerticesPoints[aIndex].Position := dfVec3f(Vertices[aIndex], 10);
  VerticesPoints[aIndex].SetTextureRegion(fPointTR, False);
  fBatch.Childs.Add(VerticesPoints[aIndex]);
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

  fBatch.Childs.Delete(VerticesPoints[aIndex], True);
  if aIndex <> High(Vertices) then
  begin
    Move(Vertices[aIndex + 1], Vertices[aIndex], (High(Vertices) - aIndex) * SizeOf(TdfVec2f));
    Move(VerticesPoints[aIndex + 1], VerticesPoints[aIndex], (High(VerticesPoints) - aIndex) * SizeOf(TglrSprite));
  end;
  SetLength(Vertices, Length(Vertices) - 1);
  SetLength(VerticesPoints, Length(VerticesPoints) - 1);
end;

function TMoon.GetVertexIndexAtPos(aPos: TdfVec2f): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Length(Vertices) - 1 do
    if (Vertices[i] - aPos).LengthQ < 49 then
    Exit(i);
end;

procedure TMoon.RenderSelf;
begin
  fMaterial.Bind();
  Render.DrawTriangles(fVB, fIB, 0, fIndCount);
  fMaterial.Unbind();
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
  aStream.Read(Vertices[0], count * SizeOf(TdfVec2f));
  for i := 0 to count - 1 do
  begin
    VerticesPoints[i] := TglrSprite.Create(15, 15, dfVec2f(0.5, 0.5));
    VerticesPoints[i].Position := dfVec3f(Vertices[i], 10);
    VerticesPoints[i].SetTextureRegion(fPointTR, False);
    VerticesPoints[i].Visible := False;
    fBatch.Childs.Add(VerticesPoints[i]);
  end;
  UpdateData();

  if aFreeStreamOnFinish then
    aStream.Free();
end;

function TMoon.SaveLevel: TglrStream;
var
  count: Word;
  size: LongInt;
  p: Pointer;
begin
  count := Length(Vertices);
  size := SizeOf(Word) + count * SizeOf(TdfVec2f);
  GetMem(p, size);
  Result := TglrStream.Init(p, size, True);
  Result.Write(count, SizeOf(Word));
  Result.Write(Vertices[0], count * SizeOf(TdfVec2f));
end;

end.

