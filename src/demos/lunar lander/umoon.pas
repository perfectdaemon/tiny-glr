unit uMoon;

{$mode delphi}

interface

uses
  tinyglr, glrMath;

type

  { TMoon }

  TMoon = class
  protected
    fVB: TglrVertexBuffer;
    fIB: TglrIndexBuffer;
    fMaterial: TglrMaterial;
    fTR: PglrTextureRegion;
    fVecCount, fIndCount: Integer;
  public
    MaxY: Single;
    Vertices: array of TdfVec2f;
    constructor Create(aMaterial: TglrMaterial; aTexRegion: PglrTextureRegion); virtual;
    destructor Destroy(); override;

    procedure UpdateData();

    procedure RenderSelf();
  end;

implementation

{ TMoon }

constructor TMoon.Create(aMaterial: TglrMaterial; aTexRegion: PglrTextureRegion);
begin
  inherited Create();
  fVB := TglrVertexBuffer.Create(nil, 65536, vfPos3Tex2);
  fIB := TglrIndexBuffer.Create(nil, 65536, ifShort);
  fMaterial := aMaterial;
  fTR := aTexRegion;

  //debug
  MaxY := Render.Height;
  SetLength(Vertices, 8);
  Vertices[0] := dfVec2f(50, 400);
  Vertices[1] := dfVec2f(100, 480);
  Vertices[2] := dfVec2f(150, 320);
  Vertices[3] := dfVec2f(200, 500);
  Vertices[4] := dfVec2f(250, 400);
  Vertices[5] := dfVec2f(300, 480);
  Vertices[6] := dfVec2f(350, 320);
  Vertices[7] := dfVec2f(400, 520);
  UpdateData();
end;

destructor TMoon.Destroy;
begin
  fVB.Free();
  fIB.Free();
  inherited Destroy;
end;

procedure TMoon.UpdateData;
var
  i: Integer;
//  q: array[0..3] of TglrVertexP3T2;
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
end;

procedure TMoon.RenderSelf;
begin
  fMaterial.Bind();
  Render.DrawTriangles(fVB, fIB, 0, fIndCount);
  fMaterial.Unbind();
end;

end.

