unit uLevel;

interface

uses
  tinyglr, glrMath;

type

  { TColorPoint }

  TColorPoint = class
    sprite: TglrSprite;
    constructor Create(); virtual;
    destructor Destroy(); override;

    procedure Setup(aPos: TglrVec2f; aColor: TglrVec4f);
  end;

  TColorPoints = TglrObjectList<TColorPoint>;

  { TFinish }

  TFinish = class
    sprite: TglrSprite;
    constructor Create(); virtual;
    destructor Destroy(); override;
    procedure Setup(aPos: TglrVec2f; aColor: TglrVec4f);
  end;

  { TBlender }

  TBlenderType = (btAdd, btMultiply, btClear, btAverage);

  TBlender = class
  protected
    function GetResultColor(): TglrVec4f;
  public
    blenderType: TBlenderType;
    sprite: TglrSprite;
    colorPoints: TglrObjectList<TColorPoint>;
    result: TColorPoint;
    constructor Create(); virtual;
    destructor Destroy(); override;

    property ResultColor: TglrVec4f read GetResultColor;

    procedure Setup(aPos: TglrVec2f; aType: TBlenderType);

    function CanUse(): Boolean;
  end;

  TBlenders = TglrObjectList<TBlender>;

  { TLevel }

  TLevel = class
  public
    points: TColorPoints;
    blenders: TBlenders;
    finish: TFinish;

    constructor Create(); virtual;
    destructor Destroy(); override;

    procedure LoadFromStream(aStream: TglrStream; aFreeStreamOnFinish: Boolean = True);
  end;

implementation

uses
  resload;

const
  pointSize = 25;

{ TLevel }

constructor TLevel.Create;
begin
  inherited Create;
  points := TColorPoints.Create();
  blenders := TBlenders.Create();
  finish := TFinish.Create();
end;

destructor TLevel.Destroy;
begin
  points.Free(True);
  blenders.Free(True);
  finish.Free();
  inherited Destroy;
end;

procedure TLevel.LoadFromStream(aStream: TglrStream;
  aFreeStreamOnFinish: Boolean);
var
  data: TglrStringList;
begin
  data := LoadStringList(aStream);
  if aFreeStreamOnFinish then
    aStream.Free();


end;

{ TFinish }

constructor TFinish.Create;
begin
  inherited Create();
  sprite := TglrSprite.Create(pointSize, pointSize, Vec2f(0.5, 0.5));
end;

destructor TFinish.Destroy;
begin
  sprite.Free();
  inherited Destroy;
end;

procedure TFinish.Setup(aPos: TglrVec2f; aColor: TglrVec4f);
begin
  sprite.Position.x := aPos.x;
  sprite.Position.y := aPos.y;
end;

{ TColorPoint }

constructor TColorPoint.Create;
begin
  inherited Create();
  sprite := TglrSprite.Create(pointSize, pointSize, Vec2f(0.5, 0.5));
end;

destructor TColorPoint.Destroy;
begin
  sprite.Free();
  inherited Destroy;
end;

procedure TColorPoint.Setup(aPos: TglrVec2f; aColor: TglrVec4f);
begin
  sprite.Position.x := aPos.x;
  sprite.Position.y := aPos.y;
  sprite.SetVerticesColor(aColor);
end;

{ TBlender }

function TBlender.GetResultColor: TglrVec4f;
var
  i: Integer;
begin
  Result.Reset();
  if (colorPoints.Count = 0) then
    Exit();

  case blenderType of
    btAdd:
    begin
      for i := 0 to colorPoints.Count - 1 do
        Result += colorPoints[i].sprite.Vertices[0].col;
    end;

    btMultiply:
    begin
      Result := colorPoints[0].sprite.Vertices[0].col;
      for i := 1 to colorPoints.Count - 1 do
        Result *= colorPoints[i].sprite.Vertices[0].col;
    end;

    btAverage:
    begin
      for i := 0 to colorPoints.Count - 1 do
        Result += colorPoints[i].sprite.Vertices[0].col;
      Result *= (1 / colorPoints.Count);
    end;

    btClear:
    begin
      with colorPoints[0].sprite.Vertices[0].col do
        if (x > y) then
          if (x > z) then
            Result.x := x
          else
            Result.z := z
        else
          if (y > z) then
            Result.y := y
          else
            Result.z := z;
    end;
  end;

end;

constructor TBlender.Create;
begin
  inherited Create();
  sprite := TglrSprite.Create(pointSize * 2, pointSize * 2, Vec2f(0.5, 0.5));
  sprite.SetVerticesColor(Vec4f(0.6, 0.6, 0.6, 1.0));
  colorPoints := TColorPoints.Create();
  result := TColorPoint.Create();
end;

destructor TBlender.Destroy;
begin
  colorPoints.Free(False);
  sprite.Free();
  result.Free();
  inherited Destroy;
end;

procedure TBlender.Setup(aPos: TglrVec2f; aType: TBlenderType);
begin
  sprite.Position.x := aPos.x;
  sprite.Position.y := aPos.y;
  blenderType := aType;
end;

function TBlender.CanUse: Boolean;
begin
  Result := colorPoints.Count > 0;
end;


end.

