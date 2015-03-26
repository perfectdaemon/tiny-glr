{
  Based on eXgine© Math unit (New BSD License)
}

unit glr_math;

interface

const
  //Допустимая погрешность сравнения с нулем
  cEPS = 0.0001;

  deg2rad: Single = 3.1415 / 180;
  rad2deg = 180 / 3.1415;
  pi = 3.14;
  twopi = 2 * pi;

type
  TglrBox2f = record
    x1, y1, x2, y2: Single;
  end;

  TglrBox2i = record
    x1, y1, x2, y2: Integer;
  end;

  TglrBox3f = record
    x1, y1, z1, x2, y2, z2: Single;
  end;

  TglrBox3i = record
    x1, y1, z1, x2, y2, z2: Integer;
  end;

  TglrBB = record
    Left, Right, Top, Bottom: Single;
  end;

  TglrBBi = record
    Left, Right, Top, Bottom: Integer;
  end;

{$REGION ' Single Vectors '}

  { TglrVec2f }

  TglrVec2f = record
    x, y: Single;
    class operator Equal (const v1, v2: TglrVec2f): Boolean;
    class operator Add (const v1, v2: TglrVec2f): TglrVec2f;
    class operator Subtract (const v1, v2: TglrVec2f): TglrVec2f;
    class operator Multiply (const v1, v2: TglrVec2f): TglrVec2f;
    class operator Multiply (const v: TglrVec2f; a: Single): TglrVec2f;
    class operator Multiply (const a: Single; const v: TglrVec2f): TglrVec2f;
    class operator Divide   (const v: TglrVec2f; const a: Single): TglrVec2f;
    class operator Negative (const v: TglrVec2f): TglrVec2f;
    function Dot(const v: TglrVec2f): Single;
    function Reflect(const n: TglrVec2f): TglrVec2f;
    function Refract(const n: TglrVec2f; Factor: Single): TglrVec2f;
    function Length: Single;
    function LengthQ: Single;
    function Normal: TglrVec2f;
    function Dist(const v: TglrVec2f): Single;
    function Lerp(const v: TglrVec2f; t: Single): TglrVec2f;
    function Clamp(const Min, Max: TglrVec2f): TglrVec2f; overload;
    function Clamp(const Min, Max: Single): TglrVec2f; overload;
    function Rotate(Angle: Single): TglrVec2f;
    function Angle(const v: TglrVec2f): Single;
    function NegateVector(): TglrVec2f;
    procedure Negate;
    procedure Normalize();
    procedure Reset();
    function GetRotationAngle(): Single;
  end;

  { TglrVec3f }

  TglrVec3f = record
    x, y, z: Single;
    class operator Equal(const v1, v2: TglrVec3f): Boolean;
    class operator Add(const v1, v2: TglrVec3f): TglrVec3f;
    class operator Subtract(const v1, v2: TglrVec3f): TglrVec3f;
    class operator Multiply(const v1, v2: TglrVec3f): TglrVec3f;
    class operator Multiply(const v: TglrVec3f; a: Single): TglrVec3f;
    class operator Multiply(const a: Single; const v: TglrVec3f): TglrVec3f;
    class operator Divide   (const v: TglrVec3f; const a: Single): TglrVec3f;
    class operator Negative (const v: TglrVec3f): TglrVec3f;
    function Lerp(const v: TglrVec3f; t: Single): TglrVec3f;
    function Dot(const v: TglrVec3f): Single;
    function Cross(const v: TglrVec3f): TglrVec3f;
    function Normal: TglrVec3f;
    procedure Normalize;
    function Length: Single;
    function LengthQ: Single;
    function DistAbs(const v: TglrVec3f): Single;
    function Reflect(const n: TglrVec3f): TglrVec3f;
    function Refract(const n: TglrVec3f; Factor: Single): TglrVec3f;
    function Clamp(const Min, Max: TglrVec3f): TglrVec3f;
    function Rotate(Angle: Single; const Axis: TglrVec3f): TglrVec3f;
    function NegateVector(): TglrVec3f;
    procedure Negate;
    procedure Reset();
  end;

  { TglrVec4f }

  TglrVec4f = record
    x, y, z, w: Single;
    class operator Equal(const v1, v2: TglrVec4f): Boolean;
    class operator Multiply(const A: Single; v: TglrVec4f): TglrVec4f;
    class operator Multiply(const v: TglrVec4f; a: Single): TglrVec4f;
    class operator Multiply(const v1, v2: TglrVec4f): TglrVec4f;
    class operator Add(const v1, v2: TglrVec4f): TglrVec4f;
    class operator Subtract(const v1, v2: TglrVec4f): TglrVec4f;
    function Dot(const v: TglrVec3f): Single;
    function Normal: TglrVec4f;
    procedure Normalize;
    procedure Reset();
  end;
{$ENDREGION}

{$REGION ' Quaternions '}
  TglrQuat = record
    x, y, z, w : Single;
    class operator Equal(const q1, q2: TglrQuat): Boolean;
    class operator Add(const q1, q2: TglrQuat): TglrQuat;
    class operator Subtract(const q1, q2: TglrQuat): TglrQuat;
    class operator Multiply(const q: TglrQuat; x: Single): TglrQuat;
    class operator Multiply(const q1, q2: TglrQuat): TglrQuat;
    class operator Multiply(const q: TglrQuat; const v: TglrVec3f): TglrVec3f;
    function Invert: TglrQuat;
    function Lerp(const q: TglrQuat; t: Single): TglrQuat;
    function Dot(const q: TglrQuat): Single;
    procedure Normalize;
    function FromVectorAngle(const v: TglrVec3f; Angle: Single): TglrQuat;
    function Euler: TglrVec3f;
  end;

  TglrDualQuat = record
  private
    function GetPos: TglrVec3f;
  public
    Real, Dual : TglrQuat;
    class operator Multiply(const dq1, dq2: TglrDualQuat): TglrDualQuat;
    function Lerp(const dq: TglrDualQuat; t: Single): TglrDualQuat;
    property Pos: TglrVec3f read GetPos;
    property Rot: TglrQuat read Real;
  end;
{$ENDREGION}

{$REGION ' Matrices '}
  PglrMat4fArray = ^TglrMat4fArray;
  TglrMat4fArray = array [0..3, 0..3] of Single;

  PglrMat4fRowArray = ^TglrMat4fRowArray;
  TglrMat4fRowArray = array [0..3] of TglrVec4f;

  TglrMat4f = record
  private
    function  GetPos: TglrVec3f;
    procedure SetPos(const v: TglrVec3f);
    function  GetRot: TglrQuat;
    procedure SetRot(const q: TglrQuat);
  public
    e00, e10, e20, e30,
    e01, e11, e21, e31,
    e02, e12, e22, e32,
    e03, e13, e23, e33: Single;
    class operator Add(const a, b: TglrMat4f): TglrMat4f;
    class operator Multiply(const m: TglrMat4f; v: TglrVec3f): TglrVec3f;
    class operator Multiply(const a, b: TglrMat4f): TglrMat4f;
    class operator Multiply(const a: TglrMat4f; x: Single): TglrMat4f;
    procedure Identity;
    function  Det: Single;
    function  Inverse: TglrMat4f;
    function  Transpose: TglrMat4f;
    function  FromVectorAngle(const v: TglrVec3f; Angle: Single): TglrMat4f;
    procedure Ortho(Left, Right, Bottom, Top, ZNear, ZFar: Single);
    procedure Frustum(Left, Right, Bottom, Top, ZNear, ZFar: Single);
    procedure Perspective(FOV, Aspect, ZNear, ZFar: Single);
    procedure Translate(const v: TglrVec3f);
    procedure Rotate(Angle: Single; const v: TglrVec3f);
    procedure Scale(const v: TglrVec3f);

    property Pos: TglrVec3f read GetPos write SetPos;
    property Rot: TglrQuat read GetRot write SetRot;
  end;

{$ENDREGION}

  PglrVec2f = ^TglrVec2f;
  PglrVec3f = ^TglrVec3f;
  PglrVec4f = ^TglrVec4f;

const
    IdentMat : TglrMat4f = (
    e00: 1; e10: 0; e20: 0; e30: 0;
    e01: 0; e11: 1; e21: 0; e31: 0;
    e02: 0; e12: 0; e22: 1; e32: 0;
    e03: 0; e13: 0; e23: 0; e33: 1;
  );

function Max(x, y: Single): Single; overload; inline;
function Min(x, y: Single): Single; overload; inline;
function Max(x, y: Integer): Integer; overload; inline;
function Min(x, y: Integer): Integer; overload; inline;
function Sign(x: Single): Integer; inline;
function Equalf(x, y: Single): Boolean; inline;

function IsPointInBox(aPoint: TglrVec2f; aMin, aMax: TglrVec2f): Boolean;
function LineCircleIntersect(aLineStart, aLineVec: TglrVec2f;
  aCircleCenter: TglrVec2f; aRadius: Single): Boolean;

function Ceil(const X: Extended): Integer;
function Floor(const X: Extended): Integer;

function Tan(const X: Extended): Extended;
function ArcTan2(Y, X: Extended): Extended;
procedure SinCos(const Theta: Extended; out Sin, Cos: Extended);
function ArcCos(const X: Single): Single;
function ArcSin(const X: Single): Single;

function Clamp(x, Min, Max: Integer): Integer; overload; inline;
function Clamp(x, Min, Max: Single): Single; overload; inline;
function Pow(x, y: Single): Single;

function Lerp(aFrom, aTo, aT: Single): Single;
function LerpAngles(aFrom, aTo, t: Single): Single;

function Hsv2Rgb(hsv: TglrVec3f): TglrVec3f;
function Hsva2Rgba(hsva: TglrVec4f): TglrVec4f;
  
function Vec2f(x, y: Single): TglrVec2f; overload; inline;
function Vec2f(v: TglrVec3f): TglrVec2f; overload; inline;
function Vec2f(rotationAngle: Single): TglrVec2f; overload; inline;
function Vec3f(x, y, z: Single): TglrVec3f; overload; inline;
function Vec3f(v: TglrVec2f; z: Single): TglrVec3f; overload; inline;
function Vec3f(v: TglrVec4f): TglrVec3f; overload; inline;
function Vec4f(x, y, z: Single; w: Single = 0): TglrVec4f; overload; inline;
function Vec4f(v: TglrVec3f; w: Single = 0): TglrVec4f; overload; inline;
function Quat(x, y, z, w: Single): TglrQuat; overload; inline;
function Quat(const v: TglrVec3f; Angle: Single): TglrQuat; overload; inline;

implementation

function Max(x, y: Single): Single;
begin
  if x > y then
    Result := x
  else
    Result := y;
end;

function Min(x, y: Single): Single;
begin
  if x < y then
    Result := x
  else
    Result := y;
end;

function Max(x, y: Integer): Integer;
begin
  if x > y then
    Result := x
  else
    Result := y;
end;

function Min(x, y: Integer): Integer;
begin
  if x < y then
    Result := x
  else
    Result := y;
end;

function Sign(x: Single): Integer;
begin
  if x > 0 then
    Result := 1
  else
    if x < 0 then
      Result := -1
    else
      Result := 0;
end;

function Equalf(x, y: Single): Boolean;
begin
  Result := Abs(x - y) < cEPS;
end;

function IsPointInBox(aPoint: TglrVec2f; aMin, aMax: TglrVec2f): Boolean;
begin
  Result := (aPoint.x >= aMin.x) and (aPoint.x <= aMax.x) and
    (aPoint.y >= aMin.y) and (aPoint.y <= aMax.y);
end;

// ToDo: Useless variables, rewrite it?
function LineCircleIntersect(aLineStart, aLineVec: TglrVec2f;
  aCircleCenter: TglrVec2f; aRadius: Single): Boolean;
var
  lineStartToCenter: TglrVec2f;
  cosine, sine{, lineProjLength}, dist{, angle}: Single;
begin
  lineStartToCenter := (aCircleCenter - aLineStart);
  cosine := lineStartToCenter.Normal.Dot(aLineVec.Normal);
  //angle := arccos(cosine);
  sine := sqrt(1 - sqr(cosine));
  //lineProjLength := Abs(cosine * lineStartToCenter.Length);
  dist := Abs(sine * lineStartToCenter.Length);
  Result := (dist < aRadius);// and (sqr(lineProjLength) > aLineVec.LengthQ);
end;

function Ceil(const X: Extended): Integer;
begin
  Result := Integer(Trunc(X));
  if Frac(X) > 0 then
    Inc(Result);
end;

function Floor(const X: Extended): Integer;
begin
  Result := Integer(Trunc(X));
  if Frac(X) < 0 then
    Dec(Result);
end;
                    
function Tan(const X: Extended): Extended; assembler;
asm
  FLD    X
  FPTAN
  FSTP   ST(0)
  FWAIT
end;

function ArcTan2(Y, X: Extended): Extended; assembler;
asm
  FLD     Y
  FLD     X
  FPATAN
  FWAIT
end;

procedure SinCos(const Theta: Extended; out Sin, Cos: Extended); assembler;
asm
  FLD     Theta
  FSINCOS
  FSTP    tbyte ptr [edx]    // Cos
  FSTP    tbyte ptr [eax]    // Sin
  FWAIT
end;

function ArcCos(const X: Single): Single;
begin
  if abs(X) > 1 then
    Result := 0
  else
    Result := ArcTan2(Sqrt(1 - X * X), X);
end;

function ArcSin(const X: Single): Single;
begin
  Result := ArcTan2(X, Sqrt((1 + X) * (1 - X)));
end;

function Clamp(x, Min, Max: Integer): Integer;
begin
  if x < min then
    Result := min
  else
    if x > max then
      Result := max
    else
      Result := x;
end;

function Clamp(x, Min, Max: Single): Single;
begin
  if x < min then
    Result := min
  else
    if x > max then
      Result := max
    else
      Result := x;
end;

function Pow(x, y: Single): Single;
begin
  Result := exp(ln(x) * y);
end;

function Lerp(aFrom, aTo, aT: Single): Single;
begin
  Result := aFrom + (aTo - aFrom) * aT;
end;


function LerpAngles(aFrom, aTo, t: Single): Single;
begin
    // Ensure that 0 <= angle < 2pi for both "from" and "to"
  while (aFrom < 0) do
    aFrom := aFrom + twopi * rad2deg;
  while (aFrom >= twopi * rad2deg) do
    aFrom := aFrom - twopi * rad2deg;

  while (aTo < 0) do
    aTo := aTo + twopi * rad2deg;
  while (aTo >= twopi * rad2deg) do
    aTo := aTo - twopi * rad2deg;

  if (Abs(aFrom - aTo) < pi * rad2deg) then
  begin
    Exit(Lerp(aFrom, aTo, t));
  end;

  // If we get here we have the more complex case.
  // First, increment the lesser value to be greater.
  if (aFrom < aTo) then
    aFrom := aFrom + twopi * rad2deg
  else
    aTo := aTo + twopi * rad2deg;

  Result := Lerp(aFrom, aTo, t);

  // Now ensure the return value is between 0 and 2pi
  if (Result >= twopi * rad2deg) then
    Result := Result - twopi * rad2deg;
end;

{Hue: 0-360, Sat: 0-100, Value: 0-100}
{R, G, B: 0-1}
function Hsv2Rgb(hsv: TglrVec3f): TglrVec3f;
var
  Hi: Integer;
  Vmin, Vinc, Vdec, a: Single;
begin
  Hi := Round(hsv.x) div 60;
  if Hi > 5 then
    Hi := 5;
  Vmin := ((100 - hsv.y) * hsv.z) / 100;
  a := (hsv.z - Vmin) * (Round(hsv.x) mod 60) / 60;
  Vinc := Vmin + a;
  Vdec := hsv.z - a;
  case Hi of
    0: Exit(Vec3f(hsv.z, Vinc, Vmin) * 0.01);
    1: Exit(Vec3f(Vdec, hsv.z, Vmin) * 0.01);
    2: Exit(Vec3f(Vmin, hsv.z, Vinc) * 0.01);
    3: Exit(Vec3f(Vmin, Vdec, hsv.z) * 0.01);
    4: Exit(Vec3f(Vinc, Vmin, hsv.z) * 0.01);
    5: Exit(Vec3f(hsv.z, Vmin, Vdec) * 0.01);
  end;
end;

function Hsva2Rgba(hsva: TglrVec4f): TglrVec4f;
begin
  Result := Vec4f(Hsv2Rgb(Vec3f(hsva)), hsva.w);
end;

function Rgb2Hsv(rgb: TglrVec3f): TglrVec3f;
begin

end;


function Vec2f(x, y: Single): TglrVec2f;
begin
  Result.x := x;
  Result.y := y;
end;

function Vec2f(v: TglrVec3f): TglrVec2f;
begin
  Result.x := v.x;
  Result.y := v.y;
end;

function Vec2f(rotationAngle: Single): TglrVec2f;
begin
  Result := Vec2f(cos(-rotationAngle * deg2rad), sin(rotationAngle * deg2rad)).Normal;
end;

function Vec3f(x, y, z: Single): TglrVec3f; overload;
begin
  Result.x := x;
  Result.y := y;
  Result.z := z;
end;

function Vec3f(v: TglrVec2f; z: Single): TglrVec3f; overload;
begin
  Result.x := v.x;
  Result.y := v.y;
  Result.z := z;
end;

function Vec3f(v: TglrVec4f): TglrVec3f; overload;
begin
  Result.x := v.x;
  Result.y := v.y;
  Result.z := v.z;
end;

function Vec4f(x, y, z: Single; w: Single = 0): TglrVec4f; overload;
begin
  Result.x := x;
  Result.y := y;
  Result.z := z;
  Result.w := w;
end;

function Vec4f(v: TglrVec3f; w: Single = 0): TglrVec4f; overload;
begin
  Result.x := v.x;
  Result.y := v.y;
  Result.z := v.z;
  Result.w := w;
end;

function Quat(x, y, z, w: Single): TglrQuat;
begin
  Result.x := x;
  Result.y := y;
  Result.z := z;
  Result.w := w;
end;

function Quat(const v: TglrVec3f; Angle: Single): TglrQuat;
begin
  Result := Result.FromVectorAngle(v, Angle);
end;

{$REGION ' Single Vectors '}
class operator TglrVec2f.Equal(const v1, v2: TglrVec2f): Boolean;
begin
  Result := (Abs(v1.x - v2.x) <= cEPS)and
            (Abs(v1.y - v2.y) <= cEPS);
end;

class operator TglrVec2f.Add(const v1, v2: TglrVec2f): TglrVec2f;
begin
  Result.x := v1.x + v2.x;
  Result.y := v1.y + v2.y;
end;

class operator TglrVec2f.Subtract(const v1, v2: TglrVec2f): TglrVec2f;
begin
  Result.x := v1.x - v2.x;
  Result.y := v1.y - v2.y;
end;

class operator TglrVec2f.Negative(const v: TglrVec2f): TglrVec2f;
begin
  Result.x := - v.x;
  Result.y := - v.y;
end;

class operator TglrVec2f.Multiply(const v1, v2: TglrVec2f): TglrVec2f;
begin
  Result.x := v1.x * v2.x;
  Result.y := v1.y * v2.y;
end;

class operator TglrVec2f.Multiply(const v: TglrVec2f; a: Single): TglrVec2f;
begin
  Result.x := v.x * a;
  Result.y := v.y * a;
end;

class operator TglrVec2f.Multiply(const a: Single; const v: TglrVec2f): TglrVec2f;
begin
  Result := v * a;
end;

class operator TglrVec2f.Divide(const v: TglrVec2f; const a: Single): TglrVec2f;
begin
  Result := v * (1 / a);
end;

function TglrVec2f.Dot(const v: TglrVec2f): Single;
begin
  Result := x * v.x + y * v.y;
end;

function TglrVec2f.Reflect(const n: TglrVec2f): TglrVec2f;
begin
  Result := Self - (n * (2 * Dot(n)));
end;

function TglrVec2f.Refract(const n: TglrVec2f; Factor: Single): TglrVec2f;
var
  d, s : Single;
begin
  d := Dot(n);
  s := 1 - sqr(Factor) * (1 - sqr(d));
  if s < cEPS then
    Result := Reflect(n)
  else
    Result := Self * Factor - n * (sqrt(s) + d * Factor);
end;

function TglrVec2f.Length: Single;
begin
  Result := sqrt(LengthQ);
end;

function TglrVec2f.LengthQ: Single;
begin
  Result := sqr(x) + sqr(y);
end;

function TglrVec2f.Normal: TglrVec2f;
var
  Len : Single;
begin
  Len := Length;
  if Len < cEPS then
    Result := Vec2f(0, 0)
  else
    Result := Self * (1 / Len);
end;

function TglrVec2f.Dist(const v: TglrVec2f): Single;
begin
  Result := (v - Self).Length;
end;

function TglrVec2f.Lerp(const v: TglrVec2f; t: Single): TglrVec2f;
begin
  Result := Self + (v - Self) * t;
end;

function TglrVec2f.Clamp(const Min, Max: TglrVec2f): TglrVec2f;
begin
  Result := Vec2f(glr_math.Clamp(x, Min.x, Max.x), glr_math.Clamp(y, Min.y, Max.y));
end;

function TglrVec2f.Clamp(const Min, Max: Single): TglrVec2f;
begin
  if LengthQ < Sqr(Min) then
    Result := Self.Normal * Min
  else if LengthQ > Sqr(Max) then
    Result := Self.Normal * Max
  else
    Result := Self;
end;

function TglrVec2f.Rotate(Angle: Single): TglrVec2f;
var
  s, c : Extended;
begin
  SinCos(Angle, s, c);
  Result := Vec2f(x * c - y * s, x * s + y * c);
end;

function TglrVec2f.Angle(const v: TglrVec2f): Single;
begin
  Result := ArcCos(Dot(v) / sqrt(LengthQ * v.LengthQ))
end;

function TglrVec2f.NegateVector(): TglrVec2f;
begin
  Result.x := -x;
  Result.y := -y;
end;

procedure TglrVec2f.Negate;
begin
  x := -x;
  y := -y;
end;

procedure TglrVec2f.Normalize();
begin
  Self := Self.Normal();
end;

procedure TglrVec2f.Reset();
begin
  x := 0;
  y := 0;
end;

function TglrVec2f.GetRotationAngle(): Single;
begin
  Result := {90 + }ArcTan2(y, x) *  rad2deg;
//  if y > 0 then
//    Result := 90 + rad2deg * ArcCos(x)
//  else
//    Result := 90 - rad2deg * ArcCos(x);
end;

//======================Vec3f

class operator TglrVec3f.Equal(const v1, v2: TglrVec3f): Boolean;
begin
  Result := (Abs(v1.x - v2.x) <= cEPS)and
            (Abs(v1.y - v2.y) <= cEPS)and
            (Abs(v1.z - v2.z) <= cEPS);
end;

class operator TglrVec3f.Add(const v1, v2: TglrVec3f): TglrVec3f;
begin
  Result.x := v1.x + v2.x;
  Result.y := v1.y + v2.y;
  Result.z := v1.z + v2.z;
end;

class operator TglrVec3f.Subtract(const v1, v2: TglrVec3f): TglrVec3f;
begin
  Result.x := v1.x - v2.x;
  Result.y := v1.y - v2.y;
  Result.z := v1.z - v2.z;
end;

class operator TglrVec3f.Multiply(const v1, v2: TglrVec3f): TglrVec3f;
begin
  Result.x := v1.x * v2.x;
  Result.y := v1.y * v2.y;
  Result.z := v1.z * v2.z;
end;

class operator TglrVec3f.Multiply(const v: TglrVec3f; a: Single): TglrVec3f;
begin
  Result.x := a * v.x;
  Result.y := a * v.y;
  Result.z := a * v.z;
end;

class operator TglrVec3f.Multiply(const a: Single; const v: TglrVec3f): TglrVec3f;
begin
  Result := v * a;
end;

class operator TglrVec3f.Divide(const v: TglrVec3f; const a: Single): TglrVec3f;
begin
  Result := v * (1 / a);
end;

class operator TglrVec3f.Negative(const v: TglrVec3f): TglrVec3f;
begin
  Result.x := - v.x;
  Result.y := - v.y;
  Result.z := - v.z;
end;

function TglrVec3f.Lerp(const v: TglrVec3f; t: Single): TglrVec3f;
begin
  Result := Self + (v - Self) * t;
end;

function TglrVec3f.Dot(const v: TglrVec3f): Single;
begin
  Result := x * v.x + y * v.y + z * v.z;
end;

function TglrVec3f.Cross(const v: TglrVec3f): TglrVec3f;
begin
  Result.x := y * v.z - z * v.y;
  Result.y := z * v.x - x * v.z;
  Result.z := x * v.y - y * v.x;
end;

function TglrVec3f.Normal: TglrVec3f;
var
  Len : Single;
begin
  Len := Length;
  if Len > cEPS then
    Result := Self * (1 / Len)
  else
    Result := Vec3f(0, 0, 0);
end;

procedure TglrVec3f.Normalize;
begin
  Self := Normal;
end;

function TglrVec3f.Length: Single;
begin
  Result := sqrt(LengthQ);
end;

function TglrVec3f.LengthQ: Single;
begin
  Result := sqr(x) + sqr(y) + sqr(z);
end;

function TglrVec3f.DistAbs(const v: TglrVec3f): Single;
begin
  Result := abs(x - v.x) + abs(y - v.y) + abs(z - v.z);
end;

function TglrVec3f.Reflect(const n: TglrVec3f): TglrVec3f;
begin
  Result := Self - (n * (2 * Dot(n)));
end;

function TglrVec3f.Refract(const n: TglrVec3f; Factor: Single): TglrVec3f;
var
  d, s : Single;
begin
  d := Dot(n);
  s := 1 - sqr(Factor) * (1 - sqr(d));
  if s < cEPS then
    Result := Reflect(n)
  else
    Result := Self * Factor  - n * (sqrt(s) + d * Factor);
end;

function TglrVec3f.Clamp(const Min, Max: TglrVec3f): TglrVec3f;
begin
  Result := Vec3f(glr_math.Clamp(x, Min.x, Max.x), glr_math.Clamp(y, Min.y, Max.y), glr_math.Clamp(z, Min.z, Max.z));
end;

function TglrVec3f.Rotate(Angle: Single; const Axis: TglrVec3f): TglrVec3f;
var
  s, c : Extended;
  v0, v1, v2 : TglrVec3f;
begin
  SinCos(Angle, s, c);
  v0 := Axis * Dot(Axis);
  v1 := Self - v0;
  v2 := Axis.Cross(v1);
  Result.x := v0.x + v1.x * c + v2.x * s;
  Result.y := v0.y + v1.y * c + v2.y * s;
  Result.z := v0.z + v1.z * c + v2.z * s;
end;

function TglrVec3f.NegateVector(): TglrVec3f;
begin
  Result.x := -x;
  Result.y := -y;
  Result.z := -z;
end;

procedure TglrVec3f.Negate;
begin
  x := -x;
  y := -y;
  z := -z;
end;

procedure TglrVec3f.Reset();
begin
  x := 0;
  y := 0;
  z := 0;
end;

//======================Vec4f

class operator TglrVec4f.Equal(const v1, v2: TglrVec4f): Boolean;
begin
  Result := (Abs(v1.x - v2.x) <= cEPS) and
            (Abs(v1.y - v2.y) <= cEPS) and
            (Abs(v1.z - v2.z) <= cEPS) and
            (Abs(v1.w - v2.w) <= cEPS);
end;

class operator TglrVec4f.Multiply(const A: Single; v: TglrVec4f): TglrVec4f;
begin
  Result.x := A * v.x;
  Result.y := A * v.y;
  Result.z := A * v.z;
  Result.w := v.w;
end;

class operator TglrVec4f.Multiply(const v: TglrVec4f; a: Single): TglrVec4f;
begin
  result := a * v;
end;

class operator TglrVec4f.Multiply(const v1, v2: TglrVec4f): TglrVec4f;
begin
  Result.x := v1.x * v2.x;
  Result.y := v1.y * v2.y;
  Result.z := v1.z * v2.z;
  Result.w := v1.w * v2.w;
end;

class operator TglrVec4f.Add(const v1, v2: TglrVec4f): TglrVec4f;
begin
  with Result do
  begin
    x := v1.x + v2.x;
    y := v1.y + v2.y;
    z := v1.z + v2.z;
    w := v1.w + v2.w;
  end;
end;

class operator TglrVec4f.Subtract(const v1, v2: TglrVec4f): TglrVec4f;
begin
  with Result do
  begin
    x := v1.x - v2.x;
    y := v1.y - v2.y;
    z := v1.z - v2.z;
    w := v1.w - v2.w;
  end;
end;

function TglrVec4f.Dot(const v: TglrVec3f): Single;
begin
  Result := x * v.x + y * v.y + z * v.z + w;
end;

function TglrVec4f.Normal: TglrVec4f;
var
  Len : Single;
begin
  Len := sqrt(sqr(x) + sqr(y) + sqr(z) + sqr(w));
  if Len > cEPS then
    Self := (1 / Len) * Self 
  else
    Result := Self;
end;

procedure TglrVec4f.Normalize;
begin
  Self := Normal;
end;

procedure TglrVec4f.Reset;
begin
  x := 0;
  y := 0;
  z := 0;
  w := 0;
end;

{$ENDREGION}

{$REGION ' Quaternions '}
class operator TglrQuat.Equal(const q1, q2: TglrQuat): Boolean;
begin
  Result := (abs(q1.x - q2.x) <= cEPS) and
            (abs(q1.y - q2.y) <= cEPS) and
            (abs(q1.z - q2.z) <= cEPS) and
            (abs(q1.w - q2.w) <= cEPS);
end;

class operator TglrQuat.Add(const q1, q2: TglrQuat): TglrQuat;
begin
  Result.x := q1.x + q2.x;
  Result.y := q1.y + q2.y;
  Result.z := q1.z + q2.z;
  Result.w := q1.w + q2.w;
end;

class operator TglrQuat.Subtract(const q1, q2: TglrQuat): TglrQuat;
begin
  Result.x := q1.x - q2.x;
  Result.y := q1.y - q2.y;
  Result.z := q1.z - q2.z;
  Result.w := q1.w - q2.w;
end;

class operator TglrQuat.Multiply(const q: TglrQuat; x: Single): TglrQuat;
begin
  Result.x := q.x * x;
  Result.y := q.y * x;
  Result.z := q.z * x;
  Result.w := q.w * x;
end;

class operator TglrQuat.Multiply(const q1, q2: TglrQuat): TglrQuat;
begin
  Result.x := q1.w * q2.x + q1.x * q2.w + q1.y * q2.z - q1.z * q2.y;
  Result.y := q1.w * q2.y + q1.y * q2.w + q1.z * q2.x - q1.x * q2.z;
  Result.z := q1.w * q2.z + q1.z * q2.w + q1.x * q2.y - q1.y * q2.x;
  Result.w := q1.w * q2.w - q1.x * q2.x - q1.y * q2.y - q1.z * q2.z;
end;

class operator TglrQuat.Multiply(const q: TglrQuat; const v: TglrVec3f): TglrVec3f;
begin
  with (q * Quat(v.x, v.y, v.z, 0) * q.Invert) do
    Result := Vec3f(x, y, z);
end;

function TglrQuat.Invert: TglrQuat;
begin
  Result := Quat(-x, -y, -z, w);
end;

function TglrQuat.Lerp(const q: TglrQuat; t: Single): TglrQuat;
begin
  if Dot(q) < 0 then
    Result := Self - (q + Self) * t
  else
    Result := Self + (q - Self) * t;
end;

function TglrQuat.Dot(const q: TglrQuat): Single;
begin
  Result := x * q.x + y * q.y + z * q.z + w * q.w;
end;

procedure TglrQuat.Normalize;
var
  Len : Single;
begin
  Len := sqrt(sqr(x) + sqr(y) + sqr(z) + sqr(w));
  if Len > 0 then
  begin
    Len := 1 / Len;
    x := x * Len;
    y := y * Len;
    z := z * Len;
    w := w * Len;
  end;
end;

function TglrQuat.FromVectorAngle(const v: TglrVec3f; Angle: Single): TglrQuat;
var
  s, c : Extended;
begin
  SinCos(Angle * 0.5, s, c);
  Result.x := v.x * s;
  Result.y := v.y * s;
  Result.z := v.z * s;
  Result.w := c;
end;

function TglrQuat.Euler: TglrVec3f;
var
  D : Single;
begin
  D := 2 * x * z + y * w;
  if abs(D) > 1 - cEPS then
  begin
    Result.x := 0;
    if D > 0 then
      Result.y := -pi * 0.5
    else
      Result.y :=  pi * 0.5;
    Result.z := ArcTan2(-2 * (y * z - w * x), 2 * (w * w + y * y) - 1);
  end else
  begin
    Result.x := -ArcTan2(2 * (y * z + w * x), 2 * (w * w + z * z) - 1);
    Result.y := ArcSin(-d);
    Result.z := -ArcTan2(2 * (x * y + w * z), 2 * (w * w + x * x) - 1);
  end;
end;

class operator TglrDualQuat.Multiply(const dq1, dq2: TglrDualQuat): TglrDualQuat;
begin
  Result.Real := dq1.Real * dq2.Real;
  Result.Dual := dq1.Real * dq2.Dual + dq1.Dual * dq2.Real;
end;

function TglrDualQuat.Lerp(const dq: TglrDualQuat; t: Single): TglrDualQuat;
begin
  if Real.Dot(dq.Real) < 0 then
  begin
    Result.Real := Real - (dq.Real + Real) * t;
    Result.Dual := Dual - (dq.Dual + Dual) * t;
  end else
  begin
    Result.Real := Real + (dq.Real - Real) * t;
    Result.Dual := Dual + (dq.Dual - Dual) * t;
  end;
end;

function TglrDualQuat.GetPos: TglrVec3f;
begin
  with Result do
  begin
    x := (Dual.x * Real.w - Real.x * Dual.w + Real.y * Dual.z - Real.z * Dual.y) * 2;
    y := (Dual.y * Real.w - Real.y * Dual.w + Real.z * Dual.x - Real.x * Dual.z) * 2;
    z := (Dual.z * Real.w - Real.z * Dual.w + Real.x * Dual.y - Real.y * Dual.x) * 2;
  end;
end;

{$ENDREGION}

{$REGION ' Matrices '}
class operator TglrMat4f.Add(const a, b: TglrMat4f): TglrMat4f;
var
  MA   : TglrMat4fArray absolute a;
  MB   : TglrMat4fArray absolute b;
  Res  : TglrMat4fArray absolute Result;
  i, j : Integer;
begin
  for i := 0 to 3 do
    for j := 0 to 3 do
      Res[i][j] := MA[i][j] + MB[i][j];
end;

class operator TglrMat4f.Multiply(const m: TglrMat4f; v: TglrVec3f): TglrVec3f;
begin
  with m do
    Result := Vec3f(e00 * v.x + e01 * v.y + e02 * v.z + e03,
                      e10 * v.x + e11 * v.y + e12 * v.z + e13,
                      e20 * v.x + e21 * v.y + e22 * v.z + e23);
end;

class operator TglrMat4f.Multiply(const a, b: TglrMat4f): TglrMat4f;
begin                    
  with Result do
  begin
    e00 := a.e00 * b.e00 + a.e01 * b.e10 + a.e02 * b.e20 + a.e03 * b.e30;
    e10 := a.e10 * b.e00 + a.e11 * b.e10 + a.e12 * b.e20 + a.e13 * b.e30;
    e20 := a.e20 * b.e00 + a.e21 * b.e10 + a.e22 * b.e20 + a.e23 * b.e30;
    e30 := a.e30 * b.e00 + a.e31 * b.e10 + a.e32 * b.e20 + a.e33 * b.e30;
    e01 := a.e00 * b.e01 + a.e01 * b.e11 + a.e02 * b.e21 + a.e03 * b.e31;
    e11 := a.e10 * b.e01 + a.e11 * b.e11 + a.e12 * b.e21 + a.e13 * b.e31;
    e21 := a.e20 * b.e01 + a.e21 * b.e11 + a.e22 * b.e21 + a.e23 * b.e31;
    e31 := a.e30 * b.e01 + a.e31 * b.e11 + a.e32 * b.e21 + a.e33 * b.e31;
    e02 := a.e00 * b.e02 + a.e01 * b.e12 + a.e02 * b.e22 + a.e03 * b.e32;
    e12 := a.e10 * b.e02 + a.e11 * b.e12 + a.e12 * b.e22 + a.e13 * b.e32;
    e22 := a.e20 * b.e02 + a.e21 * b.e12 + a.e22 * b.e22 + a.e23 * b.e32;
    e32 := a.e30 * b.e02 + a.e31 * b.e12 + a.e32 * b.e22 + a.e33 * b.e32;
    e03 := a.e00 * b.e03 + a.e01 * b.e13 + a.e02 * b.e23 + a.e03 * b.e33;
    e13 := a.e10 * b.e03 + a.e11 * b.e13 + a.e12 * b.e23 + a.e13 * b.e33;
    e23 := a.e20 * b.e03 + a.e21 * b.e13 + a.e22 * b.e23 + a.e23 * b.e33;
    e33 := a.e30 * b.e03 + a.e31 * b.e13 + a.e32 * b.e23 + a.e33 * b.e33;
  end;
end;

class operator TglrMat4f.Multiply(const a: TglrMat4f; x: Single): TglrMat4f;
var
  i, j : Integer;
  Mat  : TglrMat4fArray absolute a;
  Res  : TglrMat4fArray absolute Result;
begin
  for i := 0 to 3 do
    for j := 0 to 3 do
      Res[i, j] := Mat[i, j] * x;
end;

procedure TglrMat4f.Identity;
begin
  Self := IdentMat;
end;

function TglrMat4f.Det: Single;
begin
  Result := e00 * (e11 * (e22 * e33 - e32 * e23) - e21 * (e12 * e33 - e32 * e13) + e31 * (e12 * e23 - e22 * e13)) -
            e10 * (e01 * (e22 * e33 - e32 * e23) - e21 * (e02 * e33 - e32 * e03) + e31 * (e02 * e23 - e22 * e03)) +
            e20 * (e01 * (e12 * e33 - e32 * e13) - e11 * (e02 * e33 - e32 * e03) + e31 * (e02 * e13 - e12 * e03)) -
            e30 * (e01 * (e12 * e23 - e22 * e13) - e11 * (e02 * e23 - e22 * e03) + e21 * (e02 * e13 - e12 * e03));
end;

function TglrMat4f.Inverse: TglrMat4f;
var
  D : Single;
begin
  D := 1 / Det;
  Result.e00 :=  (e11 * (e22 * e33 - e32 * e23) - e21 * (e12 * e33 - e32 * e13) + e31 * (e12 * e23 - e22 * e13)) * D;
  Result.e01 := -(e01 * (e22 * e33 - e32 * e23) - e21 * (e02 * e33 - e32 * e03) + e31 * (e02 * e23 - e22 * e03)) * D;
  Result.e02 :=  (e01 * (e12 * e33 - e32 * e13) - e11 * (e02 * e33 - e32 * e03) + e31 * (e02 * e13 - e12 * e03)) * D;
  Result.e03 := -(e01 * (e12 * e23 - e22 * e13) - e11 * (e02 * e23 - e22 * e03) + e21 * (e02 * e13 - e12 * e03)) * D;
  Result.e10 := -(e10 * (e22 * e33 - e32 * e23) - e20 * (e12 * e33 - e32 * e13) + e30 * (e12 * e23 - e22 * e13)) * D;
  Result.e11 :=  (e00 * (e22 * e33 - e32 * e23) - e20 * (e02 * e33 - e32 * e03) + e30 * (e02 * e23 - e22 * e03)) * D;
  Result.e12 := -(e00 * (e12 * e33 - e32 * e13) - e10 * (e02 * e33 - e32 * e03) + e30 * (e02 * e13 - e12 * e03)) * D;
  Result.e13 :=  (e00 * (e12 * e23 - e22 * e13) - e10 * (e02 * e23 - e22 * e03) + e20 * (e02 * e13 - e12 * e03)) * D;
  Result.e20 :=  (e10 * (e21 * e33 - e31 * e23) - e20 * (e11 * e33 - e31 * e13) + e30 * (e11 * e23 - e21 * e13)) * D;
  Result.e21 := -(e00 * (e21 * e33 - e31 * e23) - e20 * (e01 * e33 - e31 * e03) + e30 * (e01 * e23 - e21 * e03)) * D;
  Result.e22 :=  (e00 * (e11 * e33 - e31 * e13) - e10 * (e01 * e33 - e31 * e03) + e30 * (e01 * e13 - e11 * e03)) * D;
  Result.e23 := -(e00 * (e11 * e23 - e21 * e13) - e10 * (e01 * e23 - e21 * e03) + e20 * (e01 * e13 - e11 * e03)) * D;
  Result.e30 := -(e10 * (e21 * e32 - e31 * e22) - e20 * (e11 * e32 - e31 * e12) + e30 * (e11 * e22 - e21 * e12)) * D;
  Result.e31 :=  (e00 * (e21 * e32 - e31 * e22) - e20 * (e01 * e32 - e31 * e02) + e30 * (e01 * e22 - e21 * e02)) * D;
  Result.e32 := -(e00 * (e11 * e32 - e31 * e12) - e10 * (e01 * e32 - e31 * e02) + e30 * (e01 * e12 - e11 * e02)) * D;
  Result.e33 :=  (e00 * (e11 * e22 - e21 * e12) - e10 * (e01 * e22 - e21 * e02) + e20 * (e01 * e12 - e11 * e02)) * D;
end;

function TglrMat4f.Transpose: TglrMat4f;
var
  i, j : Integer;
  Res  : array [0..3, 0..3] of Single absolute Result;
begin
  for i := 0 to 3 do
    for j := 0 to 3 do
      Res[i, j] := PglrMat4fArray(@Self)^[j, i];
end;

function TglrMat4f.FromVectorAngle(const v: TglrVec3f; Angle: Single): TglrMat4f;
var
  s, c  : Extended;
  ic : Single;
  xy, yz, zx, xs, ys, zs, icxy, icyz, iczx : Single;
begin
  SinCos(Angle, s, c);
  ic := 1 - c;

  with Result, v.Normal do
  begin
    xy := x * y;
    yz := y * z;
    zx := z * x;
    xs := x * s;
    ys := y * s;
    zs := z * s;
    icxy := ic * xy;
    icyz := ic * yz;
    iczx := ic * zx;
    e00 := ic * x * x + c;  e01 := icxy - zs;       e02 := iczx + ys;       e03 := 0.0;
    e10 := icxy + zs;       e11 := ic * y * y + c;  e12 := icyz - xs;       e13 := 0.0;
    e20 := iczx - ys;       e21 := icyz + xs;       e22 := ic * z * z + c;  e23 := 0.0;
    e30 := 0.0;             e31 := 0.0;             e32 := 0.0;             e33 := 1.0;
  end;
end;

procedure TglrMat4f.Translate(const v: TglrVec3f);
var
  m : TglrMat4f;
begin
  m.Identity;
  m.Pos := v;
  Self := Self * m;
end;

procedure TglrMat4f.Rotate(Angle: Single; const v: TglrVec3f);
var
  m : TglrMat4f;
begin
  m := m.FromVectorAngle(v, Angle);
  Self := Self * m;
end;

procedure TglrMat4f.Scale(const v: TglrVec3f);
var
  m : TglrMat4f;
begin
  m.Identity;
  m.e00 := v.x;
  m.e11 := v.y;
  m.e22 := v.z;
  Self := m * Self;
end;

procedure TglrMat4f.Ortho(Left, Right, Bottom, Top, ZNear, ZFar: Single);
begin
  e00 := 2 / (Right - Left);
  e10 := 0;
  e20 := 0;
  e30 := 0;

  e01 := 0;
  e11 := 2 / (Top - Bottom);
  e21 := 0;
  e31 := 0;

  e02 := 0;
  e12 := 0;
  e22 := -2 / (ZFar - ZNear);
  e32 := 0;

  e03 := -(Right + Left) / (Right - Left);
  e13 := -(Top + Bottom) / (Top - Bottom);
  e23 := -(ZFar + ZNear) / (ZFar - ZNear);
  e33 := 1;
end;

procedure TglrMat4f.Frustum(Left, Right, Bottom, Top, ZNear, ZFar: Single);
begin
  e00 := 2 * ZNear / (Right - Left);
  e10 := 0;
  e20 := 0;
  e30 := 0;

  e01 := 0;
  e11 := 2 * ZNear / (Top - Bottom);
  e21 := 0;
  e31 := 0;

  e02 := (Right + Left) / (Right - Left);
  e12 := (Top + Bottom) / (Top - Bottom);
  e22 := (ZFar + ZNear) / (ZNear - ZFar);
  e32 := -1;

  e03 := 0;
  e13 := 0;
  e23 := 2 * ZFar * ZNear / (ZNear - ZFar);
  e33 := 0;
end;

procedure TglrMat4f.Perspective(FOV, Aspect, ZNear, ZFar: Single);
var
  x, y : Single;
begin
  FOV := min(179.9, max(0, FOV));
  y := ZNear * Tan(FOV * deg2rad * 0.5);
  x := y * Aspect;
  Frustum(-x, x, -y, y, ZNear, ZFar);
end;

function TglrMat4f.GetPos: TglrVec3f;
begin
  Result := Vec3f(e03, e13, e23);
end;

procedure TglrMat4f.SetPos(const v: TglrVec3f);
begin
  e03 := v.x;
  e13 := v.y;
  e23 := v.z;
end;

function TglrMat4f.GetRot: TglrQuat;
var
  t, s : Single;
begin
  t := e00 + e11 + e22 + 1;
  with Result do
    if t > cEPS then
    begin
      s := 0.5 / sqrt(t);
      w := 0.25 / s;
      x := (e21 - e12) * s;
      y := (e02 - e20) * s;
      z := (e10 - e01) * s;
    end else
    begin
      if (e00 > e11) and (e00 > e22) then
      begin
        s := 2 * sqrt(1 + e00 - e11 - e22);
        w := (e21 - e12) / s;
        x := 0.25 * s;
        y := (e01 + e10) / s;
        z := (e02 + e20) / s;
      end else
        if e11 > e22 then
        begin
          s := 2 * sqrt(1 + e11 - e00 - e22);
          w := (e02 - e20) / s;
          x := (e01 + e10) / s;
          y := 0.25 * s;
          z := (e12 + e21) / s;
        end else
        begin
          s := 2 * sqrt(1 + e22 - e00 - e11);
          w := (e10 - e01) / s;
          x := (e02 + e20) / s;
          y := (e12 + e21) / s;
          z := 0.25 * s;
        end;
      Normalize;
    end;
end;

procedure TglrMat4f.SetRot(const q: TglrQuat);
var
  sqw, sqx, sqy, sqz, invs : Single;
  tmp1, tmp2 : Single;
begin
  with q do
  begin
    sqw := w * w;
    sqx := x * x;
    sqy := y * y;
    sqz := z * z;

    invs := 1 / (sqx + sqy + sqz + sqw);
    e00 := ( sqx - sqy - sqz + sqw) * invs;
    e11 := (-sqx + sqy - sqz + sqw) * invs;
    e22 := (-sqx - sqy + sqz + sqw) * invs;

    tmp1 := x * y;
    tmp2 := z * w;
    e10 := 2 * (tmp1 + tmp2) * invs;
    e01 := 2 * (tmp1 - tmp2) * invs;

    tmp1 := x * z;
    tmp2 := y * w;
    e20 := 2 * (tmp1 - tmp2) * invs;
    e02 := 2 * (tmp1 + tmp2) * invs;
    
    tmp1 := y * z;
    tmp2 := x * w;
    e21 := 2 * (tmp1 + tmp2) * invs;
    e12 := 2 * (tmp1 - tmp2) * invs;
  end;
end;

{$ENDREGION}

end.
