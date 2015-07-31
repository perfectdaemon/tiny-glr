unit uUnit;

interface

uses
  glr_render2d,
  uObjects;
(*
type

  { TUnit }

  TUnit = class (TglrSprite)
  protected
    fBulletOwner: TBulletOwner;
    fWeaponDir: TglrVec2f;
    fT, fSmokeT: Single; // for shooting

    fBonusTriple, fBonusRicochet: Boolean;
    fBonusT: array[TBonusType] of Single;
    procedure SetEnabled(const aEnabled: Boolean); virtual;
  public
    HealthMax: Integer;
    FireThreshold, RotateSpeed, DirectSpeed: Single;
    MainWeaponVelocity, MainWeaponDispersion: Single;
    AltWeaponVelocity: Single;
    MainWeaponDamage, AltWeaponDamage: Integer;

    Health: Integer;
    Weapon: TglrSprite;

    AltWeaponCount: Integer;

    constructor Create(); virtual;
    destructor Destroy(); override;

    procedure Update(const dt: Double; axisX, axisY: Integer); virtual;
    procedure Fire();
    procedure FireAlternative();

    procedure GetKilled(); virtual;

    property Enabled: Boolean read fVisible write SetEnabled;
  end;

  { TPlayer }

  TPlayer = class (TUnit)
  protected
    fHealthAnimateT, fGameOverT: Single;
    procedure SetEnabled(const aEnabled: Boolean); override;
  public
    FrontBumper: TglrSprite;
    BonusInfo, HealthText, AltWeaponText: TglrText;
    constructor Create(); override;
    destructor Destroy(); override;
    procedure Update(const dt: Double; axisX, axisY: Integer); override;
    procedure GetKilled(); override;
  end;

  { TEnemy }

  TEnemy = class (TUnit)
  protected
    procedure SetEnabled(const aEnabled: Boolean); override;
  public
    class var
      _HealthMax: Integer;
      _FireThreshold, _RotateSpeed, _DirectSpeed: Single;
      _MainWeaponVelocity, _MainWeaponDispersion: Single;
      _AltWeaponVelocity: Single;
      _MainWeaponDamage, _AltWeaponDamage: Integer;
    constructor Create(); override;
    procedure Update(const dt: Double; axisX, axisY: Integer); override;
    procedure GetKilled(); override;
  end;

  { TEnemyManager }

  TEnemyManager = class (TglrPool<TEnemy>)
  private
    fT: Single;
    fBatch: TglrSpriteBatch;
  public
    EnemySpawnInterval: Single;
    EnemySpawnCount: Integer;
    WaveCount: Integer;
    constructor Create(aBatch: TglrSpriteBatch); virtual;

    procedure Update(const dt: Double);
    procedure RenderSelf();
  end;
*)
implementation

end.

