unit uObjects;

interface

uses
  glr_render2d,
  glr_utils,
  glr_math;
(*
type
  { TPopupText }

  TPopupText = class (TglrText)
  private
    procedure SetEnabled(const aEnabled: Boolean);
  public
    T: Single;
    property Enabled: Boolean read fVisible write SetEnabled;
  end;

  { TPopupManager }

  TPopupManager = class (TglrPool<TPopupText>)
  private
    fBatch: TglrFontBatch;
  public
    constructor Create(aBatch: TglrFontBatch); virtual;

    procedure Update(const dt: Double);
    procedure RenderSelf();
  end;


  TBonusType = (bHealth);

  { TBonus }

  TBonus = class (TglrSprite)
  private
    fT, fLifeTime: Single;
    fBonusType: TBonusType;
    fVelocity: TglrVec3f;
    procedure SetEnabled(const aEnabled: Boolean);
    procedure SetBonusType(aBonusType: TBonusType);
  public
    property BonusType: TBonusType read fBonusType write SetBonusType;
    procedure Use();

    property Enabled: Boolean read fVisible write SetEnabled;
  end;

  { TBonusManager }

  TBonusManager = class (TglrPool<TBonus>)
  private
    fBatch: TglrFontBatch;
  public
    constructor Create(aBatch: TglrFontBatch); virtual;

    procedure Update(const dt: Double);
    procedure RenderSelf();
  end;

  TBulletType = (bSimple, bFragile, bRocket);

  TBulletOwner = (bPlayer, bEnemy);

  { TBullet }

  TBullet = class (TglrSprite)
  private
    procedure SetEnabled(const aEnabled: Boolean);
  public
    T, LifeTime: Single;
    Velocity: TglrVec2f;
    BType: TBulletType;
    Owner: TBulletOwner;
    Damage: Integer;
    property Enabled: Boolean read fVisible write SetEnabled;
  end;

  { TBulletManager }

  TBulletManager = class (TglrPool<TBullet>)
  private
    fBatch: TglrSpriteBatch;
  public
    constructor Create(aBatch: TglrSpriteBatch); virtual;

    procedure Update(const dt: Double);
    procedure RenderSelf();
  end;
   *)
implementation

end.

