unit glr_particles2d;

{$i defines.inc}

interface

uses
  glr_render, glr_render2d, glr_utils, glr_math;

type

  { TglrParticle2D }

  TglrParticle2D = class (TglrSprite)
    T: Single;
    LifeTime: Single;
    Velocity: TglrVec2f;
    procedure Reset();
  end;
  TglrParticles2D = TglrObjectList<TglrParticle2D>;

  TglrUpdateCallback = procedure(const dt: Double) of object;

  { TglrCustomParticleEmitter2D }

  TglrCustomParticleEmitter2D = class
  protected
    fBatch: TglrSpriteBatch;
    fMaterial: TglrMaterial;
    fTextureRegion: PglrTextureRegion;
    fActiveParticles: Integer;
  public
    Visible, Enabled: Boolean;
    Duration, Time: Single;

    Particles: TglrParticles2D;

    OnUpdate: TglrUpdateCallback;

    constructor Create(aBatch: TglrSpriteBatch;
      aMaterial: TglrMaterial;
      aTextureRegion: PglrTextureRegion = nil); virtual;
    destructor Destroy(); override;

    function GetNewParticle(): TglrParticle2D;

    procedure Update(const dt: Double);
    procedure RenderSelf();

    property ActiveParticlesCount: Integer read fActiveParticles;
  end;

  //Range of byte is 0..100 (means percent of emitter animation duration)
  TglrSingleDic = TglrDictionary<Byte, Single>;
  TglrIntDic = TglrDictionary<Byte, Integer>;
  TglrVec2fDic = TglrDictionary<Byte, TglrVec2f>;
  TglrVec4fDic = TglrDictionary<Byte, TglrVec4f>;

  { TglrParticleEmitter2D }

  TglrParticleEmitter2D = class
  protected
    fBatch: TglrSpriteBatch;
    fMaterial: TglrMaterial;
    fTextureRegion: PglrTextureRegion;
    fParticles: TglrParticles2D;

    function GetFreeParticleIndex(): Integer;
  public
    //Dynamics
    OriginBoxMinMax: TglrVec4fDic;
    VelocityMinMax: TglrVec4fDic;
    VelocityDispersionAngle: TglrSingleDic;
    VelocityAngularMinMax: TglrVec2fDic;
    Color: TglrVec4fDic;
    ParticlesPerSecond: TglrIntDic;
    LifetimeMinMax: TglrVec2fDic;
    ParticleSizeMinMax: TglrVec4fDic;

    //Params
    Duration, Time: Single;

    constructor Create(aBatch: TglrSpriteBatch;
      aMaterial: TglrMaterial; aTextureRegion: PglrTextureRegion = nil); virtual; overload;
    constructor Create(aBatch: TglrSpriteBatch;
      aMaterial: TglrMaterial;
      const aStream: TglrStream;
      aTextureRegion: PglrTextureRegion = nil;
      aFreeStreamOnFinish: Boolean = True); virtual; overload;
    destructor Destroy(); override;

    function SaveToStream(): TglrStream;

    procedure Update(const dt: Double);
  end;

implementation

{ TglrParticle2D }

procedure TglrParticle2D.Reset;
begin
  T := 0;
  Velocity.Reset();
  LifeTime := 0.0;
  fRot := 0;
  Visible := True;
end;

{ TglrCustomParticleEmitter2D }

constructor TglrCustomParticleEmitter2D.Create(aBatch: TglrSpriteBatch;
  aMaterial: TglrMaterial; aTextureRegion: PglrTextureRegion);
var
  s: TglrParticle2D;
  i: Integer;
begin
  inherited Create();
  fBatch := aBatch;
  fMaterial := aMaterial;
  fTextureRegion := aTextureRegion;
  Particles := TglrParticles2D.Create(128);

  for i := 0 to Particles.Count - 1 do
  begin
    s := TglrParticle2D.Create();
    if fTextureRegion <> nil then
      s.SetTextureRegion(fTextureRegion)
    else
    begin
      s.Width := fMaterial.Textures[0].Texture.Width;
      s.Height := fMaterial.Textures[0].Texture.Height;
    end;
    s.Reset();
    s.Visible := False;
    Particles.Add(s);
  end;

  Duration := 1.0;
  Time := 0;
  Enabled := True;
  Visible := True;
end;

destructor TglrCustomParticleEmitter2D.Destroy;
begin
  Particles.Free(True);
  inherited Destroy;
end;

function TglrCustomParticleEmitter2D.GetNewParticle: TglrParticle2D;
var
  i: Integer;
  p: TglrParticle2D;
begin
  fActiveParticles += 1;

  for i := 0 to Particles.Count - 1 do
    if not Particles[i].Visible then
    begin
      Particles[i].Reset();
      if (fTextureRegion <> nil) then
        Particles[i].SetTextureRegion(fTextureRegion);
      Particles[i].SetVerticesColor(Vec4f(1, 1, 1, 1));
      Exit(Particles[i]);
    end;

  p := TglrParticle2D.Create();
  p.Reset();
  if fTextureRegion <> nil then
    p.SetTextureRegion(fTextureRegion);
  Particles.Add(p);
  Exit(p);
end;

procedure TglrCustomParticleEmitter2D.Update(const dt: Double);
var
  i: Integer;
begin
  if not Visible then
    Exit();

  Time += dt;

  for i := 0 to Particles.Count - 1 do
    if Particles[i].Visible then
      with Particles[i] do
      begin
        T := T + dt;
        if T >= LifeTime then
        begin
          Visible := False;
          fActiveParticles -= 1;
        end
        else
          Position += Vec3f(Velocity * dt, 0);
      end;

  if Assigned(OnUpdate) then
    OnUpdate(dt);
end;

procedure TglrCustomParticleEmitter2D.RenderSelf;
var
  i: Integer;
begin
  fMaterial.Bind();
  fBatch.Start();
  for i := 0 to Particles.Count - 1 do
    fBatch.Draw(Particles[i]);
  fBatch.Finish();
end;

{ TglrParticleEmitter2D }

function TglrParticleEmitter2D.GetFreeParticleIndex: Integer;
var
  i: Integer;
  p: TglrParticle2D;
begin
  for i := 0 to fParticles.Count - 1 do
    if not fParticles[i].Visible then
      Exit(i);

  p := TglrParticle2D.Create();
  if fTextureRegion <> nil then
    p.SetTextureRegion(fTextureRegion);
  p.Velocity.Reset();
  p.T := 0.0;
  p.Visible := False;
  p.LifeTime := 0;
  Exit(fParticles.Add(p));
end;

constructor TglrParticleEmitter2D.Create(aBatch: TglrSpriteBatch;
  aMaterial: TglrMaterial; aTextureRegion: PglrTextureRegion);
var
  s: TglrParticle2D;
  i: Integer;
begin
  inherited Create();
  fBatch := aBatch;
  fMaterial := aMaterial;
  fTextureRegion := aTextureRegion;
  fParticles := TglrParticles2D.Create(128);
  for i := 0 to 127 do
  begin
    s := TglrParticle2D.Create();
    if fTextureRegion <> nil then
      s.SetTextureRegion(fTextureRegion);
    s.Velocity.Reset();
    s.T := 0.0;
    s.Visible := False;
    s.LifeTime := 0;
    fParticles.Add(s);
  end;

  Duration := 1.0;
  Time := 0;

  OriginBoxMinMax         := TglrVec4fDic.Create (100);
  VelocityMinMax          := TglrVec4fDic.Create (100);
  VelocityDispersionAngle := TglrSingleDic.Create(100);
  VelocityAngularMinMax   := TglrVec2fDic.Create(100);
  Color                   := TglrVec4fDic.Create (100);
  ParticlesPerSecond      := TglrIntDic.Create   (100);
  LifetimeMinMax          := TglrVec2fDic.Create (100);
  ParticleSizeMinMax      := TglrVec4fDic.Create (100);
end;

constructor TglrParticleEmitter2D.Create(aBatch: TglrSpriteBatch;
  aMaterial: TglrMaterial; const aStream: TglrStream;
  aTextureRegion: PglrTextureRegion; aFreeStreamOnFinish: Boolean);
begin
  Create(aBatch, aMaterial, aTextureRegion);
end;

destructor TglrParticleEmitter2D.Destroy;
begin
  fParticles.Free(True);
  OriginBoxMinMax.Free();
  VelocityMinMax.Free();
  VelocityDispersionAngle.Free();
  VelocityAngularMinMax.Free();
  Color.Free();
  ParticlesPerSecond.Free();
  LifetimeMinMax.Free();
  ParticleSizeMinMax.Free();
  inherited Destroy;
end;

function TglrParticleEmitter2D.SaveToStream: TglrStream;
begin
  Log.Write(lCritical, 'ParticleEmitter2D: SaveToStream is not implemented');
end;

procedure TglrParticleEmitter2D.Update(const dt: Double);
var
  i: Integer;
begin
  Time += dt;

  for i := 0 to fParticles.Count - 1 do
    if fParticles[i].Visible then
      with fParticles[i] do
      begin
        Position += Vec3f(Velocity * dt, 0);
        T := T + dt;

        if T >= LifeTime then
          Visible := False
        else
        begin
          //todo: Update other stuff

        end;
      end;

  //todo: add new particles

end;

end.

