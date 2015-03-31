unit glr_render2d;

{$i defines.inc}

interface

uses
  glr_render, glr_scene, glr_math, glr_utils;

type

  { TglrSprite }

  TglrSprite = class (TglrNode)
  protected
    fRot, fWidth, fHeight: Single;
    fPP: TglrVec2f;
    fTextureRegion: PglrTextureRegion;

    procedure SetRot(const aRot: Single); virtual;
    procedure SetWidth(const aWidth: Single); virtual;
    procedure SetHeight(const aHeight: Single); virtual;
    procedure SetPP(const aPP: TglrVec2f); virtual;
  public
    Vertices: array[0..3] of TglrVertexP3T2C4;

    constructor Create(); override; overload;
    constructor Create(aWidth, aHeight: Single; aPivotPoint: TglrVec2f); virtual; overload;
    destructor Destroy(); override;

    property Rotation: Single read fRot write SetRot;
    property Width: Single read fWidth write SetWidth;
    property Height: Single read fHeight write SetHeight;
    property PivotPoint: TglrVec2f read fPP write SetPP;

    procedure SetDefaultVertices(); virtual;//Sets vertices due to width, height, pivot point and rotation
    procedure SetDefaultTexCoords(); //Sets default texture coords
    procedure SetVerticesColor(aColor: TglrVec4f);
    procedure SetVerticesAlpha(aAlpha: Single);
    procedure SetSize(aWidth, aHeight: Single); overload;
    procedure SetSize(aSize: TglrVec2f); overload;

    procedure SetTextureRegion(aRegion: PglrTextureRegion; aAdjustSpriteSize: Boolean = True); virtual;
    function GetTextureRegion(): PglrTextureRegion;
    procedure RenderSelf(); override;
  end;

  TglrSpriteList = TglrObjectList<TglrSprite>;

  { TglrText }

  TglrTextHorAlign = (haLeft, haCenter, haRight);

  TglrText = class (TglrNode)
  protected
    fHorAlign: TglrTextHorAlign;
    fTextWidth: Single;
    fTextWidthChanged: Boolean;
    procedure SetHorAlign(aValue: TglrTextHorAlign);
    procedure SetTextWidth(aValue: Single);
  public
    Text: WideString;
    LetterSpacing, LineSpacing: Single;
    Color: TglrVec4f;
    Scale: Single;
    PivotPoint: TglrVec2f;
    constructor Create(const aText: WideString = ''); virtual; reintroduce;
    destructor Destroy(); override;

    property TextWidth: Single read fTextWidth write SetTextWidth;
    property HorAlign: TglrTextHorAlign read fHorAlign write SetHorAlign;

    procedure RenderSelf(); override;
  end;

  { TglrFont }

  TglrFont = class
  protected
    type
      TglrCharData = record
        ID: WideChar;
        py: Word;
        w, h: Word;
        tx, ty, tw, th: Single;
      end;
      PglrCharData = ^TglrCharData;

    var
      Material: TglrMaterial;
      Texture: TglrTexture;
      Table: array [WideChar] of PglrCharData;
      CharData: array of TglrCharData;

    function GetCharQuad(aChar: WideChar; aScale: Single): TglrQuadP3T2C4;
  public
    MaxCharHeight: Word;

    constructor Create(); virtual; overload;
    constructor Create(aStream: TglrStream;
      aFreeStreamOnFinish: Boolean = True); virtual; overload;
    destructor Destroy(); override;
  end;

  { TglrSpriteBatch }

  TglrSpriteBatch = class
  protected
    fVData: array[0..65535] of TglrVertexP3T2C4;
    fIData: array[0..65535] of Word;
    fCount: Word;
    fVB: TglrVertexBuffer;
    fIB: TglrIndexBuffer;
  public
    constructor Create(); virtual;
    destructor Destroy(); override;

    procedure Start();
    procedure Draw(aSprite: TglrSprite); overload;
    procedure Draw(aSprites: array of TglrSprite); overload;
    procedure Draw(aSprites: TglrSpriteList); overload;
    procedure Finish();
  end;

  { TglrFontBatch }

  TglrFontBatch = class
  protected
    fVData: array[0..65535] of TglrVertexP3T2C4;
    fIData: array[0..65535] of Word;
    fCount: Word;
    fVB: TglrVertexBuffer;
    fIB: TglrIndexBuffer;
    fFont: TglrFont;
    function GetTextOrigin(aText: TglrText): TglrVec2f;
  public
    constructor Create(aFont: TglrFont); virtual;
    destructor Destroy(); override;

    procedure Start();
    procedure Draw(aText: TglrText);
    procedure Finish();
  end;

implementation

uses
  glr_core, glr_filesystem, glr_resload;

const
  SpriteIndices: array[0..5] of Word = (0, 1, 2, 2, 3, 0);

{ TglrSprite }

procedure TglrSprite.SetRot(const aRot: Single);
begin
  if (not Equalf(aRot, fRot)) then
  begin
    Matrix.Identity();
    Matrix.Rotate(aRot * deg2rad, Vec3f(0, 0, 1));
    fRot := aRot;
    if fRot > 360 then
      fRot -= 360
    else if fRot < -360 then
      fRot += 360;
  end;
end;

procedure TglrSprite.SetWidth(const aWidth: Single);
begin
  if (not Equalf(aWidth, fWidth)) then
  begin
    fWidth := aWidth;
    SetDefaultVertices();
  end;
end;

procedure TglrSprite.SetHeight(const aHeight: Single);
begin
  if (not Equalf(aHeight, fHeight)) then
  begin
    fHeight := aHeight;
    SetDefaultVertices();
  end;
end;

procedure TglrSprite.SetPP(const aPP: TglrVec2f);
begin
  if (aPP <> fPP) then
  begin
    fPP := aPP;
    SetDefaultVertices();
  end;
end;

constructor TglrSprite.Create;
begin
  Create(1, 1, Vec2f(0.5, 0.5));
end;

constructor TglrSprite.Create(aWidth, aHeight: Single; aPivotPoint: TglrVec2f);
begin
  inherited Create();
  fWidth := aWidth;
  fHeight := aHeight;
  fPP := aPivotPoint;
  SetDefaultVertices();
  SetDefaultTexCoords();
  SetVerticesColor(Vec4f(1, 1, 1, 1));
  fTextureRegion := nil;
end;

destructor TglrSprite.Destroy;
begin
  inherited Destroy;
end;

procedure TglrSprite.SetDefaultVertices;
begin
  Vertices[0].vec := Vec3f((Vec2f(1, 1) - fPP) * Vec2f(fWidth, fHeight), 0);
  Vertices[1].vec := Vec3f((Vec2f(1, 0) - fPP) * Vec2f(fWidth, fHeight), 0);
  Vertices[2].vec := Vec3f((fPP.NegateVector)    * Vec2f(fWidth, fHeight), 0);
  Vertices[3].vec := Vec3f((Vec2f(0, 1) - fPP) * Vec2f(fWidth, fHeight), 0);
end;

procedure TglrSprite.SetDefaultTexCoords;
begin
  Vertices[0].tex := Vec2f(1, 1);
  Vertices[1].tex := Vec2f(1, 0);
  Vertices[2].tex := Vec2f(0, 0);
  Vertices[3].tex := Vec2f(0, 1);
end;

procedure TglrSprite.SetVerticesColor(aColor: TglrVec4f);
var
  i: Integer;
begin
  for i := 0 to 3 do
    Vertices[i].col := aColor;
end;

procedure TglrSprite.SetVerticesAlpha(aAlpha: Single);
var
  i: Integer;
begin
  for i := 0 to 3 do
    Vertices[i].col.w := aAlpha;
end;

procedure TglrSprite.SetSize(aWidth, aHeight: Single);
begin
  fWidth := aWidth;
  fHeight := aHeight;
  SetDefaultVertices();
end;

procedure TglrSprite.SetSize(aSize: TglrVec2f);
begin
  SetSize(aSize.x, aSize.y);
end;

procedure TglrSprite.SetTextureRegion(aRegion: PglrTextureRegion;
  aAdjustSpriteSize: Boolean);
begin
  with aRegion^ do
    if not Rotated then
    begin
      Vertices[0].tex := Vec2f(tx + tw, ty + th);
      Vertices[1].tex := Vec2f(tx + tw, ty);
      Vertices[2].tex := Vec2f(tx, ty);
      Vertices[3].tex := Vec2f(tx, ty + th);
      if aAdjustSpriteSize then
      begin
        Width := tw * Texture.Width;
        Height := th * Texture.Height;
      end;
    end
    else
    begin
      Vertices[0].tex := Vec2f(tx, ty + th);
      Vertices[1].tex := Vec2f(tx + tw, ty + th);
      Vertices[2].tex := Vec2f(tx + tw, ty);
      Vertices[3].tex := Vec2f(tx, ty);
      if aAdjustSpriteSize then
      begin
        Width := th * Texture.Height;
        Height := tw * Texture.Width;
      end;
    end;
  fTextureRegion := aRegion;
end;

function TglrSprite.GetTextureRegion: PglrTextureRegion;
begin
  Result := fTextureRegion;
end;

procedure TglrSprite.RenderSelf;
begin

end;

{ TglrText }

procedure TglrText.SetHorAlign(aValue: TglrTextHorAlign);
begin
  if fHorAlign = aValue then
    Exit();
  fHorAlign := aValue;
end;

procedure TglrText.SetTextWidth(aValue: Single);
begin
  if fTextWidth = aValue then
    Exit();
  fTextWidth := aValue;
  fTextWidthChanged := True;
  //Log.Write(lCritical, 'Text.SetTextWidth is not implemented');
end;

constructor TglrText.Create(const aText: WideString);
begin
  inherited Create();
  Text := aText;
  LineSpacing := 2.0;
  LetterSpacing := 1.0;
  Color := Vec4f(1, 1, 1, 1);
  Scale := 1.0;

  PivotPoint.Reset();
  fHorAlign := haLeft;
  fTextWidth := -1;
  fTextWidthChanged := False;
end;

destructor TglrText.Destroy;
begin
  inherited Destroy;
end;

procedure TglrText.RenderSelf;
begin

end;

{ TglrFont }

function TglrFont.GetCharQuad(aChar: WideChar; aScale: Single): TglrQuadP3T2C4;
begin
  FillChar(Result[0], SizeOf(TglrVertexP3T2C4) * 4, 0);
  if Table[aChar] = nil then
    Exit();
  with Table[aChar]^ do
  begin
    Result[0].vec := Vec3f(w, py + h, 0) * aScale;
    Result[1].vec := Vec3f(w, py, 0) * aScale;
    Result[2].vec := Vec3f(0, py, 0) * aScale;
    Result[3].vec := Vec3f(0, py + h, 0) * aScale;

    Result[0].tex := Vec2f(tx + tw, ty + th);
    Result[1].tex := Vec2f(tx + tw, ty);
    Result[2].tex := Vec2f(tx, ty);
    Result[3].tex := Vec2f(tx, ty + th);
  end;
end;

constructor TglrFont.Create;
begin
  inherited Create();
  if not Default.fInited then
    Log.Write(lCritical, 'Font: Can not create default font - default assets are disabled');
  Create(FileSystem.ReadResource('default assets/default.fnt'));
end;

constructor TglrFont.Create(aStream: TglrStream; aFreeStreamOnFinish: Boolean);
var
  data: Pointer;
  charCount, i: LongWord;
begin
  inherited Create();
  if Default.fInited then
    Material := TglrMaterial.Create(Default.SpriteShader)
  else
    Material := TglrMaterial.Create(TglrShaderProgram(nil));

  Texture := TglrTexture.Create(aStream, extBmp, False);
  Material.AddTexture(Texture, 'uDiffuse');

  data := LoadFontData(aStream, charCount);
  SetLength(Self.CharData, charCount);
  Move(data^, CharData[0], charCount * SizeOf(TglrCharData));
  FreeMem(data);
  MaxCharHeight := 0;
  for i := 0 to charCount - 1 do
  begin
    Table[CharData[i].ID] := @CharData[i];
    if CharData[i].h > MaxCharHeight then
      MaxCharHeight := CharData[i].h;
  end;

  if aFreeStreamOnFinish then
    aStream.Free();
end;

destructor TglrFont.Destroy;
begin
  Material.Free();
  Texture.Free();
  SetLength(CharData, 0);
  inherited Destroy;
end;

{ TglrSpriteBatch }

constructor TglrSpriteBatch.Create;
begin
  inherited Create;
  fVB := TglrVertexBuffer.Create(nil, 65536, vfPos3Tex2Col4, uStreamDraw);
  fIB := TglrIndexBuffer.Create(nil, 65536, ifShort);
end;

destructor TglrSpriteBatch.Destroy;
begin
  fVB.Free();
  fIB.Free();
  inherited Destroy;
end;

procedure TglrSpriteBatch.Start;
begin
  fCount := 0;
end;

procedure TglrSpriteBatch.Draw(aSprite: TglrSprite);
var
  i: Integer;
begin
  if (aSprite.Visible) then
  begin
    for i := 0 to 3 do
    begin
      fVData[fCount * 4 + i] := aSprite.Vertices[i];
      fVData[fCount * 4 + i].vec := aSprite.AbsoluteMatrix * fVData[fCount * 4 + i].vec;
    end;

    for i := 0 to 5 do
      fIData[fCount * 6 + i] := SpriteIndices[i] + fCount * 4;
    fCount += 1;
  end;
end;

procedure TglrSpriteBatch.Draw(aSprites: array of TglrSprite);
var
  i: Integer;
begin
  for i := 0 to Length(aSprites) - 1 do
    Draw(aSprites[i]);
end;

procedure TglrSpriteBatch.Draw(aSprites: TglrSpriteList);
var
  i: Integer;
begin
  for i := 0 to aSprites.Count - 1 do
    Draw(aSprites[i]);
end;

procedure TglrSpriteBatch.Finish;
begin
  if fCount = 0 then
    Exit();
  fVB.Update(@fVData[0], 0, fCount * 4);
  fIB.Update(@fIData[0], 0, fCount * 6);
  Render.Params.ModelViewProj := Render.Params.ViewProj;
  Render.DrawTriangles(fVB, fIB, 0, fCount * 6);
  // Nvidia threading optimization (TO) fails when you use
  // SpriteBatch several times per one frame, like
  // Start; Draw(); ... Finish();
  // Start; Draw(); ... Finish();
  // Nvidia with TO enabled will draw only last batch.
  // This is shit.

  // Note: suddenly removing of BindBuffer(0) after update helped...
  // I will save it here for next generations...
end;

{ TglrFontBatch }

function TglrFontBatch.GetTextOrigin(aText: TglrText): TglrVec2f;
var
  i: Integer;
  maxWidth: Single;
  textSize: TglrVec2f;
begin
  maxWidth := 0;
  textSize.Reset();
  for i := 1 to Length(aText.Text) do
    if fFont.Table[aText.Text[i]] <> nil then
      with fFont.Table[aText.Text[i]]^ do
      begin
        // Setup text height on first visible character
        if (textSize.y < 1) and (h > 0) then
          textSize.y := fFont.MaxCharHeight + aText.LineSpacing;

        if ID = #10 then
        begin
          textSize.y += fFont.MaxCharHeight + aText.LineSpacing;
          if maxWidth > textSize.x then
            textSize.x := maxWidth;
          maxWidth := 0;
          continue;
        end;
        maxWidth += w + aText.LetterSpacing;
      end;
  textSize.x := Max(textSize.x, maxWidth);
  textSize := textSize * aText.Scale;

  Result.Reset();

  Result := (-1 * textSize) * aText.PivotPoint;
end;

constructor TglrFontBatch.Create(aFont: TglrFont);
begin
  inherited Create;
  if (aFont = nil) then
    Log.Write(lCritical, 'FontBatch: Null pointer provided, Font object expected');
  fFont := aFont;
  fVB := TglrVertexBuffer.Create(nil, 65536, vfPos3Tex2Col4, uStreamDraw);
  fIB := TglrIndexBuffer.Create(nil, 65536, ifShort);
end;

destructor TglrFontBatch.Destroy;
begin
  fVB.Free();
  fIB.Free();
  inherited Destroy;
end;

procedure TglrFontBatch.Start;
begin
  fCount := 0;
end;

procedure TglrFontBatch.Draw(aText: TglrText);
var
  origin, start: TglrVec2f;
  quad: TglrQuadP3T2C4;
  j, k: Integer;

  lastSpace: Integer;
  width: Single;
begin
  if (not aText.Visible) or (aText.Text = '') then
    Exit();

  origin := GetTextOrigin(aText);
  start := origin;

  // Make a word wrap
  if (aText.TextWidth > 0) and (aText.fTextWidthChanged) then
  begin
    lastSpace := 0;
    width := 0;
    j := 1;
    while (j <= Length(aText.Text)) do
    begin
      if fFont.Table[aText.Text[j]] = nil then
        continue;
      if (aText.Text[j] = #10) then
        aText.Text[j] := ' ';

      if (aText.Text[j] = ' ') then
        lastSpace := j;

      width += fFont.Table[aText.Text[j]].w * aText.Scale + aText.LetterSpacing;
      if (width > aText.TextWidth) and (lastSpace > 0) then
      begin
        aText.Text[lastSpace] := #10;
        j := lastSpace + 1;
        width := 0;
        lastSpace := 0;
      end
      else
        j += 1;
    end;
    aText.fTextWidthChanged := False;
  end;

  for j := 1 to Length(aText.Text) do
  begin
    if (aText.Text[j] = #10) then
    begin
      start.x := origin.x;
      start.y += (aText.LineSpacing + fFont.MaxCharHeight) * aText.Scale;
      continue;
    end;

    if fFont.Table[aText.Text[j]] = nil then
      continue;

    quad := fFont.GetCharQuad(aText.Text[j], aText.Scale);
    //Do not need it anymore - included in AbsoluteMatrix computing
    //child.Matrix.Pos := child.Position;
    for k := 0 to 3 do
    begin
      fVData[fCount * 4 + k] := quad[k];
      fVData[fCount * 4 + k].vec += Vec3f(start, 0);
      fVData[fCount * 4 + k].vec := aText.AbsoluteMatrix * fVData[fCount * 4 + k].vec;
      fVData[fCount * 4 + k].col := aText.Color;
    end;

    for k := 0 to 5 do
      fIData[fCount * 6 + k] := SpriteIndices[k] + fCount * 4;

    start.x += quad[0].vec.x + aText.LetterSpacing;
    fCount += 1;
  end;
end;

procedure TglrFontBatch.Finish;
begin
  if fCount = 0 then
    Exit();
  fVB.Update(@fVData[0], 0, fCount * 4);
  fIB.Update(@fIData[0], 0, fCount * 6);

  Render.Params.ModelViewProj := Render.Params.ViewProj;
  fFont.Material.Bind();
  Render.DrawTriangles(fVB, fIB, 0, 6 * fCount);
end;

end.

