unit glr_gui;

{$i defines.inc}

interface

uses
  glr_core, glr_render, glr_render2d, glr_scene, glr_math, glr_utils;

type
  TglrGuiElement = class;

  TglrGuiBooleanCallback = procedure (Sender: TglrGuiElement; aValue: Boolean) of object;

  TglrGuiInputCallback = procedure(Sender: TglrGuiElement; Event: PglrInputEvent) of object;

  { TglrGuiElement }

  TglrGuiElement = class (TglrSprite)
  protected
    fNormalTextureRegion,
    fOverTextureRegion,
    fClickedTextureRegion,
    fDisabledTextureRegion: PglrTextureRegion;

    fIsMouseOver: Boolean;
    fEnabled, fFocused: Boolean;

    procedure SetRot(const aRot: Single); override;
    procedure SetWidth(const aWidth: Single); override;
    procedure SetHeight(const aHeight: Single); override;
    procedure SetPP(const aPP: TglrVec2f); override;
    procedure SetNormalTextureRegion(const aTextureRegion: PglrTextureRegion);

    procedure SetEnabled(const aValue: Boolean);
    procedure SetFocused(const aValue: Boolean);
    procedure ProcessInput(Event: PglrInputEvent);
    function IsHit(X, Y: Single): Boolean;
  public
    // Input events
    OnClick, OnTouchDown, OnTouchUp, OnTouchMove, OnMouseOver, OnMouseOut: TglrGuiInputCallback;

    // Other events
    OnEnable, OnFocus: TglrGuiBooleanCallback;

    ZIndex: Integer;

    HitBox: TglrBB;

    // Texture regions for various states of button
    property NormalTextureRegion: PglrTextureRegion read fNormalTextureRegion write SetNormalTextureRegion;
    property OverTextureRegion: PglrTextureRegion read fOverTextureRegion write fOverTextureRegion;
    property ClickedTextureRegion: PglrTextureRegion read fClickedTextureRegion write fClickedTextureRegion;
    property DisabledTextureRegion: PglrTextureRegion read fDisabledTextureRegion write fDisabledTextureRegion;

    property Enabled: Boolean read fEnabled write SetEnabled;
    property Focused: Boolean read fFocused write SetFocused;

    procedure UpdateHitBox();
    procedure SetDefaultVertices(); override;

    constructor Create(aWidth, aHeight: Single; aPivotPoint: TglrVec2f); override; overload;
  end;

  TglrGuiElementsList = TglrObjectList<TglrGuiElement>;

  { TglrGuiLayout }

  { 2014-12-24 : Draft version, do not use }
  TglrGuiLayout = class (TglrGuiElement)
  protected
    fX1, fX2, fY1, fY2: Single;
    fElements: TglrGuiElementsList;
    procedure SetWidth(const aWidth: Single); override;
    procedure SetHeight(const aHeight: Single); override;
    procedure SetVisible(const aVisible: Boolean); override;

    procedure UpdatePatchesPosition();
  public
    Patches: array[0..7] of TglrSprite; //9th patch is element itself (center one)
    constructor Create(aWidth, aHeight: Single; aPivotPoint: TglrVec2f); override; overload;
    destructor Destroy(); override;

    procedure SetNinePatchBorders(x1, x2, y1, y2: Single);

    procedure AddElement(aElement: TglrGuiElement);
    procedure RemoveElement(aElement: TglrGuiElement);

    procedure SetTextureRegion(aRegion: PglrTextureRegion; aAdjustSpriteSize: Boolean =
      True); override;
  end;

  { TglrGuiButton }

  TglrGuiButton = class (TglrGuiElement)
  protected
    procedure SetVisible(const aVisible: Boolean); override;
  public
    TextLabel: TglrText;
    constructor Create(aWidth, aHeight: Single; aPivotPoint: TglrVec2f); override; overload;
    destructor Destroy(); override;
  end;

  { TglrGuiManager }

  TglrGuiManager = class (TglrGuiElementsList)
  protected
    fFontBatch: TglrFontBatch;
    fSpriteBatch: TglrSpriteBatch;
    fMaterial: TglrMaterial;
    procedure SetFocused(aElement: TglrGuiElement);
  public
    Focused: TglrGuiElement;

    constructor Create(Material: TglrMaterial; Font: TglrFont; aCapacity: LongInt = 4); reintroduce;
    destructor Destroy(); override;

    procedure ProcessInput(Event: PglrInputEvent; GuiCamera: TglrCamera);

    procedure Update(const dt: Double);

    procedure Render();
  end;

implementation

{ TglrGuiLayout }

procedure TglrGuiLayout.SetWidth(const aWidth: Single);
begin
  inherited SetWidth(aWidth);
  Patches[1].Width := Width;
  Patches[6].Width := Width;
  UpdatePatchesPosition();
end;

procedure TglrGuiLayout.SetHeight(const aHeight: Single);
begin
  inherited SetHeight(aHeight);
  Patches[3].Height := Height;
  Patches[4].Height := Height;
  UpdatePatchesPosition();
end;

procedure TglrGuiLayout.SetVisible(const aVisible: Boolean);
var
  i: Integer;
begin
  inherited SetVisible(aVisible);
  for i := 0 to Length(Patches) - 1 do
    Patches[i].Visible := aVisible;

  for i := 0 to fElements.Count - 1 do
    fElements[i].Visible := aVisible;
end;

procedure TglrGuiLayout.UpdatePatchesPosition;
var
  pp: TglrVec2f;
begin
  pp := Vec2f(0.5, 0.5);
  Patches[0].Position := Vec3f(-Width * pp.x, -Height * pp.y, 1);
  Patches[1].Position := Vec3f( 0,            -Height * pp.y, 1);
  Patches[2].Position := Vec3f( Width * pp.x, -Height * pp.y, 1);

  Patches[3].Position := Vec3f(-Width * pp.x,              0, 1);
  Patches[4].Position := Vec3f( Width * pp.x,              0, 1);

  Patches[5].Position := Vec3f(-Width * pp.x,  Height * pp.y, 1);
  Patches[6].Position := Vec3f( 0,             Height * pp.y, 1);
  Patches[7].Position := Vec3f( Width * pp.x,  Height * pp.y, 1);
end;

constructor TglrGuiLayout.Create(aWidth, aHeight: Single; aPivotPoint: TglrVec2f);
var
  i: Integer;
begin
  inherited Create(aWidth, aHeight, aPivotPoint * 3 - vec2f(1, 1));
  for i := 0 to Length(Patches) - 1 do
  begin
    Patches[i] := TglrSprite.Create(Width, Height, aPivotPoint{ * 3 - vec2f(1, 1)});
    Patches[i].Visible := False;
    Patches[i].Parent := Self;
  end;

  UpdatePatchesPosition();

  fElements := TglrGuiElementsList.Create();
end;

destructor TglrGuiLayout.Destroy;
var
  i: Integer;
begin
  for i := 0 to Length(Patches) - 1 do
    Patches[i].Free();

  fElements.Free(False);
  inherited Destroy;
end;

procedure TglrGuiLayout.SetNinePatchBorders(x1, x2, y1, y2: Single);
var
  i: Integer;
begin
  fX1 := x1;
  fX2 := x2;
  fY1 := y1;
  fY2 := y2;
  for i := 0 to Length(Patches) - 1 do
    Patches[i].Visible := True;
  if (NormalTextureRegion) <> nil then
    SetTextureRegion(NormalTextureRegion, False);
end;

procedure TglrGuiLayout.AddElement(aElement: TglrGuiElement);
begin
  fElements.Add(aElement);
  aElement.Parent := Self;
end;

procedure TglrGuiLayout.RemoveElement(aElement: TglrGuiElement);
begin
  fElements.Delete(aElement);
  aElement.Parent := nil;
end;

procedure TglrGuiLayout.SetTextureRegion(aRegion: PglrTextureRegion;
  aAdjustSpriteSize: Boolean);

  function ChangeTextureRegion(aFrom: PglrTextureRegion; x, y, w, h: Single): PglrTextureRegion;
  begin
    Result := aFrom;
    with Result^ do
    begin
      tx := x;
      ty := y;
      tw := w;
      th := h;
    end;
  end;

var
  cx1, cx2, cy1, cy2: Single;
  patchRegion: TglrTextureRegion;

begin
  if (fX2 <= 0) or (fY2 <= 0) then
    inherited SetTextureRegion(aRegion, aAdjustSpriteSize)
  else
  begin
    patchRegion := aRegion^;
    cx1 := aRegion.tw * fX1;
    cx2 := aRegion.tw * fX2;
    cy1 := aRegion.th * fY1;
    cy2 := aRegion.th * fY2;

    with aRegion^ do
    begin
      Patches[0].SetTextureRegion(ChangeTextureRegion(@patchRegion, tx,       ty,       cx1,       cy1), aAdjustSpriteSize);
      Patches[1].SetTextureRegion(ChangeTextureRegion(@patchRegion, tx + cx1, ty,       cx2 - cx1, cy1), aAdjustSpriteSize);
      Patches[2].SetTextureRegion(ChangeTextureRegion(@patchRegion, tx + cx2, ty,       tw - cx2,  cy1), aAdjustSpriteSize);
      Patches[3].SetTextureRegion(ChangeTextureRegion(@patchRegion, tx,       ty + cy1, cx1,       cy2 - cy1), aAdjustSpriteSize);
      inherited SetTextureRegion(ChangeTextureRegion( @patchRegion, tx + cx1, ty + cy1, cx2 - cx1, cy2 - cy1), aAdjustSpriteSize);
      Patches[4].SetTextureRegion(ChangeTextureRegion(@patchRegion, tx + cx2, ty + cy1, tw - cx2,  cy2 - cy1), aAdjustSpriteSize);
      Patches[5].SetTextureRegion(ChangeTextureRegion(@patchRegion, tx,       ty + cy2, cx1,       th - cy2), aAdjustSpriteSize);
      Patches[6].SetTextureRegion(ChangeTextureRegion(@patchRegion, tx + cx1, ty + cy2, cx2 - cx1, th - cy2), aAdjustSpriteSize);
      Patches[7].SetTextureRegion(ChangeTextureRegion(@patchRegion, tx + cx2, ty + cy2, tw - cx2,  th - cy2), aAdjustSpriteSize);
    end;
  end;
end;

{ TglrGuiButton }

procedure TglrGuiButton.SetVisible(const aVisible: Boolean);
begin
  inherited SetVisible(aVisible);
  TextLabel.Visible := aVisible;
end;

constructor TglrGuiButton.Create(aWidth, aHeight: Single; aPivotPoint: TglrVec2f);
begin
  inherited Create(aWidth, aHeight, aPivotPoint);
  TextLabel := TglrText.Create();
  TextLabel.Parent := Self;
end;

destructor TglrGuiButton.Destroy;
begin
  TextLabel.Free();
  inherited Destroy;
end;

{ TglrGuiManager }

procedure TglrGuiManager.SetFocused(aElement: TglrGuiElement);
begin
  if (Focused <> nil) then
    Focused.Focused := False;
  if (aElement <> nil) then
    aElement.Focused := True;

  Focused := aElement;
end;

constructor TglrGuiManager.Create(Material: TglrMaterial; Font: TglrFont;
  aCapacity: LongInt);
begin
  inherited Create(aCapacity);
  Focused := nil;
  fMaterial := Material;
  fSpriteBatch := TglrSpriteBatch.Create();
  fFontBatch := TglrFontBatch.Create(Font);
end;

destructor TglrGuiManager.Destroy;
begin
  fSpriteBatch.Free();
  fFontBatch.Free();
  inherited Destroy;
end;

procedure TglrGuiManager.ProcessInput(Event: PglrInputEvent;
  GuiCamera: TglrCamera);
var
  i: Integer;
  touchVec: TglrVec3f;
begin
  // WIP, don't kill me
  touchVec := GuiCamera.AbsoluteMatrix * Vec3f(Event.X, Event.Y, 0);

  for i := 0 to FCount - 1 do
    if FItems[i].Enabled then
      // Send ProcessInput for keys and wheel to focused only elements
      // Other messages - to all elements
      if (not (Event.InputType in [itKeyDown, itKeyUp, itWheel])) or (FItems[i].Focused) then
        FItems[i].ProcessInput(Event);
end;

procedure TglrGuiManager.Update(const dt: Double);
begin

end;

procedure TglrGuiManager.Render;
var
  i, j: Integer;
  b: TglrGuiButton;
begin
  // Render sprites
  fMaterial.Bind();
  fSpriteBatch.Start();

  for i := 0 to FCount - 1 do
    // If it is GuiLayout it has 9 sprites to draw (9-patch)
    if (FItems[i] is TglrGuiLayout) then
    begin
      fSpriteBatch.Draw(FItems[i]);
      for j := 0 to 7 do
        fSpriteBatch.Draw(TglrGuiLayout(FItems[i]).Patches[j]);
    end
    // In any other cases - just draw element itself
    else
      fSpriteBatch.Draw(FItems[i]);

  fSpriteBatch.Finish();

  // Render any text in components
  fFontBatch.Start();
  for i := 0 to FCount - 1 do
    // GuiButton has Text object
    if (FItems[i] is TglrGuiButton) then
    begin
      b := TglrGuiButton(FItems[i]);
      b.TextLabel.Position.z := b.Position.z + 1;
      fFontBatch.Draw(b.TextLabel);
    end;
  fFontBatch.Finish();
end;

{ TglrGuiElement }

procedure TglrGuiElement.SetRot(const aRot: Single);
begin
  inherited SetRot(aRot);
  UpdateHitBox();
end;

procedure TglrGuiElement.SetWidth(const aWidth: Single);
begin
  inherited SetWidth(aWidth);
  UpdateHitBox();
end;

procedure TglrGuiElement.SetHeight(const aHeight: Single);
begin
  inherited SetHeight(aHeight);
  UpdateHitBox();
end;

procedure TglrGuiElement.SetPP(const aPP: TglrVec2f);
begin
  inherited SetPP(aPP);
  UpdateHitBox();
end;

procedure TglrGuiElement.SetNormalTextureRegion(
  const aTextureRegion: PglrTextureRegion);
begin
  fNormalTextureRegion := aTextureRegion;
  if (aTextureRegion <> nil) then
    SetTextureRegion(aTextureRegion);
end;

procedure TglrGuiElement.SetEnabled(const aValue: Boolean);
begin
  if fEnabled = aValue then
    Exit();
  fEnabled := aValue;
  if Assigned(OnEnable) then
    OnEnable(Self, aValue);
  if fEnabled then
  begin
    if Assigned(NormalTextureRegion) then
      SetTextureRegion(NormalTextureRegion);
  end
  else
    if Assigned(DisabledTextureRegion) then
      SetTextureRegion(DisabledTextureRegion);
end;

procedure TglrGuiElement.SetFocused(const aValue: Boolean);
begin
  if fFocused = aValue then
    Exit();
  fFocused := aValue;
  if Assigned(OnFocus) then
    OnFocus(Self, aValue);
end;

procedure TglrGuiElement.ProcessInput(Event: PglrInputEvent);
begin
  // Warning! Implemented partially!
  case Event.InputType of
    itTouchDown:
      if IsHit(Event.X, Event.Y) then
      begin
        if Assigned(OnTouchDown) then
          OnTouchDown(Self, Event);
        if Assigned(ClickedTextureRegion) then
          SetTextureRegion(ClickedTextureRegion);
        Focused := True;
      end
      else
        Focused := False;
    itTouchUp:
      if IsHit(Event.X, Event.Y) then
      begin
        if Assigned(OnTouchUp) then
          OnTouchUp(Self, Event);
        if Assigned(OverTextureRegion) then
          SetTextureRegion(OverTextureRegion);
        if Focused then
          if Assigned(OnClick) then
            OnClick(Self, Event);
      end
      else
        if Assigned(NormalTextureRegion) then
          SetTextureRegion(NormalTextureRegion);
    itTouchMove:
    begin
      if IsHit(Event.X, Event.Y) then
      begin
        if Assigned(OnTouchMove) then
          OnTouchMove(Self, Event);
        if not fIsMouseOver then
        begin
          if Assigned(OnMouseOver) then
            OnMouseOver(Self, Event);
          if Assigned(OverTextureRegion) then
            SetTextureRegion(OverTextureRegion);
        end;
        fIsMouseOver := True;
      end
      else
      begin
        if fIsMouseOver then
        begin
          if Assigned(OnMouseOut) then
            OnMouseOut(Self, Event);
          if Assigned(NormalTextureRegion) then
            SetTextureRegion(NormalTextureRegion);
        end;
        fIsMouseOver := False;
      end;
    end;
  end;
end;

function TglrGuiElement.IsHit(X, Y: Single): Boolean;
var
  i: Integer;
  Point, p1, p2: TglrVec2f;
  absMatrix: TglrMat4f;
  intersect: Boolean;
begin
  // First check out bounding box
  x -= Position.x;
  y -= Position.y;
  with HitBox do
    Result := (x >= Left) and (x <= Right) and (y >= Top) and (y <= Bottom);
  if not Result then
    Exit();

  // If Point is in bounding box, then make a raycasts
  Result := False;
  absMatrix := AbsoluteMatrix;
  Point := Vec2f(X + Position.x, Y + Position.y);
  for i := 0 to 3 do
  begin
    p1 := Vec2f(absMatrix * Vertices[i].vec);
    p2 := Vec2f(absMatrix * Vertices[(i + 1) mod 4].vec);
    intersect := ((p1.y > Point.y) <> (p2.y > Point.y))
      and (Point.x < (p2.x - p1.x) * (Point.y - p1.y) / (p2.y - p1.y) + p1.x);
   if (intersect) then
     Result := not Result;
  end;
  Exit(Result);
end;

procedure TglrGuiElement.UpdateHitBox;
begin
  HitBox.Bottom := Max(Vertices[0].vec.y, Vertices[1].vec.y) + Position.y;
  HitBox.Top := Min(Vertices[2].vec.y, Vertices[3].vec.y) - Position.y;
  HitBox.Right := Max(Vertices[0].vec.x, Vertices[3].vec.x) + Position.x;
  HitBox.Left := Min(Vertices[1].vec.x, Vertices[2].vec.x) - Position.x;
end;

procedure TglrGuiElement.SetDefaultVertices;
begin
  inherited SetDefaultVertices;
  UpdateHitBox();
end;

constructor TglrGuiElement.Create(aWidth, aHeight: Single;
  aPivotPoint: TglrVec2f);
begin
  inherited Create(aWidth, aHeight, aPivotPoint);
  UpdateHitBox();
  fFocused := False;
  fEnabled := True;
  fIsMouseOver := False;

  ZIndex := 0;

  NormalTextureRegion := nil;
  OverTextureRegion := nil;
  ClickedTextureRegion := nil;
  DisabledTextureRegion := nil;
end;

end.

