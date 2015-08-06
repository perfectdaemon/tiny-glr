unit uAssets;

interface

uses
  glr_scene,
  glr_math,
  glr_render,
  glr_render2d;

type

  { Assets }

  Assets = class
  public
    // Base assets
    class var SpriteShader: TglrShaderProgram;
    class var BlankTexture: TglrTexture;

    class var GuiMaterial: TglrMaterial;
    class var GuiCamera: TglrCamera;
    class var GuiSpriteBatch: TglrSpriteBatch;

    class var FontMain: TglrFont;
    class var FontMainBatch: TglrFontBatch;

    class var PixelateShader: TglrShaderProgram;
    class var PixelateMaterial: TglrMaterial;
    class var PixelateTexture: TglrTexture;

    class var FrameBuffer: TglrFrameBuffer;

    class procedure LoadBase();
    class procedure UnloadBase();

    // Level specified assets
    // ...
  end;

  { Texts }

  Texts = class
  public
    class var
      MenuNewGame, MenuSettings, MenuExit, MenuTitle, MenuAuthorName,

      MenuApply, MenuBack,

      SettingsMusicVolume, SettingsSoundVolume

      : UnicodeString;


    class procedure LoadMenu();
  end;

  { Colors }

  Colors = class
  private
    class procedure LoadPalette();
  public
    class var
      // Palette
      Palette: array [0..7] of TglrVec4f;

      Red, White, Black, Gray,

      MenuButton, MenuButtonText, MenuButtonOver,
      MenuSlider, MenuSliderOver,

      MenuText,

      MenuBackground

      : TglrVec4f;

    class procedure Load();
  end;

  { Sizes }

  Sizes = class
    class var
      ButtonSize, ButtonSizeOver, ButtonTextOffset
      : TglrVec2f;

    class procedure Load();
  end;

const
  W_WIDTH = 512;
  W_HEIGHT = 512;

implementation

uses
  glr_filesystem, glr_core, glr_utils, glr_resload;

const
  R_BASE = 'pixel/';
  R_PALETTE = R_BASE + 'palette.txt';

  R_FONT_MAIN = R_BASE + 'HelveticaLight19b.fnt';

{ Sizes }

class procedure Sizes.Load;
begin
  ButtonSize := Vec2f(220, 50);
  ButtonSizeOver := Vec2f(230, 60);
  ButtonTextOffset := Vec2f(-70, -15);
end;

{ Colors }

class procedure Colors.LoadPalette;
var
  stream: TglrStream;
  row, rows: TglrStringList;
  i: Integer;
begin
  stream := FileSystem.ReadResource(R_PALETTE);
  rows := LoadStringList(stream);
  stream.Free();
  for i := 0 to rows.Count - 1 do
  begin
    row := StrSplit(rows[i], #9);
    Palette[i] := Color4ub(
      Convert.ToInt(row[0]),
      Convert.ToInt(row[1]),
      Convert.ToInt(row[2]));
    row.Free();
  end;
  rows.Free();
end;

class procedure Colors.Load;
begin
  // Load palette
  LoadPalette();


  Red := Color4ub(241, 0, 0);
  White := Color4ub(255, 255, 255);
  Black := Color4ub(0, 0, 0);
  Gray := Color4ub(60, 60, 60);

  MenuButton      := Palette[2];   //Color4ub(80, 160, 255);
  MenuButtonText  := Palette[1]; //Color4ub(230, 230, 230);
  MenuButtonOver  := White; //Color4ub(160, 250, 250);
  MenuSlider      := Palette[2];
  MenuSliderOver  := White;
  MenuText        := Palette[6];
  MenuBackground  := Palette[5]; //:= Color4ub(20, 50, 110);
end;

{ Texts }

class procedure Texts.LoadMenu();
begin
  MenuNewGame := UTF8Decode('Новая игра');
  MenuSettings := UTF8Decode('Настройки');
  MenuExit := UTF8Decode('Выход');
  MenuTitle := UTF8Decode('Arena Shooter для igdc #123');
  MenuAuthorName := UTF8Decode('perfect.daemon, 2015 (c)');

  MenuApply := UTF8Decode('Применить');
  MenuBack  := UTF8Decode('Назад');

  SettingsMusicVolume := UTF8Decode('Музыка');
  SettingsSoundVolume := UTF8Decode('Звуки');
end;

{ Assets }

class procedure Assets.LoadBase();
var
  blankData: Pointer;
begin
  SpriteShader := TglrShaderProgram.Create();
  SpriteShader.Attach(FileSystem.ReadResource(R_BASE + 'SpriteShaderV.txt'), stVertex);
  SpriteShader.Attach(FileSystem.ReadResource(R_BASE + 'SpriteShaderF.txt'), stFragment);
  SpriteShader.Link();

  GetMem(blankData, 1 * 1 * 3(*RGB*));
  FillChar(blankData^, 3, $FF);
  BlankTexture := TglrTexture.Create(blankData, 1, 1, tfRGB8);
  FreeMem(blankData);

  GuiMaterial := TglrMaterial.Create(SpriteShader);
  GuiMaterial.AddTexture(Default.BlankTexture, 'uDiffuse');

  GuiSpriteBatch := TglrSpriteBatch.Create();

  GuiCamera := TglrCamera.Create();
  GuiCamera.SetProjParams(0, 0, Render.Width, Render.Height, 45, 0.1, 100, pmOrtho, pTopLeft);
  GuiCamera.SetViewParams(
    Vec3f(0, 0, 100),
    Vec3f(0, 0, 0),
    Vec3f(0, 1, 0));

  FontMain := TglrFont.Create(FileSystem.ReadResource(R_FONT_MAIN), SpriteShader);
  FontMainBatch := TglrFontBatch.Create(FontMain);

  PixelateTexture := TglrTexture.Create(nil, W_WIDTH, W_HEIGHT, tfRGB8);
  PixelateTexture.SetFilter(fNearest);

  PixelateShader := TglrShaderProgram.Create();
  PixelateShader.Attach(FileSystem.ReadResource(R_BASE + 'SpriteShaderV.txt'), stVertex);
  PixelateShader.Attach(FileSystem.ReadResource(R_BASE + 'PixelateF.txt'), stFragment);
  PixelateShader.Link();

  PixelateMaterial := TglrMaterial.Create(PixelateShader);
  PixelateMaterial.AddTexture(PixelateTexture, 'uDiffuse');

  FrameBuffer := TglrFrameBuffer.Create();
  FrameBuffer.AttachTexture(PixelateTexture.Id);
end;

class procedure Assets.UnloadBase();
begin
  SpriteShader.Free();
  BlankTexture.Free();

  GuiMaterial.Free();

  GuiSpriteBatch.Free();

  GuiCamera.Free();

  FontMainBatch.Free();
  FontMain.Free();

  PixelateTexture.Free();
  PixelateShader.Free();
  PixelateMaterial.Free();
  FrameBuffer.Free();
end;

end.

