unit uAssets;

interface

uses
  glr_scene,
  glr_render,
  glr_render2d;

type

  { Assets }

  Assets = class
  public
    // Base assets

    class var GuiAtlas: TglrTextureAtlas;
    class var GuiMaterial: TglrMaterial;
    class var GuiCamera: TglrCamera;
    class var GuiSpriteBatch: TglrSpriteBatch;

    class var FontMain: TglrFont;
    class var FontMainBatch: TglrFontBatch;

    class procedure LoadBase();
    class procedure UnloadBase();

    // Level specified assets
    // ...
  end;

const
  R_GUI_ATLAS_BUTTON      = 'button.png';
  R_GUI_ATLAS_BUTTON_OVER = 'button_over.png';
  R_GUI_ATLAS_SLIDER_BACK = 'slider_back.png';
  R_GUI_ATLAS_SLIDER_FILL = 'slider_fill.png';
  R_GUI_ATLAS_SLIDER_BTN  = 'slider_btn.png';

implementation

uses
  glr_filesystem, glr_core, glr_math;

const
  R_BASE = 'data/';

  R_GUI_ATLAS_IMG = R_BASE + 'gui.tga';
  R_GUI_ATLAS_TXT = R_BASE + 'gui.atlas';

  R_FONT_MAIN = R_BASE + 'AmazingGrotesk20b.bmp';

{ Assets }

class procedure Assets.LoadBase();
begin
  GuiAtlas := TglrTextureAtlas.Create(
    FileSystem.ReadResource(R_GUI_ATLAS_IMG),
    FileSystem.ReadResource(R_GUI_ATLAS_TXT),
    extTga, aextCheetah);

  GuiMaterial := TglrMaterial.Create(Default.SpriteShader);
  GuiMaterial.AddTexture(GuiAtlas, 'uDiffuse');

  GuiSpriteBatch := TglrSpriteBatch.Create();

  GuiCamera := TglrCamera.Create();
  GuiCamera.SetProjParams(0, 0, Render.Width, Render.Height, 45, 0.1, 100, pmOrtho, pTopLeft);
  GuiCamera.SetViewParams(
    Vec3f(0, 0, 100),
    Vec3f(0, 0, 0),
    Vec3f(0, 1, 0));

  FontMain := TglrFont.Create(FileSystem.ReadResource(R_FONT_MAIN));
  FontMainBatch := TglrFontBatch.Create(FontMain);
end;

class procedure Assets.UnloadBase();
begin
  GuiMaterial.Free();
  GuiAtlas.Free();

  GuiSpriteBatch.Free();

  GuiCamera.Free();

  FontMainBatch.Free();
  FontMain.Free();
end;

end.

