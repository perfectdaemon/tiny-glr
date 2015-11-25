unit uAssets;

interface

uses
  glr_core,
  glr_math,
  glr_scene,
  glr_render,
  glr_render2d;

type

  { TArenaAssets }

  TArenaAssets = class
  public
    MainCamera, GuiCamera: TglrCamera;

    MainMaterial, GuiMaterial: TglrMaterial;

    SpriteBatch: TglrSpriteBatch;
    FontBatch: TglrFontBatch;

    FrameBuffer: TglrFrameBuffer;
    BloodTexture: TglrTexture;

    procedure Init();
    procedure DeInit();
  end;

var
  Assets: TArenaAssets;

implementation

{ TArenaAssets }

procedure TArenaAssets.Init;
begin
  MainMaterial := TglrMaterial.Create(Default.SpriteShader);
  MainMaterial.AddTexture(Default.BlankTexture, 'uDiffuse');

  MainCamera := TglrCamera.Create();
  MainCamera.SetProjParams(0, 0, Render.Width, Render.Height, 45, 0.1, 100, pmOrtho, pCenter);
  MainCamera.SetViewParams(Vec3f(0, 0, 100), Vec3f(0, 0, 0), Vec3f(0, 1, 0));

  GuiMaterial := TglrMaterial.Create(Default.SpriteShader);
  GuiMaterial.AddTexture(Default.BlankTexture, 'uDiffuse');

  GuiCamera := TglrCamera.Create();
  GuiCamera.SetProjParams(0, 0, Render.Width, Render.Height, 45, 0.1, 100, pmOrtho, pTopLeft);
  GuiCamera.SetViewParams(Vec3f(0, 0, 100), Vec3f(0, 0, 0), Vec3f(0, 1, 0));

  SpriteBatch := TglrSpriteBatch.Create();
  FontBatch := TglrFontBatch.Create(Default.Font);

  BloodTexture := TglrTexture.Create(nil, 1024, 1024, tfRGB8);

  FrameBuffer := TglrFrameBuffer.Create();
  FrameBuffer.AttachTexture(BloodTexture.Id);
end;

procedure TArenaAssets.DeInit;
begin
  MainMaterial.Free();
  MainCamera.Free();
  GuiMaterial.Free();
  GuiCamera.Free();
  SpriteBatch.Free();
  FontBatch.Free();
  FrameBuffer.Free();
  BloodTexture.Free();
end;

end.

