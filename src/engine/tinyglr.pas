unit tinyglr;

{$mode delphi}

interface

uses
  ogl;

type
  {$REGION 'Render'}

  TglrTextureId = type LongWord;
  TglrShaderId = type LongWord;
  TglrIndexBufferId = type LongWord;
  TglrVertexBufferId = type LongWord;
  TglrFrameBufferId = type LongWord;
  TglrIndex = type Word;


  { TglrTexture }

  TglrTexture = class
    Id: TglrTextureId;
    procedure Bind();
    constructor Create(); virtual;
    destructor Destroy(); override;
  end;


  TglrVertexBuffer = class
    Id: TglrVertexBufferId;
    procedure Bind();
    constructor Create(); virtual;
    destructor Destroy(); override;
  end;

  TglrIndexBuffer = class
    Id: TglrIndexBufferId;
    procedure Bind();
    constructor Create(); virtual;
    destructor Destroy(); override;
  end;

  TglrFrameBuffer = class
    Id: TglrFrameBufferId;
    procedure Bind();
    constructor Create(); virtual;
    destructor Destroy(); override;
  end;


  TglrBlendingMode = (bmOpaque, bmTransparency, bmAdditive, bmAlphaTest50,
                    bmAlphaTest100, bmModulate);
  TglrCullMode = ( cmNone, cmBack, cmFront );
  TglrFuncComparison = ( fcNever, fcLess, fcEqual, fcLessOrEqual, fcGreater, fcNotEqual, fcGreaterOrEqual, fcAlways );

  GLRender = class
  private
  protected
  	class var fBlendingMode: TglrBlendingMode;
  	class var fCullMode: TglrCullMode;
  	class var fDepthWrite, depthTest: Boolean;
  	class var fShader: TglrShaderId;
  	class var fTexture: TglrTextureId;
  	class var fVB: TglrVertexBufferId;
  	class var fIB: TglrIndexBufferId;
  	class var fFB: TglrFrameBufferId;
  public
  	class procedure Init();
    class procedure DeInit();

  	class procedure SetCullMode(aCullMode: TglrCullMode);
  	class procedure SetBlendingMode(aBlendingMode: TglrBlendingMode);
  	class procedure SetLighting(aEnabled: Boolean);
  	class procedure SetDepthWrite(aEnabled: Boolean);
  	class procedure SetDepthTest(aEnabled: Boolean);
  	class procedure SetDepthFunc(aComparison: TglrFuncComparison);
  	class procedure SetAlphaTest(aComparison: TglrFuncComparison; aValue: Single);

  	class procedure SetShader(aShader: TglrShaderId);
  	class procedure SetTexture(aTexture: TglrTextureId);

  	//todo:
  	class procedure DrawTriangles();
  	class procedure DrawPoints();
  	class procedure DrawPointSprites();

  end;


implementation

uses
  ogl;

{ TglrTexture }

procedure TglrTexture.Bind;
begin
  gl.BindTexture;
end;

constructor TglrTexture.Create;
begin

end;

destructor TglrTexture.Destroy;
begin
  inherited Destroy;
end;

end.

