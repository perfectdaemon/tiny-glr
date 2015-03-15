unit ogl;

interface

type
  TGLConst = (
  // Boolean
    GL_FALSE = 0, GL_TRUE,
  // AttribMask
    GL_DEPTH_BUFFER_BIT = $0100, GL_STENCIL_BUFFER_BIT = $0400, GL_COLOR_BUFFER_BIT = $4000,
  // Begin Mode
    GL_POINTS = 0, GL_LINES, GL_LINE_LOOP, GL_LINE_STRIP, GL_TRIANGLES, GL_TRIANGLE_STRIP, GL_TRIANGLE_FAN, GL_QUADS, GL_QUAD_STRIP, GL_POLYGON,
  // Alpha Function
    GL_NEVER = $0200, GL_LESS, GL_EQUAL, GL_LEQUAL, GL_GREATER, GL_NOTEQUAL, GL_GEQUAL, GL_ALWAYS,
  // Blending Factor
    GL_ZERO = 0, GL_ONE, GL_SRC_COLOR = $0300, GL_ONE_MINUS_SRC_COLOR, GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA, GL_DST_ALPHA, GL_ONE_MINUS_DST_ALPHA, GL_DST_COLOR = $0306, GL_ONE_MINUS_DST_COLOR, GL_SRC_ALPHA_SATURATE,
  // DrawBuffer Mode
    GL_NONE = 0, GL_FRONT = $0404, GL_BACK, GL_FRONT_AND_BACK = $0408,
  // Pixel params
    GL_UNPACK_ALIGNMENT = $0CF5,
  // Tests
    GL_DEPTH_TEST = $0B71, GL_STENCIL_TEST = $0B90, GL_ALPHA_TEST = $0BC0, GL_SCISSOR_TEST = $0C11,
  // GetTarget
    GL_CULL_FACE = $0B44, GL_BLEND = $0BE2,
  // Data Types
    GL_BYTE = $1400, GL_UNSIGNED_BYTE, GL_SHORT, GL_UNSIGNED_SHORT, GL_INT, GL_UNSIGNED_INT, GL_FLOAT, GL_HALF_FLOAT = $140B, GL_UNSIGNED_SHORT_5_6_5 = $8363, GL_UNSIGNED_SHORT_4_4_4_4_REV = $8365, GL_UNSIGNED_SHORT_1_5_5_5_REV,
  // Matrix Mode
    GL_MODELVIEW = $1700, GL_PROJECTION, GL_TEXTURE,
  // Pixel Format
    GL_DEPTH_COMPONENT = $1902, GL_RED, GL_GREEN, GL_BLUE, GL_ALPHA, GL_RGB, GL_RGBA, GL_LUMINANCE, GL_LUMINANCE_ALPHA,
    GL_DEPTH_COMPONENT16 = $81A5, GL_DEPTH_COMPONENT24, GL_DEPTH_COMPONENT32, GL_ALPHA8 = $803C, GL_LUMINANCE8 = $8040,
    GL_LUMINANCE8_ALPHA8 = $8045, GL_RGB8 = $8051, GL_RGBA8 = $8058, GL_BGR = $80E0, GL_BGRA, GL_RGB5 = $8050,
    GL_RGBA4 = $8056, GL_RGB5_A1 = $8057, GL_RG = $8227, GL_R16F = $822D, GL_R32F, GL_RG16F, GL_RG32F, GL_RGBA32F = $8814,
    GL_RGBA16F = $881A,
    GL_LUMINANCE16 = $8042,
    GL_LUMINANCE16_ALPHA16 = $8048,
    GL_RGB16 = $8054,
    GL_RGBA16 = $805B,
  // PolygonMode
    GL_POINT = $1B00, GL_LINE, GL_FILL,
  // Smooth
    GL_POINT_SMOOTH = $0B10, GL_LINE_SMOOTH = $0B20, GL_POLYGON_SMOOTH = $0B41,
  // List mode
    GL_COMPILE = $1300, GL_COMPILE_AND_EXECUTE,
  // Lighting
    GL_LIGHTING = $0B50, GL_LIGHT0 = $4000, GL_AMBIENT = $1200, GL_DIFFUSE, GL_SPECULAR, GL_POSITION, GL_SPOT_DIRECTION, GL_SPOT_EXPONENT, GL_SPOT_CUTOFF, GL_CONSTANT_ATTENUATION, GL_LINEAR_ATTENUATION, GL_QUADRATIC_ATTENUATION,
  // StencilOp
    GL_KEEP = $1E00, GL_REPLACE, GL_INCR, GL_DECR,
  // GetString Parameter
    GL_VENDOR = $1F00, GL_RENDERER, GL_VERSION, GL_EXTENSIONS, GL_SHADING_LANGUAGE_VERSION = $8B8C,
  // TextureEnvParameter
    GL_TEXTURE_ENV_MODE = $2200, GL_TEXTURE_ENV_COLOR,
  // TextureEnvTarget
    GL_TEXTURE_ENV = $2300,
  // Texture Filter
    GL_NEAREST = $2600, GL_LINEAR, GL_NEAREST_MIPMAP_NEAREST = $2700, GL_LINEAR_MIPMAP_NEAREST, GL_NEAREST_MIPMAP_LINEAR, GL_LINEAR_MIPMAP_LINEAR, GL_TEXTURE_MAG_FILTER = $2800, GL_TEXTURE_MIN_FILTER,
  // Texture Wrap Mode
    GL_TEXTURE_WRAP_S = $2802, GL_TEXTURE_WRAP_T, GL_TEXTURE_WRAP_R = $8072,
    GL_CLAMP = $2900, GL_REPEAT = $2901, GL_CLAMP_TO_EDGE = $812F, GL_CLAMP_TO_BORDER = $812D, GL_MIRRORED_REPEAT = $8370,
    GL_TEXTURE_BASE_LEVEL = $813C, GL_TEXTURE_MAX_LEVEL,
  // Textures
    GL_TEXTURE_1D = $0DE0, GL_TEXTURE_2D = $0DE1, GL_TEXTURE_3D = $806F, GL_TEXTURE0 = $84C0, GL_TEXTURE_MAX_ANISOTROPY = $84FE, GL_MAX_TEXTURE_MAX_ANISOTROPY, GL_GENERATE_MIPMAP = $8191,
    GL_TEXTURE_CUBE_MAP = $8513, GL_TEXTURE_CUBE_MAP_POSITIVE_X = $8515, GL_TEXTURE_CUBE_MAP_NEGATIVE_X, GL_TEXTURE_CUBE_MAP_POSITIVE_Y, GL_TEXTURE_CUBE_MAP_NEGATIVE_Y, GL_TEXTURE_CUBE_MAP_POSITIVE_Z, GL_TEXTURE_CUBE_MAP_NEGATIVE_Z,
  // Compressed Textures
    GL_COMPRESSED_RGB_S3TC_DXT1 = $83F0, GL_COMPRESSED_RGBA_S3TC_DXT1, GL_COMPRESSED_RGBA_S3TC_DXT3, GL_COMPRESSED_RGBA_S3TC_DXT5,
  // FBO
    GL_FRAMEBUFFER = $8D40, GL_RENDERBUFFER, GL_COLOR_ATTACHMENT0 = $8CE0, GL_DEPTH_ATTACHMENT = $8D00, GL_FRAMEBUFFER_BINDING = $8CA6, GL_FRAMEBUFFER_COMPLETE = $8CD5,
  // Shaders
    GL_FRAGMENT_SHADER = $8B30, GL_VERTEX_SHADER, GL_GEOMETRY_SHADER = $8DD9, GL_GEOMETRY_VERTICES_OUT = $8DDA, GL_GEOMETRY_INPUT_TYPE, GL_GEOMETRY_OUTPUT_TYPE, GL_MAX_GEOMETRY_OUTPUT_VERTICES = $8DE0, GL_COMPILE_STATUS = $8B81, GL_LINK_STATUS, GL_VALIDATE_STATUS, GL_INFO_LOG_LENGTH,
  // VBO
    GL_VERTEX_ARRAY = $8074, GL_NORMAL_ARRAY, GL_COLOR_ARRAY, GL_INDEX_ARRAY_EXT, GL_TEXTURE_COORD_ARRAY,
    GL_ARRAY_BUFFER = $8892, GL_ELEMENT_ARRAY_BUFFER,   GL_READ_ONLY = $88B8, GL_WRITE_ONLY = $88B9, GL_READ_WRITE = $88BA,
    GL_STREAM_DRAW  = $88E0, GL_STREAM_READ, GL_STREAM_COPY,
    GL_STATIC_DRAW = $88E4, GL_STATIC_READ, GL_STATIC_COPY,
    GL_DYNAMIC_DRAW = $88E8, GL_DYNAMIC_READ, GL_DYNAMIC_COPY,

  // Queries
    GL_SAMPLES_PASSED = $8914, GL_QUERY_COUNTER_BITS = $8864, GL_CURRENT_QUERY, GL_QUERY_RESULT, GL_QUERY_RESULT_AVAILABLE,
    //GL_MAX_CONST = High(LongInt),


    GL_COLOR = $1800,
    GL_POINT_SPRITE = $8861,
    GL_COORD_REPLACE = $8862,
    GL_POINT_DISTANCE_ATTENUATION = $8129,
    GL_POINT_SIZE_MIN = $8126,
    GL_POINT_SIZE_MAX = $8127,
    GL_POINT_FADE_THRESHOLD_SIZE = $8128,


    //GL_MODULATE = $2100, GL_DECAL = $2101, GL_ADD = $0104,

    GL_MULTISAMPLE = $809D

    //wgl Choose pixel format
//    WGL_DRAW_TO_WINDOW_ARB = $2001,
//    WGL_ACCELERATION_ARB = $2003,
//    WGL_SUPPORT_OPENGL_ARB = $2010,
//    WGL_DOUBLE_BUFFER_ARB = $2011,
//    WGL_COLOR_BITS_ARB = $2014,
//    WGL_ALPHA_BITS_ARB = $201B,
//    WGL_DEPTH_BITS_ARB = $2022,
//    WGL_STENCIL_BITS_ARB = $2023,
//    WGL_FULL_ACCELERATION_ARB = $2027,
//    WGL_SAMPLE_BUFFERS_ARB = $2041,
//    WGL_SAMPLES_ARB	= $2042
  );

  PGLConst = ^TGLConst;

  { TglrGL }

  TglrGL = record
  private
    Lib : LongWord;
  public
    {$IFDEF WINDOWS}
    GetProc        : function (ProcName: PAnsiChar): Pointer; stdcall;
    SwapInterval   : function (Interval: LongInt): LongInt; stdcall;
    ChoosePixelFormat: function(DC: LongWord; const piAttribIList: PInteger; const pfAttribFList: PSingle;
      nMaxFormats: Cardinal; piFormats: PInteger; nNumFormats: PCardinal) : LongBool; stdcall;
    {$ENDIF}
    GetIntegerv    : procedure (pname: TGLConst; params: Pointer); stdcall;
    GetFloatv      : procedure (pname: TGLConst; params: Pointer); stdcall;
    GetString      : function (name: TGLConst): PAnsiChar; stdcall;
    GetError       : function (): LongWord; stdcall;
    Flush          : procedure;
    Finish         : procedure;
    PixelStorei    : procedure (pname: TGLConst; param: LongInt); stdcall;
    GenTextures    : procedure (n: LongInt; textures: Pointer); stdcall;
    DeleteTextures : procedure (n: LongInt; textures: Pointer); stdcall;
    BindTexture    : procedure (target: TGLConst; texture: LongWord); stdcall;
    TexParameteri  : procedure (target, pname: TGLConst; param: LongInt); stdcall;
    TexParameterf  : procedure (target, pname: TGLConst; param: Single);
    TexImage2D     : procedure (target: TGLConst; level: LongInt; internalformat: TGLConst; width, height, border: LongInt; format, _type: TGLConst; data: Pointer); stdcall;
    TexSubImage2D  : procedure (target: TGLConst; level, x, y, width, height: LongInt; format, _type: TGLConst; data: Pointer); stdcall;
    GetTexImage    : procedure (target: TGLConst; level: LongInt; format, _type: TGLConst; data: Pointer); stdcall;
    GenerateMipmap : procedure (target: TGLConst); stdcall;
    CopyTexSubImage2D    : procedure (target: TGLConst; level, xoffset, yoffset, x, y, width, height: LongInt); stdcall;
    CompressedTexImage2D : procedure (target: TGLConst; level: LongInt; internalformat: TGLConst; width, height, border, imageSize: LongInt; data: Pointer); stdcall;
    ActiveTexture        : procedure (texture: LongWord); stdcall;
    ClientActiveTexture  : procedure (texture: TGLConst); stdcall;
    Clear          : procedure (mask: TGLConst); stdcall;
    ClearColor     : procedure (red, green, blue, alpha: Single); stdcall;
    ColorMask      : procedure (red, green, blue, alpha: Boolean); stdcall;
    DepthMask      : procedure (flag: Boolean); stdcall;
    StencilMask    : procedure (mask: LongWord); stdcall;
    Enable         : procedure (cap: TGLConst); stdcall;
    Disable        : procedure (cap: TGLConst); stdcall;
    CullFace       : procedure (mode: TGLConst); stdcall;
    AlphaFunc      : procedure (func: TGLConst; factor: Single); stdcall;
    BlendFunc      : procedure (sfactor, dfactor: TGLConst); stdcall;
    StencilFunc    : procedure (func: TGLConst; ref: LongInt; mask: LongWord); stdcall;
    DepthFunc      : procedure (func: TGLConst); stdcall;
    StencilOp      : procedure (fail, zfail, zpass: TGLConst); stdcall;
    Viewport       : procedure (x, y, width, height: LongInt); stdcall;
    Scissor        : procedure (x, y, width, height: LongInt); stdcall;
    PointSize      : procedure (size: Single); stdcall;
    DrawElements    : procedure (mode: TGLConst; count: LongInt; _type: TGLConst; const indices: Pointer); stdcall;
    Ortho           : procedure (left, right, bottom, top, zNear, zFar: Double); stdcall;
    Frustum         : procedure (left, right, bottom, top, zNear, zFar: Double); stdcall;
    ReadPixels      : procedure (x, y, width, height: LongInt; format, _type: TGLConst; pixels: Pointer); stdcall;
    DrawBuffer      : procedure (mode: TGLConst); stdcall;
    ReadBuffer      : procedure (mode: TGLConst); stdcall;
  // VBO
    GenBuffers      : procedure (n: LongInt; buffers: Pointer); stdcall;
    DeleteBuffers   : procedure (n: LongInt; const buffers: Pointer); stdcall;
    BindBuffer      : procedure (target: TGLConst; buffer: LongWord); stdcall;
    BufferData      : procedure (target: TGLConst; size: LongInt; const data: Pointer; usage: TGLConst); stdcall;
    BufferSubData   : procedure (target: TGLConst; offset, size: LongInt; const data: Pointer); stdcall;
    MapBuffer       : function  (target, access: TGLConst): Pointer; stdcall;
    UnmapBuffer     : function  (target: TGLConst): Boolean; stdcall;
  // GLSL Shaders
    GetProgramiv      : procedure (_program: LongWord; pname: TGLConst; params: Pointer); stdcall;
    CreateProgram     : function: LongWord;
    DeleteProgram     : procedure (_program: LongWord); stdcall;
    LinkProgram       : procedure (_program: LongWord); stdcall;
    ValidateProgram   : procedure (_program: LongWord); stdcall;
    UseProgram        : procedure (_program: LongWord); stdcall;
    GetProgramInfoLog : procedure (_program: LongWord; maxLength: LongInt; var length: LongInt; infoLog: PAnsiChar); stdcall;
    GetShaderiv       : procedure (_shader: LongWord; pname: TGLConst; params: Pointer); stdcall;
    CreateShader      : function  (shaderType: TGLConst): LongWord; stdcall;
    DeleteShader      : procedure (_shader: LongWord); stdcall;
    ShaderSource      : procedure (_shader: LongWord; count: LongInt; src: Pointer; len: Pointer); stdcall;
    AttachShader      : procedure (_program, _shader: LongWord); stdcall;
    CompileShader     : procedure  (_shader: LongWord); stdcall;
    GetShaderInfoLog  : procedure (_shader: LongWord; maxLength: LongInt; var length: LongInt; infoLog: PAnsiChar); stdcall;
    GetUniformLocation  : function  (_program: LongWord; const ch: PAnsiChar): LongInt; stdcall;
    Uniform1iv          : procedure (location, count: LongInt; value: Pointer); stdcall;
    Uniform1fv          : procedure (location, count: LongInt; value: Pointer); stdcall;
    Uniform2fv          : procedure (location, count: LongInt; value: Pointer); stdcall;
    Uniform3fv          : procedure (location, count: LongInt; value: Pointer); stdcall;
    Uniform4fv          : procedure (location, count: LongInt; value: Pointer); stdcall;
    UniformMatrix3fv    : procedure (location, count: LongInt; transpose: Boolean; value: Pointer); stdcall;
    UniformMatrix4fv    : procedure (location, count: LongInt; transpose: Boolean; value: Pointer); stdcall;
    GetAttribLocation        : function  (_program: LongWord; const ch: PAnsiChar): LongInt; stdcall;
    EnableVertexAttribArray  : procedure (index: LongWord); stdcall;
    DisableVertexAttribArray : procedure (index: LongWord); stdcall;
    VertexAttribPointer      : procedure (index: LongWord; size: LongInt; _type: TGLConst; normalized: Boolean; stride: LongInt; const ptr: Pointer); stdcall;
    BindAttribLocation       : procedure (_program: LongWord; index: LongWord; const name: PAnsiChar); stdcall;
  // FBO
    DrawBuffers             : procedure (n: LongInt; bufs: Pointer); stdcall;
    GenFramebuffers         : procedure (n: LongInt; framebuffers: Pointer); stdcall;
    DeleteFramebuffers      : procedure (n: LongInt; framebuffers: Pointer); stdcall;
    BindFramebuffer         : procedure (target: TGLConst; framebuffer: LongWord); stdcall;
    FramebufferTexture2D    : procedure (target, attachment, textarget: TGLConst; texture: LongWord; level: LongInt); stdcall;
    FramebufferRenderbuffer : procedure (target, attachment, renderbuffertarget: TGLConst; renderbuffer: LongWord); stdcall;
    CheckFramebufferStatus  : function  (target: TGLConst): TGLConst; stdcall;
    GenRenderbuffers        : procedure (n: LongInt; renderbuffers: Pointer); stdcall;
    DeleteRenderbuffers     : procedure (n: LongInt; renderbuffers: Pointer); stdcall;
    BindRenderbuffer        : procedure (target: TGLConst; renderbuffer: LongWord); stdcall;
    RenderbufferStorage     : procedure (target, internalformat: TGLConst; width, height: LongInt); stdcall;
  // Multisample
    SampleCoverage          : procedure (value: Single; invert: Boolean); stdcall;

    PointParameterfv        : procedure (pname: TGLConst; const params: Pointer); stdcall;
    PointParameterf         : procedure (pname: TGLConst; param: Single); stdcall;
    TexEnvf                 : procedure (target, pname: TGLConst; param: Single); stdcall;
    TexEnvi                 : procedure (target, pname: TGLConst; param: TGLConst); stdcall;

    procedure Init;
    procedure Free;
    function IsExtensionSupported(const Extension: String): Boolean;
  end;

var
  gl: TglrGL;

implementation

uses
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF}
  glr_utils;

procedure TglrGL.Init;
type
  TProcArray = array [-1..(SizeOf(TglrGL) - SizeOf(Lib)) div 4 - 1] of Pointer;
const
  ProcName : array [0..(SizeOf(TglrGL) - SizeOf(Lib)) div 4 - 1] of PAnsiChar = (
    {$IFDEF WINDOWS}
    'wglGetProcAddress',
    'wglSwapIntervalEXT',
    'wglChoosePixelFormatARB',
    {$ENDIF}
    'glGetIntegerv',
    'glGetFloatv',
    'glGetString',
    'glGetError',
    'glFlush',
    'glFinish',
    'glPixelStorei',
    'glGenTextures',
    'glDeleteTextures',
    'glBindTexture',
    'glTexParameteri',
    'glTexParameterf',
    'glTexImage2D',
    'glTexSubImage2D',
    'glGetTexImage',
    'glGenerateMipmapEXT',
    'glCopyTexSubImage2D',
    'glCompressedTexImage2DARB',
    'glActiveTextureARB',
    'glClientActiveTextureARB',
    'glClear',
    'glClearColor',
    'glColorMask',
    'glDepthMask',
    'glStencilMask',
    'glEnable',
    'glDisable',
    'glCullFace',
    'glAlphaFunc',
    'glBlendFunc',
    'glStencilFunc',
    'glDepthFunc',
    'glStencilOp',
    'glViewport',
    'glScissor',
    'glPointSize',
    'glDrawElements',
    'glOrtho',
    'glFrustum',
    'glReadPixels',
    'glDrawBuffer',
    'glReadBuffer',
    'glGenBuffers',
    'glDeleteBuffers',
    'glBindBuffer',
    'glBufferData',
    'glBufferSubData',
    'glMapBuffer',
    'glUnmapBuffer',
    'glGetProgramiv',
    'glCreateProgram',
    'glDeleteProgram',
    'glLinkProgram',
    'glValidateProgram',
    'glUseProgram',
    'glGetProgramInfoLog',
    'glGetShaderiv',
    'glCreateShader',
    'glDeleteShader',
    'glShaderSource',
    'glAttachShader',
    'glCompileShader',
    'glGetShaderInfoLog',
    'glGetUniformLocation',
    'glUniform1iv',
    'glUniform1fv',
    'glUniform2fv',
    'glUniform3fv',
    'glUniform4fv',
    'glUniformMatrix3fv',
    'glUniformMatrix4fv',
    'glGetAttribLocation',
    'glEnableVertexAttribArray',
    'glDisableVertexAttribArray',
    'glVertexAttribPointer',
    'glBindAttribLocation',
    'glDrawBuffers',
    'glGenFramebuffersEXT',
    'glDeleteFramebuffersEXT',
    'glBindFramebufferEXT',
    'glFramebufferTexture2DEXT',
    'glFramebufferRenderbufferEXT',
    'glCheckFramebufferStatusEXT',
    'glGenRenderbuffersEXT',
    'glDeleteRenderbuffersEXT',
    'glBindRenderbufferEXT',
    'glRenderbufferStorageEXT',
    'glSampleCoverage',
    'glPointParameterfv',
    'glPointParameterf',
    'glTexEnvf',
    'glTexEnvi'
  );
var
  i    : LongInt;
  Proc : ^TProcArray;
begin
  {$IFDEF WINDOWS}
  Lib := LoadLibraryA(opengl32);
  {$ELSE}
  {$ENDIF}
  if Lib <> 0 then
  begin
    Proc := @Self;
    {$IFDEF WINDOWS}
    Proc^[0] := GetProcAddress(Lib, ProcName[0]); // gl.GetProc
    {$ENDIF}
    for i := 1 to High(ProcName) do
    begin
      Proc^[i] := GetProc(ProcName[i]);
      if Proc^[i] = nil then
        Proc^[i] := GetProcAddress(Lib, ProcName[i]);
      if Proc^[i] = nil then
        Log.Write(lWarning, 'Pointer for "' + ProcName[i] + '" is nil');
    end;
  end;
  Set8087CW($133F); //Предотвращает EInvalidOp при 1/0, 0/0, -1/0
end;

function TglrGL.IsExtensionSupported(const Extension: String): Boolean;
var
  extensions: PAnsiChar;
begin
  extensions := gl.GetString(TGLConst.GL_EXTENSIONS);
  Result := Pos(Extension, extensions) <> 0;
end;

procedure TglrGL.Free;
begin
  {$IFDEF WINDOWS}
  FreeLibrary(Lib);
  {$ENDIF}
end;

end.
