unit sys_win;

interface

uses
  Windows,
  tinyglr;

type
  { TglrWindow }

  TglrWindow = class (TglrAppView)
  private
    time, startTime, freq: LARGE_INTEGER;
    msg: TMSG;
    currentTime, lastTime: Integer;
  protected
    fHandle: THandle;
    fDC: HDC;
    fRC: HGLRC;

    fDeltaTime: Double;
    function GetTime(): Integer;
    function WndProc(hWnd: HWND; message: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
  public
    fShouldQuit: Boolean;
    constructor Create(aData: Pointer); override;
    destructor Destroy(); override;

    procedure Loop(); override;
  end;
  PglrWindow = ^TglrWindow;

  function FileExists(const FileName: AnsiString): Boolean;
  procedure FindFiles(const aPath, aExt: AnsiString; var aList: TglrStringList);
  function ExtractFileExt(const aFileName: AnsiString): AnsiString;

implementation

function FileExists(const FileName: AnsiString): Boolean;
var
  Attr: Dword;
begin
  Attr := GetFileAttributesA(PAnsiChar(FileName));
  if (Attr <> $ffffffff) then
    Result := (Attr and FILE_ATTRIBUTE_DIRECTORY) = 0
  else
    Result := False;
end;

var
  wnd: TglrWindow; //temp variable for WndProc

{ TglrWindow }

function WndProcFirst(hWnd: HWND; message: UINT; wParam: WPARAM;
  lParam: LPARAM): LRESULT; stdcall;
begin
  wnd := TglrWindow(GetWindowLongPtrA(hwnd, GWL_USERDATA));
  if (wnd <> nil) then
    Result := wnd.WndProc(hWnd, message, wParam, lParam)
  else
    Result := DefWindowProcA(hWnd, message, wParam, lParam);
end;


type
  TSearchRec = Record
    Time : Longint;
    Size : Int64;
    Attr : Longint;
    Name : String;
    ExcludeAttr : Longint;
    FindHandle : THandle;
    FindData : TWin32FindData;
  end;

const
  { File attributes }
  faDirectory = $00000010;
  faAnyFile   = $0000003f;

  Invalid_Handle_value = HANDLE(-1);

function FindFirstFileA(lpFileName: LPCSTR; var lpFindFileData: TWIN32FindDataA): THandle; external 'kernel32' name 'FindFirstFileA';
function GetLastError:DWORD; external 'kernel32' name 'GetLastError';

function FindMatch(var f: TSearchRec): Longint;
begin
  { Find file with correct attribute }
  while (F.FindData.dwFileAttributes and Cardinal(F.ExcludeAttr)) <> 0 do
    if not FindNextFile (F.FindHandle, F.FindData) then
      Exit(GetLastError);

  { Convert some attributes back }
//  WinToDosTime(F.FindData.ftLastWriteTime,F.Time);
  f.size := F.FindData.NFileSizeLow + (QWord(MaxDWord) + 1) * F.FindData.NFileSizeHigh;
  f.attr := F.FindData.dwFileAttributes;
  f.Name := StrPas(@F.FindData.cFileName[0]);
  Result := 0;
end;


function FindFirst(const Path: String; Attr: Longint; out Rslt: TSearchRec): Longint;
begin
  Rslt.Name := Path;
  Rslt.Attr := attr;
  Rslt.ExcludeAttr := (not Attr) and ($1e); { $1e = faHidden or faSysFile or faVolumeID or faDirectory }
  { FindFirstFile is a Win32 Call }
  Rslt.FindHandle := FindFirstFile (PChar(Path), Rslt.FindData);
  if (Rslt.FindHandle = Invalid_Handle_value) then
    Exit(GetLastError);

  { Find file with correct attribute }
  Result := FindMatch(Rslt);
end;


function FindNext(var Rslt: TSearchRec): Longint;
begin
  if FindNextFile(Rslt.FindHandle, Rslt.FindData) then
    Result := FindMatch(Rslt)
  else
    Result := GetLastError;
end;


procedure FindClose(var F: TSearchrec);
begin
  if (F.FindHandle <> INVALID_HANDLE_VALUE) then
    Windows.FindClose(F.FindHandle);
end;

function ExtractFileExt(const aFileName: AnsiString): AnsiString;
var
  i: Integer;
begin
  i := Length(aFileName) - 1;
  while ((i > 0) and (not (aFileName[i] in ['.', '/', '\', ':']))) do
    i -= 1;

  if (aFileName[i] = '.') and (i > 0) then
    Result := Copy(aFileName, i, MaxInt);
end;

procedure FindFiles(const aPath, aExt: AnsiString; var aList: TglrStringList);
var
  searchResult: TSearchRec;
  fPath: AnsiString;
begin
  fPath := aPath;
  if (fPath <> '') then
    if (fPath[Length(fPath)] = '\') then
      fPath[Length(fPath)] := '/'
    else if (fPath[Length(fPath)] <> '/') then
      fPath += '/';

  if (FindFirst(fPath + '*', faAnyFile, searchResult) = 0) then
    try
      repeat
        if (searchResult.Attr and faDirectory) = 0 then
        begin
          if (aExt = '') or (ExtractFileExt(searchResult.Name) = aExt) then
            aList.Add((fPath + searchResult.Name));
        end
        else if (searchResult.Name <> '.') and (searchResult.Name <> '..') then
        begin
          FindFiles(fPath + searchResult.Name, aExt, aList);
        end;
      until FindNext(searchResult) <> 0
    finally
      FindClose(searchResult);
    end;
end;

function TglrWindow.GetTime(): Integer;
begin
  QueryPerformanceCounter(@time);
  Result := round(1000 * (time.QuadPart - startTime.QuadPart) / freq.QuadPart);
end;

function TglrWindow.WndProc(hWnd: HWND; message: UINT; wParam: WPARAM;
  lParam: LPARAM): LRESULT; stdcall;
begin
  Result := 0;
  case (message) of
    WM_ACTIVATEAPP:
      if (wParam = 0) then
        Core.Pause()
      else
        Core.Resume();

    WM_CLOSE, WM_DESTROY:
      Self.fShouldQuit := True;

    WM_MOUSEMOVE:
      if (wParam and MK_LBUTTON <> 0) then
        Core.InputReceived(itTouchMove, kLeftButton, LOWORD(lParam), HIWORD(lParam), 0)
      else if (wParam and MK_RBUTTON <> 0) then
        Core.InputReceived(itTouchMove, kRightButton, LOWORD(lParam), HIWORD(lParam), 0)
      else if (wParam and MK_MBUTTON <> 0) then
        Core.InputReceived(itTouchMove, kMiddleButton, LOWORD(lParam), HIWORD(lParam), 0)
      else
        Core.InputReceived(itTouchMove, kNoInput, LOWORD(lParam), HIWORD(lParam), 0);

    WM_LBUTTONDOWN, WM_LBUTTONUP, WM_LBUTTONDBLCLK:
      if (message = WM_LBUTTONUP) then
        Core.InputReceived(itTouchUp, kLeftButton, LOWORD(lParam), HIWORD(lParam), 0)
      else
        Core.InputReceived(itTouchDown, kLeftButton, LOWORD(lParam), HIWORD(lParam), 0);


    WM_RBUTTONDOWN, WM_RBUTTONUP, WM_RBUTTONDBLCLK:
      if (message = WM_LBUTTONUP) then
        Core.InputReceived(itTouchUp, kRightButton, LOWORD(lParam), HIWORD(lParam), 0)
      else
        Core.InputReceived(itTouchDown, kRightButton, LOWORD(lParam), HIWORD(lParam), 0);

    WM_KEYDOWN, WM_KEYUP:
      if (message = WM_KEYUP) then
        Core.InputReceived(itKeyUp, TglrKey(wParam), 0, 0, 0)
      else
        Core.InputReceived(itKeyDown, TglrKey(wParam), 0, 0, 0);

    WM_MOUSEWHEEL:
      if (wParam > 0) then
        Core.InputReceived(itWheel, kWheelUp, LOWORD(lParam), HIWORD(lParam), 1 * (HIWORD(wParam) div WHEEL_DELTA))
      else
        Core.InputReceived(itWheel, kWheelDown, LOWORD(lParam), HIWORD(lParam), -1 * (HIWORD(wParam) div WHEEL_DELTA));

    else
      Result := DefWindowProcA(hWnd, message, wParam, lParam);
  end;
end;

constructor TglrWindow.Create(aData: Pointer);
var
  p: TglrInitParams;
  r: RECT;
  pfd: PIXELFORMATDESCRIPTOR;
  wStyle: LongWord;
  wClass: TWndClass;
begin
  p := TglrInitParams(aData^);
  ZeroMemory(@wClass, SizeOf(wClass));
  with wClass do
  begin
    style := CS_VREDRAW or CS_HREDRAW or CS_OWNDC;
    hInstance := 0;
    hIcon := LoadIcon(0, IDI_WINLOGO);
    hCursor := LoadCursor(0, IDC_ARROW);
    hbrBackground := GetStockObject (White_Brush);
    lpfnWndProc := @WndProcFirst;
    lpszClassName := 'TglrWindow';
  end;
  Windows.RegisterClass(wClass);

  wStyle := WS_VISIBLE or WS_OVERLAPPED or WS_CAPTION or WS_SYSMENU or WS_MINIMIZEBOX or WS_CLIPSIBLINGS or WS_CLIPCHILDREN; //WS_VISIBLE or WS_CAPTION or WS_SYSMENU or WS_MINIMIZEBOX or WS_CLIPCHILDREN;

  SetRect(r, 0, 0, p.Width, p.Height);
  AdjustWindowRect(r, wStyle, False);
  fHandle := CreateWindow('TglrWindow', PAnsiChar(Utf8ToAnsi(p.Caption)), wStyle, p.X, p.Y, r.Right - r.Left, r.Bottom - r.Top, 0, 0, 0, Self);
  SetWindowLongPtrA(fHandle, GWL_USERDATA, LONG_PTR(Self));

  fDC := GetDC(fHandle);

  ZeroMemory(@pfd, SizeOf(pfd));
  pfd.nSize := SizeOf(pfd);
  pfd.nVersion := 1;
  pfd.dwFlags := PFD_DRAW_TO_WINDOW or PFD_SUPPORT_OPENGL or PFD_DOUBLEBUFFER;
  pfd.cColorBits := 32;
  pfd.cAlphaBits := 8;
  pfd.cDepthBits := 24;
  pfd.cStencilBits := 8;

  SetPixelFormat(fDC, ChoosePixelFormat(fDC, pfd), @pfd);
  fRC := wglCreateContext(fDC);
  wglMakeCurrent(fDC, fRC);

  QueryPerformanceFrequency(@freq);
  QueryPerformanceCounter(@startTime);

  fShouldQuit := False;
end;

destructor TglrWindow.Destroy();
begin
  wglMakeCurrent(0, 0);
  wglDeleteContext(fRC);
  ReleaseDC(fHandle, fDC);
  CloseWindow(fHandle);
  DestroyWindow(fHandle);
end;

procedure TglrWindow.Loop();
begin
  repeat
    if (PeekMessageA(msg, fHandle, 0, 0, PM_REMOVE)) then
    begin
      TranslateMessage(msg);
      DispatchMessageA(msg);
    end
    else
    begin
      currentTime := getTime();
      fDeltaTime := (currentTime - lastTime) / 1000.0;
      lastTime := currentTime;

      if (fDeltaTime > 0.05) then
        fDeltaTime := 0.05;

      Core.Update(fDeltaTime);
      Core.RenderAll();

      SwapBuffers(fDC);
    end;
  until Self.fShouldQuit;
end;


end.

