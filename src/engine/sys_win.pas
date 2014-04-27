unit sys_win;

interface

uses
  Windows,
  tinyglr;

type

  { TglrWindow }

  TglrWindow = class (TglrAppView)
  protected
    fHandle: THandle;
    fCaption: UnicodeString;
    fWidth, fHeight, fX, fY: Integer;
    fDesRect: TRect;
    fStyle: LongWord;
    fClass: TWndClass;
    fDC: hDC;
  public

  end;

implementation

end.

