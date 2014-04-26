unit sys_win;

//{$mode delphi}

interface

uses
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

