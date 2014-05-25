unit uMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, EditBtn, Spin;

type

  { TForm1 }

  TForm1 = class(TForm)
    bChooseFont: TButton;
    bLoadFont: TButton;
    bChooseFilePath: TButton;
    bGenerate: TButton;
    cbBold: TCheckBox;
    cbItalic: TCheckBox;
    editFilePath: TEdit;
    editSize: TSpinEdit;
    FontDialog: TFontDialog;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    mSymbols: TMemo;
    procedure bChooseFontClick(Sender: TObject);
    procedure bGenerateClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  uFont;

{ TForm1 }

procedure TForm1.bChooseFontClick(Sender: TObject);
begin
  if FontDialog.Execute() then
  begin
//    FontDialog.Font.;
  end;
end;

procedure TForm1.bGenerateClick(Sender: TObject);
var
  d: TFontDisplay;
begin
  d := TFontDisplay.Create();
  d.Free();
end;

end.

