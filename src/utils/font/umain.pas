unit uMain;

{$mode delphi}{$H+}

interface

uses
  uFont,
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, EditBtn, Spin;

type

  { TForm1 }

  TForm1 = class(TForm)
    bChooseFont: TButton;
    bLoadFont: TButton;
    bChooseFilePath: TButton;
    bGenerate: TButton;
    bSave: TButton;
    cbBold: TCheckBox;
    cbItalic: TCheckBox;
    editFontName: TEdit;
    editFilePath: TEdit;
    editSize: TSpinEdit;
    FontDialog: TFontDialog;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label2: TLabel;
    mSymbols: TMemo;
    Panel1: TPanel;
    procedure bChooseFontClick(Sender: TObject);
    procedure bGenerateClick(Sender: TObject);
    procedure bSaveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FontDisplay: TFontDisplay;
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  Windows;

{ TForm1 }

procedure TForm1.bChooseFontClick(Sender: TObject);
begin
  if FontDialog.Execute() then
  begin
    editFontName.Text := FontDialog.Font.Name;
    cbBold.Checked := FontDialog.Font.Bold;
    cbItalic.Checked := FontDialog.Font.Italic;
    editSize.Value := FontDialog.Font.Size;
  end;
end;

procedure TForm1.bGenerateClick(Sender: TObject);
begin
  FontDisplay.GenFree();
  FontDisplay.GenInit(editFontName.Text, editSize.Value, cbBold.Checked, cbItalic.Checked);
  FontDisplay.AddChars(UTF8Decode(mSymbols.Lines.Text));
  FontDisplay.PackChars();
  //todo: draw bitmap to panel
  FontDisplay.Draw(GetDC(Panel1.Handle));
end;

procedure TForm1.bSaveClick(Sender: TObject);
begin
  FontDisplay.SaveBmp(editFilePath.Text);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FontDisplay := TFontDisplay.Create();
  editFilePath.Text := ExtractFileDir(ParamStr(0));
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FontDisplay.Free();
end;

end.

