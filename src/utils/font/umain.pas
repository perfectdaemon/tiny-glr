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
    procedure editFontNameChange(Sender: TObject);
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
var
  g: Graphics.TBitmap;
begin
  FontDisplay.GenFree();
  FontDisplay.ClearChars();
  FontDisplay.GenInit(editFontName.Text, editSize.Value, cbBold.Checked, cbItalic.Checked);
  FontDisplay.AddChars(UTF8Decode(mSymbols.Lines.Text));
  FontDisplay.PackChars();
  //todo: draw bitmap to panel
  g := Graphics.TBitmap.Create();
  FontDisplay.SaveBmp(editFilePath.Text);
  g.LoadFromFile(editFilePath.Text + '.bmp');
  Panel1.Canvas.Brush.Color := clBlack;
  Panel1.Canvas.FillRect(0, 0, Panel1.Width, Panel1.Height);
  Panel1.Canvas.Draw(0, 0, g);
  g.Free();
end;

procedure TForm1.editFontNameChange(Sender: TObject);
var
  name, path: String;
begin
  name := ExtractFileName(editFilePath.Text);
  path := ExtractFilePath(editFilePath.Text);

  name := editFontName.Text + IntToStr(editSize.Value);
  if (cbBold.Checked) then
    name += 'b';
  if (cbItalic.Checked) then
    name += 'i';
  editFilePath.Text := path + name;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FontDisplay := TFontDisplay.Create();
  editFilePath.Text := ExtractFileDir(ParamStr(0)) + '\';
  editFontName.Text := 'Arial';
  cbBold.Checked := True;
  cbItalic.Checked := False;
  editSize.Value := 12;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FontDisplay.Free();
end;

end.

