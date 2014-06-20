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
    Label1: TLabel;
    Label2: TLabel;
    labelSize: TLabel;
    mSymbols: TMemo;
    OpenDialog: TOpenDialog;
    Panel1: TPanel;
    procedure bChooseFontClick(Sender: TObject);
    procedure bGenerateClick(Sender: TObject);
    procedure bLoadFontClick(Sender: TObject);
    procedure bSaveClick(Sender: TObject);
    procedure editFontNameChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FontGen: TFontGenerator;
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
  s: TStream;
(*  p_in, p_out: Pointer;
  size: LongInt;
  f: File;
*)
begin
  if (Trim(editFontName.Text) = '') then
  begin
    ShowMessage('Font name is empty');
    Exit;
  end;

  FontGen.GenFree();
  FontGen.ClearChars();
  FontGen.GenInit(editFontName.Text, editSize.Value, cbBold.Checked, cbItalic.Checked);
  FontGen.AddChars(UTF8Decode(mSymbols.Lines.Text));
  FontGen.PackChars();
  labelSize.Caption := 'Size: ' + IntToStr(FontGen.TexWidth) + 'x' + IntToStr(FontGen.TexHeight);
  g := Graphics.TBitmap.Create();
  s := FontGen.SaveBmpToStream();
  s.Position := 0;
  g.LoadFromStream(s, s.Size);
  Panel1.Canvas.Brush.Color := clDefault;
  Panel1.Canvas.Clear();
  Panel1.Canvas.Brush.Color := clBlack;
  Panel1.Canvas.FillRect(0, 0, FontGen.TexWidth, FontGen.TexHeight);
  Panel1.Canvas.Draw(0, 0, g);

(*
  //compress
  GetMem(p_in, s.Size);
  s.Position := 0;
  s.Read(p_in^, s.Size);
  CompressData(p_in, s.Size, p_out, size);

  AssignFile(f, editFilePath.Text + '.fnt');
  Rewrite(f, 1);
  BlockWrite(f, p_out^, size);
  CloseFile(f);

  FreeMem(p_in);
  FreeMem(p_out);
*)

  g.Free();
  s.Free();
end;

function AddFontResourceEx(name: LPCSTR; fl: LongWord; pdv: Pointer): longint; stdcall; external 'gdi32' name 'AddFontResourceExA';

procedure TForm1.bLoadFontClick(Sender: TObject);
var
  p: PChar;
begin
  if (OpenDialog.Execute) then
  begin
    p := PChar(AnsiString(OpenDialog.FileName));
    if (AddFontResourceEx(p, 16, nil) = 0) then
      ShowMessage('Font loading failed!')
    else
    begin
      editFontName.Text := ExtractFileNameOnly(OpenDialog.FileName);
      ShowMessage('Font loading successful. Do not forget to edit font name. Generator specify file name as font name');
    end;
  end;
end;

procedure TForm1.bSaveClick(Sender: TObject);
(*
var
  f: File;
  p_in, p_out: Pointer;
  size: LongInt;  *)
begin
  FontGen.SaveBmpToFile(editFilePath.Text);

(*
  //decompress and save
  AssignFile(f, editFilePath.Text + '.fnt');
  Reset(f, 1);
  size := FileSize(editFilePath.Text + '.fnt');
  GetMem(p_in, size);
  GetMem(p_out, size * 100);
  BlockRead(f, p_in^, size);
  CloseFile(f);

  DecompressData(p_in, size, p_out, size);

  AssignFile(f, editFilePath.Text + '1.bmp');
  Rewrite(f, 1);
  BlockWrite(f, p_out^, size);
  CloseFile(f);

  FreeMem(p_in);
  FreeMem(p_out);
*)
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
  FontGen := TFontGenerator.Create();
  editFilePath.Text := ExtractFileDir(ParamStr(0)) + '\';
  editFontName.Text := 'Arial';
  cbBold.Checked := True;
  cbItalic.Checked := False;
  editSize.Value := 12;
  OpenDialog.InitialDir := ExtractFileDir(ParamStr(0)) + '\';
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FontGen.Free();
end;

end.

