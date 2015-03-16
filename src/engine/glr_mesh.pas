unit glr_mesh;

{$i defines.inc}

interface

uses
  glr_render, glr_scene, glr_utils;

  {$REGION ' .OBJ mesh file format specific types '}
type
  TglrObjFace = record
    v, vt, vn: array[0..2] of LongWord;
  end;
  TglrObjFaceList = TglrList<TglrObjFace>;

  TglrObjIndex = record
    v, vn, vt: LongWord;
  end;
  TglrObjIndexList = TglrList<TglrObjIndex>;

  TglrObjRawSubMeshData = class
    Name: AnsiString;
    f: TglrObjFaceList;
    constructor Create(); virtual;
    destructor Destroy(); override;
  end;

  TglrObjRawSubMeshDataList = TglrObjectList<TglrObjRawSubMeshData>;

  TglrObjRawMeshData = class
  public
    v, vn: TglrVec3fList;
    vt: TglrVec2fList;
    subs: TglrObjRawSubMeshDataList;
    constructor Create(); virtual;
    destructor Destroy(); override;
  end;

  { TglrMeshData }

  {$ENDREGION}

  {$REGION ' Mesh '}
  TglrSubMeshData = record
    name: AnsiString;
    start, count: LongWord;
  end;

  TglrMeshData = record
    vBuffer: TglrVertexBuffer;
    iBuffer: TglrIndexBuffer;
    vData, iData: Pointer;
    vLength, iLength: LongWord;
    subMeshes: array of TglrSubMeshData;
    procedure FreeMemory();
  end;

  // RawGlr - Internal framework format
  // Obj    - Wavefront .obj format
  TglrMeshFormat = (mfRawGlr, mfObj);

  TglrMesh = class (TglrNode)
  public
    Data: TglrMeshData;
    constructor Create(aStream: TglrStream; aFormat: TglrMeshFormat;
      aFreeStreamOnFinish: Boolean = True); virtual; overload;
    constructor Create(); override; overload;
    destructor Destroy(); override;
  end;

  {$ENDREGION}


implementation

uses
  glr_resload;

{ TglrObjRawSubMeshData }

constructor TglrObjRawSubMeshData.Create;
begin
  inherited;
  Name := '';
  f := TglrObjFaceList.Create(32);
end;

destructor TglrObjRawSubMeshData.Destroy;
begin
  f.Free();
  inherited Destroy;
end;

{ TglrObjRawMeshData }

constructor TglrObjRawMeshData.Create;
begin
  inherited;
  v := TglrVec3fList.Create(32);
  vn := TglrVec3fList.Create(32);
  vt := TglrVec2fList.Create(32);
  subs := TglrObjRawSubMeshDataList.Create(1);
end;

destructor TglrObjRawMeshData.Destroy;
begin
  v.Free();
  vn.Free();
  vt.Free();
  inherited Destroy;
end;

{ TglrMeshData }

procedure TglrMeshData.FreeMemory;
begin
  if (vData <> nil) then
    FreeMem(vData);
  if (iData <> nil) then
    FreeMem(iData);
  vData := nil;
  iData := nil;
end;

{ TglrMesh }

constructor TglrMesh.Create(aStream: TglrStream; aFormat: TglrMeshFormat;
  aFreeStreamOnFinish: Boolean);
begin
  inherited Create();

  Data := LoadMesh(aStream, aFormat);

  if aFreeStreamOnFinish then
    aStream.Free();
end;

constructor TglrMesh.Create;
begin
  inherited Create;
  FillChar(Data, SizeOf(TglrMeshData), 0);
end;

destructor TglrMesh.Destroy;
begin
  Data.FreeMemory();
  if Assigned(Data.vBuffer) then
    Data.vBuffer.Free();
  if Assigned(Data.iBuffer) then
    Data.iBuffer.Free();
  inherited Destroy;
end;

end.

