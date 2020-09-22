unit OBJLoader;

interface

uses
  Classes,
  SysUtils,
  StrUtils,
  System.Math.Vectors,
  Generics.Collections,
  Shape,
  Material,
  Math;

type
  TOBJFile = class
  public
    v: TList<TVector>;
    vn: TList<TVector>;
    constructor Create();
  end;

function Rebound(Shps: TList<TShape>): TBounds;
function LoadOBJ(fileName: string; ofst: TVector): TShape;
function SubDevide(DvOn: Word; const PMin, PMax: TVector; shps: TList<TShape>): TShape;

implementation

{ TOBJFile }

constructor TOBJFile.Create;
begin
  Self.v := TList<TVector>.Create;
  Self.vn := TList<TVector>.Create;
end;

function Rebound(Shps: TList<TShape>): TBounds;
var
  PMin: TVector;
  PMax: TVector;
  Indx: Word;
  Bnds: TBounds;
begin
  // Initialize Min/Max values
  PMin := TVector.Create(10000000, 10000000, 10000000);
  PMax := TVector.Create(-10000000, -10000000, -10000000);

  // Calc the bound
  Indx := 0;
  while Indx < Shps.Count do
  begin
    Bnds := Shps[Indx].Bound;
    PMin.X := Min(PMin.X, Bnds.PMin.X);
    PMin.Y := Min(PMin.Y, Bnds.PMin.Y);
    PMin.W := Min(PMin.W, Bnds.PMin.W);
    PMax.X := Max(PMax.X, Bnds.PMax.X);
    PMax.Y := Max(PMax.Y, Bnds.PMax.Y);
    PMax.W := Max(PMax.W, Bnds.PMax.W);

    Indx := Indx + 1;
  end;

  Result := TBounds.Create(PMin, PMax);
end;

{ Subdeviding an OBJ }
function SubDevide(DvOn: Word; const PMin, PMax: TVector; shps: TList<TShape>): TShape;
var PMdn, PMdx: TVector;
    ShpL, ShpR, ShpN: TList<TShape>;
    ObjL, ObjR: TShape;
    Indx: Word;
    Bund: TBounds;
begin
  // Group objects per 8
  if shps.Count < 4 then
  begin
    Result := AabbCreate(PMin, PMax, shps);
    Exit;
  end;

  // Otherwise split on an axis (DvOn: 0 -> X, 1 -> Y, 2 -> Z)
  PMdn := PMax;
  case DvOn of
    0: PMdn.X := (PMin.X + PMax.X) * 0.5;
    1: PMdn.Y := (PMin.Y + PMax.Y) * 0.5;
    2: PMdn.W := (PMin.W + PMax.W) * 0.5;
  end;
  PMdx := PMin;
  case DvOn of
    0: PMdx.X := (PMin.X + PMax.X) * 0.5;
    1: PMdx.Y := (PMin.Y + PMax.Y) * 0.5;
    2: PMdx.W := (PMin.W + PMax.W) * 0.5;
  end;

  // Divide over the two lists
  ShpL := TList<TShape>.Create;
  ShpR := TList<TShape>.Create;
  Indx := 0;
  while Indx < shps.Count do
  begin
    Bund := shps[Indx].Bound;
    if ((Bund.PMax.X < PMdn.X) or (DvOn <> 0)) and
       ((Bund.PMax.Y < PMdn.Y) or (DvOn <> 1)) and
       ((Bund.PMax.W < PMdn.W) or (DvOn <> 2)) then
    begin
      ShpL.Add(shps[Indx]);
    end
    else
    begin
      ShpR.Add(shps[Indx]);
    end;

    Indx := Indx + 1;
  end;

  if ShpL.Count = 0 then
  begin
    Result := AabbCreate(PMin, PMax, ShpR);
    Exit;
  end;
  if ShpR.Count = 0 then
  begin
    Result := AabbCreate(PMin, PMax, ShpL);
    Exit;
  end;

  // Recurse
  DvOn := (DvOn + 1) mod 3;
  ObjL := SubDevide(DvOn, PMin, PMdn, ShpL);
  Bund := Rebound(ShpR);
  ObjR := SubDevide(DvOn, Bund.PMin, Bund.PMax, ShpR);

  // Add sub-shapes to object
  ShpN := TList<TShape>.Create;
  ShpN.Add(ObjL);
  ShpN.Add(ObjR);
  Result := AabbCreate(PMin, PMax, ShpN);
end;

{ OBJ File Loader }

procedure Split(Delimiter: Char; Str: string; ListOfStrings: TStrings);
begin
  ListOfStrings.Clear;
  ListOfStrings.Delimiter := Delimiter;
  ListOfStrings.StrictDelimiter := True;
  ListOfStrings.DelimitedText := Str;
end;

function ParsVec3(const ofst: TVector; x, y, z: string): TVector;
begin
  Result := TVector.Create(StrToFloat(x), StrToFloat(z), StrToFloat(y)) + ofst;
end;

function ParsFIdx(ObjF: TOBJFile; p: string): TVector;
var
  SepL: TStringList;
  vIdx: Integer;
begin
  SepL := TStringList.Create;
  Split('/', p, SepL);

  vIdx := StrToInt(SepL[0]) - 1;
  Result := ObjF.v[vIdx];
end;

function ParsTria(ObjF: TOBJFile; pos1, pos2, pos3: string): TShape;
begin
  Result := TriaCreate(ParsFIdx(ObjF, pos1), ParsFIdx(ObjF, pos2),
    ParsFIdx(ObjF, pos3), TMaterial.Create(TVector.Create(244, 244, 244), 0));
end;

function LoadOBJ(fileName: string; ofst: TVector): TShape;
var
  PMin, PMax: TVector;
  Vec3: TVector;
  TxtF: TextFile;
  SepL: TStringList;
  ObjF: TOBJFile;
  shps: TList<TShape>;
  Line: string;
begin
  AssignFile(TxtF, fileName);
  begin
    try
      Reset(TxtF);
    except
      raise Exception.Create('File not found: ' + fileName);
    end;
  end;
  // Initialize Min/Max values
  PMin := TVector.Create(10000000,10000000,10000000);
  PMax := TVector.Create(-10000000,-10000000,-10000000);

  // Initilize Objects
  SepL := TStringList.Create;
  shps := TList<TShape>.Create;
  ObjF := TOBJFile.Create;

  // Scan the file
  while not Eof(TxtF) do
  begin
    Readln(TxtF, Line);
    Split(' ', Line, SepL);

    // Skip lines and polys larger than 4
    if (SepL.Count < 4) or (SepL.Count > 6)  then Continue;

    case IndexStr(SepL[0], ['v', 'vn', 'f', '#']) of
      0:
        begin
          Vec3 := ParsVec3(ofst, SepL[1], SepL[2], SepL[3]);
          ObjF.v.Add(Vec3);

          // BB scaling
          PMin.X := Min(PMin.X, Vec3.x);
          PMin.Y := Min(PMin.Y, Vec3.y);
          PMin.W := Min(PMin.W, Vec3.w);
          PMax.X := Max(PMax.X, Vec3.x);
          PMax.Y := Max(PMax.Y, Vec3.y);
          PMax.W := Max(PMax.W, Vec3.w);
        end;
       //1: ObjF.vn.Add(ParsVec3(TVector.Zero, SepL[1], SepL[2], SepL[3]));
       2:
        begin
          if SepL[3] = '' then Continue;

          shps.Add(ParsTria(ObjF, SepL[1], SepL[2], SepL[3]));
          if SepL.Count = 5 then begin
            if SepL[4] = '' then Continue;
            shps.Add(ParsTria(ObjF, SepL[2], SepL[3], SepL[4]));
          end;
        end;
       3: Continue;
    end;
  end;

  // Add BB information
  Result := SubDevide(0, PMin, PMax, shps);

  OBJF.Destroy;
  CloseFile(TxtF);
end;

end.
