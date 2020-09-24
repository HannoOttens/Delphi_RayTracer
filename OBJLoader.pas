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

function Rebound(Stre: TDynStore; Shps: TList<Cardinal>): TBounds;
function SubDevide(Stre: TDynStore; DvOn: Word; const IMin, IMax: Cardinal;
  Shps: TList<Cardinal>): Cardinal;
function LoadOBJ(Stre: TDynStore; fileName: string; const ofst: TVector): Cardinal;

implementation

{ TOBJFile }

constructor TOBJFile.Create;
begin
  Self.v := TList<TVector>.Create;
  Self.vn := TList<TVector>.Create;
end;

function Rebound(Stre: TDynStore; Shps: TList<Cardinal>): TBounds;
var
  PMin: TVector;
  PMax: TVector;
  Indx: Cardinal;
  Bnds: TBounds;
begin
  // Initialize Min/Max values
  PMin := TVector.Create(10000000, 10000000, 10000000);
  PMax := TVector.Create(-10000000, -10000000, -10000000);

  // Calc the bound
  Indx := 0;
  while Indx < Shps.Count do
  begin
    Bnds := Bound(Stre, Stre.Shps[Shps[Indx]]);
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
function SubDevide(Stre: TDynStore; DvOn: Word; const IMin, IMax: Cardinal;
  Shps: TList<Cardinal>): Cardinal;
var
  PMdn, PMdx, PMin, PMax: TVector;
  ShpL, ShpR: TList<Cardinal>;
  IdxL, IdxR: Cardinal;
  Indx: Cardinal;
  Bnds: TBounds;
begin
  // Base cases
  if Shps.Count = 1 then
  begin
    Result := Shps[0];
    Exit;
  end;
  if Shps.Count < 8 then
  begin
    Result := Stre.Shps.Add(AabbCreate(IMin, IMax, Shps.ToArray));
    Exit;
  end;

  // Otherwise split on an axis (DvOn: 0 -> X, 1 -> Y, 2 -> Z)
  PMin := Stre.VPos[IMin];
  PMax := Stre.VPos[IMax];
  PMdn := PMax;
  case DvOn of
    0:
      PMdn.X := (PMin.X + PMax.X) * 0.5;
    1:
      PMdn.Y := (PMin.Y + PMax.Y) * 0.5;
    2:
      PMdn.W := (PMin.W + PMax.W) * 0.5;
  end;
  PMdx := PMin;
  case DvOn of
    0:
      PMdx.X := (PMin.X + PMax.X) * 0.5;
    1:
      PMdx.Y := (PMin.Y + PMax.Y) * 0.5;
    2:
      PMdx.W := (PMin.W + PMax.W) * 0.5;
  end;

  // Divide over the two lists
  ShpL := TList<Cardinal>.Create;
  ShpR := TList<Cardinal>.Create;
  Indx := 0;
  while Indx < Shps.Count do
  begin
    Bnds := Bound(Stre, Stre.Shps[Shps[Indx]]);
    if ((Bnds.PMax.X < PMdn.X) or (DvOn <> 0)) and
      ((Bnds.PMax.Y < PMdn.Y) or (DvOn <> 1)) and
      ((Bnds.PMax.W < PMdn.W) or (DvOn <> 2)) then
    begin
      ShpL.Add(Shps[Indx]);
    end
    else
    begin
      ShpR.Add(Shps[Indx]);
    end;
    Indx := Indx + 1;
  end;

  // Free memory of shps array
  Shps.Destroy;

  if ShpL.Count = 0 then
  begin
    Bnds := Rebound(Stre, ShpR);
    Result := Stre.Shps.Add(AabbCreate(Stre.VPos.Add(Bnds.PMin), IMax, ShpR.ToArray));
    Exit;
  end
  else if ShpR.Count = 0 then
  begin
    Result := Stre.Shps.Add(AabbCreate(IMin, Stre.VPos.Add(PMdn), ShpL.ToArray));
    Exit;
  end
  else
  begin
    // Add new points to store & recurse
    DvOn := (DvOn + 1) mod 3;
    IdxL := SubDevide(Stre, DvOn, IMin, Stre.VPos.Add(PMdn), ShpL);
    Bnds := Rebound(Stre, ShpR);
    IdxR := SubDevide(Stre, DvOn, Stre.VPos.Add(Bnds.PMin), IMax, ShpR);

    // Add sub-shapes to object
    Result := Stre.Shps.Add(AabbCreate(IMin, IMax, [IdxL, IdxR]));
  end;
end;

{ OBJ File Loader }

procedure Split(Delimiter: Char; Str: string; ListOfStrings: TStrings);
begin
  ListOfStrings.Clear;
  ListOfStrings.Delimiter := Delimiter;
  ListOfStrings.StrictDelimiter := True;
  ListOfStrings.DelimitedText := Str;
end;

function ParsVec3(const ofst: TVector; X, Y, z: string): TVector;
begin
  Result := TVector.Create(StrToFloat(X), StrToFloat(Y), StrToFloat(z)) + ofst;
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

function ParsTria(Stre: TDynStore; ObjF: TOBJFile;
  pos1, pos2, pos3: string): TShape;
var
  IPs1, IPs2, IPs3: Cardinal;
begin
  IPs1 := Stre.VPos.Add(ParsFIdx(ObjF, pos1));
  IPs2 := Stre.VPos.Add(ParsFIdx(ObjF, pos2));
  IPs3 := Stre.VPos.Add(ParsFIdx(ObjF, pos3));

  Result := TriaCreate(IPs1, IPs2, IPs3, (Stre.VPos[IPs2] - Stre.VPos[IPs1])
    .CrossProduct(Stre.VPos[IPs3] - Stre.VPos[IPs2]),
    TMaterial.Create(TVector.Create(244, 244, 244), 0));
end;

function LoadOBJ(Stre: TDynStore; fileName: string; const ofst: TVector): Cardinal;
var
  PMin, PMax: TVector;
  Vec3: TVector;
  TxtF: TextFile;
  SepL: TStringList;
  ObjF: TOBJFile;
  Shps: TList<Cardinal>;
  SIdx: Cardinal;
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
  PMin := TVector.Create(10000000, 10000000, 10000000);
  PMax := TVector.Create(-10000000, -10000000, -10000000);

  // Initilize Objects
  SepL := TStringList.Create;
  Shps := TList<Cardinal>.Create;
  ObjF := TOBJFile.Create;

  // Scan the file
  while not Eof(TxtF) do
  begin
    Readln(TxtF, Line);
    Split(' ', Line, SepL);

    // Skip lines and polys larger than 4
    if (SepL.Count < 4) or (SepL.Count > 6) then
      Continue;

    case IndexStr(SepL[0], ['v', 'vn', 'f', '#']) of
      0:
        begin
          Vec3 := ParsVec3(ofst, SepL[1], SepL[2], SepL[3]);
          ObjF.v.Add(Vec3);

          // BB scaling
          PMin.X := Min(PMin.X, Vec3.X);
          PMin.Y := Min(PMin.Y, Vec3.Y);
          PMin.W := Min(PMin.W, Vec3.W);
          PMax.X := Max(PMax.X, Vec3.X);
          PMax.Y := Max(PMax.Y, Vec3.Y);
          PMax.W := Max(PMax.W, Vec3.W);
        end;
      // 1: ObjF.vn.Add(ParsVec3(TVector.Zero, SepL[1], SepL[2], SepL[3]));
      2:
        begin
          if SepL[3] = '' then
            Continue;
          SIdx := Stre.Shps.Add(ParsTria(Stre, ObjF, SepL[1], SepL[2],
            SepL[3]));
          Shps.Add(SIdx);

          // Parse 2nd triangle from a quad
          if SepL.Count = 5 then
          begin
            if SepL[4] = '' then
              Continue;
            SIdx := Stre.Shps.Add(ParsTria(Stre, ObjF, SepL[3], SepL[4],
              SepL[1]));
            Shps.Add(SIdx);
          end;
        end;
      3:
        Continue;
    end;
  end;

  Result := SubDevide(Stre, 0, Stre.VPos.Add(PMin), Stre.VPos.Add(PMax), Shps);
  ObjF.Destroy;
  CloseFile(TxtF);
end;

end.
