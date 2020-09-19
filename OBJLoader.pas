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
    v:  TList<TVector>;
    vn: TList<TVector>;
    constructor Create();
  end;

function LoadOBJ(fileName: string; ofst: TVector): TList<TShape>;

implementation

{ TOBJFile }

constructor TOBJFile.Create;
begin
  Self.v  := TList<TVector>.Create;
  Self.vn := TList<TVector>.Create;
end;

{ OBJ File Loader }

procedure Split(Delimiter: Char; Str: string; ListOfStrings: TStrings);
begin
  ListOfStrings.Clear;
  ListOfStrings.Delimiter := Delimiter;
  ListOfStrings.StrictDelimiter := True;
  ListOfStrings.DelimitedText := Str;
end;

function ParsVec3(ofst: TVector; x, y, z: string): TVector;
begin
  Result := TVector.Create(StrToFloat(x), StrToFloat(z), StrToFloat(y)) + ofst;
end;

function ParsFIdx(ObjF: TOBJFile; p: string): TVector;
var SepL: TStringList;
    vIdx: Integer;
begin
  SepL := TStringList.Create;
  Split('/', p, SepL);

  vIdx := StrToInt(SepL[0]) - 1;
  Result := ObjF.v[vIdx];
end;

function ParsTria(ObjF: TOBJFile; pos1, pos2, pos3: string): TTriangle;
begin
  Result := TTriangle.Create(
    ParsFIdx(ObjF, pos1),
    ParsFIdx(ObjF, pos2),
    ParsFIdx(ObjF, pos3),
    TMaterial.Create(TVector.Create(244,244,244), 0)
  );
end;

function LoadOBJ(fileName: string; ofst: TVector): TList<TShape>;
var
  TxtF: TextFile;
  Line: string;
  SepL: TStringList;
  ObjF: TOBJFile;
begin
  AssignFile(TxtF, fileName);
  begin
    try
      Reset(TxtF);
    except
      raise Exception.Create('File not found: ' + fileName);
    end;
  end;

  SepL := TStringList.Create;
  Result := TList<TShape>.Create;
  ObjF := TOBJFile.Create;

  while not Eof(TxtF) do
  begin
    Readln(TxtF, Line);
    Split(' ', Line, SepL);

    case IndexStr(SepL[0], ['v', 'f']) of
      0: ObjF.v.Add(ParsVec3(ofst, SepL[1], SepL[2], SepL[3]));
      // 1: ObjF.vn.Add(ParsVec3(SepL[1], SepL[2], SepL[3]));
      1: begin
        Result.Add(ParsTria(ObjF, SepL[1], SepL[2], SepL[3]));
      end;
    end;
  end;

  CloseFile(TxtF);
end;

end.
