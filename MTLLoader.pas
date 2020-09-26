unit MTLLoader;

interface

uses Classes,
  SysUtils,
  StrUtils,
  System.Math.Vectors,
  Generics.Collections,
  Shape,
  Material,
  Math,
  Utils;


function LoadMTL(Stre: TDynStore; fileName: string) : TDictionary<string, TMaterial>;

implementation

function LoadMTL(Stre: TDynStore; fileName: string) : TDictionary<string, TMaterial>;
var
  TxtF: TextFile;
  SepL: TStringList;
  SIdx: Cardinal;
  Line: string;
  CMnm: string;
  CMtl: TMaterial;
begin
  AssignFile(TxtF, fileName);
  begin
    try
      Reset(TxtF);
    except
      raise Exception.Create('File not found: ' + fileName);
    end;
  end;

  // Initilize Objects
  SepL := TStringList.Create;
  CMnm := '';

  // Scan the file
  while not Eof(TxtF) do
  begin
    Readln(TxtF, Line);
    Split(' ', Line, SepL);
    Result := TDictionary<string,TMaterial>.Create;

    case IndexStr(SepL[0], ['newmtl', 'Kd', 'reflectiveness']) of
      0:
        begin
          if CMnm <> '' then
          begin
             Result.Add(CMnm, CMtl);
          end;

          CMtl := TMaterial.Create(TVector.Zero, 0);
          CMnm := SepL[1];
        end;
      1:
        begin
          CMtl.color := 255 * ParsVec3(SepL[1], SepL[2], SepL[3]);
        end;
      2:
        begin
          CMtl.reflective := StrToFloat(SepL[1]);
        end;
    end;
  end;

  if CMnm <> '' then
  begin
    Result.Add(CMnm, CMtl);
  end;

  CloseFile(TxtF);
end;

end.
