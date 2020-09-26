unit Utils;

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

procedure Split(Delimiter: Char; Str: string; ListOfStrings: TStrings);
function ParsVec3(const X, Y, z: string): TVector;

implementation

procedure Split(Delimiter: Char; Str: string; ListOfStrings: TStrings);
begin
  ListOfStrings.Clear;
  ListOfStrings.Delimiter := Delimiter;
  ListOfStrings.StrictDelimiter := True;
  ListOfStrings.DelimitedText := Str;
end;

function ParsVec3(const X, Y, z: string): TVector;
begin
  Result := TVector.Create(StrToFloat(X), StrToFloat(Y), StrToFloat(z));
end;

end.
