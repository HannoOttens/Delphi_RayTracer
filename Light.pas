unit Light;

interface

uses Vcl.Graphics, System.Math.Vectors;

type
  TLight = class
    pos, lum: TVector;
    pow: Integer;
    constructor Create(pos, lum: TVector; pow: Integer);
  end;

implementation

{ TLight }

constructor TLight.Create(pos, lum: TVector; pow: Integer);
begin
  Self.pos := pos;
  Self.lum := lum;
  Self.pow := pow;
end;

end.
