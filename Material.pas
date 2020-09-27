unit Material;

interface

uses System.Math.Vectors, Vcl.Graphics;

type
  TMaterial = record
  public
    color: TVector;
    reflective: Single;
    opaqueness: Single;
    refractive: Single;
    constructor Create(color: TVector;
      reflective, opaqueness, refractive: Single);
  end;

implementation

{ TMaterial }

constructor TMaterial.Create(color: TVector;
  reflective, opaqueness, refractive: Single);
begin
  Self.color := color;
  Self.reflective := reflective;
  Self.opaqueness := opaqueness;
  Self.refractive := refractive;
end;

end.
