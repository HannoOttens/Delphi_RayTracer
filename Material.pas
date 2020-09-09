unit Material;

interface

uses System.Math.Vectors, Vcl.Graphics;

type
  TMaterial = class
  public
    color: TVector;
    reflective: Single;
    constructor Create(color: TVector; reflective: Single);
  end;

implementation

{ TMaterial }

constructor TMaterial.Create(color: TVector; reflective: Single);
begin
  Self.color := color;
  Self.reflective := reflective;
end;

end.
