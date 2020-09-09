unit Shape;

interface

uses Vcl.Graphics, System.Math.Vectors, Ray, Material, Nullable, Intersection;

type
  TShape = class abstract
  public
    pos: TVector;
    mat: TMaterial;
    /// <summary>Find the intersection with a ray</summary>
    /// <returns>A non-null point after intersection</returns>
    function Intersect(Ray: TRay): TNullable<TIntersection>; virtual; abstract;
  end;

type
  TShpere = class(TShape)
  public
    pos: TVector;
    rad: Single;
    radSqr: Single;
    mat: TMaterial;
    constructor Create(pos: TVector; rad: Single; mat: TMaterial);
    function Intersect(Ray: TRay): TNullable<TIntersection>; override;
  end;

implementation

{ TShpere }

constructor TShpere.Create(pos: TVector; rad: Single; mat: TMaterial);
begin
  Self.pos := pos;
  Self.mat := mat;
  Self.rad := rad;
  Self.radSqr := rad * rad;
end;

function TShpere.Intersect(Ray: TRay): TNullable<TIntersection>;
var
  v: TVector;
  b, det: Single;

begin
  Result := Default (TNullable<TIntersection>);

  v := Ray.org - pos;
  b := -(v.DotProduct(Ray.dir));
  det := (b * b) - v.DotProduct(v) + radSqr;

  // Sphere was not hit
  if det <= 0 then
    Exit;

  det := Sqrt(det);

  // Sphere was not hit
  if b + det <= 0 then
    Exit;

  Result.Value := TIntersection.Create();
  Result.Value.d := b - det;
  Result.Value.mat := Self.mat;
  Result.Value.point := Result.Value.d * Ray.dir;
  Result.Value.normal := (Result.Value.point - pos).Normalize;
end;

end.
