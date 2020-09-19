unit Shape;

interface

uses Vcl.Graphics, System.Math.Vectors, Ray, Material, Nullable, Intersection,
  System.Math;

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
    rad: Single;
    radSqr: Single;
    constructor Create(pos: TVector; rad: Single; mat: TMaterial);
    function Intersect(Ray: TRay): TNullable<TIntersection>; override;
  end;

type
  TPlane = class(TShape)
  public
    norm: TVector;
    constructor Create(norm: TVector; dist: Single; mat: TMaterial);
    // Distance from the origin
    function Intersect(Ray: TRay): TNullable<TIntersection>; override;
  end;

type
  TTriangle = class(TShape)
  public
    norm: TVector;
    pos2: TVector;
    pos3: TVector;
    constructor Create(pos, pos2, pos3: TVector; mat: TMaterial);
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
  Result.Value.point := Ray.org + Result.Value.d * Ray.dir;
  Result.Value.normal := (Result.Value.point - pos).Normalize;
end;

{ TPlane }

constructor TPlane.Create(norm: TVector; dist: Single; mat: TMaterial);
begin
  Self.mat := mat;
  Self.norm := norm;
  Self.pos := dist * norm;
end;

function TPlane.Intersect(Ray: TRay): TNullable<TIntersection>;
var
  cosi, dist: Single;
begin
  Result := Default (TNullable<TIntersection>);

  cosi := Ray.dir.DotProduct(norm);
  dist := (pos - Ray.org).DotProduct(norm) / cosi;

  if (dist < 0) or (dist = 0) then
    Exit;

  Result.Value := TIntersection.Create();
  Result.Value.d := dist;
  Result.Value.mat := mat;
  Result.Value.point := Ray.org + dist * Ray.dir;
  Result.Value.normal := norm * -Sign(cosi);;
end;

{ TTraingle }

constructor TTriangle.Create(pos, pos2, pos3: TVector; mat: TMaterial);
begin
  Self.pos := pos;
  Self.pos2 := pos2;
  Self.pos3 := pos3;
  Self.mat := mat;
  Self.norm := (pos2 - pos).CrossProduct(pos3 - pos2).Normalize;
end;

function TTriangle.Intersect(Ray: TRay): TNullable<TIntersection>;
var
  pHit: TVector;
  cosi, dist: Single;
begin
  Result := Default (TNullable<TIntersection>);

  cosi := Ray.dir.DotProduct(norm);
  dist := (pos - Ray.org).DotProduct(norm) / cosi;

  // Dit not hit plane traingle lies in
  if (dist < 0) or (cosi = 0) then
    Exit;

  pHit := Ray.org + dist * Ray.dir;
  if (norm.DotProduct((pos2 - pos).CrossProduct(pHit - pos)) <= 0) then
    Exit;
  if (norm.DotProduct((pos3 - pos2).CrossProduct(pHit - pos2)) <= 0) then
    Exit;
  if (norm.DotProduct((pos - pos3).CrossProduct(pHit - pos3)) <= 0) then
    Exit;

  Result.Value := TIntersection.Create();
  Result.Value.d := dist;
  Result.Value.mat := mat;
  Result.Value.point := Ray.org + dist * Ray.dir;
  Result.Value.normal := norm * -Sign(cosi);;
end;

end.
