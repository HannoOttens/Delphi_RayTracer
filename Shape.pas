unit Shape;

interface

uses Vcl.Graphics, System.Math.Vectors, Ray, Material, Nullable, Intersection,
  System.Math, Generics.Collections, SysUtils;

type
  TBounds = record
    PMin, PMax: TVector;
    constructor Create(PMin, PMax: TVector);
  end;

type
  TShape = class abstract
  public
    pos: TVector;
    mat: TMaterial;
    /// <summary>Find the intersection with a ray</summary>
    /// <returns>A non-null point after intersection</returns>
    function Intersect(Ray: TRay): TNullable<TIntersection>; virtual; abstract;
    /// <summary>Calculate the bounds of this shape</summary>
    /// <returns>The object bounds</returns>
    function Bound(): TBounds; virtual; abstract;
    // TODO: Only return PMAX, we do not need PMin for splitting the bounds
  end;

type
  TShpere = class(TShape)
  public
    rad: Single;
    radSqr: Single;
    constructor Create(pos: TVector; rad: Single; mat: TMaterial);
    function Intersect(Ray: TRay): TNullable<TIntersection>; override;
    function Bound(): TBounds; override;
  end;

type
  TPlane = class(TShape)
  public
    norm: TVector;
    constructor Create(norm: TVector; dist: Single; mat: TMaterial);
    function Intersect(Ray: TRay): TNullable<TIntersection>; override;
    function Bound(): TBounds; override;
  end;

type
  TTriangle = class(TShape)
  public
    norm: TVector;
    pos2: TVector;
    pos3: TVector;
    constructor Create(pos, pos2, pos3: TVector; mat: TMaterial);
    function Intersect(Ray: TRay): TNullable<TIntersection>; override;
    function Bound(): TBounds; override;
  end;

type
  TOBJ = class(TShape)
  public
    shps: TList<TShape>;
    PMin, PMax: TVector;
    constructor Create(PMin, PMax: TVector; shps: TList<TShape>);
    function Intersect(Ray: TRay): TNullable<TIntersection>; override;
    function Bound(): TBounds; override;
  end;

implementation

{ TShpere }

function TShpere.Bound: TBounds;
begin
  Result := TBounds.Create(pos + TVector.Create(rad, rad, rad),
    pos - TVector.Create(rad, rad, rad));
end;

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

function TPlane.Bound: TBounds;
begin
  raise Exception.Create('A plane does not have a finite bounding box');
end;

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

function TTriangle.Bound: TBounds;
begin
  Result := TBounds.Create(
    TVector.Create(
       Min(pos.X, Min(pos2.X, pos3.X)),
       Min(pos.Y, Min(pos2.Y, pos3.Y)),
       Min(pos.W, Min(pos2.W, pos3.W))
    ),
    TVector.Create(
       Max(pos.X, Max(pos2.X, pos3.X)),
       Max(pos.Y, Max(pos2.Y, pos3.Y)),
       Max(pos.W, Max(pos2.W, pos3.W))
    ) 
  );
end;

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

{ TOBJ }

function TOBJ.Bound: TBounds;
begin
  Result := TBounds.Create(PMin, PMax);
end;

constructor TOBJ.Create(PMin, PMax: TVector; shps: TList<TShape>);
begin
  Self.PMin := PMin;
  Self.PMax := PMax;
  Self.shps := shps;
end;

function TOBJ.Intersect(Ray: TRay): TNullable<TIntersection>;
var
  VMin, VMax: Single;
  TMin, TMax: Single;
  d: Single;
  intsct: TNullable<TIntersection>;
  shpe: TShape;
  indx: Word;
begin
  Result := Default (TNullable<TIntersection>);

  VMin := (PMin.X - Ray.org.X) * Ray.invDir.X;
  VMax := (PMax.X - Ray.org.X) * Ray.invDir.X;
  TMin := Min(VMin, VMax);
  TMax := Max(VMin, VMax);

  if (TMax < TMin) or (TMax < 0) then
    Exit;

  VMin := (PMin.Y - Ray.org.Y) * Ray.invDir.Y;
  VMax := (PMax.Y - Ray.org.Y) * Ray.invDir.Y;
  TMin := Max(TMin, Min(VMin, VMax));
  TMax := Min(TMax, Max(VMin, VMax));

  if (TMax < TMin) or (TMax < 0) then
    Exit;

  VMin := (PMin.W - Ray.org.W) * Ray.invDir.W;
  VMax := (PMax.W - Ray.org.W) * Ray.invDir.W;
  TMin := Max(TMin, Min(VMin, VMax));
  TMax := Min(TMax, Max(VMin, VMax));

  if (TMax < TMin) or (TMax < 0) then
    Exit;

  // TODO: Set to Single.MaxValue
  d := 100000000;
  indx := 0;
  while indx < shps.Count do
  begin
    shpe := shps[indx];
    intsct := shpe.Intersect(Ray);
    if intsct.HasValue and (intsct.Value.d < d) then
    begin
      d := intsct.Value.d;
      Result := intsct;
    end;

    indx := indx + 1;
  end;
end;

{ TBounds }

constructor TBounds.Create(PMin, PMax: TVector);
begin
  Self.PMin := PMin;
  Self.PMax := PMax;
end;

end.
