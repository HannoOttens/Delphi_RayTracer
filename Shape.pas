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
  EShape = (Sphr, Plne, Tria, Aabb, ObjB);
  TShape = record
  public
    kind: EShape;
    pos: TVector;
    mat: TMaterial;

    /// <summary>Find the intersection with a ray</summary>
    /// <returns>A non-null point after intersection</returns>
    function Intersect(Ray: TRay; Dist: Single): TNullable<TIntersection>;
    /// <summary>Calculate the bounds of this shape</summary>
    /// <returns>The object bounds</returns>
    // TODO: Only return PMAX, we do not need PMin for splitting the bounds
    function Bound(): TBounds;

    Case EShape of
       Sphr: (rad, radSqr: Single);
       Plne: (pnrm: TVector);
       Tria: (tnrm, pos2, pos3: TVector);
       Aabb: (PMin, PMax: TVector; Shps: TList<TShape>);
    end;

function SphrCreate(pos: TVector; rad: Single; mat: TMaterial): TShape;
function PlneCreate(norm: TVector; dist: Single; mat: TMaterial): TShape;
function TriaCreate(pos, pos2, pos3: TVector; mat: TMaterial): TShape;
function AabbCreate(PMin, PMax: TVector; shps: TList<TShape>): TShape;

implementation

{ TShpere }

function SphrCreate(pos: TVector; rad: Single; mat: TMaterial): TShape;
begin
  Result.kind := EShape.Sphr;
  Result.pos := pos;
  Result.mat := mat;
  Result.rad := rad;
  Result.radSqr := rad * rad;
end;

function SphrBound(Shpe: TShape): TBounds;
begin
  Result := TBounds.Create(
    Shpe.pos - TVector.Create(Shpe.rad, Shpe.rad, Shpe.rad),
    Shpe.pos + TVector.Create(Shpe.rad, Shpe.rad, Shpe.rad));
end;

function SphrIntersect(Shpe: TShape; Ray: TRay; Dist: Single): TNullable<TIntersection>;
var
  v: TVector;
  b, det, d: Single;
begin
  Result := Default (TNullable<TIntersection>);

  v := Ray.org - Shpe.pos;
  b := -(v.DotProduct(Ray.dir));
  det := (b * b) - v.DotProduct(v) + Shpe.radSqr;

  // Sphere was not hit
  if det <= 0 then
    Exit;

  det := Sqrt(det);

  // Sphere was not hit
  d := b - det;
  if d > Dist then
    Exit;
  if b + det <= 0 then
    Exit;

  Result.Value := TIntersection.Create();
  Result.Value.d := b - det;
  Result.Value.mat := Shpe.mat;
  Result.Value.point := Ray.org + Result.Value.d * Ray.dir;
  Result.Value.normal := (Result.Value.point - Shpe.pos).Normalize;
end;

{ TPlane }

function PlneCreate(norm: TVector; dist: Single; mat: TMaterial): TShape;
begin
  Result.kind := EShape.Plne;
  Result.mat := mat;
  Result.pos := dist * norm;
  Result.pnrm := norm.Normalize;
end;

function PlneBound(shpe: TShape): TBounds;
begin
  raise Exception.Create('A plane does not have a finite bounding box');
end;

function PlneIntersect(Shpe: TShape; Ray: TRay; Dist: Single): TNullable<TIntersection>;
var
  cosi, d: Single;
begin
  Result := Default (TNullable<TIntersection>);

  cosi := Ray.dir.DotProduct(Shpe.pnrm);
  d := (Shpe.pos - Ray.org).DotProduct(Shpe.pnrm) / cosi;

  if (d > dist) or (d < 0) or (cosi = 0) then
    Exit;

  Result.Value := TIntersection.Create();
  Result.Value.d := d;
  Result.Value.mat := Shpe.mat;
  Result.Value.point := Ray.org + d * Ray.dir;
  Result.Value.normal := Shpe.pnrm * -Sign(cosi);;
end;

{ TTraingle }

function TriaCreate(pos, pos2, pos3: TVector; mat: TMaterial): TShape;
begin
  Result.kind := EShape.Tria;
  Result.pos := pos;
  Result.pos2 := pos2;
  Result.pos3 := pos3;
  Result.mat := mat;
  Result.tnrm := (pos2 - pos).CrossProduct(pos3 - pos2).Normalize;
end;

function TriaBound(Sphe: TShape): TBounds;
begin
  Result := TBounds.Create(
    TVector.Create(
       Min(Sphe.pos.X, Min(Sphe.pos2.X, Sphe.pos3.X)),
       Min(Sphe.pos.Y, Min(Sphe.pos2.Y, Sphe.pos3.Y)),
       Min(Sphe.pos.W, Min(Sphe.pos2.W, Sphe.pos3.W))
    ),
    TVector.Create(
       Max(Sphe.pos.X, Max(Sphe.pos2.X, Sphe.pos3.X)),
       Max(Sphe.pos.Y, Max(Sphe.pos2.Y, Sphe.pos3.Y)),
       Max(Sphe.pos.W, Max(Sphe.pos2.W, Sphe.pos3.W))
    ) 
  );
end;

function TriaIntersect(Shpe: TShape; Ray: TRay; Dist: Single): TNullable<TIntersection>;
var
  pHit: TVector;
  cosi, d, u, v, w: Single;
begin
  Result := Default (TNullable<TIntersection>);

  cosi := Ray.dir.DotProduct(Shpe.tnrm);
  d := (Shpe.pos - Ray.org).DotProduct(Shpe.tnrm) / cosi;

  // Dit not hit plane traingle lies in
  if (d > Dist) or (d < 0) or (cosi = 0) then
    Exit;

  pHit := Ray.org + d * Ray.dir;
  w := Shpe.tnrm.DotProduct((Shpe.pos2 - Shpe.pos).CrossProduct(pHit - Shpe.pos));
  if (w <= 0) then
    Exit;
  u := Shpe.tnrm.DotProduct((Shpe.pos3 - Shpe.pos2).CrossProduct(pHit - Shpe.pos2));
  if (u <= 0) then
    Exit;
  v := Shpe.tnrm.DotProduct((Shpe.pos - Shpe.pos3).CrossProduct(pHit - Shpe.pos3));
  if (v <= 0) then
    Exit;

  Result.Value := TIntersection.Create();
  Result.Value.d := d;
  Result.Value.mat := Shpe.mat;
  Result.Value.point := Ray.org + d * Ray.dir;
  Result.Value.normal := -Sign(cosi) * Shpe.tnrm;
end;

{ TAabb }

function AabbCreate(PMin, PMax: TVector; shps: TList<TShape>): TShape;
begin
  Result.Kind := EShape.Aabb;
  Result.PMin := PMin;
  Result.PMax := PMax;
  Result.Shps := shps;
end;

function AabbBound(Shpe: TShape): TBounds;
begin
  Result := TBounds.Create(Shpe.PMin, Shpe.PMax);
end;

function AabbIntersect(Shpe: TShape; Ray: TRay; Dist: Single): TNullable<TIntersection>;
var
  VMin, VMax: Single;
  TMin, TMax: Single;
  d: Single;
  intsct: TNullable<TIntersection>;
  indx: Word;
begin
  Result := Default (TNullable<TIntersection>);

  VMin := (Shpe.PMin.X - Ray.org.X) * Ray.invDir.X;
  VMax := (Shpe.PMax.X - Ray.org.X) * Ray.invDir.X;
  TMin := Min(VMin, VMax);
  TMax := Max(VMin, VMax);

  if (TMax < TMin) or (TMax < 0) or (TMin > Dist) then
    Exit;

  VMin := (Shpe.PMin.Y - Ray.org.Y) * Ray.invDir.Y;
  VMax := (Shpe.PMax.Y - Ray.org.Y) * Ray.invDir.Y;
  TMin := Max(TMin, Min(VMin, VMax));
  TMax := Min(TMax, Max(VMin, VMax));

  if (TMax < TMin) or (TMax < 0) or (TMin > Dist) then
    Exit;

  VMin := (Shpe.PMin.W - Ray.org.W) * Ray.invDir.W;
  VMax := (Shpe.PMax.W - Ray.org.W) * Ray.invDir.W;
  TMin := Max(TMin, Min(VMin, VMax));
  TMax := Min(TMax, Max(VMin, VMax));

  if (TMax < TMin) or (TMax < 0) or (TMin > Dist) then
    Exit;

  d := Dist;
  indx := 0;
  while indx < Shpe.shps.Count do
  begin
    intsct := Shpe.shps[indx].Intersect(Ray, d);
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

{ TShape }

function TShape.Bound: TBounds;
begin
  case kind of
     Sphr: Result := SphrBound(Self);
     Plne: Result := PlneBound(Self);
     Tria: Result := TriaBound(Self);
     Aabb: Result := AabbBound(Self);
  end;
end;

function TShape.Intersect(Ray: TRay; Dist: Single): TNullable<TIntersection>;
begin
  case kind of
     Sphr: Result := SphrIntersect(Self, Ray, Dist);
     Plne: Result := PlneIntersect(Self, Ray, Dist);
     Tria: Result := TriaIntersect(Self, Ray, Dist);
     Aabb: Result := AabbIntersect(Self, Ray, Dist);
  end;
end;

end.
