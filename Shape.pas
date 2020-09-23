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
    Kind: EShape;
    PIdx: Word;
    Mtrl: TMaterial;

    Case EShape of
      Sphr: (rad, radSqr: Single);
      Plne: (PNrm: TVector);
      Tria: (TNrm: TVector; Pdx2, Pdx3: Word);
      Aabb: (PMax, IdxR, IdxL: Word);
  end;

type
  TShapes = array of TShape;

type
  Aabb2 = record
  public
    PMin, PMax: TVector;
    Shps: TShapes;
  end;

type
  TStore = record
  public
    VPos: TArray<TVector>;
    VNrm: TArray<TVector>;
    VTxt: TArray<TVector>;
    Shps: TArray<TShape>;
  end;

type
  TDynStore = class
  public
    VPos: TList<TVector>;
    VNrm: TList<TVector>;
    VTxt: TList<TVector>;
    Shps: TList<TShape>;

    function ToFixStore(): TStore;
    constructor Create;

  end;


function SphrCreate(PIdx: Word; rad: Single;   Mtrl: TMaterial): TShape;
function PlneCreate(PIdx: Word; norm: TVector; Mtrl: TMaterial): TShape;
function TriaCreate(Pdx1, Pdx2, Pdx3: Word;    Mtrl: TMaterial): TShape;
function AabbCreate(Pdx1, Pdx2, IdxR, IdxL: Word): TShape;

/// <summary>Find the intersection with a ray</summary>
/// <param name="Ray">The ray to intesrsect with</param>
/// <returns>A non-null point after intersection</returns>
function Intersect(const Stre: TStore; const Shpe: TShape; const Ray: TRay;
  const Dist: Single): TNullable<TIntersection>;
/// <summary>Calculate the bounds of this shape</summary>
/// <returns>The object bounds</returns>
// TODO: Only return PMAX, we do not need PMin for splitting the bounds
function Bound(const Stre: TStore; const Shpe: TShape): TBounds;

implementation

{ TDynStore }

constructor TDynStore.Create;
begin
  VPos := TList<TVector>.Create;
  VNrm := TList<TVector>.Create;
  VTxt := TList<TVector>.Create;
  Shps := TList<TShape>.Create;
end;

function TDynStore.ToFixStore: TStore;
begin
  Result.VPos := VPos.ToArray;
  Result.VNrm := VNrm.ToArray;
  Result.VTxt := VTxt.ToArray;
  Result.Shps := Shps.ToArray;
end;


{ TShpere }

function SphrCreate(PIdx: Word; rad: Single; Mtrl: TMaterial): TShape;
begin
  Result.Kind := EShape.Sphr;
  Result.PIdx := PIdx;
  Result.Mtrl := Mtrl;
  Result.rad := rad;
  Result.radSqr := rad * rad;
end;

function SphrBound(const Stre: TStore; const Shpe: TShape): TBounds;
begin
  // Todo: calculate pointer instead of [] again and again?
  Result := TBounds.Create(
    Stre.VPos[Shpe.PIdx] - TVector.Create(Shpe.rad, Shpe.rad, Shpe.rad),
    Stre.VPos[Shpe.PIdx] + TVector.Create(Shpe.rad, Shpe.rad, Shpe.rad));
end;

function SphrIntersect(const Stre: TStore; const Shpe: TShape; const Ray: TRay;
  const Dist: Single): TNullable<TIntersection>;
var
  v: TVector;
  b, det, d: Single;
begin
  Result := Default (TNullable<TIntersection>);

  v := Ray.org - Stre.VPos[Shpe.PIdx];
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
  Result.Value.mat := Shpe.Mtrl;
  Result.Value.point := Ray.org + Result.Value.d * Ray.dir;
  Result.Value.normal := (Result.Value.point - Stre.VPos[Shpe.PIdx]).Normalize;
end;

{ TPlane }

function PlneCreate(PIdx: Word; norm: TVector; Mtrl: TMaterial): TShape;
begin
  Result.kind := EShape.Plne;
  Result.Mtrl := Mtrl;
  Result.PIdx := PIdx;
  Result.pnrm := norm.Normalize;
end;

function PlneBound(const Stre: TStore; const shpe: TShape): TBounds;
begin
  raise Exception.Create('A plane does not have a finite bounding box');
end;

function PlneIntersect(const Stre: TStore; const Shpe: TShape; const Ray: TRay;
  const Dist: Single): TNullable<TIntersection>;
var
  cosi, d: Single;
begin
  Result := Default (TNullable<TIntersection>);

  cosi := Ray.dir.DotProduct(Shpe.pnrm);
  d := (Stre.VPos[Shpe.PIdx] - Ray.org).DotProduct(Shpe.pnrm) / cosi;

  if (d > dist) or (d < 0) or (cosi = 0) then
    Exit;

  Result.Value := TIntersection.Create();
  Result.Value.d := d;
  Result.Value.mat := Shpe.Mtrl;
  Result.Value.point := Ray.org + d * Ray.dir;
  Result.Value.normal := Shpe.pnrm * -Sign(cosi);;
end;

{ TTraingle }

function TriaCreate(Pdx1, Pdx2, Pdx3: Word; Mtrl: TMaterial): TShape;
begin
  Result.kind := EShape.Tria;
  Result.PIdx := Pdx1;
  Result.Pdx2 := Pdx2;
  Result.Pdx3 := Pdx3;
  Result.Mtrl := Mtrl;
end;

function TriaBound(const Stre: TStore; const Sphe: TShape): TBounds;
  var pos1, pos2, pos3: TVector;
begin
  pos1 := Stre.VPos[Sphe.PIdx];
  pos2 := Stre.VPos[Sphe.Pdx2];
  pos3 := Stre.VPos[Sphe.Pdx3];
  Result := TBounds.Create(
    TVector.Create(
       Min(pos1.X, Min(pos2.X, pos3.X)),
       Min(pos1.Y, Min(pos2.Y, pos3.Y)),
       Min(pos1.W, Min(pos2.W, pos3.W))
    ),
    TVector.Create(
       Max(pos1.X, Max(pos2.X, pos3.X)),
       Max(pos1.Y, Max(pos2.Y, pos3.Y)),
       Max(pos1.W, Max(pos2.W, pos3.W))
    )
  );
end;

function TriaIntersect(const Stre: TStore; const Shpe: TShape; const Ray: TRay;
  const Dist: Single): TNullable<TIntersection>;
var
  pHit: TVector;
  cosi, d, u, v, w: Single;
  pos1, pos2, pos3: TVector;
begin
  Result := Default (TNullable<TIntersection>);

  pos1 := Stre.VPos[Shpe.PIdx];
  pos2 := Stre.VPos[Shpe.Pdx2];
  pos3 := Stre.VPos[Shpe.Pdx3];

  cosi := Ray.dir.DotProduct(Shpe.tnrm);
  d := (pos1 - Ray.org).DotProduct(Shpe.tnrm) / cosi;

  // Dit not hit plane traingle lies in
  if (d > Dist) or (d < 0) or (cosi = 0) then
    Exit;

  pHit := Ray.org + d * Ray.dir;
  w := Shpe.tnrm.DotProduct((pos2 - pos1).CrossProduct(pHit - pos1));
  if (w <= 0) then
    Exit;
  u := Shpe.tnrm.DotProduct((pos3 - pos2).CrossProduct(pHit - pos2));
  if (u <= 0) then
    Exit;
  v := Shpe.tnrm.DotProduct((pos1 - pos3).CrossProduct(pHit - pos3));
  if (v <= 0) then
    Exit;

  Result.Value := TIntersection.Create();
  Result.Value.d := d;
  Result.Value.mat := Shpe.Mtrl;
  Result.Value.point := Ray.org + d * Ray.dir;
  Result.Value.normal := -Sign(cosi) * Shpe.tnrm;
end;

{ TAabb }

function AabbCreate(Pdx1, Pdx2, IdxR, IdxL: Word): TShape;
begin
  Result.Kind := EShape.Aabb;
  Result.PIdx := Pdx1;
  Result.PMax := Pdx2;
  Result.IdxR := IdxR;
  Result.IdxR := IdxL;
end;

function AabbBound(const Stre: TStore; const Shpe: TShape): TBounds;
begin
  Result := TBounds.Create(Stre.VPos[Shpe.PIdx], Stre.VPos[Shpe.PMax]);
end;

function AabbIntersect(const Stre: TStore; const Shpe: TShape; const Ray: TRay;
  const Dist: Single): TNullable<TIntersection>;
var
  PMin, PMax: TVector;
  VMin, VMax: Single;
  TMin, TMax: Single;
  d: Single;
  intsct: TNullable<TIntersection>;
  indx: Word;
begin
  Result := Default (TNullable<TIntersection>);

  PMin := Stre.VPos[Shpe.PIdx];
  PMax := Stre.VPos[Shpe.PMax];

  VMin := (PMin.X - Ray.org.X) * Ray.invDir.X;
  VMax := (PMax.X - Ray.org.X) * Ray.invDir.X;
  TMin := Min(VMin, VMax);
  TMax := Max(VMin, VMax);

  if (TMax < TMin) or (TMax < 0) or (TMin > Dist) then
    Exit;

  VMin := (PMin.Y - Ray.org.Y) * Ray.invDir.Y;
  VMax := (PMax.Y - Ray.org.Y) * Ray.invDir.Y;
  TMin := Max(TMin, Min(VMin, VMax));
  TMax := Min(TMax, Max(VMin, VMax));

  if (TMax < TMin) or (TMax < 0) or (TMin > Dist) then
    Exit;

  VMin := (PMin.W - Ray.org.W) * Ray.invDir.W;
  VMax := (PMax.W - Ray.org.W) * Ray.invDir.W;
  TMin := Max(TMin, Min(VMin, VMax));
  TMax := Min(TMax, Max(VMin, VMax));

  if (TMax < TMin) or (TMax < 0) or (TMin > Dist) then
    Exit;

  d := Dist;
  indx := 0;

  // Intersect L
  intsct := Intersect(Stre, Stre.Shps[Shpe.IdxL], Ray, d);
  if intsct.HasValue and (intsct.Value.d < d) then
  begin
    d := intsct.Value.d;
    Result := intsct;
  end;

  // Intersect R
  intsct := Intersect(Stre, Stre.Shps[Shpe.IdxR], Ray, d);
  if intsct.HasValue and (intsct.Value.d < d) then
  begin
    d := intsct.Value.d;
    Result := intsct;
  end;
end;

{ TBounds }

constructor TBounds.Create(PMin, PMax: TVector);
begin
  Self.PMin := PMin;
  Self.PMax := PMax;
end;

{ TShape }

function Bound(const Stre: TStore; const Shpe: TShape): TBounds;
begin
  case Shpe.Kind of
     Sphr: Result := SphrBound(Stre, Shpe);
     Plne: Result := PlneBound(Stre, Shpe);
     Tria: Result := TriaBound(Stre, Shpe);
     Aabb: Result := AabbBound(Stre, Shpe);
  end;
end;

function Intersect(const Stre: TStore; const Shpe: TShape; const Ray: TRay;
  const Dist: Single): TNullable<TIntersection>;
begin
  case Shpe.Kind of
     Sphr: Result := SphrIntersect(Stre, Shpe, Ray, Dist);
     Plne: Result := PlneIntersect(Stre, Shpe, Ray, Dist);
     Tria: Result := TriaIntersect(Stre, Shpe, Ray, Dist);
     Aabb: Result := AabbIntersect(Stre, Shpe, Ray, Dist);
  end;
end;

end.
