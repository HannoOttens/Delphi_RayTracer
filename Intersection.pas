unit Intersection;

interface

uses System.Math.Vectors, Material;

type
  TIntersection = class
    point, normal: TVector;
    d: single;
    mat: TMaterial;
  end;

implementation

end.
