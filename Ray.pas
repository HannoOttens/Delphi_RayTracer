unit Ray;

interface

uses System.Math.Vectors;

type
  TRay = class
  public
    dir, org: TVector;
    bounces: Word;
    constructor Create(org, dir: TVector; bounces: Word);
  end;

implementation

{ TRay }

constructor TRay.Create(org, dir: TVector; bounces: Word);
begin
  Self.org := org;
  Self.dir := dir;
  Self.bounces := bounces;
end;

end.
