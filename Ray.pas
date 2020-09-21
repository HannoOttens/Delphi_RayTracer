unit Ray;

interface

uses System.Math.Vectors;

type
  TRay = record
  public
    dir, invDir, org: TVector;
    bounces: Word;
    constructor Create(org, dir, invDir: TVector; bounces: Word);
  end;

implementation

{ TRay }

constructor TRay.Create(org, dir, invDir: TVector; bounces: Word);
begin
  Self.org := org;
  Self.invDir := invDir;
  Self.dir := dir;
  Self.bounces := bounces;
end;

end.
