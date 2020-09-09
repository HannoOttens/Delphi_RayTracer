unit Camera;

interface
uses System.Math.Vectors;

type
  TCamera = class
    public
      canvasDist: Word;
      pos: TVector;
      look: TVector;
      constructor Create(pos, look: TVector; canvasDist: Word);
  end;

implementation

{ TCamera }

constructor TCamera.Create(pos, look: TVector; canvasDist: Word);
begin
  Self.pos := pos;
  Self.look := look;
  Self.canvasDist := canvasDist;
end;

end.

