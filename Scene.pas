unit Scene;

interface

uses Generics.Collections, Shape, Light;

type
  TScene = class
  public
    shapes: TList<TShape>;
    ligths: TList<TLight>;
    constructor Create(shapes: TList<TShape>; ligths: TList<TLight>);
  end;

implementation

{ TScene }

constructor TScene.Create(shapes: TList<TShape>; ligths: TList<TLight>);
begin
  Self.shapes := shapes;
  Self.ligths := ligths;
end;

end.
