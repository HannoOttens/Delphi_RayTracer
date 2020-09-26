unit Scene;

interface

uses Generics.Collections, Shape, Light;

type
  TScene = class
  public
    shapes: TList<Cardinal>;
    ligths: TList<TLight>;
    constructor Create(shapes: TList<Cardinal>; ligths: TList<TLight>);
  end;

implementation

{ TScene }

constructor TScene.Create(shapes: TList<Cardinal>; ligths: TList<TLight>);
begin
  Self.shapes := shapes;
  Self.ligths := ligths;
end;

end.
