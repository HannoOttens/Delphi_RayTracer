unit Scene;

interface

uses Generics.Collections, Shape, Light;

type
  TScene = class
  public
    shapes: TArray<Cardinal>;
    ligths: TArray<TLight>;
    constructor Create(shapes: TArray<Cardinal>; ligths: TArray<TLight>);
  end;

implementation

{ TScene }

constructor TScene.Create(shapes: TArray<Cardinal>; ligths: TArray<TLight>);
begin
  Self.shapes := shapes;
  Self.ligths := ligths;
end;

end.
