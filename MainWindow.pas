unit MainWindow;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, System.Math.Vectors,
  Vcl.ExtCtrls, Math;

type
  TForm1 = class(TForm)
    PaintBox1: TPaintBox;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation

uses Generics.Collections,
  Nullable, Camera, Ray, Scene, Light, Shape, Material, Intersection;

{$R *.dfm}

function OriginRay(cam: TCamera; x, y, xMax, yMax: Word): TRay;
var
  aspRat, fX, fZ: Single;
  p0, up, right, dir: TVector;
begin
  // Find up and right vectors
  up := TVector.Create(0, 0, 1);
  right := up.CrossProduct(cam.look).Normalize;
  up := cam.look.CrossProduct(right);

  // Caluclate camera stuff
  aspRat := xMax / yMax;
  p0 := cam.pos + cam.look * cam.canvasDist; // p0 is incorrect

  // Calculate origin ray
  fX := (x / xMax) - 0.5;
  fZ := (y / yMax) - 0.5;
  dir := cam.pos + p0 + fX * aspRat * right - fZ * up;

  OriginRay := TRay.Create(cam.pos, dir.Normalize, 0);
end;

function LightIntersect(Scene: TScene; lr: TRay; ld: Single): Boolean;
var
  intsct: TNullable<TIntersection>;
  Shape: TShape;
begin
  Result := false;

  for Shape in Scene.shapes do
  begin
    intsct := Shape.Intersect(lr);
    if intsct.HasValue and (intsct.Value.d < ld) then
      Exit;
  end;

  Result := true;
end;

function ColClamp(v: Single): Byte;
begin
  Result := Min(253, Round(v));
end;

function PowInt(n: Single; e: Integer): Single;
begin
  Result := 1;
  while (true) do
  begin
    if (e and 1 <> 0) then
      Result := Result * n;
    e := e shr 1;
    e := e - 1;
    if (e < 1) then
      break;
    n := n * n;
  end;
end;

function RayTrace(Scene: TScene; cam: TCamera; Ray: TRay): TVector;
var
  Shape: TShape;
  Light: TLight;
  intsct, hit: TNullable<TIntersection>;
  lv, col, s: TVector;
  d, ld, attn, pow: Single;
  I: Integer;
begin
  // Intersect the scene
  d := Single.MaxValue;
  for Shape in Scene.shapes do
  begin
    intsct := Shape.Intersect(Ray);
    if intsct.HasValue and (intsct.Value.d < d) then
    begin
      d := intsct.Value.d;
      hit := intsct;
    end;
  end;

  // Calculate the color
  if (d < Single.MaxValue) and hit.HasValue then
  begin
    col := TVector.Create(0, 0, 0);

    for I := 0 to Scene.ligths.Count - 1 do
    begin
      lv := Scene.ligths[I].pos - hit.Value.point;
      ld := lv.Length;
      lv := lv.Normalize;

      if LightIntersect(Scene, TRay.Create(hit.Value.point + 0.001 *
        hit.Value.normal, lv, 0), ld) then
      begin
        // Diffuse light
        attn := 1 / (ld * ld);
        pow := Min(1, Max(0, lv.DotProduct(hit.Value.normal) * attn *
          Scene.ligths[I].pow));
        col := col + pow * hit.Value.mat.color *
          (1.0 - hit.Value.mat.reflective);

        // Specular light
        s := lv - 2.0 * lv.DotProduct(hit.Value.normal) * hit.Value.normal;
        col := col + PowInt(Max(Ray.dir.DotProduct(s), 0), 200) * Scene.ligths
          [I].lum * 0.5;
      end;
    end;

    // Ambient
    col := col + hit.Value.mat.color * 0.05 * (1.0 - hit.Value.mat.reflective);

    // Reflections
    if hit.Value.mat.reflective > 0 then
    begin
      // Create reflected ray
      Ray.dir := Ray.dir - 2 * hit.Value.normal *
        hit.Value.normal.DotProduct(Ray.dir);
      Ray.dir := Ray.dir.Normalize;
      Ray.org := hit.Value.point + 0.001 * hit.Value.normal;
      Ray.bounces := Ray.bounces + 1;

      // Calculate color
      col := col + hit.Value.mat.reflective * RayTrace(Scene, cam, Ray);
    end;

    Result := col;
  end
  else
  begin // No intersect, background color
    Result := TVector.Create(10, 25, 25);
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  xMax, yMax, x, y: Word;
  cam: TCamera;
  Scene: TScene;
  org: TRay;
  col: TVector;
  I: Integer;
begin
  // Define scene
  Scene := TScene.Create(TList<TShape>.Create, TList<TLight>.Create);

  // Add shapes
  Scene.shapes.Add(TShpere.Create(TVector.Create(3, 30, 0), 2,
    TMaterial.Create(TVector.Create(0, 255, 255), 0)));
  Scene.shapes.Add(TShpere.Create(TVector.Create(-3, 35, 0), 3,
    TMaterial.Create(TVector.Create(0, 255, 0), 0)));
  Scene.shapes.Add(TShpere.Create(TVector.Create(0, 28, -2), 2,
    TMaterial.Create(TVector.Create(255, 0, 0), 0)));
  Scene.shapes.Add(TShpere.Create(TVector.Create(1, 26, 1), 0.5,
    TMaterial.Create(TVector.Create(255, 255, 0), 0)));
  Scene.shapes.Add(TShpere.Create(TVector.Create(1, 26, 1), 0.5,
    TMaterial.Create(TVector.Create(255, 255, 0), 0)));
  Scene.shapes.Add(TShpere.Create(TVector.Create(1, 60, 10), 20,
    TMaterial.Create(TVector.Create(0, 0, 255), 0.8)));

  // Add lights
  Scene.ligths.Add(TLight.Create(TVector.Create(-2, 40, 10), TVector.Create(255,
    255, 255), 50));

  // Define camera
  cam := TCamera.Create(TVector.Create(0, 0, 0), TVector.Create(0, 1, 0), 3);

  // Raytrace all the pixels
  xMax := Form1.PaintBox1.Width;
  yMax := Form1.PaintBox1.Height;
  for x := 0 to xMax do
    for y := 0 to yMax do
    begin
      col := TVector.Zero;
      for I := 0 to 1 do
      begin

         org := OriginRay(cam, x, y, xMax, yMax);
         col := col + RayTrace(Scene, cam, org);
      end;

      Form1.PaintBox1.Canvas.Pixels[x, y] :=
        RGB(ColClamp(col.x), ColClamp(col.y), ColClamp(col.W));
    end;
end;

end.
