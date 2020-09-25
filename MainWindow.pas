unit MainWindow;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, System.Math.Vectors,
  Vcl.ExtCtrls, Math, Generics.Collections,
  Nullable, Camera, Ray, Scene, Light, Shape, Material, Intersection, OBJLoader;

type
  TImage = array of array of TColor;

type
  TForm1 = class(TForm)
    PaintBox1: TPaintBox;
    Button1: TButton;
    Label1: TLabel;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  end;

type
  TRayTraceThread = class(TThread)
  protected
    xMax, yMax, tIdx, mIdx: Word;
    cam: TCamera;
    Scene: TScene;
    img: TImage;
    Stre: TDynStore;
    procedure Execute; override;
  public
    constructor Create(img: TImage; const xMax, yMax, tIdx, mIdx: Word;
      cam: TCamera; Stre: TDynStore; Scene: TScene);
  end;

type
  TThreads = array [0 .. 6] of TRayTraceThread;

type
  TMainTaskThread = class(TThread)
  protected
    threads: TThreads;
    img: TImage;
    procedure Execute; override;
  public
    constructor Create(threads: TThreads; img: TImage);
  end;

var
  Form1: TForm1;
  img: TImage;

implementation

{ TRunProcessThread }

constructor TRayTraceThread.Create(img: TImage;
  const xMax, yMax, tIdx, mIdx: Word; cam: TCamera; Stre: TDynStore;
  Scene: TScene);
begin
  inherited Create(True);
  Priority := tpHighest;

  // Props
  Self.tIdx := tIdx;
  Self.mIdx := mIdx;
  Self.cam := cam;
  Self.Scene := Scene;
  Self.img := img;
  Self.yMax := yMax;
  Self.xMax := xMax;
  Self.Stre := Stre;

  FreeOnTerminate := False;
  Resume;
end;

{ TMainTaskThread }

constructor TMainTaskThread.Create(threads: TThreads; img: TImage);
begin
  inherited Create(True);
  Priority := tpHighest;

  // Props
  Self.threads := threads;
  Self.img := img;

  FreeOnTerminate := False;
  Resume;
end;

procedure TMainTaskThread.Execute;
var
  tIdx, x, y, xMax, yMax: Word;
  time: DWORD;
begin
  Form1.Label1.Caption := 'TRACING';
  time := GetTickCount;

  tIdx := 0;
  while tIdx < Length(threads) do
  begin
    threads[tIdx].WaitFor;
    tIdx := tIdx + 1;
  end;

  time := GetTickCount - time;
  Form1.Label1.Caption := IntToStr(time) + 'ms';

  xMax := Form1.PaintBox1.Width;
  yMax := Form1.PaintBox1.Height;

  // Copy pixels
  x := 0;
  while x < xMax do
  begin
    y := 0;
    while y < yMax do
    begin
      Form1.PaintBox1.Canvas.Pixels[x, y] := img[x, y];
      y := y + 1;
    end;
    x := x + 1;
  end;

end;

{ TForm }

{$R *.dfm}

function Inv(v: Single): Single;
begin
  if v <> 0 then
  begin
    Result := 1 / v;
  end
  else
    Result := Single.PositiveInfinity;
end;

function InvVec3(v: TVector): TVector;
begin
  Result := TVector.Create(Inv(v.x), Inv(v.y), Inv(v.W));
end;

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
  dir := (cam.pos + p0 + fX * aspRat * right - fZ * up).Normalize;
  OriginRay := TRay.Create(cam.pos, dir, InvVec3(dir), 0);
end;

function LightIntersect(Stre: TDynStore; Scene: TScene; lr: TRay; ld: Single): Boolean;
var
  intsct: TNullable<TIntersection>;
  I: Cardinal;
begin
  Result := False;

  I := 0;
  while I < Scene.shapes.Count do
  begin
    intsct := Intersect(Stre, Stre.Shps[Scene.shapes[I]], lr, ld);
    if intsct.HasValue and (intsct.Value.d < ld) then
      Exit;

    I := I + 1;
  end;

  Result := True;
end;

function ColClamp(v: Single): Byte;
begin
  Result := Min(253, Round(v));
end;

function PowInt(n: Single; e: Integer): Single;
begin
  Result := 1;
  while (True) do
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

function RayTrace(Stre: TDynStore; Scene: TScene; cam: TCamera; Ray: TRay): TVector;
var
  NRay: TRay;
  Shape: TShape;
  Light: TLight;
  intsct, hit: TNullable<TIntersection>;
  lv, col, s: TVector;
  d, ld, attn, pow: Single;
  I: Integer;
begin
  // Intersect the scene
  d := Single.PositiveInfinity;
  I := 0;
  while I < Scene.shapes.Count do
  begin
    intsct := Intersect(Stre, Stre.Shps[Scene.shapes[I]], Ray, d);
    if intsct.HasValue and (intsct.Value.d < d) then
    begin
      d := intsct.Value.d;
      hit := intsct;
    end;
    I := I + 1;
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

      if LightIntersect(Stre, Scene, TRay.Create(hit.Value.point + 0.01 *
        hit.Value.normal, lv, InvVec3(lv), 0), ld) then
      begin
        // Diffuse light
        attn := Scene.ligths[I].pow / (ld * ld);
        pow := Min(1, Max(0, lv.DotProduct(hit.Value.normal) * attn));
        col := col + pow * hit.Value.mat.color *
          (1.0 - hit.Value.mat.reflective);

        // Specular light
        s := lv - 2.0 * lv.DotProduct(hit.Value.normal) * hit.Value.normal;
        col := col + PowInt(Max(Ray.dir.DotProduct(s), 0), 200) * Scene.ligths
          [I].lum * attn;
      end;
    end;

    // Ambient
    col := col + hit.Value.mat.color * 0.05 * (1.0 - hit.Value.mat.reflective);

    // Reflections
    if (Ray.bounces < 5) and (hit.Value.mat.reflective > 0) then
    begin
      // Create reflected ray
      Ray.dir := Ray.dir - 2 * hit.Value.normal *
        hit.Value.normal.DotProduct(Ray.dir);
      Ray.dir := Ray.dir.Normalize;
      Ray.invDir := InvVec3(Ray.dir);
      Ray.org := hit.Value.point + 0.01 * hit.Value.normal;
      Ray.bounces := Ray.bounces + 1;

      // Calculate color
      col := col + hit.Value.mat.reflective * RayTrace(Stre, Scene, cam, Ray);
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
  cam: TCamera;
  Scene: TScene;
  xMax, yMax, tIdx: Word;
  PIdx, SIdx: Cardinal;
  tArr: TThreads;
  Bnds: TBounds;
  Stre: TDynStore;
begin
  // Define scene
  Scene := TScene.Create(TList<Cardinal>.Create, TList<TLight>.Create);

  // Initialize store
  Stre := TDynStore.Create;

  // Add shapes
//  PIdx := Stre.VPos.Add(TVector.Create(6, 30, -2));
//  SIdx := Stre.Shps.Add(SphrCreate(PIdx, 2, TMaterial.Create(TVector.Create(0,
//    255, 255), 0)));
//  Scene.shapes.Add(SIdx);
//
//  PIdx := Stre.VPos.Add(TVector.Create(-8, 35, 0));
//  SIdx := Stre.Shps.Add(SphrCreate(PIdx, 3, TMaterial.Create(TVector.Create(0,
//    255, 0), 0.5)));
//  Scene.shapes.Add(SIdx);
//
//  PIdx := Stre.VPos.Add(TVector.Create(0, 28, -3));
//  SIdx := Stre.Shps.Add(SphrCreate(PIdx, 2, TMaterial.Create(TVector.Create(255,
//    0, 0), 0)));
//  Scene.shapes.Add(SIdx);
//
//  PIdx := Stre.VPos.Add(TVector.Create(1, 26, 1));
//  SIdx := Stre.Shps.Add(SphrCreate(PIdx, 0.5,
//    TMaterial.Create(TVector.Create(255, 255, 0), 0)));
//  Scene.shapes.Add(SIdx);

  // Add objs
  Scene.shapes.Add(LoadOBJ(Stre, 'C:\Repos\Delphi_RayTracer\house.obj',
    TVector.Create(-10, 50, -5)));
//  Scene.shapes.Add(LoadOBJ(Stre, 'C:\Repos\Delphi_RayTracer\horse.obj',
//    TVector.Create(0, 300, -40)));
//  Scene.shapes.Add(LoadOBJ(Stre, 'C:\Repos\Delphi_RayTracer\teapot.obj',
//    TVector.Create(10, 40, 3)));
//  Scene.shapes.Add(LoadOBJ(Stre, 'C:\Repos\Delphi_RayTracer\cow.obj',
//    TVector.Create(10, 40, 3)));

//  // Octree scene
//  Bnds := Rebound(Stre, Scene.shapes);
//  SIdx := SubDevide(Stre, 0, Stre.VPos.Add(Bnds.PMin), Stre.VPos.Add(Bnds.PMax),
//    Scene.shapes);
//  Scene.shapes := TList<Cardinal>.Create();
//  Scene.shapes.Add(SIdx);

  // Add lights
  Scene.ligths.Add(TLight.Create(TVector.Create(-2, 0, 10), TVector.Create(255,
    255, 255), 50));

  // Define camera
  cam := TCamera.Create(TVector.Create(0, 0, 0), TVector.Create(0, 1, 0), 3);

  // Get canvasSize
  xMax := Form1.PaintBox1.Width;
  yMax := Form1.PaintBox1.Height;
  SetLength(img, xMax, yMax);

  // Create threads
  tIdx := 0;
  while tIdx < Length(tArr) do
  begin
    tArr[tIdx] := TRayTraceThread.Create(img, xMax, yMax, tIdx, Length(tArr), cam,
      Stre, Scene);
    tIdx := tIdx + 1;
  end;

  TMainTaskThread.Create(tArr, img);
end;

procedure TRayTraceThread.Execute;
var
  x, y: Word;
  org: TRay;
  col: TVector;
  I: Integer;
begin
  NameThreadForDebugging('RT_' + IntToStr(tIdx));

  // Raytrace all the pixels for this thread
  x := 0;
  while x < xMax do
  begin
    y := tIdx;
    while y < yMax do
    begin
      col := TVector.Zero;
      for I := 0 to 1 do
      begin

        org := OriginRay(cam, x, y, xMax, yMax);
        col := col + RayTrace(Stre, Scene, cam, org);
      end;

      img[x, y] := RGB(ColClamp(col.x), ColClamp(col.y), ColClamp(col.W));

      y := y + mIdx;
    end;
    x := x + 1;
  end;
  inherited;
end;

procedure TForm1.Button2Click(Sender: TObject);
var xMax, yMax, x, y: Word;
begin
  xMax := Form1.PaintBox1.Width;
  yMax := Form1.PaintBox1.Height;

  // Copy pixels
  x := 0;
  while x < xMax do
  begin
    y := 0;
    while y < yMax do
    begin
      Form1.PaintBox1.Canvas.Pixels[x, y] := img[x, y];
      y := y + 1;
    end;
    x := x + 1;
  end;
end;

end.
