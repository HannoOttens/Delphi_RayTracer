program RayTracer;

uses
  Vcl.Forms,
  MainWindow in 'MainWindow.pas' {Form1},
  Camera in 'Camera.pas',
  Ray in 'Ray.pas',
  Shape in 'Shape.pas',
  Material in 'Material.pas',
  Scene in 'Scene.pas',
  Light in 'Light.pas',
  Nullable in 'Nullable.pas',
  Intersection in 'Intersection.pas',
  OBJLoader in 'OBJLoader.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
