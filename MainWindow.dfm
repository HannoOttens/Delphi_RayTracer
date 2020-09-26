object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Main'
  ClientHeight = 602
  ClientWidth = 954
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object PaintBox1: TPaintBox
    Left = 8
    Top = 8
    Width = 938
    Height = 557
  end
  object Label1: TLabel
    Left = 903
    Top = 576
    Width = 3
    Height = 13
  end
  object Button1: TButton
    Left = 408
    Top = 571
    Width = 489
    Height = 25
    Caption = 'RAYTRACE'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 8
    Top = 571
    Width = 394
    Height = 25
    Caption = 'Redraw'
    TabOrder = 1
    OnClick = Button2Click
  end
end
