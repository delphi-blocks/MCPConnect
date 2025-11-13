object Form1: TForm1
  Left = 271
  Top = 114
  Caption = 'Form1'
  ClientHeight = 695
  ClientWidth = 873
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 13
  object Label1: TLabel
    Left = 24
    Top = 48
    Width = 20
    Height = 13
    Caption = 'Port'
  end
  object ButtonStart: TButton
    Left = 24
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Start'
    TabOrder = 0
    OnClick = ButtonStartClick
  end
  object ButtonStop: TButton
    Left = 105
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Stop'
    TabOrder = 1
    OnClick = ButtonStopClick
  end
  object EditPort: TEdit
    Left = 24
    Top = 67
    Width = 121
    Height = 21
    TabOrder = 2
    Text = '8080'
  end
  object ButtonOpenBrowser: TButton
    Left = 24
    Top = 112
    Width = 107
    Height = 25
    Caption = 'Open Browser'
    TabOrder = 3
    OnClick = ButtonOpenBrowserClick
  end
  object memoLog: TMemo
    Left = 0
    Top = 200
    Width = 873
    Height = 495
    Align = alBottom
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -18
    Font.Name = 'Consolas'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 4
    ExplicitTop = 344
    ExplicitWidth = 1267
  end
  object Button1: TButton
    Left = 472
    Top = 112
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 5
    OnClick = Button1Click
  end
  object ApplicationEvents1: TApplicationEvents
    OnIdle = ApplicationEvents1Idle
    Left = 288
    Top = 24
  end
end
