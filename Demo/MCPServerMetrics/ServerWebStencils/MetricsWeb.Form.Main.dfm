object frmMain: TfrmMain
  Left = 271
  Top = 114
  Caption = 'MCPConnect metrics - WebStencils dashboard'
  ClientHeight = 520
  ClientWidth = 940
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 13
  object Label1: TLabel
    Left = 24
    Top = 40
    Width = 21
    Height = 13
    Caption = 'Port'
  end
  object LabelHint: TLabel
    Left = 160
    Top = 63
    Width = 53
    Height = 13
    Caption = 'templates:'
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
  object ButtonOpenDashboard: TButton
    Left = 186
    Top = 8
    Width = 130
    Height = 25
    Caption = 'Open Dashboard'
    TabOrder = 2
    OnClick = ButtonOpenDashboardClick
  end
  object EditPort: TEdit
    Left = 24
    Top = 59
    Width = 121
    Height = 21
    TabOrder = 3
    Text = '8080'
  end
  object memoLog: TMemo
    Left = 0
    Top = 104
    Width = 940
    Height = 416
    Align = alBottom
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Consolas'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 4
    WordWrap = False
  end
  object ApplicationEvents1: TApplicationEvents
    OnIdle = ApplicationEvents1Idle
    Left = 96
    Top = 152
  end
  object TimerSample: TTimer
    Interval = 2000
    OnTimer = TimerSampleTimer
    Left = 24
    Top = 152
  end
  object TimerHarvest: TTimer
    Interval = 30000
    OnTimer = TimerHarvestTimer
    Left = 24
    Top = 200
  end
end
