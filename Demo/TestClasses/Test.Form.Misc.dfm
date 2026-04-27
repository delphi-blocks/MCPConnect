object frmMisc: TfrmMisc
  Left = 0
  Top = 0
  Caption = 'Misc'
  ClientHeight = 383
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 15
  object edtTemplate: TEdit
    Left = 23
    Top = 16
    Width = 305
    Height = 23
    TabOrder = 0
    Text = 'demo://weather/{city}/temp/{um}'
  end
  object edtURI: TEdit
    Left = 23
    Top = 43
    Width = 305
    Height = 23
    TabOrder = 1
    Text = 'demo://weather/rome/temp/celsius'
  end
  object Button2: TButton
    Left = 23
    Top = 70
    Width = 75
    Height = 25
    Caption = 'Router'
    TabOrder = 2
    OnClick = Button2Click
  end
  object btnMatches: TButton
    Left = 23
    Top = 112
    Width = 75
    Height = 25
    Caption = 'Matches'
    TabOrder = 3
    OnClick = btnMatchesClick
  end
  object memoLog: TMemo
    Left = 0
    Top = 151
    Width = 624
    Height = 232
    Align = alBottom
    Lines.Strings = (
      'memoLog')
    TabOrder = 4
  end
  object btnDelphiQueue: TButton
    Left = 432
    Top = 42
    Width = 75
    Height = 25
    Caption = 'btnDelphiQueue'
    TabOrder = 5
    OnClick = btnDelphiQueueClick
  end
  object btnMCPQueue: TButton
    Left = 432
    Top = 88
    Width = 75
    Height = 25
    Caption = 'btnMCPQueue'
    TabOrder = 6
    OnClick = btnMCPQueueClick
  end
  object btnPrintQueue: TButton
    Left = 513
    Top = 88
    Width = 75
    Height = 25
    Caption = 'btnPrintQueue'
    TabOrder = 7
    OnClick = btnPrintQueueClick
  end
end
