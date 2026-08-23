object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'MCPConnect C++Builder BigQuery Metadata Sample'
  ClientHeight = 700
  ClientWidth = 980
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 15
  object LblEndpointCaption: TLabel
    Left = 16
    Top = 18
    Width = 166
    Height = 15
    Caption = 'MCP endpoint: http://127.0.0.1:'
  end
  object LblServerStatus: TLabel
    Left = 16
    Top = 52
    Width = 109
    Height = 15
    Caption = 'MCP status: stopped'
  end
  object LblTokenStatus: TLabel
    Left = 16
    Top = 78
    Width = 155
    Height = 15
    Caption = 'Token source: not configured'
  end
  object LblProject: TLabel
    Left = 16
    Top = 116
    Width = 51
    Height = 15
    Caption = 'Project ID'
  end
  object LblDataset: TLabel
    Left = 480
    Top = 116
    Width = 53
    Height = 15
    Caption = 'Dataset ID'
  end
  object LblResult: TLabel
    Left = 16
    Top = 184
    Width = 63
    Height = 15
    Caption = 'Result JSON'
  end
  object LblLog: TLabel
    Left = 16
    Top = 514
    Width = 59
    Height = 15
    Caption = 'Sample log'
  end
  object EdtPort: TEdit
    Left = 219
    Top = 14
    Width = 73
    Height = 23
    TabOrder = 0
    Text = '8080'
  end
  object BtnStart: TButton
    Left = 312
    Top = 12
    Width = 128
    Height = 27
    Caption = 'Start MCP Server'
    TabOrder = 1
    OnClick = BtnStartClick
  end
  object BtnStop: TButton
    Left = 448
    Top = 12
    Width = 104
    Height = 27
    Caption = 'Stop Server'
    TabOrder = 2
    OnClick = BtnStopClick
  end
  object BtnOpenOutput: TButton
    Left = 840
    Top = 12
    Width = 124
    Height = 27
    Caption = 'Open Output Folder'
    TabOrder = 3
    OnClick = BtnOpenOutputClick
  end
  object EdtProjectId: TEdit
    Left = 16
    Top = 136
    Width = 440
    Height = 23
    TabOrder = 4
  end
  object EdtDatasetId: TEdit
    Left = 480
    Top = 136
    Width = 484
    Height = 23
    TabOrder = 5
  end
  object BtnListDatasets: TButton
    Left = 16
    Top = 165
    Width = 144
    Height = 27
    Caption = 'Test: List Datasets'
    TabOrder = 6
    OnClick = BtnListDatasetsClick
  end
  object BtnDescribeDataset: TButton
    Left = 168
    Top = 165
    Width = 168
    Height = 27
    Caption = 'Test: Describe Dataset'
    TabOrder = 7
    OnClick = BtnDescribeDatasetClick
  end
  object MemoResult: TMemo
    Left = 16
    Top = 204
    Width = 948
    Height = 302
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Consolas'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 8
    WordWrap = False
  end
  object MemoLog: TMemo
    Left = 16
    Top = 534
    Width = 948
    Height = 150
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Consolas'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 9
  end
end
