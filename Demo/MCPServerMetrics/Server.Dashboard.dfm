object FrameMetricsDashboard: TFrameMetricsDashboard
  Left = 0
  Top = 0
  Width = 932
  Height = 439
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  ParentFont = False
  TabOrder = 0
  object Pages: TPageControl
    Left = 0
    Top = 0
    Width = 932
    Height = 439
    ActivePage = TabMetrics
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    object TabMetrics: TTabSheet
      Caption = 'Metrics'
      object Grid: TListView
        Left = 0
        Top = 0
        Width = 696
        Height = 409
        Align = alClient
        Columns = <
          item
            Caption = 'Meter'
            Width = 110
          end
          item
            Caption = 'Instrument'
            Width = 170
          end
          item
            Caption = 'Kind'
            Width = 80
          end
          item
            Caption = 'Labels'
            Width = 220
          end
          item
            Caption = 'Count'
            Width = 60
          end
          item
            Caption = 'Value'
            Width = 90
          end
          item
            Caption = 'Detail'
            Width = 170
          end
          item
            Caption = 'Unit'
            Width = 60
          end>
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Consolas'
        Font.Style = []
        GridLines = True
        ReadOnly = True
        RowSelect = True
        ParentFont = False
        TabOrder = 0
        ViewStyle = vsReport
        ExplicitHeight = 407
      end
      object PanelActions: TPanel
        Left = 696
        Top = 0
        Width = 228
        Height = 409
        Align = alRight
        BevelOuter = bvNone
        TabOrder = 1
        ExplicitHeight = 407
        object LabelHint: TLabel
          Left = 10
          Top = 320
          Width = 208
          Height = 81
          AutoSize = False
          Caption = 
            'The grid reads Collect, so it never disturbs the accumulation. T' +
            'he files receive a delta every 30 s, or on demand here.'
          WordWrap = True
        end
        object ChkLive: TCheckBox
          Left = 10
          Top = 10
          Width = 208
          Height = 21
          Caption = 'Live grid (1 s)'
          Checked = True
          State = cbChecked
          TabOrder = 0
        end
        object BtnHarvestDelta: TButton
          Left = 10
          Top = 40
          Width = 208
          Height = 26
          Caption = 'Harvest delta -> files'
          TabOrder = 1
          OnClick = BtnHarvestDeltaClick
        end
        object BtnHarvestCumulative: TButton
          Left = 10
          Top = 72
          Width = 208
          Height = 26
          Caption = 'Harvest cumulative -> files'
          TabOrder = 2
          OnClick = BtnHarvestCumulativeClick
        end
        object BtnSample: TButton
          Left = 10
          Top = 104
          Width = 208
          Height = 26
          Caption = 'Sample host gauges now'
          TabOrder = 3
          OnClick = BtnSampleClick
        end
        object BtnClear: TButton
          Left = 10
          Top = 136
          Width = 208
          Height = 26
          Caption = 'Clear all instruments'
          TabOrder = 4
          OnClick = BtnClearClick
        end
        object BtnWorkload: TButton
          Left = 10
          Top = 176
          Width = 208
          Height = 26
          Caption = 'Simulate workload (50)'
          TabOrder = 5
          OnClick = BtnWorkloadClick
        end
        object BtnErrors: TButton
          Left = 10
          Top = 208
          Width = 208
          Height = 26
          Caption = 'Simulate errors (10)'
          TabOrder = 6
          OnClick = BtnErrorsClick
        end
        object BtnCardinality: TButton
          Left = 10
          Top = 248
          Width = 208
          Height = 26
          Caption = 'Cardinality demo'
          TabOrder = 7
          OnClick = BtnCardinalityClick
        end
        object BtnProvider: TButton
          Left = 10
          Top = 280
          Width = 208
          Height = 26
          Caption = 'Separate provider demo'
          TabOrder = 8
          OnClick = BtnProviderClick
        end
      end
    end
    object TabSlowest: TTabSheet
      Caption = 'Slowest'
      object MemoSlowest: TMemo
        Left = 0
        Top = 0
        Width = 924
        Height = 409
        Align = alClient
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Consolas'
        Font.Style = []
        Lines.Strings = (
          'The slowest histogram series appear here on every harvest.')
        ParentFont = False
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 0
        WordWrap = False
        ExplicitHeight = 407
      end
    end
    object TabText: TTabSheet
      Caption = 'Text'
      object MemoText: TMemo
        Left = 0
        Top = 0
        Width = 924
        Height = 409
        Align = alClient
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Consolas'
        Font.Style = []
        Lines.Strings = (
          'The latest harvest, rendered by MetricsToText.')
        ParentFont = False
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 0
        WordWrap = False
        ExplicitHeight = 407
      end
    end
    object TabJson: TTabSheet
      Caption = 'JSON'
      object MemoJson: TMemo
        Left = 0
        Top = 0
        Width = 924
        Height = 409
        Align = alClient
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Consolas'
        Font.Style = []
        Lines.Strings = (
          'The latest harvest, rendered by MetricsToJson.')
        ParentFont = False
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 0
        WordWrap = False
        ExplicitHeight = 407
      end
    end
    object TabExporters: TTabSheet
      Caption = 'Exporters'
      object MemoExporters: TMemo
        Left = 0
        Top = 0
        Width = 924
        Height = 409
        Align = alClient
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Consolas'
        Font.Style = []
        Lines.Strings = (
          'The exporter list appears here once the hub is started.')
        ParentFont = False
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 0
        WordWrap = False
        ExplicitHeight = 407
      end
    end
    object TabLog: TTabSheet
      Caption = 'Log'
      object MemoLog: TMemo
        Left = 0
        Top = 0
        Width = 924
        Height = 409
        Align = alClient
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Consolas'
        Font.Style = []
        ParentFont = False
        ScrollBars = ssBoth
        TabOrder = 0
        WordWrap = False
        ExplicitHeight = 407
      end
    end
    object TabCases: TTabSheet
      Caption = 'Use cases'
      object MemoCases: TMemo
        Left = 0
        Top = 0
        Width = 924
        Height = 409
        Align = alClient
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 0
        WordWrap = False
        ExplicitHeight = 407
      end
    end
    object TabAbout: TTabSheet
      Caption = 'About'
      object MemoAbout: TMemo
        Left = 0
        Top = 0
        Width = 924
        Height = 409
        Align = alClient
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 0
        WordWrap = False
        ExplicitHeight = 407
      end
    end
  end
  object UiTimer: TTimer
    OnTimer = UiTimerTimer
    Left = 880
    Top = 8
  end
  object SampleTimer: TTimer
    Interval = 3000
    OnTimer = SampleTimerTimer
    Left = 880
    Top = 40
  end
end
