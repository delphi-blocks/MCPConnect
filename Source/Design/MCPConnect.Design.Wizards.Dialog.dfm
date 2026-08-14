object formMCPProjectWizard: TformMCPProjectWizard
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'New MCP Server Project'
  ClientHeight = 401
  ClientWidth = 604
  Color = clWindow
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  TextHeight = 15
  object PanelBanner: TPanel
    Left = 0
    Top = 56
    Width = 180
    Height = 297
    Align = alLeft
    BevelOuter = bvNone
    Color = 15921906
    ParentBackground = False
    TabOrder = 0
    object ImageBanner: TImage
      Left = 0
      Top = 0
      Width = 180
      Height = 297
      Align = alClient
      Center = True
      ExplicitHeight = 353
    end
  end
  object PanelButtons: TPanel
    Left = 0
    Top = 353
    Width = 604
    Height = 48
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 3
    object ButtonBack: TButton
      Left = 316
      Top = 10
      Width = 85
      Height = 27
      Caption = '< &Back'
      TabOrder = 0
      OnClick = ButtonBackClick
    end
    object ButtonNext: TButton
      Left = 407
      Top = 10
      Width = 85
      Height = 27
      Caption = '&Next >'
      Default = True
      TabOrder = 1
      OnClick = ButtonNextClick
    end
    object ButtonCancel: TButton
      Left = 503
      Top = 10
      Width = 85
      Height = 27
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 2
    end
  end
  object PanelHeader: TPanel
    Left = 0
    Top = 0
    Width = 604
    Height = 56
    Align = alTop
    BevelOuter = bvNone
    Color = clWindow
    ParentBackground = False
    TabOrder = 1
    object LabelPageTitle: TLabel
      Left = 16
      Top = 10
      Width = 102
      Height = 17
      Caption = 'Application type'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object LabelPageHint: TLabel
      Left = 16
      Top = 31
      Width = 392
      Height = 15
      AutoSize = False
      Caption = 'Choose the kind of executable that will host the MCP server.'
    end
  end
  object PageControl: TPageControl
    Left = 180
    Top = 56
    Width = 424
    Height = 297
    ActivePage = TabSummary
    Align = alClient
    Style = tsFlatButtons
    TabOrder = 2
    object TabAppKind: TTabSheet
      Caption = 'Application'
      object LabelAppKindHint: TLabel
        Left = 16
        Top = 148
        Width = 385
        Height = 60
        AutoSize = False
        Caption = 
          'A VCL application shows a small form with Start and Stop buttons' +
          ', handy while debugging. A console application is the leaner cho' +
          'ice, and the only one that can serve the STDIO transport. A Wind' +
          'ows service runs unattended and is installed with /install.'
        WordWrap = True
      end
      object RadioGroupAppKind: TRadioGroup
        Left = 16
        Top = 16
        Width = 385
        Height = 113
        Caption = ' Host application '
        ItemIndex = 1
        Items.Strings = (
          'Console application'
          'VCL application (Start / Stop form)'
          'Windows service')
        TabOrder = 0
        OnClick = OptionChanged
      end
    end
    object TabTransport: TTabSheet
      Caption = 'Transport'
      ImageIndex = 1
      object LabelTransportHint: TLabel
        Left = 16
        Top = 152
        Width = 385
        Height = 60
        AutoSize = False
        Caption = 
          'STDIO is used by desktop MCP clients that spawn the server proce' +
          'ss. Indy embeds an HTTP server in the executable. WebBroker rout' +
          'es the requests through a TWebModule.'
        WordWrap = True
      end
      object RadioGroupTransport: TRadioGroup
        Left = 16
        Top = 16
        Width = 385
        Height = 113
        Caption = ' Transport '
        ItemIndex = 1
        Items.Strings = (
          'STDIO (console only)'
          'HTTP with an embedded Indy server'
          'HTTP through a WebBroker WebModule')
        TabOrder = 0
        OnClick = OptionChanged
      end
    end
    object TabServer: TTabSheet
      Caption = 'Server'
      ImageIndex = 2
      object LabelServerName: TLabel
        Left = 16
        Top = 24
        Width = 65
        Height = 15
        Caption = 'Server name'
      end
      object LabelServerVersion: TLabel
        Left = 216
        Top = 24
        Width = 73
        Height = 15
        Caption = 'Server version'
      end
      object LabelServerPort: TLabel
        Left = 16
        Top = 80
        Width = 22
        Height = 15
        Caption = 'Port'
      end
      object LabelMcpPath: TLabel
        Left = 216
        Top = 80
        Width = 77
        Height = 15
        Caption = 'MCP path info'
      end
      object LabelServiceName: TLabel
        Left = 16
        Top = 136
        Width = 121
        Height = 15
        Caption = 'Windows service name'
        Visible = False
      end
      object EditServerName: TEdit
        Left = 16
        Top = 45
        Width = 185
        Height = 23
        TabOrder = 0
      end
      object EditServerVersion: TEdit
        Left = 216
        Top = 45
        Width = 185
        Height = 23
        TabOrder = 1
      end
      object EditServerPort: TEdit
        Left = 16
        Top = 101
        Width = 185
        Height = 23
        TabOrder = 2
      end
      object EditMcpPath: TEdit
        Left = 216
        Top = 101
        Width = 185
        Height = 23
        TabOrder = 3
      end
      object EditServiceName: TEdit
        Left = 16
        Top = 157
        Width = 185
        Height = 23
        TabOrder = 4
        Visible = False
      end
    end
    object TabSecurity: TTabSheet
      Caption = 'Security'
      ImageIndex = 3
      object GroupCORS: TGroupBox
        Left = 16
        Top = 8
        Width = 385
        Height = 97
        Caption = ' CORS '
        TabOrder = 0
        object LabelCORSMethods: TLabel
          Left = 16
          Top = 44
          Width = 93
          Height = 15
          Caption = 'Allowed methods'
        end
        object LabelCORSOrigins: TLabel
          Left = 200
          Top = 44
          Width = 82
          Height = 15
          Caption = 'Allowed origins'
        end
        object CheckCORS: TCheckBox
          Left = 16
          Top = 21
          Width = 353
          Height = 17
          Caption = 'Enable CORS'
          TabOrder = 0
          OnClick = OptionChanged
        end
        object EditCORSMethods: TEdit
          Left = 16
          Top = 63
          Width = 169
          Height = 23
          TabOrder = 1
        end
        object EditCORSOrigins: TEdit
          Left = 200
          Top = 63
          Width = 169
          Height = 23
          TabOrder = 2
          TextHint = 'empty = any origin'
        end
      end
      object RadioGroupAuth: TRadioGroup
        Left = 16
        Top = 111
        Width = 385
        Height = 41
        Caption = ' Authentication '
        Columns = 3
        ItemIndex = 0
        Items.Strings = (
          'None'
          'Static token'
          'OAuth 2.0')
        TabOrder = 1
        OnClick = OptionChanged
      end
      object PanelToken: TPanel
        Left = 16
        Top = 158
        Width = 385
        Height = 105
        BevelOuter = bvNone
        TabOrder = 2
        Visible = False
        object LabelTokenValue: TLabel
          Left = 0
          Top = 4
          Width = 32
          Height = 15
          Caption = 'Token'
        end
        object LabelTokenLocation: TLabel
          Left = 0
          Top = 52
          Width = 55
          Height = 15
          Caption = 'Read from'
        end
        object LabelTokenHeader: TLabel
          Left = 184
          Top = 52
          Width = 117
          Height = 15
          Caption = 'Header / cookie name'
        end
        object EditTokenValue: TEdit
          Left = 0
          Top = 23
          Width = 369
          Height = 23
          TabOrder = 0
          TextHint = 'my-secret-token'
        end
        object ComboTokenLocation: TComboBox
          Left = 0
          Top = 71
          Width = 169
          Height = 23
          Style = csDropDownList
          ItemIndex = 0
          TabOrder = 1
          Text = 'Authorization: Bearer'
          OnChange = OptionChanged
          Items.Strings = (
            'Authorization: Bearer'
            'Cookie'
            'Custom header')
        end
        object EditTokenHeader: TEdit
          Left = 184
          Top = 71
          Width = 185
          Height = 23
          TabOrder = 2
        end
      end
      object PanelOAuth: TPanel
        Left = 16
        Top = 158
        Width = 385
        Height = 105
        BevelOuter = bvNone
        TabOrder = 3
        Visible = False
        object LabelOAuthResource: TLabel
          Left = 0
          Top = 4
          Width = 114
          Height = 15
          Caption = 'Resource (server URL)'
        end
        object LabelOAuthAuthServer: TLabel
          Left = 184
          Top = 4
          Width = 106
          Height = 15
          Caption = 'Authorization server'
        end
        object LabelOAuthIssuer: TLabel
          Left = 0
          Top = 52
          Width = 72
          Height = 15
          Caption = 'Trusted issuer'
        end
        object LabelOAuthScopes: TLabel
          Left = 184
          Top = 52
          Width = 94
          Height = 15
          Caption = 'Supported scopes'
        end
        object EditOAuthResource: TEdit
          Left = 0
          Top = 23
          Width = 169
          Height = 23
          TabOrder = 0
          TextHint = 'https://localhost:8080'
        end
        object EditOAuthAuthServer: TEdit
          Left = 184
          Top = 23
          Width = 185
          Height = 23
          TabOrder = 1
          TextHint = 'https://login.example.com/v2.0'
        end
        object EditOAuthIssuer: TEdit
          Left = 0
          Top = 71
          Width = 169
          Height = 23
          TabOrder = 2
        end
        object EditOAuthScopes: TEdit
          Left = 184
          Top = 71
          Width = 185
          Height = 23
          TabOrder = 3
          TextHint = 'openid, email, profile'
        end
      end
    end
    object TabOptions: TTabSheet
      Caption = 'Options'
      ImageIndex = 4
      object GroupSession: TGroupBox
        Left = 16
        Top = 8
        Width = 385
        Height = 161
        Caption = ' Sessions '
        TabOrder = 0
        object LabelSessionHeader: TLabel
          Left = 200
          Top = 48
          Width = 71
          Height = 15
          Caption = 'Header name'
        end
        object LabelSessionTimeout: TLabel
          Left = 200
          Top = 96
          Width = 99
          Height = 15
          Caption = 'Timeout (minutes)'
        end
        object CheckSession: TCheckBox
          Left = 16
          Top = 21
          Width = 353
          Height = 17
          Caption = 'Enable session tracking'
          TabOrder = 0
          OnClick = OptionChanged
        end
        object RadioGroupSessionLocation: TRadioGroup
          Left = 16
          Top = 48
          Width = 169
          Height = 73
          Caption = ' Session id location '
          ItemIndex = 0
          Items.Strings = (
            'HTTP header'
            'Cookie')
          TabOrder = 1
          OnClick = OptionChanged
        end
        object EditSessionHeader: TEdit
          Left = 200
          Top = 67
          Width = 169
          Height = 23
          TabOrder = 2
        end
        object EditSessionTimeout: TEdit
          Left = 200
          Top = 115
          Width = 169
          Height = 23
          TabOrder = 3
        end
      end
      object GroupSamples: TGroupBox
        Left = 16
        Top = 183
        Width = 385
        Height = 65
        Caption = ' Sample code '
        TabOrder = 1
        object CheckSamples: TCheckBox
          Left = 16
          Top = 28
          Width = 353
          Height = 17
          Caption = 'Create a unit with a sample tool, resource and prompt'
          TabOrder = 0
          OnClick = OptionChanged
        end
      end
    end
    object TabSummary: TTabSheet
      Caption = 'Summary'
      ImageIndex = 5
      object MemoSummary: TMemo
        Left = 16
        Top = 12
        Width = 385
        Height = 241
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Consolas'
        Font.Style = []
        ParentFont = False
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 0
      end
    end
  end
end
