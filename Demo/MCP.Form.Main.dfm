object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 611
  ClientWidth = 1270
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 15
  object lblLog: TLabel
    Left = 8
    Top = 107
    Width = 58
    Height = 15
    Caption = 'Memo Log'
  end
  object lblSnippets: TLabel
    Left = 601
    Top = 107
    Width = 83
    Height = 15
    Caption = 'Memo Snippets'
  end
  object btnEnvelope: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Test Envelope'
    TabOrder = 0
    OnClick = btnEnvelopeClick
  end
  object btnId: TButton
    Left = 8
    Top = 39
    Width = 75
    Height = 25
    Caption = 'Test Id'
    TabOrder = 1
    OnClick = btnIdClick
  end
  object btnRequest: TButton
    Left = 114
    Top = 8
    Width = 129
    Height = 25
    Caption = 'Request Position'
    TabOrder = 2
    OnClick = btnRequestClick
  end
  object btnResponse: TButton
    Left = 274
    Top = 8
    Width = 129
    Height = 25
    Caption = 'Response Serialize'
    TabOrder = 3
    OnClick = btnResponseClick
  end
  object btnRequestDes: TButton
    Left = 114
    Top = 70
    Width = 129
    Height = 25
    Caption = 'Request Deserialize'
    TabOrder = 4
    OnClick = btnRequestDesClick
  end
  object btnRequestNamed: TButton
    Left = 114
    Top = 39
    Width = 129
    Height = 25
    Caption = 'Request Named'
    TabOrder = 5
    OnClick = btnRequestNamedClick
  end
  object btnRtti: TButton
    Left = 432
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Call Rtti'
    TabOrder = 6
    OnClick = btnRttiClick
  end
  object btnResponseDes: TButton
    Left = 274
    Top = 39
    Width = 129
    Height = 25
    Caption = 'Response Deserialize'
    TabOrder = 7
    OnClick = btnResponseDesClick
  end
  object Panel1: TPanel
    Left = 0
    Top = 128
    Width = 1270
    Height = 483
    Align = alBottom
    Caption = 'Panel1'
    TabOrder = 8
    object mmoLog: TMemo
      Left = 1
      Top = 1
      Width = 600
      Height = 481
      Align = alLeft
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -18
      Font.Name = 'Consolas'
      Font.Style = []
      ParentFont = False
      ScrollBars = ssBoth
      TabOrder = 0
      WordWrap = False
    end
    object mmoSnippets: TMemo
      Left = 601
      Top = 1
      Width = 668
      Height = 481
      Align = alClient
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -18
      Font.Name = 'Consolas'
      Font.Style = []
      Lines.Strings = (
        'Syntax:'
        '--> data sent to Server'
        '<-- data sent to Client'
        ''
        ''
        'RPC call with positional parameters:'
        '--> {"jsonrpc": "2.0", "method": "subtract", "params": [42, '
        '23], "id": 1}'
        '<-- {"jsonrpc": "2.0", "result": 19, "id": 1}'
        ''
        '--> {"jsonrpc": "2.0", "method": "subtract", "params": [23, '
        '42], "id": 2}'
        '<-- {"jsonrpc": "2.0", "result": -19, "id": 2}'
        ''
        'RPC call with named parameters:'
        '--> {"jsonrpc": "2.0", "method": "subtract", "params": '
        '{"subtrahend": 23, "minuend": 42}, "id": 3}'
        '<-- {"jsonrpc": "2.0", "result": 19, "id": 3}'
        '--> {"jsonrpc": "2.0", "method": "subtract", "params": '
        '{"minuend": 42, "subtrahend": 23}, "id": 4}'
        '<-- {"jsonrpc": "2.0", "result": 19, "id": 4}'
        ''
        'a Notification:'
        '--> {"jsonrpc": "2.0", "method": "update", "params": '
        '[1,2,3,4,5]}'
        '--> {"jsonrpc": "2.0", "method": "foobar"}'
        ''
        'RPC call of non-existent method:'
        '--> {"jsonrpc": "2.0", "method": "foobar", "id": "1"}'
        '<-- {"jsonrpc": "2.0", "error": {"code": -32601, "message": '
        '"Procedure not found."}, "id": "1"}'
        ''
        'RPC call with invalid JSON:'
        '--> {"jsonrpc": "2.0", "method": "foobar, "params": "bar", '
        '"baz]'
        '<-- {"jsonrpc": "2.0", "error": {"code": -32700, "message": '
        '"Parse error."}, "id": null}'
        ''
        'RPC call with invalid Request object:'
        '--> {"jsonrpc": "2.0", "method": 1, "params": "bar"}'
        '<-- {"jsonrpc": "2.0", "error": {"code": -32600, "message": '
        '"Invalid Request."}, "id": null}'
        ''
        'RPC call Batch, invalid JSON:'
        '--> [ {"jsonrpc": "2.0", "method": "sum", "params": [1,2,4], '
        '"id": "1"},{"jsonrpc": "2.0", "method" ]'
        '<-- {"jsonrpc": "2.0", "error": {"code": -32700, "message": '
        '"Parse error."}, "id": null}'
        ''
        'RPC call with an empty Array:'
        '--> [] '
        '<-- "jsonrpc": "2.0", "error": {"code": -32600, "message": '
        '"Invalid Request."}, "id": null}'
        ''
        'RPC call with an invalid Batch (but not empty):'
        '--> [1]'
        '<-- [ {"jsonrpc": "2.0", "error": {"code": -32600, "message": '
        '"Invalid Request."}, "id": null} ]'
        ''
        'RPC call with invalid Batch:'
        '--> [1,2,3]'
        '<-- ['
        ' {"jsonrpc": "2.0", "error": {"code": -32600, "message": '
        '"Invalid Request."}, "id": null}, '
        ' {"jsonrpc": "2.0", "error": {"code": -32600, "message": '
        '"Invalid Request."}, "id": null}, '
        ' {"jsonrpc": "2.0", "error": {"code": -32600, "message": '
        '"Invalid Request."}, "id": null} '
        ']'
        ''
        'RPC call Batch:'
        '--> ['
        ' {"jsonrpc": "2.0", "method": "sum", "params": [1,2,4], "id": '
        '"1"},'
        ' {"jsonrpc": "2.0", "method": "notify_hello", "params": [7]},'
        ' {"jsonrpc": "2.0", "method": "subtract", "params": [42,23], '
        '"id": "2"},'
        ' {"foo": "boo"},'
        ' {"jsonrpc": "2.0", "method": "foo.get", "params": {"name": '
        '"myself"}, "id": "5"},'
        ' {"jsonrpc": "2.0", "method": "get_data", "id": "9"} '
        ']'
        '<-- ['
        ' {"jsonrpc": "2.0", "result": 7, "id": "1"},'
        ' {"jsonrpc": "2.0", "result": 19, "id": "2"},'
        ' {"jsonrpc": "2.0", "error": {"code": -32600, "message": '
        '"Invalid Request."}, "id": null},'
        ' {"jsonrpc": "2.0", "error": {"code": -32601, "message": '
        '"Method not found."}, "id": "5"},'
        ' {"jsonrpc": "2.0", "result": ["hello", 5], "id": "9"}'
        ']'
        ''
        'RPC call Batch (all notifications):'
        '--> ['
        ' {"jsonrpc": "2.0", "method": "notify_sum", "params": '
        '[1,2,4]},'
        ' {"jsonrpc": "2.0", "method": "notify_hello", "params": [7]},'
        ']'
        ''
        '')
      ParentFont = False
      ScrollBars = ssVertical
      TabOrder = 1
    end
  end
end
