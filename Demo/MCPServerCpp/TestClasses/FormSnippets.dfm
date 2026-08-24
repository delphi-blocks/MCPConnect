object frmSnippets: TfrmSnippets
  Left = 0
  Top = 0
  Caption = 'JSON_RPC Snippets'
  ClientHeight = 512
  ClientWidth = 751
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 15
  object memoSnippets: TMemo
    Left = 0
    Top = 0
    Width = 751
    Height = 512
    Align = alClient
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Consolas'
    Font.Style = []
    Lines.Strings = (
      'Syntax:'
      '--> Client -> Server'
      '<-- Server -> Client'
      ''
      ''
      '************************'
      'RPC call with positional parameters:'
      
        '--> {"jsonrpc": "2.0", "method": "subtract", "params": [42, 23],' +
        ' "id": 1}'
      '<-- {"jsonrpc": "2.0", "result": 19, "id": 1}'
      ''
      
        '--> {"jsonrpc": "2.0", "method": "subtract", "params": [23, 42],' +
        ' "id": 2}'
      '<-- {"jsonrpc": "2.0", "result": -19, "id": 2}'
      ''
      
        '--> {"jsonrpc": "2.0", "method": "getname", "params": [{"name": ' +
        '"luca"}], "id": 2}'
      ''
      
        '{"jsonrpc": "2.0", "method": "createperson", "params": ["luca"],' +
        ' "id": 2}'
      ''
      '************************'
      'RPC call with named parameters:'
      '--> {"jsonrpc": "2.0", "method": "subtract", "params":'
      '{"subtrahend": 23, "minuend": 42}, "id": 3}'
      '<-- {"jsonrpc": "2.0", "result": 19, "id": 3}'
      '--> {"jsonrpc": "2.0", "method": "subtract", "params":'
      '{"minuend": 42, "subtrahend": 23}, "id": 4}'
      '<-- {"jsonrpc": "2.0", "result": 19, "id": 4}'
      ''
      '--> {"jsonrpc": "2.0", "method": "getname", "params": '
      '{"person": {"name": "luca"}}, "id": 2}'
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
      '************************'
      'RPC call with invalid JSON:'
      '--> {"jsonrpc": "2.0", "method": "foobar, "params": "bar", '
      '"baz]'
      '<-- {"jsonrpc": "2.0", "error": {"code": -32700, "message": '
      '"Parse error."}, "id": null}'
      ''
      '************************'
      'RPC call with invalid Request object:'
      '--> {"jsonrpc": "2.0", "method": 1, "params": "bar"}'
      '<-- {"jsonrpc": "2.0", "error": {"code": -32600, "message": '
      '"Invalid Request."}, "id": null}'
      ''
      '************************'
      'RPC call Batch, invalid JSON:'
      '--> [ {"jsonrpc": "2.0", "method": "sum", "params": [1,2,4], '
      '"id": "1"},{"jsonrpc": "2.0", "method" ]'
      '<-- {"jsonrpc": "2.0", "error": {"code": -32700, "message": '
      '"Parse error."}, "id": null}'
      ''
      '************************'
      'RPC call with an empty Array:'
      '--> [] '
      '<-- "jsonrpc": "2.0", "error": {"code": -32600, "message": '
      '"Invalid Request."}, "id": null}'
      ''
      '************************'
      'RPC call with an invalid Batch (but not empty):'
      '--> [1]'
      '<-- [ {"jsonrpc": "2.0", "error": {"code": -32600, "message": '
      '"Invalid Request."}, "id": null} ]'
      ''
      '************************'
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
      '************************'
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
      '************************'
      'RPC call Batch (all notifications):'
      '--> ['
      ' {"jsonrpc": "2.0", "method": "notify_sum", "params": '
      '[1,2,4]},'
      ' {"jsonrpc": "2.0", "method": "notify_hello", "params": [7]},'
      ']')
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 0
    WordWrap = False
  end
end
