unit MCPServerDemo.Tools;

interface

uses
  System.Classes, System.SysUtils, System.JSON,
  MCPConnect.JRPC.Core,

  MCPConnect.MCP.Types,
  MCPConnect.MCP.Attributes;

type
  TTestTool = class
  public
    [McpTool('double_or_nothing', 'Doubles or zeroes the param value')]
    function TestParam(
    [McpParam('value1', 'Test Parameter 1 for MCP')] AParam1: Int64;
    [McpParam('value2', 'Test Parameter 2 for MCP')] AParam2: Boolean
    ): Integer;

    [McpTool('discounted_items', 'Retrieves a list of discounted items on Wintech-Italia based on the specified item type')]
    function GetDiscountedItems(
      [McpParam('itemType', 'The type of item to filter. Valid values: ''courses'', ''product'', ''consulting''')]
       const AItemType: string
    ): TStringList;
  end;

implementation

{ TTestTool }

function TTestTool.GetDiscountedItems(const AItemType: string): TStringList;
begin
  //'courses'', ''product'', ''consulting';
  Result := TStringList.Create;
  try
    if AItemType = 'courses' then
    begin
      Result.Add('Programmazione ad Oggetti con Delphi');
      Result.Add('Delphi Modern Development');
    end
    else if AItemType = 'product' then
    begin
      Result.Add('Fast Report');
      Result.Add('UniDAC');
    end
    else
      Result.Add('none');
  except
    Result.Free;
    raise;
  end;
end;

function TTestTool.TestParam(AParam1: Int64; AParam2: Boolean): Integer;
begin
  Result := AParam1;
end;

end.
