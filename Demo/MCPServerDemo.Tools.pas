unit MCPServerDemo.Tools;

interface

uses
  System.Classes, System.SysUtils, System.JSON, Vcl.Graphics, Vcl.ExtCtrls,
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

    [McpTool('test_stream', 'Tests TStream as a Result', 'mime:')]
    function TestStream(): TStream;

    [McpTool('discounted_items', 'Retrieves a list of discounted items on Wintech-Italia based on the specified item type')]
    function GetDiscountedItems(
      [McpParam('itemType', 'The type of item to filter. Valid values: ''courses'', ''product'', ''consulting''')]
       const AItemType: string
    ): TStringList;


    [McpTool('course_image', 'Retrieves the image for the selected course', 'mime:image/bmp')]
    function GetImage(
      [McpParam('name', 'Course name')] const AName: string
    ): TPicture;




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

function TTestTool.GetImage(const AName: string): TPicture;
begin
  Result := TPicture.Create;
  Result.LoadFromFile('..\..\media\italy.bmp');
end;

function TTestTool.TestParam(AParam1: Int64; AParam2: Boolean): Integer;
begin
  Result := AParam1;
end;

function TTestTool.TestStream: TStream;
begin
  Result := TFileStream.Create('..\..\media\sample.pdf', fmOpenRead);
end;

end.
