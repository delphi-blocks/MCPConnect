unit MCPServer.Tools.Test;

interface

uses
  System.Classes, System.SysUtils, System.JSON, System.Generics.Collections,
  System.IOUtils, System.Rtti,

  {$IFDEF FRAMEWORK_VCL}
  Vcl.Graphics, Vcl.ExtCtrls, Vcl.Dialogs,
  {$ENDIF}

  Neon.Core.Persistence,
  Neon.Core.Persistence.JSON,

  MCPConnect.Configuration.MCP,
  MCPConnect.JRPC.Core,
  MCPConnect.JRPC.Classes,
  MCPConnect.MCP.Types,
  MCPConnect.MCP.Tools,
  MCPConnect.MCP.Attributes,
  MCPConnect.Session.Core;

type
  TPerson = class
  private
    FName: string;
    FDeveloper: Boolean;
  public
    constructor Create(const AName: string; ADev: Boolean);

    property Name: string read FName write FName;
    property Developer: Boolean read FDeveloper write FDeveloper;
  end;

  TPersons = class(TObjectList<TPerson>);

  //[McpScope('test')]
  TTestTool = class
  public
    //[McpTool('double_or_nothing', 'Doubles or zeroes the param value', 'category=group1,app=ui://delphiday/ticket-app,icon=money.png')]
    function TestParam(
      [McpParam('value1', 'Test Parameter 1 for MCP')] AValue: Int64;
      [McpParam('value2', 'Test Parameter 2 for MCP')] ADouble: Boolean
    ): Integer;

    [McpTool('test_structured', 'Test Structured Output', 'structured,icon=money.png')]
    function TestStructuredObject: TPerson;

    [McpTool('test_structured_array', 'Test Structured Output', 'icon=money.png')]
    function TestStructuredArray(): TPersons;

    //[McpTool('discounted_items', 'Retrieves a list of discounted items on Wintech-Italia based on the specified item type', 'icon=discount.png')]
    function GetDiscountedItems(
      [McpParam('itemType', 'The type of item to filter. Valid values: ''courses'', ''product'', ''consulting''')] const AItemType: string
    ): string;

    //[McpTool('splitstring', 'Gets the content by splitting the string (e.g. "hello,world" -> ["hello", "world"])', 'icon=tags.png')]
    function GetSplitString(
      [McpParam('value', 'The string to work with')] const AValue: string
    ): TContentList;

    //[McpTool('get-person', 'Get a person info given his name', 'icon=person.png,structured')]
    function GetPerson(
      [McpParam('name', 'The name of the person to get')] const AName: string
    ): TPerson;


    {$IFDEF FRAMEWORK_VCL}
    //[McpTool('course_image', 'Retrieves the image for the selected course', 'disabled, icon=photo.png')]
    function GetImage(
      [McpParam('name', 'Course name')] const AName: string
    ): TPicture;
    {$ENDIF}

  end;

  TTicketProgressNotification = class(TJRPCNotification)
  public
    constructor Create(APosition, ASize: Integer);
  end;

implementation

{ TTestTool }

function TTestTool.GetDiscountedItems(const AItemType: string): string;
begin
  //'courses'', ''product'', ''consulting';
  var LStringList := TStringList.Create;
  try
    if AItemType = 'courses' then
    begin
      LStringList.Add('Programmazione ad Oggetti con Delphi');
      LStringList.Add('Delphi Modern Development');
    end
    else if AItemType = 'product' then
    begin
      LStringList.Add('Fast Report');
      LStringList.Add('UniDAC');
    end
    else
      LStringList.Add('none');
    Result := LStringList.Text;
  finally
    LStringList.Free;
  end;
end;

{$IFDEF FRAMEWORK_VCL}
function TTestTool.GetImage(const AName: string): TPicture;
begin
  Result := TPicture.Create;
  Result.LoadFromFile(TPath.Combine(GetCurrentDir, 'data\italy.bmp'));
end;
{$ENDIF}

function TTestTool.GetPerson(const AName: string): TPerson;
begin
  Result := TPerson.Create(AName, True);
end;

function TTestTool.GetSplitString(const AValue: string): TContentList;
begin
  Result := TContentList.Create;

  var LStrings := AValue.Split([',']);
  for var LString in LStrings do
    Result.AddText(LString);
end;

function TTestTool.TestParam(AValue: Int64; ADouble: Boolean): Integer;
begin
  if ADouble then
    Result := AValue * 2
  else
    Result := 0;
end;

function TTestTool.TestStructuredArray: TPersons;
begin
  Result := TPersons.Create;
  Result.Add(TPerson.Create('Paolo', True));
  Result.Add(TPerson.Create('Lucia', False));
  Result.Add(TPerson.Create('Luca', True));
end;

function TTestTool.TestStructuredObject: TPerson;
begin
  Result := TPerson.Create('Paolo', True);
end;

{ TPerson }

constructor TPerson.Create(const AName: string; ADev: Boolean);
begin
  inherited Create;
  FName := AName;
  FDeveloper := ADev;
end;

{ TTicketProgressNotification }

constructor TTicketProgressNotification.Create(APosition, ASize: Integer);
begin
  inherited Create;
  Method := 'notification/logging';
  AddNamedParam('position', APosition);
  AddNamedParam('size', ASize);
end;

end.
