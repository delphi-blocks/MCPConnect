unit MCPServer.Tools.Test;

{
  ==============================================================================
   MCPConnect demo - the serialization playground
  ==============================================================================

  Where MCPServer.Tools.pas tells the DelphiDay story, this unit exists to
  exercise the *return-type* machinery: what happens when a tool returns an
  object, a list of objects, a multi-part result or a VCL image, and how the
  JSON Schema published in tools/list is derived from it.

  It is NOT registered by default - the RegisterClass call is commented out in
  MCPServer.Config (.Tools section) to keep the demo tool list readable.
  Uncomment it when you want to see structured output or image content in an
  MCP client.

  Reading order, from the simplest to the most involved:

    TestParam            plain scalars in, scalar out
    GetDiscountedItems   string out, built from a TStringList
    GetSplitString       TContentList - several content blocks in one result
    TestStructuredObject 'structured' tag - an object also lands in structuredContent
    TestStructuredArray  the trap: an array cannot carry structuredContent
    GetPerson            structured output driven by a parameter
    GetImage             a VCL TPicture turned into image content by a writer

  Several methods are deliberately left without an [McpTool] attribute
  (TestParam, GetDiscountedItems, GetImage): they stay invisible to the
  protocol while remaining perfectly callable Delphi code, which is the point
  of attribute-driven registration. Restore the commented attribute above a
  method to publish it.
}

interface

uses
  System.Classes, System.SysUtils, System.JSON, System.Generics.Collections,
  System.IOUtils, System.Rtti,

  // FRAMEWORK_VCL is defined by the VCL hosts of this demo (Indy, WebBroker)
  // and absent from the console/service ones, so the image tool below compiles
  // out where TPicture does not exist.
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
  /// <summary>
  ///   A minimal object result. Neon serializes the published properties, and
  ///   the same reflection produces the JSON Schema shown in tools/list -
  ///   which is why a well-named property is also good prompt engineering.
  /// </summary>
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

  /// <summary>
  ///   Scope 'test', so the published names are test_structured_output,
  ///   test_splitstring and so on.
  /// </summary>
  [McpScope('test')]
  TTestTool = class
  public
    /// <summary>
    ///   Undecorated on purpose - see the unit header. Restore the attribute
    ///   to publish it; note the tag string carrying three different kinds of
    ///   metadata at once (a category, an app link and an icon).
    /// </summary>
    //[McpTool('double_or_nothing', 'Doubles or zeroes the param value', 'category=group1,app=ui://delphiday/ticket-app,icon=money.png')]
    function TestParam(
      [McpParam('value1', 'Test Parameter 1 for MCP')] AValue: Int64;
      [McpParam('value2', 'Test Parameter 2 for MCP')] ADouble: Boolean
    ): Integer;

    /// <summary>
    ///   The 'structured' tag makes MCPConnect publish an outputSchema for the
    ///   tool and repeat the serialized result in structuredContent, next to
    ///   the human-readable text block. Clients that understand it can then
    ///   consume the result as data rather than re-parsing prose.
    ///   It only works for object results - see TestStructuredArray.
    /// </summary>
    [McpTool('structured_output', 'Test Structured Output', 'structured,icon=money.png')]
    function TestStructuredObject: TPerson;

    /// <summary>
    ///   The counter-example: this returns a JSON *array*, so it is NOT tagged
    ///   'structured'. Adding the tag would raise at registration time, because
    ///   outputSchema and structuredContent are restricted to objects (see
    ///   TMCPToolsConfig.WriteOutputSchema in
    ///   Source\MCPConnect.Configuration.MCP.pas).
    ///   To get structured output from a list, wrap it in an object with the
    ///   list as one of its properties.
    /// </summary>
    [McpTool('test_structured_array', 'Test Structured Output', 'icon=money.png')]
    function TestStructuredArray(): TPersons;

    /// <summary>
    ///   Undecorated. Kept as an example of documenting an enumerated
    ///   parameter in prose, inside the [McpParam] description, since the
    ///   schema type is only 'string'.
    /// </summary>
    //[McpTool('discounted_items', 'Retrieves a list of discounted items on Wintech-Italia based on the specified item type', 'icon=discount.png')]
    function GetDiscountedItems(
      [McpParam('itemType', 'The type of item to filter. Valid values: ''courses'', ''product'', ''consulting''')] const AItemType: string
    ): string;

    /// <summary>
    ///   Returns several content blocks from one call: each AddText becomes a
    ///   separate entry in the result content array. The same list also
    ///   accepts AddImage, AddAudio, AddBlob and AddLink, so text and binary
    ///   parts can be mixed freely.
    /// </summary>
    [McpTool('splitstring', 'Gets the content by splitting the string (e.g. "hello,world" -> ["hello", "world"])', 'icon=tags.png')]
    function GetSplitString(
      [McpParam('value', 'The string to work with')] const AValue: string
    ): TContentList;

    /// <summary>
    ///   Structured output again, this time driven by an input parameter -
    ///   the shape a real lookup tool would have.
    /// </summary>
    [McpTool('get-person', 'Get a person info given his name', 'icon=person.png,structured')]
    function GetPerson(
      [McpParam('name', 'The name of the person to get')] const AName: string
    ): TPerson;


    {$IFDEF FRAMEWORK_VCL}
    /// <summary>
    ///   Returns a VCL TPicture. That works only because
    ///   MCPServer.Config registers TMCPPictureWriter with
    ///   Server.RegisterWriter: the writer converts the picture to PNG and
    ///   emits an image content block. Without a writer for the type, the call
    ///   would fail at serialization.
    ///   The 'disabled' tag in the commented attribute is also worth noting -
    ///   it keeps a registered tool out of tools/list without deleting it.
    /// </summary>
    //[McpTool('course_image', 'Retrieves the image for the selected course', 'disabled, icon=photo.png')]
    function GetImage(
      [McpParam('name', 'Course name')] const AName: string
    ): TPicture;
    {$ENDIF}

  end;

  /// <summary>
  ///   Dead code: an exact duplicate of the class with the same name in
  ///   MCPServer.Tools.pas. Nothing in this unit references it, and because
  ///   MCPServer.Tools.Test comes later in the MCPServer.Config uses clause it
  ///   silently shadows the original. Safe to delete.
  /// </summary>
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
  // The returned object is freed by the framework once the writer has turned
  // it into content, so there is no try/finally here.
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

  // One content block per fragment: the client receives an array, not a
  // single string with separators in it.
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
  // A TObjectList result serializes to a JSON array and its items are freed
  // with the list.
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
