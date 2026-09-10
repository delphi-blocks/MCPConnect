{******************************************************************************}
{                                                                              }
{  Delphi MCP Connect Library                                                  }
{                                                                              }
{  Copyright (c) Paolo Rossi <dev@paolorossi.net>                              }
{                Luca Minuti <code@lucaminuti.it>                              }
{  All rights reserved.                                                        }
{                                                                              }
{  https://github.com/delphi-blocks/MCPConnect                                 }
{                                                                              }
{  Licensed under the MIT license                                              }
{                                                                              }
{******************************************************************************}

{
  The emit side of the "x-mcp-header" annotation: a "header=" tag on
  [McpParam] (or on WithParam) puts the annotation in the generated
  inputSchema, and the constraints the spec puts on its value are checked at
  registration.

  The validate side - comparing the Mcp-Param-* header a client sends against
  the annotated argument - is MCPConnect.Tests.Transport.RequestHeaders.
}
unit MCPConnect.Tests.MCP.ParamHeaders;

interface

uses
  System.SysUtils,
  System.JSON,
  DUnitX.TestFramework,

  MCPConnect.MCP.Server,
  MCPConnect.MCP.Attributes,
  MCPConnect.MCP.Types.Base,
  MCPConnect.MCP.Types.Tool,
  MCPConnect.MCP.Middleware.Headers,
  MCPConnect.Configuration.MCP;

type
  TRoutedTools = class
  public
    [McpTool('query', 'Runs a query in a region')]
    function Query(
      [McpParam('region', 'The region to run in', 'header=Region')] const region: string;
      [McpParam('sql', 'The statement')] const sql: string
    ): string;

    [McpTool('page', 'Reads a page')]
    function Page(
      [McpParam('index', 'The page index', 'header=Page-Index')] const index: Integer;
      [McpParam('verbose', 'Whether to expand', 'header=Verbose')] const verbose: Boolean
    ): string;
  end;

  /// <summary>A header name that is not an RFC 9110 token.</summary>
  TBadHeaderNameTools = class
  public
    [McpTool('query', 'Runs a query')]
    function Query(
      [McpParam('region', 'The region', 'header="A Region"')] const region: string
    ): string;
  end;

  /// <summary>Two parameters asking for the same header, cased differently.</summary>
  TDuplicateHeaderTools = class
  public
    [McpTool('query', 'Runs a query')]
    function Query(
      [McpParam('region', 'The region', 'header=Region')] const region: string;
      [McpParam('fallback', 'The fallback region', 'header=region')] const fallback: string
    ): string;
  end;

  /// <summary>A "number" parameter, which the spec excludes.</summary>
  TFloatHeaderTools = class
  public
    [McpTool('scale', 'Scales something')]
    function Scale(
      [McpParam('factor', 'The factor', 'header=Factor')] const factor: Double
    ): string;
  end;

  TRecordParam = record
    Value: string;
  end;

  /// <summary>A non-primitive parameter.</summary>
  TObjectHeaderTools = class
  public
    [McpTool('store', 'Stores something')]
    function Store(
      [McpParam('payload', 'The payload', 'header=Payload')] const payload: TRecordParam
    ): string;
  end;

  /// <summary>The programmatic path has to enforce the same rules.</summary>
  TProgrammaticTools = class
  public
    function Query(const region, sql: string): string;
  end;

  [TestFixture]
  TMCPHeaderParamNameRuleTest = class(TObject)
  public
    [Test]
    [TestCase('simple', 'Region,True')]
    [TestCase('hyphenated', 'Page-Index,True')]
    [TestCase('lowercase', 'region,True')]
    [TestCase('digits', 'X2,True')]
    [TestCase('token punctuation', 'X-Tenant_Id.v2,True')]
    [TestCase('space', 'A Region,False')]
    [TestCase('colon', 'X:Region,False')]
    [TestCase('slash', 'X/Region,False')]
    [TestCase('parenthesis', 'X(Region),False')]
    [TestCase('at sign', 'X@Region,False')]
    procedure TestNameRule(const AName: string; AExpected: Boolean);

    [Test]
    procedure TestEmptyNameIsRejected;
    [Test]
    procedure TestCommaIsRejected;
    [Test]
    procedure TestControlCharactersAreRejected;
    [Test]
    procedure TestNonAsciiIsRejected;
  end;

  /// <summary>
  ///   Every check is a registration-time one: a client's remedy for an
  ///   annotation it rejects is to drop the tool from tools/list, so a bad
  ///   value has to be refused where the developer can still act on it.
  /// </summary>
  [TestFixture]
  TMCPHeaderParamEmitTest = class(TObject)
  private
    FServer: TMCPServer;
    FConfig: IMCPConfig;

    /// <summary>The "x-mcp-header" annotation on AParamName, or '' when absent.</summary>
    function Annotation(const AToolName, AParamName: string): string;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestTagWritesTheAnnotation;
    [Test]
    procedure TestUntaggedParamCarriesNoAnnotation;
    [Test]
    procedure TestIntegerAndBooleanAreAllowed;
    [Test]
    procedure TestAnnotationIsWrittenVerbatim;

    [Test]
    procedure TestInvalidHeaderNameRaises;
    [Test]
    procedure TestDuplicateHeaderNameRaises;
    [Test]
    procedure TestNumberParamRaises;
    [Test]
    procedure TestNonPrimitiveParamRaises;

    [Test]
    procedure TestProgrammaticTagWritesTheAnnotation;
    [Test]
    procedure TestProgrammaticDuplicateRaises;
  end;

implementation

uses
  MCPConnect.MCP.Types.Tools;

{ tool classes }

function TRoutedTools.Query(const region: string; const sql: string): string;
begin
  Result := region + '/' + sql;
end;

function TRoutedTools.Page(const index: Integer; const verbose: Boolean): string;
begin
  Result := index.ToString;
end;

function TBadHeaderNameTools.Query(const region: string): string;
begin
  Result := region;
end;

function TDuplicateHeaderTools.Query(const region: string; const fallback: string): string;
begin
  Result := region;
end;

function TFloatHeaderTools.Scale(const factor: Double): string;
begin
  Result := '';
end;

function TObjectHeaderTools.Store(const payload: TRecordParam): string;
begin
  Result := payload.Value;
end;

function TProgrammaticTools.Query(const region, sql: string): string;
begin
  Result := region;
end;

{ TMCPHeaderParamNameRuleTest }

procedure TMCPHeaderParamNameRuleTest.TestNameRule(const AName: string;
  AExpected: Boolean);
begin
  Assert.AreEqual(AExpected, IsValidHeaderParamName(AName), AName);
end;

procedure TMCPHeaderParamNameRuleTest.TestEmptyNameIsRejected;
begin
  Assert.IsFalse(IsValidHeaderParamName(''), 'A header name MUST NOT be empty');
end;

procedure TMCPHeaderParamNameRuleTest.TestCommaIsRejected;
begin
  // Spelled out rather than given as a [TestCase], which splits on commas
  Assert.IsFalse(IsValidHeaderParamName('X,Region'));
end;

procedure TMCPHeaderParamNameRuleTest.TestControlCharactersAreRejected;
begin
  // CR and LF are named explicitly by the spec: a name carrying either would
  // let an annotation write a header of its own
  Assert.IsFalse(IsValidHeaderParamName('X' + #13 + 'Region'), 'CR');
  Assert.IsFalse(IsValidHeaderParamName('X' + #10 + 'Region'), 'LF');
  Assert.IsFalse(IsValidHeaderParamName('X' + #9 + 'Region'), 'HTAB');
  Assert.IsFalse(IsValidHeaderParamName('X' + #0 + 'Region'), 'NUL');
end;

procedure TMCPHeaderParamNameRuleTest.TestNonAsciiIsRejected;
begin
  Assert.IsFalse(IsValidHeaderParamName('Regi' + Char($F3) + 'n'));
end;

{ TMCPHeaderParamEmitTest }

procedure TMCPHeaderParamEmitTest.Setup;
begin
  FServer := TMCPServer.Create(nil);
  FConfig := FServer.Plugin.Configure<IMCPConfig>;
end;

procedure TMCPHeaderParamEmitTest.TearDown;
begin
  FConfig := nil;
  FServer.Free;
end;

function TMCPHeaderParamEmitTest.Annotation(const AToolName,
  AParamName: string): string;
var
  LTool: TMCPTool;
  LProperties, LParam: TJSONValue;
begin
  Result := '';

  if not FConfig.Tools.Registry.TryGetValue(AToolName, LTool) then
    Assert.Fail('Tool [' + AToolName + '] is not registered');

  LProperties := LTool.InputSchema.GetValue('properties');
  Assert.IsTrue(LProperties is TJSONObject, 'the schema must have properties');

  LParam := TJSONObject(LProperties).GetValue(AParamName);
  Assert.IsTrue(LParam is TJSONObject,
    'parameter [' + AParamName + '] must be in the schema');

  LParam := TJSONObject(LParam).GetValue(MCP_SCHEMA_HEADER_KEYWORD);
  if LParam is TJSONString then
    Result := TJSONString(LParam).Value;
end;

procedure TMCPHeaderParamEmitTest.TestTagWritesTheAnnotation;
begin
  FConfig.Tools.RegisterClass(TRoutedTools);

  Assert.AreEqual('Region', Annotation('query', 'region'),
    'The header= tag is what puts the annotation in the schema');
end;

procedure TMCPHeaderParamEmitTest.TestUntaggedParamCarriesNoAnnotation;
begin
  FConfig.Tools.RegisterClass(TRoutedTools);

  Assert.AreEqual('', Annotation('query', 'sql'),
    'A parameter that did not ask to be mirrored is left alone');
end;

procedure TMCPHeaderParamEmitTest.TestIntegerAndBooleanAreAllowed;
begin
  FConfig.Tools.RegisterClass(TRoutedTools);

  Assert.AreEqual('Page-Index', Annotation('page', 'index'));
  Assert.AreEqual('Verbose', Annotation('page', 'verbose'));
end;

procedure TMCPHeaderParamEmitTest.TestAnnotationIsWrittenVerbatim;
begin
  // The value is the name portion of Mcp-Param-<name>, so the prefix is not
  // part of what the annotation carries, and the casing is the author's
  FConfig.Tools.RegisterClass(TRoutedTools);

  Assert.AreEqual('Page-Index', Annotation('page', 'index'));
end;

procedure TMCPHeaderParamEmitTest.TestInvalidHeaderNameRaises;
begin
  Assert.WillRaise(
    procedure
    begin
      FConfig.Tools.RegisterClass(TBadHeaderNameTools);
    end,
    EMCPException, 'A name that is not a token would make a client drop the tool');
end;

procedure TMCPHeaderParamEmitTest.TestDuplicateHeaderNameRaises;
begin
  Assert.WillRaise(
    procedure
    begin
      FConfig.Tools.RegisterClass(TDuplicateHeaderTools);
    end,
    EMCPException, 'Uniqueness is case-insensitive within one inputSchema');
end;

procedure TMCPHeaderParamEmitTest.TestNumberParamRaises;
begin
  Assert.WillRaise(
    procedure
    begin
      FConfig.Tools.RegisterClass(TFloatHeaderTools);
    end,
    EMCPException, '"number" is excluded even though it is a primitive');
end;

procedure TMCPHeaderParamEmitTest.TestNonPrimitiveParamRaises;
begin
  Assert.WillRaise(
    procedure
    begin
      FConfig.Tools.RegisterClass(TObjectHeaderTools);
    end,
    EMCPException, 'Only a primitive can be mirrored into a header');
end;

procedure TMCPHeaderParamEmitTest.TestProgrammaticTagWritesTheAnnotation;
begin
  FConfig.Tools
    .RegisterTool(TProgrammaticTools, 'Query', 'query', 'Runs a query')
      .WithParam('region', 'region', 'The region', 'header=Region')
      .WithParam('sql', 'sql', 'The statement')
    .EndTool;

  Assert.AreEqual('Region', Annotation('query', 'region'),
    'The attribute-free path takes the same tag');
  Assert.AreEqual('', Annotation('query', 'sql'));
end;

procedure TMCPHeaderParamEmitTest.TestProgrammaticDuplicateRaises;
begin
  Assert.WillRaise(
    procedure
    begin
      FConfig.Tools
        .RegisterTool(TProgrammaticTools, 'Query', 'query', 'Runs a query')
          .WithParam('region', 'region', 'The region', 'header=Region')
          .WithParam('sql', 'sql', 'The statement', 'header=REGION')
        .EndTool;
    end,
    EMCPException, 'The attribute-free path goes through the same checks');
end;

initialization
  TDUnitX.RegisterTestFixture(TMCPHeaderParamNameRuleTest);
  TDUnitX.RegisterTestFixture(TMCPHeaderParamEmitTest);

end.
