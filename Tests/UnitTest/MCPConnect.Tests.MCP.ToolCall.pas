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
unit MCPConnect.Tests.MCP.ToolCall;

{
  End-to-end tools/call tests through TMCPToolsApi:

  * structured (record / class) arguments must be bound with the same Neon
    configuration used to generate the inputSchema (camelCase by default), so
    that a client following the schema actually gets its values through;
  * array results must be returned as JSON text content, not as an embedded
    blob (MCP blobs are base64 data).
}

interface

uses
  System.SysUtils, System.JSON,
  DUnitX.TestFramework,

  MCPConnect.JRPC.Classes,
  MCPConnect.JRPC.Core,
  MCPConnect.JRPC.Server,
  MCPConnect.Configuration.MCP,
  MCPConnect.MCP.Types,
  MCPConnect.MCP.Tools,
  MCPConnect.MCP.Attributes,
  MCPConnect.MCP.Authorization,
  MCPConnect.MCP.Server.Api;

type
  TToolCallPoint = record
    X: Double;
    Y: Double;
    Z: Double;
  end;

  TToolCallPerson = class
  private
    FFirstName: string;
    FBirthYear: Integer;
  public
    property FirstName: string read FFirstName write FFirstName;
    property BirthYear: Integer read FBirthYear write FBirthYear;
  end;

  TToolCallService = class
  public
    [McpTool('point_sum', 'Sum of the coordinates of a point')]
    function PointSum([McpParam('point', 'A point')] const APoint: TToolCallPoint): Double;

    [McpTool('points_count', 'Number of points and sum of their X')]
    function PointsCount([McpParam('points', 'Points')] const APoints: TArray<TToolCallPoint>): string;

    [McpTool('person_label', 'Label for a person')]
    function PersonLabel([McpParam('person', 'A person')] APerson: TToolCallPerson): string;

    [McpTool('numbers', 'An array result')]
    function Numbers: TArray<Integer>;
  end;

  [TestFixture]
  TMCPToolCallTest = class(TObject)
  private
    FServer: TJRPCServer;
    FConfig: IMCPConfig;
    FGarbage: IGarbageCollector;
    FToken: TMCPAccessToken;
    FContext: TJRPCContext;

    function CallTool(const AName, AArgumentsJSON: string): TCallToolResult;
    function CallToolText(const AName, AArgumentsJSON: string): string;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestRecordArgument_CamelCaseKeys_AreBound;
    [Test]
    procedure TestRecordArrayArgument_CamelCaseKeys_AreBound;
    [Test]
    procedure TestClassArgument_CamelCaseKeys_AreBound;
    [Test]
    procedure TestInputSchema_UsesCamelCaseMembers;
    [Test]
    procedure TestArrayResult_IsJSONTextContent;
  end;

implementation

uses
  System.Generics.Collections;

{ TToolCallService }

function TToolCallService.PointSum(const APoint: TToolCallPoint): Double;
begin
  Result := APoint.X + APoint.Y + APoint.Z;
end;

function TToolCallService.PointsCount(const APoints: TArray<TToolCallPoint>): string;
var
  LSum: Double;
  LPoint: TToolCallPoint;
begin
  LSum := 0;
  for LPoint in APoints do
    LSum := LSum + LPoint.X;
  Result := Format('%d:%d', [Length(APoints), Round(LSum)]);
end;

function TToolCallService.PersonLabel(APerson: TToolCallPerson): string;
begin
  Result := Format('%s-%d', [APerson.FirstName, APerson.BirthYear]);
end;

function TToolCallService.Numbers: TArray<Integer>;
begin
  Result := [1, 2, 3];
end;

{ TMCPToolCallTest }

procedure TMCPToolCallTest.Setup;
begin
  FServer := TJRPCServer.Create(nil);
  FConfig := FServer.Plugin.Configure<IMCPConfig>;
  FConfig.Tools.RegisterClass(TToolCallService);

  FGarbage := TGarbageCollector.CreateInstance;
  FToken := TMCPAccessToken.Create;

  FContext := TJRPCContext.Create;
  FContext.AddContent(FServer);
  FContext.AddContent(FGarbage);
  FContext.AddContent(FToken);
end;

procedure TMCPToolCallTest.TearDown;
begin
  FContext.Free;
  FToken.Free;
  FGarbage := nil;
  FConfig := nil;
  FServer.Free;
end;

function TMCPToolCallTest.CallTool(const AName, AArgumentsJSON: string): TCallToolResult;
var
  LApi: TMCPToolsApi;
  LParams: TCallToolParams;
begin
  LApi := TMCPToolsApi.Create;
  LParams := TCallToolParams.Create;
  try
    LApi.RPCContext := FContext;
    LApi.MCPConfig := FServer.GetConfiguration<TMCPConfig>;
    LParams.Name := AName;
    LParams.Arguments.Free;
    LParams.Arguments := TJSONObject.ParseJSONValue(AArgumentsJSON) as TJSONObject;
    Result := LApi.CallTool(LParams);
  finally
    LParams.Free;
    LApi.Free;
  end;
end;

function TMCPToolCallTest.CallToolText(const AName, AArgumentsJSON: string): string;
var
  LResult: TCallToolResult;
begin
  LResult := CallTool(AName, AArgumentsJSON);
  try
    Assert.AreEqual(1, LResult.Content.Count);
    Assert.IsTrue(LResult.Content[0] is TTextContent,
      'Expected text content, got ' + LResult.Content[0].ClassName);
    Result := TTextContent(LResult.Content[0]).Text;
  finally
    LResult.Free;
  end;
end;

procedure TMCPToolCallTest.TestRecordArgument_CamelCaseKeys_AreBound;
begin
  // Before the fix the keys were matched case-sensitively against "X", "Y",
  // "Z": every coordinate silently stayed 0
  Assert.AreEqual('6', CallToolText('point_sum', '{"point": {"x": 1, "y": 2, "z": 3}}'));
end;

procedure TMCPToolCallTest.TestRecordArrayArgument_CamelCaseKeys_AreBound;
begin
  Assert.AreEqual('2:11', CallToolText('points_count',
    '{"points": [{"x": 1, "y": 0, "z": 0}, {"x": 10, "y": 0, "z": 0}]}'));
end;

procedure TMCPToolCallTest.TestClassArgument_CamelCaseKeys_AreBound;
begin
  Assert.AreEqual('Ada-1815', CallToolText('person_label',
    '{"person": {"firstName": "Ada", "birthYear": 1815}}'));
end;

procedure TMCPToolCallTest.TestInputSchema_UsesCamelCaseMembers;
var
  LTool: TMCPTool;
  LSchema: string;
begin
  LTool := FServer.GetConfiguration<TMCPConfig>.Tools.Registry['person_label'];
  LSchema := LTool.InputSchema.ToJSON;
  Assert.Contains(LSchema, '"firstName"');
  Assert.Contains(LSchema, '"birthYear"');
end;

procedure TMCPToolCallTest.TestArrayResult_IsJSONTextContent;
begin
  Assert.AreEqual('[1,2,3]', CallToolText('numbers', '{}'));
end;

initialization
  TDUnitX.RegisterTestFixture(TMCPToolCallTest);

end.
