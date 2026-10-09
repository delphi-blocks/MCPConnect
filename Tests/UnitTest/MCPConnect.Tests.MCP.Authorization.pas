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

/// <summary>
///   Scope-based authorization of tools, resources, templates, App UIs and
///   prompts: [McpRequiredScope], RequireScope, the default authorizer and the
///   custom ones. The operations are driven through TMCPTransportHandler with a
///   token validator describing the caller, so that what is covered is the
///   whole path from the Authorization header to the filtered answer.
/// </summary>
unit MCPConnect.Tests.MCP.Authorization;

interface

uses
  System.SysUtils, System.JSON,
  DUnitX.TestFramework,

  JRPC.Core,
  JRPC.Classes,

  MCPConnect.Configuration.Auth,
  MCPConnect.Configuration.Legacy,
  MCPConnect.Configuration.MCP,
  MCPConnect.MCP.Attributes,
  MCPConnect.MCP.Authorization,
  MCPConnect.MCP.Server,
  MCPConnect.MCP.Server.Api,
  MCPConnect.MCP.Types.Base,
  MCPConnect.MCP.Types.Notifications,
  MCPConnect.MCP.Types.Subscriptions,
  MCPConnect.Transport.Base;

type
  [McpRequiredScope('orders:read')]
  TScopedService = class
  public
    [McpTool('list_orders', 'Lists the orders')]
    function ListOrders: string;

    [McpTool('delete_order', 'Deletes an order')]
    [McpRequiredScope('orders:write; orders:admin')]
    function DeleteOrder: string;

    [McpResource('orders', 'res://orders', 'text/plain', 'All the orders')]
    function GetOrders: string;

    [McpTemplate('order', 'res://orders/{id}', 'text/plain', 'One order')]
    function GetOrder([McpParam('id', 'Order id')] const AId: string): string;

    [McpAppUI('orders_ui', 'ui://orders', 'Orders UI')]
    function GetOrdersUI: string;

    [McpPrompt('summarize_orders', 'Summarize', 'Summarizes the orders')]
    function SummarizeOrders: string;
  end;

  TPublicService = class
  public
    [McpTool('ping', 'Answers pong')]
    function Ping: string;

    [McpPrompt('greet', 'Greet', 'Greets the user')]
    function Greet: string;
  end;

  TDenyAllAuthorizer = class(TInterfacedObject, IMCPAuthorizer)
  public
    function Authorize(AContext: TJRPCContext; const AItem: TMCPAuthItem;
      AIdentity: TMCPAccessToken): Boolean;
  end;

  /// <summary>A response writer that streams nothing.</summary>
  TSilentAuthorizationWriter = class(TInterfacedObject, IMCPTransportWriter)
  public
    procedure Write(const AValue: string);
    function Connected: Boolean;
    function SupportsStreaming: Boolean;
  end;

  [TestFixture]
  TMCPScopeListTest = class(TObject)
  public
    [Test]
    procedure TestParse_SplitsOnCommasAndSemicolons_TrimsAndDropsDuplicates;
    [Test]
    procedure TestParse_EmptyStringGivesEmptyList;
    [Test]
    procedure TestHasScope_MatchesWholeCaseSensitiveScopesOnly;
    [Test]
    procedure TestHasScope_ReadsEntraScpToo;
    [Test]
    procedure TestHasScopes_EmptyListIsAlwaysSatisfied;
    [Test]
    procedure TestHasScopes_RequiresAllAndReportsTheMissingOnes;
  end;

  [TestFixture]
  TMCPRequiredScopeConfigTest = class(TObject)
  private
    FServer: TMCPServer;
    FConfig: IMCPConfig;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestAttribute_ClassAndMethodScopesAreMerged;
    [Test]
    procedure TestAttribute_AppliesToResourcesTemplatesUIAndPrompts;
    [Test]
    procedure TestAttribute_NoAttributeMeansNoScopes;
    [Test]
    procedure TestAttribute_HonoredByProgrammaticRegistration;
    [Test]
    procedure TestRegisterTool_RequireScopeOnTheBuilder;
    [Test]
    procedure TestRequireScope_AddsToAnyRegisteredItem;
    [Test]
    procedure TestRequireScope_UnknownItemRaises;
    [Test]
    procedure TestSetAuthorizerClass_ClassWithoutInterfaceRaises;
  end;

  [TestFixture]
  TMCPAccessGuardTest = class(TObject)
  public
    [Test]
    procedure TestNoTokenInContext_FailsClosed;
    [Test]
    procedure TestCheck_ItemWithoutScopesIsAlwaysAllowed;
  end;

  /// <summary>
  ///   The MCP operations, driven through the transport. The token is the list
  ///   of scopes the caller has, comma separated; NoScopes stands for none.
  /// </summary>
  [TestFixture]
  TMCPAuthorizationApiTest = class(TObject)
  private const
    NoScopes = '-';
    AllScopes = 'orders:read,orders:write,orders:admin';
  private
    FServer: TMCPServer;
    FScopes: string;

    procedure ConfigureServer(ALegacy: Boolean = False);
    function MCPConfig: IMCPConfig;

    /// <summary>POSTs a JSON-RPC request and returns the whole answer. Caller owns it.</summary>
    function Call(const AMethod: string; const AParams: string = '{}'): TJSONObject;
    function ResultOf(const AMethod: string; const AParams: string = '{}'): TJSONObject;
    /// <summary>The error message of the answer, after checking it is -32602.</summary>
    function InvalidParamsMessage(const AMethod, AParams: string): string;

    /// <summary>The AKey of every item in AMember, across every page, sorted.</summary>
    function ListAll(const AMethod, AMember, AKey: string): string;

    function ListTools: string;
    function ListResources: string;
    function ListTemplates: string;
    function ListPrompts: string;
    function CallToolError(const AName: string): string;
    function ReadResourceError(const AUri: string): string;
    function GetPromptError(const AName: string): string;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestToolsList_NoScopes_HidesScopedTools;
    [Test]
    procedure TestToolsList_PartialScopes_ShowsOnlyTheSatisfiedOnes;
    [Test]
    procedure TestToolsList_AllScopes_ShowsEverything;
    [Test]
    procedure TestCallTool_Denied_LooksLikeAMissingTool;
    [Test]
    procedure TestCallTool_Allowed_Runs;

    [Test]
    procedure TestResourcesList_FiltersResourcesAndUI;
    [Test]
    procedure TestTemplatesList_FiltersTemplates;
    [Test]
    procedure TestReadResource_Denied_LooksLikeAMissingResource;
    [Test]
    procedure TestReadTemplate_Denied_LooksLikeAMissingResource;

    [Test]
    procedure TestPromptList_FiltersPrompts;
    [Test]
    procedure TestGetPrompt_Denied_LooksLikeAMissingPrompt;

    [Test]
    procedure TestComplete_DeniedPrompt_LooksLikeAMissingPrompt;
    [Test]
    procedure TestComplete_DeniedTemplate_LooksLikeAMissingResource;
    [Test]
    procedure TestComplete_Allowed_Answers;

    [Test]
    procedure TestPaging_FilteredBeforeThePageIsCut;

    [Test]
    procedure TestLegacyClient_IsFilteredToo;

    [Test]
    procedure TestSetAuthorizer_CustomRuleDecidesAlone;
    [Test]
    procedure TestSetAuthorizer_ReceivesItemDetailsAndIdentity;
    [Test]
    procedure TestSetAuthorizer_CanCombineWithTheDefaultCheck;
    [Test]
    procedure TestSetAuthorizer_ExceptionCountsAsDenial;
    [Test]
    procedure TestSetAuthorizer_NilRestoresTheDefault;
    [Test]
    procedure TestSetAuthorizerClass_IsUsed;
  end;

  /// <summary>
  ///   subscriptions/listen, called directly: its acknowledgement goes to the
  ///   message queue, not into the answer.
  /// </summary>
  [TestFixture]
  TMCPAuthorizationListenTest = class(TObject)
  private
    FServer: TMCPServer;
    FApi: TMCPSubscriptionsApi;
    FContext: TJRPCContext;
    FGarbage: IGarbageCollector;
    FQueue: TMCPMessageQueue;
    FRequest: TJRPCRequest;
    FToken: TMCPAccessToken;

    /// <summary>The resource uris the acknowledgement kept, sorted.</summary>
    function AcknowledgedUris(const AUris: TArray<string>): string;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestNoScopes_DropsTheProtectedUris;
    [Test]
    procedure TestWithScopes_KeepsThem;
  end;

implementation

uses
  System.Generics.Collections;

// The answers are sorted by key already; sorting here keeps the expectations
// independent of that
function JoinSorted(const ANames: TArray<string>): string;
var
  LNames: TArray<string>;
  LName: string;
begin
  LNames := Copy(ANames);
  TArray.Sort<string>(LNames);

  Result := '';
  for LName in LNames do
    Result := Result + LName + ';';
end;

{ TScopedService }

function TScopedService.ListOrders: string;
begin
  Result := 'orders';
end;

function TScopedService.DeleteOrder: string;
begin
  Result := 'deleted';
end;

function TScopedService.GetOrders: string;
begin
  Result := 'orders';
end;

function TScopedService.GetOrder(const AId: string): string;
begin
  Result := 'order ' + AId;
end;

function TScopedService.GetOrdersUI: string;
begin
  Result := '<html></html>';
end;

function TScopedService.SummarizeOrders: string;
begin
  Result := 'Summarize the orders';
end;

{ TPublicService }

function TPublicService.Ping: string;
begin
  Result := 'pong';
end;

function TPublicService.Greet: string;
begin
  Result := 'Hello';
end;

{ TDenyAllAuthorizer }

function TDenyAllAuthorizer.Authorize(AContext: TJRPCContext;
  const AItem: TMCPAuthItem; AIdentity: TMCPAccessToken): Boolean;
begin
  Result := False;
end;

{ TSilentAuthorizationWriter }

procedure TSilentAuthorizationWriter.Write(const AValue: string);
begin
end;

function TSilentAuthorizationWriter.Connected: Boolean;
begin
  Result := True;
end;

function TSilentAuthorizationWriter.SupportsStreaming: Boolean;
begin
  Result := False;
end;

{ TMCPScopeListTest }

procedure TMCPScopeListTest.TestParse_SplitsOnCommasAndSemicolons_TrimsAndDropsDuplicates;
var
  LScopes: TArray<string>;
begin
  LScopes := TMCPScopeList.Parse(' a, b;c ,, a ; ');

  Assert.AreEqual(3, Length(LScopes));
  Assert.AreEqual('a', LScopes[0]);
  Assert.AreEqual('b', LScopes[1]);
  Assert.AreEqual('c', LScopes[2]);
end;

procedure TMCPScopeListTest.TestParse_EmptyStringGivesEmptyList;
begin
  Assert.AreEqual(0, Length(TMCPScopeList.Parse('')));
  Assert.AreEqual(0, Length(TMCPScopeList.Parse(' ; , ')));
end;

procedure TMCPScopeListTest.TestHasScope_MatchesWholeCaseSensitiveScopesOnly;
var
  LToken: TMCPAccessToken;
begin
  LToken := TMCPAccessToken.Create;
  try
    LToken.Scope := 'orders:read  orders:write';

    Assert.IsTrue(LToken.HasScope('orders:read'));
    Assert.IsTrue(LToken.HasScope('orders:write'));
    Assert.IsFalse(LToken.HasScope('orders'), 'A prefix is not a scope');
    Assert.IsFalse(LToken.HasScope('Orders:read'), 'Scopes are case-sensitive');
    Assert.IsFalse(LToken.HasScope(''));
  finally
    LToken.Free;
  end;
end;

procedure TMCPScopeListTest.TestHasScope_ReadsEntraScpToo;
var
  LToken: TMCPAccessToken;
begin
  LToken := TMCPAccessToken.Create;
  try
    LToken.FromString('{"scp":"orders:read orders:write"}');

    Assert.IsTrue(LToken.HasScopes(['orders:read', 'orders:write']),
      'An Entra ID token carries its scopes in "scp"');
  finally
    LToken.Free;
  end;
end;

procedure TMCPScopeListTest.TestHasScopes_EmptyListIsAlwaysSatisfied;
var
  LToken: TMCPAccessToken;
begin
  LToken := TMCPAccessToken.Create;
  try
    Assert.IsTrue(LToken.HasScopes([]), 'A token without scopes satisfies an empty list');
  finally
    LToken.Free;
  end;
end;

procedure TMCPScopeListTest.TestHasScopes_RequiresAllAndReportsTheMissingOnes;
var
  LToken: TMCPAccessToken;
  LMissing: TArray<string>;
begin
  LToken := TMCPAccessToken.Create;
  try
    LToken.Scope := 'a b';

    Assert.IsTrue(LToken.HasScopes(['a', 'b']));
    Assert.IsFalse(LToken.HasScopes(['a', 'c']));

    LMissing := LToken.MissingScopes(['c', 'a', 'd']);
    Assert.AreEqual(2, Length(LMissing));
    Assert.AreEqual('c', LMissing[0]);
    Assert.AreEqual('d', LMissing[1]);
  finally
    LToken.Free;
  end;
end;

{ TMCPRequiredScopeConfigTest }

procedure TMCPRequiredScopeConfigTest.Setup;
begin
  FServer := TMCPServer.Create(nil);
  FConfig := FServer.Plugin.Configure<IMCPConfig>;
end;

procedure TMCPRequiredScopeConfigTest.TearDown;
begin
  FConfig := nil;
  FServer.Free;
end;

procedure TMCPRequiredScopeConfigTest.TestAttribute_ClassAndMethodScopesAreMerged;
var
  LScopes: TArray<string>;
begin
  FConfig.Tools.RegisterClass(TScopedService);

  LScopes := FConfig.Tools.Registry['list_orders'].RequiredScopes;
  Assert.AreEqual(1, Length(LScopes));
  Assert.AreEqual('orders:read', LScopes[0]);

  LScopes := FConfig.Tools.Registry['delete_order'].RequiredScopes;
  Assert.AreEqual(3, Length(LScopes));
  Assert.AreEqual('orders:read', LScopes[0]);
  Assert.AreEqual('orders:write', LScopes[1]);
  Assert.AreEqual('orders:admin', LScopes[2]);
end;

procedure TMCPRequiredScopeConfigTest.TestAttribute_AppliesToResourcesTemplatesUIAndPrompts;
begin
  FConfig.Resources.RegisterClass(TScopedService);
  FConfig.Prompts.RegisterClass(TScopedService);

  Assert.AreEqual(1, Length(FConfig.Resources.Registry['res://orders'].RequiredScopes), 'Resource');
  Assert.AreEqual(1, Length(FConfig.Resources.Registry['ui://orders'].RequiredScopes), 'App UI');
  Assert.AreEqual(1, Length(FConfig.Resources.TemplateRegistry['res://orders/{id}'].RequiredScopes), 'Template');
  Assert.AreEqual(1, Length(FConfig.Prompts.Registry['summarize_orders'].RequiredScopes), 'Prompt');
end;

procedure TMCPRequiredScopeConfigTest.TestAttribute_NoAttributeMeansNoScopes;
begin
  FConfig.Tools.RegisterClass(TPublicService);
  FConfig.Prompts.RegisterClass(TPublicService);

  Assert.AreEqual(0, Length(FConfig.Tools.Registry['ping'].RequiredScopes));
  Assert.AreEqual(0, Length(FConfig.Prompts.Registry['greet'].RequiredScopes));
end;

procedure TMCPRequiredScopeConfigTest.TestAttribute_HonoredByProgrammaticRegistration;
begin
  // Leaving the attributes out here would open what they close
  FConfig.Tools.RegisterTool(TScopedService, 'DeleteOrder', 'manual_delete', 'Delete').EndTool;
  FConfig.Resources
    .RegisterResource(TScopedService, 'GetOrders', 'manual_orders', 'res://manual')
    .RegisterTemplate(TScopedService, 'GetOrder', 'manual_order', 'res://manual/{id}', ['id'])
    .RegisterUI(TScopedService, 'GetOrdersUI', 'manual_ui', 'ui://manual');
  FConfig.Prompts.RegisterPrompt(TScopedService, 'SummarizeOrders', 'manual_summary', []);

  Assert.AreEqual(3, Length(FConfig.Tools.Registry['manual_delete'].RequiredScopes), 'Tool');
  Assert.AreEqual(1, Length(FConfig.Resources.Registry['res://manual'].RequiredScopes), 'Resource');
  Assert.AreEqual(1, Length(FConfig.Resources.TemplateRegistry['res://manual/{id}'].RequiredScopes), 'Template');
  Assert.AreEqual(1, Length(FConfig.Resources.Registry['ui://manual'].RequiredScopes), 'App UI');
  Assert.AreEqual(1, Length(FConfig.Prompts.Registry['manual_summary'].RequiredScopes), 'Prompt');
end;

procedure TMCPRequiredScopeConfigTest.TestRegisterTool_RequireScopeOnTheBuilder;
var
  LScopes: TArray<string>;
begin
  FConfig.Tools.RegisterTool(TPublicService, 'Ping', 'manual_ping', 'Ping')
    .RequireScope('x, y')
    .RequireScope('y;z')
    .EndTool;

  LScopes := FConfig.Tools.Registry['manual_ping'].RequiredScopes;
  Assert.AreEqual(3, Length(LScopes));
  Assert.AreEqual('x', LScopes[0]);
  Assert.AreEqual('y', LScopes[1]);
  Assert.AreEqual('z', LScopes[2]);
end;

procedure TMCPRequiredScopeConfigTest.TestRequireScope_AddsToAnyRegisteredItem;
begin
  FConfig.Tools.RegisterClass(TScopedService).RegisterClass(TPublicService);
  FConfig.Resources.RegisterClass(TScopedService);
  FConfig.Prompts.RegisterClass(TPublicService);

  FConfig.Tools
    .RequireScope('ping', 'extra')
    .RequireScope('list_orders', 'extra');
  FConfig.Resources
    .RequireScope('ui://orders', 'extra')
    .RequireScope('res://orders/{id}', 'extra');
  FConfig.Prompts.RequireScope('greet', 'extra');

  Assert.AreEqual(1, Length(FConfig.Tools.Registry['ping'].RequiredScopes));
  Assert.AreEqual(2, Length(FConfig.Tools.Registry['list_orders'].RequiredScopes), 'Added to the attribute scopes');
  Assert.AreEqual(2, Length(FConfig.Resources.Registry['ui://orders'].RequiredScopes));
  Assert.AreEqual(2, Length(FConfig.Resources.TemplateRegistry['res://orders/{id}'].RequiredScopes));
  Assert.AreEqual(1, Length(FConfig.Prompts.Registry['greet'].RequiredScopes));
end;

procedure TMCPRequiredScopeConfigTest.TestRequireScope_UnknownItemRaises;
begin
  Assert.WillRaise(
    procedure
    begin
      FConfig.Tools.RequireScope('missing', 'x');
    end,
    EMCPException);

  Assert.WillRaise(
    procedure
    begin
      FConfig.Resources.RequireScope('res://missing', 'x');
    end,
    EMCPException);

  Assert.WillRaise(
    procedure
    begin
      FConfig.Prompts.RequireScope('missing', 'x');
    end,
    EMCPException);
end;

procedure TMCPRequiredScopeConfigTest.TestSetAuthorizerClass_ClassWithoutInterfaceRaises;
begin
  Assert.WillRaise(
    procedure
    begin
      FConfig.Security.SetAuthorizerClass(TObject);
    end,
    EMCPException);
end;

{ TMCPAccessGuardTest }

procedure TMCPAccessGuardTest.TestNoTokenInContext_FailsClosed;
var
  LContext: TJRPCContext;
  LGuard: TMCPAccessGuard;
  LItem: TMCPAuthItem;
begin
  LItem := Default(TMCPAuthItem);
  LItem.RequiredScopes := ['orders:read'];

  // A context built without an access token: the guard makes up an empty one
  LContext := TJRPCContext.Create;
  try
    LGuard := TMCPAccessGuard.Create(LContext, TMCPScopeAuthorizer.Create);
    try
      Assert.IsNotNull(LGuard.Identity, 'The authorizer is promised an identity');
      Assert.IsFalse(LGuard.IsAllowed(LItem));
    finally
      LGuard.Free;
    end;
  finally
    LContext.Free;
  end;

  Assert.IsFalse(TMCPScopeAuthorizer.Check(LItem, nil), 'No identity at all');
end;

procedure TMCPAccessGuardTest.TestCheck_ItemWithoutScopesIsAlwaysAllowed;
var
  LItem: TMCPAuthItem;
begin
  LItem := Default(TMCPAuthItem);

  Assert.IsTrue(TMCPScopeAuthorizer.Check(LItem, nil));
end;

{ TMCPAuthorizationApiTest }

procedure TMCPAuthorizationApiTest.Setup;
begin
  FScopes := NoScopes;
  ConfigureServer;
end;

procedure TMCPAuthorizationApiTest.TearDown;
begin
  FreeAndNil(FServer);
end;

procedure TMCPAuthorizationApiTest.ConfigureServer(ALegacy: Boolean);
begin
  FreeAndNil(FServer);
  FServer := TMCPServer.Create(nil);

  FServer.Plugin.Configure<IMCPConfig>
    .Security
      // Not what these tests are about: they would only add headers to every body
      .SetHeaderValidation(TMCPValidationLevel.Off)
      .SetMetaValidation(TMCPValidationLevel.Off)
    .BackToMCP
    .Tools
      .RegisterClass(TScopedService)
      .RegisterClass(TPublicService)
    .BackToMCP
    .Resources
      .RegisterClass(TScopedService)
    .BackToMCP
    .Prompts
      .RegisterClass(TScopedService)
      .RegisterClass(TPublicService)
    .BackToMCP
  .ApplyConfig;

  // The caller's identity comes from the token, as an API key validator gives it
  FServer.Plugin.Configure<IAuthTokenConfig>
    .SetTokenValidator(
      function (AContext: TJRPCContext; const AToken: string;
        AIdentity: TMCPAccessToken): Boolean
      begin
        AIdentity.Subject := 'alice';
        if AToken <> NoScopes then
          AIdentity.Scope := AToken.Replace(',', ' ');
        Result := True;
      end)
  .ApplyConfig;

  if ALegacy then
    FServer.Plugin.Configure<IMCPLegacyConfig>
      .SetEnabled(True)
      .SetLogWarning(False)
    .ApplyConfig;
end;

function TMCPAuthorizationApiTest.MCPConfig: IMCPConfig;
begin
  Result := FServer.Plugin.Configure<IMCPConfig>;
end;

function TMCPAuthorizationApiTest.Call(const AMethod, AParams: string): TJSONObject;
var
  LHandler: TMCPTransportHandler;
  LContent: string;
  LValue: TJSONValue;
begin
  LContent := '';

  LHandler := TMCPTransportHandler.Create(FServer, TSilentAuthorizationWriter.Create);
  try
    LHandler.ProcessRequest(
      procedure (ARequest: TMCPTransportRequest)
      begin
        ARequest.Url := '/';
        ARequest.Command := 'POST';
        ARequest.Protocol := TTransportProtocol.StreamableHTTP;
        ARequest.Accept := 'application/json';
        ARequest.SetHeader('Authorization', 'Bearer ' + FScopes);
        ARequest.Content := Format('{"jsonrpc":"2.0","id":1,"method":"%s","params":%s}',
          [AMethod, AParams]);
      end,
      procedure (AResponse: TMCPTransportResponse)
      begin
        LContent := AResponse.Content;
      end);
  finally
    LHandler.Free;
  end;

  LValue := TJSONObject.ParseJSONValue(LContent);
  if not (LValue is TJSONObject) then
  begin
    LValue.Free;
    Assert.Fail('The server must answer a JSON object: ' + LContent);
  end;
  Result := TJSONObject(LValue);
end;

function TMCPAuthorizationApiTest.ResultOf(const AMethod, AParams: string): TJSONObject;
var
  LAnswer: TJSONObject;
  LResult: TJSONValue;
begin
  LAnswer := Call(AMethod, AParams);
  try
    LResult := LAnswer.GetValue('result');
    Assert.IsTrue(LResult is TJSONObject, AMethod + ' must answer a result: ' + LAnswer.ToJSON);
    Result := LResult.Clone as TJSONObject;
  finally
    LAnswer.Free;
  end;
end;

function TMCPAuthorizationApiTest.InvalidParamsMessage(const AMethod, AParams: string): string;
var
  LAnswer: TJSONObject;
  LError: TJSONObject;
begin
  LAnswer := Call(AMethod, AParams);
  try
    Assert.IsTrue(LAnswer.TryGetValue<TJSONObject>('error', LError),
      AMethod + ' must answer an error: ' + LAnswer.ToJSON);
    Assert.AreEqual(-32602, LError.GetValue<Integer>('code'), LAnswer.ToJSON);
    Result := LError.GetValue<string>('message');
  finally
    LAnswer.Free;
  end;
end;

function TMCPAuthorizationApiTest.ListAll(const AMethod, AMember, AKey: string): string;
var
  LResult: TJSONObject;
  LItems: TJSONArray;
  LItem: TJSONValue;
  LCursor: string;
  LParams: string;
  LNames: TArray<string>;
begin
  LNames := [];
  LCursor := '';
  repeat
    if LCursor = '' then
      LParams := '{}'
    else
      LParams := Format('{"cursor":"%s"}', [LCursor]);

    LResult := ResultOf(AMethod, LParams);
    try
      LItems := LResult.GetValue(AMember) as TJSONArray;
      Assert.IsNotNull(LItems, LResult.ToJSON);

      LCursor := LResult.GetValue<string>('nextCursor', '');
      // A short page with a cursor after it would mean the list was cut before
      // being filtered
      if LCursor <> '' then
        Assert.IsTrue(LItems.Count > 0, 'A page followed by another must not be empty');

      for LItem in LItems do
        LNames := LNames + [(LItem as TJSONObject).GetValue<string>(AKey)];
    finally
      LResult.Free;
    end;
  until LCursor = '';

  Result := JoinSorted(LNames);
end;

function TMCPAuthorizationApiTest.ListTools: string;
begin
  Result := ListAll('tools/list', 'tools', 'name');
end;

function TMCPAuthorizationApiTest.ListResources: string;
begin
  Result := ListAll('resources/list', 'resources', 'uri');
end;

function TMCPAuthorizationApiTest.ListTemplates: string;
begin
  Result := ListAll('resources/templates/list', 'resourceTemplates', 'uriTemplate');
end;

function TMCPAuthorizationApiTest.ListPrompts: string;
begin
  Result := ListAll('prompts/list', 'prompts', 'name');
end;

function TMCPAuthorizationApiTest.CallToolError(const AName: string): string;
begin
  Result := InvalidParamsMessage('tools/call', Format('{"name":"%s","arguments":{}}', [AName]));
end;

function TMCPAuthorizationApiTest.ReadResourceError(const AUri: string): string;
begin
  Result := InvalidParamsMessage('resources/read', Format('{"uri":"%s"}', [AUri]));
end;

function TMCPAuthorizationApiTest.GetPromptError(const AName: string): string;
begin
  Result := InvalidParamsMessage('prompts/get', Format('{"name":"%s","arguments":{}}', [AName]));
end;

procedure TMCPAuthorizationApiTest.TestToolsList_NoScopes_HidesScopedTools;
begin
  Assert.AreEqual('ping;', ListTools);
end;

procedure TMCPAuthorizationApiTest.TestToolsList_PartialScopes_ShowsOnlyTheSatisfiedOnes;
begin
  FScopes := 'orders:read,orders:write';

  Assert.AreEqual('list_orders;ping;', ListTools, 'delete_order also needs orders:admin');
end;

procedure TMCPAuthorizationApiTest.TestToolsList_AllScopes_ShowsEverything;
begin
  FScopes := AllScopes;

  Assert.AreEqual('delete_order;list_orders;ping;', ListTools);
end;

procedure TMCPAuthorizationApiTest.TestCallTool_Denied_LooksLikeAMissingTool;
begin
  FScopes := 'orders:read';

  Assert.AreEqual(Format(SMCPToolNotFound, ['delete_order']), CallToolError('delete_order'),
    'A denied tool must look exactly like a missing one');
  Assert.AreEqual(Format(SMCPToolNotFound, ['missing']), CallToolError('missing'));
end;

procedure TMCPAuthorizationApiTest.TestCallTool_Allowed_Runs;
var
  LResult: TJSONObject;
begin
  FScopes := 'orders:read';

  LResult := ResultOf('tools/call', '{"name":"list_orders","arguments":{}}');
  try
    Assert.Contains(LResult.ToJSON, 'orders');
  finally
    LResult.Free;
  end;
end;

procedure TMCPAuthorizationApiTest.TestResourcesList_FiltersResourcesAndUI;
begin
  Assert.AreEqual('', ListResources);

  FScopes := 'orders:read';
  Assert.AreEqual('res://orders;ui://orders;', ListResources);
end;

procedure TMCPAuthorizationApiTest.TestTemplatesList_FiltersTemplates;
begin
  Assert.AreEqual('', ListTemplates);

  FScopes := 'orders:read';
  Assert.AreEqual('res://orders/{id};', ListTemplates);
end;

procedure TMCPAuthorizationApiTest.TestReadResource_Denied_LooksLikeAMissingResource;
begin
  Assert.AreEqual(Format(SMCPResourceNotFound, ['res://orders']), ReadResourceError('res://orders'));
  Assert.AreEqual(Format(SMCPResourceNotFound, ['ui://orders']), ReadResourceError('ui://orders'));
end;

procedure TMCPAuthorizationApiTest.TestReadTemplate_Denied_LooksLikeAMissingResource;
begin
  Assert.AreEqual(Format(SMCPResourceNotFound, ['res://orders/42']), ReadResourceError('res://orders/42'));
end;

procedure TMCPAuthorizationApiTest.TestPromptList_FiltersPrompts;
begin
  Assert.AreEqual('greet;', ListPrompts);

  FScopes := 'orders:read';
  Assert.AreEqual('greet;summarize_orders;', ListPrompts);
end;

procedure TMCPAuthorizationApiTest.TestGetPrompt_Denied_LooksLikeAMissingPrompt;
begin
  Assert.AreEqual(Format(SMCPPromptNotFound, ['summarize_orders']), GetPromptError('summarize_orders'));
end;

procedure TMCPAuthorizationApiTest.TestComplete_DeniedPrompt_LooksLikeAMissingPrompt;
begin
  Assert.AreEqual(Format(SMCPPromptNotFound, ['summarize_orders']),
    InvalidParamsMessage('completion/complete',
      '{"ref":{"type":"ref/prompt","name":"summarize_orders"},"argument":{"name":"x","value":""}}'));
end;

procedure TMCPAuthorizationApiTest.TestComplete_DeniedTemplate_LooksLikeAMissingResource;
begin
  Assert.AreEqual(Format(SMCPResourceNotFound, ['res://orders/{id}']),
    InvalidParamsMessage('completion/complete',
      '{"ref":{"type":"ref/resource","uri":"res://orders/{id}"},"argument":{"name":"id","value":""}}'));

  Assert.AreEqual(Format(SMCPResourceNotFound, ['res://orders']),
    InvalidParamsMessage('completion/complete',
      '{"ref":{"type":"ref/resource","uri":"res://orders"},"argument":{"name":"id","value":""}}'),
    'A plain resource uri is checked as well');
end;

procedure TMCPAuthorizationApiTest.TestComplete_Allowed_Answers;
begin
  FScopes := 'orders:read';

  // No provider is registered: an allowed target gets an empty completion
  ResultOf('completion/complete',
    '{"ref":{"type":"ref/prompt","name":"summarize_orders"},"argument":{"name":"x","value":""}}').Free;
  ResultOf('completion/complete',
    '{"ref":{"type":"ref/resource","uri":"res://orders/{id}"},"argument":{"name":"id","value":""}}').Free;
end;

procedure TMCPAuthorizationApiTest.TestPaging_FilteredBeforeThePageIsCut;
begin
  // One item per page: filtering a page after cutting it would leave the page
  // of a hidden item empty, with a cursor pointing past it
  MCPConfig.Server.SetPageSize(1);
  FScopes := 'orders:read';

  Assert.AreEqual('list_orders;ping;', ListTools);
  Assert.AreEqual('res://orders;ui://orders;', ListResources);
  Assert.AreEqual('res://orders/{id};', ListTemplates);
  Assert.AreEqual('greet;summarize_orders;', ListPrompts);

  FScopes := NoScopes;
  Assert.AreEqual('ping;', ListTools);
  Assert.AreEqual('', ListResources);
end;

procedure TMCPAuthorizationApiTest.TestLegacyClient_IsFilteredToo;
begin
  ConfigureServer(True);

  Assert.AreEqual('ping;', ListTools);
  Assert.AreEqual(Format(SMCPToolNotFound, ['list_orders']), CallToolError('list_orders'));

  FScopes := 'orders:read';
  Assert.AreEqual('list_orders;ping;', ListTools);
end;

procedure TMCPAuthorizationApiTest.TestSetAuthorizer_CustomRuleDecidesAlone;
begin
  MCPConfig.Security.SetAuthorizer(
    function (AContext: TJRPCContext; const AItem: TMCPAuthItem;
      AIdentity: TMCPAccessToken): Boolean
    begin
      Result := AItem.Name <> 'ping';
    end);

  // No scopes at all, yet the scoped tools are shown: the custom rule replaces the default
  Assert.AreEqual('delete_order;list_orders;', ListTools);
  Assert.AreEqual(Format(SMCPToolNotFound, ['ping']), CallToolError('ping'));
end;

procedure TMCPAuthorizationApiTest.TestSetAuthorizer_ReceivesItemDetailsAndIdentity;
var
  LKinds: TArray<string>;
  LIdentityOk: Boolean;
  LTemplateUri: string;
begin
  LKinds := [];
  LIdentityOk := True;
  LTemplateUri := '';

  MCPConfig.Security.SetAuthorizer(
    function (AContext: TJRPCContext; const AItem: TMCPAuthItem;
      AIdentity: TMCPAccessToken): Boolean
    begin
      LIdentityOk := LIdentityOk and Assigned(AContext) and Assigned(AIdentity) and
        (AIdentity.Subject = 'alice');
      case AItem.Kind of
        TMCPAuthItemKind.Tool: LKinds := LKinds + ['T'];
        TMCPAuthItemKind.Resource: LKinds := LKinds + ['R'];
        TMCPAuthItemKind.Template:
        begin
          LKinds := LKinds + ['M'];
          LTemplateUri := AItem.Uri;
        end;
        TMCPAuthItemKind.UI: LKinds := LKinds + ['U'];
        TMCPAuthItemKind.Prompt: LKinds := LKinds + ['P'];
      else
        Assert.Fail('Unexpected item kind');
      end;
      Result := True;
    end);

  ListTools;
  ListResources;
  ListTemplates;
  Assert.AreEqual('res://orders/{id}', LTemplateUri, 'When listing a template its uri template is passed');
  ResultOf('resources/read', '{"uri":"res://orders/42"}').Free;
  Assert.AreEqual('res://orders/42', LTemplateUri, 'When reading a template the requested uri is passed');
  ListPrompts;

  Assert.AreEqual('M;M;P;P;R;T;T;T;U;', JoinSorted(LKinds));
  Assert.IsTrue(LIdentityOk, 'The authorizer must get the request context and the caller identity');
end;

procedure TMCPAuthorizationApiTest.TestSetAuthorizer_CanCombineWithTheDefaultCheck;
begin
  FScopes := AllScopes;
  MCPConfig.Security.SetAuthorizer(
    function (AContext: TJRPCContext; const AItem: TMCPAuthItem;
      AIdentity: TMCPAccessToken): Boolean
    begin
      Result := TMCPScopeAuthorizer.Check(AItem, AIdentity) and not AItem.Name.StartsWith('delete');
    end);

  Assert.AreEqual('list_orders;ping;', ListTools);
end;

procedure TMCPAuthorizationApiTest.TestSetAuthorizer_ExceptionCountsAsDenial;
begin
  MCPConfig.Security.SetAuthorizer(
    function (AContext: TJRPCContext; const AItem: TMCPAuthItem;
      AIdentity: TMCPAccessToken): Boolean
    begin
      raise Exception.Create('Authorizer failure');
    end);

  Assert.AreEqual('', ListTools);
  Assert.AreEqual(Format(SMCPToolNotFound, ['ping']), CallToolError('ping'));
end;

procedure TMCPAuthorizationApiTest.TestSetAuthorizer_NilRestoresTheDefault;
begin
  MCPConfig.Security
    .SetAuthorizerClass(TDenyAllAuthorizer)
    .SetAuthorizer(nil);

  Assert.AreEqual('ping;', ListTools);
end;

procedure TMCPAuthorizationApiTest.TestSetAuthorizerClass_IsUsed;
begin
  FScopes := AllScopes;
  MCPConfig.Security.SetAuthorizerClass(TDenyAllAuthorizer);

  Assert.AreEqual('', ListTools);
  Assert.AreEqual('', ListPrompts);
end;

{ TMCPAuthorizationListenTest }

procedure TMCPAuthorizationListenTest.Setup;
begin
  FServer := TMCPServer.Create(nil);
  FServer.Plugin.Configure<IMCPConfig>.Resources.RegisterClass(TScopedService);

  FGarbage := TGarbageCollector.Create;
  FQueue := TMCPMessageQueue.Create;
  FRequest := TJRPCRequest.Create;
  FRequest.Id := TJRPCID(7);
  FToken := TMCPAccessToken.Create;

  FContext := TJRPCContext.Create;
  FContext.AddContent(TObject(FGarbage));
  FContext.AddContent(FServer.GetConfiguration<TMCPConfig>);
  FContext.AddContent(FQueue);
  FContext.AddContent(FRequest);
  FContext.AddContent(FToken);

  FApi := TMCPSubscriptionsApi.Create;
  FContext.Inject(FApi);
end;

procedure TMCPAuthorizationListenTest.TearDown;
begin
  FApi.Free;
  FContext.Free;
  FToken.Free;
  FRequest.Free;
  FQueue.Free;
  FGarbage := nil;
  FServer.Free;
end;

function TMCPAuthorizationListenTest.AcknowledgedUris(const AUris: TArray<string>): string;
var
  LParams: TSubscriptionsListenRequestParams;
  LMessage: TJRPCMessage;
  LNotifications: TJSONObject;
  LUris: TJSONArray;
  LUri: TJSONValue;
  LNames: TArray<string>;
begin
  LParams := TSubscriptionsListenRequestParams.Create;
  try
    LParams.Notifications.ResourceSubscriptions := AUris;
    FApi.Listen(LParams).Free;
  finally
    LParams.Free;
  end;

  LNames := [];
  LMessage := FQueue.Dequeue;
  Assert.IsNotNull(LMessage, 'Listen should have enqueued an acknowledgement');
  try
    LNotifications := ((LMessage as TJRPCNotification).Params as TJSONObject)
      .GetValue('notifications') as TJSONObject;
    if LNotifications.TryGetValue<TJSONArray>('resourceSubscriptions', LUris) then
      for LUri in LUris do
        LNames := LNames + [LUri.Value];
  finally
    LMessage.Free;
  end;

  Result := JoinSorted(LNames);
end;

procedure TMCPAuthorizationListenTest.TestNoScopes_DropsTheProtectedUris;
begin
  Assert.AreEqual('', AcknowledgedUris(['res://orders', 'res://orders/{id}', 'ui://orders']),
    'A protected uri must be dropped like an unknown one');
end;

procedure TMCPAuthorizationListenTest.TestWithScopes_KeepsThem;
begin
  FToken.Scope := 'orders:read';

  Assert.AreEqual('res://orders;res://orders/{id};ui://orders;',
    AcknowledgedUris(['res://orders', 'res://orders/{id}', 'ui://orders', 'res://nope']));
end;

initialization
  TDUnitX.RegisterTestFixture(TMCPScopeListTest);
  TDUnitX.RegisterTestFixture(TMCPRequiredScopeConfigTest);
  TDUnitX.RegisterTestFixture(TMCPAccessGuardTest);
  TDUnitX.RegisterTestFixture(TMCPAuthorizationApiTest);
  TDUnitX.RegisterTestFixture(TMCPAuthorizationListenTest);

end.
