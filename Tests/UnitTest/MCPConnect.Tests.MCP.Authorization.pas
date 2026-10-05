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
unit MCPConnect.Tests.MCP.Authorization;

interface

uses
  System.SysUtils,
  DUnitX.TestFramework,

  MCPConnect.JRPC.Classes,
  MCPConnect.JRPC.Core,
  MCPConnect.JRPC.Server,
  MCPConnect.Configuration.MCP,
  MCPConnect.MCP.Types,
  MCPConnect.MCP.Tools,
  MCPConnect.MCP.Resources,
  MCPConnect.MCP.Prompts,
  MCPConnect.MCP.Attributes,
  MCPConnect.MCP.Authorization,
  MCPConnect.MCP.Server.Api;

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
    procedure TestHasScopes_EmptyListIsAlwaysSatisfied;
    [Test]
    procedure TestHasScopes_RequiresAllAndReportsTheMissingOnes;
  end;

  [TestFixture]
  TMCPRequiredScopeConfigTest = class(TObject)
  private
    FServer: TJRPCServer;
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
    procedure TestRegisterTool_RequireScopeOnTheBuilder;
    [Test]
    procedure TestRequireScope_AddsToAnyRegisteredItem;
    [Test]
    procedure TestRequireScope_UnknownItemRaises;
    [Test]
    procedure TestSetAuthorizerClass_ClassWithoutInterfaceRaises;
  end;

  [TestFixture]
  TMCPAuthorizationApiTest = class(TObject)
  private
    FServer: TJRPCServer;
    FConfig: IMCPConfig;
    FGarbage: IGarbageCollector;
    FToken: TMCPAccessToken;
    FContext: TJRPCContext;

    function MCPConfig: TMCPConfig;
    function ListTools: string;
    function ListResources: string;
    function ListTemplates: string;
    function ListPrompts: string;
    procedure CallTool(const AName: string);
    procedure ReadResource(const AUri: string);
    procedure GetPrompt(const AName: string);
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
    procedure TestToolsList_NoTokenInContext_FailsClosed;
    [Test]
    procedure TestCallTool_Denied_RaisesToolNotFound;
    [Test]
    procedure TestCallTool_Allowed_Runs;

    [Test]
    procedure TestResourcesList_FiltersResourcesAndUI;
    [Test]
    procedure TestTemplatesList_FiltersTemplates;
    [Test]
    procedure TestReadResource_Denied_RaisesResourceNotFound;
    [Test]
    procedure TestReadTemplate_Denied_RaisesResourceNotFound;

    [Test]
    procedure TestPromptList_FiltersPrompts;
    [Test]
    procedure TestGetPrompt_Denied_RaisesPromptNotFound;

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

implementation

uses
  System.Generics.Collections;

const
  ALL_SCOPES = 'orders:read orders:write orders:admin';

// The registries are ordered dictionaries only on some Delphi versions
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
  FServer := TJRPCServer.Create(nil);
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

{ TMCPAuthorizationApiTest }

procedure TMCPAuthorizationApiTest.Setup;
begin
  FServer := TJRPCServer.Create(nil);
  FConfig := FServer.Plugin.Configure<IMCPConfig>;
  FConfig.Tools.RegisterClass(TScopedService).RegisterClass(TPublicService);
  FConfig.Resources.RegisterClass(TScopedService);
  FConfig.Prompts.RegisterClass(TScopedService).RegisterClass(TPublicService);

  FGarbage := TGarbageCollector.CreateInstance;
  FToken := TMCPAccessToken.Create;

  FContext := TJRPCContext.Create;
  FContext.AddContent(FServer);
  FContext.AddContent(FGarbage);
  FContext.AddContent(FToken);
end;

procedure TMCPAuthorizationApiTest.TearDown;
begin
  FContext.Free;
  FToken.Free;
  FGarbage := nil;
  FConfig := nil;
  FServer.Free;
end;

function TMCPAuthorizationApiTest.MCPConfig: TMCPConfig;
begin
  Result := FServer.GetConfiguration<TMCPConfig>;
end;

function TMCPAuthorizationApiTest.ListTools: string;
var
  LApi: TMCPToolsApi;
  LList: TListToolsResult;
  LTool: TMCPTool;
  LNames: TArray<string>;
begin
  LNames := [];
  LApi := TMCPToolsApi.Create;
  try
    LApi.RPCContext := FContext;
    LApi.MCPConfig := MCPConfig;
    LList := LApi.ToolsList;
    try
      for LTool in LList.Tools do
        LNames := LNames + [LTool.Name];
    finally
      LList.Free;
    end;
  finally
    LApi.Free;
  end;

  Result := JoinSorted(LNames);
end;

function TMCPAuthorizationApiTest.ListResources: string;
var
  LApi: TMCPResourcesApi;
  LList: TListResourcesResult;
  LRes: TMCPResource;
  LNames: TArray<string>;
begin
  LNames := [];
  LApi := TMCPResourcesApi.Create;
  try
    LApi.RPCContext := FContext;
    LApi.MCPConfig := MCPConfig;
    LList := LApi.ResourcesList;
    try
      for LRes in LList.Resources do
        LNames := LNames + [LRes.Uri];
    finally
      LList.Free;
    end;
  finally
    LApi.Free;
  end;

  Result := JoinSorted(LNames);
end;

function TMCPAuthorizationApiTest.ListTemplates: string;
var
  LApi: TMCPResourcesApi;
  LList: TListResourceTemplatesResult;
  LTpl: TMCPResourceTemplate;
  LNames: TArray<string>;
begin
  LNames := [];
  LApi := TMCPResourcesApi.Create;
  try
    LApi.RPCContext := FContext;
    LApi.MCPConfig := MCPConfig;
    LList := LApi.TemplatesList;
    try
      for LTpl in LList.ResourceTemplates do
        LNames := LNames + [LTpl.UriTemplate.GetValueOrDefault];
    finally
      LList.Free;
    end;
  finally
    LApi.Free;
  end;

  Result := JoinSorted(LNames);
end;

function TMCPAuthorizationApiTest.ListPrompts: string;
var
  LApi: TMCPPromptsApi;
  LList: TListPromptsResult;
  LPrompt: TMCPPrompt;
  LNames: TArray<string>;
begin
  LNames := [];
  LApi := TMCPPromptsApi.Create;
  try
    LApi.RPCContext := FContext;
    LApi.MCPConfig := MCPConfig;
    LList := LApi.PromptList;
    try
      for LPrompt in LList.Prompts do
        LNames := LNames + [LPrompt.Name];
    finally
      LList.Free;
    end;
  finally
    LApi.Free;
  end;

  Result := JoinSorted(LNames);
end;

procedure TMCPAuthorizationApiTest.CallTool(const AName: string);
var
  LApi: TMCPToolsApi;
  LParams: TCallToolParams;
begin
  LApi := TMCPToolsApi.Create;
  LParams := TCallToolParams.Create;
  try
    LApi.RPCContext := FContext;
    LApi.MCPConfig := MCPConfig;
    LParams.Name := AName;
    LApi.CallTool(LParams).Free;
  finally
    LParams.Free;
    LApi.Free;
  end;
end;

procedure TMCPAuthorizationApiTest.ReadResource(const AUri: string);
var
  LApi: TMCPResourcesApi;
  LParams: TReadResourceParams;
begin
  LApi := TMCPResourcesApi.Create;
  LParams := TReadResourceParams.Create;
  try
    LApi.RPCContext := FContext;
    LApi.MCPConfig := MCPConfig;
    LParams.Uri := AUri;
    LApi.ReadResource(LParams).Free;
  finally
    LParams.Free;
    LApi.Free;
  end;
end;

procedure TMCPAuthorizationApiTest.GetPrompt(const AName: string);
var
  LApi: TMCPPromptsApi;
  LParams: TGetPromptParams;
begin
  LApi := TMCPPromptsApi.Create;
  LParams := TGetPromptParams.Create;
  try
    LApi.RPCContext := FContext;
    LApi.MCPConfig := MCPConfig;
    LParams.Name := AName;
    LApi.ReadPrompt(LParams).Free;
  finally
    LParams.Free;
    LApi.Free;
  end;
end;

procedure TMCPAuthorizationApiTest.TestToolsList_NoScopes_HidesScopedTools;
begin
  Assert.AreEqual('ping;', ListTools);
end;

procedure TMCPAuthorizationApiTest.TestToolsList_PartialScopes_ShowsOnlyTheSatisfiedOnes;
begin
  FToken.Scope := 'orders:read orders:write';

  Assert.AreEqual('list_orders;ping;', ListTools, 'delete_order also needs orders:admin');
end;

procedure TMCPAuthorizationApiTest.TestToolsList_AllScopes_ShowsEverything;
begin
  FToken.Scope := ALL_SCOPES;

  Assert.AreEqual('delete_order;list_orders;ping;', ListTools);
end;

procedure TMCPAuthorizationApiTest.TestToolsList_NoTokenInContext_FailsClosed;
var
  LContext: TJRPCContext;
begin
  // A context built without an access token gets an empty identity
  LContext := TJRPCContext.Create;
  try
    LContext.AddContent(FServer);
    LContext.AddContent(FGarbage);
    FContext.Free;
    FContext := LContext;
  except
    LContext.Free;
    raise;
  end;

  Assert.AreEqual('ping;', ListTools);
end;

procedure TMCPAuthorizationApiTest.TestCallTool_Denied_RaisesToolNotFound;
begin
  FToken.Scope := 'orders:read';

  Assert.WillRaiseWithMessage(
    procedure
    begin
      CallTool('delete_order');
    end,
    EMCPException, Format(SMCPToolNotFound, ['delete_order']),
    'A denied tool must look exactly like a missing one');
end;

procedure TMCPAuthorizationApiTest.TestCallTool_Allowed_Runs;
begin
  FToken.Scope := 'orders:read';

  Assert.WillNotRaiseAny(
    procedure
    begin
      CallTool('list_orders');
      CallTool('ping');
    end);
end;

procedure TMCPAuthorizationApiTest.TestResourcesList_FiltersResourcesAndUI;
begin
  Assert.AreEqual('', ListResources);

  FToken.Scope := 'orders:read';
  Assert.AreEqual('res://orders;ui://orders;', ListResources);
end;

procedure TMCPAuthorizationApiTest.TestTemplatesList_FiltersTemplates;
begin
  Assert.AreEqual('', ListTemplates);

  FToken.Scope := 'orders:read';
  Assert.AreEqual('res://orders/{id};', ListTemplates);
end;

procedure TMCPAuthorizationApiTest.TestReadResource_Denied_RaisesResourceNotFound;
begin
  Assert.WillRaiseWithMessage(
    procedure
    begin
      ReadResource('res://orders');
    end,
    EMCPException, Format(SMCPResourceNotFound, ['res://orders']));

  Assert.WillRaiseWithMessage(
    procedure
    begin
      ReadResource('ui://orders');
    end,
    EMCPException, Format(SMCPResourceNotFound, ['ui://orders']));
end;

procedure TMCPAuthorizationApiTest.TestReadTemplate_Denied_RaisesResourceNotFound;
begin
  Assert.WillRaiseWithMessage(
    procedure
    begin
      ReadResource('res://orders/42');
    end,
    EMCPException, Format(SMCPResourceNotFound, ['res://orders/42']));
end;

procedure TMCPAuthorizationApiTest.TestPromptList_FiltersPrompts;
begin
  Assert.AreEqual('greet;', ListPrompts);

  FToken.Scope := 'orders:read';
  Assert.AreEqual('greet;summarize_orders;', ListPrompts);
end;

procedure TMCPAuthorizationApiTest.TestGetPrompt_Denied_RaisesPromptNotFound;
begin
  Assert.WillRaiseWithMessage(
    procedure
    begin
      GetPrompt('summarize_orders');
    end,
    EMCPException, Format(SMCPPromptNotFound, ['summarize_orders']));
end;

procedure TMCPAuthorizationApiTest.TestSetAuthorizer_CustomRuleDecidesAlone;
begin
  FConfig.Security.SetAuthorizer(
    function (AContext: TJRPCContext; const AItem: TMCPAuthItem;
      AIdentity: TMCPAccessToken): Boolean
    begin
      Result := AItem.Name <> 'ping';
    end);

  // No scopes at all, yet the scoped tools are shown: the custom rule replaces the default
  Assert.AreEqual('delete_order;list_orders;', ListTools);

  Assert.WillRaiseWithMessage(
    procedure
    begin
      CallTool('ping');
    end,
    EMCPException, Format(SMCPToolNotFound, ['ping']));
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
  FToken.Subject := 'alice';

  FConfig.Security.SetAuthorizer(
    function (AContext: TJRPCContext; const AItem: TMCPAuthItem;
      AIdentity: TMCPAccessToken): Boolean
    begin
      LIdentityOk := LIdentityOk and Assigned(AIdentity) and (AIdentity.Subject = 'alice')
        and (AContext = FContext);
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
  ReadResource('res://orders/42');
  Assert.AreEqual('res://orders/42', LTemplateUri, 'When reading a template the requested uri is passed');
  ListPrompts;

  Assert.AreEqual('M;M;P;P;R;T;T;T;U;', JoinSorted(LKinds));
  Assert.IsTrue(LIdentityOk, 'The authorizer must get the request context and the caller identity');
end;

procedure TMCPAuthorizationApiTest.TestSetAuthorizer_CanCombineWithTheDefaultCheck;
begin
  FToken.Scope := ALL_SCOPES;
  FConfig.Security.SetAuthorizer(
    function (AContext: TJRPCContext; const AItem: TMCPAuthItem;
      AIdentity: TMCPAccessToken): Boolean
    begin
      Result := TMCPScopeAuthorizer.Check(AItem, AIdentity) and not AItem.Name.StartsWith('delete');
    end);

  Assert.AreEqual('list_orders;ping;', ListTools);
end;

procedure TMCPAuthorizationApiTest.TestSetAuthorizer_ExceptionCountsAsDenial;
begin
  FConfig.Security.SetAuthorizer(
    function (AContext: TJRPCContext; const AItem: TMCPAuthItem;
      AIdentity: TMCPAccessToken): Boolean
    begin
      raise Exception.Create('Authorizer failure');
    end);

  Assert.AreEqual('', ListTools);

  Assert.WillRaiseWithMessage(
    procedure
    begin
      CallTool('ping');
    end,
    EMCPException, Format(SMCPToolNotFound, ['ping']));
end;

procedure TMCPAuthorizationApiTest.TestSetAuthorizer_NilRestoresTheDefault;
begin
  FConfig.Security
    .SetAuthorizerClass(TDenyAllAuthorizer)
    .SetAuthorizer(nil);

  Assert.AreEqual('ping;', ListTools);
end;

procedure TMCPAuthorizationApiTest.TestSetAuthorizerClass_IsUsed;
begin
  FToken.Scope := ALL_SCOPES;
  FConfig.Security.SetAuthorizerClass(TDenyAllAuthorizer);

  Assert.AreEqual('', ListTools);
  Assert.AreEqual('', ListPrompts);
end;

initialization
  TDUnitX.RegisterTestFixture(TMCPScopeListTest);
  TDUnitX.RegisterTestFixture(TMCPRequiredScopeConfigTest);
  TDUnitX.RegisterTestFixture(TMCPAuthorizationApiTest);

end.
