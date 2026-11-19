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
unit MCPConnect.Tests.WebMCP;

interface

uses
  System.SysUtils,
  System.Classes,
  System.JSON,
  Web.HTTPApp,
  DUnitX.TestFramework,

  MCPConnect.MCP.Server,
  MCPConnect.Configuration.MCP,
  MCPConnect.MCP.Attributes,
  MCPConnect.MCP.Types.Tool,
  MCPConnect.WebMCP;

type
  /// <summary>
  ///   The route helper of TMCPWebMCP: the script, and where it is served.
  /// </summary>
  [TestFixture]
  TWebMCPScriptRouteTest = class
  private
    FServer: TMCPServer;
    FWebMCP: TMCPWebMCP;
    FModule: TCustomWebDispatcher;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    [Test]
    procedure ClientScriptIsStableAndNotEmpty;

    [Test]
    procedure RegisterRouteAddsGetActionAtScriptUrl;

    [Test]
    procedure RegisterRouteFollowsACustomScriptUrl;
  end;

  /// <summary>
  ///   Tools for the page-declared selection tests: one allowed everywhere,
  ///   one allowed for a group, one never opted in, one disabled.
  /// </summary>
  TWebMCPPageTools = class
  public
    [McpTool('shared_tool', 'Allowed on every page', 'webmcp')]
    function SharedTool: string;

    [McpTool('grouped_tool', 'Allowed for the checkout group', 'webmcp=checkout')]
    function GroupedTool: string;

    [McpTool('hidden_tool', 'Never opted in to WebMCP')]
    function HiddenTool: string;

    [McpTool('off_tool', 'Opted in, but disabled', 'webmcp,disabled')]
    function OffTool: string;
  end;

  /// <summary>
  ///   A page that names the tools it offers: View and ToolsJSON with a list
  ///   of names.
  /// </summary>
  [TestFixture]
  TWebMCPPageToolsTest = class
  private
    FServer: TMCPServer;
    FWebMCP: TMCPWebMCP;
    function Names(const AJSON: string): string;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    [Test]
    procedure OffersOnlyTheNamedTools;

    [Test]
    procedure KeepsAGroupedToolToItsGroup;

    [Test]
    procedure SkipsToolsNotOptedIn;

    [Test]
    procedure SkipsDisabledAndUnknownTools;

    [Test]
    procedure AppliesTheServerAndThePageFilters;

    [Test]
    procedure ViewCarriesTheNamedTools;

    [Test]
    procedure GroupViewIsUnchanged;
  end;

implementation

{ TWebMCPScriptRouteTest }

procedure TWebMCPScriptRouteTest.Setup;
begin
  FServer := TMCPServer.Create(nil);
  FModule := TCustomWebDispatcher.Create(nil);
end;

procedure TWebMCPScriptRouteTest.TearDown;
begin
  FModule.Free;
  FWebMCP.Free;
  FServer.Free;
end;

procedure TWebMCPScriptRouteTest.ClientScriptIsStableAndNotEmpty;
begin
  Assert.IsNotEmpty(TMCPWebMCP.ClientScript);
  Assert.AreEqual(TMCPWebMCP.ClientScript, TMCPWebMCP.ClientScript);
  Assert.IsFalse(TMCPWebMCP.ClientScript.Contains(#13), 'Line breaks are normalized to LF');
end;

procedure TWebMCPScriptRouteTest.RegisterRouteAddsGetActionAtScriptUrl;
begin
  FWebMCP := TMCPWebMCP.Create(FServer);
  FWebMCP.RegisterRoute(FModule);

  Assert.AreEqual(1, FModule.Actions.Count);
  Assert.AreEqual('/webmcp.js', FModule.Actions[0].PathInfo);
  Assert.IsTrue(FModule.Actions[0].MethodType = mtGet, 'The script is fetched with GET');
  Assert.IsTrue(Assigned(FModule.Actions[0].OnAction));
end;

procedure TWebMCPScriptRouteTest.RegisterRouteFollowsACustomScriptUrl;
begin
  FWebMCP := TMCPWebMCP.Create(FServer, '/mcp', '/static/bridge.js');
  FWebMCP.RegisterRoute(FModule);

  Assert.AreEqual('/static/bridge.js', FModule.Actions[0].PathInfo);
end;

{ TWebMCPPageTools }

function TWebMCPPageTools.SharedTool: string;
begin
  Result := 'shared';
end;

function TWebMCPPageTools.GroupedTool: string;
begin
  Result := 'grouped';
end;

function TWebMCPPageTools.HiddenTool: string;
begin
  Result := 'hidden';
end;

function TWebMCPPageTools.OffTool: string;
begin
  Result := 'off';
end;

{ TWebMCPPageToolsTest }

procedure TWebMCPPageToolsTest.Setup;
begin
  FServer := TMCPServer.Create(nil);
  FServer.Plugin.Configure<IMCPConfig>
    .Tools
      .RegisterClass(TWebMCPPageTools);
  FWebMCP := TMCPWebMCP.Create(FServer);
end;

procedure TWebMCPPageToolsTest.TearDown;
begin
  FWebMCP.Free;
  FServer.Free;
end;

function TWebMCPPageToolsTest.Names(const AJSON: string): string;
var
  LArray: TJSONArray;
  LItem: TJSONValue;
begin
  // The tool names in the order the list carries them, comma-separated
  Result := '';
  LArray := TJSONObject.ParseJSONValue(AJSON) as TJSONArray;
  try
    for LItem in LArray do
    begin
      if not Result.IsEmpty then
        Result := Result + ',';
      Result := Result + LItem.GetValue<string>('name');
    end;
  finally
    LArray.Free;
  end;
end;

procedure TWebMCPPageToolsTest.OffersOnlyTheNamedTools;
begin
  Assert.AreEqual('shared_tool', Names(FWebMCP.ToolsJSON(['shared_tool'])));
  Assert.AreEqual('', Names(FWebMCP.ToolsJSON([])), 'A page that names nothing offers nothing');
end;

procedure TWebMCPPageToolsTest.KeepsAGroupedToolToItsGroup;
begin
  Assert.AreEqual('shared_tool',
    Names(FWebMCP.ToolsJSON(['shared_tool', 'grouped_tool'])),
    'A page outside the group cannot offer the tool, even naming it');
  Assert.AreEqual('shared_tool',
    Names(FWebMCP.ToolsJSON(['shared_tool', 'grouped_tool'], 'tickets')),
    'Nor can a page of another group');
  // Sorted by name, as tools/list is
  Assert.AreEqual('grouped_tool,shared_tool',
    Names(FWebMCP.ToolsJSON(['shared_tool', 'grouped_tool'], 'checkout')));
  Assert.AreEqual('shared_tool',
    Names(FWebMCP.ToolsJSON(['shared_tool'], 'checkout')),
    'The group allows the tool, the page still has to name it');
end;

procedure TWebMCPPageToolsTest.SkipsToolsNotOptedIn;
begin
  Assert.AreEqual('', Names(FWebMCP.ToolsJSON(['hidden_tool'])),
    'A page chooses among the opted-in tools, it cannot add one');

  FWebMCP.Expose(['hidden_tool']);
  Assert.AreEqual('hidden_tool', Names(FWebMCP.ToolsJSON(['hidden_tool'])),
    'Expose opts a tool in, as the tag does');
end;

procedure TWebMCPPageToolsTest.SkipsDisabledAndUnknownTools;
begin
  Assert.AreEqual('shared_tool',
    Names(FWebMCP.ToolsJSON(['off_tool', 'no_such_tool', ' shared_tool '])));
end;

procedure TWebMCPPageToolsTest.AppliesTheServerAndThePageFilters;
begin
  Assert.AreEqual('grouped_tool',
    Names(FWebMCP.ToolsJSON(['shared_tool', 'grouped_tool'], 'checkout',
      function (ATool: TMCPTool): Boolean
      begin
        Result := ATool.Name <> 'shared_tool';
      end)), 'The page filter');

  FWebMCP.SetFilter(
    function (ATool: TMCPTool): Boolean
    begin
      Result := ATool.Name <> 'grouped_tool';
    end);
  Assert.AreEqual('shared_tool',
    Names(FWebMCP.ToolsJSON(['shared_tool', 'grouped_tool'], 'checkout')), 'The server filter');
end;

procedure TWebMCPPageToolsTest.ViewCarriesTheNamedTools;
var
  LView: TMCPWebMCPView;
begin
  LView := FWebMCP.View(['shared_tool', 'grouped_tool'], 'checkout');
  try
    Assert.AreEqual(2, LView.ToolCount);
    Assert.IsTrue(LView.HasTools);
    Assert.AreEqual('grouped_tool,shared_tool', Names(LView.Tools));
  finally
    LView.Free;
  end;
end;

procedure TWebMCPPageToolsTest.GroupViewIsUnchanged;
begin
  Assert.AreEqual('shared_tool', Names(FWebMCP.ToolsJSON('')));
  Assert.AreEqual('grouped_tool,shared_tool', Names(FWebMCP.ToolsJSON('checkout')));
end;

initialization
  TDUnitX.RegisterTestFixture(TWebMCPScriptRouteTest);
  TDUnitX.RegisterTestFixture(TWebMCPPageToolsTest);

end.
