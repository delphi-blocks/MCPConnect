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
  Web.HTTPApp,
  DUnitX.TestFramework,

  MCPConnect.MCP.Server,
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

initialization
  TDUnitX.RegisterTestFixture(TWebMCPScriptRouteTest);

end.
