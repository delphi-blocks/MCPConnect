unit MCPServer.Config;

interface

uses
  System.SysUtils, System.Classes,
  IdGlobal, IdContext, IdBaseComponent, IdComponent,
  IdCustomTCPServer, IdCustomHTTPServer, IdHTTPServer,

  MCPConnect.JRPC.Core,
  MCPConnect.JRPC.Classes,
  MCPConnect.JRPC.Server,

  MCPConnect.MCP.Server.Api,
  MCPConnect.MCP.Types,

  MCPConnect.Configuration.MCP,
  MCPConnect.Configuration.Session,
  MCPConnect.Configuration.Auth,

  MCPConnect.Content.Writers.RTL,
  {$IFDEF FRAMEWORK_VCL}
  MCPConnect.Content.Writers.VCL,
  {$ENDIF}

  MCPConnect.Session.Core;

type
  TServerConfigurator = class
    class procedure ConfigureServer(AServer: TJRPCServer);
    class procedure UnregisterFeatures(AServer: TJRPCServer);
  end;

implementation

uses
  System.IOUtils,
  System.TypInfo,
  Logify,

  MCPServer.Resources,
  MCPServer.Apps,
  MCPServer.Tools,
  MCPServer.Prompts;

{ TServerConfigurator }

class procedure TServerConfigurator.ConfigureServer(AServer: TJRPCServer);
begin
  AServer

    .Plugin.Configure<IMCPConfig>

      .Server
        .SetName('todo-mcp-server')
        .SetVersion('1.0.0')
        .RegisterWriter(TMCPStreamWriter)
        .RegisterWriter(TMCPStringListWriter)
      .BackToMCP

      .Security
        .SetCORS(True)
        .SetAllowedMethods(['GET', 'POST'])
        .SetAllowedOrigins(['*'])
        //.SetRequireOrigin(True)
      .BackToMCP

      .Resources
        .RegisterClass(TTodoResource)
        .RegisterClass(TTodoAppUI)
      .BackToMCP

      .Prompts
        .RegisterClass(TTodoPrompts)
      .BackToMCP

      .Tools
        .RegisterClass(TTodoTool)
      .BackToMCP
  ;
end;

class procedure TServerConfigurator.UnregisterFeatures(AServer: TJRPCServer);
begin
  AServer.Plugin.Configure<IMCPConfig>

  .Tools
    .UnregisterClass(TTodoTool)
  .BackToMCP

  .Resources
    .UnregisterClass(TTodoResource)
    .UnregisterClass(TTodoAppUI)
  .BackToMCP

  .Prompts
    .ClearAll()
  .BackToMCP

  ;
end;

end.
