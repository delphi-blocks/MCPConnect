program MCPConsoleIndyTest;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  JRPC.Classes in '..\..\Libs\JRPC\Source\JRPC.Classes.pas',
  JRPC.Core in '..\..\Libs\JRPC\Source\JRPC.Core.pas',
  JRPC.Invoker in '..\..\Libs\JRPC\Source\JRPC.Invoker.pas',
  JRPC.Server in '..\..\Libs\JRPC\Source\JRPC.Server.pas',
  MCPConnect.Configuration.Auth in '..\..\Source\MCPConnect.Configuration.Auth.pas',
  MCPConnect.Configuration.Core in '..\..\Source\MCPConnect.Configuration.Core.pas',
  MCPConnect.Configuration.MCP in '..\..\Source\MCPConnect.Configuration.MCP.pas',
  MCPConnect.Configuration.Neon in '..\..\Source\MCPConnect.Configuration.Neon.pas',
  MCPConnect.Content.Writers in '..\..\Source\MCPConnect.Content.Writers.pas',
  MCPConnect.Content.Writers.RTL in '..\..\Source\MCPConnect.Content.Writers.RTL.pas',
  MCPConnect.JRPC.Middleware in '..\..\Source\MCPConnect.JRPC.Middleware.pas',
  MCPConnect.MCP.Attributes in '..\..\Source\MCPConnect.MCP.Attributes.pas',
  MCPConnect.MCP.Config in '..\..\Source\MCPConnect.MCP.Config.pas',
  MCPConnect.MCP.Invoker in '..\..\Source\MCPConnect.MCP.Invoker.pas',
  MCPConnect.MCP.Middleware in '..\..\Source\MCPConnect.MCP.Middleware.pas',
  MCPConnect.MCP.Server in '..\..\Source\MCPConnect.MCP.Server.pas',
  MCPConnect.MCP.Server.Api in '..\..\Source\MCPConnect.MCP.Server.Api.pas',
  MCPConnect.MCP.Types.Base in '..\..\Source\MCPConnect.MCP.Types.Base.pas',
  MCPConnect.MCP.Types.Mrtr in '..\..\Source\MCPConnect.MCP.Types.Mrtr.pas',
  MCPConnect.MCP.Types.Prompts in '..\..\Source\MCPConnect.MCP.Types.Prompts.pas',
  MCPConnect.MCP.Types.Resources in '..\..\Source\MCPConnect.MCP.Types.Resources.pas',
  MCPConnect.MCP.Types.Tool in '..\..\Source\MCPConnect.MCP.Types.Tool.pas',
  MCPConnect.MCP.Types.Tools in '..\..\Source\MCPConnect.MCP.Types.Tools.pas',
  MCPConnect.Security.Jwks in '..\..\Source\MCPConnect.Security.Jwks.pas',
  MCPConnect.Security.Token.JOSE in '..\..\Source\MCPConnect.Security.Token.JOSE.pas',
  MCPConnect.Security.Token in '..\..\Source\MCPConnect.Security.Token.pas',
  MCPConnect.Transport.AcceptParser in '..\..\Source\MCPConnect.Transport.AcceptParser.pas',
  MCPConnect.Transport.Base in '..\..\Source\MCPConnect.Transport.Base.pas',
  MCPConnect.Transport.Indy in '..\..\Source\MCPConnect.Transport.Indy.pas',
  MCPConnect.Transport.MediaType in '..\..\Source\MCPConnect.Transport.MediaType.pas',
  ConsoleTest.Tools in 'ConsoleTest.Tools.pas';

procedure StartServer;
var
  LServer: TMCPIndyServer;
begin
  LServer := TMCPIndyServer.CreateMCPServer(nil);
  try
    LServer.MCPServer
      .Plugin.Configure<IMCPConfig>
        .Server
          .SetName('mcp-console-test')
          .SetVersion('1.0.0')
        .BackToMCP
        .Tools
          .RegisterClass(TTestTool)
        .BackToMCP
      .ApplyConfig;

    LServer.Bindings.Clear;
    LServer.DefaultPort := 8080;
    LServer.Active := True;

    Writeln('MCP server listening on port 8080. Press Enter to stop.');
    Readln;

    LServer.Active := False;
  finally
    LServer.Free;
  end;
end;

begin
  try
    StartServer;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
