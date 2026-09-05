program MCPConsoleIndyTest;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  MCPConnect.Configuration.Auth in '..\..\Source\MCPConnect.Configuration.Auth.pas',
  MCPConnect.Configuration.Core in '..\..\Source\MCPConnect.Configuration.Core.pas',
  MCPConnect.Configuration.MCP in '..\..\Source\MCPConnect.Configuration.MCP.pas',
  MCPConnect.Configuration.Neon in '..\..\Source\MCPConnect.Configuration.Neon.pas',
  MCPConnect.Content.Writers in '..\..\Source\MCPConnect.Content.Writers.pas',
  MCPConnect.Content.Writers.RTL in '..\..\Source\MCPConnect.Content.Writers.RTL.pas',
  MCPConnect.JRPC.Classes in '..\..\Source\MCPConnect.JRPC.Classes.pas',
  MCPConnect.JRPC.Core in '..\..\Source\MCPConnect.JRPC.Core.pas',
  MCPConnect.JRPC.Invoker in '..\..\Source\MCPConnect.JRPC.Invoker.pas',
  MCPConnect.JRPC.Server in '..\..\Source\MCPConnect.JRPC.Server.pas',
  MCPConnect.MCP.Attributes in '..\..\Source\MCPConnect.MCP.Attributes.pas',
  MCPConnect.MCP.Config in '..\..\Source\MCPConnect.MCP.Config.pas',
  MCPConnect.MCP.Invoker in '..\..\Source\MCPConnect.MCP.Invoker.pas',
  MCPConnect.MCP.Prompts in '..\..\Source\MCPConnect.MCP.Prompts.pas',
  MCPConnect.MCP.Resources in '..\..\Source\MCPConnect.MCP.Resources.pas',
  MCPConnect.MCP.Server.Api in '..\..\Source\MCPConnect.MCP.Server.Api.pas',
  MCPConnect.MCP.Tools in '..\..\Source\MCPConnect.MCP.Tools.pas',
  MCPConnect.MCP.Types in '..\..\Source\MCPConnect.MCP.Types.pas',
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
  LServer: TJRPCIndyServer;
begin
  LServer := TJRPCIndyServer.CreateMCPServer(nil);
  try
    LServer.JRPCServer
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
