program MCPServerStdio;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  MCPConnect.Configuration.Auth in '..\..\..\Source\MCPConnect.Configuration.Auth.pas',
  MCPConnect.Configuration.Core in '..\..\..\Source\MCPConnect.Configuration.Core.pas',
  MCPConnect.Configuration.MCP in '..\..\..\Source\MCPConnect.Configuration.MCP.pas',
  MCPConnect.Configuration.Neon in '..\..\..\Source\MCPConnect.Configuration.Neon.pas',
  MCPConnect.Configuration.Session in '..\..\..\Source\MCPConnect.Configuration.Session.pas',
  MCPConnect.JRPC.Core in '..\..\..\Source\MCPConnect.JRPC.Core.pas',
  MCPConnect.JRPC.Server in '..\..\..\Source\MCPConnect.JRPC.Server.pas',
  MCPConnect.JRPC.Invoker in '..\..\..\Source\MCPConnect.JRPC.Invoker.pas',
  MCPConnect.MCP.Config in '..\..\..\Source\MCPConnect.MCP.Config.pas',
  MCPConnect.MCP.Resources in '..\..\..\Source\MCPConnect.MCP.Resources.pas',
  MCPConnect.MCP.Server.Api in '..\..\..\Source\MCPConnect.MCP.Server.Api.pas',
  MCPConnect.MCP.Tools in '..\..\..\Source\MCPConnect.MCP.Tools.pas',
  MCPConnect.MCP.Attributes in '..\..\..\Source\MCPConnect.MCP.Attributes.pas',
  MCPConnect.MCP.Prompts in '..\..\..\Source\MCPConnect.MCP.Prompts.pas',
  MCPConnect.MCP.Invoker in '..\..\..\Source\MCPConnect.MCP.Invoker.pas',
  MCPConnect.MCP.Types in '..\..\..\Source\MCPConnect.MCP.Types.pas',
  MCPConnect.JRPC.Classes in '..\..\..\Source\MCPConnect.JRPC.Classes.pas',
  MCPConnect.Session.Core in '..\..\..\Source\MCPConnect.Session.Core.pas',
  MCPConnect.Content.Writers in '..\..\..\Source\MCPConnect.Content.Writers.pas',
  MCPConnect.Content.Writers.RTL in '..\..\..\Source\MCPConnect.Content.Writers.RTL.pas',
  MCPConnect.Content.Writers.VCL in '..\..\..\Source\MCPConnect.Content.Writers.VCL.pas',
  MCPConnect.Transport.Stdio in '..\..\..\Source\MCPConnect.Transport.Stdio.pas',
  MCPServer.Tools in '..\MCPServer.Tools.pas',
  MCPServer.Config in '..\MCPServer.Config.pas',
  MCPServer.Resources in '..\MCPServer.Resources.pas',
  MCPServer.Apps in '..\MCPServer.Apps.pas',
  MCPServer.Notifications in '..\MCPServer.Notifications.pas',
  MCPServer.Prompts in '..\MCPServer.Prompts.pas';

procedure StartServer;
var
  LServer: TJRPCStdioServer;
begin
  LServer := TJRPCStdioServer.Create(nil);
  try
    TServerConfigurator.ConfigureServer(LServer.JRPCServer);
    LServer.StartServerAndWait;
  finally
    LServer.Free;
  end;
end;

begin
  ReportMemoryLeaksOnShutdown := True;
  try
    StartServer;
  except
    on E: Exception do
      Writeln(ErrOutput, E.ClassName, ': ', E.Message);
  end;
end.
