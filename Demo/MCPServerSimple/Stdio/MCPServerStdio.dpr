program MCPServerStdio;

{
  ==============================================================================
   MCPConnect demo - MCP server over STDIO
  ==============================================================================

  The simplest transport there is, and the one Claude Desktop speaks natively.

  There is no socket, no port and no URL: the MCP *client* launches this
  executable as a child process and exchanges JSON-RPC messages with it over
  the standard input/output pipes, one message per line. That has a few
  consequences worth knowing:

  * one client per process. The client starts an instance for itself and kills
    it when the conversation ends - so sessions are implicit and there is
    nothing to configure for them;
  * no authentication is needed or possible. Whoever can launch the process is
    already trusted; there are no headers to carry a token and no 401 a client
    could act on. MCPConnect therefore skips the OAuth gate entirely for STDIO;
  * anything written to *stdout* that is not a JSON-RPC message corrupts the
    stream. Never Writeln for diagnostics - use stderr (as the exception
    handler below does) or a Logify file adapter;
  * server -> client notifications still work: they are written to the same
    stdout as extra JSON lines, so the progress messages and list-changed
    notifications enqueued by the tools behave exactly as they do over SSE.

  To register it with Claude Desktop, add an entry to the mcpServers object of
  claude_desktop_config.json named, say, "delphi-day", whose "command" member
  is the full path to MCPServerStdio.exe. Optional "args" and "env" members
  are passed to the process as you would expect.

  (A JSON sample cannot be shown here: Delphi block comments do not nest, so
  the first closing brace would end this comment.)

  Remember that the process inherits the working directory chosen by the
  client, so refer to files through TPath.GetAppPath, never GetCurrentDir.
}

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
    // Exactly the same call the Indy, WebBroker and service hosts make: the
    // server definition knows nothing about the transport carrying it.
    TServerConfigurator.ConfigureServer(LServer.JRPCServer);

    // Blocks until the client closes the pipes. If the host process needs to
    // do something else in the meantime (drive a UI, poll a queue), use
    // StartServer and call ProcessRequests in your own loop instead - see
    // TJRPCStdioServer in MCPConnect.Transport.Stdio.pas.
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
      // ErrOutput, not the default output: stdout belongs to the protocol.
      Writeln(ErrOutput, E.ClassName, ': ', E.Message);
  end;
end.
