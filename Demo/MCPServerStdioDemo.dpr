program MCPServerStdioDemo;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  MCPConnect.Configuration.Auth in '..\Source\MCPConnect.Configuration.Auth.pas',
  MCPConnect.Configuration.Core in '..\Source\MCPConnect.Configuration.Core.pas',
  MCPConnect.Configuration.MCP in '..\Source\MCPConnect.Configuration.MCP.pas',
  MCPConnect.Configuration.Neon in '..\Source\MCPConnect.Configuration.Neon.pas',
  MCPConnect.Configuration.Session in '..\Source\MCPConnect.Configuration.Session.pas',
  MCPConnect.JRPC.Core in '..\Source\MCPConnect.JRPC.Core.pas',
  MCPConnect.MCP.Config in '..\Source\MCPConnect.MCP.Config.pas',
  MCPConnect.MCP.Resources in '..\Source\MCPConnect.MCP.Resources.pas',
  MCPConnect.MCP.Server.Api in '..\Source\MCPConnect.MCP.Server.Api.pas',
  MCPConnect.MCP.Tools in '..\Source\MCPConnect.MCP.Tools.pas',
  MCPConnect.Transport.Stdio in '..\Source\MCPConnect.Transport.Stdio.pas',
  MCPConnect.Content.Writers in '..\Source\MCPConnect.Content.Writers.pas',
  MCPConnect.MCP.Attributes in '..\Source\MCPConnect.MCP.Attributes.pas',
  MCPConnect.Core.Utils in '..\Source\MCPConnect.Core.Utils.pas',
  MCPConnect.MCP.Prompts in '..\Source\MCPConnect.MCP.Prompts.pas',
  MCPConnect.JRPC.Invoker in '..\Source\MCPConnect.JRPC.Invoker.pas',
  MCPConnect.MCP.Invoker in '..\Source\MCPConnect.MCP.Invoker.pas',
  MCPConnect.MCP.Types in '..\Source\MCPConnect.MCP.Types.pas',
  MCPConnect.JRPC.Server in '..\Source\MCPConnect.JRPC.Server.pas',
  MCPConnect.Session.Core in '..\Source\MCPConnect.Session.Core.pas',
  MCPServerDemo.Tools in 'MCPServerDemo.Tools.pas',
  MCPConnect.Content.Writers.RTL in '..\Source\MCPConnect.Content.Writers.RTL.pas',
  MCPConnect.Content.Writers.VCL in '..\Source\MCPConnect.Content.Writers.VCL.pas';

procedure StartServer;
var
  LJRPCServer: TJRPCServer;
  LStdioServer: TJRPCStdioServer;
begin
  LJRPCServer := TJRPCServer.Create(nil);
  try
    LJRPCServer
      .Plugin.Configure<IAuthTokenConfig>
        .SetToken('my-secret-token')
        .ApplyConfig

      .Plugin.Configure<ISessionConfig>
        .SetTimeout(30)  // 30 minutes timeout
        .SetSessionClass(TShoppingSession)  // Use custom typed session
        .ApplyConfig

      .Plugin.Configure<IMCPConfig>
        .Server
          .SetName('delphi-mcp-server')
          .SetVersion('2.0.0')
          .SetCapabilities([Tools, Resources])


          .RegisterWriter(TMCPImageWriter)
          .RegisterWriter(TMCPPictureWriter)
          .RegisterWriter(TMCPStreamWriter)
          .RegisterWriter(TMCPStringListWriter)
        .BackToMCP

      .Tools
        .RegisterClass(TTestTool)
        .RegisterClass(TDelphiDayTool)
        .RegisterClass(TShoppingCartTool)  // Session-based shopping cart
      .BackToMCP
    ;

    LStdioServer := TJRPCStdioServer.Create(nil);
    try
      LStdioServer.Server := LJRPCServer;
//      LStdioServer.StartServer;
//      while not LStdioServer.Terminated do
//      begin
//        LStdioServer.ProcessRequests;
//        Sleep(1000);
//        Writeln('ping');
//      end;
      LStdioServer.StartServerAndWait;

    finally
      LStdioServer.Free;
    end;
  finally
    LJRPCServer.Free;
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
