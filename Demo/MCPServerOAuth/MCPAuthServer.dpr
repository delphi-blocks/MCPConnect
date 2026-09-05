program MCPAuthServer;
{$APPTYPE GUI}

uses
  Vcl.Forms,
  MCPAuthServerIndy.Form.Main in 'MCPAuthServerIndy.Form.Main.pas' {frmMain},
  MCPAuthServer.Config in 'MCPAuthServer.Config.pas',
  MCPAuthServer.Tools in 'MCPAuthServer.Tools.pas',
  MCPConnect.Configuration.Auth in '..\..\Source\MCPConnect.Configuration.Auth.pas',
  MCPConnect.Configuration.Core in '..\..\Source\MCPConnect.Configuration.Core.pas',
  MCPConnect.Configuration.MCP in '..\..\Source\MCPConnect.Configuration.MCP.pas',
  MCPConnect.Configuration.Neon in '..\..\Source\MCPConnect.Configuration.Neon.pas',
  MCPConnect.Content.Writers in '..\..\Source\MCPConnect.Content.Writers.pas',
  MCPConnect.Content.Writers.RTL in '..\..\Source\MCPConnect.Content.Writers.RTL.pas',
  MCPConnect.Content.Writers.VCL in '..\..\Source\MCPConnect.Content.Writers.VCL.pas',
  MCPConnect.JRPC.Core in '..\..\Source\MCPConnect.JRPC.Core.pas',
  MCPConnect.JRPC.Invoker in '..\..\Source\MCPConnect.JRPC.Invoker.pas',
  MCPConnect.JRPC.Server in '..\..\Source\MCPConnect.JRPC.Server.pas',
  MCPConnect.MCP.Attributes in '..\..\Source\MCPConnect.MCP.Attributes.pas',
  MCPConnect.MCP.Config in '..\..\Source\MCPConnect.MCP.Config.pas',
  MCPConnect.MCP.Invoker in '..\..\Source\MCPConnect.MCP.Invoker.pas',
  MCPConnect.MCP.Types.Base in '..\..\Source\MCPConnect.MCP.Types.Base.pas',
  MCPConnect.MCP.Types.Prompts in '..\..\Source\MCPConnect.MCP.Types.Prompts.pas',
  MCPConnect.MCP.Types.Resources in '..\..\Source\MCPConnect.MCP.Types.Resources.pas',
  MCPConnect.MCP.Types.Tools in '..\..\Source\MCPConnect.MCP.Types.Tools.pas',
  MCPConnect.MCP.Server.Api in '..\..\Source\MCPConnect.MCP.Server.Api.pas',
  MCPConnect.Transport.Indy in '..\..\Source\MCPConnect.Transport.Indy.pas',
  MCPConnect.JRPC.Classes in '..\..\Source\MCPConnect.JRPC.Classes.pas',
  MCPConnect.Transport.Base in '..\..\Source\MCPConnect.Transport.Base.pas',
  MCPConnect.Transport.AcceptParser in '..\..\Source\MCPConnect.Transport.AcceptParser.pas',
  MCPConnect.Transport.MediaType in '..\..\Source\MCPConnect.Transport.MediaType.pas',
  MCPConnect.Security.Jwks in '..\..\Source\MCPConnect.Security.Jwks.pas',
  MCPConnect.Security.Token in '..\..\Source\MCPConnect.Security.Token.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
