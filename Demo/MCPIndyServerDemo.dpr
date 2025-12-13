program MCPIndyServerDemo;
{$APPTYPE GUI}

uses
  Vcl.Forms,
  MCPConnect.Configuration.Auth in '..\Source\MCPConnect.Configuration.Auth.pas',
  MCPConnect.Configuration.Core in '..\Source\MCPConnect.Configuration.Core.pas',
  MCPConnect.Configuration.MCP in '..\Source\MCPConnect.Configuration.MCP.pas',
  MCPConnect.Configuration.Neon in '..\Source\MCPConnect.Configuration.Neon.pas',
  MCPConnect.Configuration.Session in '..\Source\MCPConnect.Configuration.Session.pas',
  MCPConnect.Content.Writers in '..\Source\MCPConnect.Content.Writers.pas',
  MCPConnect.Content.Writers.RTL in '..\Source\MCPConnect.Content.Writers.RTL.pas',
  MCPConnect.Content.Writers.VCL in '..\Source\MCPConnect.Content.Writers.VCL.pas',
  MCPConnect.Core.Utils in '..\Source\MCPConnect.Core.Utils.pas',
  MCPConnect.JRPC.Core in '..\Source\MCPConnect.JRPC.Core.pas',
  MCPConnect.JRPC.Invoker in '..\Source\MCPConnect.JRPC.Invoker.pas',
  MCPConnect.JRPC.Server in '..\Source\MCPConnect.JRPC.Server.pas',
  MCPConnect.MCP.Attributes in '..\Source\MCPConnect.MCP.Attributes.pas',
  MCPConnect.MCP.Invoker in '..\Source\MCPConnect.MCP.Invoker.pas',
  MCPConnect.MCP.Prompts in '..\Source\MCPConnect.MCP.Prompts.pas',
  MCPConnect.MCP.Resources in '..\Source\MCPConnect.MCP.Resources.pas',
  MCPConnect.MCP.Server.Api in '..\Source\MCPConnect.MCP.Server.Api.pas',
  MCPConnect.MCP.Tools in '..\Source\MCPConnect.MCP.Tools.pas',
  MCPConnect.MCP.Types in '..\Source\MCPConnect.MCP.Types.pas',
  MCPConnect.MCP.Config in '..\Source\MCPConnect.MCP.Config.pas',
  MCPConnect.Session.Core in '..\Source\MCPConnect.Session.Core.pas',
  MCPConnect.Transport.Indy in '..\Source\MCPConnect.Transport.Indy.pas',
  MCPServerDemo.MainFormIndy in 'MCPServerDemo.MainFormIndy.pas' {Form1},
  MCPServerDemo.Tools in 'MCPServerDemo.Tools.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
