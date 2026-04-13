program TestClasses;

uses
  Vcl.Forms,
  Test.Form.Main in 'Test.Form.Main.pas' {frmMain},
  Test.Form.Snippets in 'Test.Form.Snippets.pas' {frmSnippets},
  Test.Form.Misc in 'Test.Form.Misc.pas' {frmMisc},
  MCPConnect.Configuration.Auth in '..\..\Source\MCPConnect.Configuration.Auth.pas',
  MCPConnect.Configuration.Core in '..\..\Source\MCPConnect.Configuration.Core.pas',
  MCPConnect.Configuration.MCP in '..\..\Source\MCPConnect.Configuration.MCP.pas',
  MCPConnect.Configuration.Neon in '..\..\Source\MCPConnect.Configuration.Neon.pas',
  MCPConnect.Configuration.Session in '..\..\Source\MCPConnect.Configuration.Session.pas',
  MCPConnect.Content.Writers in '..\..\Source\MCPConnect.Content.Writers.pas',
  MCPConnect.Content.Writers.RTL in '..\..\Source\MCPConnect.Content.Writers.RTL.pas',
  MCPConnect.Content.Writers.VCL in '..\..\Source\MCPConnect.Content.Writers.VCL.pas',
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
  MCPConnect.MCP.Types in '..\..\Source\MCPConnect.MCP.Types.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmSnippets, frmSnippets);
  Application.CreateForm(TfrmMisc, frmMisc);
  Application.Run;
end.
