program MCPTest;

uses
  Vcl.Forms,
  MCP.Form.Main in 'MCP.Form.Main.pas' {frmMain},
  MCP.Tools in '..\Source\MCP.Tools.pas',
  MCP.Attributes in '..\Source\MCP.Attributes.pas',
  MCP.Invoker in '..\Source\MCP.Invoker.pas',
  JSON.RPC in '..\Source\JSON.RPC.pas',
  MCP.Types in '..\Source\MCP.Types.pas',
  MCP.Resources in '..\Source\MCP.Resources.pas',
  MCP.Prompts in '..\Source\MCP.Prompts.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
