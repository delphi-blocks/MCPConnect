program MCPTest;

uses
  Vcl.Forms,
  MCP.Form.Main in 'MCP.Form.Main.pas' {Form1},
  JRPC.Classes in '..\Source\JRPC.Classes.pas',
  MCP.Tools in '..\Source\MCP.Tools.pas',
  MCP.Attributes in '..\Source\MCP.Attributes.pas',
  MCP.Invoker in '..\Source\MCP.Invoker.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
