program MCPTest;

uses
  Vcl.Forms,
  MCP.Form.Main in 'MCP.Form.Main.pas' {Form1},
  JRPC.Classes in '..\Source\JRPC.Classes.pas',
  MCP.Core.Tools in '..\Source\MCP.Core.Tools.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
