program MCPTest;

uses
  Vcl.Forms,
  MCP.Form.Main in 'MCP.Form.Main.pas' {frmMain},
  MCP.Form.Snippets in 'MCP.Form.Snippets.pas' {frmSnippets};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmSnippets, frmSnippets);
  Application.Run;
end.
