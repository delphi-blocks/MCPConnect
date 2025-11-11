program MCPTest;

uses
  Vcl.Forms,
  MCP.Form.Main in 'MCP.Form.Main.pas' {frmMain};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
