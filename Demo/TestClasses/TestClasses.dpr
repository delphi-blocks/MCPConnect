program TestClasses;

uses
  Vcl.Forms,
  Test.Form.Main in 'Test.Form.Main.pas' {frmMain},
  Test.Form.Snippets in 'Test.Form.Snippets.pas' {frmSnippets};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmSnippets, frmSnippets);
  Application.Run;
end.
