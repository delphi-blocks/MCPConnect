program TestClasses;

uses
  Vcl.Forms,
  Test.Form.Main in 'Test.Form.Main.pas' {frmMain},
  Test.Form.Snippets in 'Test.Form.Snippets.pas' {frmSnippets},
  Test.Form.Misc in 'Test.Form.Misc.pas' {frmMisc};

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
