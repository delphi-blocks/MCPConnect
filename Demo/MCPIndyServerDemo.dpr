program MCPIndyServerDemo;
{$APPTYPE GUI}

uses
  Vcl.Forms,
  MCPServerDemo.MainFormIndy in 'MCPServerDemo.MainFormIndy.pas' {Form1},
  MCPServerDemo.Tools in 'MCPServerDemo.Tools.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
