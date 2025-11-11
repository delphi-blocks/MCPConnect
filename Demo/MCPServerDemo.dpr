program MCPServerDemo;
{$APPTYPE GUI}

uses
  Vcl.Forms,
  Web.WebReq,
  IdHTTPWebBrokerBridge,
  MCPServerDemo.MainForm in 'MCPServerDemo.MainForm.pas' {Form1},
  MCPServerDemo.WebModule in 'MCPServerDemo.WebModule.pas' {WebModule1: TWebModule},
  MCPServerDemo.Tools in 'MCPServerDemo.Tools.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  if WebRequestHandler <> nil then
    WebRequestHandler.WebModuleClass := WebModuleClass;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
