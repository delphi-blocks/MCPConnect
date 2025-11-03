program MCPServerDemo;
{$APPTYPE GUI}

uses
  Vcl.Forms,
  Web.WebReq,
  IdHTTPWebBrokerBridge,
  MCPServerDemo.MainForm in 'MCPServerDemo.MainForm.pas' {Form1},
  MCPServerDemo.WebModule in 'MCPServerDemo.WebModule.pas' {WebModule1: TWebModule},
  JSON.RPC.Dispacher in '..\Source\JSON.RPC.Dispacher.pas',
  JSON.RPC in '..\Source\JSON.RPC.pas',
  MCPServerDemo.Api in 'MCPServerDemo.Api.pas',
  MCP.Types in '..\Source\MCP.Types.pas',
  MCP.Invoker in '..\Source\MCP.Invoker.pas',
  MCP.Utils in '..\Source\MCP.Utils.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  if WebRequestHandler <> nil then
    WebRequestHandler.WebModuleClass := WebModuleClass;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
