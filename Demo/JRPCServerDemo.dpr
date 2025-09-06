program JRPCServerDemo;
{$APPTYPE GUI}

uses
  Vcl.Forms,
  Web.WebReq,
  IdHTTPWebBrokerBridge,
  ServerDemo.UI.Main in 'ServerDemo.UI.Main.pas' {Form1},
  ServerDemo.WebModule in 'ServerDemo.WebModule.pas' {WebModule1: TWebModule},
  JSON.RPC.Dispacher in '..\Source\JSON.RPC.Dispacher.pas',
  JSON.RPC in '..\Source\JSON.RPC.pas',
  ServerDemo.JRPC.Api in 'ServerDemo.JRPC.Api.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  if WebRequestHandler <> nil then
    WebRequestHandler.WebModuleClass := WebModuleClass;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
