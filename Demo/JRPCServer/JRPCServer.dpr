program JRPCServer;
{$APPTYPE GUI}

uses
  Vcl.Forms,
  Web.WebReq,
  IdHTTPWebBrokerBridge,
  JRPCServer.UI.Main in 'JRPCServer.UI.Main.pas' {Form1},
  JRPCServer.WebModule in 'JRPCServer.WebModule.pas' {WebModule1: TWebModule},
  JRPCServer.JRPC.Api in 'JRPCServer.JRPC.Api.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  if WebRequestHandler <> nil then
    WebRequestHandler.WebModuleClass := WebModuleClass;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
