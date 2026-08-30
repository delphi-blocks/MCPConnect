program MCPServerWebBroker;

{
  ==============================================================================
   MCPConnect demo - MCP server over WebBroker
  ==============================================================================

  WebBroker is the transport to pick when the MCP endpoint has to live inside
  an existing Delphi web application, or be deployed into a web server rather
  than run as a service of its own.

  The interesting property is that the MCP wiring - in
  MCPServerWebBroker.WebModule.pas - is completely independent of the host:
  this .dpr embeds an Indy bridge so the thing can be run and debugged from the
  IDE, but the same web module compiles unchanged into

      ISAPI dll / Apache module / CGI / FastCGI

  by swapping this project file for the corresponding Delphi web project
  template. Nothing in the web module, the tools or the configuration changes.

  Endpoint with the default port:  http://localhost:8080/mcp

  Caveat worth knowing before you choose this transport: streaming
  server -> client notifications (SSE) over WebBroker needs Delphi 13.1, gated
  by HAS_WEBBROKER_SSE in Source\MCPConnect.inc. On Delphi 11 and 12 use the
  Indy transport (Demo\MCPServer\Indy) when notifications matter.

  As with the other demos, copy the Demo\data folder next to the .exe.
}

{$APPTYPE GUI}

uses
  Vcl.Forms,
  Web.WebReq,
  IdHTTPWebBrokerBridge,
  MCPConnect.Configuration.Auth in '..\..\..\Source\MCPConnect.Configuration.Auth.pas',
  MCPConnect.Configuration.Core in '..\..\..\Source\MCPConnect.Configuration.Core.pas',
  MCPConnect.Configuration.MCP in '..\..\..\Source\MCPConnect.Configuration.MCP.pas',
  MCPConnect.Configuration.Neon in '..\..\..\Source\MCPConnect.Configuration.Neon.pas',
  MCPConnect.Content.Writers in '..\..\..\Source\MCPConnect.Content.Writers.pas',
  MCPConnect.Content.Writers.RTL in '..\..\..\Source\MCPConnect.Content.Writers.RTL.pas',
  MCPConnect.Content.Writers.VCL in '..\..\..\Source\MCPConnect.Content.Writers.VCL.pas',
  MCPConnect.JRPC.Classes in '..\..\..\Source\MCPConnect.JRPC.Classes.pas',
  MCPConnect.JRPC.Core in '..\..\..\Source\MCPConnect.JRPC.Core.pas',
  MCPConnect.JRPC.Invoker in '..\..\..\Source\MCPConnect.JRPC.Invoker.pas',
  MCPConnect.JRPC.Server in '..\..\..\Source\MCPConnect.JRPC.Server.pas',
  MCPConnect.MCP.Attributes in '..\..\..\Source\MCPConnect.MCP.Attributes.pas',
  MCPConnect.MCP.Invoker in '..\..\..\Source\MCPConnect.MCP.Invoker.pas',
  MCPConnect.MCP.Server.Api in '..\..\..\Source\MCPConnect.MCP.Server.Api.pas',
  MCPConnect.MCP.Types.Base in '..\..\..\Source\MCPConnect.MCP.Types.Base.pas',
  MCPConnect.MCP.Types.Prompts in '..\..\..\Source\MCPConnect.MCP.Types.Prompts.pas',
  MCPConnect.MCP.Types.Resources in '..\..\..\Source\MCPConnect.MCP.Types.Resources.pas',
  MCPConnect.MCP.Types.Tools in '..\..\..\Source\MCPConnect.MCP.Types.Tools.pas',
  MCPConnect.MCP.Config in '..\..\..\Source\MCPConnect.MCP.Config.pas',
  MCPConnect.Transport.WebBroker in '..\..\..\Source\MCPConnect.Transport.WebBroker.pas',
  MCPServer.Tools in '..\MCPServer.Tools.pas',
  MCPServer.Resources in '..\MCPServer.Resources.pas',
  MCPServer.Apps in '..\MCPServer.Apps.pas',
  MCPServerWebBroker.Form.Main in 'MCPServerWebBroker.Form.Main.pas' {frmMain},
  MCPServerWebBroker.WebModule in 'MCPServerWebBroker.WebModule.pas' {WebModule1: TWebModule},
  MCPServer.Config in '..\MCPServer.Config.pas',
  MCPServer.Notifications in '..\MCPServer.Notifications.pas',
  MCPServer.Prompts in '..\MCPServer.Prompts.pas';

{$R *.res}

begin
  // MCPConnect frees tool results, GC-registered objects and sessions for you,
  // so a leak dialog on shutdown points at the demo code, not the library.
  ReportMemoryLeaksOnShutdown := True;

  // Tells WebBroker which web module to instantiate per request. The module
  // registers the variable in its own unit (WebModuleClass), so this line is
  // the whole connection between the .dpr and the MCP endpoint.
  if WebRequestHandler <> nil then
    WebRequestHandler.WebModuleClass := WebModuleClass;

  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
