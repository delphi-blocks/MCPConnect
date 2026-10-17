program MCPServerWebStencils;

{
  ==============================================================================
   MCPConnect demo - the metrics dashboard, rendered with WebStencils
  ==============================================================================

   The same metrics the Indy showcase (..\MCPServerMetrics.dproj) serves as JSON
   on GET /metrics, served here as HTML by Embarcadero's WebStencils template
   engine - server-side, in the same process as the MCP server, with no build
   step and no JavaScript framework.

   One web module answers everything (MetricsWeb.WebModule.pas):

       POST /mcp              the MCP endpoint (TMCPDispatcher)
       GET  /dashboard        the page, also at '/'
       GET  /dashboard/live   the fragment the page's script swaps in
       GET  /dashboard.css    the stylesheet, served verbatim
       GET  /metrics          the same numbers as JSON

   The page is rendered from the metrics of this process, so there is no HTTP
   hop and nothing to configure for CORS. Templates are in templates\, next to
   the executable this project produces (or wherever -templates points).

   WebStencils needs Delphi 12 or newer: the unit Web.Stencils is part of the
   RTL there and not in Delphi 11.

   Deployment: the web module compiles unchanged into ISAPI, Apache, CGI or
   FastCGI - only this project file changes.
}

{$APPTYPE GUI}

uses
  Vcl.Forms,
  Web.WebReq,
  IdHTTPWebBrokerBridge,
  MetricsWeb.Form.Main in 'MetricsWeb.Form.Main.pas' {frmMain},
  MetricsWeb.WebModule in 'MetricsWeb.WebModule.pas' {WebModule1: TWebModule},
  MetricsWeb.View in 'MetricsWeb.View.pas',
  MetricsWeb.History in 'MetricsWeb.History.pas',
  Server.Config in '..\Server.Config.pas',
  Server.Tools in '..\Server.Tools.pas',
  Server.Middleware in '..\Server.Middleware.pas',
  Server.Metrics in '..\Server.Metrics.pas',
  Server.Metrics.Document in '..\Server.Metrics.Document.pas',
  Server.Metrics.Middleware in '..\Server.Metrics.Middleware.pas',
  MCPConnect.Configuration.Auth in '..\..\..\Source\MCPConnect.Configuration.Auth.pas',
  MCPConnect.Configuration.Core in '..\..\..\Source\MCPConnect.Configuration.Core.pas',
  MCPConnect.Configuration.MCP in '..\..\..\Source\MCPConnect.Configuration.MCP.pas',
  MCPConnect.Configuration.Neon in '..\..\..\Source\MCPConnect.Configuration.Neon.pas',
  MCPConnect.Content.Writers in '..\..\..\Source\MCPConnect.Content.Writers.pas',
  MCPConnect.Content.Writers.RTL in '..\..\..\Source\MCPConnect.Content.Writers.RTL.pas',
  MCPConnect.Content.Writers.VCL in '..\..\..\Source\MCPConnect.Content.Writers.VCL.pas',
  MCPConnect.MCP.Attributes in '..\..\..\Source\MCPConnect.MCP.Attributes.pas',
  MCPConnect.MCP.Invoker in '..\..\..\Source\MCPConnect.MCP.Invoker.pas',
  MCPConnect.MCP.Server.Api in '..\..\..\Source\MCPConnect.MCP.Server.Api.pas',
  MCPConnect.MCP.Types.Base in '..\..\..\Source\MCPConnect.MCP.Types.Base.pas',
  MCPConnect.MCP.Types.Prompts in '..\..\..\Source\MCPConnect.MCP.Types.Prompts.pas',
  MCPConnect.MCP.Types.Resources in '..\..\..\Source\MCPConnect.MCP.Types.Resources.pas',
  MCPConnect.MCP.Types.Tools in '..\..\..\Source\MCPConnect.MCP.Types.Tools.pas',
  MCPConnect.MCP.Config in '..\..\..\Source\MCPConnect.MCP.Config.pas',
  MCPConnect.Transport.WebBroker in '..\..\..\Source\MCPConnect.Transport.WebBroker.pas',
  MCPConnect.Metrics in '..\..\..\Source\MCPConnect.Metrics.pas',
  MCPConnect.Metrics.Exporters in '..\..\..\Source\MCPConnect.Metrics.Exporters.pas',
  MCPConnect.Metrics.Exporters.Files in '..\..\..\Source\MCPConnect.Metrics.Exporters.Files.pas',
  MCPConnect.Metrics.Exporters.Otlp in '..\..\..\Source\MCPConnect.Metrics.Exporters.Otlp.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;

  // Tells WebBroker which web module to instantiate per request. The module
  // registers the variable in its own unit (WebModuleClass).
  if WebRequestHandler <> nil then
    WebRequestHandler.WebModuleClass := WebModuleClass;

  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
