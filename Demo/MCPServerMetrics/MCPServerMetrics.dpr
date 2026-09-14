program MCPServerMetrics;

{
  ==============================================================================
   MCPConnect demo - MCP server over HTTP, hosted by an Indy TCP/HTTP server
  ==============================================================================

  Run this project, press Start, and point an MCP client at

      http://localhost:8080/

  Things to know before you build:

  * copy (or link) the Demo\data folder next to the produced .exe - it holds
    the tool icons, the MCP App HTML, index.md and the PDF resource;
  * the MCPConnect units are listed below with explicit relative paths so that
    the demo builds without installing the packages first. In your own project
    you would instead add Source\ (plus Libs\Neon\Source, Libs\Logify\Source
    and the JOSE folders) to the project search path and keep the uses clause
    short;
  * the server definition itself lives in ..\MCPServer.Config.pas and is
    shared, unchanged, with the WebBroker, Stdio and Windows Service flavours
    of this same demo.
}

{$APPTYPE GUI}

uses
  Vcl.Forms,
  Server.Config in 'Server.Config.pas',
  Server.Tools in 'Server.Tools.pas',
  Server.Middleware in 'Server.Middleware.pas',
  Server.Dashboard in 'Server.Dashboard.pas' {FrameDashboard: TFrameMetricsDashboard},
  Server.Metrics in 'Server.Metrics.pas',
  Server.Metrics.Features in 'Server.Metrics.Features.pas',
  Server.Metrics.Middleware in 'Server.Metrics.Middleware.pas',
  Server.Form.Main in 'Server.Form.Main.pas' {frmMain};

{$R *.res}

begin
  // The demo runs with leak reporting on: MCPConnect frees tool results, GC-
  // registered objects and sessions for you, so a leak dialog on shutdown
  // means the demo code (not the library) forgot something.
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
