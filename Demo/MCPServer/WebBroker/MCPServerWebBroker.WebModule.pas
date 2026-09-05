unit MCPServerWebBroker.WebModule;

{
  ==============================================================================
   MCPConnect demo - the WebBroker web module
  ==============================================================================

  This is where the MCP server is plugged into WebBroker. Two objects live here:

    TJRPCServer      the protocol engine (transport agnostic)
    TJRPCDispatcher  an IWebDispatch component that routes the requests
                     matching PathInfo to that engine

  TJRPCDispatcher registers itself with its owning TWebModule in its
  constructor, so there is no action to add to the Actions collection and no
  OnAction handler to write - setting PathInfo and Server is the whole wiring.
  Requests that do *not* match PathInfo fall through to the default handler
  below, which is why the endpoint and an ordinary web site can coexist in one
  module.

  WebBroker creates one web module *per request thread*, so the TJRPCServer is
  created as a global singleton (no owner) shared by every dispatcher instance.
  This ensures a single session manager and configuration across all threads.
  The server is freed in the finalization section.

  Deployment: the same web module compiles unchanged into a standalone Indy
  host (this demo), ISAPI, Apache, CGI or FastCGI - only the .dpr changes.

  SSE note: streaming server -> client notifications over WebBroker requires
  Delphi 13.1 (the HAS_WEBBROKER_SSE define in Source\MCPConnect.inc). On
  Delphi 11 and 12 the responses are still correct, but notifications cannot be
  pushed - use the Indy transport (Demo\MCPServer\Indy) if you need them.
}

interface

uses
  System.SysUtils, System.Classes, Web.HTTPApp,

  Neon.Core.Types,
  Neon.Core.Persistence,

  // Content writers must be linked in the host that uses them: the demo
  // registers them from MCPServer.Config, but the units have to be reachable.
  MCPConnect.Content.Writers.RTL,
  MCPConnect.Content.Writers.VCL,

  MCPConnect.Transport.WebBroker,   // TJRPCDispatcher
  MCPConnect.MCP.Server;            // TMCPServer

type
  TWebModule1 = class(TWebModule)
    procedure WebModule1DefaultHandlerAction(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure WebModuleCreate(Sender: TObject);
  private
  public
    { Public declarations }
  end;

var
  WebModuleClass: TComponentClass = TWebModule1;
  JRPCServer: TMCPServer;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

uses
  MCPServer.Config;

{$R *.dfm}

procedure TWebModule1.WebModule1DefaultHandlerAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  // Anything that is not /mcp lands here
  Response.Content :=
    '<html>' +
    '<head><title>Web Server Application</title></head>' +
    '<body>Web Server Application</body>' +
    '</html>';
end;

procedure TWebModule1.WebModuleCreate(Sender: TObject);
var
  LJRPCDispatcher: TJRPCDispatcher;
begin
  // Singleton: WebBroker creates one web module per thread, but all share one server
  if not Assigned(JRPCServer) then
  begin
    JRPCServer := TMCPServer.Create(nil);
    TServerConfigurator.ConfigureServer(JRPCServer);
  end;

  // The dispatcher hooks itself into the owning web module
  LJRPCDispatcher := TJRPCDispatcher.Create(Self);
  LJRPCDispatcher.PathInfo := '/mcp';
  LJRPCDispatcher.Server := JRPCServer;
end;

initialization

  JRPCServer := nil;

finalization

  JRPCServer.Free;

end.
