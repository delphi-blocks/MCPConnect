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

  Both objects are owned by the web module, so their lifetime follows it. That
  matters more than usual here: WebBroker creates one web module *per request
  thread*, so with the default pooling you can get several TJRPCServer instances,
  each with its own configuration and its own session manager. For this demo
  that is harmless; a production server that must share state across threads
  should create the TJRPCServer once (a singleton or a form field) and hand the
  same instance to every dispatcher.

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
  MCPConnect.JRPC.Server;           // TJRPCServer

type
  TWebModule1 = class(TWebModule)
    procedure WebModule1DefaultHandlerAction(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure WebModuleCreate(Sender: TObject);
  private
    /// <summary>
    ///   The protocol engine: JSON-RPC dispatch, plugin configuration chain,
    ///   session manager. Knows nothing about HTTP.
    /// </summary>
    FJRPCServer: TJRPCServer;

    /// <summary>
    ///   The bridge between WebBroker and the engine. Owned by the web module,
    ///   which it also registers itself with.
    /// </summary>
    FJRPCDispatcher: TJRPCDispatcher;
  public
    { Public declarations }
  end;

var
  WebModuleClass: TComponentClass = TWebModule1;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

uses
  MCPServer.Config;

{$R *.dfm}

procedure TWebModule1.WebModule1DefaultHandlerAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  // Anything that is not /mcp lands here. Handy to prove the server is alive
  // from a browser, since the MCP endpoint itself only answers POST (and GET
  // with Accept: text/event-stream).
  Response.Content :=
    '<html>' +
    '<head><title>Web Server Application</title></head>' +
    '<body>Web Server Application</body>' +
    '</html>';
end;

procedure TWebModule1.WebModuleCreate(Sender: TObject);
begin
  // 1) The engine. Owned by the web module (Self), so it is freed with it.
  FJRPCServer := TJRPCServer.Create(Self);

  // 2) Declare what the server exposes - the shared definition used by every
  //    transport in this demo. See ..\MCPServer.Config.pas.
  TServerConfigurator.ConfigureServer(FJRPCServer);

  // 3) The HTTP route. The dispatcher hooks itself into the owning web module
  //    on construction; PathInfo is the mask it answers to, so the endpoint
  //    for this demo is  http://localhost:8080/mcp
  FJRPCDispatcher := TJRPCDispatcher.Create(Self);
  FJRPCDispatcher.PathInfo := '/mcp';
  FJRPCDispatcher.Server := FJRPCServer;
end;

end.
