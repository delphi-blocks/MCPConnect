unit MCPAuthServerWebBroker.WebModule;

interface

uses
  System.SysUtils, System.Classes, Web.HTTPApp,

  Neon.Core.Types,
  Neon.Core.Persistence,

  MCPConnect.Content.Writers.RTL,
  MCPConnect.Content.Writers.VCL,
  MCPConnect.Transport.WebBroker,
  MCPConnect.JRPC.Server;

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
  JRPCServer: TJRPCServer;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

uses
  MCPAuthServer.Config;

{$R *.dfm}

procedure TWebModule1.WebModule1DefaultHandlerAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
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
    JRPCServer := TJRPCServer.Create(nil);
    TServerConfigurator.ConfigureServer(JRPCServer);
  end;

  LJRPCDispatcher := TJRPCDispatcher.Create(Self);
  LJRPCDispatcher.PathInfo := '*';
  LJRPCDispatcher.Server := JRPCServer;
end;

initialization

  JRPCServer := nil;

finalization

  JRPCServer.Free;

end.
