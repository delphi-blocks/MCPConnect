unit MCPServerWebBroker.WebModule;

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
    FJRPCServer: TJRPCServer;
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
  Response.Content :=
    '<html>' +
    '<head><title>Web Server Application</title></head>' +
    '<body>Web Server Application</body>' +
    '</html>';
end;

procedure TWebModule1.WebModuleCreate(Sender: TObject);
begin
  FJRPCServer := TJRPCServer.Create(Self);

  TServerConfigurator.ConfigureServer(FJRPCServer);

  FJRPCDispatcher := TJRPCDispatcher.Create(Self);
  FJRPCDispatcher.PathInfo := '/mcp';
  FJRPCDispatcher.Server := FJRPCServer;
end;

end.
