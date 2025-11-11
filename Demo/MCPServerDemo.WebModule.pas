unit MCPServerDemo.WebModule;

interface

uses
  System.SysUtils, System.Classes, Web.HTTPApp,

  Neon.Core.Types,
  Neon.Core.Persistence,

  JSON.RPC.Dispacher,
  JSON.RPC.Server;

type
  TWebModule1 = class(TWebModule)
    procedure WebModule1DefaultHandlerAction(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure WebModuleCreate(Sender: TObject);
  private
    FJRPCServer: TJRPCServer;
    FJRPCDispacher: TJRPCDispacher;
  public
    { Public declarations }
  end;

var
  WebModuleClass: TComponentClass = TWebModule1;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

uses
  JSON.RPC,
  JRPC.Configuration.MCP,
  JRPC.Configuration.Auth,
  JRPC.Configuration.Neon,

  MCP.Types,
  // Implemetation of MCP API
  MCP.Server.Api,

  MCPServerDemo.Tools;

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

  FJRPCServer
    .Plugin.Configure<IAuthTokenConfig>
      .SetToken('my-secret-token')
      .ApplyConfig

    .Plugin.Configure<IMCPConfig>
      .SetServerName('delphi-mcp-server')
      .SetServerVersion('2.0.0')
      .SetToolClass(TTestTool)
      .ApplyConfig;

//    .Plugin.Configure<IJRPCNeonConfig>
//      .SetNeonConfig(MCPNeonConfig)
//      .ApplyConfig;

  FJRPCDispacher := TJRPCDispacher.Create(Self);
  FJRPCDispacher.PathInfo := '/mcp';
  FJRPCDispacher.Server := FJRPCServer;
end;

end.
