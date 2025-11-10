unit MCPServerDemo.WebModule;

interface

uses
  System.SysUtils, System.Classes, Web.HTTPApp,
  Neon.Core.Types,
  Neon.Core.Persistence,
  JSON.RPC.Dispacher, JRPC.Server;

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
  JRPC.Configuration.Authentication, JRPC.Configuration.Neon;

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
    .Plugin.Configure<IJRCPAuthTokenConfig>()
      .SetTokenLocation(TAuthTokenLocation.Header)
      .SetTokenCustomHeader('x-test')
      .SetToken('my-secret-token')
      .ApplyConfig;

//    .Plugin.Configure<IJRPCNeonConfig>
        //TNeonConfiguration.Camel
        //.SetMembers([TNeonMembers.Fields]);
        //TJSONValueSerializer);
//      .ApplyConfig;

  FJRPCDispacher := TJRPCDispacher.Create(Self);
  FJRPCDispacher.PathInfo := '/mcp';
  FJRPCDispacher.Server := FJRPCServer;
end;

end.
