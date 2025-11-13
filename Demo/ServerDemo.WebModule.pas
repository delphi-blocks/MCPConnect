unit ServerDemo.WebModule;

interface

uses
  System.SysUtils, System.Classes, Web.HTTPApp,
  Neon.Core.Types,
  Neon.Core.Persistence,

  MCPConnect.JRPC.Server,
  MCPConnect.Transport.WebBroker;

type
  TWebModule1 = class(TWebModule)
    procedure WebModule1DefaultHandlerAction(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure WebModuleCreate(Sender: TObject);
  private
    FJRPCDispatcher: TJRPCDispatcher;
    FJRPCServer: TJRPCServer;
  public
    { Public declarations }
  end;

var
  WebModuleClass: TComponentClass = TWebModule1;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

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

  FJRPCDispatcher := TJRPCDispatcher.Create(Self);
  FJRPCDispatcher.PathInfo := '/jrpc';
  FJRPCDispatcher.Server := FJRPCServer;
end;

end.
