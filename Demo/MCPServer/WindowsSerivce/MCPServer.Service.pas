unit MCPServer.Service;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  System.IOUtils,
  Vcl.SvcMgr, System.Win.Registry,

  MCPConnect.JRPC.Server,
  MCPConnect.Transport.Indy;

type
  TServiceModule = class(TService)
    procedure ServiceAfterInstall(Sender: TService);
    procedure ServiceAfterUninstall(Sender: TService);
    procedure ServiceExecute(Sender: TService);
    procedure ServiceCreate(Sender: TObject);
  private
    { Private declarations }
  public
    function GetServiceController: TServiceController; override;
    { Public declarations }
  end;

{.$DEFINE ENABLE_FILE_LOG}

var
  ServiceModule: TServiceModule;

implementation

uses
  Logify,
  {$IFDEF ENABLE_FILE_LOG}
  Logify.Adapter.Files,
  {$ENDIF}
  MCPServer.Config;

const
  MCPServiceName = 'MCPServerDemo';

{$R *.dfm}

procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  ServiceModule.Controller(CtrlCode);
end;

function TServiceModule.GetServiceController: TServiceController;
begin
  Result := ServiceController;
end;

procedure TServiceModule.ServiceAfterInstall(Sender: TService);
var
  LReg: TRegistry;
begin
  Logger.LogInfo('Registered ' + DisplayName);
  LReg := TRegistry.Create(KEY_READ or KEY_WRITE);
  try
    LReg.RootKey := HKEY_LOCAL_MACHINE;
    if LReg.OpenKey('\SYSTEM\CurrentControlSet\Services\' + Name, False) then
    begin
      LReg.WriteString('Description', 'Demo WiRL Service');
      LReg.CloseKey;
    end;
  finally
    LReg.Free;
  end;
end;

procedure TServiceModule.ServiceAfterUninstall(Sender: TService);
begin
  Logger.LogInfo('Unregistered ' + DisplayName);
end;

procedure TServiceModule.ServiceCreate(Sender: TObject);
begin
  Name := MCPServiceName;
  DisplayName := MCPServiceName;
end;

procedure TServiceModule.ServiceExecute(Sender: TService);
var
  FServer: TJRPCIndyServer;
begin
  FServer := TJRPCIndyServer.CreateMCPServer(nil);
  try
    FServer.Bindings.Clear;
    FServer.DefaultPort := 8080;

    TServerConfigurator.ConfigureServer(FServer.JRPCServer);

    Logger.LogInfo('Service starting...');
    FServer.Active := True;
    Logger.LogInfo('Service started...');

    while not Terminated do
      ServiceThread.ProcessRequests(True); // wait for termination

    Logger.LogInfo('Service stopping...');
    FServer.Active := False;
    Logger.LogInfo('Service stopped...');
  finally
    FreeAndNil(FServer);
  end;
end;

initialization

{$IFDEF ENABLE_FILE_LOG}
TLoggerAdapterRegistry.Instance.RegisterFactory(
  TLogifyAdapterFilesFactory.CreateAdapterFactory('file',
    procedure(var AConfig: TFileLogConfig)
    begin
      AConfig.Level := TLogLevel.Debug;
      AConfig.SetLogName('mcpservicedemo');
      AConfig.Path := TPath.GetSharedDocumentsPath;
      AConfig.Ext := 'log';
      AConfig.Append := True;
    end
  ));
{$ENDIF}

end.
