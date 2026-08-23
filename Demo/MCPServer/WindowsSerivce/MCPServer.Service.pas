unit MCPServer.Service;

{
  ==============================================================================
   MCPConnect demo - MCP server hosted in a Windows service
  ==============================================================================

  Same Indy transport as Demo\MCPServer\Indy, same shared server definition,
  but running under the Service Control Manager instead of a VCL form. This is
  the deployment shape for an MCP server that must be reachable without anyone
  logged on.

  Install / uninstall (from an elevated prompt):

      MCPWindowsService.exe /install
      MCPWindowsService.exe /uninstall

  The service is named MCPServerDemo and listens on port 8080.

  Three things behave differently from the desktop demo, and all three bite:

  1. the current directory is not the executable folder (it is typically
     C:\Windows\system32), so every file access must go through
     TPath.GetAppPath - see TDelphiDayTool.BuyTickets, which still uses
     GetCurrentDir and therefore fails here;
  2. there is no UI and no IDE Event Log to write to, so the Logify debug
     adapter used by the other hosts is useless. Enable the ENABLE_FILE_LOG
     define below to get a real log file instead;
  3. the service account needs read access to the data\ folder (icons, the MCP
     App HTML, the PDF resource) and write access wherever the demo appends
     purchase.log. LocalSystem has it under Program Files; a dedicated service
     account usually does not.
}

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

// Enable to write a real log file (see the initialization section at the
// bottom). Off by default so the demo does not litter the shared documents
// folder; turn it on as soon as something misbehaves, because a service has
// nowhere else to report to.
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
  // Delphi writes the service name and display name at install time but not
  // the description shown in services.msc - hence this registry write.
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
  // Name is the key under HKLM\SYSTEM\CurrentControlSet\Services and the one
  // net start / sc use; DisplayName is what services.msc shows. Changing Name
  // after an install leaves the old registration behind, so uninstall first.
  Name := MCPServiceName;
  DisplayName := MCPServiceName;
end;

procedure TServiceModule.ServiceExecute(Sender: TService);
var
  FServer: TJRPCIndyServer;
begin
  // ServiceExecute *is* the service: everything happens inside it, and the
  // service stops when this method returns. So the server is created, run and
  // destroyed here rather than in OnStart/OnStop.
  FServer := TJRPCIndyServer.CreateMCPServer(nil);
  try
    // No form to read the port from - it is fixed here. A real service would
    // read it from the registry, an .ini or a command-line parameter captured
    // at install time.
    FServer.Bindings.Clear;
    FServer.DefaultPort := 8080;

    // The one line shared with every other host in this demo.
    TServerConfigurator.ConfigureServer(FServer.JRPCServer);

    Logger.LogInfo('Service starting...');
    FServer.Active := True;
    Logger.LogInfo('Service started...');

    // The service main loop. ProcessRequests(True) blocks until the SCM sends
    // something (stop, pause, shutdown), which keeps the thread idle instead
    // of spinning. Terminated is set when a stop request arrives.
    // Indy serves the HTTP requests on its own threads throughout.
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

// A service has no console and no IDE Event Log, so without an adapter the
// library logs nowhere. This one writes mcpservicedemo*.log to the shared
// documents folder - a path the service account can reach, unlike the user
// profile folders.
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
