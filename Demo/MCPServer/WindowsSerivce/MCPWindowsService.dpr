program MCPWindowsService;

{
  ==============================================================================
   MCPConnect demo - MCP server as a Windows service
  ==============================================================================

  A VCL service application (Vcl.SvcMgr) hosting the Indy transport, so the MCP
  endpoint is available on port 8080 with nobody logged on.

      MCPWindowsService.exe /install      (elevated)
      MCPWindowsService.exe /uninstall

  Service name and display name: MCPServerDemo.

  All the MCP work happens in MCPServer.Service.pas; the server definition is
  the shared ..\MCPServer.Config.pas, identical to the desktop demos. Read the
  header of MCPServer.Service.pas before deploying: the working directory, the
  absence of any log destination and the service account permissions all
  differ from the IDE experience.

  Note that this project links no content-writer units: it is a service, not a
  VCL forms application, so the TPicture/TBitmap writers are compiled out of
  MCPServer.Config along with the tools that would return them.
}

uses
  Vcl.SvcMgr,
  MCPServer.Service in 'MCPServer.Service.pas' {ServiceModule: TService},

  // The shared server definition and the classes it registers - the same six
  // units every flavour of this demo links.
  MCPServer.Config in '..\MCPServer.Config.pas',
  MCPServer.Tools in '..\MCPServer.Tools.pas',
  MCPServer.Notifications in '..\MCPServer.Notifications.pas',
  MCPServer.Apps in '..\MCPServer.Apps.pas',
  MCPServer.Prompts in '..\MCPServer.Prompts.pas',
  MCPServer.Resources in '..\MCPServer.Resources.pas',
  MCPServer.Tools.Test in '..\MCPServer.Tools.Test.pas';

{$R *.RES}

begin
  // Boilerplate from the Delphi service application template, kept verbatim.
  //
  // Windows 2003 Server requires StartServiceCtrlDispatcher to be
  // called before CoRegisterClassObject, which can be called indirectly
  // by Application.Initialize. TServiceApplication.DelayInitialize allows
  // Application.Initialize to be called from TService.Main (after
  // StartServiceCtrlDispatcher has been called).
  //
  // Delayed initialization of the Application object may affect
  // events which then occur prior to initialization, such as
  // TService.OnCreate. It is only recommended if the ServiceApplication
  // registers a class object with OLE and is intended for use with
  // Windows 2003 Server.
  //
  // Application.DelayInitialize := True;
  //
  if not Application.DelayInitialize or Application.Installing then
    Application.Initialize;
  Application.CreateForm(TServiceModule, ServiceModule);
  Application.Run;
end.
