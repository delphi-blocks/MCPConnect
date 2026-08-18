program MCPWindowsService;

uses
  Vcl.SvcMgr,
  MCPServer.Service in 'MCPServer.Service.pas' {ServiceModule: TService},
  MCPServer.Config in '..\MCPServer.Config.pas',
  MCPServer.Tools in '..\MCPServer.Tools.pas',
  MCPServer.Notifications in '..\MCPServer.Notifications.pas',
  MCPServer.Apps in '..\MCPServer.Apps.pas',
  MCPServer.Prompts in '..\MCPServer.Prompts.pas',
  MCPServer.Resources in '..\MCPServer.Resources.pas',
  MCPServer.Tools.Test in '..\MCPServer.Tools.Test.pas';

{$R *.RES}

begin
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
