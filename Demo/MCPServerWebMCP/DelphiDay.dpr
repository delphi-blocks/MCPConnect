program DelphiDay;

{
  ==============================================================================
   MCPConnect demo - the Delphi Day 2027 site, with WebMCP
  ==============================================================================

   A small conference site (speakers, sessions, a personal schedule, tickets and
   a cart) rendered with WebStencils, whose pages offer the browser's own agent
   the tools that make sense on each of them through WebMCP. Open it in a
   browser with WebMCP enabled and ask the agent to build you a schedule and
   put the tickets in your cart: it does what you could do with the mouse, by
   calling the same Delphi code - and stops at the Confirm purchase button,
   which only you can press.

       DelphiDay.exe [-port 8080] [-templates <folder>]

   Then open http://localhost:8080. The MCP endpoint is POST /mcp.

   This is the 2025-protocol (main branch) version: the visitor is an MCP
   session carried in a cookie. The 2026-07-28 branch has a stateless one.

   WebStencils needs Delphi 12 or newer: Web.Stencils is part of the RTL there
   and not in Delphi 11.
}

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.IOUtils,
  Web.WebReq,
  IdHTTPWebBrokerBridge,
  Logify,
  Logify.Adapter.Console,
  // Referenced here, not just found on the search path: these units register the
  // MCP API (initialize, tools/call and the rest) in their initialization
  // sections, which the linker drops from a program that never names them.
  MCPConnect.Configuration.Auth,
  MCPConnect.Configuration.Core,
  MCPConnect.Configuration.MCP,
  MCPConnect.Configuration.Neon,
  MCPConnect.Configuration.Session,
  MCPConnect.Content.Writers,
  MCPConnect.Content.Writers.RTL,
  MCPConnect.JRPC.Classes,
  MCPConnect.JRPC.Core,
  MCPConnect.JRPC.Invoker,
  MCPConnect.JRPC.Server,
  MCPConnect.MCP.Attributes,
  MCPConnect.MCP.Invoker,
  MCPConnect.MCP.Prompts,
  MCPConnect.MCP.Resources,
  MCPConnect.MCP.Server.Api,
  MCPConnect.MCP.Tools,
  MCPConnect.MCP.Types,
  MCPConnect.MCP.Config,
  MCPConnect.Session.Core,
  MCPConnect.Transport.WebBroker,
  DelphiDay.Data in 'DelphiDay.Data.pas',
  DelphiDay.Service in 'DelphiDay.Service.pas',
  DelphiDay.View in 'DelphiDay.View.pas',
  DelphiDay.Tools in 'DelphiDay.Tools.pas',
  DelphiDay.WebModule in 'DelphiDay.WebModule.pas' {DelphiDayModule: TWebModule};

function ParamValue(const AName, ADefault: string): string;
var
  LIndex: Integer;
begin
  for LIndex := 1 to ParamCount - 1 do
    if SameText(ParamStr(LIndex), AName) then
      Exit(ParamStr(LIndex + 1));
  Result := ADefault;
end;

var
  LServer: TIdHTTPWebBrokerBridge;
  LLogging: ILoggerAdapterFactory;
  LPort: Integer;
begin
  try
    LLogging := TLogifyAdapterConsoleFactory.CreateAdapterFactory(TLogLevel.Info);
    TLoggerAdapterRegistry.Instance.RegisterFactory(LLogging);

    TemplatesRoot := ParamValue('-templates', '');
    LPort := StrToIntDef(ParamValue('-port', '8080'), 8080);

    // Before the socket opens: the first visitor must not pay for loading the
    // catalog and scanning the tool class.
    ConfigureSite(TPath.Combine(ExtractFilePath(ParamStr(0)), 'data' + PathDelim + 'delphiday.json'));

    if WebRequestHandler <> nil then
      WebRequestHandler.WebModuleClass := WebModuleClass;

    LServer := TIdHTTPWebBrokerBridge.Create(nil);
    try
      LServer.DefaultPort := LPort;
      LServer.Active := True;
      Writeln(Format('Delphi Day: http://localhost:%d  (MCP endpoint POST /mcp, templates %s)',
        [LPort, ResolveTemplatesRoot]));
      Writeln('Press Enter to stop.');
      Readln;
      LServer.Active := False;
    finally
      LServer.Free;
    end;
  except
    on E: Exception do
    begin
      Writeln(E.ClassName, ': ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
