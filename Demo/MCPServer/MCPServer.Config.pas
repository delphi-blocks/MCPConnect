unit MCPServer.Config;

interface

uses
  System.SysUtils, System.Classes,
  IdGlobal, IdContext, IdBaseComponent, IdComponent,
  IdCustomTCPServer, IdCustomHTTPServer, IdHTTPServer,

  MCPConnect.JRPC.Core,
  MCPConnect.JRPC.Classes,
  MCPConnect.JRPC.Server,

  MCPConnect.MCP.Server.Api,
  MCPConnect.MCP.Types,

  MCPConnect.Configuration.MCP,
  MCPConnect.Configuration.Session,
  MCPConnect.Configuration.Auth,

  MCPConnect.Content.Writers.RTL,
  {$IFDEF FRAMEWORK_VCL}
  MCPConnect.Content.Writers.VCL,
  {$ENDIF}

  MCPConnect.Session.Core,
  MCPServer.Notifications;

type
  TServerConfigurator = class
    class procedure ConfigureServer(AServer: TJRPCServer);
  end;

implementation

uses
  System.IOUtils,
  System.TypInfo,
  Logify,
  MCPServer.Resources,
  MCPServer.Apps,
  MCPServer.Tools,
  MCPServer.Tools.Test,
  MCPServer.Prompts;


{ TServerConfigurator }

class procedure TServerConfigurator.ConfigureServer(AServer: TJRPCServer);
var
  LDataPath: string;
begin
  LDataPath := TPath.Combine(ExtractFilePath(ParamStr(0)), 'data');

  AServer

//    .Plugin.Configure<IAuthTokenConfig>
//      .SetToken('my-secret-token')
//    .ApplyConfig

    .Plugin.Configure<ISessionConfig>
      .SetLocation(TSessionIdLocation.Header)
      .SetHeaderName('Mcp-Session-Id')
      .SetTimeout(30)  // 30 minutes timeout
      .SetSessionClass(TShoppingSession)  // Use custom typed session
    .ApplyConfig

    .Plugin.Configure<IMCPConfig>
      .Server
        .SetName('delphi-mcp-server')
        .SetVersion('2.0.0')

        // If not set, the server checks the registered tools, resources, etc. and automatically fills the capabilities.
        //.SetCapabilities([Tools, Resources])
        //.SetCapabilities(LCapabilities)
        //.SetCapabilities(
        //  procedure (ACapabilities: TServerCapabilities)
        //  begin
        //    ACapabilities.Tools.ListChanged := True;
        //  end
        //)

        .SetIconFolder(TPath.Combine(LDataPath, 'icons'))

        {$IFDEF FRAMEWORK_VCL}
        .RegisterWriter(TMCPImageWriter)
        .RegisterWriter(TMCPPictureWriter)
        {$ENDIF}
        .RegisterWriter(TMCPStreamWriter)
        .RegisterWriter(TMCPStringListWriter)

      .BackToMCP

      .MessageHandling

        // Uncomment to register a class that overrides the standard MCP API
        // (useful especially for notifications)
        // .RegisterApi(TNotificationHandler)

        // Fires when the client cancels an in-flight request (notifications/cancelled)
        .OnCancelled(
          procedure (AContext: TJRPCContext; AParams: TCancelledNotificationParams)
          begin
            Logger.LogDebug('Cancelled');
          end
        )

        // Fires once the client completes the initialize handshake (notifications/initialized)
        .OnInitialized(
          procedure (AContext: TJRPCContext)
          begin
            Logger.LogDebug('Notification: Initialized');
          end
        )

        // Fires when the client adjusts the minimum log severity (logging/setLevel)
        .OnSetLogLevel(
          procedure (AContext: TJRPCContext; ALevel: TLogSetLevel)
          begin
            Logger.LogDebug('Log level set to %s',
              [GetEnumName(TypeInfo(TLogSetLevel), Ord(ALevel))]);
          end
        )

      .BackToMCP

      .Security
        .SetCORS(True)
        .SetAllowedMethods(['POST'])
        {$IFNDEF DEBUG}
        .SetAllowedOrigins(['http://localhost'])
        {$ENDIF}
      .BackToMCP

      .Resources
        .SetBasePath(LDataPath)

        .RegisterClass(TWeatherResource)
        .RegisterClass(TDelphiDayAppUI)
        .RegisterFile('index.md', 'Indice Documentazione')
        .RegisterFile('documentation\mcp\mcpconnect.pdf', 'MCPConnect Introduction')
      .BackToMCP

      .Prompts
        .RegisterClass(TSamplePrompts)
      .BackToMCP

      .Tools

        .RegisterTool(TRegisterToolTest, 'RandomNumber', 'random', 'Generates random numbers within a specified range', 'icon=money.png')
          .WithParam('AMax', 'range', 'Range parameter for Random')
          .EndTool

        .RegisterClass(TTestTool)
        .RegisterClass(TDelphiDayTool)
        .RegisterClass(TShoppingCartTool)  // Session-based shopping cart

      .BackToMCP
  ;
end;

end.
