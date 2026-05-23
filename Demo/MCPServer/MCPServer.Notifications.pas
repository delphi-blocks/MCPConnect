unit MCPServer.Notifications;

interface

uses
  System.Classes, System.SysUtils,

  MCPConnect.JRPC.Core,
  MCPConnect.JRPC.Classes,
  MCPConnect.MCP.Types,

  MCPServer.Tools;

type
  // Demo override: registered via MessageHandling.RegisterApi, takes over the
  // whole "notifications" namespace. While this class is registered, the
  // typed OnInitialized / OnCancelled callbacks in MCPServer.Config are NOT
  // invoked for those methods (the built-in TMCPNotificationsApi is bypassed).
  [JRPC('notifications')]
  TNotificationHandler = class
  private
    [Context]
    Session: TShoppingSession;
  public
    [JRPC('initialized'), JRPCNotification]
    procedure Initialized;

    [JRPC('cancelled'), JRPCNotification]
    procedure Cancelled([JRPCParams] AParams: TCancelledNotificationParams);
  end;

implementation

uses
  Logify;

{ TNotificationHandler }

procedure TNotificationHandler.Initialized;
begin
  Logger.LogDebug('[TNotificationHandler] Initialized (override active) ' + Session.SessionId);
end;

procedure TNotificationHandler.Cancelled(AParams: TCancelledNotificationParams);
begin
  Logger.LogDebug('[TNotificationHandler] Cancelled (override active)');
end;

end.
