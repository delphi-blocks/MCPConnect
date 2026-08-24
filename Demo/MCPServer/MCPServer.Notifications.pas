unit MCPServer.Notifications;

{
  ==============================================================================
   MCPConnect demo - overriding a whole JSON-RPC namespace
  ==============================================================================

  MCPConnect ships built-in API classes for the standard MCP namespaces
  (initialize, tools, resources, prompts, notifications, logging - see
  MCPConnect.MCP.Server.Api). Most servers never touch them and simply hook the
  typed callbacks offered by .MessageHandling (OnInitialized, OnCancelled,
  OnSetLogLevel).

  When that is not enough - you need extra methods in a namespace, or full
  control over how they are dispatched - register your own class instead:

      .MessageHandling.RegisterApi(TNotificationHandler)

  This is the JRPC layer showing through: MCPConnect.JRPC.* is a complete,
  standalone JSON-RPC 2.0 implementation, and MCP is "just" a set of APIs
  registered on top of it. The same [JRPC] / [JRPCMethod] / [JRPCParam]
  attributes are what Tests\JRPCServer uses without any MCP at all.

  Note that RegisterApi is per-server and takes precedence over the global
  TJRPCRegistry, so two servers in the same process can expose different
  implementations of the same namespace.
}

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
  //
  // [JRPC('notifications')] on the class + [JRPC('initialized')] on the method
  // compose the wire name: "notifications/initialized".
  [JRPC('notifications')]
  TNotificationHandler = class
  private
    // [Context] injection works here exactly as it does in a tool class: this
    // API object is created per request and receives the same services.
    [Context]
    Session: TShoppingSession;
  public
    /// <summary>
    ///   [JRPCNotification] marks the method as a notification handler: no
    ///   response is produced, and returning a value would be meaningless.
    ///   Omit it and the method becomes a request handler that must answer.
    /// </summary>
    [JRPC('initialized'), JRPCNotification]
    procedure Initialized;

    /// <summary>
    ///   [JRPCParams] binds the *whole* params object to a single typed
    ///   argument, instead of matching JSON members to parameters one by one.
    /// </summary>
    [JRPC('cancelled'), JRPCNotification]
    procedure Cancelled([JRPCParams] AParams: TCancelledNotificationParams);
  end;

implementation

uses
  Logify;

{ TNotificationHandler }

procedure TNotificationHandler.Initialized;
begin
  // Reaching the session from here proves the injection works outside tools
  // too - handy to warm up per-client state as soon as the handshake ends.
  Logger.LogDebug('[TNotificationHandler] Initialized (override active) ' + Session.SessionId);
end;

procedure TNotificationHandler.Cancelled(AParams: TCancelledNotificationParams);
begin
  // AParams.RequestId identifies the in-flight call the client gave up on.
  // A real implementation would flag it so a long-running tool can stop early.
  Logger.LogDebug('[TNotificationHandler] Cancelled (override active)');
end;

end.
