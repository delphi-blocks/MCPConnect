{******************************************************************************}
{                                                                              }
{  Delphi MCP Connect Library                                                  }
{                                                                              }
{  Copyright (c) Paolo Rossi <dev@paolorossi.net>                              }
{                Luca Minuti <code@lucaminuti.it>                              }
{  All rights reserved.                                                        }
{                                                                              }
{  https://github.com/delphi-blocks/MCPConnect                                 }
{                                                                              }
{  Licensed under the MIT license                                              }
{                                                                              }
{******************************************************************************}
unit MCPConnect.MCP.Types.Subscriptions;

interface

{$SCOPEDENUMS ON}

uses
  System.SysUtils, System.JSON,

  JRPC.Core,
  Neon.Core.Types,
  Neon.Core.Attributes,
  Neon.Core.Nullables,
  Neon.Core.Persistence,
  Neon.Core.Persistence.JSON,

  MCPConnect.MCP.Types.Base,
  MCPConnect.MCP.Types.Notifications;

const
  /// <summary>
  ///   The long-lived request that opens a notification stream. It replaces the
  ///   HTTP GET endpoint, "resources/subscribe" and "resources/unsubscribe".
  /// </summary>
  MCP_SUBSCRIPTIONS_LISTEN = 'subscriptions/listen';

  /// <summary>
  ///   The notification acknowledging which of the requested notification types
  ///   the server will actually send.
  /// </summary>
  MCP_NOTIFY_SUBSCRIPTIONS_ACKNOWLEDGED = 'notifications/subscriptions/acknowledged';

type
  /// <summary>
  ///   The notification types a client opts in to on a "subscriptions/listen"
  ///   request.
  /// </summary>
  /// <remarks>
  ///   Every type is opt-in: the server MUST NOT send a notification type the
  ///   client did not ask for here.
  /// </remarks>
  TSubscriptionFilter = class
  public
    /// <summary>Receive "notifications/tools/list_changed".</summary>
    ToolsListChanged: NullBoolean;

    /// <summary>Receive "notifications/prompts/list_changed".</summary>
    PromptsListChanged: NullBoolean;

    /// <summary>Receive "notifications/resources/list_changed".</summary>
    ResourcesListChanged: NullBoolean;

    /// <summary>
    ///   Receive "notifications/resources/updated" for these resource uris.
    ///   Replaces the former "resources/subscribe" request.
    /// </summary>
    [NeonInclude(IncludeIf.NotEmpty)] ResourceSubscriptions: TArray<string>;
  public
    /// <summary>True when the flag is present and set.</summary>
    function WantsToolsListChanged: Boolean;
    function WantsPromptsListChanged: Boolean;
    function WantsResourcesListChanged: Boolean;

    /// <summary>True when AUri was subscribed to, matched case-sensitively.</summary>
    function WantsResource(const AUri: string): Boolean;

    /// <summary>True when nothing at all was requested.</summary>
    function IsEmpty: Boolean;

    /// <summary>Copies the requested types onto ATarget.</summary>
    procedure AssignTo(ATarget: TSubscriptionFilter);
  end;

  /// <summary>
  ///   Parameters of a "subscriptions/listen" request.
  /// </summary>
  TSubscriptionsListenRequestParams = class(TRequestMetaParams)
  public
    /// <summary>
    ///   REQUIRED. The notifications the client opts in to on this stream.
    /// </summary>
    Notifications: TSubscriptionFilter;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  /// <summary>
  ///   Extends ResultMetaObject with the identifier of the subscription stream
  ///   the result closes.
  /// </summary>
  TSubscriptionsListenResultMeta = class(TResultMetaObject)
  public
    /// <summary>
    ///   REQUIRED. The JSON-RPC id of the "subscriptions/listen" request that
    ///   opened the stream, which equals this response's own id.
    /// </summary>
    /// <remarks>
    ///   A RequestId is a string or an integer, so it is carried as raw JSON;
    ///   set it with SetSubscriptionId.
    /// </remarks>
    [NeonProperty(MCP_META_SUBSCRIPTION_ID)]
    SubscriptionId: TJSONValue;
  public
    destructor Destroy; override;

    procedure SetSubscriptionId(const AId: string); overload;
    procedure SetSubscriptionId(AId: Int64); overload;
  end;

  /// <summary>
  ///   The response to a "subscriptions/listen" request, signalling that the
  ///   subscription ended gracefully - during server shutdown, say.
  /// </summary>
  /// <remarks>
  ///   Because the stream is long-lived, this is sent only when the server
  ///   tears the subscription down; an abrupt transport close carries no
  ///   response at all. The body is otherwise empty.
  /// </remarks>
  TSubscriptionsListenResult = class(TBaseResult)
  public
    constructor Create;

    /// <summary>
    ///   The stream identifier, in the result "_meta", where it is required.
    /// </summary>
    function Meta: TSubscriptionsListenResultMeta;
  end;

  /// <summary>
  ///   Parameters of a "notifications/subscriptions/acknowledged" notification.
  /// </summary>
  TSubscriptionsAcknowledgedNotificationParams = class(TMCPNotificationParams)
  public
    /// <summary>
    ///   REQUIRED. The subset of the requested notification types the server
    ///   agreed to honour. A type the server cannot serve - prompts changing on
    ///   a server with no prompts, say - is left out.
    /// </summary>
    Notifications: TSubscriptionFilter;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

{ TSubscriptionFilter }

function TSubscriptionFilter.WantsToolsListChanged: Boolean;
begin
  Result := ToolsListChanged.HasValue and ToolsListChanged.Value;
end;

function TSubscriptionFilter.WantsPromptsListChanged: Boolean;
begin
  Result := PromptsListChanged.HasValue and PromptsListChanged.Value;
end;

function TSubscriptionFilter.WantsResourcesListChanged: Boolean;
begin
  Result := ResourcesListChanged.HasValue and ResourcesListChanged.Value;
end;

function TSubscriptionFilter.WantsResource(const AUri: string): Boolean;
var
  LUri: string;
begin
  // Resource uris are compared as the identifiers they are: case-sensitively
  for LUri in ResourceSubscriptions do
    if LUri = AUri then
      Exit(True);

  Result := False;
end;

function TSubscriptionFilter.IsEmpty: Boolean;
begin
  Result := not WantsToolsListChanged and
            not WantsPromptsListChanged and
            not WantsResourcesListChanged and
            (Length(ResourceSubscriptions) = 0);
end;

procedure TSubscriptionFilter.AssignTo(ATarget: TSubscriptionFilter);
begin
  ATarget.ToolsListChanged := ToolsListChanged;
  ATarget.PromptsListChanged := PromptsListChanged;
  ATarget.ResourcesListChanged := ResourcesListChanged;
  ATarget.ResourceSubscriptions := Copy(ResourceSubscriptions);
end;

{ TSubscriptionsListenRequestParams }

constructor TSubscriptionsListenRequestParams.Create;
begin
  inherited;
  Notifications := TSubscriptionFilter.Create;
end;

destructor TSubscriptionsListenRequestParams.Destroy;
begin
  Notifications.Free;
  inherited;
end;

{ TSubscriptionsListenResultMeta }

destructor TSubscriptionsListenResultMeta.Destroy;
begin
  SubscriptionId.Free;
  inherited;
end;

procedure TSubscriptionsListenResultMeta.SetSubscriptionId(const AId: string);
begin
  SubscriptionId.Free;
  SubscriptionId := TJSONString.Create(AId);
end;

procedure TSubscriptionsListenResultMeta.SetSubscriptionId(AId: Int64);
begin
  SubscriptionId.Free;
  SubscriptionId := TJSONNumber.Create(AId);
end;

{ TSubscriptionsListenResult }

constructor TSubscriptionsListenResult.Create;
begin
  inherited;

  // The result "_meta" has to carry the stream id, which the plain
  // TResultMetaObject built by TBaseResult has no room for
  ResultMeta.Free;
  ResultMeta := TSubscriptionsListenResultMeta.Create;
end;

function TSubscriptionsListenResult.Meta: TSubscriptionsListenResultMeta;
begin
  Result := ResultMeta as TSubscriptionsListenResultMeta;
end;

{ TSubscriptionsAcknowledgedNotificationParams }

constructor TSubscriptionsAcknowledgedNotificationParams.Create;
begin
  inherited;
  Notifications := TSubscriptionFilter.Create;
end;

destructor TSubscriptionsAcknowledgedNotificationParams.Destroy;
begin
  Notifications.Free;
  inherited;
end;

end.
