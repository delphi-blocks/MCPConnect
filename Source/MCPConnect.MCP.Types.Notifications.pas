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
unit MCPConnect.MCP.Types.Notifications;

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

  MCPConnect.MCP.Types.Base;

const
  /// <summary>
  ///   The notification methods a server sends to a client.
  /// </summary>
  MCP_NOTIFY_CANCELLED = 'notifications/cancelled';
  MCP_NOTIFY_PROGRESS = 'notifications/progress';
  MCP_NOTIFY_MESSAGE = 'notifications/message';
  MCP_NOTIFY_TOOLS_LIST_CHANGED = 'notifications/tools/list_changed';
  MCP_NOTIFY_PROMPTS_LIST_CHANGED = 'notifications/prompts/list_changed';
  MCP_NOTIFY_RESOURCES_LIST_CHANGED = 'notifications/resources/list_changed';
  MCP_NOTIFY_RESOURCES_UPDATED = 'notifications/resources/updated';

  /// <summary>
  ///   The _meta key correlating a notification with the
  ///   "subscriptions/listen" stream it was delivered on.
  /// </summary>
  MCP_META_SUBSCRIPTION_ID = 'io.modelcontextprotocol/subscriptionId';

type
  /// <summary>
  ///   Extends MetaObject with the notification-specific fields. All key naming
  ///   rules from MetaObject apply.
  /// </summary>
  TNotificationMetaObject = class(TFlatMetaClass)
  public
    /// <summary>
    ///   Identifies the subscription stream this notification was delivered on.
    ///   The server MUST set it on every notification sent over a
    ///   "subscriptions/listen" stream, so the client can correlate the two;
    ///   it is absent on notifications sent outside one, such as progress for
    ///   an in-flight request.
    /// </summary>
    /// <remarks>
    ///   A RequestId is a string or an integer, so it is carried as raw JSON.
    ///   Set it through SetSubscriptionId rather than by hand; the meta object
    ///   owns whatever it holds.
    /// </remarks>
    [NeonProperty(MCP_META_SUBSCRIPTION_ID), NeonInclude(IncludeIf.NotEmpty)]
    SubscriptionId: TJSONValue;
  public
    destructor Destroy; override;

    procedure SetSubscriptionId(const AId: string); overload;
    procedure SetSubscriptionId(AId: Int64); overload;

    function HasSubscriptionId: Boolean;
  end;

  /// <summary>
  ///   Common params of any notification: the optional "_meta".
  /// </summary>
  /// <remarks>
  ///   Also the complete params of the three list-changed notifications, which
  ///   carry nothing else.
  /// </remarks>
  TMCPNotificationParams = class
  public
    [NeonProperty('_meta'), NeonInclude(IncludeIf.NotEmpty)]
    NotificationMeta: TNotificationMetaObject;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    /// <summary>
    ///   Marks these params as belonging to a subscription stream. Chainable.
    /// </summary>
    function OnSubscription(const AId: string): TMCPNotificationParams; overload;
    function OnSubscription(AId: Int64): TMCPNotificationParams; overload;

    /// <summary>Renders the params. The caller owns the result.</summary>
    function ToJSON: TJSONObject;
  end;

  TMCPNotificationParamsClass = class of TMCPNotificationParams;

  /// <summary>
  ///   Params of a "notifications/tools|prompts|resources/list_changed"
  ///   notification: nothing beyond the common "_meta".
  /// </summary>
  TListChangedNotificationParams = class(TMCPNotificationParams);

  /// <summary>
  ///   Params of a "notifications/cancelled" notification.
  /// </summary>
  TCancelledNotificationParams = class(TMCPNotificationParams)
  public
    /// <summary>
    ///   REQUIRED. The id of the request being cancelled. A string or an
    ///   integer, so carried as raw JSON; set it with SetRequestId.
    /// </summary>
    [NeonInclude(IncludeIf.NotEmpty)] RequestId: TJSONValue;

    /// <summary>An optional human-readable reason for the cancellation.</summary>
    Reason: NullString;
  public
    destructor Destroy; override;

    procedure SetRequestId(const AId: string); overload;
    procedure SetRequestId(AId: Int64); overload;
  end;

  /// <summary>
  ///   Params of a "notifications/progress" notification.
  /// </summary>
  TProgressNotificationParams = class(TMCPNotificationParams)
  public
    /// <summary>
    ///   REQUIRED. The token the client attached to the originating request, in
    ///   its "_meta.progressToken". A string or an integer, so carried as raw
    ///   JSON; set it with SetProgressToken.
    /// </summary>
    [NeonInclude(IncludeIf.NotEmpty)] ProgressToken: TJSONValue;

    /// <summary>
    ///   REQUIRED. Progress so far. It MUST increase with every notification,
    ///   even when Total is unknown.
    /// </summary>
    Progress: Double;

    /// <summary>Total units of work, if known.</summary>
    Total: NullDouble;

    /// <summary>An optional message describing the current step.</summary>
    Message: NullString;
  public
    destructor Destroy; override;

    procedure SetProgressToken(const AToken: string); overload;
    procedure SetProgressToken(AToken: Int64); overload;

    /// <summary>
    ///   Answers on the token carried by a request, copying it so the request
    ///   params stay the owner of theirs.
    /// </summary>
    procedure SetProgressToken(AToken: TJSONValue); overload;
  end;

  /// <summary>
  ///   Params of a "notifications/message" log notification.
  /// </summary>
  TLoggingMessageNotificationParams = class(TMCPNotificationParams)
  public
    /// <summary>REQUIRED. The severity of this message.</summary>
    Level: TMCPLogLevel;

    /// <summary>An optional name for the logger issuing the message.</summary>
    Logger: NullString;

    /// <summary>
    ///   REQUIRED. The data to log: any JSON value, a string or an object being
    ///   the usual choices. Owned by the params.
    /// </summary>
    Data: TJSONValue;
  public
    constructor Create; override;
    destructor Destroy; override;

    /// <summary>Sets Data to a plain string message.</summary>
    procedure SetData(const AMessage: string); overload;

    /// <summary>Sets Data to AValue, taking ownership of it.</summary>
    procedure SetData(AValue: TJSONValue); overload;
  end;

  /// <summary>
  ///   Params of a "notifications/resources/updated" notification.
  /// </summary>
  TResourceUpdatedNotificationParams = class(TMCPNotificationParams)
  public
    /// <summary>
    ///   REQUIRED. The uri of the resource that changed. It may be a
    ///   sub-resource of the one the client actually subscribed to.
    /// </summary>
    Uri: string;
  end;

  /// <summary>
  ///   Builds the server-to-client notifications as ready-to-enqueue
  ///   TJRPCNotification messages.
  /// </summary>
  /// <example>
  ///   <code>
  ///   FQueue.Enqueue(TMCPNotification.ToolListChanged);
  ///   FQueue.Enqueue(TMCPNotification.Progress(LToken, 3, 10, 'Indexing'));
  ///   </code>
  /// </example>
  TMCPNotification = class
  public
    /// <summary>
    ///   Wraps AParams as a notification for AMethod, taking ownership of them.
    ///   The entry point to use when the params need a subscription id, or any
    ///   other _meta key, set before they are rendered.
    /// </summary>
    class function FromParams(const AMethod: string; AParams: TMCPNotificationParams): TJRPCNotification; static;

    class function Cancelled(const ARequestId: string; const AReason: string = ''): TJRPCNotification; overload; static;
    class function Cancelled(ARequestId: Int64; const AReason: string = ''): TJRPCNotification; overload; static;

    class function Progress(const AToken: string; AProgress: Double): TJRPCNotification; overload; static;
    class function Progress(const AToken: string; AProgress, ATotal: Double; const AMessage: string = ''): TJRPCNotification; overload; static;
    class function Progress(AToken: Int64; AProgress: Double): TJRPCNotification; overload; static;
    class function Progress(AToken: Int64; AProgress, ATotal: Double; const AMessage: string = ''): TJRPCNotification; overload; static;

    /// <summary>
    ///   Answers on the token a request carried, taken straight from its
    ///   RequestMeta.ProgressToken. The token is copied, not adopted.
    /// </summary>
    class function Progress(AToken: TJSONValue; AProgress: Double): TJRPCNotification; overload; static;
    class function Progress(AToken: TJSONValue; AProgress, ATotal: Double; const AMessage: string = ''): TJRPCNotification; overload; static;

    class function LogMessage(ALevel: TMCPLogLevel; const AMessage: string; const ALogger: string = ''): TJRPCNotification; overload; static;
    class function LogMessage(ALevel: TMCPLogLevel; AData: TJSONValue; const ALogger: string = ''): TJRPCNotification; overload; static;

    class function ToolListChanged: TJRPCNotification; static;
    class function PromptListChanged: TJRPCNotification; static;
    class function ResourceListChanged: TJRPCNotification; static;

    class function ResourceUpdated(const AUri: string): TJRPCNotification; static;
  end;

implementation

{ TNotificationMetaObject }

destructor TNotificationMetaObject.Destroy;
begin
  SubscriptionId.Free;
  inherited;
end;

procedure TNotificationMetaObject.SetSubscriptionId(const AId: string);
begin
  SubscriptionId.Free;
  SubscriptionId := TJSONString.Create(AId);
end;

procedure TNotificationMetaObject.SetSubscriptionId(AId: Int64);
begin
  SubscriptionId.Free;
  SubscriptionId := TJSONNumber.Create(AId);
end;

function TNotificationMetaObject.HasSubscriptionId: Boolean;
begin
  Result := Assigned(SubscriptionId);
end;

{ TMCPNotificationParams }

constructor TMCPNotificationParams.Create;
begin
  NotificationMeta := TNotificationMetaObject.Create;
end;

destructor TMCPNotificationParams.Destroy;
begin
  NotificationMeta.Free;
  inherited;
end;

function TMCPNotificationParams.OnSubscription(const AId: string): TMCPNotificationParams;
begin
  NotificationMeta.SetSubscriptionId(AId);
  Result := Self;
end;

function TMCPNotificationParams.OnSubscription(AId: Int64): TMCPNotificationParams;
begin
  NotificationMeta.SetSubscriptionId(AId);
  Result := Self;
end;

function TMCPNotificationParams.ToJSON: TJSONObject;
begin
  Result := TNeon.ObjectToJSON(Self, MCPNeonConfig) as TJSONObject;
end;

{ TCancelledNotificationParams }

destructor TCancelledNotificationParams.Destroy;
begin
  RequestId.Free;
  inherited;
end;

procedure TCancelledNotificationParams.SetRequestId(const AId: string);
begin
  RequestId.Free;
  RequestId := TJSONString.Create(AId);
end;

procedure TCancelledNotificationParams.SetRequestId(AId: Int64);
begin
  RequestId.Free;
  RequestId := TJSONNumber.Create(AId);
end;

{ TProgressNotificationParams }

destructor TProgressNotificationParams.Destroy;
begin
  ProgressToken.Free;
  inherited;
end;

procedure TProgressNotificationParams.SetProgressToken(const AToken: string);
begin
  ProgressToken.Free;
  ProgressToken := TJSONString.Create(AToken);
end;

procedure TProgressNotificationParams.SetProgressToken(AToken: Int64);
begin
  ProgressToken.Free;
  ProgressToken := TJSONNumber.Create(AToken);
end;

procedure TProgressNotificationParams.SetProgressToken(AToken: TJSONValue);
begin
  if ProgressToken = AToken then
    Exit;

  ProgressToken.Free;
  if Assigned(AToken) then
    ProgressToken := AToken.Clone as TJSONValue
  else
    ProgressToken := nil;
end;

{ TLoggingMessageNotificationParams }

constructor TLoggingMessageNotificationParams.Create;
begin
  inherited;
  // "data" is required, so it starts as an explicit null rather than absent
  Data := TJSONNull.Create;
end;

destructor TLoggingMessageNotificationParams.Destroy;
begin
  Data.Free;
  inherited;
end;

procedure TLoggingMessageNotificationParams.SetData(const AMessage: string);
begin
  SetData(TJSONString.Create(AMessage));
end;

procedure TLoggingMessageNotificationParams.SetData(AValue: TJSONValue);
begin
  if Data = AValue then
    Exit;

  Data.Free;
  Data := AValue;
end;

{ TMCPNotification }

class function TMCPNotification.FromParams(const AMethod: string;
  AParams: TMCPNotificationParams): TJRPCNotification;
begin
  Assert(Assigned(AParams), 'TMCPNotification.FromParams: AParams cannot be nil');

  Result := TJRPCNotification.Create;
  try
    Result.Method := AMethod;
    // TJRPCMethod.Params takes ownership of the rendered object
    Result.Params := AParams.ToJSON;
  except
    Result.Free;
    raise;
  end;
  AParams.Free;
end;

class function TMCPNotification.Cancelled(const ARequestId, AReason: string): TJRPCNotification;
var
  LParams: TCancelledNotificationParams;
begin
  LParams := TCancelledNotificationParams.Create;
  LParams.SetRequestId(ARequestId);
  if not AReason.IsEmpty then
    LParams.Reason := AReason;

  Result := FromParams(MCP_NOTIFY_CANCELLED, LParams);
end;

class function TMCPNotification.Cancelled(ARequestId: Int64; const AReason: string): TJRPCNotification;
var
  LParams: TCancelledNotificationParams;
begin
  LParams := TCancelledNotificationParams.Create;
  LParams.SetRequestId(ARequestId);
  if not AReason.IsEmpty then
    LParams.Reason := AReason;

  Result := FromParams(MCP_NOTIFY_CANCELLED, LParams);
end;

class function TMCPNotification.Progress(const AToken: string; AProgress: Double): TJRPCNotification;
var
  LParams: TProgressNotificationParams;
begin
  LParams := TProgressNotificationParams.Create;
  LParams.SetProgressToken(AToken);
  LParams.Progress := AProgress;

  Result := FromParams(MCP_NOTIFY_PROGRESS, LParams);
end;

class function TMCPNotification.Progress(const AToken: string; AProgress, ATotal: Double;
  const AMessage: string): TJRPCNotification;
var
  LParams: TProgressNotificationParams;
begin
  LParams := TProgressNotificationParams.Create;
  LParams.SetProgressToken(AToken);
  LParams.Progress := AProgress;
  LParams.Total := ATotal;
  if not AMessage.IsEmpty then
    LParams.Message := AMessage;

  Result := FromParams(MCP_NOTIFY_PROGRESS, LParams);
end;

class function TMCPNotification.Progress(AToken: Int64; AProgress: Double): TJRPCNotification;
var
  LParams: TProgressNotificationParams;
begin
  LParams := TProgressNotificationParams.Create;
  LParams.SetProgressToken(AToken);
  LParams.Progress := AProgress;

  Result := FromParams(MCP_NOTIFY_PROGRESS, LParams);
end;

class function TMCPNotification.Progress(AToken: Int64; AProgress, ATotal: Double;
  const AMessage: string): TJRPCNotification;
var
  LParams: TProgressNotificationParams;
begin
  LParams := TProgressNotificationParams.Create;
  LParams.SetProgressToken(AToken);
  LParams.Progress := AProgress;
  LParams.Total := ATotal;
  if not AMessage.IsEmpty then
    LParams.Message := AMessage;

  Result := FromParams(MCP_NOTIFY_PROGRESS, LParams);
end;

class function TMCPNotification.Progress(AToken: TJSONValue; AProgress: Double): TJRPCNotification;
var
  LParams: TProgressNotificationParams;
begin
  LParams := TProgressNotificationParams.Create;
  LParams.SetProgressToken(AToken);
  LParams.Progress := AProgress;

  Result := FromParams(MCP_NOTIFY_PROGRESS, LParams);
end;

class function TMCPNotification.Progress(AToken: TJSONValue; AProgress, ATotal: Double; const AMessage: string): TJRPCNotification;
var
  LParams: TProgressNotificationParams;
begin
  LParams := TProgressNotificationParams.Create;
  LParams.SetProgressToken(AToken);
  LParams.Progress := AProgress;
  LParams.Total := ATotal;
  if not AMessage.IsEmpty then
    LParams.Message := AMessage;

  Result := FromParams(MCP_NOTIFY_PROGRESS, LParams);
end;

class function TMCPNotification.LogMessage(ALevel: TMCPLogLevel; const AMessage,
  ALogger: string): TJRPCNotification;
begin
  Result := LogMessage(ALevel, TJSONString.Create(AMessage), ALogger);
end;

class function TMCPNotification.LogMessage(ALevel: TMCPLogLevel; AData: TJSONValue;
  const ALogger: string): TJRPCNotification;
var
  LParams: TLoggingMessageNotificationParams;
begin
  LParams := TLoggingMessageNotificationParams.Create;
  LParams.Level := ALevel;
  LParams.SetData(AData);
  if not ALogger.IsEmpty then
    LParams.Logger := ALogger;

  Result := FromParams(MCP_NOTIFY_MESSAGE, LParams);
end;

class function TMCPNotification.ToolListChanged: TJRPCNotification;
begin
  Result := FromParams(MCP_NOTIFY_TOOLS_LIST_CHANGED, TListChangedNotificationParams.Create);
end;

class function TMCPNotification.PromptListChanged: TJRPCNotification;
begin
  Result := FromParams(MCP_NOTIFY_PROMPTS_LIST_CHANGED, TListChangedNotificationParams.Create);
end;

class function TMCPNotification.ResourceListChanged: TJRPCNotification;
begin
  Result := FromParams(MCP_NOTIFY_RESOURCES_LIST_CHANGED, TListChangedNotificationParams.Create);
end;

class function TMCPNotification.ResourceUpdated(const AUri: string): TJRPCNotification;
var
  LParams: TResourceUpdatedNotificationParams;
begin
  LParams := TResourceUpdatedNotificationParams.Create;
  LParams.Uri := AUri;

  Result := FromParams(MCP_NOTIFY_RESOURCES_UPDATED, LParams);
end;

end.
