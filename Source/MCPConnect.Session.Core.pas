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
unit MCPConnect.Session.Core;

interface

uses
  System.SysUtils, System.Rtti, System.Classes, System.Generics.Collections,
  System.SyncObjs, System.JSON,

  MCPConnect.JRPC.Core;

const
  /// <summary>
  ///   JSON-RPC error code: Session not found
  /// </summary>
  JRPC_SESSION_NOT_FOUND = -32001;

  /// <summary>
  ///   JSON-RPC error code: Session has expired
  /// </summary>
  JRPC_SESSION_EXPIRED = -32002;

  /// <summary>
  ///   JSON-RPC error code: Invalid session ID format
  /// </summary>
  JRPC_SESSION_INVALID = -32003;

type
  /// <summary>
  ///   Base exception for all session-related errors.
  ///   Inherits from EJRPCException to integrate with JSON-RPC error handling.
  /// </summary>
  EMCPSessionException = class(EJRPCException)
  public
    constructor Create(const AMessage: string; ACode: Integer);
  end;

  /// <summary>
  ///   Exception raised when a session ID is not found
  /// </summary>
  EMCPSessionNotFoundError = class(EMCPSessionException)
  public
    constructor Create(const ASessionId: string);
  end;

  /// <summary>
  ///   Exception raised when a session has expired
  /// </summary>
  EMCPSessionExpiredError = class(EMCPSessionException)
  public
    constructor Create(const ASessionId: string);
  end;

  /// <summary>
  ///   Exception raised when a session ID format is invalid
  /// </summary>
  EMCPSessionInvalidError = class(EMCPSessionException)
  public
    constructor Create(const ASessionId: string);
  end;

  // Forward declarations
  TMCPSessionBase = class;
  TMCPSessionDataClass = class of TMCPSessionBase;

  TMCPSessionQueues = class
    Notifications: TMCPNotificationQueue;
    Responses: TMCPResponseQueue;
    Requests: TMCPRequestQueue;
    Errors: TMCPErrorQueue;

    constructor Create(AMaxItems: Integer);
    destructor Destroy; override;
  end;

  /// <summary>
  ///   Abstract base class for session data.
  ///   Contains only core session properties (ID, timestamps).
  ///   Extend this class to create custom session data with typed properties.
  /// </summary>
  TMCPSessionBase = class abstract
  protected
    FSessionId: string;
    FCreatedAt: TDateTime;
    FLastAccessedAt: TDateTime;
    FInbound: TMCPSessionQueues;
    FOutbound: TMCPMessageQueue;
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    ///   Unique session identifier
    /// </summary>
    property SessionId: string read FSessionId;

    /// <summary>
    ///   Timestamp when the session was created
    /// </summary>
    property CreatedAt: TDateTime read FCreatedAt;

    /// <summary>
    ///   Timestamp of last access (updated on each request)
    /// </summary>
    property LastAccessedAt: TDateTime read FLastAccessedAt write FLastAccessedAt;

    property Inbound: TMCPSessionQueues read FInbound write FInbound;
    property Outbound: TMCPMessageQueue read FOutbound write FOutbound;
  end;

  /// <summary>
  ///   Concrete session data implementation with JSON storage.
  ///   Use this if you need dynamic storage without creating a custom class.
  ///   For typed properties, create your own class inheriting from TMCPSessionBase.
  /// </summary>
  TMCPSessionData = class(TMCPSessionBase)
  private
    FData: TJSONObject;
  public
    /// <summary>
    ///   Direct access to the JSON data object.
    ///   Use this to store/retrieve session data using TJSONObject API.
    ///   Memory management is handled automatically by the session.
    /// </summary>
    property Data: TJSONObject read FData;

    constructor Create;
    destructor Destroy; override;
  end;

  /// <summary>
  ///   Thread-safe session manager singleton.
  ///   Manages session lifecycle, creation, retrieval, and lazy cleanup.
  /// </summary>
  TMCPSessionManager = class
  private
    class var FInstance: TMCPSessionManager;
  private
    FLock: TCriticalSection;
    FSessions: TDictionary<string, TMCPSessionBase>;
    FSessionClass: TMCPSessionDataClass;
    FTimeoutMinutes: Integer;

    function GenerateSessionId: string;
    function IsExpired(ASession: TMCPSessionBase): Boolean;
    procedure RemoveExpiredSession(const ASessionId: string);
  protected
    class function GetInstance: TMCPSessionManager; static;
  public
    class constructor Create;
    class destructor Destroy;
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    ///   Create a new session with a generated ID
    /// </summary>
    function CreateSession: TMCPSessionBase;

    /// <summary>
    ///   Get an existing session by ID. Raises exception if not found or expired.
    /// </summary>
    function GetSession(const ASessionId: string): TMCPSessionBase;

    /// <summary>
    ///   Try to get a session by ID. Returns False if not found or expired.
    /// </summary>
    function TryGetSession(const ASessionId: string; out ASession: TMCPSessionBase): Boolean;

    /// <summary>
    ///   Destroy a session and free its resources
    /// </summary>
    procedure DestroySession(const ASessionId: string);

    /// <summary>
    ///   Check if a session exists (does not update last access time)
    /// </summary>
    function SessionExists(const ASessionId: string): Boolean;

    /// <summary>
    ///   Class used to create new session instances
    /// </summary>
    property SessionClass: TMCPSessionDataClass read FSessionClass write FSessionClass;

    /// <summary>
    ///   Session timeout in minutes (default: 30)
    /// </summary>
    property TimeoutMinutes: Integer read FTimeoutMinutes write FTimeoutMinutes;

    /// <summary>
    ///   Singleton instance of the session manager
    /// </summary>
    class property Instance: TMCPSessionManager read GetInstance;
  end;

implementation

uses
  System.DateUtils,
  Neon.Core.Utils;

{ EMCPSessionException }

constructor EMCPSessionException.Create(const AMessage: string; ACode: Integer);
begin
  inherited Create(AMessage);
  FCode := ACode;  // Inherited from EJRPCException
end;

{ EMCPSessionNotFoundError }

constructor EMCPSessionNotFoundError.Create(const ASessionId: string);
begin
  inherited Create(Format('Session "%s" not found', [ASessionId]), JRPC_SESSION_NOT_FOUND);
end;

{ EMCPSessionExpiredError }

constructor EMCPSessionExpiredError.Create(const ASessionId: string);
begin
  inherited Create(Format('Session "%s" has expired', [ASessionId]), JRPC_SESSION_EXPIRED);
end;

{ EMCPSessionInvalidError }

constructor EMCPSessionInvalidError.Create(const ASessionId: string);
begin
  inherited Create(Format('Session ID "%s" is invalid', [ASessionId]), JRPC_SESSION_INVALID);
end;

{ TMCPSessionData }

constructor TMCPSessionData.Create;
begin
  inherited Create;
  FData := TJSONObject.Create;
end;

destructor TMCPSessionData.Destroy;
begin
  FData.Free;
  inherited;
end;

{ TMCPSessionManager }

class constructor TMCPSessionManager.Create;
begin
  FInstance := nil;
end;

constructor TMCPSessionManager.Create;
begin
  inherited;
  FLock := TCriticalSection.Create;
  FSessions := TDictionary<string, TMCPSessionBase>.Create;
  FSessionClass := TMCPSessionData;
  FTimeoutMinutes := 30;
end;

class destructor TMCPSessionManager.Destroy;
begin
  FInstance.Free;
end;

destructor TMCPSessionManager.Destroy;
var
  LSession: TMCPSessionBase;
begin
  for LSession in FSessions.Values do
    LSession.Free;
  FSessions.Free;
  FLock.Free;
  inherited;
end;

function TMCPSessionManager.CreateSession: TMCPSessionBase;
var
  LSessionId: string;
begin
  LSessionId := GenerateSessionId;

  FLock.Enter;
  try
    Result := TRttiUtils.CreateInstance(FSessionClass) as TMCPSessionBase;
    // Initialize session properties (same unit, can access private fields)
    Result.FSessionId := LSessionId;
    Result.FCreatedAt := Now;
    Result.FLastAccessedAt := Now;
    FSessions.Add(LSessionId, Result);
  finally
    FLock.Leave;
  end;
end;

procedure TMCPSessionManager.DestroySession(const ASessionId: string);
var
  LSession: TMCPSessionBase;
begin
  FLock.Enter;
  try
    if FSessions.TryGetValue(ASessionId, LSession) then
    begin
      FSessions.Remove(ASessionId);
      LSession.Free;
    end;
  finally
    FLock.Leave;
  end;
end;

function TMCPSessionManager.GenerateSessionId: string;
var
  LGuid: TGUID;
begin
  CreateGUID(LGuid);
  Result := GUIDToString(LGuid);
  // Remove curly braces { and }
  Result := Copy(Result, 2, Length(Result) - 2);
end;

class function TMCPSessionManager.GetInstance: TMCPSessionManager;
begin
  if not Assigned(FInstance) then
    FInstance := TMCPSessionManager.Create;
  Result := FInstance;
end;

function TMCPSessionManager.GetSession(const ASessionId: string): TMCPSessionBase;
begin
  if not TryGetSession(ASessionId, Result) then
    raise EMCPSessionNotFoundError.Create(ASessionId);
end;

function TMCPSessionManager.IsExpired(ASession: TMCPSessionBase): Boolean;
var
  LElapsedMinutes: Int64;
begin
  LElapsedMinutes := MinutesBetween(Now, ASession.LastAccessedAt);
  Result := LElapsedMinutes > FTimeoutMinutes;
end;

procedure TMCPSessionManager.RemoveExpiredSession(const ASessionId: string);
var
  LSession: TMCPSessionBase;
begin
  if FSessions.TryGetValue(ASessionId, LSession) then
  begin
    FSessions.Remove(ASessionId);
    LSession.Free;
  end;
end;

function TMCPSessionManager.SessionExists(const ASessionId: string): Boolean;
begin
  FLock.Enter;
  try
    Result := FSessions.ContainsKey(ASessionId);
  finally
    FLock.Leave;
  end;
end;

function TMCPSessionManager.TryGetSession(const ASessionId: string;
  out ASession: TMCPSessionBase): Boolean;
begin
  FLock.Enter;
  try
    Result := FSessions.TryGetValue(ASessionId, ASession);

    if Result then
    begin
      // Lazy cleanup: check if expired
      if IsExpired(ASession) then
      begin
        RemoveExpiredSession(ASessionId);
        raise EMCPSessionExpiredError.Create(ASessionId);
      end;

      // Update last accessed time
      ASession.LastAccessedAt := Now;
    end;
  finally
    FLock.Leave;
  end;
end;

{ TMCPSessionQueues }

constructor TMCPSessionQueues.Create(AMaxItems: Integer);
begin
  Notifications := TMCPNotificationQueue.Create(AMaxItems);
  Responses := TMCPResponseQueue.Create(AMaxItems);
  Requests := TMCPRequestQueue.Create(AMaxItems);
  Errors := TMCPErrorQueue.Create(AMaxItems);
end;

destructor TMCPSessionQueues.Destroy;
begin
  Notifications.Free;
  Responses.Free;
  Requests.Free;
  Errors.Free;

  inherited;
end;

{ TMCPSessionBase }

constructor TMCPSessionBase.Create;
begin
  inherited Create;
  FInbound := TMCPSessionQueues.Create(100);
  FOutbound := TMCPMessageQueue.Create(100);
end;

destructor TMCPSessionBase.Destroy;
begin
  FInbound.Free;
  FOutbound.Free;
  inherited;
end;

end.
