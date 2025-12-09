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
  ESessionException = class(EJRPCException)
  public
    constructor Create(const AMessage: string; ACode: Integer);
  end;

  /// <summary>
  ///   Exception raised when a session ID is not found
  /// </summary>
  ESessionNotFoundError = class(ESessionException)
  public
    constructor Create(const ASessionId: string);
  end;

  /// <summary>
  ///   Exception raised when a session has expired
  /// </summary>
  ESessionExpiredError = class(ESessionException)
  public
    constructor Create(const ASessionId: string);
  end;

  /// <summary>
  ///   Exception raised when a session ID format is invalid
  /// </summary>
  ESessionInvalidError = class(ESessionException)
  public
    constructor Create(const ASessionId: string);
  end;

  // Forward declarations
  TSessionBase = class;
  TSessionDataClass = class of TSessionBase;

  /// <summary>
  ///   Abstract base class for session data.
  ///   Contains only core session properties (ID, timestamps).
  ///   Extend this class to create custom session data with typed properties.
  /// </summary>
  TSessionBase = class abstract
  private
    FSessionId: string;
    FCreatedAt: TDateTime;
    FLastAccessedAt: TDateTime;
  public
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
  end;

  /// <summary>
  ///   Concrete session data implementation with JSON storage.
  ///   Use this if you need dynamic storage without creating a custom class.
  ///   For typed properties, create your own class inheriting from TSessionBase.
  /// </summary>
  TSessionData = class(TSessionBase)
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
  TSessionManager = class
  private
    class var FInstance: TSessionManager;
  private
    FLock: TCriticalSection;
    FSessions: TDictionary<string, TSessionBase>;
    FSessionClass: TSessionDataClass;
    FTimeoutMinutes: Integer;

    function GenerateSessionId: string;
    function IsExpired(ASession: TSessionBase): Boolean;
    procedure RemoveExpiredSession(const ASessionId: string);
  protected
    class function GetInstance: TSessionManager; static;
  public
    /// <summary>
    ///   Create a new session with a generated ID
    /// </summary>
    function CreateSession: TSessionBase;

    /// <summary>
    ///   Get an existing session by ID. Raises exception if not found or expired.
    /// </summary>
    function GetSession(const ASessionId: string): TSessionBase;

    /// <summary>
    ///   Try to get a session by ID. Returns False if not found or expired.
    /// </summary>
    function TryGetSession(const ASessionId: string; out ASession: TSessionBase): Boolean;

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
    property SessionClass: TSessionDataClass read FSessionClass write FSessionClass;

    /// <summary>
    ///   Session timeout in minutes (default: 30)
    /// </summary>
    property TimeoutMinutes: Integer read FTimeoutMinutes write FTimeoutMinutes;

    /// <summary>
    ///   Singleton instance of the session manager
    /// </summary>
    class property Instance: TSessionManager read GetInstance;

    class constructor Create;
    class destructor Destroy;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  System.DateUtils,
  Neon.Core.Utils;

{ ESessionException }

constructor ESessionException.Create(const AMessage: string; ACode: Integer);
begin
  inherited Create(AMessage);
  FCode := ACode;  // Inherited from EJRPCException
end;

{ ESessionNotFoundError }

constructor ESessionNotFoundError.Create(const ASessionId: string);
begin
  inherited Create(Format('Session "%s" not found', [ASessionId]), JRPC_SESSION_NOT_FOUND);
end;

{ ESessionExpiredError }

constructor ESessionExpiredError.Create(const ASessionId: string);
begin
  inherited Create(Format('Session "%s" has expired', [ASessionId]), JRPC_SESSION_EXPIRED);
end;

{ ESessionInvalidError }

constructor ESessionInvalidError.Create(const ASessionId: string);
begin
  inherited Create(Format('Session ID "%s" is invalid', [ASessionId]), JRPC_SESSION_INVALID);
end;

{ TSessionData }

constructor TSessionData.Create;
begin
  inherited Create;
  FData := TJSONObject.Create;
end;

destructor TSessionData.Destroy;
begin
  FData.Free;
  inherited;
end;

{ TSessionManager }

class constructor TSessionManager.Create;
begin
  FInstance := nil;
end;

constructor TSessionManager.Create;
begin
  inherited;
  FLock := TCriticalSection.Create;
  FSessions := TDictionary<string, TSessionBase>.Create;
  FSessionClass := TSessionData;
  FTimeoutMinutes := 30;
end;

class destructor TSessionManager.Destroy;
begin
  FInstance.Free;
end;

destructor TSessionManager.Destroy;
var
  LSession: TSessionBase;
begin
  for LSession in FSessions.Values do
    LSession.Free;
  FSessions.Free;
  FLock.Free;
  inherited;
end;

function TSessionManager.CreateSession: TSessionBase;
var
  LSessionId: string;
begin
  LSessionId := GenerateSessionId;

  FLock.Enter;
  try
    Result := TRttiUtils.CreateInstance(FSessionClass) as TSessionBase;
    // Initialize session properties (same unit, can access private fields)
    Result.FSessionId := LSessionId;
    Result.FCreatedAt := Now;
    Result.FLastAccessedAt := Now;
    FSessions.Add(LSessionId, Result);
  finally
    FLock.Leave;
  end;
end;

procedure TSessionManager.DestroySession(const ASessionId: string);
var
  LSession: TSessionBase;
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

function TSessionManager.GenerateSessionId: string;
var
  LGuid: TGUID;
begin
  CreateGUID(LGuid);
  Result := GUIDToString(LGuid);
  // Remove curly braces { and }
  Result := Copy(Result, 2, Length(Result) - 2);
end;

class function TSessionManager.GetInstance: TSessionManager;
begin
  if not Assigned(FInstance) then
    FInstance := TSessionManager.Create;
  Result := FInstance;
end;

function TSessionManager.GetSession(const ASessionId: string): TSessionBase;
begin
  if not TryGetSession(ASessionId, Result) then
    raise ESessionNotFoundError.Create(ASessionId);
end;

function TSessionManager.IsExpired(ASession: TSessionBase): Boolean;
var
  LElapsedMinutes: Int64;
begin
  LElapsedMinutes := MinutesBetween(Now, ASession.LastAccessedAt);
  Result := LElapsedMinutes > FTimeoutMinutes;
end;

procedure TSessionManager.RemoveExpiredSession(const ASessionId: string);
var
  LSession: TSessionBase;
begin
  if FSessions.TryGetValue(ASessionId, LSession) then
  begin
    FSessions.Remove(ASessionId);
    LSession.Free;
  end;
end;

function TSessionManager.SessionExists(const ASessionId: string): Boolean;
begin
  FLock.Enter;
  try
    Result := FSessions.ContainsKey(ASessionId);
  finally
    FLock.Leave;
  end;
end;

function TSessionManager.TryGetSession(const ASessionId: string;
  out ASession: TSessionBase): Boolean;
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
        raise ESessionExpiredError.Create(ASessionId);
      end;

      // Update last accessed time
      ASession.LastAccessedAt := Now;
    end;
  finally
    FLock.Leave;
  end;
end;

end.
