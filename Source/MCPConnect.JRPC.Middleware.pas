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
unit MCPConnect.JRPC.Middleware;

{
  Middleware core: cross-cutting behaviour applied to every JSON-RPC message
  without touching the classes that implement the business logic.

  A middleware wraps the rest of the chain ("onion" model): whatever runs before
  the call to AChain.Next is pre-processing, whatever runs after it is
  post-processing, and not calling Next at all suppresses the operation. There
  is no separate declaration of "pre" or "post": the position in the code is the
  declaration.

  Each hook is its own interface, so a middleware declares what it takes part in
  simply by implementing it:

    TAuditMiddleware = class(TMiddleware, IRequestMiddleware)

  The pipeline asks Supports() which chains a class belongs to, and a hook whose
  signature does not match is a compile error rather than a method that silently
  never runs. New hooks are added by declaring new interfaces, which is how the
  MCP operation hooks (OnCallTool, OnReadResource, ...) extend this from
  MCPConnect.MCP.Middleware without this unit knowing about them.

  The continuation is deliberately NOT an anonymous method. It is a record
  cursor passed by value plus a method pointer for the terminal handler, so a
  message costs no heap allocation, captures nothing, and produces a readable
  call stack. Passing the cursor by value also makes Next re-entrant: calling it
  twice (a retry) walks the same tail of the chain both times.

  This unit holds the transport- and protocol-agnostic part: the message level
  hooks usable by a plain JSON-RPC server.

  See Docs/middleware.md for the full specification.
}

interface

{$I MCPConnect.inc}
{$SCOPEDENUMS ON}

uses
  System.SysUtils, System.TypInfo, System.SyncObjs, System.Generics.Collections,
  System.Generics.Defaults,

  JRPC.Core,
  JRPC.Classes,

  MCPConnect.Configuration.Core;

const
  /// <summary>
  ///   Runs first, so it wraps everything else and can observe any exception
  ///   raised downstream.
  /// </summary>
  MW_PRIORITY_ERROR_HANDLING = 1000;

  /// <summary>Identifies the caller.</summary>
  MW_PRIORITY_AUTHENTICATION = 2000;

  /// <summary>Decides what the identified caller may do.</summary>
  MW_PRIORITY_AUTHORIZATION = 3000;

  /// <summary>Logging, timing, metrics: observes an already validated call.</summary>
  MW_PRIORITY_OBSERVABILITY = 4000;

  /// <summary>
  ///   Priority of a middleware that claims none, which makes the chain fall
  ///   back to plain registration order.
  /// </summary>
  MW_PRIORITY_USER = 5000;

type
  /// <summary>
  ///   Kind of the message the chain is processing.
  /// </summary>
  TMiddlewareMessageKind = (Request, Notification, Response, Error);

  /// <summary>
  ///   Context handed to every middleware hook. It lives for the duration of a
  ///   single message: it is not thread-safe and must not be kept past the call.
  /// </summary>
  TMiddlewareContext = class(TObject)
  private
    FMethod: string;
    FKind: TMiddlewareMessageKind;
    FTimestamp: TDateTime;
    FMessage: TJRPCMessage;
    FRPCContext: TJRPCContext;
    FGarbage: IGarbageCollector;
    FResponses: TMCPMessageQueue;
    FProduced: TObjectList<TJRPCMessage>;
    FPreviousOnEnqueue: TQueueEvent<TJRPCMessage>;
    FObserving: Boolean;

    /// <summary>
    ///   Records a copy of everything the message produces. Hooked to the
    ///   queue's OnEnqueue, which fires while the queue is still locked, so a
    ///   message is recorded before the thread writing to the client can take
    ///   it away.
    /// </summary>
    procedure QueueEnqueued(ASender: TObject; AMessage: TJRPCMessage);
  public
    constructor Create(const AMethod: string; const AKind: TMiddlewareMessageKind;
      AMessage: TJRPCMessage; ARPCContext: TJRPCContext;
      const AGarbage: IGarbageCollector; AResponses: TMCPMessageQueue;
      const AObserveResponses: Boolean);
    destructor Destroy; override;

    /// <summary>
    ///   Hands an object to the garbage collector of the current request: it
    ///   then lives as long as the request, not as long as the middleware.
    ///   Use it for anything published through RPCContext.
    /// </summary>
    procedure Own(AObject: TObject);

    /// <summary>
    ///   Shorthand for RPCContext.FindContextDataAs: returns nil when the
    ///   request context holds no object of the requested class.
    /// </summary>
    function Find<T: class>: T;

    /// <summary>The JSON-RPC method name, e.g. "tools/call".</summary>
    property Method: string read FMethod;

    property Kind: TMiddlewareMessageKind read FKind;

    /// <summary>When the message entered the chain.</summary>
    property Timestamp: TDateTime read FTimestamp;

    /// <summary>
    ///   The raw message. Owned by the pipeline: read it, change it if the
    ///   middleware means to, but never free it.
    /// </summary>
    property Message: TJRPCMessage read FMessage;

    /// <summary>
    ///   Request context: resolves the configurations, the objects injected
    ///   with [Context], and anything published by the other middleware. It is
    ///   the per-request state of the chain, so there is no separate state bag.
    /// </summary>
    property RPCContext: TJRPCContext read FRPCContext;

    /// <summary>
    ///   Adds a message to what this one answers with. This is how a middleware
    ///   that suppresses an operation still says something back.
    /// </summary>
    /// <remarks>
    ///   Takes ownership of AMessage: it is freed once written to the client.
    /// </remarks>
    procedure Emit(AMessage: TJRPCMessage);

    /// <summary>
    ///   Copies of what this message has produced so far, in the order it was
    ///   produced. Meant to be read after AChain.Next has returned, which is
    ///   when it holds the whole answer.
    /// </summary>
    /// <remarks>
    ///   A copy, and not the queue itself, because the queue is a pipe and not
    ///   a buffer: the thread writing to the client consumes it while the
    ///   handler is still running, so by the time a hook unwinds the messages
    ///   are gone, and reading the queue would take them away from the client
    ///   anyway. The copies belong to this context and die with it, so a
    ///   middleware must not free them nor keep them past the call.
    ///
    ///   Only recorded when there is a middleware to read them.
    /// </remarks>
    function Produced: TArray<TJRPCMessage>;
  end;

  IMessageMiddleware = interface;
  IRequestMiddleware = interface;
  INotificationMiddleware = interface;

  /// <summary>
  ///   The real handler a chain ends on. A method pointer, not an anonymous
  ///   method: two pointers, no allocation, no capture.
  /// </summary>
  TMessageTerminal = procedure (AContext: TMiddlewareContext) of object;
  TRequestTerminal = procedure (AContext: TMiddlewareContext) of object;
  TNotificationTerminal = procedure (AContext: TMiddlewareContext) of object;

  /// <summary>
  ///   Cursor over the OnMessage chain. A record passed by value: calling Next
  ///   does not move the caller's cursor, so a middleware may call it more than
  ///   once and walk the same tail of the chain every time.
  /// </summary>
  TMessageChain = record
  private
    FChain: TArray<IMessageMiddleware>;
    FIndex: Integer;
    FTerminal: TMessageTerminal;
  public
    class function Create(const AChain: TArray<IMessageMiddleware>;
      const ATerminal: TMessageTerminal): TMessageChain; static;

    /// <summary>
    ///   Runs the rest of the chain, or the real handler when no middleware is
    ///   left. Not calling it suppresses the operation.
    /// </summary>
    procedure Next(AContext: TMiddlewareContext);
  end;

  /// <summary>Cursor over the OnRequest chain. See TMessageChain.</summary>
  TRequestChain = record
  private
    FChain: TArray<IRequestMiddleware>;
    FIndex: Integer;
    FTerminal: TRequestTerminal;
  public
    class function Create(const AChain: TArray<IRequestMiddleware>;
      const ATerminal: TRequestTerminal): TRequestChain; static;

    procedure Next(AContext: TMiddlewareContext);
  end;

  /// <summary>Cursor over the OnNotification chain. See TMessageChain.</summary>
  TNotificationChain = record
  private
    FChain: TArray<INotificationMiddleware>;
    FIndex: Integer;
    FTerminal: TNotificationTerminal;
  public
    class function Create(const AChain: TArray<INotificationMiddleware>;
      const ATerminal: TNotificationTerminal): TNotificationChain; static;

    procedure Next(AContext: TMiddlewareContext);
  end;

  /// <summary>
  ///   Common denominator of every middleware. Carries no hook: a middleware
  ///   takes part in a chain by implementing the interface of that chain.
  /// </summary>
  /// <remarks>
  ///   One instance is created per request, so the fields of a middleware are
  ///   private to the message being processed and need no lock. State that must
  ///   outlive the request belongs in a shared object registered on the server
  ///   and injected with [Context].
  /// </remarks>
  IMiddleware = interface
  ['{6D6EE2DB-5A30-46B3-86C0-1BDA223E6DFF}']
    function GetName: string;

    /// <summary>Name used in logs and diagnostics.</summary>
    property Name: string read GetName;
  end;

  /// <summary>
  ///   Takes part in the message chain: every message, so requests,
  ///   notifications, responses coming back from the client, and malformed
  ///   messages too.
  /// </summary>
  IMessageMiddleware = interface(IMiddleware)
  ['{16ECF889-CAA9-46B7-B7DA-C8F9A35F253B}']
    procedure OnMessage(AContext: TMiddlewareContext;
      const AChain: TMessageChain);
  end;

  /// <summary>
  ///   Takes part in the request chain: messages carrying an id, which expect a
  ///   response.
  /// </summary>
  IRequestMiddleware = interface(IMiddleware)
  ['{5FF09347-AB19-46BD-A746-5C66DB034D34}']
    procedure OnRequest(AContext: TMiddlewareContext;
      const AChain: TRequestChain);
  end;

  /// <summary>
  ///   Takes part in the notification chain: fire-and-forget messages, so
  ///   nothing to return.
  /// </summary>
  INotificationMiddleware = interface(IMiddleware)
  ['{1A474F62-959B-495F-9A53-76D907522F70}']
    procedure OnNotification(AContext: TMiddlewareContext;
      const AChain: TNotificationChain);
  end;

  TMiddlewareClass = class of TMiddleware;

  /// <summary>
  ///   Base class for middleware. It brings reference counting, a default name
  ///   and the declared priority; the hooks come from the interfaces the
  ///   descendant chooses to implement, as many as it needs:
  ///
  ///     TAuditMiddleware = class(TMiddleware, IRequestMiddleware,
  ///       ICallToolMiddleware)
  /// </summary>
  TMiddleware = class(TInterfacedObject, IMiddleware)
  protected
    function GetName: string; virtual;
  public
    /// <summary>
    ///   Virtual so that the pipeline can build an instance from a class
    ///   reference and still run the constructor of the actual class. A
    ///   middleware needing arguments is registered with a factory instead.
    /// </summary>
    constructor Create; virtual;

    /// <summary>
    ///   Priority used when a middleware is registered without naming one.
    ///   Lower values run first, so they sit further out: first to see the
    ///   request, last to see the response. Middleware sharing a priority keep
    ///   their registration order.
    /// </summary>
    /// <remarks>
    ///   The base value is MW_PRIORITY_USER, which makes the chain follow plain
    ///   registration order. A middleware whose position is part of what it is,
    ///   rather than a choice of whoever registers it, overrides this: an error
    ///   handler has to wrap everything to be of any use, so it answers
    ///   MW_PRIORITY_ERROR_HANDLING and works right without the caller having
    ///   to know. Registering with an explicit priority still wins.
    /// </remarks>
    class function DefaultPriority: Integer; virtual;

    property Name: string read GetName;
  end;

  /// <summary>
  ///   One registration: the class, where it sits in the chain, and how to
  ///   build it.
  /// </summary>
  TMiddlewareEntry = record
    MiddlewareClass: TMiddlewareClass;
    Priority: Integer;
    /// <summary>
    ///   Registration order. Only used to break ties between equal priorities,
    ///   which is what makes the ordering stable.
    /// </summary>
    Sequence: Integer;
    /// <summary>Optional: builds the instance when the class needs arguments.</summary>
    Factory: TFunc<IMiddleware>;

    function CreateInstance: IMiddleware;
  end;

  TMiddlewareEntries = TArray<TMiddlewareEntry>;

  /// <summary>
  ///   The middleware registered on a server, in the order they run.
  /// </summary>
  /// <remarks>
  ///   Middleware may be added and removed while requests are in flight, so
  ///   the list is shared state. A lock on the writes would not be enough: a
  ///   chain must not change underneath a message that already started. Reads
  ///   therefore go through an immutable snapshot, rebuilt from scratch on every
  ///   change and swapped in one assignment. A change takes effect from the next
  ///   message on, and messages already running finish with the chain they
  ///   started with.
  ///
  ///   It is the same reasoning already applied to
  ///   IJRPCApplication.GetConfigurations, which hands out an array rather than
  ///   the live collection for exactly this reason.
  /// </remarks>
  TMiddlewareList = class(TObject)
  private
    FApplication: IJRPCApplication;
    FLock: TCriticalSection;
    FEntries: TList<TMiddlewareEntry>;
    FShared: TObjectList<TObject>;
    /// <summary>
    ///   Shared objects withdrawn with RemoveShared. Still owned, so that they
    ///   die with the server as they would have anyway, but no longer handed out.
    /// </summary>
    FRetired: TObjectList<TObject>;
    FSequence: Integer;
    FSnapshot: TMiddlewareEntries;
    FChainCache: TDictionary<TGUID, TMiddlewareEntries>;

    function GetCount: Integer;
    /// <summary>
    ///   The current snapshot, taken under the lock. Grabbing the reference is
    ///   not atomic on its own (a pointer read plus a refcount increment), so
    ///   every read goes through here rather than touching FSnapshot: the
    ///   critical section is two instructions long, and it is what guarantees a
    ///   reader sees either the whole old chain or the whole new one.
    /// </summary>
    function Snapshot: TMiddlewareEntries;
    /// <summary>Rebuilds the snapshot and drops the per-hook cache. Call under the lock.</summary>
    procedure Invalidate;
    function AddEntry(AClass: TMiddlewareClass; const APriority: Integer;
      const AFactory: TFunc<IMiddleware>): TMiddlewareList;
  public
    constructor Create(const AApplication: IJRPCApplication);
    destructor Destroy; override;

    /// <summary>
    ///   Adds a middleware class with the priority the class declares in
    ///   DefaultPriority. A new instance is built per request.
    /// </summary>
    function Add(AClass: TMiddlewareClass;
      const AFactory: TFunc<IMiddleware> = nil): TMiddlewareList; overload;

    /// <summary>
    ///   Same, imposing the priority from the outside: this is how a third
    ///   party middleware is moved without touching its source. Ties are broken
    ///   by registration order.
    /// </summary>
    function Add(AClass: TMiddlewareClass; const APriority: Integer;
      const AFactory: TFunc<IMiddleware> = nil): TMiddlewareList; overload;

    /// <summary>
    ///   Registers a shared object, alive as long as the server and injectable
    ///   into middleware (and tools) with [Context]. This is the one place
    ///   where middleware state is shared between requests, so making it
    ///   thread-safe is the object's own business.
    /// </summary>
    /// <remarks>
    ///   Takes ownership outright: the object is freed when the server goes
    ///   down. Never pass something that is already owned elsewhere - a
    ///   component on a form, say - or it is freed twice.
    /// </remarks>
    function AddShared(AObject: TObject): TMiddlewareList;

    /// <summary>
    ///   Stops handing out a shared object: from the next request on it is no
    ///   longer put in the request context, so nothing can be injected with it
    ///   any more.
    /// </summary>
    /// <remarks>
    ///   It is not freed here. A request already running may hold it in its own
    ///   context, and freeing it underneath would leave that request with a
    ///   dangling pointer; the list keeps it and frees it with the server, which
    ///   is what it would have done anyway.
    ///
    ///   Clear does not touch the shared objects: it is about the chain, which
    ///   is a different axis. This is the way to withdraw one.
    /// </remarks>
    function RemoveShared(AObject: TObject): TMiddlewareList;

    function Remove(AClass: TMiddlewareClass): TMiddlewareList;

    /// <summary>
    ///   Empties the chain. Leaves the shared objects alone: see RemoveShared.
    /// </summary>
    function Clear: TMiddlewareList;

    function Contains(AClass: TMiddlewareClass): Boolean;
    function ToArray: TArray<TMiddlewareClass>;

    /// <summary>
    ///   The registrations that implement AIID, already ordered. This is what a
    ///   pipeline asks for to build a chain: pass the GUID of the hook
    ///   interface, e.g. IRequestMiddleware. Worked out once per hook and kept
    ///   until the list changes.
    /// </summary>
    function EntriesFor(const AIID: TGUID): TMiddlewareEntries;

    /// <summary>
    ///   The shared objects, to be put in the context of every request.
    /// </summary>
    function SharedObjects: TArray<TObject>;

    /// <summary>
    ///   Back to the application, to carry on with the fluent configuration.
    ///   Same meaning as IJRPCConfiguration.BackToApp.
    /// </summary>
    function BackToApp: IJRPCApplication;

    property Count: Integer read GetCount;
  end;

  /// <summary>
  ///   The middleware of a single message: builds one instance per registered
  ///   class, keeps it for as long as the message lasts, and hands out the
  ///   chains.
  /// </summary>
  /// <remarks>
  ///   Instances are per message, so a middleware may keep what it needs in its
  ///   own fields with no lock. They are also reused across the levels of the
  ///   same message, which is what lets the same middleware carry something
  ///   from OnRequest down to OnCallTool in a plain field instead of a bag of
  ///   loose values.
  ///
  ///   Instantiation is lazy: asking for a chain only builds the middleware of
  ///   that chain, so one that hooks tools/call alone costs nothing on a
  ///   tools/list.
  /// </remarks>
  TMiddlewarePipeline = class(TObject)
  private
    FList: TMiddlewareList;
    FRPCContext: TJRPCContext;
    FInstances: TDictionary<TMiddlewareClass, IMiddleware>;
    FMessageContext: TMiddlewareContext;

    function InstanceOf(const AEntry: TMiddlewareEntry): IMiddleware;
  public
    constructor Create(AList: TMiddlewareList; ARPCContext: TJRPCContext);
    destructor Destroy; override;

    /// <summary>
    ///   Opens a message: from here on the chains are built on AContext, and
    ///   the middleware instances are fresh.
    /// </summary>
    /// <remarks>
    ///   The pipeline itself lives as long as the request, so that it can sit
    ///   in the request context and be found by the api classes; what is per
    ///   message is what these two calls bracket.
    /// </remarks>
    procedure BeginMessage(AContext: TMiddlewareContext);

    /// <summary>Closes the message, dropping its middleware instances.</summary>
    procedure EndMessage;

    /// <summary>
    ///   The context of the message being handled, or nil outside one. It is
    ///   what an api method passes to the hooks of its own operation.
    /// </summary>
    property Context: TMiddlewareContext read FMessageContext;

    /// <summary>
    ///   The middleware taking part in the chain of T, in order, already built
    ///   and with their [Context] fields injected.
    /// </summary>
    /// <remarks>
    ///   T is a hook interface, e.g. ChainFor&lt;IRequestMiddleware&gt;: its GUID
    ///   is what the registrations are matched against, and it is read from the
    ///   type itself so that the caller does not have to name it twice.
    /// </remarks>
    function ChainFor<T: IInterface>: TArray<T>;

    /// <summary>
    ///   True when no middleware is registered at all, so the caller can skip
    ///   building a context and go straight to the handler.
    /// </summary>
    function IsEmpty: Boolean;
  end;

implementation

{ TMiddlewareContext }

constructor TMiddlewareContext.Create(const AMethod: string;
  const AKind: TMiddlewareMessageKind; AMessage: TJRPCMessage;
  ARPCContext: TJRPCContext; const AGarbage: IGarbageCollector;
  AResponses: TMCPMessageQueue; const AObserveResponses: Boolean);
begin
  inherited Create;
  FMethod := AMethod;
  FKind := AKind;
  FMessage := AMessage;
  FRPCContext := ARPCContext;
  FGarbage := AGarbage;
  FResponses := AResponses;
  FTimestamp := Now;

  // Nothing is recorded when no middleware is going to read it.
  if not AObserveResponses or not Assigned(FResponses) then
    Exit;

  FProduced := TObjectList<TJRPCMessage>.Create(True);
  FPreviousOnEnqueue := FResponses.OnEnqueue;
  FResponses.OnEnqueue := QueueEnqueued;
  FObserving := True;
end;

destructor TMiddlewareContext.Destroy;
begin
  if FObserving then
    FResponses.OnEnqueue := FPreviousOnEnqueue;

  FProduced.Free;
  inherited;
end;

procedure TMiddlewareContext.QueueEnqueued(ASender: TObject; AMessage: TJRPCMessage);
var
  LCopy: TJRPCMessage;
begin
  // Whoever was listening before still gets told: this observer borrows the
  // event for the span of one message, it does not take it over.
  if Assigned(FPreviousOnEnqueue) then
    FPreviousOnEnqueue(ASender, AMessage);

  // Clone is declared on the concrete messages, not on TJRPCMessage, and the
  // four of them are siblings: no ambiguity in the order of the tests.
  LCopy := nil;
  if AMessage is TJRPCNotification then
    LCopy := TJRPCNotification(AMessage).Clone
  else if AMessage is TJRPCRequest then
    LCopy := TJRPCRequest(AMessage).Clone
  else if AMessage is TJRPCResponse then
    LCopy := TJRPCResponse(AMessage).Clone
  else if AMessage is TJRPCError then
    LCopy := TJRPCError(AMessage).Clone;

  if Assigned(LCopy) then
    FProduced.Add(LCopy);
end;

procedure TMiddlewareContext.Emit(AMessage: TJRPCMessage);
begin
  if Assigned(FResponses) then
    FResponses.Enqueue(AMessage);
end;

function TMiddlewareContext.Produced: TArray<TJRPCMessage>;
begin
  if Assigned(FProduced) then
    Exit(FProduced.ToArray);

  Result := nil;
end;

function TMiddlewareContext.Find<T>: T;
begin
  Result := RPCContext.FindContextDataAs<T>;
end;

procedure TMiddlewareContext.Own(AObject: TObject);
begin
  if Assigned(FGarbage) then
    FGarbage.Add(AObject);
end;

{ TMessageChain }

class function TMessageChain.Create(const AChain: TArray<IMessageMiddleware>;
  const ATerminal: TMessageTerminal): TMessageChain;
begin
  Result.FChain := AChain;
  Result.FIndex := 0;
  Result.FTerminal := ATerminal;
end;

procedure TMessageChain.Next(AContext: TMiddlewareContext);
var
  LNext: TMessageChain;
begin
  if FIndex >= Length(FChain) then
  begin
    FTerminal(AContext);
    Exit;
  end;

  // Copying the cursor rather than advancing this one is what makes Next
  // repeatable: the caller's chain stays where it was.
  LNext := Self;
  Inc(LNext.FIndex);
  FChain[FIndex].OnMessage(AContext, LNext);
end;

{ TRequestChain }

class function TRequestChain.Create(const AChain: TArray<IRequestMiddleware>;
  const ATerminal: TRequestTerminal): TRequestChain;
begin
  Result.FChain := AChain;
  Result.FIndex := 0;
  Result.FTerminal := ATerminal;
end;

procedure TRequestChain.Next(AContext: TMiddlewareContext);
var
  LNext: TRequestChain;
begin
  if FIndex >= Length(FChain) then
  begin
    FTerminal(AContext);
    Exit;
  end;

  LNext := Self;
  Inc(LNext.FIndex);
  FChain[FIndex].OnRequest(AContext, LNext);
end;

{ TNotificationChain }

class function TNotificationChain.Create(const AChain: TArray<INotificationMiddleware>;
  const ATerminal: TNotificationTerminal): TNotificationChain;
begin
  Result.FChain := AChain;
  Result.FIndex := 0;
  Result.FTerminal := ATerminal;
end;

procedure TNotificationChain.Next(AContext: TMiddlewareContext);
var
  LNext: TNotificationChain;
begin
  if FIndex >= Length(FChain) then
  begin
    FTerminal(AContext);
    Exit;
  end;

  LNext := Self;
  Inc(LNext.FIndex);
  FChain[FIndex].OnNotification(AContext, LNext);
end;

{ TMiddleware }

constructor TMiddleware.Create;
begin
  inherited Create;
end;

class function TMiddleware.DefaultPriority: Integer;
begin
  Result := MW_PRIORITY_USER;
end;

function TMiddleware.GetName: string;
begin
  Result := ClassName;
end;

{ TMiddlewareEntry }

function TMiddlewareEntry.CreateInstance: IMiddleware;
begin
  if Assigned(Factory) then
    Exit(Factory());

  Result := MiddlewareClass.Create;
end;

{ TMiddlewareList }

constructor TMiddlewareList.Create(const AApplication: IJRPCApplication);
begin
  inherited Create;
  FApplication := AApplication;
  FLock := TCriticalSection.Create();
  FEntries := TList<TMiddlewareEntry>.Create();
  FShared := TObjectList<TObject>.Create(True);
  FRetired := TObjectList<TObject>.Create(True);
  FChainCache := TDictionary<TGUID, TMiddlewareEntries>.Create();
end;

destructor TMiddlewareList.Destroy;
begin
  FChainCache.Free;
  FRetired.Free;
  FShared.Free;
  FEntries.Free;
  FLock.Free;
  inherited;
end;

function TMiddlewareList.AddEntry(AClass: TMiddlewareClass;
  const APriority: Integer; const AFactory: TFunc<IMiddleware>): TMiddlewareList;
var
  LEntry: TMiddlewareEntry;
begin
  Result := Self;

  if not Assigned(AClass) then
    Exit;

  FLock.Enter();
  try
    LEntry.MiddlewareClass := AClass;
    LEntry.Priority := APriority;
    LEntry.Sequence := FSequence;
    LEntry.Factory := AFactory;
    Inc(FSequence);

    FEntries.Add(LEntry);
    Invalidate;
  finally
    FLock.Leave();
  end;
end;

function TMiddlewareList.Add(AClass: TMiddlewareClass;
  const AFactory: TFunc<IMiddleware>): TMiddlewareList;
begin
  Result := AddEntry(AClass, AClass.DefaultPriority, AFactory);
end;

function TMiddlewareList.Add(AClass: TMiddlewareClass; const APriority: Integer;
  const AFactory: TFunc<IMiddleware>): TMiddlewareList;
begin
  Result := AddEntry(AClass, APriority, AFactory);
end;

function TMiddlewareList.AddShared(AObject: TObject): TMiddlewareList;
begin
  Result := Self;

  if not Assigned(AObject) then
    Exit;

  FLock.Enter();
  try
    FShared.Add(AObject);
  finally
    FLock.Leave();
  end;
end;

function TMiddlewareList.RemoveShared(AObject: TObject): TMiddlewareList;
begin
  Result := Self;

  if not Assigned(AObject) then
    Exit;

  FLock.Enter();
  try
    // Extract and not Remove: the list owns its objects, and Remove would free
    // this one while a request already running may still be holding it.
    if Assigned(FShared.Extract(AObject)) then
      FRetired.Add(AObject);
  finally
    FLock.Leave();
  end;
end;

function TMiddlewareList.BackToApp: IJRPCApplication;
begin
  Result := FApplication;
end;

function TMiddlewareList.Clear: TMiddlewareList;
begin
  Result := Self;

  FLock.Enter();
  try
    FEntries.Clear;
    Invalidate;
  finally
    FLock.Leave();
  end;
end;

function TMiddlewareList.Contains(AClass: TMiddlewareClass): Boolean;
var
  LEntry: TMiddlewareEntry;
begin
  for LEntry in Snapshot do
    if LEntry.MiddlewareClass = AClass then
      Exit(True);

  Result := False;
end;

function TMiddlewareList.EntriesFor(const AIID: TGUID): TMiddlewareEntries;
var
  LEntry: TMiddlewareEntry;
  LResult: TMiddlewareEntries;
  LSnapshot: TMiddlewareEntries;
  LCount: Integer;
begin
  FLock.Enter();
  try
    if FChainCache.TryGetValue(AIID, Result) then
      Exit;
  finally
    FLock.Leave();
  end;

  // The snapshot is already ordered, so filtering keeps the order. Supports on
  // a class reference answers before any instance exists, which is what lets
  // the chains be worked out at registration time.
  LSnapshot := Snapshot;
  LCount := 0;
  SetLength(LResult, Length(LSnapshot));
  for LEntry in LSnapshot do
    if Supports(LEntry.MiddlewareClass, AIID) then
    begin
      LResult[LCount] := LEntry;
      Inc(LCount);
    end;
  SetLength(LResult, LCount);

  FLock.Enter();
  try
    FChainCache.AddOrSetValue(AIID, LResult);
  finally
    FLock.Leave();
  end;

  Result := LResult;
end;

function TMiddlewareList.GetCount: Integer;
begin
  Result := Length(Snapshot);
end;

function TMiddlewareList.Snapshot: TMiddlewareEntries;
begin
  FLock.Enter();
  try
    Result := FSnapshot;
  finally
    FLock.Leave();
  end;
end;

procedure TMiddlewareList.Invalidate;
var
  LSnapshot: TMiddlewareEntries;
begin
  LSnapshot := FEntries.ToArray;

  // Sequence is unique and increasing, so ordering by (Priority, Sequence)
  // is stable by construction: equal priorities keep their registration order.
  TArray.Sort<TMiddlewareEntry>(LSnapshot, TComparer<TMiddlewareEntry>.Construct(
    function (const ALeft, ARight: TMiddlewareEntry): Integer
    begin
      Result := ALeft.Priority - ARight.Priority;
      if Result = 0 then
        Result := ALeft.Sequence - ARight.Sequence;
    end
  ));

  // One assignment: a reader either sees the whole old chain or the whole new one.
  FSnapshot := LSnapshot;
  FChainCache.Clear;
end;

function TMiddlewareList.Remove(AClass: TMiddlewareClass): TMiddlewareList;
var
  LIndex: Integer;
begin
  Result := Self;

  FLock.Enter();
  try
    for LIndex := FEntries.Count - 1 downto 0 do
      if FEntries[LIndex].MiddlewareClass = AClass then
        FEntries.Delete(LIndex);

    Invalidate;
  finally
    FLock.Leave();
  end;
end;

function TMiddlewareList.SharedObjects: TArray<TObject>;
begin
  FLock.Enter();
  try
    Result := FShared.ToArray;
  finally
    FLock.Leave();
  end;
end;

{ TMiddlewarePipeline }

constructor TMiddlewarePipeline.Create(AList: TMiddlewareList;
  ARPCContext: TJRPCContext);
begin
  inherited Create;
  FList := AList;
  FRPCContext := ARPCContext;
  FInstances := TDictionary<TMiddlewareClass, IMiddleware>.Create();
end;

destructor TMiddlewarePipeline.Destroy;
begin
  FInstances.Free;
  inherited;
end;

procedure TMiddlewarePipeline.BeginMessage(AContext: TMiddlewareContext);
begin
  FMessageContext := AContext;
  FInstances.Clear;
end;

procedure TMiddlewarePipeline.EndMessage;
begin
  FMessageContext := nil;
  FInstances.Clear;
end;

function TMiddlewarePipeline.InstanceOf(const AEntry: TMiddlewareEntry): IMiddleware;
begin
  if FInstances.TryGetValue(AEntry.MiddlewareClass, Result) then
    Exit;

  Result := AEntry.CreateInstance;
  if Assigned(FRPCContext) then
    FRPCContext.Inject(Result);

  FInstances.AddOrSetValue(AEntry.MiddlewareClass, Result);
end;

function TMiddlewarePipeline.ChainFor<T>: TArray<T>;
var
  LEntries: TMiddlewareEntries;
  LIndex: Integer;
  LIID: TGUID;
begin
  LIID := GetTypeData(TypeInfo(T)).GUID;

  LEntries := FList.EntriesFor(LIID);
  SetLength(Result, Length(LEntries));
  for LIndex := 0 to High(LEntries) do
    Supports(InstanceOf(LEntries[LIndex]), LIID, Result[LIndex]);
end;

function TMiddlewarePipeline.IsEmpty: Boolean;
begin
  Result := FList.Count = 0;
end;

function TMiddlewareList.ToArray: TArray<TMiddlewareClass>;
var
  LIndex: Integer;
  LSnapshot: TMiddlewareEntries;
begin
  LSnapshot := Snapshot;
  SetLength(Result, Length(LSnapshot));
  for LIndex := 0 to High(LSnapshot) do
    Result[LIndex] := LSnapshot[LIndex].MiddlewareClass;
end;

end.
