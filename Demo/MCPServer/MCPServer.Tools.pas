unit MCPServer.Tools;

{
  ==============================================================================
   MCPConnect demo - Tools (the functions the model is allowed to call)
  ==============================================================================

  A "tool class" in MCPConnect is a plain Delphi class. It has no base class to
  inherit from and no interface to implement: attributes and RTTI do all the
  work. What makes a method visible to the protocol is the [McpTool] attribute
  and nothing else - undecorated methods stay private to your code.

  This unit shows, in one place, most of what the library can do for a tool:

    * [McpScope]  - a namespace prefix for every tool name in the class
    * [McpTool]   - name, description and a comma-separated tag string
    * [McpParam]  - per-parameter description (feeds the JSON input schema)
    * [McpApp]    - links a tool to an interactive MCP App (ui:// resource)
    * [Context]   - dependency injection of per-request/per-session services:
                      IGarbageCollector  - deterministic cleanup
                      TShoppingSession   - typed, per-client state
                      TMCPMessageQueue   - server -> client notifications
    * return types - a plain string, a Neon-serializable object graph, or a
                     TContentList when the answer mixes text and binary content

  Lifecycle: the class is instantiated *per call*, the [Context] fields are
  injected, the method runs, the result is serialized, the instance is freed.
  Never keep state in a field of a tool class - use the session instead.
}

interface

uses
  System.Classes, System.SysUtils, System.JSON, System.Generics.Collections,
  System.IOUtils, System.Rtti,

  // Neon does the JSON (de)serialization of parameters and results, and also
  // generates the JSON Schema published in tools/list.
  Neon.Core.Persistence,
  Neon.Core.Persistence.JSON,

  MCPConnect.Configuration.MCP,
  MCPConnect.JRPC.Core,
  MCPConnect.JRPC.Classes,   // IGarbageCollector, ContextAttribute
  MCPConnect.MCP.Types,      // TContentList, TToolListChangedNotification
  MCPConnect.MCP.Tools,
  MCPConnect.MCP.Attributes, // McpTool, McpParam, McpScope, McpApp
  MCPConnect.Session.Core;   // TMCPSessionBase

type
  /// <summary>
  ///   One line of the shopping cart. A plain class: Neon can serialize it as
  ///   is, and the TObjectDictionary below owns and frees the instances.
  /// </summary>
  TCartItem = class
  private
    FItemId: string;
    FQuantity: Integer;
  public
    property ItemId: string read FItemId write FItemId;
    property Quantity: Integer read FQuantity write FQuantity;

    constructor Create(const AItemId: string; AQuantity: Integer);
  end;

  /// <summary>
  ///   The typed session for this demo.
  ///
  ///   Registered in MCPServer.Config with
  ///   <c>.Plugin.Configure&lt;ISessionConfig&gt;.SetSessionClass(TShoppingSession)</c>,
  ///   it is created on first contact with a client and destroyed when the
  ///   session times out. One instance per MCP client, so two chat windows get
  ///   two independent carts.
  ///
  ///   Requirements: descend from TMCPSessionBase (which supplies SessionId,
  ///   CreatedAt, LastAccessedAt) and expose a parameterless constructor.
  ///
  ///   Thread safety: the session manager serializes access per session, but
  ///   if you spawn your own threads inside a tool you own the locking.
  /// </summary>
  TShoppingSession = class(TMCPSessionBase)
  private
    FCart: TObjectDictionary<string, TCartItem>;
  public
    property Cart: TObjectDictionary<string, TCartItem> read FCart;

    constructor Create;
    destructor Destroy; override;
  end;

  /// <summary>
  ///   A conference ticket. Published properties are what Neon serializes, so
  ///   this class doubles as the JSON contract seen by the model.
  /// </summary>
  TTicket = class
  private
    FTitle: string;
    FDate: TDateTime;
    FPrice: Currency;
    FId: Integer;
  public
    property Id: Integer read FId write FId;
    property Title: string read FTitle write FTitle;
    property Price: Currency read FPrice write FPrice;
    property Date: TDateTime read FDate write FDate;

    constructor Create(AId: Integer; const ATitle: string; ADate: TDateTime; APrice: Currency);
  end;

  /// <summary>
  ///   Returning a TObjectList descendant is enough: Neon turns it into a JSON
  ///   array and the garbage collector frees list and items together.
  /// </summary>
  TTickets = class(TObjectList<TTicket>);

  /// <summary>
  ///   The DelphiDay ticket desk.
  ///
  ///   [McpScope] prefixes every tool name declared in this class, so
  ///   'get-tickets' is published as 'delphiday_get-tickets'. The separator is
  ///   '_' by default and can be changed with Server.SetScopeSeparator.
  ///   Scoping keeps names unique when several tool classes - or several
  ///   servers behind one client - are combined.
  /// </summary>
  [McpScope('delphiday')]
  TDelphiDayTool = class
  private
    // -------------------------------------------------------------------
    //  [Context] injection
    // -------------------------------------------------------------------
    //  These private fields are filled by the framework before the tool
    //  method runs. Field visibility is irrelevant (RTTI reaches private
    //  fields); the *type* is what selects the service to inject.

    /// <summary>
    ///   Request-scoped garbage collector. Anything handed to FGC.Add is freed
    ///   when the request ends, whatever happens in between - which is what
    ///   makes multi-step object construction exception safe. An overload
    ///   accepts a custom TDisposeAction for non-Free cleanup.
    /// </summary>
    [Context] FGC: IGarbageCollector;

    /// <summary>
    ///   The session instance belonging to this caller. Never nil: if the
    ///   client sent no session id, one has just been created for it.
    /// </summary>
    [Context] FSession: TShoppingSession;

    /// <summary>
    ///   The outbound message queue. Anything enqueued here is pushed to the
    ///   client *while the tool is still running*: over HTTP as Server-Sent
    ///   Events (each event carries an id, so a client reconnecting with
    ///   Last-Event-ID can replay what it missed), over STDIO as JSON lines.
    ///   This is how progress updates and list-changed notifications work.
    /// </summary>
    [Context] Responses: TMCPMessageQueue;

  public
    /// <summary>
    ///   Lists the available tickets.
    ///
    ///   Tags ('icon=badge.png'): the third argument of [McpTool] is a
    ///   comma-separated tag string mixing MCP annotations and MCPConnect
    ///   metadata - readonly, destructive, idempotent, openworld,
    ///   category=..., icon=..., disabled, structured, app=ui://...
    ///   Icon file names resolve against Server.SetIconFolder.
    ///
    ///   [McpApp] links this tool to an interactive MCP App: when the client
    ///   supports Apps, it renders the ui:// HTML resource instead of (or
    ///   beside) the raw result. The same link can be expressed as the tag
    ///   'app=ui://delphiday/ticket-app'.
    /// </summary>
    [McpTool('get-tickets', 'Get the list of available tickets for the DelphiDay', 'icon=badge.png')]
    [McpApp('ui://delphiday/ticket-app')]
    function GetTickets: TTickets;

    /// <summary>
    ///   Adds a ticket to the cart of the calling session.
    ///   [McpParam] supplies the description that ends up in the tool JSON
    ///   input schema - this is the text the model reads to decide what to
    ///   pass, so it is worth writing carefully. The parameter *type* gives
    ///   the schema type; a parameter with a default value becomes optional.
    /// </summary>
    [McpTool('cart-add', 'Add a ticket to the cart', 'icon=cart-add.png')]
    function AddToCart(
      [McpParam('ticket_id', 'ID of the ticket to add')] const ATicketId: string;
      [McpParam('quantity', 'Number of tickets')] AQuantity: Integer
    ): string;

    /// <summary>
    ///   Reads the cart. A plain string result is wrapped in a single text
    ///   content block - the simplest possible tool signature.
    /// </summary>
    [McpTool('cart-get', 'Get all tickets in the cart', 'icon=cart-full.png')]
    function GetCart: string;

    [McpTool('cart-remove', 'Remove a ticket from the cart', 'icon=cart-remove.png')]
    function RemoveFromCart(
      [McpParam('ticket_id', 'ID of the ticket to remove')] const ATicketId: string
    ): string;

    [McpTool('cart-clear', 'Clear all tickets from the cart', 'icon=cart.png')]
    function ClearCart: string;

    /// <summary>
    ///   Confirms the purchase. Returns a TContentList, the type to use when a
    ///   single tool result carries several blocks of different kinds - here a
    ///   text confirmation plus the ticket image. TContentList also offers
    ///   AddAudio, AddBlob and AddLink.
    /// </summary>
    [McpTool('buy-tickets', 'Confirm purchase of all tickets in the cart', 'icon=cart-full.png')]
    function BuyTickets: TContentList;

    /// <summary>
    ///   Diagnostic helper: proves that the session really is per-client and
    ///   survives across calls.
    /// </summary>
    [McpTool('session-info', 'Get session information (ID, created time, last accessed)', 'icon=gear.png')]
    function GetSessionInfo: string;
  end;

  /// <summary>
  ///   A custom server -> client notification.
  ///
  ///   Deriving from TJRPCNotification and filling Method + named params is
  ///   all it takes: the transport serializes it as a JSON-RPC notification
  ///   (no id, no response expected) on the open SSE/STDIO channel.
  /// </summary>
  TTicketProgressNotification = class(TJRPCNotification)
  public
    constructor Create(APosition, ASize: Integer);
  end;

implementation

{ TCartItem }

constructor TCartItem.Create(const AItemId: string; AQuantity: Integer);
begin
  inherited Create;
  FItemId := AItemId;
  FQuantity := AQuantity;
end;

{ TShoppingSession }

constructor TShoppingSession.Create;
begin
  inherited Create;
  // doOwnsValues: the dictionary frees the TCartItem instances, so the whole
  // cart disappears with the session.
  FCart := TObjectDictionary<string, TCartItem>.Create([doOwnsValues]);
end;

destructor TShoppingSession.Destroy;
begin
  FCart.Free;
  inherited;
end;

{ TDelphiDayTool }

function TDelphiDayTool.BuyTickets: TContentList;
var
  LItem: TCartItem;
  LList: TStringList;
begin
  // The result object is freed by the framework after serialization, so it is
  // safe to build it and Exit early.
  if FSession.Cart.Count = 0 then
  begin
    Result := TContentList.Create;
    Result.AddText('Cart is empty, nothing to buy.');
    Exit;
  end;

  LList := TStringList.Create;
  try
    LList.Add('Tickets purchased:');
    LList.Add('');
    for LItem in FSession.Cart.Values do
    begin
      LList.Add(Format('  - Ticket %s: quantity %d', [LItem.ItemId, LItem.Quantity]));
      TFile.AppendAllText('purchase.log', Format('%s - Ticket ID %s, Quantity: %d' + sLineBreak,
        [DateTimeToStr(Now), LItem.ItemId, LItem.Quantity]));
    end;

    FSession.Cart.Clear;

    Result := TContentList.Create;
    Result.AddText(LList.Text + sLineBreak +
      'Purchase completed successfully. Since you made the reservation through an LLM, ' +
      'you will be offered an aperitif at the end of the conference!');
  finally
    LList.Free;
  end;

  // Second content block: binary content. AddImage reads the stream and
  // base64-encodes it into an MCP image block - the stream itself stays ours
  // to free, which is why it is wrapped in try/finally.
  var LStream := TFileStream.Create(TPath.Combine(GetCurrentDir, 'data\ticket.png'), fmOpenRead or fmShareDenyWrite);
  try
    Result.AddImage('image/png', LStream);
  finally
    LStream.Free;
  end;
end;

function TDelphiDayTool.GetTickets: TTickets;
begin
  // Sends progress notifications while loading tickets, simulating a slow backend
  Result := TTickets.Create;

  // Hand the result to the request garbage collector *immediately after
  // creation*: if any of the steps below raised, the list would otherwise
  // leak. This is the recommended pattern whenever a tool builds its result
  // in more than one step.
  FGC.Add(Result);

  Result.Add(TTicket.Create(1, 'Conferenza + Seminari', EncodeDate(2026, 6, 9), 179.0));
  Responses.Enqueue(TTicketProgressNotification.Create(1, 3));
  Sleep(500);

  Result.Add(TTicket.Create(2, 'Solo Conferenza', EncodeDate(2026, 6, 10), 0));
  Responses.Enqueue(TTicketProgressNotification.Create(2, 3));
  Sleep(500);

  Result.Add(TTicket.Create(3, 'Young Ticket', EncodeDate(2026, 6, 10), 69.0));
  Responses.Enqueue(TTicketProgressNotification.Create(3, 3));
  Sleep(500);

  // Tells the client its cached tool list is stale, so it re-issues tools/list.
  // Send this whenever tools are registered or unregistered at runtime (the
  // resource and prompt equivalents are TResourceListChangedNotification and
  // TPromptListChangedNotification).
  Responses.Enqueue(TToolListChangedNotification.Create());
end;

function TDelphiDayTool.AddToCart(const ATicketId: string; AQuantity: Integer): string;
var
  LItem: TCartItem;
begin
  // All the state lives in the session: the tool instance itself is thrown
  // away as soon as this method returns.
  if FSession.Cart.TryGetValue(ATicketId, LItem) then
  begin
    LItem.Quantity := LItem.Quantity + AQuantity;
    Result := Format('Updated ticket %s in cart. New quantity: %d', [ATicketId, LItem.Quantity]);
  end
  else
  begin
    FSession.Cart.Add(ATicketId, TCartItem.Create(ATicketId, AQuantity));
    Result := Format('Added ticket %s to cart. Quantity: %d', [ATicketId, AQuantity]);
  end;
end;

function TDelphiDayTool.GetCart: string;
var
  LItem: TCartItem;
  LList: TStringList;
begin
  if FSession.Cart.Count = 0 then
    Exit('Cart is empty');

  LList := TStringList.Create;
  try
    LList.Add('Ticket Cart:');
    LList.Add('');
    for LItem in FSession.Cart.Values do
      LList.Add(Format('  - Ticket %s: quantity %d', [LItem.ItemId, LItem.Quantity]));
    Result := LList.Text;
  finally
    LList.Free;
  end;
end;

function TDelphiDayTool.RemoveFromCart(const ATicketId: string): string;
begin
  if FSession.Cart.Count = 0 then
    Exit('Cart is empty');

  if FSession.Cart.ContainsKey(ATicketId) then
  begin
    FSession.Cart.Remove(ATicketId);
    Result := Format('Removed ticket %s from cart', [ATicketId]);
  end
  else
    // Returning a normal string for a "not found" case keeps the call
    // successful. Raise an exception instead when the model should treat it as
    // a failure: MCPConnect maps exceptions to JSON-RPC errors and reports the
    // original exception class and message.
    Result := Format('Ticket %s not found in cart', [ATicketId]);
end;

function TDelphiDayTool.ClearCart: string;
begin
  FSession.Cart.Clear;
  Result := 'Cart cleared successfully';
end;

function TDelphiDayTool.GetSessionInfo: string;
begin
  // SessionId / CreatedAt / LastAccessedAt come from TMCPSessionBase.
  Result := Format(
    'Session Info:' + sLineBreak +
    '  ID: %s' + sLineBreak +
    '  Created: %s' + sLineBreak +
    '  Last Accessed: %s',
    [
      FSession.SessionId,
      DateTimeToStr(FSession.CreatedAt),
      DateTimeToStr(FSession.LastAccessedAt)
    ]
  );
end;

{ TTicketProgressNotification }

constructor TTicketProgressNotification.Create(APosition, ASize: Integer);
begin
  inherited Create;
  // Method is the JSON-RPC method name the client will see. Custom names are
  // legal, but a client only reacts to the ones it knows: the standard MCP
  // names are 'notifications/progress' (with the progressToken taken from the
  // request _meta) and 'notifications/message' for log output.
  Method := 'notification/logging';
  AddNamedParam('position', APosition);
  AddNamedParam('size', ASize);
end;

{ TTicket }

constructor TTicket.Create(AId: Integer; const ATitle: string; ADate: TDateTime; APrice: Currency);
begin
  inherited Create;
  FId := AId;
  FTitle := ATitle;
  FDate := ADate;
  FPrice := APrice;
end;

end.
