unit MCPServer.Tools;

interface

uses
  System.Classes, System.SysUtils, System.JSON, System.Generics.Collections,
  System.IOUtils, System.Rtti,

  Neon.Core.Persistence,
  Neon.Core.Persistence.JSON,

  MCPConnect.Configuration.MCP,
  MCPConnect.JRPC.Core,
  MCPConnect.JRPC.Classes,
  MCPConnect.MCP.Types,
  MCPConnect.MCP.Tools,
  MCPConnect.MCP.Attributes,
  MCPConnect.Session.Core;

type
  TCartItem = class
  private
    FItemId: string;
    FQuantity: Integer;
  public
    property ItemId: string read FItemId write FItemId;
    property Quantity: Integer read FQuantity write FQuantity;

    constructor Create(const AItemId: string; AQuantity: Integer);
  end;

  TShoppingSession = class(TMCPSessionBase)
  private
    FCart: TObjectDictionary<string, TCartItem>;
  public
    property Cart: TObjectDictionary<string, TCartItem> read FCart;

    constructor Create;
    destructor Destroy; override;
  end;

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

  TTickets = class(TObjectList<TTicket>);

  [McpScope('delphiday')]
  TDelphiDayTool = class
  private
    [Context] FGC: IGarbageCollector;
    [Context] FSession: TShoppingSession;
    [Context] Responses: TMCPMessageQueue;

  public
    [McpTool('get-tickets', 'Get the list of available tickets for the DelphiDay', 'icon=badge.png')]
    [McpApp('ui://delphiday/ticket-app')]
    function GetTickets: TTickets;

    [McpTool('cart-add', 'Add a ticket to the cart', 'icon=cart-add.png')]
    function AddToCart(
      [McpParam('ticket_id', 'ID of the ticket to add')] const ATicketId: string;
      [McpParam('quantity', 'Number of tickets')] AQuantity: Integer
    ): string;

    [McpTool('cart-get', 'Get all tickets in the cart', 'icon=cart-full.png')]
    function GetCart: string;

    [McpTool('cart-remove', 'Remove a ticket from the cart', 'icon=cart-remove.png')]
    function RemoveFromCart(
      [McpParam('ticket_id', 'ID of the ticket to remove')] const ATicketId: string
    ): string;

    [McpTool('cart-clear', 'Clear all tickets from the cart', 'icon=cart.png')]
    function ClearCart: string;

    [McpTool('buy-tickets', 'Confirm purchase of all tickets in the cart', 'icon=cart-full.png')]
    function BuyTickets: TContentList;

    [McpTool('session-info', 'Get session information (ID, created time, last accessed)', 'icon=gear.png')]
    function GetSessionInfo: string;
  end;

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

  Responses.Enqueue(TToolListChangedNotification.Create());
end;

function TDelphiDayTool.AddToCart(const ATicketId: string; AQuantity: Integer): string;
var
  LItem: TCartItem;
begin
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
    Result := Format('Ticket %s not found in cart', [ATicketId]);
end;

function TDelphiDayTool.ClearCart: string;
begin
  FSession.Cart.Clear;
  Result := 'Cart cleared successfully';
end;

function TDelphiDayTool.GetSessionInfo: string;
begin
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
