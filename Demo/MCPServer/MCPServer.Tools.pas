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
  /// <summary>
  ///   Shopping cart item with typed properties
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
  ///   Custom typed session for shopping cart.
  ///   No JSON storage - pure typed properties.
  /// </summary>
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

  //[McpNamespace('delphiday')]
  TDelphiDayTool = class
  private
    [Context] FGC: IGarbageCollector;
    //[Context] FSession: TShoppingSession;
    [Context] Responses: TMCPMessageQueue;

  public
    [McpTool('get-tickets', 'Get the list of available tickets for the DelphiDay', 'icon=badge.png')]
    [McpApp('ui://delphiday/ticket-app')]
    function GetTickets: TTickets;

    [McpTool('buy-tickets', 'Buy tickets for the DelphiDay', 'icon=cart-full.png')]
    function BuyTicket(
      [McpParam('id', 'ID of the ticket to buy')] AId: Integer;
      [McpParam('quantity', 'Number of tickets to buy')] AQuantity: Integer
    ): TContentList;
  end;


  /// <summary>
  ///   Shopping cart tool that uses typed session to maintain state across requests
  /// </summary>
  [McpScope('shopping')]
  TShoppingCartTool = class
  private
    [Context] FSession: TShoppingSession;
  public
    [McpTool('cart_add', 'Add an item to the shopping cart', 'icon=cart-add.png')]
    function AddToCart(
      [McpParam('item_id', 'ID of the item to add')] const AItemId: string;
      [McpParam('quantity', 'Quantity to add')] AQuantity: Integer
    ): string;

    [McpTool('cart_get', 'Get all items in the shopping cart', 'icon=cart-full.png')]
    function GetCart: string;

    [McpTool('cart_remove', 'Remove an item from the shopping cart', 'icon=cart-remove.png')]
    function RemoveFromCart(
      [McpParam('item_id', 'ID of the item to remove')] const AItemId: string
    ): string;

    [McpTool('cart_clear', 'Clear all items from the shopping cart', 'icon=cart.png')]
    function ClearCart: string;

    [McpTool('session_info', 'Get session information (ID, created time, last accessed)', 'icon=gear.png')]
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

function TDelphiDayTool.BuyTicket(AId, AQuantity: Integer): TContentList;
var
  LExePath: string;
begin
  LExePath := ExtractFileDir(ParamStr(0));

  TFile.AppendAllText('purchase.log', Format('%s - Ticket ID %d, People: %d' + sLineBreak, [DateTimeToStr(Now), AId, AQuantity]));

  Result := TContentList.Create;

  Result.AddText('Purchase completed successfully. Since you made the reservation through an LLM, you will be offered an aperitif at the end of the conference!');

  var LStream := TFileStream.Create(TPath.Combine(GetCurrentDir, 'data\ticket.png'), fmOpenRead or fmShareDenyWrite);
  try
    Result.AddImage('image/png', LStream);
  finally
    LStream.Free;
  end;
end;

function TDelphiDayTool.GetTickets: TTickets;
begin
  Result := TTickets.Create;
  FGC.Add(Result);

  Result.Add(TTicket.Create(1, 'Conferenza + Seminari', EncodeDate(2026, 6, 9), 179.0));
  Responses.Enqueue(TTicketProgressNotification.Create(1, 3));
  Sleep(1000);

  Result.Add(TTicket.Create(2, 'Solo Conferenza', EncodeDate(2026, 6, 10), 0));
  Responses.Enqueue(TTicketProgressNotification.Create(2, 3));
  Sleep(1000);

  Result.Add(TTicket.Create(3, 'Young Ticket', EncodeDate(2026, 6, 10), 69.0));
  Responses.Enqueue(TTicketProgressNotification.Create(3, 3));
  Sleep(1000);

  Responses.Enqueue(TToolListChangedNotification.Create());

end;

{ TShoppingCartTool }

function TShoppingCartTool.AddToCart(const AItemId: string; AQuantity: Integer): string;
var
  LItem: TCartItem;
begin
  // Check if item already exists in cart
  if FSession.Cart.TryGetValue(AItemId, LItem) then
  begin
    // Update quantity
    LItem.Quantity := LItem.Quantity + AQuantity;
    Result := Format('Updated %s in cart. New quantity: %d', [AItemId, LItem.Quantity]);
  end
  else
  begin
    // Add new item
    LItem := TCartItem.Create(AItemId, AQuantity);
    FSession.Cart.Add(AItemId, LItem);
    Result := Format('Added %s to cart. Quantity: %d', [AItemId, AQuantity]);
  end;
end;

function TShoppingCartTool.GetCart: string;
var
  LItem: TCartItem;
  LList: TStringList;
begin
  if FSession.Cart.Count = 0 then
    Exit('Cart is empty');

  LList := TStringList.Create;
  try
    LList.Add('Shopping Cart:');
    LList.Add('');
    for LItem in FSession.Cart.Values do
      LList.Add(Format('  - %s: quantity %d', [LItem.ItemId, LItem.Quantity]));
    LList.Add('');
    LList.Add(Format('Total items: %d', [FSession.Cart.Count]));
    Result := LList.Text;
  finally
    LList.Free;
  end;
end;

function TShoppingCartTool.RemoveFromCart(const AItemId: string): string;
begin
  if FSession.Cart.Count = 0 then
    Exit('Cart is empty');

  if FSession.Cart.ContainsKey(AItemId) then
  begin
    FSession.Cart.Remove(AItemId);
    Result := Format('Removed %s from cart', [AItemId]);
  end
  else
    Result := Format('Item %s not found in cart', [AItemId]);
end;

function TShoppingCartTool.ClearCart: string;
begin
  FSession.Cart.Clear;
  Result := 'Cart cleared successfully';
end;

function TShoppingCartTool.GetSessionInfo: string;
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
