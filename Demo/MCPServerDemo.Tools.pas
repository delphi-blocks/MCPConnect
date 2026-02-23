unit MCPServerDemo.Tools;

interface

uses
  System.Classes, System.SysUtils, System.JSON, System.Generics.Collections,
  System.IOUtils, System.Rtti,

  Vcl.Graphics, Vcl.ExtCtrls, Vcl.Dialogs,

  Neon.Core.Persistence,
  Neon.Core.Persistence.JSON,

  MCPConnect.Configuration.MCP,
  MCPConnect.JRPC.Core,
  MCPConnect.MCP.Types,
  MCPConnect.MCP.Attributes,
  MCPConnect.Core.Utils,
  MCPConnect.Session.Core;

type
  TPerson = class
  private
    FName: string;
  public
    property Name: string read FName write FName;
    constructor Create(const AName: string);
  end;

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
  TShoppingSession = class(TSessionBase)
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
    FDescription: string;
    FId: Integer;
  public
    property Id: Integer read FId write FId;
    property Title: string read FTitle write FTitle;
    property Price: Currency read FPrice write FPrice;
    property Date: TDateTime read FDate write FDate;
    property Description: string read FDescription write FDescription;

    constructor Create(AId: Integer; const ATitle: string; ADate: TDateTime; APrice: Currency; const ADescription: string);
  end;

  TTickets = class(TObjectList<TTicket>)
  end;

  //[McpNamespace('delphiday')]
  TDelphiDayTool = class
  private
    [Context] FGC: IGarbageCollector;
  public
    [McpTool('get_tickets', 'Get the list of available tickets for the DelphiDay', 'icon=badge.png')]
    [McpApp('ui://delphiday/ticket-app')]
    function GetTickets: TTickets;

    [McpTool('buy_ticket', 'Buy tickets for the DelphiDay', 'icon=cart-full.png')]
    function BuyTicket(
      [McpParam('id', 'ID of the ticket to buy')] AId: Integer;
      [McpParam('quantity', 'Number of tickets to buy')] AQuantity: Integer
    ): TContentList;
  end;

  //[McpScope('test')]
  TTestTool = class
  public
    [McpTool('double_or_nothing', 'Doubles or zeroes the param value',
      'category=group1,app=ui://delphiday/ticket-app,icon=money.png')]
    function TestParam(
      [McpParam('value1', 'Test Parameter 1 for MCP')] AValue: Int64;
      [McpParam('value2', 'Test Parameter 2 for MCP')] ADouble: Boolean
    ): Integer;

    [McpTool('discounted_items', 'Retrieves a list of discounted items on Wintech-Italia based on the specified item type', 'icon=discount.png')]
    function GetDiscountedItems(
      [McpParam('itemType', 'The type of item to filter. Valid values: ''courses'', ''product'', ''consulting''')] const AItemType: string
    ): string;


    [McpTool('course_image', 'Retrieves the image for the selected course', 'disabled, icon=photo.png')]
    function GetImage(
      [McpParam('name', 'Course name')] const AName: string
    ): TPicture;

    [McpTool('splitstring', 'Gets the content by splitting the string (e.g. "hello,world" -> ["hello", "world"])', 'icon=tags.png')]
    function GetSplitString(
      [McpParam('value', 'The string to work with')] const AValue: string
    ): TContentList;

    [McpTool('get-person', 'Get a person info given his name', 'icon=person.png')]
    function GetPerson(
      [McpParam('name', 'The name of the person to get')] const AName: string
    ): TPerson;

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

{ TTestTool }

function TTestTool.GetDiscountedItems(const AItemType: string): string;
begin
  //'courses'', ''product'', ''consulting';
  var LStringList := TStringList.Create;
  try
    if AItemType = 'courses' then
    begin
      LStringList.Add('Programmazione ad Oggetti con Delphi');
      LStringList.Add('Delphi Modern Development');
    end
    else if AItemType = 'product' then
    begin
      LStringList.Add('Fast Report');
      LStringList.Add('UniDAC');
    end
    else
      LStringList.Add('none');
    Result := LStringList.Text;
  finally
    LStringList.Free;
  end;
end;


function TTestTool.GetImage(const AName: string): TPicture;
begin
  Result := TPicture.Create;
  Result.LoadFromFile(TPath.Combine(GetCurrentDir, 'data\italy.bmp'));
end;

function TTestTool.GetPerson(const AName: string): TPerson;
begin
  Result := TPerson.Create(AName);
end;

function TTestTool.GetSplitString(const AValue: string): TContentList;
begin
  var LResultBuilder := TToolResultBuilder.CreateInstance;
  var LStrings := AValue.Split([',']);
  for var LString in LStrings do
  begin
    LResultBuilder.AddText(LString);
  end;
  Result := LResultBuilder.Build;
end;

function TTestTool.TestParam(AValue: Int64; ADouble: Boolean): Integer;
begin
  if ADouble then
    Result := AValue * 2
  else
    Result := 0;
end;

{ TPerson }

constructor TPerson.Create(const AName: string);
begin
  inherited Create;
  FName := AName;
end;

{ TDelphiDayTool }

function TDelphiDayTool.BuyTicket(AId, AQuantity: Integer): TContentList;
var
  LExePath: string;
begin
  LExePath := ExtractFileDir(ParamStr(0));

  TFile.AppendAllText('purchase.log', Format('%s - Ticket ID %d, People: %d' + sLineBreak, [DateTimeToStr(Now), AId, AQuantity]));

  var LResultBuilder := TToolResultBuilder.CreateInstance;
  LResultBuilder.AddText('Purchase completed successfully. Since you made the reservation through an LLM, you will be offered an aperitif at the end of the conference!');

  var LStream := TFileStream.Create(TPath.Combine(GetCurrentDir, 'data\ticket.png'), fmOpenRead or fmShareDenyWrite);
  try
    LResultBuilder.AddImage('image/png', LStream);
  finally
    LStream.Free;
  end;

  Result := LResultBuilder.Build;
end;

function TDelphiDayTool.GetTickets: TTickets;
begin
  Result := TTickets.Create;
  FGC.Add(Result);
  Result.Add(TTicket.Create(1, 'Conferenza + Seminari', StrToDate('19/11/2025'), 179.0, ''));
  Result.Add(TTicket.Create(2, 'Solo Conferenza', StrToDate('19/11/2025'), 0, ''));
  Result.Add(TTicket.Create(3, 'Young ticket', StrToDate('19/11/2025'), 69.0, ''));
end;

{ TTicket }

constructor TTicket.Create(AId: Integer; const ATitle: string; ADate: TDateTime;
  APrice: Currency; const ADescription: string);
begin
  inherited Create;
  FId := AId;
  FTitle := ATitle;
  FDate := ADate;
  FPrice := APrice;
  FDescription := ADescription;
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

end.
