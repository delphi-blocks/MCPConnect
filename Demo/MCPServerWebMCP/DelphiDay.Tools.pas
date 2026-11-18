unit DelphiDay.Tools;

{
  The tools the DelphiDay site offers to the agent in the visitor's browser

  Every tool is an ordinary [McpTool] on an ordinary class, so any MCP client
  can call them at /mcp as well. What makes them WebMCP tools is the "webmcp"
  tag: with a value ("webmcp=sessions") a tool is offered only on the pages that
  render that group, without one it is offered on every page.

      group        page          tools
      -----------  ------------  ------------------------------------------
      speakers     /speakers     search_speakers
      sessions     /sessions     search_sessions, get_session, add_to_schedule
      schedule     /schedule     remove_from_schedule
      tickets      /tickets      list_tickets, check_availability, add_to_cart
      checkout     /checkout     remove_from_cart, apply_discount, place_order
      (none)       every page    view_schedule, view_cart

  Whose schedule and cart these are is decided by the session: the page creates
  it and puts its id in a cookie, the server reads the session id from that
  cookie, and the WebMCP script calls /mcp from the page with same-origin
  credentials - so the request carries the cookie like any other, and the tool
  receives the session - a TDelphiDaySession, which holds the visitor's
  schedule and cart - through [Context].

  A rule that is broken is not an exception the protocol reports: it is an
  answer the agent reads and can act on, so it goes back as a tool result
  flagged isError.
}

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,

  MCPConnect.JRPC.Classes,   // ContextAttribute
  MCPConnect.MCP.Types,      // TTextContent
  MCPConnect.MCP.Tools,      // TCallToolResult
  MCPConnect.MCP.Attributes,
  
  DelphiDay.Data,
  DelphiDay.Service;

type
  TDelphiDayTools = class
  private
    /// <summary>
    ///   The session of the request: the visitor, with their schedule and
    ///   cart. Context data is found by its exact class, which is the one
    ///   registered with SetSessionClass. The server answers a request without
    ///   a session itself (HTTP 400), so whenever a tool runs there is one.
    /// </summary>
    [Context] FSession: TDelphiDaySession;

    /// <summary>
    ///   The visitor the request comes from: its session.
    /// </summary>
    function Visitor: TDelphiDaySession;

    /// <summary>
    ///   Runs ACall and turns a broken rule into an answer for the agent.
    /// </summary>
    function Run(const ACall: TFunc<string>): TCallToolResult;

    function DescribeSession(ASession: TSession; AWithSummary: Boolean = False): string;
    function ListSessions(const ASessions: TArray<TSession>): string;
  public
    // --- /speakers ---------------------------------------------------------

    [McpTool('search_speakers', 'Search the speakers of Delphi Day 2027 by name, company or topic. Returns each speaker with their company, topics and sessions.', 'readonly,webmcp=speakers')]
    function SearchSpeakers(
      [McpParam('query', 'Text to look for in the speaker name, company, topics or session titles. Empty for everyone.')] const AQuery: string = '';
      [McpParam('topic', 'Only speakers with a session in this track, e.g. AI, Tooling, Web & Data, Language & RTL, Mobile & Cross-platform')] const ATopic: string = ''
    ): TCallToolResult;

    // --- /sessions ---------------------------------------------------------

    [McpTool('search_sessions', 'Search the sessions (seminars on Tuesday 8 June, talks on Wednesday 9 June) by text, track, level or day. Returns the session numbers to use with get_session and add_to_schedule.', 'readonly,webmcp=sessions')]
    function SearchSessions(
      [McpParam('query', 'Text to look for in the title, summary, track or speaker. Empty for any.')] const AQuery: string = '';
      [McpParam('track', 'Only this track, e.g. AI, Tooling, Web & Data, Language & RTL, Mobile & Cross-platform, Delphi')] const ATrack: string = '';
      [McpParam('level', 'Only this level: Beginner, Intermediate or Advanced')] const ALevel: string = '';
      [McpParam('day', '1 = Tuesday 8 June (seminars), 2 = Wednesday 9 June (conference), 0 = both')] ADay: Integer = 0
    ): TCallToolResult;

    [McpTool('get_session', 'Get the full details of one session: title, speaker, time, room, level and abstract.', 'readonly,webmcp=sessions')]
    function GetSession(
      [McpParam('session_id', 'The session number, from search_sessions')] ASessionId: Integer
    ): TCallToolResult;

    [McpTool('add_to_schedule', 'Add a session to the visitor''s personal schedule. Refused, with the reason, if it overlaps a session already in the schedule.', 'idempotent,webmcp=sessions')]
    function AddToSchedule(
      [McpParam('session_id', 'The session number, from search_sessions')] ASessionId: Integer
    ): TCallToolResult;

    // --- /schedule ---------------------------------------------------------

    [McpTool('remove_from_schedule', 'Remove a session from the visitor''s personal schedule.', 'idempotent,webmcp=schedule')]
    function RemoveFromSchedule(
      [McpParam('session_id', 'The session number to remove')] ASessionId: Integer
    ): TCallToolResult;

    [McpTool('view_schedule', 'Show the visitor''s personal schedule in order, and which tickets it needs.', 'readonly,webmcp')]
    function ViewSchedule: TCallToolResult;

    // --- /tickets ----------------------------------------------------------

    [McpTool('list_tickets', 'List the ticket types with their price in euros, what each one covers and how many seats are left.', 'readonly,webmcp=tickets')]
    function ListTickets: TCallToolResult;

    [McpTool('check_availability', 'Check whether a number of tickets of one type can still be bought. Does not reserve anything.', 'readonly,webmcp=tickets')]
    function CheckAvailability(
      [McpParam('ticket_type', 'The ticket type: seminar, conference, combo or student')] const ATicketType: string;
      [McpParam('quantity', 'How many tickets, 1 to 10')] AQuantity: Integer = 1
    ): TCallToolResult;

    [McpTool('add_to_cart', 'Add tickets to the visitor''s cart. Nothing is bought until the visitor confirms the order on the Checkout page.', 'webmcp=tickets')]
    function AddToCart(
      [McpParam('ticket_type', 'The ticket type: seminar, conference, combo or student')] const ATicketType: string;
      [McpParam('quantity', 'How many tickets to add, 1 to 10')] AQuantity: Integer = 1
    ): TCallToolResult;

    // --- /checkout ---------------------------------------------------------

    [McpTool('view_cart', 'Show the cart: the tickets, the discount, the total in euros, and whether an order is waiting for the visitor''s confirmation.', 'readonly,webmcp')]
    function ViewCart: TCallToolResult;

    [McpTool('remove_from_cart', 'Remove one ticket type from the cart.', 'idempotent,webmcp=checkout')]
    function RemoveFromCart(
      [McpParam('ticket_type', 'The ticket type to remove: seminar, conference, combo or student')] const ATicketType: string
    ): TCallToolResult;

    [McpTool('apply_discount', 'Apply a discount code to the cart.', 'idempotent,webmcp=checkout')]
    function ApplyDiscount(
      [McpParam('code', 'The discount code')] const ACode: string
    ): TCallToolResult;

    [McpTool('place_order', 'Prepare the order for the tickets in the cart. This does NOT buy anything: the visitor must press "Confirm purchase" on the Checkout page themselves. Tell them so after calling it.', 'webmcp=checkout')]
    function PlaceOrder(
      [McpParam('attendee_name', 'Full name of the person who will attend')] const AAttendeeName: string;
      [McpParam('attendee_email', 'E-mail address for the confirmation')] const AAttendeeEmail: string
    ): TCallToolResult;
  end;

implementation

{ TDelphiDayTools }

function TDelphiDayTools.Visitor: TDelphiDaySession;
begin
  if not Assigned(FSession) then
    raise EDelphiDay.Create('No visitor is known for this request. These tools work on the page ' +
      'of the DelphiDay site: open it in the browser first, then call them from there.');
  Result := FSession;
end;

function TDelphiDayTools.Run(const ACall: TFunc<string>): TCallToolResult;
var
  LText: string;
  LFailed: Boolean;
begin
  LFailed := False;
  try
    LText := ACall();
  except
    on E: EDelphiDay do
    begin
      // A broken rule is an answer the agent reads, not a protocol error
      LText := E.Message;
      LFailed := True;
    end;
  end;

  Result := TCallToolResult.Create;
  Result.AddContent(TTextContent.CreateWithText(LText));
  if LFailed then
    Result.IsError := True;
end;

function TDelphiDayTools.DescribeSession(ASession: TSession; AWithSummary: Boolean): string;
var
  LSpeaker: TSpeaker;
  LCatalog: TCatalog;
begin
  LCatalog := DelphiDayService.Catalog;
  LSpeaker := LCatalog.FindSpeaker(ASession.SpeakerId);
  Result := Format('#%d "%s" - %s (%s), %s, %s - %s, %s, %s',
    [ASession.Id, ASession.Title, LSpeaker.Name, LanguageName(LSpeaker.Language),
     LCatalog.DayName(ASession.Day), ASession.TimeText, ASession.Room, ASession.Track, ASession.Level]);
  if ASession.Kind = TSessionKind.Seminar then
    Result := Result + ' [seminar: needs a Seminar day or Combo ticket]';
  if AWithSummary then
    Result := Result + sLineBreak + ASession.Summary;
end;

function TDelphiDayTools.ListSessions(const ASessions: TArray<TSession>): string;
var
  LSession: TSession;
begin
  if Length(ASessions) = 0 then
    Exit('No session matches.');
  Result := Format('%d session(s):', [Length(ASessions)]);
  for LSession in ASessions do
    Result := Result + sLineBreak + DescribeSession(LSession);
end;

function TDelphiDayTools.SearchSpeakers(const AQuery, ATopic: string): TCallToolResult;
begin
  Result := Run(
    function: string
    var
      LSpeaker: TSpeaker;
      LSession: TSession;
      LSpeakers: TArray<TSpeaker>;
    begin
      LSpeakers := DelphiDayService.SearchSpeakers(AQuery, ATopic);
      if Length(LSpeakers) = 0 then
        Exit('No speaker matches.');

      Result := Format('%d speaker(s):', [Length(LSpeakers)]);
      for LSpeaker in LSpeakers do
      begin
        Result := Result + sLineBreak + '- ' + LSpeaker.Name;
        if LSpeaker.Company <> '' then
          Result := Result + ' (' + LSpeaker.Company + ')';
        Result := Result + ' - speaks ' + LanguageName(LSpeaker.Language) + ' - topics: ' + LSpeaker.Topics;
        for LSession in DelphiDayService.Catalog.Sessions do
          if SameText(LSession.SpeakerId, LSpeaker.Id) then
            Result := Result + sLineBreak + '    ' + Format('session #%d "%s" (%s, %s)',
              [LSession.Id, LSession.Title, DelphiDayService.Catalog.DayName(LSession.Day), LSession.TimeText]);
      end;
    end);
end;

function TDelphiDayTools.SearchSessions(const AQuery, ATrack, ALevel: string;
  ADay: Integer): TCallToolResult;
begin
  Result := Run(
    function: string
    begin
      Result := ListSessions(DelphiDayService.SearchSessions(AQuery, ATrack, ALevel, ADay));
    end);
end;

function TDelphiDayTools.GetSession(ASessionId: Integer): TCallToolResult;
begin
  Result := Run(
    function: string
    var
      LSession: TSession;
    begin
      LSession := DelphiDayService.Catalog.FindSession(ASessionId);
      if LSession = nil then
        raise EDelphiDay.CreateFmt('There is no session %d', [ASessionId]);
      Result := DescribeSession(LSession, True);
    end);
end;

function TDelphiDayTools.AddToSchedule(ASessionId: Integer): TCallToolResult;
begin
  Result := Run(
    function: string
    begin
      Result := DelphiDayService.AddToSchedule(Visitor, ASessionId);
    end);
end;

function TDelphiDayTools.RemoveFromSchedule(ASessionId: Integer): TCallToolResult;
begin
  Result := Run(
    function: string
    begin
      Result := DelphiDayService.RemoveFromSchedule(Visitor, ASessionId);
    end);
end;

function TDelphiDayTools.ViewSchedule: TCallToolResult;
begin
  Result := Run(
    function: string
    var
      LId: Integer;
      LVisitor: TDelphiDaySession;
      LState: TVisitorState;
    begin
      LVisitor := Visitor;
      LState := DelphiDayService.Snapshot(LVisitor);
      if Length(LState.Schedule) = 0 then
        Exit('The schedule is empty.');

      Result := Format('%d session(s) in the schedule:', [Length(LState.Schedule)]);
      for LId in LState.Schedule do
        Result := Result + sLineBreak + DescribeSession(DelphiDayService.Catalog.FindSession(LId));
      Result := Result + sLineBreak + DelphiDayService.TicketAdvice(LVisitor);
    end);
end;

function TDelphiDayTools.ListTickets: TCallToolResult;
begin
  Result := Run(
    function: string
    var
      LTicket: TTicketType;
      LSeats: Integer;
    begin
      Result := 'Ticket types (prices in euros):';
      for LTicket in DelphiDayService.Catalog.Tickets do
      begin
        LSeats := DelphiDayService.SeatsLeft(LTicket.Id);
        Result := Result + sLineBreak + Format('- %s (type "%s"): EUR %d - %s - ',
          [LTicket.Name, LTicket.Id, LTicket.Price, LTicket.Description]);
        if LSeats = 0 then
          Result := Result + 'SOLD OUT'
        else
          Result := Result + Format('%d seat(s) left', [LSeats]);
      end;
      Result := Result + sLineBreak + 'Discount codes exist; apply one with apply_discount.';
    end);
end;

function TDelphiDayTools.CheckAvailability(const ATicketType: string;
  AQuantity: Integer): TCallToolResult;
begin
  Result := Run(
    function: string
    var
      LTicket: TTicketType;
      LSeats: Integer;
    begin
      LTicket := DelphiDayService.Catalog.FindTicket(ATicketType);
      if LTicket = nil then
        raise EDelphiDay.CreateFmt('Unknown ticket type "%s". The types are: seminar, conference, combo, student',
          [ATicketType]);

      LSeats := DelphiDayService.SeatsLeft(LTicket.Id);
      if LSeats = 0 then
        Result := Format('%s is sold out.', [LTicket.Name])
      else if AQuantity <= LSeats then
        Result := Format('%d x %s can be bought: %d seat(s) left, EUR %d each.',
          [AQuantity, LTicket.Name, LSeats, LTicket.Price])
      else
        Result := Format('%d x %s cannot be bought: only %d seat(s) left.',
          [AQuantity, LTicket.Name, LSeats]);
    end);
end;

function TDelphiDayTools.AddToCart(const ATicketType: string; AQuantity: Integer): TCallToolResult;
begin
  Result := Run(
    function: string
    begin
      Result := DelphiDayService.AddToCart(Visitor, ATicketType, AQuantity);
    end);
end;

function TDelphiDayTools.ViewCart: TCallToolResult;
begin
  Result := Run(
    function: string
    var
      LVisitor: TDelphiDaySession;
      LState: TVisitorState;
      LLine: TCartLine;
      LTicket: TTicketType;
      LTotals: TCartTotals;
    begin
      LVisitor := Visitor;
      LState := DelphiDayService.Snapshot(LVisitor);
      if Length(LState.Cart) = 0 then
        Exit('The cart is empty.');

      Result := 'Cart:';
      for LLine in LState.Cart do
      begin
        LTicket := DelphiDayService.Catalog.FindTicket(LLine.TicketId);
        Result := Result + sLineBreak + Format('- %d x %s (type "%s") at EUR %d',
          [LLine.Quantity, LTicket.Name, LTicket.Id, LTicket.Price]);
      end;

      LTotals := DelphiDayService.Totals(LVisitor);
      Result := Result + sLineBreak + Format('Subtotal EUR %s', [CentsText(LTotals.Subtotal)]);
      if LTotals.DiscountPercent > 0 then
        Result := Result + sLineBreak + Format('Discount %s (%d%%): -EUR %s',
          [LState.DiscountCode, LTotals.DiscountPercent, CentsText(LTotals.Discount)]);
      Result := Result + sLineBreak + Format('Total EUR %s', [CentsText(LTotals.Total)]);

      if LState.HasPending then
        Result := Result + sLineBreak + Format('An order for %s is waiting for the visitor to press ' +
          '"Confirm purchase" on the Checkout page.', [LState.PendingName])
      else
        Result := Result + sLineBreak + 'No order has been placed yet.';
    end);
end;

function TDelphiDayTools.RemoveFromCart(const ATicketType: string): TCallToolResult;
begin
  Result := Run(
    function: string
    begin
      Result := DelphiDayService.RemoveFromCart(Visitor, ATicketType);
    end);
end;

function TDelphiDayTools.ApplyDiscount(const ACode: string): TCallToolResult;
begin
  Result := Run(
    function: string
    begin
      Result := DelphiDayService.ApplyDiscount(Visitor, ACode);
    end);
end;

function TDelphiDayTools.PlaceOrder(const AAttendeeName, AAttendeeEmail: string): TCallToolResult;
begin
  Result := Run(
    function: string
    begin
      Result := DelphiDayService.PlaceOrder(Visitor, AAttendeeName, AAttendeeEmail);
    end);
end;

end.
