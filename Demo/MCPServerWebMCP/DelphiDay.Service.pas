unit DelphiDay.Service;

{
  The business rules of the site, in one place

  The HTML forms (DelphiDay.WebModule) and the MCP tools (DelphiDay.Tools) both
  call this class and nothing else, so a visitor and a browser agent acting for
  them are held to the same rules: the same conflict check on the schedule, the
  same seat limits, the same discount codes.

  A visitor is an MCP session, of the typed class TDelphiDaySession: their
  schedule, cart and pending order live in the session object itself, so they
  go when the session expires and the session manager frees it - one lifetime,
  not two. The session id travels in a cookie, which the server is configured
  to read, so a tool call made from the page lands in the same session as the
  page. Nothing is stored or charged - this is a demo.

  The service holds the rules and the ticket stock, and works on the session it
  is handed. A page that has no session yet reads as an empty schedule and cart.

  Every public method takes the lock, so the web module and the MCP dispatcher,
  which run on different threads, can both call in.
}

interface

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.Generics.Collections,
  System.Generics.Defaults,

  MCPConnect.Session.Core,

  DelphiDay.Data;

const
  /// <summary>
  ///   The cookie that carries the session id, on the pages and, because the
  ///   WebMCP script calls /mcp from the page with same-origin credentials, on
  ///   every tool call. It is also the name the session configuration reads.
  /// </summary>
  VISITOR_COOKIE = 'dd_visitor';

  MAX_TICKETS_PER_LINE = 10;

type
  /// <summary>
  ///   A rule of the site was broken. The message is written for the person -
  ///   or the agent - who has to put it right.
  /// </summary>
  EDelphiDay = class(Exception);

  TCartLine = record
    TicketId: string;
    Quantity: Integer;
  end;

  /// <summary>
  ///   Amounts are in cents.
  /// </summary>
  TCartTotals = record
    Subtotal: Integer;
    Discount: Integer;
    Total: Integer;
    DiscountPercent: Integer;
  end;

  /// <summary>
  ///   What a visitor has, copied out under the lock so a caller can read it
  ///   at leisure.
  /// </summary>
  TVisitorState = record
    Schedule: TArray<Integer>;
    Cart: TArray<TCartLine>;
    DiscountCode: string;
    HasPending: Boolean;
    PendingName: string;
    PendingEmail: string;
    Orders: TArray<string>;
  end;

  /// <summary>
  ///   A visitor of the site: the MCP session, with what they have chosen.
  ///   Registered with ISessionConfig.SetSessionClass, so the session manager
  ///   creates it, the tools receive it through [Context], and it is freed
  ///   with the session. Its fields are read and written under the service's
  ///   lock: a page and a tool call can use the same session at once.
  /// </summary>
  TDelphiDaySession = class(TMCPSessionBase)
  public
    Schedule: TList<Integer>;
    Cart: TDictionary<string, Integer>;
    DiscountCode: string;
    HasPending: Boolean;
    PendingName: string;
    PendingEmail: string;
    Orders: TList<string>;

    constructor Create;
    destructor Destroy; override;
  end;

  TDelphiDayService = class
  private
    FLock: TCriticalSection;
    FCatalog: TCatalog;

    /// <summary>
    ///   The session a change is made to. A change needs one: the web module
    ///   creates it before a form is handled, and the server before a tool runs.
    /// </summary>
    function Need(AVisitor: TDelphiDaySession): TDelphiDaySession;
    function TotalsOf(AVisitor: TDelphiDaySession): TCartTotals;
    function DiscountPercentOf(const ACode: string): Integer;
    procedure InvalidatePending(AVisitor: TDelphiDaySession);
    function SortedSchedule(AVisitor: TDelphiDaySession): TArray<Integer>;
    function CoversSeminars(AVisitor: TDelphiDaySession): Boolean;
    function CoversConference(AVisitor: TDelphiDaySession): Boolean;
  public
    constructor Create(const ADataFile: string);
    destructor Destroy; override;

    function Snapshot(AVisitor: TDelphiDaySession): TVisitorState;

    // --- the program -------------------------------------------------------

    function SearchSpeakers(const AQuery, ATopic: string): TArray<TSpeaker>;
    function SearchSessions(const AQuery, ATrack, ALevel: string;
      ADay: Integer): TArray<TSession>;

    /// <summary>
    ///   Every distinct track and level, for the filters.
    /// </summary>
    function Tracks: TArray<string>;
    function Levels: TArray<string>;

    // --- the schedule ------------------------------------------------------

    /// <summary>
    ///   Adds a session. Refused when it overlaps one already there.
    /// </summary>
    function AddToSchedule(AVisitor: TDelphiDaySession; ASessionId: Integer): string;
    function RemoveFromSchedule(AVisitor: TDelphiDaySession; ASessionId: Integer): string;

    /// <summary>
    ///   Which tickets the schedule needs, and whether the cart has them.
    /// </summary>
    function TicketAdvice(AVisitor: TDelphiDaySession): string;

    // --- tickets and the cart ----------------------------------------------

    function SeatsLeft(const ATicketId: string): Integer;
    function AddToCart(AVisitor: TDelphiDaySession; const ATicketId: string; AQuantity: Integer): string;
    function RemoveFromCart(AVisitor: TDelphiDaySession; const ATicketId: string): string;
    function ApplyDiscount(AVisitor: TDelphiDaySession; const ACode: string): string;
    function Totals(AVisitor: TDelphiDaySession): TCartTotals;

    // --- the order ---------------------------------------------------------

    /// <summary>
    ///   Prepares the order. Nothing is bought: the visitor has to confirm it
    ///   on the page, which is the one step an agent cannot take.
    /// </summary>
    function PlaceOrder(AVisitor: TDelphiDaySession; const AName, AEmail: string): string;
    function ConfirmOrder(AVisitor: TDelphiDaySession): string;
    function CancelPending(AVisitor: TDelphiDaySession): string;

    property Catalog: TCatalog read FCatalog;
  end;

/// <summary>
///   Cents as 12.50.
/// </summary>
function CentsText(ACents: Integer): string;

var
  /// <summary>
  ///   The one service of the process, created before the socket opens.
  /// </summary>
  DelphiDayService: TDelphiDayService;

implementation

function CentsText(ACents: Integer): string;
begin
  Result := Format('%d.%.2d', [ACents div 100, ACents mod 100]);
end;

{ TDelphiDaySession }

constructor TDelphiDaySession.Create;
begin
  inherited Create;
  Schedule := TList<Integer>.Create;
  Cart := TDictionary<string, Integer>.Create;
  Orders := TList<string>.Create;
end;

destructor TDelphiDaySession.Destroy;
begin
  Orders.Free;
  Cart.Free;
  Schedule.Free;
  inherited;
end;

{ TDelphiDayService }

constructor TDelphiDayService.Create(const ADataFile: string);
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FCatalog := TCatalog.Create;
  FCatalog.LoadFromFile(ADataFile);
end;

destructor TDelphiDayService.Destroy;
begin
  FCatalog.Free;
  FLock.Free;
  inherited;
end;

function TDelphiDayService.Need(AVisitor: TDelphiDaySession): TDelphiDaySession;
begin
  if AVisitor = nil then
    raise EDelphiDay.Create('There is no visitor session for this change');
  Result := AVisitor;
end;

function TDelphiDayService.SortedSchedule(AVisitor: TDelphiDaySession): TArray<Integer>;
var
  LList: TList<Integer>;
begin
  LList := TList<Integer>.Create(AVisitor.Schedule);
  try
    LList.Sort(TComparer<Integer>.Construct(
      function (const ALeft, ARight: Integer): Integer
      var
        LA, LB: TSession;
      begin
        LA := FCatalog.FindSession(ALeft);
        LB := FCatalog.FindSession(ARight);
        Result := LA.Day - LB.Day;
        if Result = 0 then
          Result := LA.StartMinute - LB.StartMinute;
        if Result = 0 then
          Result := CompareText(LA.Room, LB.Room);
      end));
    Result := LList.ToArray;
  finally
    LList.Free;
  end;
end;

function TDelphiDayService.Snapshot(AVisitor: TDelphiDaySession): TVisitorState;
var
  LVisitor: TDelphiDaySession;
  LTicket: TTicketType;
  LLine: TCartLine;
  LQuantity: Integer;
  LLines: TList<TCartLine>;
begin
  Result := Default(TVisitorState);
  // A page seen before any change: no session yet, nothing chosen
  if AVisitor = nil then
    Exit;

  FLock.Enter;
  try
    LVisitor := Need(AVisitor);
    Result.Schedule := SortedSchedule(LVisitor);
    Result.DiscountCode := LVisitor.DiscountCode;
    Result.HasPending := LVisitor.HasPending;
    Result.PendingName := LVisitor.PendingName;
    Result.PendingEmail := LVisitor.PendingEmail;
    Result.Orders := LVisitor.Orders.ToArray;

    // In the order of the ticket list, not the order they were added
    LLines := TList<TCartLine>.Create;
    try
      for LTicket in FCatalog.Tickets do
        if LVisitor.Cart.TryGetValue(LTicket.Id, LQuantity) then
        begin
          LLine.TicketId := LTicket.Id;
          LLine.Quantity := LQuantity;
          LLines.Add(LLine);
        end;
      Result.Cart := LLines.ToArray;
    finally
      LLines.Free;
    end;
  finally
    FLock.Leave;
  end;
end;

// --- the program -------------------------------------------------------------

function TDelphiDayService.SearchSpeakers(const AQuery, ATopic: string): TArray<TSpeaker>;
var
  LSpeaker: TSpeaker;
  LSession: TSession;
  LList: TList<TSpeaker>;
  LQuery, LTopic: string;
  LMatch: Boolean;
begin
  LQuery := AQuery.Trim.ToLower;
  LTopic := ATopic.Trim.ToLower;
  LList := TList<TSpeaker>.Create;
  try
    for LSpeaker in FCatalog.Speakers do
    begin
      if (LTopic <> '') and not LSpeaker.Topics.ToLower.Contains(LTopic) then
        Continue;

      LMatch := (LQuery = '') or LSpeaker.Name.ToLower.Contains(LQuery) or
        LSpeaker.Company.ToLower.Contains(LQuery) or LSpeaker.Topics.ToLower.Contains(LQuery);
      if not LMatch then
        for LSession in FCatalog.Sessions do
          if SameText(LSession.SpeakerId, LSpeaker.Id) and
            (LSession.Title.ToLower.Contains(LQuery) or LSession.Summary.ToLower.Contains(LQuery)) then
          begin
            LMatch := True;
            Break;
          end;

      if LMatch then
        LList.Add(LSpeaker);
    end;
    Result := LList.ToArray;
  finally
    LList.Free;
  end;
end;

function TDelphiDayService.SearchSessions(const AQuery, ATrack, ALevel: string;
  ADay: Integer): TArray<TSession>;
var
  LSession: TSession;
  LSpeaker: TSpeaker;
  LList: TList<TSession>;
  LQuery, LTrack, LLevel, LSpeakerName: string;
begin
  LQuery := AQuery.Trim.ToLower;
  LTrack := ATrack.Trim.ToLower;
  LLevel := ALevel.Trim.ToLower;
  LList := TList<TSession>.Create;
  try
    for LSession in FCatalog.Sessions do
    begin
      if (ADay <> 0) and (LSession.Day <> ADay) then
        Continue;
      if (LTrack <> '') and not LSession.Track.ToLower.Contains(LTrack) then
        Continue;
      if (LLevel <> '') and (LSession.Level.ToLower <> LLevel) then
        Continue;

      if LQuery <> '' then
      begin
        LSpeaker := FCatalog.FindSpeaker(LSession.SpeakerId);
        if Assigned(LSpeaker) then
          LSpeakerName := LSpeaker.Name.ToLower
        else
          LSpeakerName := '';
        if not (LSession.Title.ToLower.Contains(LQuery) or
          LSession.Summary.ToLower.Contains(LQuery) or
          LSession.Track.ToLower.Contains(LQuery) or
          LSpeakerName.Contains(LQuery)) then
          Continue;
      end;
      LList.Add(LSession);
    end;

    LList.Sort(TComparer<TSession>.Construct(
      function (const ALeft, ARight: TSession): Integer
      begin
        Result := ALeft.Day - ARight.Day;
        if Result = 0 then
          Result := ALeft.StartMinute - ARight.StartMinute;
        if Result = 0 then
          Result := CompareText(ALeft.Room, ARight.Room);
      end));
    Result := LList.ToArray;
  finally
    LList.Free;
  end;
end;

function TDelphiDayService.Tracks: TArray<string>;
var
  LSession: TSession;
  LList: TStringList;
begin
  LList := TStringList.Create;
  try
    LList.Sorted := True;
    LList.Duplicates := dupIgnore;
    for LSession in FCatalog.Sessions do
      LList.Add(LSession.Track);
    Result := LList.ToStringArray;
  finally
    LList.Free;
  end;
end;

function TDelphiDayService.Levels: TArray<string>;
begin
  Result := ['Beginner', 'Intermediate', 'Advanced'];
end;

// --- the schedule ------------------------------------------------------------

function TDelphiDayService.AddToSchedule(AVisitor: TDelphiDaySession; ASessionId: Integer): string;
var
  LVisitor: TDelphiDaySession;
  LSession, LOther: TSession;
  LId: Integer;
begin
  LSession := FCatalog.FindSession(ASessionId);
  if LSession = nil then
    raise EDelphiDay.CreateFmt('There is no session %d', [ASessionId]);

  FLock.Enter;
  try
    LVisitor := Need(AVisitor);
    if LVisitor.Schedule.Contains(ASessionId) then
      raise EDelphiDay.CreateFmt('Session %d is already in the schedule', [ASessionId]);

    for LId in LVisitor.Schedule do
    begin
      LOther := FCatalog.FindSession(LId);
      if LSession.Overlaps(LOther) then
        raise EDelphiDay.CreateFmt(
          'Session %d (%s, %s) overlaps session %d "%s" (%s), which is already in the schedule. ' +
          'Remove that one first, or pick another session.',
          [LSession.Id, FCatalog.DayName(LSession.Day), LSession.TimeText,
           LOther.Id, LOther.Title, LOther.TimeText]);
    end;

    LVisitor.Schedule.Add(ASessionId);
    Result := Format('Added session %d "%s" (%s, %s, %s) to the schedule',
      [LSession.Id, LSession.Title, FCatalog.DayName(LSession.Day), LSession.TimeText, LSession.Room]);
  finally
    FLock.Leave;
  end;
end;

function TDelphiDayService.RemoveFromSchedule(AVisitor: TDelphiDaySession; ASessionId: Integer): string;
var
  LSession: TSession;
begin
  LSession := FCatalog.FindSession(ASessionId);
  FLock.Enter;
  try
    if (LSession = nil) or (Need(AVisitor).Schedule.Remove(ASessionId) < 0) then
      raise EDelphiDay.CreateFmt('Session %d is not in the schedule', [ASessionId]);
    Result := Format('Removed session %d "%s" from the schedule', [LSession.Id, LSession.Title]);
  finally
    FLock.Leave;
  end;
end;

function TDelphiDayService.CoversSeminars(AVisitor: TDelphiDaySession): Boolean;
var
  LPair: TPair<string, Integer>;
  LTicket: TTicketType;
begin
  for LPair in AVisitor.Cart do
  begin
    LTicket := FCatalog.FindTicket(LPair.Key);
    if Assigned(LTicket) and LTicket.CoversSeminars then
      Exit(True);
  end;
  Result := False;
end;

function TDelphiDayService.CoversConference(AVisitor: TDelphiDaySession): Boolean;
var
  LPair: TPair<string, Integer>;
  LTicket: TTicketType;
begin
  for LPair in AVisitor.Cart do
  begin
    LTicket := FCatalog.FindTicket(LPair.Key);
    if Assigned(LTicket) and LTicket.CoversConference then
      Exit(True);
  end;
  Result := False;
end;

function TDelphiDayService.TicketAdvice(AVisitor: TDelphiDaySession): string;
var
  LVisitor: TDelphiDaySession;
  LId: Integer;
  LNeedSeminars, LNeedConference, LCovered: Boolean;
begin
  if AVisitor = nil then
    Exit('The schedule is empty, so no ticket is needed yet.');

  FLock.Enter;
  try
    LVisitor := Need(AVisitor);
    LNeedSeminars := False;
    LNeedConference := False;
    for LId in LVisitor.Schedule do
      if FCatalog.FindSession(LId).Kind = TSessionKind.Seminar then
        LNeedSeminars := True
      else
        LNeedConference := True;

    if not (LNeedSeminars or LNeedConference) then
      Exit('The schedule is empty, so no ticket is needed yet.');

    LCovered := (not LNeedSeminars or CoversSeminars(LVisitor)) and
      (not LNeedConference or CoversConference(LVisitor));

    if LNeedSeminars and LNeedConference then
      Result := 'The schedule has seminars (Tuesday) and conference talks (Wednesday): ' +
        'a Combo ticket covers both.'
    else if LNeedSeminars then
      Result := 'The schedule has seminars (Tuesday): a Seminar day ticket covers it.'
    else
      Result := 'The schedule has conference talks (Wednesday): a Conference day ticket covers it, ' +
        'or a Student ticket for students.';

    if LCovered then
      Result := Result + ' The cart already has a ticket that covers it.'
    else
      Result := Result + ' The cart does not cover it yet.';
  finally
    FLock.Leave;
  end;
end;

// --- tickets and the cart ----------------------------------------------------

function TDelphiDayService.SeatsLeft(const ATicketId: string): Integer;
var
  LTicket: TTicketType;
begin
  LTicket := FCatalog.FindTicket(ATicketId);
  if LTicket = nil then
    Exit(0);
  FLock.Enter;
  try
    Result := LTicket.SeatsLeft;
  finally
    FLock.Leave;
  end;
end;

procedure TDelphiDayService.InvalidatePending(AVisitor: TDelphiDaySession);
begin
  // The order was prepared for a cart that is no longer this one
  AVisitor.HasPending := False;
end;

function TDelphiDayService.AddToCart(AVisitor: TDelphiDaySession; const ATicketId: string; AQuantity: Integer): string;
var
  LVisitor: TDelphiDaySession;
  LTicket: TTicketType;
  LInCart: Integer;
begin
  LTicket := FCatalog.FindTicket(ATicketId);
  if LTicket = nil then
    raise EDelphiDay.CreateFmt('Unknown ticket type "%s". The types are: seminar, conference, combo, student',
      [ATicketId]);
  if (AQuantity < 1) or (AQuantity > MAX_TICKETS_PER_LINE) then
    raise EDelphiDay.CreateFmt('The quantity must be between 1 and %d', [MAX_TICKETS_PER_LINE]);

  FLock.Enter;
  try
    LVisitor := Need(AVisitor);
    if not LVisitor.Cart.TryGetValue(LTicket.Id, LInCart) then
      LInCart := 0;

    if LTicket.SeatsLeft = 0 then
      raise EDelphiDay.CreateFmt('%s is sold out', [LTicket.Name]);
    if LInCart + AQuantity > LTicket.SeatsLeft then
      raise EDelphiDay.CreateFmt('Only %d seat(s) of %s are left, and %d are already in the cart',
        [LTicket.SeatsLeft, LTicket.Name, LInCart]);
    if LInCart + AQuantity > MAX_TICKETS_PER_LINE then
      raise EDelphiDay.CreateFmt('At most %d tickets of one type per order', [MAX_TICKETS_PER_LINE]);

    LVisitor.Cart.AddOrSetValue(LTicket.Id, LInCart + AQuantity);
    InvalidatePending(LVisitor);
    Result := Format('Added %d x %s (EUR %d each) to the cart. %d now in the cart.',
      [AQuantity, LTicket.Name, LTicket.Price, LInCart + AQuantity]);
  finally
    FLock.Leave;
  end;
end;

function TDelphiDayService.RemoveFromCart(AVisitor: TDelphiDaySession; const ATicketId: string): string;
var
  LVisitor: TDelphiDaySession;
  LTicket: TTicketType;
begin
  LTicket := FCatalog.FindTicket(ATicketId);
  FLock.Enter;
  try
    LVisitor := Need(AVisitor);
    if (LTicket = nil) or not LVisitor.Cart.ContainsKey(LTicket.Id) then
      raise EDelphiDay.CreateFmt('There is no "%s" ticket in the cart', [ATicketId]);
    LVisitor.Cart.Remove(LTicket.Id);
    InvalidatePending(LVisitor);
    Result := Format('Removed %s from the cart', [LTicket.Name]);
  finally
    FLock.Leave;
  end;
end;

function TDelphiDayService.DiscountPercentOf(const ACode: string): Integer;
begin
  if SameText(ACode, 'EARLYBIRD') then
    Result := 15
  else if SameText(ACode, 'MCPCONNECT') then
    Result := 10
  else
    Result := 0;
end;

function TDelphiDayService.ApplyDiscount(AVisitor: TDelphiDaySession; const ACode: string): string;
var
  LVisitor: TDelphiDaySession;
  LPercent: Integer;
begin
  LPercent := DiscountPercentOf(ACode.Trim);
  if LPercent = 0 then
    raise EDelphiDay.CreateFmt('"%s" is not a valid discount code', [ACode.Trim]);

  FLock.Enter;
  try
    LVisitor := Need(AVisitor);
    LVisitor.DiscountCode := ACode.Trim.ToUpper;
    InvalidatePending(LVisitor);
    Result := Format('Discount code %s applied: %d%% off', [LVisitor.DiscountCode, LPercent]);
  finally
    FLock.Leave;
  end;
end;

function TDelphiDayService.TotalsOf(AVisitor: TDelphiDaySession): TCartTotals;
var
  LPair: TPair<string, Integer>;
  LTicket: TTicketType;
begin
  Result.Subtotal := 0;
  for LPair in AVisitor.Cart do
  begin
    LTicket := FCatalog.FindTicket(LPair.Key);
    if Assigned(LTicket) then
      Inc(Result.Subtotal, LTicket.Price * 100 * LPair.Value);
  end;
  Result.DiscountPercent := DiscountPercentOf(AVisitor.DiscountCode);
  // Whole euros times a whole percent is whole cents: no rounding to explain
  Result.Discount := (Result.Subtotal div 100) * Result.DiscountPercent;
  Result.Total := Result.Subtotal - Result.Discount;
end;

function TDelphiDayService.Totals(AVisitor: TDelphiDaySession): TCartTotals;
begin
  Result := Default(TCartTotals);
  if AVisitor = nil then
    Exit;

  FLock.Enter;
  try
    Result := TotalsOf(AVisitor);
  finally
    FLock.Leave;
  end;
end;

// --- the order ---------------------------------------------------------------

function TDelphiDayService.PlaceOrder(AVisitor: TDelphiDaySession; const AName, AEmail: string): string;
var
  LVisitor: TDelphiDaySession;
  LPair: TPair<string, Integer>;
  LTicket: TTicketType;
  LTotals: TCartTotals;
  LEmail: string;
begin
  LEmail := AEmail.Trim;
  if AName.Trim.IsEmpty then
    raise EDelphiDay.Create('The attendee name is required');
  if (LEmail.IndexOf('@') < 1) or (LEmail.LastIndexOf('.') < LEmail.IndexOf('@')) then
    raise EDelphiDay.Create('The attendee e-mail address is not valid');

  FLock.Enter;
  try
    LVisitor := Need(AVisitor);
    if LVisitor.Cart.Count = 0 then
      raise EDelphiDay.Create('The cart is empty: add a ticket first');

    for LPair in LVisitor.Cart do
    begin
      LTicket := FCatalog.FindTicket(LPair.Key);
      if LTicket.SeatsLeft < LPair.Value then
        raise EDelphiDay.CreateFmt('%s: only %d seat(s) left, the cart has %d',
          [LTicket.Name, LTicket.SeatsLeft, LPair.Value]);
    end;

    LVisitor.HasPending := True;
    LVisitor.PendingName := AName.Trim;
    LVisitor.PendingEmail := LEmail;
    LTotals := TotalsOf(LVisitor);
    Result := Format('The order for %s is ready: total EUR %s. NOTHING HAS BEEN BOUGHT YET. ' +
      'Only the person at the keyboard can complete it: ask them to open the Checkout page and ' +
      'press "Confirm purchase".', [LVisitor.PendingName, CentsText(LTotals.Total)]);
  finally
    FLock.Leave;
  end;
end;

function TDelphiDayService.ConfirmOrder(AVisitor: TDelphiDaySession): string;
var
  LVisitor: TDelphiDaySession;
  LPair: TPair<string, Integer>;
  LTicket: TTicketType;
  LTotals: TCartTotals;
  LItems, LCode: string;
  LQuantity: Integer;
begin
  FLock.Enter;
  try
    LVisitor := Need(AVisitor);
    if not LVisitor.HasPending then
      raise EDelphiDay.Create('There is no order waiting for confirmation');

    // Seats may have gone while the order waited
    for LPair in LVisitor.Cart do
    begin
      LTicket := FCatalog.FindTicket(LPair.Key);
      if LTicket.SeatsLeft < LPair.Value then
      begin
        LVisitor.HasPending := False;
        raise EDelphiDay.CreateFmt('%s: only %d seat(s) are left now. Review the cart.',
          [LTicket.Name, LTicket.SeatsLeft]);
      end;
    end;

    LTotals := TotalsOf(LVisitor);
    LItems := '';
    for LTicket in FCatalog.Tickets do
      if LVisitor.Cart.TryGetValue(LTicket.Id, LQuantity) then
      begin
        LTicket.Sold := LTicket.Sold + LQuantity;
        if LItems <> '' then
          LItems := LItems + ', ';
        LItems := LItems + Format('%d x %s', [LQuantity, LTicket.Name]);
      end;

    LCode := 'DD27-' + IntToHex(Random($10000), 4);
    LVisitor.Orders.Add(Format('%s - %s for %s, EUR %s', [LCode, LItems, LVisitor.PendingName,
      CentsText(LTotals.Total)]));

    LVisitor.Cart.Clear;
    LVisitor.DiscountCode := '';
    LVisitor.HasPending := False;
    Result := LCode;
  finally
    FLock.Leave;
  end;
end;

function TDelphiDayService.CancelPending(AVisitor: TDelphiDaySession): string;
begin
  FLock.Enter;
  try
    Need(AVisitor).HasPending := False;
    Result := 'The order was cancelled; the cart is unchanged';
  finally
    FLock.Leave;
  end;
end;

initialization
  Randomize;

end.
