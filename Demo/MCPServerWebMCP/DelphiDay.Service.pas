unit DelphiDay.Service;

{
  The business rules of the site, in one place

  The HTML forms (DelphiDay.WebModule) and the MCP tools (DelphiDay.Tools) both
  call this class and nothing else, so a visitor and a browser agent acting for
  them are held to the same rules: the same conflict check on the schedule, the
  same seat limits, the same discount codes.

  A visitor is a random id carried in a cookie. Their schedule, cart and
  pending order live here, in memory, and are gone when the process stops -
  this is a demo, and nothing is stored or charged.

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

  DelphiDay.Data;

const
  /// <summary>
  ///   The cookie that carries the visitor's id on the pages and, because the
  ///   WebMCP script calls /mcp from the page with same-origin credentials, on
  ///   every tool call.
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

  TDelphiDayService = class
  private
    type
      TVisitor = class
      public
        Schedule: TList<Integer>;
        Cart: TDictionary<string, Integer>;
        DiscountCode: string;
        HasPending: Boolean;
        PendingName: string;
        PendingEmail: string;
        Orders: TList<string>;
        LastSeen: TDateTime;

        constructor Create;
        destructor Destroy; override;
      end;
  private
    FLock: TCriticalSection;
    FCatalog: TCatalog;
    FVisitors: TObjectDictionary<string, TVisitor>;

    function Visitor(const AId: string): TVisitor;
    procedure TrimVisitors;
    function TotalsOf(AVisitor: TVisitor): TCartTotals;
    function DiscountPercentOf(const ACode: string): Integer;
    procedure InvalidatePending(AVisitor: TVisitor);
    function SortedSchedule(AVisitor: TVisitor): TArray<Integer>;
    function CoversSeminars(AVisitor: TVisitor): Boolean;
    function CoversConference(AVisitor: TVisitor): Boolean;
  public
    constructor Create(const ADataFile: string);
    destructor Destroy; override;

    /// <summary>
    ///   Whether AId has the shape of an id this service hands out.
    /// </summary>
    class function IsValidVisitorId(const AId: string): Boolean; static;

    /// <summary>
    ///   A new visitor, with an empty schedule and cart. Returns their id.
    /// </summary>
    function NewVisitor: string;

    /// <summary>
    ///   Makes sure AId has a state: a cookie can outlive a restart.
    /// </summary>
    procedure EnsureVisitor(const AId: string);

    function Snapshot(const AVisitorId: string): TVisitorState;

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
    function AddToSchedule(const AVisitorId: string; ASessionId: Integer): string;
    function RemoveFromSchedule(const AVisitorId: string; ASessionId: Integer): string;

    /// <summary>
    ///   Which tickets the schedule needs, and whether the cart has them.
    /// </summary>
    function TicketAdvice(const AVisitorId: string): string;

    // --- tickets and the cart ----------------------------------------------

    function SeatsLeft(const ATicketId: string): Integer;
    function AddToCart(const AVisitorId, ATicketId: string; AQuantity: Integer): string;
    function RemoveFromCart(const AVisitorId, ATicketId: string): string;
    function ApplyDiscount(const AVisitorId, ACode: string): string;
    function Totals(const AVisitorId: string): TCartTotals;

    // --- the order ---------------------------------------------------------

    /// <summary>
    ///   Prepares the order. Nothing is bought: the visitor has to confirm it
    ///   on the page, which is the one step an agent cannot take.
    /// </summary>
    function PlaceOrder(const AVisitorId, AName, AEmail: string): string;
    function ConfirmOrder(const AVisitorId: string): string;
    function CancelPending(const AVisitorId: string): string;

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

const
  MAX_VISITORS = 500;

function CentsText(ACents: Integer): string;
begin
  Result := Format('%d.%.2d', [ACents div 100, ACents mod 100]);
end;

{ TDelphiDayService.TVisitor }

constructor TDelphiDayService.TVisitor.Create;
begin
  inherited Create;
  Schedule := TList<Integer>.Create;
  Cart := TDictionary<string, Integer>.Create;
  Orders := TList<string>.Create;
  LastSeen := Now;
end;

destructor TDelphiDayService.TVisitor.Destroy;
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
  FVisitors := TObjectDictionary<string, TVisitor>.Create([doOwnsValues]);
end;

destructor TDelphiDayService.Destroy;
begin
  FVisitors.Free;
  FCatalog.Free;
  FLock.Free;
  inherited;
end;

class function TDelphiDayService.IsValidVisitorId(const AId: string): Boolean;
var
  LChar: Char;
begin
  Result := AId.Length = 32;
  if Result then
    for LChar in AId do
      if not CharInSet(LChar, ['0'..'9', 'a'..'f']) then
        Exit(False);
end;

function TDelphiDayService.Visitor(const AId: string): TVisitor;
begin
  // Called with the lock held
  if not FVisitors.TryGetValue(AId, Result) then
  begin
    TrimVisitors;
    Result := TVisitor.Create;
    FVisitors.Add(AId, Result);
  end;
  Result.LastSeen := Now;
end;

procedure TDelphiDayService.TrimVisitors;
var
  LPair: TPair<string, TVisitor>;
  LOldest: string;
  LOldestSeen: TDateTime;
begin
  // A demo on the open internet must not grow without bound
  if FVisitors.Count < MAX_VISITORS then
    Exit;

  LOldest := '';
  LOldestSeen := MaxDateTime;
  for LPair in FVisitors do
    if LPair.Value.LastSeen < LOldestSeen then
    begin
      LOldestSeen := LPair.Value.LastSeen;
      LOldest := LPair.Key;
    end;
  if LOldest <> '' then
    FVisitors.Remove(LOldest);
end;

function TDelphiDayService.NewVisitor: string;
begin
  Result := LowerCase(TGUID.NewGuid.ToString.Replace('{', '').Replace('}', '').Replace('-', ''));
  EnsureVisitor(Result);
end;

procedure TDelphiDayService.EnsureVisitor(const AId: string);
begin
  FLock.Enter;
  try
    Visitor(AId);
  finally
    FLock.Leave;
  end;
end;

function TDelphiDayService.SortedSchedule(AVisitor: TVisitor): TArray<Integer>;
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

function TDelphiDayService.Snapshot(const AVisitorId: string): TVisitorState;
var
  LVisitor: TVisitor;
  LTicket: TTicketType;
  LLine: TCartLine;
  LQuantity: Integer;
  LLines: TList<TCartLine>;
begin
  FLock.Enter;
  try
    LVisitor := Visitor(AVisitorId);
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

function TDelphiDayService.AddToSchedule(const AVisitorId: string; ASessionId: Integer): string;
var
  LVisitor: TVisitor;
  LSession, LOther: TSession;
  LId: Integer;
begin
  LSession := FCatalog.FindSession(ASessionId);
  if LSession = nil then
    raise EDelphiDay.CreateFmt('There is no session %d', [ASessionId]);

  FLock.Enter;
  try
    LVisitor := Visitor(AVisitorId);
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

function TDelphiDayService.RemoveFromSchedule(const AVisitorId: string; ASessionId: Integer): string;
var
  LSession: TSession;
begin
  LSession := FCatalog.FindSession(ASessionId);
  FLock.Enter;
  try
    if (LSession = nil) or (Visitor(AVisitorId).Schedule.Remove(ASessionId) < 0) then
      raise EDelphiDay.CreateFmt('Session %d is not in the schedule', [ASessionId]);
    Result := Format('Removed session %d "%s" from the schedule', [LSession.Id, LSession.Title]);
  finally
    FLock.Leave;
  end;
end;

function TDelphiDayService.CoversSeminars(AVisitor: TVisitor): Boolean;
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

function TDelphiDayService.CoversConference(AVisitor: TVisitor): Boolean;
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

function TDelphiDayService.TicketAdvice(const AVisitorId: string): string;
var
  LVisitor: TVisitor;
  LId: Integer;
  LNeedSeminars, LNeedConference, LCovered: Boolean;
begin
  FLock.Enter;
  try
    LVisitor := Visitor(AVisitorId);
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

procedure TDelphiDayService.InvalidatePending(AVisitor: TVisitor);
begin
  // The order was prepared for a cart that is no longer this one
  AVisitor.HasPending := False;
end;

function TDelphiDayService.AddToCart(const AVisitorId, ATicketId: string; AQuantity: Integer): string;
var
  LVisitor: TVisitor;
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
    LVisitor := Visitor(AVisitorId);
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

function TDelphiDayService.RemoveFromCart(const AVisitorId, ATicketId: string): string;
var
  LVisitor: TVisitor;
  LTicket: TTicketType;
begin
  LTicket := FCatalog.FindTicket(ATicketId);
  FLock.Enter;
  try
    LVisitor := Visitor(AVisitorId);
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

function TDelphiDayService.ApplyDiscount(const AVisitorId, ACode: string): string;
var
  LVisitor: TVisitor;
  LPercent: Integer;
begin
  LPercent := DiscountPercentOf(ACode.Trim);
  if LPercent = 0 then
    raise EDelphiDay.CreateFmt('"%s" is not a valid discount code', [ACode.Trim]);

  FLock.Enter;
  try
    LVisitor := Visitor(AVisitorId);
    LVisitor.DiscountCode := ACode.Trim.ToUpper;
    InvalidatePending(LVisitor);
    Result := Format('Discount code %s applied: %d%% off', [LVisitor.DiscountCode, LPercent]);
  finally
    FLock.Leave;
  end;
end;

function TDelphiDayService.TotalsOf(AVisitor: TVisitor): TCartTotals;
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

function TDelphiDayService.Totals(const AVisitorId: string): TCartTotals;
begin
  FLock.Enter;
  try
    Result := TotalsOf(Visitor(AVisitorId));
  finally
    FLock.Leave;
  end;
end;

// --- the order ---------------------------------------------------------------

function TDelphiDayService.PlaceOrder(const AVisitorId, AName, AEmail: string): string;
var
  LVisitor: TVisitor;
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
    LVisitor := Visitor(AVisitorId);
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

function TDelphiDayService.ConfirmOrder(const AVisitorId: string): string;
var
  LVisitor: TVisitor;
  LPair: TPair<string, Integer>;
  LTicket: TTicketType;
  LTotals: TCartTotals;
  LItems, LCode: string;
  LQuantity: Integer;
begin
  FLock.Enter;
  try
    LVisitor := Visitor(AVisitorId);
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

function TDelphiDayService.CancelPending(const AVisitorId: string): string;
begin
  FLock.Enter;
  try
    Visitor(AVisitorId).HasPending := False;
    Result := 'The order was cancelled; the cart is unchanged';
  finally
    FLock.Leave;
  end;
end;

initialization
  Randomize;

end.
