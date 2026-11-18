unit DelphiDay.View;

{
  What the templates read

  WebStencils reads plain objects through RTTI, so the pages never see the
  catalog or the service: BuildPageModel copies what one request needs into
  these classes, as text and flags the templates print or test. A model is
  built per request and handed to the template processor, which frees it.
}

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,

  DelphiDay.Data,
  DelphiDay.Service;

type
  TLineRow = class
  strict private
    FText: string;
  public
    constructor Create(const AText: string);
    property Text: string read FText;
  end;

  TOptionRow = class
  strict private
    FValue: string;
    FText: string;
    FSelected: Boolean;
  public
    constructor Create(const AValue, AText: string; ASelected: Boolean);
    property Value: string read FValue;
    property Text: string read FText;
    property Selected: Boolean read FSelected;
  end;

  TSessionRow = class
  strict private
    FId: Integer;
    FTitle: string;
    FSummary: string;
    FSpeakerName: string;
    FDayName: string;
    FTimeText: string;
    FRoom: string;
    FTrack: string;
    FLevel: string;
    FKindText: string;
    FLanguageName: string;
    FFlagFile: string;
    FIsSeminar: Boolean;
    FInSchedule: Boolean;
    FNewDay: Boolean;
  public
    constructor Create(ASession: TSession; ACatalog: TCatalog; AInSchedule, ANewDay: Boolean);
    property Id: Integer read FId;
    property Title: string read FTitle;
    property Summary: string read FSummary;
    property SpeakerName: string read FSpeakerName;
    property DayName: string read FDayName;
    property TimeText: string read FTimeText;
    property Room: string read FRoom;
    property Track: string read FTrack;
    property Level: string read FLevel;
    property KindText: string read FKindText;

    /// <summary>
    ///   The language the session is given in - its speaker's - and the flag
    ///   file under /assets that stands for it.
    /// </summary>
    property LanguageName: string read FLanguageName;
    property FlagFile: string read FFlagFile;
    property IsSeminar: Boolean read FIsSeminar;
    property InSchedule: Boolean read FInSchedule;

    /// <summary>
    ///   The first session of its day in the list: the page puts a heading
    ///   above it.
    /// </summary>
    property NewDay: Boolean read FNewDay;
  end;

  TSpeakerRow = class
  strict private
    FName: string;
    FCompany: string;
    FHasCompany: Boolean;
    FTopics: string;
    FInitials: string;
    FLanguageName: string;
    FFlagFile: string;
    FSessions: TObjectList<TLineRow>;
  public
    constructor Create(ASpeaker: TSpeaker; ACatalog: TCatalog);
    destructor Destroy; override;
    property Name: string read FName;
    property Company: string read FCompany;
    property HasCompany: Boolean read FHasCompany;
    property Topics: string read FTopics;
    property Initials: string read FInitials;
    property LanguageName: string read FLanguageName;
    property FlagFile: string read FFlagFile;
    property Sessions: TObjectList<TLineRow> read FSessions;
  end;

  TTicketRow = class
  strict private
    FId: string;
    FName: string;
    FDescription: string;
    FPrice: string;
    FSeatsLeft: Integer;
    FSoldOut: Boolean;
    FLowStock: Boolean;
  public
    constructor Create(ATicket: TTicketType; ASeatsLeft: Integer);
    property Id: string read FId;
    property Name: string read FName;
    property Description: string read FDescription;
    property Price: string read FPrice;
    property SeatsLeft: Integer read FSeatsLeft;
    property SoldOut: Boolean read FSoldOut;
    property LowStock: Boolean read FLowStock;
  end;

  TCartRow = class
  strict private
    FTicketId: string;
    FName: string;
    FQuantity: Integer;
    FUnitPrice: string;
    FLineTotal: string;
  public
    constructor Create(ATicket: TTicketType; AQuantity: Integer);
    property TicketId: string read FTicketId;
    property Name: string read FName;
    property Quantity: Integer read FQuantity;
    property UnitPrice: string read FUnitPrice;
    property LineTotal: string read FLineTotal;
  end;

  TPageModel = class
  strict private
    FPage: string;
    FEventName: string;
    FCity: string;
    FMessage: string;
    FHasMessage: Boolean;
    FIsError: Boolean;
    FBack: string;

    FSpeakerCount: Integer;
    FSessionTotal: Integer;
    FScheduleCount: Integer;
    FCartCount: Integer;

    FQuery: string;
    FTrackOptions: TObjectList<TOptionRow>;
    FLevelOptions: TObjectList<TOptionRow>;
    FDayOptions: TObjectList<TOptionRow>;

    FSpeakers: TObjectList<TSpeakerRow>;
    FSessions: TObjectList<TSessionRow>;
    FSchedule: TObjectList<TSessionRow>;
    FTicketAdvice: string;
    FTickets: TObjectList<TTicketRow>;
    FCart: TObjectList<TCartRow>;
    FOrders: TObjectList<TLineRow>;

    FSubtotal: string;
    FDiscount: string;
    FTotal: string;
    FHasDiscount: Boolean;
    FDiscountText: string;
    FHasPending: Boolean;
    FPendingName: string;
    FPendingEmail: string;
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    ///   The header of the page: where it is, the event, the message shown
    ///   above it, and the counters of the navigation bar.
    /// </summary>
    procedure Fill(const APage, AEventName, ACity, AMessage: string; AIsError: Boolean;
      const AQuery: string; ASpeakerCount, ASessionTotal, AScheduleCount: Integer);
    procedure SetAdvice(const AAdvice: string);
    procedure FillCart(ACartCount: Integer; const ASubtotal, ADiscount, ATotal: string;
      AHasDiscount: Boolean; const ADiscountText: string; AHasPending: Boolean;
      const APendingName, APendingEmail: string);

    function GetHasSessions: Boolean;
    function GetHasSpeakers: Boolean;
    function GetHasSchedule: Boolean;
    function GetHasCart: Boolean;
    function GetHasOrders: Boolean;

    property Page: string read FPage;

    /// <summary>
    ///   Where the forms of this page send the visitor back to: its own path
    ///   and filters, without the message.
    /// </summary>
    property Back: string read FBack write FBack;
    property EventName: string read FEventName;
    property City: string read FCity;
    property Message: string read FMessage;
    property HasMessage: Boolean read FHasMessage;
    property IsError: Boolean read FIsError;

    property SpeakerCount: Integer read FSpeakerCount;
    property SessionTotal: Integer read FSessionTotal;
    property ScheduleCount: Integer read FScheduleCount;
    property CartCount: Integer read FCartCount;

    property Query: string read FQuery;
    property TrackOptions: TObjectList<TOptionRow> read FTrackOptions;
    property LevelOptions: TObjectList<TOptionRow> read FLevelOptions;
    property DayOptions: TObjectList<TOptionRow> read FDayOptions;

    property Speakers: TObjectList<TSpeakerRow> read FSpeakers;
    property HasSpeakers: Boolean read GetHasSpeakers;
    property Sessions: TObjectList<TSessionRow> read FSessions;
    property HasSessions: Boolean read GetHasSessions;
    property Schedule: TObjectList<TSessionRow> read FSchedule;
    property HasSchedule: Boolean read GetHasSchedule;
    property TicketAdvice: string read FTicketAdvice;
    property Tickets: TObjectList<TTicketRow> read FTickets;
    property Cart: TObjectList<TCartRow> read FCart;
    property HasCart: Boolean read GetHasCart;
    property Orders: TObjectList<TLineRow> read FOrders;
    property HasOrders: Boolean read GetHasOrders;

    property Subtotal: string read FSubtotal;
    property Discount: string read FDiscount;
    property Total: string read FTotal;
    property HasDiscount: Boolean read FHasDiscount;
    property DiscountText: string read FDiscountText;
    property HasPending: Boolean read FHasPending;
    property PendingName: string read FPendingName;
    property PendingEmail: string read FPendingEmail;
  end;

/// <summary>
///   The model of one page, for one visitor - nil before their first change,
///   which reads as an empty schedule and cart. AFilters carries the query string
///   of the request (q, track, level, day); AMessage is shown above the page.
/// </summary>
function BuildPageModel(const APage: string; AVisitor: TDelphiDaySession; AFilters: TStrings;
  const AMessage: string; AIsError: Boolean): TPageModel;

implementation

function FlagFileOf(const ALanguage: string): string;
begin
  if SameText(ALanguage, 'it') then
    Result := 'flag-italy.png'
  else
    Result := 'flag-uk.png';
end;

{ TLineRow }

constructor TLineRow.Create(const AText: string);
begin
  inherited Create;
  FText := AText;
end;

{ TOptionRow }

constructor TOptionRow.Create(const AValue, AText: string; ASelected: Boolean);
begin
  inherited Create;
  FValue := AValue;
  FText := AText;
  FSelected := ASelected;
end;

{ TSessionRow }

constructor TSessionRow.Create(ASession: TSession; ACatalog: TCatalog; AInSchedule, ANewDay: Boolean);
var
  LSpeaker: TSpeaker;
begin
  inherited Create;
  FId := ASession.Id;
  FTitle := ASession.Title;
  FSummary := ASession.Summary;
  LSpeaker := ACatalog.FindSpeaker(ASession.SpeakerId);
  if Assigned(LSpeaker) then
  begin
    FSpeakerName := LSpeaker.Name;
    FLanguageName := DelphiDay.Data.LanguageName(LSpeaker.Language);
    FFlagFile := FlagFileOf(LSpeaker.Language);
  end;
  FDayName := ACatalog.DayName(ASession.Day);
  FTimeText := ASession.TimeText;
  FRoom := ASession.Room;
  FTrack := ASession.Track;
  FLevel := ASession.Level;
  FIsSeminar := ASession.Kind = TSessionKind.Seminar;
  case ASession.Kind of
    TSessionKind.Seminar: FKindText := 'Seminar';
    TSessionKind.Keynote: FKindText := 'Keynote';
  else
    FKindText := 'Talk';
  end;
  FInSchedule := AInSchedule;
  FNewDay := ANewDay;
end;

{ TSpeakerRow }

constructor TSpeakerRow.Create(ASpeaker: TSpeaker; ACatalog: TCatalog);
var
  LSession: TSession;
  LParts: TArray<string>;
begin
  inherited Create;
  FName := ASpeaker.Name;
  FCompany := ASpeaker.Company;
  FHasCompany := ASpeaker.Company <> '';
  FTopics := ASpeaker.Topics;
  FLanguageName := DelphiDay.Data.LanguageName(ASpeaker.Language);
  FFlagFile := FlagFileOf(ASpeaker.Language);

  // The first letter of the first and the last word: a stand-in for a photo
  LParts := ASpeaker.Name.Split([' ']);
  if Length(LParts) > 1 then
    FInitials := Copy(LParts[0], 1, 1) + Copy(LParts[High(LParts)], 1, 1)
  else
    FInitials := Copy(ASpeaker.Name, 1, 1);

  FSessions := TObjectList<TLineRow>.Create(True);
  for LSession in ACatalog.Sessions do
    if SameText(LSession.SpeakerId, ASpeaker.Id) then
      FSessions.Add(TLineRow.Create(Format('#%d %s - %s %s', [LSession.Id, LSession.Title,
        ACatalog.DayName(LSession.Day), LSession.TimeText])));
end;

destructor TSpeakerRow.Destroy;
begin
  FSessions.Free;
  inherited;
end;

{ TTicketRow }

constructor TTicketRow.Create(ATicket: TTicketType; ASeatsLeft: Integer);
begin
  inherited Create;
  FId := ATicket.Id;
  FName := ATicket.Name;
  FDescription := ATicket.Description;
  FPrice := CentsText(ATicket.Price * 100);
  FSeatsLeft := ASeatsLeft;
  FSoldOut := ASeatsLeft = 0;
  FLowStock := (ASeatsLeft > 0) and (ASeatsLeft <= 5);
end;

{ TCartRow }

constructor TCartRow.Create(ATicket: TTicketType; AQuantity: Integer);
begin
  inherited Create;
  FTicketId := ATicket.Id;
  FName := ATicket.Name;
  FQuantity := AQuantity;
  FUnitPrice := CentsText(ATicket.Price * 100);
  FLineTotal := CentsText(ATicket.Price * 100 * AQuantity);
end;

{ TPageModel }

constructor TPageModel.Create;
begin
  inherited Create;
  FTrackOptions := TObjectList<TOptionRow>.Create(True);
  FLevelOptions := TObjectList<TOptionRow>.Create(True);
  FDayOptions := TObjectList<TOptionRow>.Create(True);
  FSpeakers := TObjectList<TSpeakerRow>.Create(True);
  FSessions := TObjectList<TSessionRow>.Create(True);
  FSchedule := TObjectList<TSessionRow>.Create(True);
  FTickets := TObjectList<TTicketRow>.Create(True);
  FCart := TObjectList<TCartRow>.Create(True);
  FOrders := TObjectList<TLineRow>.Create(True);
end;

destructor TPageModel.Destroy;
begin
  FOrders.Free;
  FCart.Free;
  FTickets.Free;
  FSchedule.Free;
  FSessions.Free;
  FSpeakers.Free;
  FDayOptions.Free;
  FLevelOptions.Free;
  FTrackOptions.Free;
  inherited;
end;

procedure TPageModel.Fill(const APage, AEventName, ACity, AMessage: string; AIsError: Boolean;
  const AQuery: string; ASpeakerCount, ASessionTotal, AScheduleCount: Integer);
begin
  FPage := APage;
  FEventName := AEventName;
  FCity := ACity;
  FMessage := AMessage;
  FHasMessage := AMessage <> '';
  FIsError := AIsError;
  FQuery := AQuery;
  FSpeakerCount := ASpeakerCount;
  FSessionTotal := ASessionTotal;
  FScheduleCount := AScheduleCount;
end;

procedure TPageModel.SetAdvice(const AAdvice: string);
begin
  FTicketAdvice := AAdvice;
end;

procedure TPageModel.FillCart(ACartCount: Integer; const ASubtotal, ADiscount, ATotal: string;
  AHasDiscount: Boolean; const ADiscountText: string; AHasPending: Boolean;
  const APendingName, APendingEmail: string);
begin
  FCartCount := ACartCount;
  FSubtotal := ASubtotal;
  FDiscount := ADiscount;
  FTotal := ATotal;
  FHasDiscount := AHasDiscount;
  FDiscountText := ADiscountText;
  FHasPending := AHasPending;
  FPendingName := APendingName;
  FPendingEmail := APendingEmail;
end;

function TPageModel.GetHasSessions: Boolean;
begin
  Result := FSessions.Count > 0;
end;

function TPageModel.GetHasSpeakers: Boolean;
begin
  Result := FSpeakers.Count > 0;
end;

function TPageModel.GetHasSchedule: Boolean;
begin
  Result := FSchedule.Count > 0;
end;

function TPageModel.GetHasCart: Boolean;
begin
  Result := FCart.Count > 0;
end;

function TPageModel.GetHasOrders: Boolean;
begin
  Result := FOrders.Count > 0;
end;

{ BuildPageModel }

function IsScheduled(const AState: TVisitorState; ASessionId: Integer): Boolean;
var
  LId: Integer;
begin
  for LId in AState.Schedule do
    if LId = ASessionId then
      Exit(True);
  Result := False;
end;

function BuildPageModel(const APage: string; AVisitor: TDelphiDaySession; AFilters: TStrings;
  const AMessage: string; AIsError: Boolean): TPageModel;
var
  LService: TDelphiDayService;
  LCatalog: TCatalog;
  LState: TVisitorState;
  LTotals: TCartTotals;
  LQuery, LTrack, LLevel, LDayText: string;
  LDay: Integer;
  LSession: TSession;
  LSpeaker: TSpeaker;
  LTicket: TTicketType;
  LLine: TCartLine;
  LTrackName, LLevelName, LOrder: string;
  LLastDay: Integer;
begin
  LService := DelphiDayService;
  LCatalog := LService.Catalog;
  Result := TPageModel.Create;
  try
    LQuery := AFilters.Values['q'].Trim;
    LTrack := AFilters.Values['track'].Trim;
    LLevel := AFilters.Values['level'].Trim;
    LDayText := AFilters.Values['day'].Trim;
    LDay := StrToIntDef(LDayText, 0);

    LState := LService.Snapshot(AVisitor);
    LTotals := LService.Totals(AVisitor);

    Result.Fill(APage, LCatalog.EventName, LCatalog.City, AMessage, AIsError, LQuery,
      LCatalog.Speakers.Count, LCatalog.Sessions.Count, Length(LState.Schedule));

    // --- filters ---------------------------------------------------------
    Result.TrackOptions.Add(TOptionRow.Create('', 'All tracks', LTrack = ''));
    for LTrackName in LService.Tracks do
      Result.TrackOptions.Add(TOptionRow.Create(LTrackName, LTrackName, SameText(LTrackName, LTrack)));
    Result.LevelOptions.Add(TOptionRow.Create('', 'All levels', LLevel = ''));
    for LLevelName in LService.Levels do
      Result.LevelOptions.Add(TOptionRow.Create(LLevelName, LLevelName, SameText(LLevelName, LLevel)));
    Result.DayOptions.Add(TOptionRow.Create('', 'Both days', LDay = 0));
    Result.DayOptions.Add(TOptionRow.Create('1', LCatalog.DayName(1) + ' - seminars', LDay = 1));
    Result.DayOptions.Add(TOptionRow.Create('2', LCatalog.DayName(2) + ' - conference', LDay = 2));

    // --- speakers --------------------------------------------------------
    for LSpeaker in LService.SearchSpeakers(LQuery, '') do
      Result.Speakers.Add(TSpeakerRow.Create(LSpeaker, LCatalog));

    // --- sessions --------------------------------------------------------
    LLastDay := 0;
    for LSession in LService.SearchSessions(LQuery, LTrack, LLevel, LDay) do
    begin
      Result.Sessions.Add(TSessionRow.Create(LSession, LCatalog,
        IsScheduled(LState, LSession.Id), LSession.Day <> LLastDay));
      LLastDay := LSession.Day;
    end;

    // --- the schedule ----------------------------------------------------
    LLastDay := 0;
    for var LId in LState.Schedule do
    begin
      LSession := LCatalog.FindSession(LId);
      Result.Schedule.Add(TSessionRow.Create(LSession, LCatalog, True, LSession.Day <> LLastDay));
      LLastDay := LSession.Day;
    end;
    Result.SetAdvice(LService.TicketAdvice(AVisitor));

    // --- tickets and cart ------------------------------------------------
    for LTicket in LCatalog.Tickets do
      Result.Tickets.Add(TTicketRow.Create(LTicket, LService.SeatsLeft(LTicket.Id)));

    var LCount := 0;
    for LLine in LState.Cart do
    begin
      Result.Cart.Add(TCartRow.Create(LCatalog.FindTicket(LLine.TicketId), LLine.Quantity));
      Inc(LCount, LLine.Quantity);
    end;

    Result.FillCart(LCount, CentsText(LTotals.Subtotal), CentsText(LTotals.Discount),
      CentsText(LTotals.Total), LTotals.DiscountPercent > 0,
      Format('%s (%d%% off)', [LState.DiscountCode, LTotals.DiscountPercent]),
      LState.HasPending, LState.PendingName, LState.PendingEmail);

    for LOrder in LState.Orders do
      Result.Orders.Add(TLineRow.Create(LOrder));
  except
    Result.Free;
    raise;
  end;
end;

end.
