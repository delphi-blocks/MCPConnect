unit DelphiDay.Data;

{$SCOPEDENUMS ON}

{
  The catalog of the site: speakers, sessions and ticket types.

  It is read once from data\delphiday.json (UTF-8, so the names keep their
  accents) and is immutable afterwards, with one exception: the number of
  tickets sold, which TDelphiDayService changes under its own lock. Everything
  that belongs to a visitor - the schedule, the cart - lives in the service,
  not here.
}

interface

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.JSON,
  System.Generics.Collections;

type
  TSessionKind = (Seminar, Talk, Keynote);

  TSpeaker = class
  strict private
    FId: string;
    FName: string;
    FCompany: string;
    FLanguage: string;
    FTopics: string;
  public
    constructor Create(const AId, AName, ACompany, ALanguage: string);

    property Id: string read FId;
    property Name: string read FName;
    property Company: string read FCompany;

    /// <summary>
    ///   The language the speaker presents in, as it or en. The descriptions
    ///   on the site are in English whatever it is.
    /// </summary>
    property Language: string read FLanguage;

    /// <summary>
    ///   The tracks the speaker's sessions belong to, comma separated. Filled
    ///   in once the sessions are loaded.
    /// </summary>
    property Topics: string read FTopics write FTopics;
  end;

  TSession = class
  strict private
    FId: Integer;
    FTitle: string;
    FSummary: string;
    FSpeakerId: string;
    FKind: TSessionKind;
    FDay: Integer;
    FStartMinute: Integer;
    FEndMinute: Integer;
    FRoom: string;
    FTrack: string;
    FLevel: string;
  public
    constructor Create(AId: Integer; const ATitle, ASummary, ASpeakerId: string;
      AKind: TSessionKind; ADay, AStartMinute, AEndMinute: Integer;
      const ARoom, ATrack, ALevel: string);

    /// <summary>
    ///   The times as 09:30 - 13:00.
    /// </summary>
    function TimeText: string;

    /// <summary>
    ///   Whether the two sessions are on the same day and share some minutes.
    /// </summary>
    function Overlaps(AOther: TSession): Boolean;

    property Id: Integer read FId;
    property Title: string read FTitle;
    property Summary: string read FSummary;
    property SpeakerId: string read FSpeakerId;
    property Kind: TSessionKind read FKind;
    property Day: Integer read FDay;
    property StartMinute: Integer read FStartMinute;
    property EndMinute: Integer read FEndMinute;
    property Room: string read FRoom;
    property Track: string read FTrack;
    property Level: string read FLevel;
  end;

  TTicketType = class
  strict private
    FId: string;
    FName: string;
    FDescription: string;
    FPrice: Integer;
    FCapacity: Integer;
    FSold: Integer;
    FCoversSeminars: Boolean;
    FCoversConference: Boolean;
  public
    constructor Create(const AId, AName, ADescription: string;
      APrice, ACapacity, ASold: Integer; ACoversSeminars, ACoversConference: Boolean);

    function SeatsLeft: Integer;

    property Id: string read FId;
    property Name: string read FName;
    property Description: string read FDescription;

    /// <summary>
    ///   In whole euros.
    /// </summary>
    property Price: Integer read FPrice;
    property Capacity: Integer read FCapacity;
    property Sold: Integer read FSold write FSold;
    property CoversSeminars: Boolean read FCoversSeminars;
    property CoversConference: Boolean read FCoversConference;
  end;

  TCatalog = class
  strict private
    FEventName: string;
    FCity: string;
    FDays: TDictionary<Integer, string>;
    FSpeakers: TObjectList<TSpeaker>;
    FSessions: TObjectList<TSession>;
    FTickets: TObjectList<TTicketType>;
    procedure LinkTopics;
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadFromFile(const AFileName: string);

    function FindSpeaker(const AId: string): TSpeaker;
    function FindSession(AId: Integer): TSession;
    function FindTicket(const AId: string): TTicketType;
    function DayName(ADay: Integer): string;

    property EventName: string read FEventName;
    property City: string read FCity;
    property Speakers: TObjectList<TSpeaker> read FSpeakers;
    property Sessions: TObjectList<TSession> read FSessions;
    property Tickets: TObjectList<TTicketType> read FTickets;
  end;

/// <summary>
///   The name of a language code: it is Italian, anything else English.
/// </summary>
function LanguageName(const ACode: string): string;

/// <summary>
///   09:30 to 570, and back.
/// </summary>
function ParseClock(const AText: string): Integer;
function ClockText(AMinutes: Integer): string;

implementation

function ParseClock(const AText: string): Integer;
var
  LColon: Integer;
begin
  LColon := AText.IndexOf(':');
  if LColon < 0 then
    raise EConvertError.CreateFmt('"%s" is not a time of day (HH:MM)', [AText]);
  Result := StrToInt(AText.Substring(0, LColon)) * 60 + StrToInt(AText.Substring(LColon + 1));
end;

function LanguageName(const ACode: string): string;
begin
  if SameText(ACode, 'it') then
    Result := 'Italian'
  else
    Result := 'English';
end;

function ClockText(AMinutes: Integer): string;
begin
  Result := Format('%.2d:%.2d', [AMinutes div 60, AMinutes mod 60]);
end;

{ TSpeaker }

constructor TSpeaker.Create(const AId, AName, ACompany, ALanguage: string);
begin
  inherited Create;
  FId := AId;
  FName := AName;
  FCompany := ACompany;
  FLanguage := ALanguage;
end;

{ TSession }

constructor TSession.Create(AId: Integer; const ATitle, ASummary, ASpeakerId: string;
  AKind: TSessionKind; ADay, AStartMinute, AEndMinute: Integer;
  const ARoom, ATrack, ALevel: string);
begin
  inherited Create;
  FId := AId;
  FTitle := ATitle;
  FSummary := ASummary;
  FSpeakerId := ASpeakerId;
  FKind := AKind;
  FDay := ADay;
  FStartMinute := AStartMinute;
  FEndMinute := AEndMinute;
  FRoom := ARoom;
  FTrack := ATrack;
  FLevel := ALevel;
end;

function TSession.TimeText: string;
begin
  Result := ClockText(FStartMinute) + ' - ' + ClockText(FEndMinute);
end;

function TSession.Overlaps(AOther: TSession): Boolean;
begin
  Result := (FDay = AOther.Day) and (FStartMinute < AOther.EndMinute) and
    (AOther.StartMinute < FEndMinute);
end;

{ TTicketType }

constructor TTicketType.Create(const AId, AName, ADescription: string;
  APrice, ACapacity, ASold: Integer; ACoversSeminars, ACoversConference: Boolean);
begin
  inherited Create;
  FId := AId;
  FName := AName;
  FDescription := ADescription;
  FPrice := APrice;
  FCapacity := ACapacity;
  FSold := ASold;
  FCoversSeminars := ACoversSeminars;
  FCoversConference := ACoversConference;
end;

function TTicketType.SeatsLeft: Integer;
begin
  Result := FCapacity - FSold;
  if Result < 0 then
    Result := 0;
end;

{ TCatalog }

constructor TCatalog.Create;
begin
  inherited Create;
  FDays := TDictionary<Integer, string>.Create;
  FSpeakers := TObjectList<TSpeaker>.Create(True);
  FSessions := TObjectList<TSession>.Create(True);
  FTickets := TObjectList<TTicketType>.Create(True);
end;

destructor TCatalog.Destroy;
begin
  FTickets.Free;
  FSessions.Free;
  FSpeakers.Free;
  FDays.Free;
  inherited;
end;

procedure TCatalog.LoadFromFile(const AFileName: string);
var
  LRoot, LObject, LDays: TJSONObject;
  LItem: TJSONValue;
  LPair: TJSONPair;
  LKind: string;
  LSessionKind: TSessionKind;
begin
  LRoot := TJSONObject.ParseJSONValue(TFile.ReadAllText(AFileName, TEncoding.UTF8)) as TJSONObject;
  if LRoot = nil then
    raise EJSONException.CreateFmt('%s is not a JSON object', [AFileName]);
  try
    LObject := LRoot.GetValue<TJSONObject>('event');
    FEventName := LObject.GetValue<string>('name');
    FCity := LObject.GetValue<string>('city');
    LDays := LObject.GetValue<TJSONObject>('days');
    for LPair in LDays do
      FDays.AddOrSetValue(StrToInt(LPair.JsonString.Value), LPair.JsonValue.Value);

    for LItem in LRoot.GetValue<TJSONArray>('speakers') do
    begin
      LObject := LItem as TJSONObject;
      FSpeakers.Add(TSpeaker.Create(
        LObject.GetValue<string>('id'),
        LObject.GetValue<string>('name'),
        LObject.GetValue<string>('company'),
        LObject.GetValue<string>('language')));
    end;

    for LItem in LRoot.GetValue<TJSONArray>('sessions') do
    begin
      LObject := LItem as TJSONObject;
      LKind := LObject.GetValue<string>('kind');
      if SameText(LKind, 'seminar') then
        LSessionKind := TSessionKind.Seminar
      else if SameText(LKind, 'keynote') then
        LSessionKind := TSessionKind.Keynote
      else
        LSessionKind := TSessionKind.Talk;

      FSessions.Add(TSession.Create(
        LObject.GetValue<Integer>('id'),
        LObject.GetValue<string>('title'),
        LObject.GetValue<string>('summary'),
        LObject.GetValue<string>('speaker'),
        LSessionKind,
        LObject.GetValue<Integer>('day'),
        ParseClock(LObject.GetValue<string>('start')),
        ParseClock(LObject.GetValue<string>('end')),
        LObject.GetValue<string>('room'),
        LObject.GetValue<string>('track'),
        LObject.GetValue<string>('level')));
    end;

    for LItem in LRoot.GetValue<TJSONArray>('tickets') do
    begin
      LObject := LItem as TJSONObject;
      FTickets.Add(TTicketType.Create(
        LObject.GetValue<string>('id'),
        LObject.GetValue<string>('name'),
        LObject.GetValue<string>('description'),
        LObject.GetValue<Integer>('price'),
        LObject.GetValue<Integer>('capacity'),
        LObject.GetValue<Integer>('sold'),
        LObject.GetValue<Boolean>('seminars'),
        LObject.GetValue<Boolean>('conference')));
    end;
  finally
    LRoot.Free;
  end;

  LinkTopics;
end;

procedure TCatalog.LinkTopics;
var
  LSpeaker: TSpeaker;
  LSession: TSession;
  LTopics: TStringList;
begin
  LTopics := TStringList.Create;
  try
    LTopics.Sorted := True;
    LTopics.Duplicates := dupIgnore;
    for LSpeaker in FSpeakers do
    begin
      LTopics.Clear;
      for LSession in FSessions do
        if SameText(LSession.SpeakerId, LSpeaker.Id) then
          LTopics.Add(LSession.Track);
      LTopics.Delimiter := ',';
      LTopics.StrictDelimiter := True;
      LSpeaker.Topics := LTopics.DelimitedText.Replace(',', ', ');
    end;
  finally
    LTopics.Free;
  end;
end;

function TCatalog.FindSpeaker(const AId: string): TSpeaker;
var
  LSpeaker: TSpeaker;
begin
  for LSpeaker in FSpeakers do
    if SameText(LSpeaker.Id, AId) then
      Exit(LSpeaker);
  Result := nil;
end;

function TCatalog.FindSession(AId: Integer): TSession;
var
  LSession: TSession;
begin
  for LSession in FSessions do
    if LSession.Id = AId then
      Exit(LSession);
  Result := nil;
end;

function TCatalog.FindTicket(const AId: string): TTicketType;
var
  LTicket: TTicketType;
begin
  for LTicket in FTickets do
    if SameText(LTicket.Id, AId) then
      Exit(LTicket);
  Result := nil;
end;

function TCatalog.DayName(ADay: Integer): string;
begin
  if not FDays.TryGetValue(ADay, Result) then
    Result := 'Day ' + ADay.ToString;
end;

end.
