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

/// <summary>
///   In-memory Logify adapter.
///
///   Instead of writing the log out (console, file, syslog) it keeps it inside
///   the process, and turns the [PERF] lines the library emits along the whole
///   call chain into structured, aggregated measurements.
///
///   [PERF] lines are logged at Debug level, so the adapter has to be
///   registered with a level of Debug or Trace to see any of them.
/// </summary>
unit MCPConnect.Logging.Memory;

interface

uses
  System.SysUtils, System.Classes, System.Math, System.DateUtils, System.SyncObjs,
  System.Generics.Collections, System.Generics.Defaults,

  Logify;

type
  /// <summary>
  ///   Fixed size FIFO ring: once full, every new item overwrites the oldest
  ///   one. A capacity of zero accepts (and discards) everything, which is how
  ///   a store is told not to keep a given kind of entry at all.
  /// </summary>
  TMCPLogRing<T> = class
  private
    FItems: TArray<T>;
    FHead: Integer;
    FCount: Integer;
    function GetCapacity: Integer; inline;
  public
    constructor Create(ACapacity: Integer);

    procedure Add(const AItem: T);
    procedure Clear;

    /// <summary>Oldest first.</summary>
    function ToArray: TArray<T>;

    property Count: Integer read FCount;
    property Capacity: Integer read GetCapacity;
  end;

  /// <summary>
  ///   One captured log line, before any formatting.
  /// </summary>
  TMCPLogEntry = record
    Timestamp: TDateTime;
    ThreadId: TThreadID;
    /// <summary>Qualified name of the logging class, empty for the global logger.</summary>
    LogClass: string;
    Level: TLogLevel;
    Text: string;
    /// <summary>
    ///   Exception rendered by the adapter's TLoggerFormatter: class, message,
    ///   stack trace and the nested inner chain; empty when there was none.
    /// </summary>
    ExceptionInfo: string;

    /// <summary>
    ///   AFormatter is the formatter the capturing adapter uses (its Formatter
    ///   property), so an exception is rendered the way that adapter would,
    ///   custom formatter included. Nil falls back to a plain TLoggerFormatter.
    /// </summary>
    class function New(const ALogClass, AText: string; AException: Exception;
      ALevel: TLogLevel; AFormatter: TLoggerFormatter = nil): TMCPLogEntry; static;
    function ToString: string;
  end;

  /// <summary>
  ///   A single [PERF] measurement, parsed out of a log line.
  ///
  ///   The lines look like "[PERF] &lt;scope&gt; [&lt;target&gt;] &lt;phase&gt;: &lt;n&gt; ms",
  ///   with the target and the phase both optional:
  ///   <code>
  ///   [PERF] Tool [get_weather] Method.Invoke (business logic): 42 ms
  ///   [PERF] JRPC ProcessRequest total: 51 ms
  ///   [PERF] Transport [POST /mcp] total: 55 ms (HTTP: 200)
  ///   </code>
  /// </summary>
  TMCPPerfSample = record
  public const
    PERF_PREFIX = '[PERF]';
  public
    /// <summary>Filled in by the store, not by the parser.</summary>
    Timestamp: TDateTime;
    /// <summary>Filled in by the store, not by the parser.</summary>
    ThreadId: TThreadID;
    /// <summary>The whole label, target included: what measurements are aggregated by.</summary>
    Key: string;
    /// <summary>First word of the label: Tool, Resource, Prompt, JRPC, CallTool, POST...</summary>
    Scope: string;
    /// <summary>What was measured, when the line names one in brackets: a tool name, a URI.</summary>
    Target: string;
    /// <summary>The step inside the scope: total, Method.Invoke (business logic)...</summary>
    Phase: string;
    ElapsedMs: Int64;
    /// <summary>Whatever trailed the duration, e.g. "(HTTP: 200)".</summary>
    Detail: string;
    /// <summary>The [PERF] line as it was logged.</summary>
    Text: string;

    /// <summary>
    ///   True when the message is a [PERF] line that carries a duration.
    ///   Timestamp and ThreadId are left at zero: only the caller knows them.
    /// </summary>
    class function TryParse(const AMessage: string; out ASample: TMCPPerfSample): Boolean; static;
    class function IsPerfLine(const AMessage: string): Boolean; static;

    function ToString: string;
  end;

  /// <summary>
  ///   Everything observed for one key, folded into a single record.
  /// </summary>
  TMCPPerfStat = record
    Key: string;
    Scope: string;
    Target: string;
    Phase: string;
    Count: Int64;
    TotalMs: Int64;
    MinMs: Int64;
    MaxMs: Int64;
    LastMs: Int64;
    FirstSeen: TDateTime;
    LastSeen: TDateTime;

    class function New(const ASample: TMCPPerfSample): TMCPPerfStat; static;
    procedure Update(const ASample: TMCPPerfSample);
    function AverageMs: Double;
    function ToString: string;
  end;

  TMCPPerfSampleProc = reference to procedure(const ASample: TMCPPerfSample);

  /// <summary>
  ///   Thread safe, bounded store for what the memory adapter captures.
  ///
  ///   Every [PERF] line becomes a TMCPPerfSample kept in a ring buffer, and is
  ///   folded into the per-key statistics, which survive the ring rolling over.
  ///   Other log lines are only kept when the store was built with a log
  ///   capacity.
  ///
  ///   A store handed to a factory must outlive the adapter built from it:
  ///   unregister the factory before freeing it.
  /// </summary>
  TMCPMemoryLog = class
  public const
    DEFAULT_PERF_CAPACITY = 1000;
    DEFAULT_MAX_KEYS = 1024;
    /// <summary>Unique name of the factory registered by Install.</summary>
    ADAPTER_NAME = 'MCPConnect.Memory';
  private class var
    FDefault: TMCPMemoryLog;
  private
    FLock: TCriticalSection;
    FPerfSamples: TMCPLogRing<TMCPPerfSample>;
    FLogEntries: TMCPLogRing<TMCPLogEntry>;
    FStats: TDictionary<string, TMCPPerfStat>;
    FMaxKeys: Integer;
    FPerfCount: Int64;
    FLogCount: Int64;
    FDroppedKeys: Int64;
    FOnPerfSample: TMCPPerfSampleProc;
    function GetOnPerfSample: TMCPPerfSampleProc;
    procedure SetOnPerfSample(const AValue: TMCPPerfSampleProc);
    function GetDroppedKeys: Int64;
    function GetLogCount: Int64;
    function GetPerfCount: Int64;
    // The caller must already hold FLock
    procedure UpdateStat(const ASample: TMCPPerfSample);
  public
    /// <summary>Perf samples only: raw log lines are discarded.</summary>
    constructor Create; overload;
    /// <summary>
    ///   APerfCapacity samples and ALogCapacity raw log lines are kept, the
    ///   oldest dropping out first. Zero disables that ring without disabling
    ///   the statistics, which are always collected.
    /// </summary>
    constructor Create(APerfCapacity, ALogCapacity: Integer); overload;
    destructor Destroy; override;

    /// <summary>
    ///   Process-wide store, created on first use and freed with the unit.
    /// </summary>
    class function Default: TMCPMemoryLog; static;

    /// <summary>
    ///   Registers a memory adapter over the default store and returns it.
    ///   Calling it twice is not an error: the second call is ignored and the
    ///   already collected measurements are kept.
    /// </summary>
    class function Install(ALevel: TLogLevel = TLogLevel.Debug): TMCPMemoryLog; static;
    /// <summary>
    ///   Detaches the adapter registered by Install. The store and what it
    ///   holds stay around, so a report can still be read out.
    /// </summary>
    class procedure Uninstall; static;

    /// <summary>Called by the adapter for every line that passed the level filter.</summary>
    procedure Capture(const AEntry: TMCPLogEntry);

    procedure Clear;

    /// <summary>Oldest first.</summary>
    function PerfSamples: TArray<TMCPPerfSample>;
    /// <summary>Slowest (by total time) first.</summary>
    function PerfStats: TArray<TMCPPerfStat>;
    function TryGetStat(const AKey: string; out AStat: TMCPPerfStat): Boolean;
    /// <summary>Oldest first; always empty unless the store keeps log lines.</summary>
    function LogEntries: TArray<TMCPLogEntry>;

    /// <summary>Human readable table of PerfStats.</summary>
    function PerfReport: string;

    /// <summary>
    ///   Called for every [PERF] line, outside the store's lock, on the thread
    ///   that logged it. Meant for pushing measurements somewhere else
    ///   (metrics, a UI); keep it short.
    /// </summary>
    property OnPerfSample: TMCPPerfSampleProc read GetOnPerfSample write SetOnPerfSample;

    /// <summary>Samples seen since the last Clear, including those the ring dropped.</summary>
    property PerfCount: Int64 read GetPerfCount;
    /// <summary>Log lines seen since the last Clear, including those not kept.</summary>
    property LogCount: Int64 read GetLogCount;
    /// <summary>
    ///   Samples whose key was new when the statistics were already holding
    ///   MaxKeys of them. Anything above zero means the report is incomplete.
    /// </summary>
    property DroppedKeys: Int64 read GetDroppedKeys;
    /// <summary>
    ///   How many distinct keys the statistics may hold. Bounded on purpose:
    ///   keys carry tool names and request URLs.
    /// </summary>
    property MaxKeys: Integer read FMaxKeys write FMaxKeys;
  end;

  /// <summary>
  ///   Adapter class for the Logify framework: keeps the log in a
  ///   TMCPMemoryLog instead of writing it out.
  /// </summary>
  TLogifyAdapterMemory = class(TLoggerAdapterHelper, ILoggerAdapter)
  private
    FStore: TMCPMemoryLog;
  protected
    procedure InternalLog(const AMessage, AClassName: string; AException: Exception; ALevel: TLogLevel); override;
    procedure InternalRaw(const AMessage: string; ALevel: TLogLevel); override;
  public
    constructor Create(AStore: TMCPMemoryLog; ALevel: TLogLevel); overload;

    // Non-interface methods
    procedure SetStore(AStore: TMCPMemoryLog);

    property Store: TMCPMemoryLog read FStore;
  end;

  /// <summary>
  ///   AdapterFactory class for the Logify framework
  /// </summary>
  TLogifyAdapterMemoryFactory = class(TLoggerAdapterFactory)
  private
    FLevel: TLogLevel;
    FStore: TMCPMemoryLog;
  public
    /// <summary>Writes into TMCPMemoryLog.Default.</summary>
    class function CreateAdapterFactory(ALevel: TLogLevel): TLogifyAdapterMemoryFactory; overload;
    class function CreateAdapterFactory(ALevel: TLogLevel; AStore: TMCPMemoryLog): TLogifyAdapterMemoryFactory; overload;
    class function CreateAdapterFactory(const AName: string; ALevel: TLogLevel; AStore: TMCPMemoryLog): TLogifyAdapterMemoryFactory; overload;
  public
    function CreateLoggerAdapter: ILoggerAdapter; override;

    property Level: TLogLevel read FLevel write FLevel;
    property Store: TMCPMemoryLog read FStore write FStore;
  end;

implementation

var
  /// <summary>
  ///   Set at the beginning of the unit finalization: the default store is
  ///   gone from that moment on, so an adapter the Logify registry still holds
  ///   must stop touching it.
  /// </summary>
  _Shutdown: Boolean;

/// <summary>
///   Reads " 42 ms" / " {42} ms" starting at AStart. AStop comes back pointing
///   just past the unit, so the caller can pick up whatever trails it.
/// </summary>
function TryReadElapsed(const AText: string; AStart: Integer; out AValue: Int64;
  out AStop: Integer): Boolean;
const
  // Enough for any duration; the guard is against a runaway digit run
  MAX_DIGITS = 18;
var
  LPos, LDigits: Integer;
  LBraced: Boolean;
begin
  Result := False;
  AValue := 0;
  AStop := AStart;

  LPos := AStart;
  while (LPos < AText.Length) and (AText.Chars[LPos] = ' ') do
    Inc(LPos);

  LBraced := (LPos < AText.Length) and (AText.Chars[LPos] = '{');
  if LBraced then
    Inc(LPos);

  LDigits := 0;
  while (LPos < AText.Length) and CharInSet(AText.Chars[LPos], ['0'..'9']) do
  begin
    if LDigits < MAX_DIGITS then
      AValue := AValue * 10 + (Ord(AText.Chars[LPos]) - Ord('0'));
    Inc(LDigits);
    Inc(LPos);
  end;
  if LDigits = 0 then
    Exit;

  if LBraced then
  begin
    if (LPos >= AText.Length) or (AText.Chars[LPos] <> '}') then
      Exit;
    Inc(LPos);
  end;

  while (LPos < AText.Length) and (AText.Chars[LPos] = ' ') do
    Inc(LPos);

  if LPos + 2 > AText.Length then
    Exit;
  if not (CharInSet(AText.Chars[LPos], ['m', 'M']) and CharInSet(AText.Chars[LPos + 1], ['s', 'S'])) then
    Exit;
  Inc(LPos, 2);

  // "msec", "msg"...: the unit has to end where it ends
  if (LPos < AText.Length) and CharInSet(AText.Chars[LPos], ['a'..'z', 'A'..'Z', '0'..'9']) then
    Exit;

  AStop := LPos;
  Result := True;
end;

{ TMCPLogRing<T> }

constructor TMCPLogRing<T>.Create(ACapacity: Integer);
begin
  inherited Create;
  if ACapacity > 0 then
    SetLength(FItems, ACapacity);
end;

function TMCPLogRing<T>.GetCapacity: Integer;
begin
  Result := Length(FItems);
end;

procedure TMCPLogRing<T>.Add(const AItem: T);
var
  LIndex: Integer;
begin
  if Length(FItems) = 0 then
    Exit;

  LIndex := (FHead + FCount) mod Length(FItems);
  FItems[LIndex] := AItem;
  if FCount < Length(FItems) then
    Inc(FCount)
  else
    // Full: the write above landed on the oldest item, which is now the newest
    FHead := (FHead + 1) mod Length(FItems);
end;

procedure TMCPLogRing<T>.Clear;
var
  LCapacity: Integer;
begin
  // Reallocated rather than just reset: the slots hold strings that would
  // otherwise stay referenced until something overwrites them.
  LCapacity := Length(FItems);
  FItems := nil;
  SetLength(FItems, LCapacity);
  FHead := 0;
  FCount := 0;
end;

function TMCPLogRing<T>.ToArray: TArray<T>;
var
  I: Integer;
begin
  SetLength(Result, FCount);
  for I := 0 to FCount - 1 do
    Result[I] := FItems[(FHead + I) mod Length(FItems)];
end;

{ TMCPLogEntry }

class function TMCPLogEntry.New(const ALogClass, AText: string; AException: Exception; 
  ALevel: TLogLevel; AFormatter: TLoggerFormatter): TMCPLogEntry;
var
  LFormatter: TLoggerFormatter;
  LOwnsFormatter: Boolean;
begin
  Result := Default(TMCPLogEntry);
  Result.Timestamp := Now;
  Result.ThreadId := TThread.CurrentThread.ThreadID;
  Result.LogClass := ALogClass;
  Result.Level := ALevel;
  Result.Text := AText;
  if AException <> nil then
  begin
    // The formatter is instanced, so the class function needs one: the
    // adapter's own when it has one, a throwaway default otherwise.
    LOwnsFormatter := AFormatter = nil;
    if LOwnsFormatter then
      LFormatter := TLoggerFormatter.Create
    else
      LFormatter := AFormatter;
    try
      Result.ExceptionInfo := LFormatter.FormatException(AException);
    finally
      if LOwnsFormatter then
        LFormatter.Free;
    end;
  end;
end;

function TMCPLogEntry.ToString: string;
var
  LClass, LText: string;
begin
  if LogClass = '' then
    LClass := 'default'
  else
    LClass := LogClass;

  LText := Text;
  if ExceptionInfo <> '' then
    LText := LText + sLineBreak + ExceptionInfo;

  Result := Format(TLoggerAdapterHelper.LOG_TEMPLATE, [
    DateToISO8601(Timestamp, False),
    UInt64(ThreadId).ToString,
    LClass,
    Level.ToString,
    LText
  ]);
end;

{ TMCPPerfSample }

class function TMCPPerfSample.IsPerfLine(const AMessage: string): Boolean;
begin
  Result := AMessage.TrimLeft.StartsWith(PERF_PREFIX, True);
end;

class function TMCPPerfSample.TryParse(const AMessage: string;
  out ASample: TMCPPerfSample): Boolean;
var
  LText, LBody, LLabel: string;
  LSearch, LColon, LStop, LOpen, LClose, LSpace: Integer;
  LElapsed: Int64;
begin
  Result := False;
  ASample := Default(TMCPPerfSample);

  LText := AMessage.Trim;
  if not LText.StartsWith(PERF_PREFIX, True) then
    Exit;

  LBody := LText.Substring(Length(PERF_PREFIX)).Trim;
  if LBody.IsEmpty then
    Exit;

  // "<label>: <n> ms": the first colon actually followed by a duration wins,
  // so a trailing "(HTTP: 200)" cannot be mistaken for one, and a colon inside
  // a tool name or a URI ("resource://x") is simply skipped over.
  LSearch := 0;
  LStop := -1;
  repeat
    LColon := LBody.IndexOf(':', LSearch);
    if LColon < 0 then
      Exit;
    if TryReadElapsed(LBody, LColon + 1, LElapsed, LStop) then
      Break;
    LSearch := LColon + 1;
  until False;

  LLabel := LBody.Substring(0, LColon).Trim;
  if LLabel.IsEmpty then
    Exit;

  ASample.Text := LText;
  ASample.Key := LLabel;
  ASample.ElapsedMs := LElapsed;
  ASample.Detail := LBody.Substring(LStop).Trim;

  // "<scope> [<target>] <phase>", the brackets holding a name that may itself
  // contain anything but a closing bracket, hence the search from the right
  LOpen := LLabel.IndexOf('[');
  LClose := LLabel.LastIndexOf(']');
  if (LOpen >= 0) and (LClose > LOpen) then
  begin
    ASample.Scope := LLabel.Substring(0, LOpen).Trim;
    ASample.Target := LLabel.Substring(LOpen + 1, LClose - LOpen - 1).Trim;
    ASample.Phase := LLabel.Substring(LClose + 1).Trim;
  end
  else
  begin
    LSpace := LLabel.IndexOf(' ');
    if LSpace > 0 then
    begin
      ASample.Scope := LLabel.Substring(0, LSpace);
      ASample.Phase := LLabel.Substring(LSpace + 1).Trim;
    end
    else
      ASample.Scope := LLabel;
  end;

  Result := True;
end;

function TMCPPerfSample.ToString: string;
begin
  Result := Format('%s: %d ms', [Key, ElapsedMs]);
  if Detail <> '' then
    Result := Result + ' ' + Detail;
end;

{ TMCPPerfStat }

class function TMCPPerfStat.New(const ASample: TMCPPerfSample): TMCPPerfStat;
begin
  Result := Default(TMCPPerfStat);
  Result.Key := ASample.Key;
  Result.Scope := ASample.Scope;
  Result.Target := ASample.Target;
  Result.Phase := ASample.Phase;
  Result.Count := 1;
  Result.TotalMs := ASample.ElapsedMs;
  Result.MinMs := ASample.ElapsedMs;
  Result.MaxMs := ASample.ElapsedMs;
  Result.LastMs := ASample.ElapsedMs;
  Result.FirstSeen := ASample.Timestamp;
  Result.LastSeen := ASample.Timestamp;
end;

procedure TMCPPerfStat.Update(const ASample: TMCPPerfSample);
begin
  Inc(Count);
  Inc(TotalMs, ASample.ElapsedMs);
  MinMs := Min(MinMs, ASample.ElapsedMs);
  MaxMs := Max(MaxMs, ASample.ElapsedMs);
  LastMs := ASample.ElapsedMs;
  LastSeen := ASample.Timestamp;
end;

function TMCPPerfStat.AverageMs: Double;
begin
  if Count = 0 then
    Exit(0);
  Result := TotalMs / Count;
end;

function TMCPPerfStat.ToString: string;
begin
  Result := Format('%s | count: %d, total: %d ms, avg: %.1f ms, min: %d ms, max: %d ms',
    [Key, Count, TotalMs, AverageMs, MinMs, MaxMs]);
end;

{ TMCPMemoryLog }

constructor TMCPMemoryLog.Create;
begin
  Create(DEFAULT_PERF_CAPACITY, 0);
end;

constructor TMCPMemoryLog.Create(APerfCapacity, ALogCapacity: Integer);
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FPerfSamples := TMCPLogRing<TMCPPerfSample>.Create(APerfCapacity);
  FLogEntries := TMCPLogRing<TMCPLogEntry>.Create(ALogCapacity);
  FStats := TDictionary<string, TMCPPerfStat>.Create;
  FMaxKeys := DEFAULT_MAX_KEYS;
end;

destructor TMCPMemoryLog.Destroy;
begin
  FStats.Free;
  FLogEntries.Free;
  FPerfSamples.Free;
  FLock.Free;
  inherited;
end;

class function TMCPMemoryLog.Default: TMCPMemoryLog;
var
  LNew: TMCPMemoryLog;
begin
  Result := FDefault;
  if Assigned(Result) then
    Exit;

  // Lock-free lazy creation: whoever loses the race discards its own instance
  LNew := TMCPMemoryLog.Create;
  Result := TInterlocked.CompareExchange<TMCPMemoryLog>(FDefault, LNew, nil);
  if Result = nil then
    Result := LNew
  else
    LNew.Free;
end;

class function TMCPMemoryLog.Install(ALevel: TLogLevel): TMCPMemoryLog;
begin
  Result := Default;
  if TLoggerAdapterRegistry.Instance.FindFactory(ADAPTER_NAME) = nil then
    TLoggerAdapterRegistry.Instance.RegisterFactory(
      TLogifyAdapterMemoryFactory.CreateAdapterFactory(ADAPTER_NAME, ALevel, Result));
end;

class procedure TMCPMemoryLog.Uninstall;
begin
  TLoggerAdapterRegistry.Instance.UnregisterFactory(ADAPTER_NAME);
end;

procedure TMCPMemoryLog.Capture(const AEntry: TMCPLogEntry);
var
  LSample: TMCPPerfSample;
  LIsPerf: Boolean;
  LCallback: TMCPPerfSampleProc;
begin
  LIsPerf := TMCPPerfSample.TryParse(AEntry.Text, LSample);
  if LIsPerf then
  begin
    LSample.Timestamp := AEntry.Timestamp;
    LSample.ThreadId := AEntry.ThreadId;
  end;

  FLock.Enter;
  try
    FLogEntries.Add(AEntry);
    Inc(FLogCount);

    if LIsPerf then
    begin
      FPerfSamples.Add(LSample);
      Inc(FPerfCount);
      UpdateStat(LSample);
    end;

    LCallback := FOnPerfSample;
  finally
    FLock.Leave;
  end;

  // Outside the lock: the callback runs user code that may well log again
  if LIsPerf and Assigned(LCallback) then
    LCallback(LSample);
end;

procedure TMCPMemoryLog.UpdateStat(const ASample: TMCPPerfSample);
var
  LStat: TMCPPerfStat;
begin
  if FStats.TryGetValue(ASample.Key, LStat) then
  begin
    LStat.Update(ASample);
    FStats[ASample.Key] := LStat;
    Exit;
  end;

  // Bounded on purpose: the keys carry tool names and request URLs, so an
  // unbounded dictionary would grow with the traffic and never shrink.
  if FStats.Count >= FMaxKeys then
  begin
    Inc(FDroppedKeys);
    Exit;
  end;

  FStats.Add(ASample.Key, TMCPPerfStat.New(ASample));
end;

procedure TMCPMemoryLog.Clear;
begin
  FLock.Enter;
  try
    FPerfSamples.Clear;
    FLogEntries.Clear;
    FStats.Clear;
    FPerfCount := 0;
    FLogCount := 0;
    FDroppedKeys := 0;
  finally
    FLock.Leave;
  end;
end;

function TMCPMemoryLog.PerfSamples: TArray<TMCPPerfSample>;
begin
  FLock.Enter;
  try
    Result := FPerfSamples.ToArray;
  finally
    FLock.Leave;
  end;
end;

function TMCPMemoryLog.LogEntries: TArray<TMCPLogEntry>;
begin
  FLock.Enter;
  try
    Result := FLogEntries.ToArray;
  finally
    FLock.Leave;
  end;
end;

function TMCPMemoryLog.PerfStats: TArray<TMCPPerfStat>;
begin
  FLock.Enter;
  try
    Result := FStats.Values.ToArray;
  finally
    FLock.Leave;
  end;

  // Sorted outside the lock: the array is a copy nobody else can see
  TArray.Sort<TMCPPerfStat>(Result, TComparer<TMCPPerfStat>.Construct(
    function(const ALeft, ARight: TMCPPerfStat): Integer
    begin
      // Slowest first, then by key so the report is stable between runs
      if ALeft.TotalMs > ARight.TotalMs then
        Result := -1
      else if ALeft.TotalMs < ARight.TotalMs then
        Result := 1
      else
        Result := CompareText(ALeft.Key, ARight.Key);
    end));
end;

function TMCPMemoryLog.TryGetStat(const AKey: string; out AStat: TMCPPerfStat): Boolean;
begin
  FLock.Enter;
  try
    Result := FStats.TryGetValue(AKey, AStat);
  finally
    FLock.Leave;
  end;
end;

function TMCPMemoryLog.PerfReport: string;
const
  KEY_WIDTH = 56;
  ROW = '%-*s %7d %10d %10.1f %10d %10d';
var
  LStats: TArray<TMCPPerfStat>;
  LStat: TMCPPerfStat;
  LKey: string;
  LBuilder: TStringBuilder;
begin
  LStats := PerfStats;

  LBuilder := TStringBuilder.Create;
  try
    LBuilder.AppendFormat('[PERF] %d samples, %d keys', [PerfCount, Length(LStats)]);
    if DroppedKeys > 0 then
      LBuilder.AppendFormat(', %d samples above the %d key limit', [DroppedKeys, FMaxKeys]);
    LBuilder.AppendLine;

    LBuilder.AppendFormat('%-*s %7s %10s %10s %10s %10s',
      [KEY_WIDTH, 'KEY', 'COUNT', 'TOTAL ms', 'AVG ms', 'MIN ms', 'MAX ms']);
    LBuilder.AppendLine;
    LBuilder.AppendLine(StringOfChar('-', KEY_WIDTH + 51));

    for LStat in LStats do
    begin
      LKey := LStat.Key;
      if LKey.Length > KEY_WIDTH then
        LKey := LKey.Substring(0, KEY_WIDTH - 3) + '...';
      // Invariant: the average is the only float here, and a report that reads
      // "95.0" on one machine and "95,0" on the next cannot be diffed or parsed
      LBuilder.Append(Format(ROW, [KEY_WIDTH, LKey, LStat.Count, LStat.TotalMs,
        LStat.AverageMs, LStat.MinMs, LStat.MaxMs], TFormatSettings.Invariant));
      LBuilder.AppendLine;
    end;

    Result := LBuilder.ToString;
  finally
    LBuilder.Free;
  end;
end;

function TMCPMemoryLog.GetOnPerfSample: TMCPPerfSampleProc;
begin
  FLock.Enter;
  try
    Result := FOnPerfSample;
  finally
    FLock.Leave;
  end;
end;

procedure TMCPMemoryLog.SetOnPerfSample(const AValue: TMCPPerfSampleProc);
begin
  FLock.Enter;
  try
    FOnPerfSample := AValue;
  finally
    FLock.Leave;
  end;
end;

function TMCPMemoryLog.GetPerfCount: Int64;
begin
  FLock.Enter;
  try
    Result := FPerfCount;
  finally
    FLock.Leave;
  end;
end;

function TMCPMemoryLog.GetLogCount: Int64;
begin
  FLock.Enter;
  try
    Result := FLogCount;
  finally
    FLock.Leave;
  end;
end;

function TMCPMemoryLog.GetDroppedKeys: Int64;
begin
  FLock.Enter;
  try
    Result := FDroppedKeys;
  finally
    FLock.Leave;
  end;
end;

{ TLogifyAdapterMemory }

constructor TLogifyAdapterMemory.Create(AStore: TMCPMemoryLog; ALevel: TLogLevel);
begin
  inherited Create(ALevel);
  FStore := AStore;
end;

procedure TLogifyAdapterMemory.SetStore(AStore: TMCPMemoryLog);
begin
  FStore := AStore;
end;

procedure TLogifyAdapterMemory.InternalLog(const AMessage, AClassName: string;
  AException: Exception; ALevel: TLogLevel);
begin
  if _Shutdown or not Assigned(FStore) then
    Exit;

  FStore.Capture(TMCPLogEntry.New(AClassName, AMessage, AException, ALevel, Formatter));
end;

procedure TLogifyAdapterMemory.InternalRaw(const AMessage: string; ALevel: TLogLevel);
begin
  if _Shutdown or not Assigned(FStore) then
    Exit;

  FStore.Capture(TMCPLogEntry.New('', AMessage, nil, ALevel));
end;

{ TLogifyAdapterMemoryFactory }

class function TLogifyAdapterMemoryFactory.CreateAdapterFactory(
  const AName: string; ALevel: TLogLevel; AStore: TMCPMemoryLog): TLogifyAdapterMemoryFactory;
begin
  Result := TLogifyAdapterMemoryFactory.Create();
  Result.Name := AName;
  Result.Level := ALevel;
  Result.Store := AStore;
end;

class function TLogifyAdapterMemoryFactory.CreateAdapterFactory(ALevel: TLogLevel;
  AStore: TMCPMemoryLog): TLogifyAdapterMemoryFactory;
begin
  Result := CreateAdapterFactory('', ALevel, AStore);
end;

class function TLogifyAdapterMemoryFactory.CreateAdapterFactory(
  ALevel: TLogLevel): TLogifyAdapterMemoryFactory;
begin
  Result := CreateAdapterFactory('', ALevel, TMCPMemoryLog.Default);
end;

function TLogifyAdapterMemoryFactory.CreateLoggerAdapter: ILoggerAdapter;
begin
  Result := TLogifyAdapterMemory.Create(FStore, FLevel);
end;

initialization

finalization
  // Must come first: whatever the Logify registry still holds has to find the
  // default store already disarmed.
  _Shutdown := True;
  FreeAndNil(TMCPMemoryLog.FDefault);

end.
