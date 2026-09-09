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
///   Sample harvesters for the MCPConnect.Metrics subsystem: an
///   IMetricExporter that renders the collected points as a human readable
///   text block, one that renders them as JSON (via the Neon serializer), and
///   the plain functions behind them (MetricsToText / MetricsToJson) for
///   use without an exporter instance at all.
///
///   Both exporters append to a TStrings target the caller owns and that must
///   outlive the exporter. The text exporter writes one block per Harvest
///   call; the JSON exporter writes one line per Harvest call containing a
///   complete JSON array, so consecutive harvests form a JSON-lines stream:
///   <code>
///   var LTarget: TStringList;
///       LExporter: IMetricExporter;
///   begin
///     LTarget := TStringList.Create;
///     LExporter := TMetricJsonExporter.Create(LTarget);
///     TMetrics.AddExporter(LExporter);
///     try
///       ...
///       TMetrics.Harvest;       // LTarget now holds one JSON line
///     finally
///       TMetrics.RemoveExporter(LExporter);
///       LTarget.Free;
///     end;
///   end;
///   </code>
///   These two are deliberately generic (text on screen, JSON for anything
///   machine readable). A real sink - a file, Logify, a push to a remote
///   collector - is a new IMetricExporter implementation over the typed
///   points Harvest delivers.
/// </summary>
unit MCPConnect.Metrics.Exporters;

interface

uses
  System.Classes,

  MCPConnect.Metrics;

type
  /// <summary>
  ///   Appends one human readable block per Harvest call to a TStrings
  ///   target. The block starts with a timestamped header and lists one line
  ///   per series, interpreted by instrument kind (sum for counters, value
  ///   for gauges, count/sum/average/min/max for histograms).
  /// </summary>
  TMetricTextExporter = class(TInterfacedObject, IMetricExporter)
  private
    FTarget: TStrings;
  public
    constructor Create(ATarget: TStrings);
    procedure Export(const APoints: TArray<TMetricPoint>);
    /// <summary>The TStrings every Harvest is appended to (caller owned).</summary>
    property Target: TStrings read FTarget;
  end;

  /// <summary>
  ///   Appends one line per Harvest call to a TStrings target: a complete
  ///   JSON array of metric points, ready to be parsed back or streamed as
  ///   JSON-lines. See MetricsToJson for the exact shape.
  /// </summary>
  TMetricJsonExporter = class(TInterfacedObject, IMetricExporter)
  private
    FTarget: TStrings;
  public
    constructor Create(ATarget: TStrings);
    procedure Export(const APoints: TArray<TMetricPoint>);
    /// <summary>The TStrings every Harvest is appended to (caller owned).</summary>
    property Target: TStrings read FTarget;
  end;

/// <summary>
///   Renders the points as a human readable block: a timestamped header plus
///   one line per point (the same layout TMetricTextExporter writes).
/// </summary>
function MetricsToText(const APoints: TArray<TMetricPoint>): string;

/// <summary>
///   Renders the points as a JSON array using the Neon serializer
///   (TNeonConfiguration.Camel, labels as a JSON object with sorted keys).
///   Every point becomes an object:
///   <code>
///   {
///     "meter": "demo", "name": "tool.calls", "kind": "counter",
///     "description": "", "unit": "calls",
///     "labels": { "tool": "add_task" },
///     "count": 5, "sum": 5.0, "min": 1.0, "max": 1.0, "last": 1.0,
///     "firstSeen": "2026-11-20T10:12:33.000Z", "lastSeen": "..."
///   }
///   </code>
///   Member names are camelCased from the exported record, timestamps follow
///   Neon's ISO 8601 handling, and the JSON is printed in a single line so a
///   harvest can be appended to a JSON-lines stream.
/// </summary>
function MetricsToJson(const APoints: TArray<TMetricPoint>): string;

implementation

uses
  System.JSON,
  System.Rtti,
  System.SysUtils,
  System.TypInfo,
  System.Generics.Collections,

  Neon.Core.Persistence,
  Neon.Core.Persistence.JSON,
  Neon.Core.Types;

type
  /// <summary>
  ///   Renders TMetricPoint.Labels as a JSON object - {"tool": "add_task"} -
  ///   instead of the array of {key, value} pairs the field's own shape would
  ///   produce. That object is the documented wire format (see Docs/metrics.md
  ///   6.2) and the one a collector expects of an OpenTelemetry attribute set;
  ///   the pair array is an implementation detail of how a series stores them.
  ///   Keys come out in the series' own order, which is sorted by key.
  /// </summary>
  TMetricLabelsSerializer = class(TCustomSerializer)
  protected
    class function GetTargetInfo: PTypeInfo; override;
    class function CanHandle(AType: PTypeInfo): Boolean; override;
  public
    function Serialize(const AValue: TValue; ANeonObject: TNeonRttiObject;
      AContext: ISerializerContext): TJSONValue; override;
    function Deserialize(AValue: TJSONValue; const AData: TValue;
      ANeonObject: TNeonRttiObject; AContext: IDeserializerContext): TValue; override;
  end;

{ TMetricLabelsSerializer }

class function TMetricLabelsSerializer.GetTargetInfo: PTypeInfo;
begin
  Result := TypeInfo(TArray<TMetricLabel>);
end;

class function TMetricLabelsSerializer.CanHandle(AType: PTypeInfo): Boolean;
begin
  // Compared by type info, not by TypeInfoIs: that helper only ever matches
  // class types, and the target here is a dynamic array.
  Result := AType = GetTargetInfo;
end;

function TMetricLabelsSerializer.Serialize(const AValue: TValue;
  ANeonObject: TNeonRttiObject; AContext: ISerializerContext): TJSONValue;
var
  LLabels: TArray<TMetricLabel>;
  LLabel: TMetricLabel;
  LResult: TJSONObject;
begin
  LLabels := AValue.AsType<TArray<TMetricLabel>>;

  LResult := TJSONObject.Create;
  for LLabel in LLabels do
    LResult.AddPair(LLabel.Key, LLabel.Value);
  Result := LResult;
end;

function TMetricLabelsSerializer.Deserialize(AValue: TJSONValue;
  const AData: TValue; ANeonObject: TNeonRttiObject;
  AContext: IDeserializerContext): TValue;
var
  LObject: TJSONObject;
  LLabels: TArray<TMetricLabel>;
  LIndex: Integer;
begin
  LObject := AValue as TJSONObject;

  SetLength(LLabels, LObject.Count);
  for LIndex := 0 to LObject.Count - 1 do
  begin
    LLabels[LIndex].Key := LObject.Pairs[LIndex].JsonString.Value;
    LLabels[LIndex].Value := LObject.Pairs[LIndex].JsonValue.Value;
  end;
  Result := TValue.From<TArray<TMetricLabel>>(LLabels);
end;

function MetricsToText(const APoints: TArray<TMetricPoint>): string;
var
  LBuilder: TStringBuilder;
  LPoint: TMetricPoint;
begin
  LBuilder := TStringBuilder.Create;
  try
    LBuilder.AppendFormat('[metrics] %s - %d point(s)', [DateTimeToStr(Now), Length(APoints)]);
    for LPoint in APoints do
    begin
      LBuilder.AppendLine;
      LBuilder.Append('  ');
      LBuilder.Append(LPoint.ToString);
    end;
    Result := LBuilder.ToString;
  finally
    LBuilder.Free;
  end;
end;

function MetricsToJson(const APoints: TArray<TMetricPoint>): string;
var
  LConfig: INeonConfiguration;
  LArray: TJSONArray;
  LPoint: TMetricPoint;
begin
  LConfig := TNeonConfiguration.Camel
    .RegisterSerializer(TMetricLabelsSerializer);

  LArray := TJSONArray.Create;
  try
    for LPoint in APoints do
      LArray.AddElement(
        TNeon.ValueToJSON(TValue.From<TMetricPoint>(LPoint), LConfig));

    // Single line, with the control/high characters escaped the way Neon's
    // printer escapes them (a plain ToString would leave some of them raw).
    Result := TNeon.Print(LArray, False);
  finally
    LArray.Free;
  end;
end;

{ TMetricTextExporter }

constructor TMetricTextExporter.Create(ATarget: TStrings);
begin
  inherited Create;
  FTarget := ATarget;
end;

procedure TMetricTextExporter.Export(const APoints: TArray<TMetricPoint>);
begin
  if not Assigned(FTarget) then
    raise EMetricsError.Create('TMetricTextExporter: no target strings assigned');
  FTarget.Add(MetricsToText(APoints));
end;

{ TMetricJsonExporter }

constructor TMetricJsonExporter.Create(ATarget: TStrings);
begin
  inherited Create;
  FTarget := ATarget;
end;

procedure TMetricJsonExporter.Export(const APoints: TArray<TMetricPoint>);
begin
  if not Assigned(FTarget) then
    raise EMetricsError.Create('TMetricJsonExporter: no target strings assigned');
  FTarget.Add(MetricsToJson(APoints));
end;

end.
