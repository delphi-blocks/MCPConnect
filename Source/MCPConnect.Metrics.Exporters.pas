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
  System.Generics.Collections,

  Neon.Core.Attributes,
  Neon.Core.Persistence,
  Neon.Core.Persistence.JSON,
  Neon.Core.Types;

type
  /// <summary>
  ///   Wire shape of one metric point, mapped from TMetricPoint so that the
  ///   JSON stays self contained: Kind is already the lowercase string a
  ///   consumer expects ("counter", "gauge", "histogram"), UnitName is renamed
  ///   to "unit", and labels are rendered as a JSON object with the label keys
  ///   sorted.
  /// </summary>
  TMetricJsonPoint = record
    Meter: string;
    Name: string;
    Kind: string;
    Description: string;
    [NeonProperty('unit')]
    UnitName: string;
    Labels: TDictionary<string, string>;
    Count: Int64;
    Sum: Double;
    Min: Double;
    Max: Double;
    Last: Double;
    FirstSeen: TDateTime;
    LastSeen: TDateTime;
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
  LLabel: TMetricLabel;
  LExport: TMetricJsonPoint;
begin
  LConfig := TNeonConfiguration.Camel
    .SetMapSort(TNeonSort.Alpha);

  LArray := TJSONArray.Create;
  try
    for LPoint in APoints do
    begin
      LExport := Default(TMetricJsonPoint);
      LExport.Meter := LPoint.Meter;
      LExport.Name := LPoint.Name;
      LExport.Kind := MetricKindToStr(LPoint.Kind);
      LExport.Description := LPoint.Description;
      LExport.UnitName := LPoint.UnitName;
      LExport.Count := LPoint.Count;
      LExport.Sum := LPoint.Sum;
      LExport.Min := LPoint.Min;
      LExport.Max := LPoint.Max;
      LExport.Last := LPoint.Last;
      LExport.FirstSeen := LPoint.FirstSeen;
      LExport.LastSeen := LPoint.LastSeen;

      LExport.Labels := TDictionary<string, string>.Create;
      try
        for LLabel in LPoint.Labels do
          LExport.Labels.Add(LLabel.Key, LLabel.Value);

        LArray.AddElement(
          TNeon.ValueToJSON(TValue.From<TMetricJsonPoint>(LExport), LConfig));
      finally
        LExport.Labels.Free;
      end;
    end;

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
