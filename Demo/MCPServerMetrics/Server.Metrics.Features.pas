{******************************************************************************}
{                                                                              }
{  MCPConnect - IndyMetrics showcase: the MCP features over the metrics         }
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
///   The metrics made consumable by a model: tools to read the snapshot, to
///   inspect one instrument, to trigger a workload, to reset, and to run the two
///   API demonstrations; a resource for the "load the data" style; a prompt that
///   asks the model to interpret the numbers.
///
///   Every tool is a thin wrapper over TIndyMetrics, which is where the
///   instruments and the exporters live. Nothing here records directly: the
///   tools read through Collect (a snapshot, so reading never disturbs the
///   accumulation) or call one of the hub's demonstration methods.
///
///   The class carries [McpScope('metrics')], so the tool names reach the client
///   as metrics_snapshot, metrics_instrument, ... The resource and the prompt are
///   deliberately in classes of their own, since a scope is per class.
/// </summary>
unit Server.Metrics.Features;

interface

uses
  System.SysUtils,

  MCPConnect.MCP.Attributes,
  MCPConnect.MCP.Types.Base;

type
  /// <summary>Tools over the metrics: read, inspect, generate, reset.</summary>
  [McpScope('metrics')]
  TMetricsShowcaseTools = class
  public
    [McpTool('snapshot',
      'Current in-process metrics as a human readable table, one line per series')]
    function Snapshot(): string;

    [McpTool('snapshot_json',
      'Current in-process metrics as two JSON documents: the wire shape (MetricsToJson) and a compact summary')]
    function SnapshotJson(): string;

    [McpTool('instrument',
      'Every series of one instrument, with its kind, unit and description')]
    function InstrumentReport(
      [McpParam('meter', 'Meter name, e.g. mcp.server (empty for the default meter)')]
      const AMeter: string = '';
      [McpParam('name', 'Instrument name, e.g. request.count')]
      const AName: string = 'request.count'): string;

    [McpTool('slowest',
      'The slowest histograms by average: latency, order value, argument and batch sizes')]
    function Slowest(
      [McpParam('top_n', 'How many series to list')]
      ATopN: Integer = 10): string;

    [McpTool('exporters',
      'The exporters attached to the default provider and the series-per-harvest history')]
    function Exporters(): string;

    [McpTool('use_cases',
      'What the demo is demonstrating, use case by use case')]
    function UseCases(): string;

    [McpTool('simulate_workload',
      'Records a burst of realistic application metrics: counters split by labels, a value histogram and gauges')]
    function SimulateWorkload(
      [McpParam('operations', 'How many simulated operations to record')]
      AOperations: Integer = 50;
      [McpParam('regions', 'Comma separated regions to spread the workload over')]
      const ARegions: string = 'eu,us,apac'): string;

    [McpTool('simulate_errors',
      'Records simulated error counters, split by kind (timeout, validation, upstream)')]
    function SimulateErrors(
      [McpParam('operations', 'How many errors to record')]
      AOperations: Integer = 10): string;

    [McpTool('reset',
      'Clears every instrument of the default provider (the instruments themselves are kept)')]
    function Reset(): string;

    [McpTool('cardinality',
      'Demonstrates the per-instrument series budget and DroppedSeries on a private provider')]
    function Cardinality(): string;

    [McpTool('separate_provider',
      'Demonstrates that a second provider keeps its instruments apart from the default one')]
    function SeparateProvider(): string;
  end;

  /// <summary>
  ///   The harvest as a resource: a client loads it, no tool call, no side
  ///   effect. Same JSON document MetricsToJson would write to a file.
  /// </summary>
  TMetricsShowcaseResource = class
  public
    [McpResource('metrics-live', 'resource://metrics/live', 'application/json',
      'Current in-process metrics snapshot of the IndyMetrics showcase')]
    function ReadLive(): string;
  end;

  /// <summary>
  ///   A prompt that turns the snapshot into a request for analysis, so a model
  ///   can be asked "what is wrong with the server right now" with the numbers
  ///   already in hand.
  /// </summary>
  TMetricsShowcasePrompts = class
  public
    [MCPPrompt('analyse-metrics', 'Analyse the live metrics',
      'Asks the model to read the current metrics and point out anomalies')]
    function AnalyseMetrics(
      [McpArgument('focus', 'Optional area to focus on, e.g. latency or errors')]
      const AFocus: string = ''): string;
  end;

implementation

uses
  MCPConnect.Metrics,
  Server.Metrics;

{ TMetricsShowcaseTools }

function TMetricsShowcaseTools.Snapshot: string;
begin
  Result := TServerMetrics.Instance.TextReport;
end;

function TMetricsShowcaseTools.SnapshotJson: string;
var
  LPoints: Integer;
begin
  LPoints := TServerMetrics.Instance.SnapshotSize;
  Result :=
    Format('{"series": %d, "harvests": "see metrics_exporters", "points": ', [LPoints]) +
    TServerMetrics.Instance.JsonReport + '}';
end;

function TMetricsShowcaseTools.InstrumentReport(const AMeter, AName: string): string;
begin
  Result := TServerMetrics.Instance.InstrumentReport(AMeter, AName);
end;

function TMetricsShowcaseTools.Slowest(ATopN: Integer): string;
begin
  Result := TServerMetrics.Instance.SlowestReport(ATopN);
end;

function TMetricsShowcaseTools.Exporters: string;
begin
  Result := TServerMetrics.Instance.ExportersReport;
end;

function TMetricsShowcaseTools.UseCases: string;
begin
  Result := TServerMetrics.Instance.UseCasesReport;
end;

function TMetricsShowcaseTools.SimulateWorkload(AOperations: Integer;
  const ARegions: string): string;
begin
  TServerMetrics.Instance.SimulateWorkload(AOperations, ARegions);
  Result := Format('Recorded %d simulated operations across [%s].', [AOperations, ARegions]) +
    sLineBreak + sLineBreak +
    TServerMetrics.Instance.InstrumentReport('demo.workload', 'orders.created');
end;

function TMetricsShowcaseTools.SimulateErrors(AOperations: Integer): string;
begin
  TServerMetrics.Instance.SimulateErrors(AOperations);
  Result := Format('Recorded %d simulated errors.', [AOperations]) +
    sLineBreak + sLineBreak +
    TServerMetrics.Instance.InstrumentReport('demo.workload', 'errors');
end;

function TMetricsShowcaseTools.Reset: string;
var
  LBefore: Integer;
begin
  LBefore := TServerMetrics.Instance.SnapshotSize;
  // Clear resets the recorded values, not the instruments: after this the same
  // counters keep being recorded into, from zero.
  TMetrics.Clear;
  Result := Format('Cleared %d series; instruments and exporters are kept.', [LBefore]);
end;

function TMetricsShowcaseTools.Cardinality: string;
begin
  Result := TServerMetrics.Instance.CardinalityDemo;
end;

function TMetricsShowcaseTools.SeparateProvider: string;
begin
  Result := TServerMetrics.Instance.SeparateProviderDemo;
end;

{ TMetricsShowcaseResource }

function TMetricsShowcaseResource.ReadLive: string;
begin
  Result := TServerMetrics.Instance.JsonReport;
end;

{ TMetricsShowcasePrompts }

function TMetricsShowcasePrompts.AnalyseMetrics(const AFocus: string): string;
begin
  Result :=
    'Here is the current metrics snapshot of this MCP server:' + sLineBreak + sLineBreak +
    TServerMetrics.Instance.TextReport + sLineBreak + sLineBreak +
    'Look for anomalies: a latency histogram whose average has drifted, error ' +
    'counters that are no longer zero, a request in flight gauge stuck above ' +
    'what the traffic explains, series being dropped. Summarise what is healthy ' +
    'and what is not, and suggest one concrete next step.';
  if not AFocus.IsEmpty then
    Result := Result + ' Focus especially on: ' + AFocus + '.';
end;

initialization
  // Nothing at unit load: the hub is started by the form, once the screen
  // targets its exporters write to exist.
end.
