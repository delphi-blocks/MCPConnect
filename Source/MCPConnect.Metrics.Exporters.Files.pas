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
///   A real sink for MCPConnect.Metrics harvests: TMetricFileExporter
///   appends every Harvest to a text file, one line per harvest when writing
///   JSON (a JSON-lines file) or one human readable block when writing text.
///
///   The exporter opens the file when it is created (append mode, UTF-8) and
///   keeps it open until it is released, so setup errors surface early and the
///   file is not reopened per harvest. Harvests must not run concurrently on
///   this exporter - the usual case is a single timer or shutdown flush.
///   <code>
///   var LExporter: IMetricExporter;
///   begin
///     LExporter := TMetricFileExporter.Create('metrics.jsonl');
///     TMetrics.AddExporter(LExporter);
///     ...
///     TMetrics.Harvest(True);   // one JSON line per tick
///   end;
///   </code>
///   The file target is deliberately owned by the caller of this unit - a
///   remote collector or a log sink needs its own exporter.
/// </summary>
unit MCPConnect.Metrics.Exporters.Files;

interface

uses
  System.Classes,

  MCPConnect.Metrics,
  MCPConnect.Metrics.Exporters;

type
  /// <summary>
  ///   Appends each Harvest call to a file, opened in append mode at creation.
  ///   By default every harvest becomes one line holding the full JSON array
  ///   (see MetricsToJson), so consecutive harvests form a JSON-lines
  ///   file; set AAsText to True to write the human readable blocks of
  ///   MetricsToText instead.
  /// </summary>
  TMetricFileExporter = class(TInterfacedObject, IMetricExporter)
  private
    FWriter: TStreamWriter;
    FAsText: Boolean;
  public
    /// <summary>Append-mode JSON-lines file.</summary>
    constructor Create(const AFileName: string); overload;
    /// <summary>
    ///   AAsText False (default): one JSON array per harvest.
    ///   AAsText True: one human readable block per harvest.
    /// </summary>
    constructor Create(const AFileName: string; AAsText: Boolean); overload;
    destructor Destroy; override;

    procedure Export(const APoints: TArray<TMetricPoint>);

    /// <summary>True when the exporter writes MetricsToText blocks.</summary>
    property AsText: Boolean read FAsText;
  end;

implementation

uses
  System.SysUtils;

{ TMetricFileExporter }

constructor TMetricFileExporter.Create(const AFileName: string);
begin
  Create(AFileName, False);
end;

constructor TMetricFileExporter.Create(const AFileName: string; AAsText: Boolean);
begin
  inherited Create;
  FAsText := AAsText;
  try
    FWriter := TStreamWriter.Create(AFileName, True, TEncoding.UTF8);
    FWriter.AutoFlush := True; // each harvest hits the disk: metrics are low volume
  except
    on E: Exception do
      raise EMetricsError.CreateFmt(
        'TMetricFileExporter: cannot open "%s" for append: %s', [AFileName, E.Message]);
  end;
end;

destructor TMetricFileExporter.Destroy;
begin
  FWriter.Free;
  inherited;
end;

procedure TMetricFileExporter.Export(const APoints: TArray<TMetricPoint>);
begin
  if not Assigned(FWriter) then
    raise EMetricsError.Create('TMetricFileExporter: no file open');
  if FAsText then
    FWriter.Write(MetricsToText(APoints)) // already ends with a line break
  else
    FWriter.WriteLine(MetricsToJson(APoints));
end;

end.
