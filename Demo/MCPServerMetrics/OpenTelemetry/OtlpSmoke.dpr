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
///   End-to-end check of TOtlpMetricExporter against a running OpenTelemetry
///   collector (docker compose up, in this folder, starts one).
///
///     OtlpSmoke.exe [endpoint]     default http://127.0.0.1:4318/v1/metrics
///
///   Records counters, a histogram and a gauge on three meters, then harvests
///   with a reset, without one and gzipped, and finally points the exporter at
///   a wrong path and at a closed port to show that a failed export is counted
///   and reported, never raised. Exit code: the number of failed checks.
///   Build with OtlpSmoke.Build.bat.
/// </summary>
program OtlpSmoke;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.DateUtils,
  MCPConnect.Metrics in '..\..\..\Source\MCPConnect.Metrics.pas',
  MCPConnect.Metrics.Exporters.Otlp in '..\..\..\Source\MCPConnect.Metrics.Exporters.Otlp.pas';

const
  DEFAULT_ENDPOINT = 'http://127.0.0.1:4318/v1/metrics';

type
  /// <summary>The real HTTP sender, logging what goes out and what comes back.</summary>
  TLoggingSender = class(TInterfacedObject, IOtlpHttpSender)
  private
    FInner: IOtlpHttpSender;
  public
    LastStatus: Integer;
    constructor Create;
    function Post(const AUrl: string; const AHeaders: TArray<TOtlpHeader>;
      const ABody: TBytes; ATimeoutMs: Integer): TOtlpHttpResponse;
  end;

constructor TLoggingSender.Create;
begin
  inherited Create;
  FInner := TOtlpHttpClientSender.Create;
end;

function TLoggingSender.Post(const AUrl: string; const AHeaders: TArray<TOtlpHeader>;
  const ABody: TBytes; ATimeoutMs: Integer): TOtlpHttpResponse;
var
  LHeader: TOtlpHeader;
  LHeaders: string;
begin
  LHeaders := '';
  for LHeader in AHeaders do
    LHeaders := LHeaders + LHeader.Name + '=' + LHeader.Value + ' ';
  Writeln(Format('  POST %s  (%d bytes; %s)', [AUrl, Length(ABody), LHeaders.Trim]));
  Result := FInner.Post(AUrl, AHeaders, ABody, ATimeoutMs);
  LastStatus := Result.StatusCode;
  Writeln(Format('  <- HTTP %d  %s', [Result.StatusCode, Result.Body.Trim]));
end;

var
  Failures: Integer = 0;

procedure Check(ACondition: Boolean; const AWhat: string);
begin
  if ACondition then
    Writeln('  OK   ', AWhat)
  else
  begin
    Writeln('  FAIL ', AWhat);
    Inc(Failures);
  end;
end;

procedure RecordSamples(const AProvider: IMeterProvider; AScale: Integer);
begin
  AProvider.GetMeter('mcp.server').Counter('tool.calls', 'Tool calls', '{call}')
    .Add(AScale, ['tool', 'delete_task']);
  AProvider.GetMeter('mcp.server').Counter('tool.calls', 'Tool calls', '{call}')
    .Add(2 * AScale, ['tool', 'import_tasks']);
  AProvider.GetMeter('mcp.server').Histogram('tool.duration_ms', 'Tool latency', 'ms')
    .Observe(10 * AScale, ['tool', 'delete_task']);
  AProvider.GetMeter('demo.system').Gauge('queue.depth', 'Queue depth', '{item}')
    .SetValue(5 * AScale);
  AProvider.GetMeter('').Counter('harvests').Add(1);
end;

var
  LEndpoint: string;
  LProvider: IMeterProvider;
  LSender: TLoggingSender;
  LSenderRef: IOtlpHttpSender;
  LExporter: TOtlpMetricExporter;
  LExporterRef: IMetricExporter;
  LErrors: string;
begin
  try
    LEndpoint := DEFAULT_ENDPOINT;
    if ParamCount > 0 then
      LEndpoint := ParamStr(1);

    LErrors := '';
    LProvider := TMetrics.CreateProvider;
    LSender := TLoggingSender.Create;
    LSenderRef := LSender;
    LExporter := TOtlpMetricExporter.Create(LEndpoint, LSenderRef);
    LExporterRef := LExporter;
    LExporter.AddResourceAttribute('service.name', 'mcpconnect-otlp-smoke');
    LExporter.OnError :=
      procedure(AMessage: string)
      begin
        LErrors := LErrors + AMessage + sLineBreak;
      end;
    LProvider.AddExporter(LExporterRef);

    Writeln('Payload preview:');
    RecordSamples(LProvider, 1);
    Writeln(MetricsToOtlpJson(LProvider.Collect, LExporter.ResourceAttributes,
      TTimeZone.Local.ToUniversalTime(Now)));
    Writeln;

    Writeln('1. Harvest(True) - reset harvest');
    LProvider.Harvest(True);
    Check(LSender.LastStatus = 200, 'collector answered 200');

    Writeln('2. Harvest(False) - plain harvest after more recordings');
    RecordSamples(LProvider, 3);
    LProvider.Harvest(False);
    Check(LSender.LastStatus = 200, 'collector answered 200');

    Writeln('3. Harvest(True) with gzip');
    LExporter.Compression := True;
    RecordSamples(LProvider, 1);
    LProvider.Harvest(True);
    Check(LSender.LastStatus = 200, 'collector answered 200 to a gzipped body');

    Check(LExporter.SentCount = 3, Format('SentCount = 3 (is %d)', [LExporter.SentCount]));
    Check(LExporter.FailedCount = 0, Format('FailedCount = 0 (is %d)', [LExporter.FailedCount]));
    Check(LErrors = '', 'no OnError reports: ' + LErrors.Trim);

    Writeln('4. Wrong path - must fail without raising');
    LExporter.Endpoint := LEndpoint.Replace('/v1/metrics', '/v1/nope');
    RecordSamples(LProvider, 1);
    LProvider.Harvest(False);
    Check(LExporter.FailedCount = 1, Format('FailedCount = 1 (is %d)', [LExporter.FailedCount]));
    Check(LErrors <> '', 'OnError reported: ' + LErrors.Trim);

    Writeln('5. Nothing listening - transport error must not raise');
    LExporter.Endpoint := 'http://127.0.0.1:1/v1/metrics';
    LExporter.Timeout := 2000;
    RecordSamples(LProvider, 1);
    LProvider.Harvest(False);
    Check(LExporter.FailedCount = 2, Format('FailedCount = 2 (is %d)', [LExporter.FailedCount]));
    Writeln('  LastError: ', LExporter.LastError);

    LProvider := nil;
    LExporterRef := nil;
    LSenderRef := nil;
  except
    on E: Exception do
    begin
      Writeln('UNEXPECTED ', E.ClassName, ': ', E.Message);
      Inc(Failures);
    end;
  end;

  Writeln;
  if Failures = 0 then
    Writeln('ALL CHECKS PASSED')
  else
    Writeln(Failures, ' CHECK(S) FAILED');
  ExitCode := Failures;
end.
