{******************************************************************************}
{                                                                              }
{  MCPConnect - IndyMetrics showcase: protocol telemetry as middleware          }
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
///   Records MCP telemetry where it belongs: in middleware, not in the business
///   classes.
///
///   One class implements two hooks:
///
///   * IMCPMiddleware is the universal hook, so it sees every MCP operation that
///     has a chain (tools/call, tools/list, resources/read, resources/list,
///     prompts/get, prompts/list, server/discover) and times it by method;
///   * ICallToolMiddleware sees tools/call alone, so it can label the same
///     measurements by tool name and observe the size of the arguments.
///
///   The universal hook wraps the specific one, so a tools/call is counted once
///   as a request (by method) and once as a tool call (by tool) - two different
///   questions, two different instruments.
///
///   The request in flight gauge is incremented before the call and decremented
///   in both exits, including the exception path, so it always comes back to
///   zero. Because a middleware instance is built per request, no field here
///   needs a lock; the shared state is inside the hub's instruments, which are
///   thread safe.
/// </summary>
unit Server.Metrics.Middleware;

interface

uses
  System.SysUtils,

  JRPC.Core,

  MCPConnect.JRPC.Middleware,
  MCPConnect.MCP.Middleware,
  MCPConnect.MCP.Types.Base,
  MCPConnect.MCP.Types.Tools;

type
  TMetricsMCPMiddleware = class(TMiddleware, IMCPMiddleware, ICallToolMiddleware)
  public
    /// <summary>Runs with the observability middleware, not at user priority.</summary>
    class function DefaultPriority: Integer; override;
    function GetName: string; override;
    function OnMCP(AContext: TMiddlewareContext; AParams: TRequestMetaParams;
      const AChain: TMCPChain): TBaseResult;
    function OnCallTool(AContext: TMiddlewareContext;
      AParams: TCallToolRequestParams;
      const AChain: TCallToolChain): TBaseResult;

    function IMCPMiddleware.Handle = OnMCP;
    function ICallToolMiddleware.Handle = OnCallTool;
  end;

implementation

uses
  System.Diagnostics,
  Server.Metrics;

{ TMetricsMCPMiddleware }

class function TMetricsMCPMiddleware.DefaultPriority: Integer;
begin
  Result := MW_PRIORITY_OBSERVABILITY;
end;

function TMetricsMCPMiddleware.GetName: string;
begin
  Result := 'Metrics (MCP telemetry)';
end;

function TMetricsMCPMiddleware.OnMCP(AContext: TMiddlewareContext;
  AParams: TRequestMetaParams; const AChain: TMCPChain): TBaseResult;
var
  LWatch: TStopwatch;
begin
  // The context is only nil for the api classes called directly, with no
  // transport behind them; there is nothing to label the measurement with then.
  if AContext = nil then
    Exit(AChain.Next(AContext, AParams));

  TServerMetrics.Instance.RequestStarted(AContext.Method);
  LWatch := TStopwatch.StartNew;
  try
    Result := AChain.Next(AContext, AParams);
    TServerMetrics.Instance.RequestSucceeded(AContext.Method, LWatch.Elapsed.TotalMilliseconds);
  except
    on E: Exception do
    begin
      TServerMetrics.Instance.RequestFailed(AContext.Method,
        LWatch.Elapsed.TotalMilliseconds, E);
      raise; // observing, not handling: the server still maps the error
    end;
  end;
end;

function TMetricsMCPMiddleware.OnCallTool(AContext: TMiddlewareContext;
  AParams: TCallToolRequestParams; const AChain: TCallToolChain): TBaseResult;
var
  LWatch: TStopwatch;
  LTool: string;
begin
  if (AContext = nil) or (AParams = nil) then
    Exit(AChain.Next(AContext, AParams));

  LTool := AParams.Name;
  TServerMetrics.Instance.ToolStarted(LTool, AParams.Arguments);
  LWatch := TStopwatch.StartNew;
  try
    Result := AChain.Next(AContext, AParams);
    TServerMetrics.Instance.ToolSucceeded(LTool, LWatch.Elapsed.TotalMilliseconds);
  except
    on E: Exception do
    begin
      TServerMetrics.Instance.ToolFailed(LTool,
        LWatch.Elapsed.TotalMilliseconds, E);
      raise;
    end;
  end;
end;

end.
