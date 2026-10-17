{******************************************************************************}
{                                                                              }
{  MCPConnect - IndyMetrics showcase: the metrics as plain HTTP                 }
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
///   GET /metrics: the numbers the MCP tools expose, served as an ordinary JSON
///   document over plain HTTP, so a browser dashboard can poll them without
///   speaking a byte of MCP.
///
///   The endpoint is a *transport* middleware, which is the only layer that can
///   answer a request the MCP protocol would refuse: the transport dispatches
///   POST and OPTIONS and answers 405 to everything else, and it does that at
///   the innermost point of the transport chain. A middleware that recognizes
///   its own request and simply does not call Next therefore answers it before
///   that 405 is ever reached - the same trick TOAuthMiddleware uses for the
///   RFC 9728 discovery document, and it works on the Indy and WebBroker
///   transports alike.
///
///   Its priority places it *inside* the CORS middleware (so a browser gets the
///   Access-Control-* headers, written before the chain continues) and *before*
///   the static-token gate, which makes /metrics public. A deployment that wants
///   the token to cover it too registers the same class at
///   MW_PRIORITY_AUTHENTICATION, and the gate then refuses an unauthenticated
///   GET as it would a POST.
///
///   The document itself - the fold, the rollups, the JSON shape - lives in
///   Server.Metrics.Document.pas, because the WebStencils dashboard in the
///   WebStencils folder renders the same numbers as HTML and the two must not be
///   able to disagree.
/// </summary>
unit Server.Metrics.Http;

interface

uses
  System.SysUtils,

  MCPConnect.JRPC.Middleware;

type
  /// <summary>
  ///   Answers GET /metrics with a JSON document and lets every other request
  ///   through to the chain. Stateless: the numbers it reports live in
  ///   TMetricsDocumentState, because a middleware instance is built per request.
  /// </summary>
  TMetricsHttpMiddleware = class(TMiddleware, ITransportMiddleware)
  public
    /// <summary>The path this endpoint answers on.</summary>
    const METRICS_PATH = '/metrics';

    /// <summary>
    ///   Inside CORS (MW_PRIORITY_AUTHENTICATION - 100) so the headers are
    ///   already written, and still short of the static-token gate at
    ///   MW_PRIORITY_AUTHENTICATION, so the endpoint is public.
    /// </summary>
    class function DefaultPriority: Integer; override;

    function GetName: string; override;

    procedure Handle(AContext: TMiddlewareContext; const AChain: TMiddlewareChain);
  end;

/// <summary>
///   True when AUrl addresses the metrics endpoint: the path, query string
///   ignored, with or without a trailing slash.
/// </summary>
function IsMetricsUrl(const AUrl: string): Boolean;

implementation

uses
  MCPConnect.Configuration.MCP,
  MCPConnect.Transport.Base,
  MCPConnect.Transport.MediaType,

  Server.Metrics,
  Server.Metrics.Document;

{ TMetricsHttpMiddleware --------------------------------------------------- }

class function TMetricsHttpMiddleware.DefaultPriority: Integer;
begin
  // CORS sits at MW_PRIORITY_AUTHENTICATION - 100 and the static token at
  // MW_PRIORITY_AUTHENTICATION: in between, the headers are already written and
  // the endpoint is still public.
  Result := MW_PRIORITY_AUTHENTICATION - 50;
end;

function TMetricsHttpMiddleware.GetName: string;
begin
  Result := 'Metrics (plain HTTP endpoint)';
end;

function IsMetricsUrl(const AUrl: string): Boolean;
var
  LPath: string;
  LQuery: Integer;
begin
  LPath := AUrl.Trim;
  LQuery := LPath.IndexOf('?');
  if LQuery >= 0 then
    LPath := LPath.Substring(0, LQuery);

  if LPath.EndsWith('/') and (LPath.Length > 1) then
    LPath := LPath.Substring(0, LPath.Length - 1);

  Result := SameText(LPath, TMetricsHttpMiddleware.METRICS_PATH);
end;

procedure TMetricsHttpMiddleware.Handle(AContext: TMiddlewareContext;
  const AChain: TMiddlewareChain);
var
  LRequest: TMCPTransportRequest;
  LResponse: TMCPTransportResponse;
  LConfig: TMCPConfig;
  LName, LVersion, LDescription: string;
  LReport: TMetricsReport;
begin
  if not AContext.TryFind<TMCPTransportRequest>(LRequest) or
     not AContext.TryFind<TMCPTransportResponse>(LResponse) then
  begin
    AChain.Next(AContext);
    Exit;
  end;

  // Only its own request: every other verb, path and transport goes on to the
  // chain exactly as it did before this middleware existed.
  if (LRequest.Protocol = TTransportProtocol.Stdio) or
     (not SameText(LRequest.Command, 'GET')) or
     (not IsMetricsUrl(LRequest.Url)) then
  begin
    AChain.Next(AContext);
    Exit;
  end;

  LName := '';
  LVersion := '';
  LDescription := '';
  LConfig := AContext.Find<TMCPConfig>;
  if Assigned(LConfig) and (LConfig.Server <> nil) then
  begin
    LName := LConfig.Server.Name;
    LVersion := LConfig.Server.Version;
    LDescription := LConfig.Server.Description;
  end;

  LResponse.Code := HTTP_CODE_OK;
  LResponse.ContentType := TMediaType.APPLICATION_JSON;
  // The numbers are only true for the moment they were read: a cached copy
  // would be a lie a browser then tells its user.
  LResponse.SetHeader('Cache-Control', 'no-store');

  // Sample the host first, as the WebStencils dashboard does before each report:
  // the gauges are cleared by every harvest reset and only come back with the
  // next sample, so without this a poll landing in between finds none at all.
  TServerMetrics.Instance.SampleSystem;

  LReport := BuildMetricsReport(LName, LVersion, LDescription);
  try
    LResponse.Content := MetricsReportToJson(LReport);
  finally
    LReport.Free;
  end;

  // No Next: the request ends here. That is also what keeps the transport's 405
  // for GET out of the way - it is raised at the end of the chain, which this
  // never reaches.
end;

end.
