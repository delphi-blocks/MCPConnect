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
///   Drives a real request through TMCPTransportHandler with middleware registered,
///   so that what is covered is the wiring itself: that the chains actually run
///   around the dispatch, in the right order and at the right level, and that a
///   middleware can refuse a call or observe the answer.
/// </summary>
unit MCPConnect.Tests.Transport.Middleware;

interface

uses
  System.SysUtils, System.Classes, System.JSON,
  DUnitX.TestFramework,

  MCPConnect.Configuration.MCP,
  MCPConnect.Transport.Base,
  JRPC.Core,
  MCPConnect.JRPC.Middleware,
  MCPConnect.MCP.Server,
  MCPConnect.MCP.Middleware,
  MCPConnect.MCP.Types.Base,
  MCPConnect.MCP.Types.Tools,
  MCPConnect.MCP.Attributes;

type
  /// <summary>A response writer that streams nothing: everything comes back
  ///   through the response converter.</summary>
  TSilentWriter = class(TInterfacedObject, IMCPTransportWriter)
  public
    procedure Write(const AValue: string; const AEventId: string = '');
    function Connected: Boolean;
    function SupportsStreaming: Boolean;
  end;

  /// <summary>Writes its own name going in and coming out of both levels.</summary>
  TTraceMiddleware = class(TMiddleware, IMessageMiddleware, IRequestMiddleware)
  protected
    function Tag: string; virtual; abstract;
  public
    procedure IMessageMiddleware.Handle = OnMessage;
    procedure IRequestMiddleware.Handle = OnRequest;

    procedure OnMessage(AContext: TMiddlewareContext; const AChain: TMiddlewareChain);
    procedure OnRequest(AContext: TMiddlewareContext; const AChain: TMiddlewareChain);
  end;

  TAlphaMiddleware = class(TTraceMiddleware)
  protected
    function Tag: string; override;
  end;

  TBetaMiddleware = class(TTraceMiddleware)
  protected
    function Tag: string; override;
  end;

  /// <summary>Refuses the call before the dispatch ever runs.</summary>
  TDenyMiddleware = class(TMiddleware, IRequestMiddleware)
  public
    procedure Handle(AContext: TMiddlewareContext; const AChain: TMiddlewareChain);
  end;

  /// <summary>
  ///   Reads the answer after the dispatch, the way a logging middleware does.
  /// </summary>
  TObserverMiddleware = class(TMiddleware, IMessageMiddleware)
  public
    procedure Handle(AContext: TMiddlewareContext; const AChain: TMiddlewareChain);
  end;

  /// <summary>Refuses by raising, which is the documented way to answer an error.</summary>
  TRaisingMiddleware = class(TMiddleware, IRequestMiddleware)
  public const
    Reason = 'refused by the test middleware';
  public
    procedure Handle(AContext: TMiddlewareContext; const AChain: TMiddlewareChain);
  end;

  /// <summary>
  ///   Keeps, in a plain field, something worked out at the message level and read
  ///   again at the request level: the point of one instance per message.
  /// </summary>
  TCarryOverMiddleware = class(TMiddleware, IMessageMiddleware, IRequestMiddleware)
  private
    FSeenMethod: string;
  public
    procedure IMessageMiddleware.Handle = OnMessage;
    procedure IRequestMiddleware.Handle = OnRequest;

    procedure OnMessage(AContext: TMiddlewareContext; const AChain: TMiddlewareChain);
    procedure OnRequest(AContext: TMiddlewareContext; const AChain: TMiddlewareChain);
  end;

  /// <summary>
  ///   Takes part in the request level and in the tools/call level, to show that
  ///   one instance carries what it learns from the outer level to the inner one.
  /// </summary>
  TToolTraceMiddleware = class(TMiddleware, IRequestMiddleware, ICallToolMiddleware)
  private
    FMethod: string;
  public
    procedure IRequestMiddleware.Handle = OnRequest;
    function ICallToolMiddleware.Handle = OnCallTool;

    procedure OnRequest(AContext: TMiddlewareContext; const AChain: TMiddlewareChain);
    function OnCallTool(AContext: TMiddlewareContext;
      AParams: TCallToolRequestParams; const AChain: TCallToolChain): TBaseResult;
  end;

  /// <summary>
  ///   Takes part in every MCP operation with a single hook, and says which one
  ///   it is reading AContext.Method.
  /// </summary>
  TEveryOperationMiddleware = class(TMiddleware, IMCPMiddleware)
  public
    function Handle(AContext: TMiddlewareContext; AParams: TRequestMetaParams;
      const AChain: TMCPChain): TBaseResult;
  end;

  /// <summary>Universal, and it enriches the result of whatever ran.</summary>
  TStampMiddleware = class(TMiddleware, IMCPMiddleware)
  public const
    Stamp = 'stamped-by-the-universal-hook';
  public
    function Handle(AContext: TMiddlewareContext; AParams: TRequestMetaParams;
      const AChain: TMCPChain): TBaseResult;
  end;

  /// <summary>Universal, and it refuses before the operation runs.</summary>
  TUniversalDenyMiddleware = class(TMiddleware, IMCPMiddleware)
  public
    function Handle(AContext: TMiddlewareContext; AParams: TRequestMetaParams;
      const AChain: TMCPChain): TBaseResult;
  end;

  /// <summary>
  ///   Universal, answering with a class the operation cannot use. It has to be
  ///   tried on an operation with a concrete result type: the three that return
  ///   TBaseResult accept any descendant by definition, so there is nothing to
  ///   violate there.
  /// </summary>
  TUniversalWrongClassMiddleware = class(TMiddleware, IMCPMiddleware)
  public
    function Handle(AContext: TMiddlewareContext; AParams: TRequestMetaParams;
      const AChain: TMCPChain): TBaseResult;
  end;

  /// <summary>Refuses one tool by name, without ever reaching it.</summary>
  TToolAclMiddleware = class(TMiddleware, ICallToolMiddleware)
  public const
    Forbidden = 'demo_forbidden';
  public
    function Handle(AContext: TMiddlewareContext;
      AParams: TCallToolRequestParams; const AChain: TCallToolChain): TBaseResult;
  end;

  /// <summary>Drops one tool from tools/list, per caller.</summary>
  TToolFilterMiddleware = class(TMiddleware, IListToolsMiddleware)
  public
    function Handle(AContext: TMiddlewareContext;
      AParams: TPaginatedRequestParams; const AChain: TListToolsChain): TListToolsResult;
  end;

  /// <summary>A tool class with two tools, to have something to filter.</summary>
  TDemoTools = class(TObject)
  public
    [McpTool('demo_allowed', 'A tool a caller may use')]
    function Allowed: string;

    [McpTool('demo_forbidden', 'A tool the acl refuses')]
    function Forbidden: string;
  end;

  [TestFixture]
  TTransportMiddlewareTest = class(TObject)
  private
    FServer: TMCPServer;
    /// <summary>POSTs a JSON-RPC body and returns what came back.</summary>
    function Post(const ABody: string): string;
    /// <summary>A request every server answers: this branch has no "initialize".</summary>
    function DiscoverBody: string;
    function CallToolBody(const AName: string): string;
    function ListToolsBody: string;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestNoMiddlewareLeavesDispatchUntouched();
    [Test]
    procedure TestChainsRunAroundTheDispatch();
    [Test]
    procedure TestLevelsNestMessageThenRequest();
    [Test]
    procedure TestDenyingMiddlewareStopsTheDispatch();
    [Test]
    procedure TestRaisingMiddlewareBecomesAnErrorResponse();
    [Test]
    procedure TestOneInstanceServesBothLevels();
    [Test]
    procedure TestSharedObjectReachesTheChain();
    [Test]
    procedure TestProducedIsVisibleAfterTheDispatch();
    [Test]
    procedure TestReadingProducedDoesNotStealTheAnswer();
    [Test]
    procedure TestCallToolHookWrapsTheTool();
    [Test]
    procedure TestCallToolHookCanRefuseByName();
    [Test]
    procedure TestListToolsHookFiltersTheResult();
    [Test]
    procedure TestOneInstanceCarriesAcrossLevels();

    // IMCPMiddleware: one hook for every operation
    [Test]
    procedure TestUniversalHookSeesEveryOperation();
    [Test]
    procedure TestUniversalHookWrapsTheSpecificOne();
    [Test]
    procedure TestUniversalHookEnrichesAnyResult();
    [Test]
    procedure TestUniversalHookCanRefuse();
    [Test]
    procedure TestUniversalHookAnsweringTheWrongClassIsAnError();
  end;

implementation

var
  /// <summary>
  ///   Where the test middleware write. Deliberately a plain global and not a
  ///   threadvar: HandleMessage runs on the thread CreateAsyncThread starts, so
  ///   a per-thread copy would be nil exactly where the chain runs. Fixtures run
  ///   one at a time, and ProcessRequest only returns once that thread is done.
  /// </summary>
  GTrace: TStringList;

function Trace: string;
begin
  Result := string.Join(' ', GTrace.ToStringArray);
end;

{ TSilentWriter }

procedure TSilentWriter.Write(const AValue: string; const AEventId: string);
begin
  // Nothing streams in these tests.
end;

function TSilentWriter.Connected: Boolean;
begin
  Result := True;
end;

function TSilentWriter.SupportsStreaming: Boolean;
begin
  Result := False;
end;

{ TTraceMiddleware }

procedure TTraceMiddleware.OnMessage(AContext: TMiddlewareContext;
  const AChain: TMiddlewareChain);
begin
  GTrace.Add(Tag + 'msg>');
  try
    AChain.Next(AContext);
  finally
    GTrace.Add('<' + Tag + 'msg');
  end;
end;

procedure TTraceMiddleware.OnRequest(AContext: TMiddlewareContext;
  const AChain: TMiddlewareChain);
begin
  GTrace.Add(Tag + 'req>');
  try
    AChain.Next(AContext);
  finally
    GTrace.Add('<' + Tag + 'req');
  end;
end;

function TAlphaMiddleware.Tag: string;
begin
  Result := 'A';
end;

function TBetaMiddleware.Tag: string;
begin
  Result := 'B';
end;

{ TDenyMiddleware }

procedure TDenyMiddleware.Handle(AContext: TMiddlewareContext;
  const AChain: TMiddlewareChain);
begin
  GTrace.Add('denied');
  // Next is never called: the operation is suppressed.
end;

{ TDemoTools }

function TDemoTools.Allowed: string;
begin
  Result := 'allowed';
end;

function TDemoTools.Forbidden: string;
begin
  Result := 'forbidden';
end;

{ TToolTraceMiddleware }

procedure TToolTraceMiddleware.OnRequest(AContext: TMiddlewareContext;
  const AChain: TMiddlewareChain);
begin
  FMethod := AContext.Method;
  AChain.Next(AContext);
end;

function TToolTraceMiddleware.OnCallTool(AContext: TMiddlewareContext;
  AParams: TCallToolRequestParams; const AChain: TCallToolChain): TBaseResult;
begin
  // FMethod was written at the request level of this same message.
  GTrace.Add('tool>' + AParams.Name + '@' + FMethod);
  try
    Result := AChain.Next(AContext, AParams);
  finally
    GTrace.Add('<tool');
  end;
end;

{ TEveryOperationMiddleware }

function TEveryOperationMiddleware.Handle(AContext: TMiddlewareContext;
  AParams: TRequestMetaParams; const AChain: TMCPChain): TBaseResult;
begin
  GTrace.Add('any>' + AContext.Method);
  try
    Result := AChain.Next(AContext, AParams);
  finally
    GTrace.Add('<any');
  end;
end;

{ TStampMiddleware }

function TStampMiddleware.Handle(AContext: TMiddlewareContext;
  AParams: TRequestMetaParams; const AChain: TMCPChain): TBaseResult;
begin
  // The result is the live object, whatever operation produced it.
  Result := AChain.Next(AContext, AParams);
  if Assigned(Result) then
    Result.ResultMeta.ServerInfo.Name := Stamp;
end;

{ TUniversalDenyMiddleware }

function TUniversalDenyMiddleware.Handle(AContext: TMiddlewareContext;
  AParams: TRequestMetaParams; const AChain: TMCPChain): TBaseResult;
begin
  GTrace.Add('denied:' + AContext.Method);
  Result := nil;
end;

{ TUniversalWrongClassMiddleware }

function TUniversalWrongClassMiddleware.Handle(AContext: TMiddlewareContext;
  AParams: TRequestMetaParams; const AChain: TMCPChain): TBaseResult;
begin
  // What we throw away is ours: only the object the operation finally returns
  // reaches the garbage collector of the request.
  AContext.Own(AChain.Next(AContext, AParams));

  // tools/list works on TListToolsResult, so this one cannot stand in for it
  Result := TCallToolResult.Create;
  AContext.Own(Result);   // whatever happens next, the request frees it
end;

{ TToolAclMiddleware }

function TToolAclMiddleware.Handle(AContext: TMiddlewareContext;
  AParams: TCallToolRequestParams; const AChain: TCallToolChain): TBaseResult;
begin
  if AParams.Name = Forbidden then
    raise EJRPCException.CreateFmt('tool [%s] is not allowed', [AParams.Name]);

  Result := AChain.Next(AContext, AParams);
end;

{ TToolFilterMiddleware }

function TToolFilterMiddleware.Handle(AContext: TMiddlewareContext;
  AParams: TPaginatedRequestParams; const AChain: TListToolsChain): TListToolsResult;
var
  LIndex: Integer;
begin
  Result := AChain.Next(AContext, AParams);

  // Removing is safe: the list does not own the tools. Changing one would not
  // be, since those are the objects of the registry.
  for LIndex := Result.Tools.Count - 1 downto 0 do
    if Result.Tools[LIndex].Name = TToolAclMiddleware.Forbidden then
      Result.Tools.Delete(LIndex);
end;

{ TObserverMiddleware }

procedure TObserverMiddleware.Handle(AContext: TMiddlewareContext;
  const AChain: TMiddlewareChain);
var
  LMessage: TJRPCMessage;
begin
  AChain.Next(AContext);

  for LMessage in AContext.Produced do
    GTrace.Add('produced:' + LMessage.ClassName);
end;

{ TRaisingMiddleware }

procedure TRaisingMiddleware.Handle(AContext: TMiddlewareContext;
  const AChain: TMiddlewareChain);
begin
  raise EJRPCException.Create(Reason);
end;

{ TCarryOverMiddleware }

procedure TCarryOverMiddleware.OnMessage(AContext: TMiddlewareContext;
  const AChain: TMiddlewareChain);
begin
  FSeenMethod := AContext.Method;
  AChain.Next(AContext);
end;

procedure TCarryOverMiddleware.OnRequest(AContext: TMiddlewareContext;
  const AChain: TMiddlewareChain);
begin
  GTrace.Add('carried:' + FSeenMethod);
  AChain.Next(AContext);
end;

{ TTransportMiddlewareTest }

procedure TTransportMiddlewareTest.Setup;
begin
  GTrace := TStringList.Create();
  FServer := TMCPServer.Create(nil);

  FServer.Plugin.Configure<IMCPConfig>
    .Server
      .SetName('middleware-test')
      .SetVersion('1.0.0')
    .BackToMCP
    .Security
      // These tests are about the chain, and count what is in it: the
      // request-metadata header check would be one more middleware, and one
      // more set of headers on every body below.
      .SetHeaderValidation(TMCPHeaderValidation.Off)
    .BackToMCP
    .Tools
      .RegisterClass(TDemoTools)
    .BackToMCP
  .ApplyConfig;
end;

procedure TTransportMiddlewareTest.TearDown;
begin
  FServer.Free;
  FreeAndNil(GTrace);
end;

function TTransportMiddlewareTest.DiscoverBody: string;
begin
  Result := '{"jsonrpc":"2.0","id":1,"method":"server/discover","params":{}}';
end;

function TTransportMiddlewareTest.Post(const ABody: string): string;
var
  LHandler: TMCPTransportHandler;
  LContent: string;
begin
  LContent := '';

  LHandler := TMCPTransportHandler.Create(FServer, TSilentWriter.Create);
  try
    LHandler.ProcessRequest(
      procedure (ARequest: TMCPTransportRequest)
      begin
        ARequest.Url := '/';
        ARequest.Command := 'POST';
        ARequest.Protocol := TTransportProtocol.StreamableHTTP;
        ARequest.Content := ABody;
        ARequest.Accept := 'application/json';
      end,
      procedure (AResponse: TMCPTransportResponse)
      begin
        LContent := AResponse.Content;
      end);
  finally
    LHandler.Free;
  end;

  Result := LContent;
end;

procedure TTransportMiddlewareTest.TestNoMiddlewareLeavesDispatchUntouched;
begin
  // The fast path: with nothing registered the handler must behave as before.
  Assert.Contains(Post(DiscoverBody), '"result"');
  Assert.AreEqual('', Trace, 'no middleware, nothing traced');
end;

procedure TTransportMiddlewareTest.TestChainsRunAroundTheDispatch;
begin
  FServer.Middleware.Add(TAlphaMiddleware);

  Assert.Contains(Post(DiscoverBody), '"result"',
    'the dispatch still has to produce its answer');
  Assert.AreEqual('Amsg> Areq> <Areq <Amsg', Trace);
end;

procedure TTransportMiddlewareTest.TestLevelsNestMessageThenRequest;
begin
  FServer.Middleware
    .Add(TAlphaMiddleware)
    .Add(TBetaMiddleware);

  Post(DiscoverBody);

  // Every OnMessage runs before any OnRequest: the nesting is by level, not by
  // middleware, because the two hook sites are separated by the JRPC dispatch.
  Assert.AreEqual(
    'Amsg> Bmsg> Areq> Breq> <Breq <Areq <Bmsg <Amsg', Trace);
end;

procedure TTransportMiddlewareTest.TestDenyingMiddlewareStopsTheDispatch;
var
  LResponse: string;
begin
  FServer.Middleware
    .Add(TDenyMiddleware, MW_PRIORITY_AUTHORIZATION)
    .Add(TAlphaMiddleware);

  LResponse := Post(DiscoverBody);

  Assert.AreEqual('Amsg> denied <Amsg', Trace,
    'the request level below the refusal must not run');
  Assert.DoesNotContain(LResponse, '"result"',
    'the api method must never have been reached');
end;

procedure TTransportMiddlewareTest.TestRaisingMiddlewareBecomesAnErrorResponse;
var
  LResponse: string;
begin
  FServer.Middleware.Add(TAlphaMiddleware);
  FServer.Middleware.Add(TDenyMiddleware, MW_PRIORITY_USER + 1);

  // A middleware that raises must come back as a JSON-RPC error, the same way an
  // api method raising does, and the middleware above it must still see it go by.
  FServer.Middleware.Clear;
  FServer.Middleware
    .Add(TAlphaMiddleware)
    .Add(TRaisingMiddleware);

  LResponse := Post(DiscoverBody);

  Assert.Contains(LResponse, '"error"');
  Assert.Contains(Trace, '<Amsg', 'the outer middleware still unwinds');
end;

procedure TTransportMiddlewareTest.TestOneInstanceServesBothLevels;
begin
  FServer.Middleware.Add(TCarryOverMiddleware);

  Post(DiscoverBody);

  // The field written in OnMessage is still there in OnRequest: one instance
  // serves the whole message.
  Assert.AreEqual('carried:server/discover', Trace);
end;

procedure TTransportMiddlewareTest.TestSharedObjectReachesTheChain;
begin
  FServer.Middleware.AddShared(TStringList.Create);

  Assert.AreEqual(1, Length(FServer.Middleware.SharedObjects));
  Assert.Contains(Post(DiscoverBody), '"result"',
    'a shared object must not disturb a plain request');
end;

procedure TTransportMiddlewareTest.TestProducedIsVisibleAfterTheDispatch;
begin
  FServer.Middleware.Add(TObserverMiddleware);

  Post(DiscoverBody);

  // The queue is drained by the thread writing to the client while the handler
  // is still running, so the answer has to be recorded as it goes by rather
  // than read off the queue afterwards.
  Assert.AreEqual('produced:TJRPCResponse', Trace);
end;

procedure TTransportMiddlewareTest.TestReadingProducedDoesNotStealTheAnswer;
begin
  FServer.Middleware.Add(TObserverMiddleware);

  // Looking at the answer must not consume it: the client still gets it.
  Assert.Contains(Post(DiscoverBody), '"result"');
end;

function TTransportMiddlewareTest.CallToolBody(const AName: string): string;
begin
  Result := Format(
    '{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"%s","arguments":{}}}',
    [AName]);
end;

function TTransportMiddlewareTest.ListToolsBody: string;
begin
  Result := '{"jsonrpc":"2.0","id":1,"method":"tools/list","params":{}}';
end;

procedure TTransportMiddlewareTest.TestCallToolHookWrapsTheTool;
begin
  FServer.Middleware.Add(TToolTraceMiddleware);

  Assert.Contains(Post(CallToolBody('demo_allowed')), 'allowed');
  Assert.AreEqual('tool>demo_allowed@tools/call <tool', Trace);
end;

procedure TTransportMiddlewareTest.TestCallToolHookCanRefuseByName;
var
  LResponse: string;
begin
  FServer.Middleware.Add(TToolAclMiddleware, MW_PRIORITY_AUTHORIZATION);

  LResponse := Post(CallToolBody(TToolAclMiddleware.Forbidden));
  Assert.Contains(LResponse, '"error"');
  Assert.Contains(LResponse, 'is not allowed');

  // The tool the acl allows still goes through untouched.
  Assert.Contains(Post(CallToolBody('demo_allowed')), 'allowed');
end;

procedure TTransportMiddlewareTest.TestListToolsHookFiltersTheResult;
var
  LResponse: string;
begin
  LResponse := Post(ListToolsBody);
  Assert.Contains(LResponse, 'demo_forbidden', 'both tools are there to start with');

  FServer.Middleware.Add(TToolFilterMiddleware);

  LResponse := Post(ListToolsBody);
  Assert.Contains(LResponse, 'demo_allowed');
  Assert.DoesNotContain(LResponse, 'demo_forbidden');
end;

procedure TTransportMiddlewareTest.TestOneInstanceCarriesAcrossLevels;
begin
  FServer.Middleware.Add(TToolTraceMiddleware);

  Post(CallToolBody('demo_allowed'));

  // "@tools/call" is what OnRequest put in a field of the very same instance
  // that OnCallTool then ran on.
  Assert.Contains(Trace, '@tools/call');
end;

procedure TTransportMiddlewareTest.TestUniversalHookSeesEveryOperation;
begin
  FServer.Middleware.Add(TEveryOperationMiddleware);

  // One class, one Handle, three different operations.
  Post(DiscoverBody);
  Post(ListToolsBody);
  Post(CallToolBody('demo_allowed'));

  Assert.AreEqual(
    'any>server/discover <any any>tools/list <any any>tools/call <any', Trace);
end;

procedure TTransportMiddlewareTest.TestUniversalHookWrapsTheSpecificOne;
begin
  FServer.Middleware
    .Add(TEveryOperationMiddleware)
    .Add(TToolAclMiddleware);

  Post(CallToolBody('demo_allowed'));

  // The universal hook is a level of its own, outside the operation one.
  Assert.StartsWith('any>tools/call', Trace);
  Assert.EndsWith('<any', Trace);
end;

procedure TTransportMiddlewareTest.TestUniversalHookEnrichesAnyResult;
var
  LResponse: string;
begin
  FServer.Middleware.Add(TStampMiddleware);

  // Same middleware, same line of code, two unrelated result classes.
  LResponse := Post(ListToolsBody);
  Assert.Contains(LResponse, TStampMiddleware.Stamp, 'tools/list');

  LResponse := Post(DiscoverBody);
  Assert.Contains(LResponse, TStampMiddleware.Stamp, 'server/discover');
end;

procedure TTransportMiddlewareTest.TestUniversalHookCanRefuse;
begin
  FServer.Middleware
    .Add(TUniversalDenyMiddleware)
    .Add(TToolAclMiddleware);

  Post(CallToolBody('demo_allowed'));

  // Neither the operation hook nor the tool ran.
  Assert.AreEqual('denied:tools/call', Trace);
end;

procedure TTransportMiddlewareTest.TestUniversalHookAnsweringTheWrongClassIsAnError;
var
  LResponse: string;
begin
  FServer.Middleware.Add(TUniversalWrongClassMiddleware);

  LResponse := Post(ListToolsBody);

  // A universal middleware handing back a class the operation cannot use is a
  // server side bug, and it must say so rather than corrupt the answer.
  Assert.Contains(LResponse, '"error"');
  Assert.DoesNotContain(LResponse, '"result"');
end;

initialization
  TDUnitX.RegisterTestFixture(TTransportMiddlewareTest);

end.
