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
unit MCPConnect.Tests.MCP.Middleware;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  DUnitX.TestFramework,

  MCPConnect.JRPC.Middleware,
  MCPConnect.MCP.Middleware,
  MCPConnect.MCP.Types.Base,
  MCPConnect.MCP.Types.Tools,
  MCPConnect.MCP.Server;

type
  /// <summary>
  ///   Collects what every middleware in the chain does, so a test can assert
  ///   on the order in which they ran, going in and coming back out.
  /// </summary>
  TOperationTrace = class(TObject)
  private
    FSteps: TStringList;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(const AStep: string);
    function AsText: string;
  end;

  /// <summary>Base for the test middleware: writes its own name in and out.</summary>
  TTraceCallToolMiddleware = class(TMiddleware, ICallToolMiddleware)
  protected
    function Tag: string; virtual; abstract;
  public
    function Handle(AContext: TMiddlewareContext;
      AParams: TCallToolRequestParams; const AChain: TCallToolChain): TBaseResult;
  end;

  TAlphaMiddleware = class(TTraceCallToolMiddleware)
  protected
    function Tag: string; override;
  end;

  TBetaMiddleware = class(TTraceCallToolMiddleware)
  protected
    function Tag: string; override;
  end;

  /// <summary>Calls Next twice, the way a retry does.</summary>
  TRetryMiddleware = class(TMiddleware, ICallToolMiddleware)
  public
    function Handle(AContext: TMiddlewareContext;
      AParams: TCallToolRequestParams; const AChain: TCallToolChain): TBaseResult;
  end;

  /// <summary>Never calls Next: the operation is suppressed.</summary>
  TDenyMiddleware = class(TMiddleware, ICallToolMiddleware)
  public
    function Handle(AContext: TMiddlewareContext;
      AParams: TCallToolRequestParams; const AChain: TCallToolChain): TBaseResult;
  end;

  /// <summary>Takes part in tools/list only, to prove the chains do not mix.</summary>
  TListOnlyMiddleware = class(TMiddleware, IListToolsMiddleware)
  public
    function Handle(AContext: TMiddlewareContext;
      AParams: TPaginatedRequestParams; const AChain: TListToolsChain): TListToolsResult;
  end;

  [TestFixture]
  TMCPMiddlewareChainTest = class(TObject)
  private
    FTrace: TOperationTrace;
    FOwned: TObjectList<TBaseResult>;
    FTerminalCalls: Integer;

    /// <summary>The real handler the chain ends on.</summary>
    function Terminal(AContext: TMiddlewareContext;
      AParams: TCallToolRequestParams): TBaseResult;
    function Tail(const AHooks: array of ICallToolMiddleware): TCallToolChain;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestEmptyChainCallsTerminal();
    [Test]
    procedure TestChainNestsInAndOut();
    [Test]
    procedure TestNotCallingNextSuppressesTheOperation();

    // Calling Next more than once
    [Test]
    procedure TestNextTwiceWalksTheWholeTail();
    [Test]
    procedure TestNextTwiceRunsTheHandlerTwice();
    [Test]
    procedure TestNextTwiceReturnsTheLastResult();
    [Test]
    procedure TestCallersCursorIsNotMoved();
  end;

  [TestFixture]
  TMCPHookRegistrationTest = class(TObject)
  private
    FServer: TMCPServer;
    function CountFor(const AIID: TGUID): Integer;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestHooksAreIsolatedByGuid();
  end;

implementation

threadvar
  GTrace: TOperationTrace;

{ TOperationTrace }

constructor TOperationTrace.Create;
begin
  inherited Create;
  FSteps := TStringList.Create();
end;

destructor TOperationTrace.Destroy;
begin
  FSteps.Free;
  inherited;
end;

procedure TOperationTrace.Add(const AStep: string);
begin
  FSteps.Add(AStep);
end;

function TOperationTrace.AsText: string;
begin
  Result := string.Join(' ', FSteps.ToStringArray);
end;

{ TTraceCallToolMiddleware }

function TTraceCallToolMiddleware.Handle(AContext: TMiddlewareContext;
  AParams: TCallToolRequestParams; const AChain: TCallToolChain): TBaseResult;
begin
  GTrace.Add(Tag + '>');
  try
    Result := AChain.Next(AContext, AParams);
  finally
    GTrace.Add('<' + Tag);
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

{ TRetryMiddleware }

function TRetryMiddleware.Handle(AContext: TMiddlewareContext;
  AParams: TCallToolRequestParams; const AChain: TCallToolChain): TBaseResult;
begin
  AChain.Next(AContext, AParams);
  Result := AChain.Next(AContext, AParams);
end;

{ TDenyMiddleware }

function TDenyMiddleware.Handle(AContext: TMiddlewareContext;
  AParams: TCallToolRequestParams; const AChain: TCallToolChain): TBaseResult;
begin
  GTrace.Add('denied');
  Result := nil;
end;

{ TListOnlyMiddleware }

function TListOnlyMiddleware.Handle(AContext: TMiddlewareContext;
  AParams: TPaginatedRequestParams; const AChain: TListToolsChain): TListToolsResult;
begin
  Result := AChain.Next(AContext, AParams);
end;

{ TMCPMiddlewareChainTest }

procedure TMCPMiddlewareChainTest.Setup;
begin
  FTrace := TOperationTrace.Create();
  GTrace := FTrace;
  // The results a real operation returns belong to the garbage collector of the
  // request; here the fixture stands in for it.
  FOwned := TObjectList<TBaseResult>.Create(True);
  FTerminalCalls := 0;
end;

procedure TMCPMiddlewareChainTest.TearDown;
begin
  GTrace := nil;
  FOwned.Free;
  FTrace.Free;
end;

function TMCPMiddlewareChainTest.Terminal(AContext: TMiddlewareContext;
  AParams: TCallToolRequestParams): TBaseResult;
begin
  FTrace.Add('*');
  Inc(FTerminalCalls);

  // A fresh result per call, so a test can tell one attempt from the next.
  Result := TBaseResult.Create;
  FOwned.Add(Result);
end;

function TMCPMiddlewareChainTest.Tail(
  const AHooks: array of ICallToolMiddleware): TCallToolChain;
var
  LChain: TArray<IOperationMiddleware<TCallToolRequestParams, TBaseResult>>;
  LIndex: Integer;
begin
  SetLength(LChain, Length(AHooks));
  for LIndex := 0 to High(AHooks) do
    LChain[LIndex] := AHooks[LIndex];

  Result := TCallToolChain.Create(LChain, Terminal);
end;

procedure TMCPMiddlewareChainTest.TestEmptyChainCallsTerminal;
var
  LChain: TCallToolChain;
begin
  LChain := TCallToolChain.Create(nil, Terminal);

  Assert.IsNotNull(LChain.Next(nil, nil), 'the terminal result must come back');
  Assert.AreEqual('*', FTrace.AsText);
end;

procedure TMCPMiddlewareChainTest.TestChainNestsInAndOut;
var
  LChain: TCallToolChain;
begin
  LChain := Tail([TAlphaMiddleware.Create, TBetaMiddleware.Create]);
  LChain.Next(nil, nil);

  // First in the array is outermost: first going in, last coming out.
  Assert.AreEqual('A> B> * <B <A', FTrace.AsText);
end;

procedure TMCPMiddlewareChainTest.TestNotCallingNextSuppressesTheOperation;
var
  LChain: TCallToolChain;
begin
  LChain := Tail([TDenyMiddleware.Create, TAlphaMiddleware.Create]);
  LChain.Next(nil, nil);

  Assert.AreEqual('denied', FTrace.AsText,
    'neither the tail nor the handler may run');
  Assert.AreEqual(0, FTerminalCalls);
end;

procedure TMCPMiddlewareChainTest.TestNextTwiceWalksTheWholeTail;
var
  LChain: TCallToolChain;
begin
  // The cursor is a record passed by value, so calling Next twice walks the
  // same tail both times instead of resuming where the first call stopped.
  LChain := Tail([TRetryMiddleware.Create, TAlphaMiddleware.Create,
    TBetaMiddleware.Create]);
  LChain.Next(nil, nil);

  Assert.AreEqual('A> B> * <B <A A> B> * <B <A', FTrace.AsText);
end;

procedure TMCPMiddlewareChainTest.TestNextTwiceRunsTheHandlerTwice;
var
  LChain: TCallToolChain;
begin
  LChain := Tail([TRetryMiddleware.Create, TAlphaMiddleware.Create]);
  LChain.Next(nil, nil);

  // Retrying re-executes the operation for real: this is why only an idempotent
  // one may be retried.
  Assert.AreEqual(2, FTerminalCalls);
end;

procedure TMCPMiddlewareChainTest.TestNextTwiceReturnsTheLastResult;
var
  LChain: TCallToolChain;
  LResult: TBaseResult;
begin
  LChain := Tail([TRetryMiddleware.Create]);
  LResult := LChain.Next(nil, nil);

  Assert.AreEqual(2, FOwned.Count, 'each attempt produces its own result');
  Assert.AreSame(FOwned.Last, LResult,
    'the caller gets the result of the attempt the middleware kept');
  Assert.AreNotSame(FOwned.First, LResult,
    'the discarded result is not the one returned, and is freed with the request');
end;

procedure TMCPMiddlewareChainTest.TestCallersCursorIsNotMoved;
var
  LChain: TCallToolChain;
begin
  // Same guarantee seen from outside the chain: the caller's own cursor is not
  // moved either, so the whole chain can be walked again.
  LChain := Tail([TAlphaMiddleware.Create]);
  LChain.Next(nil, nil);
  LChain.Next(nil, nil);

  Assert.AreEqual('A> * <A A> * <A', FTrace.AsText);
end;

{ TMCPHookRegistrationTest }

procedure TMCPHookRegistrationTest.Setup;
begin
  FServer := TMCPServer.Create(nil);
end;

procedure TMCPHookRegistrationTest.TearDown;
begin
  FServer.Free;
end;

function TMCPHookRegistrationTest.CountFor(const AIID: TGUID): Integer;
begin
  Result := Length(FServer.Middleware.EntriesFor(AIID));
end;

procedure TMCPHookRegistrationTest.TestHooksAreIsolatedByGuid;
begin
  FServer.Middleware
    .Add(TAlphaMiddleware)      // tools/call only
    .Add(TListOnlyMiddleware);  // tools/list only

  // Each hook interface carries its own GUID, which is what Supports matches
  // on. Were they plain aliases of the generic ancestor they would all share
  // GUID_NULL, and every middleware would end up in every chain.
  Assert.AreEqual(1, CountFor(ICallToolMiddleware), 'tools/call chain');
  Assert.AreEqual(1, CountFor(IListToolsMiddleware), 'tools/list chain');
  Assert.AreEqual(0, CountFor(IReadResourceMiddleware), 'resources/read chain');
  Assert.AreEqual(0, CountFor(IGetPromptMiddleware), 'prompts/get chain');
  Assert.AreEqual(0, CountFor(IDiscoverMiddleware), 'server/discover chain');
end;

initialization
  TDUnitX.RegisterTestFixture(TMCPMiddlewareChainTest);
  TDUnitX.RegisterTestFixture(TMCPHookRegistrationTest);

end.
