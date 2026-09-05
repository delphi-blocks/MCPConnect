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
unit MCPConnect.Tests.JRPC.Middleware;

interface

uses
  System.SysUtils, System.Classes,
  DUnitX.TestFramework,

  MCPConnect.JRPC.Core,
  MCPConnect.JRPC.Middleware,
  MCPConnect.JRPC.Server;

type
  /// <summary>
  ///   Collects what every middleware in the chain does, so a test can assert
  ///   on the order in which they ran, going in and coming back out.
  /// </summary>
  TMiddlewareTrace = class(TObject)
  private
    FSteps: TStringList;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(const AStep: string);
    function AsText: string;
  end;

  /// <summary>Base for the test middleware: writes its own name in and out.</summary>
  TTraceMiddleware = class(TMiddleware, IRequestMiddleware)
  protected
    function Tag: string; virtual; abstract;
  public
    procedure OnRequest(AContext: TMiddlewareContext;
      const AChain: TRequestChain);
  end;

  TFirstMiddleware = class(TTraceMiddleware)
  protected
    function Tag: string; override;
  end;

  TSecondMiddleware = class(TTraceMiddleware)
  protected
    function Tag: string; override;
  end;

  TThirdMiddleware = class(TTraceMiddleware)
  protected
    function Tag: string; override;
  end;

  /// <summary>Declares a priority of its own, so it must run first.</summary>
  TOuterMiddleware = class(TTraceMiddleware)
  protected
    function Tag: string; override;
  public
    class function DefaultPriority: Integer; override;
  end;

  /// <summary>Takes part in no chain: it implements no hook interface.</summary>
  TInertMiddleware = class(TMiddleware);

  /// <summary>Calls Next twice, to prove the cursor is re-entrant.</summary>
  TRetryMiddleware = class(TMiddleware, IRequestMiddleware)
  public
    procedure OnRequest(AContext: TMiddlewareContext;
      const AChain: TRequestChain);
  end;

  [TestFixture]
  TMiddlewareListTest = class(TObject)
  private
    FServer: TJRPCServer;
    function Names: string;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestEmptyByDefault();
    [Test]
    procedure TestAddKeepsRegistrationOrder();
    [Test]
    procedure TestDefaultPriorityMovesOutward();
    [Test]
    procedure TestExplicitPriorityWins();
    [Test]
    procedure TestEqualPriorityIsStable();
    [Test]
    procedure TestOnlyHookImplementorsAreInTheChain();
    [Test]
    procedure TestRemoveAndClear();
    [Test]
    procedure TestBackToAppReturnsTheServer();
    [Test]
    procedure TestFactoryIsUsedWhenGiven();
    [Test]
    procedure TestRemoveSharedStopsHandingItOut();
    [Test]
    procedure TestClearLeavesSharedObjectsAlone();
  end;

  [TestFixture]
  TMiddlewareChainTest = class(TObject)
  private
    FTrace: TMiddlewareTrace;
    procedure Terminal(AContext: TMiddlewareContext);
    function BuildChain(const AEntries: TMiddlewareEntries): TRequestChain;
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
    procedure TestCursorIsReentrant();
  end;

implementation

uses
  MCPConnect.JRPC.Classes;

threadvar
  GTrace: TMiddlewareTrace;

{ TMiddlewareTrace }

constructor TMiddlewareTrace.Create;
begin
  inherited Create;
  FSteps := TStringList.Create();
end;

destructor TMiddlewareTrace.Destroy;
begin
  FSteps.Free;
  inherited;
end;

procedure TMiddlewareTrace.Add(const AStep: string);
begin
  FSteps.Add(AStep);
end;

function TMiddlewareTrace.AsText: string;
begin
  Result := string.Join(' ', FSteps.ToStringArray);
end;

{ TTraceMiddleware }

procedure TTraceMiddleware.OnRequest(AContext: TMiddlewareContext;
  const AChain: TRequestChain);
begin
  GTrace.Add(Tag + '>');
  try
    AChain.Next(AContext);
  finally
    GTrace.Add('<' + Tag);
  end;
end;

function TFirstMiddleware.Tag: string;
begin
  Result := 'A';
end;

function TSecondMiddleware.Tag: string;
begin
  Result := 'B';
end;

function TThirdMiddleware.Tag: string;
begin
  Result := 'C';
end;

function TOuterMiddleware.Tag: string;
begin
  Result := 'OUT';
end;

class function TOuterMiddleware.DefaultPriority: Integer;
begin
  Result := MW_PRIORITY_ERROR_HANDLING;
end;

{ TRetryMiddleware }

procedure TRetryMiddleware.OnRequest(AContext: TMiddlewareContext;
  const AChain: TRequestChain);
begin
  AChain.Next(AContext);
  AChain.Next(AContext);
end;

{ TMiddlewareListTest }

procedure TMiddlewareListTest.Setup;
begin
  FServer := TJRPCServer.Create(nil);
end;

procedure TMiddlewareListTest.TearDown;
begin
  FServer.Free;
end;

function TMiddlewareListTest.Names: string;
var
  LEntry: TMiddlewareEntry;
  LResult: TStringList;
begin
  LResult := TStringList.Create();
  try
    for LEntry in FServer.Middleware.EntriesFor(IRequestMiddleware) do
      LResult.Add(LEntry.MiddlewareClass.ClassName);
    Result := string.Join(' ', LResult.ToStringArray);
  finally
    LResult.Free;
  end;
end;

procedure TMiddlewareListTest.TestEmptyByDefault;
begin
  Assert.AreEqual(0, FServer.Middleware.Count);
  Assert.AreEqual(0, Length(FServer.Middleware.EntriesFor(IRequestMiddleware)));
end;

procedure TMiddlewareListTest.TestAddKeepsRegistrationOrder;
begin
  FServer.Middleware
    .Add(TFirstMiddleware)
    .Add(TSecondMiddleware)
    .Add(TThirdMiddleware);

  Assert.AreEqual(3, FServer.Middleware.Count);
  Assert.AreEqual('TFirstMiddleware TSecondMiddleware TThirdMiddleware', Names);
end;

procedure TMiddlewareListTest.TestDefaultPriorityMovesOutward;
begin
  // Registered last, but it declares MW_PRIORITY_ERROR_HANDLING, so it must
  // come out first: this is what spares the caller from knowing the order.
  FServer.Middleware
    .Add(TFirstMiddleware)
    .Add(TSecondMiddleware)
    .Add(TOuterMiddleware);

  Assert.AreEqual('TOuterMiddleware TFirstMiddleware TSecondMiddleware', Names);
end;

procedure TMiddlewareListTest.TestExplicitPriorityWins;
begin
  // TOuterMiddleware asks for ERROR_HANDLING, but the registration overrides it.
  FServer.Middleware
    .Add(TFirstMiddleware)
    .Add(TOuterMiddleware, MW_PRIORITY_USER + 1);

  Assert.AreEqual('TFirstMiddleware TOuterMiddleware', Names);
end;

procedure TMiddlewareListTest.TestEqualPriorityIsStable;
begin
  FServer.Middleware
    .Add(TThirdMiddleware, MW_PRIORITY_AUTHORIZATION)
    .Add(TFirstMiddleware, MW_PRIORITY_AUTHORIZATION)
    .Add(TSecondMiddleware, MW_PRIORITY_AUTHORIZATION);

  // Same priority: registration order must survive the sort.
  Assert.AreEqual('TThirdMiddleware TFirstMiddleware TSecondMiddleware', Names);
end;

procedure TMiddlewareListTest.TestOnlyHookImplementorsAreInTheChain;
begin
  FServer.Middleware
    .Add(TFirstMiddleware)
    .Add(TInertMiddleware)
    .Add(TSecondMiddleware);

  Assert.AreEqual(3, FServer.Middleware.Count, 'all of them are registered');
  // ...but the one implementing no hook interface is in no chain at all.
  Assert.AreEqual('TFirstMiddleware TSecondMiddleware', Names);
end;

procedure TMiddlewareListTest.TestRemoveAndClear;
begin
  FServer.Middleware
    .Add(TFirstMiddleware)
    .Add(TSecondMiddleware);

  FServer.Middleware.Remove(TFirstMiddleware);
  Assert.AreEqual('TSecondMiddleware', Names, 'Remove must refresh the chains');

  FServer.Middleware.Clear;
  Assert.AreEqual(0, FServer.Middleware.Count);
  Assert.AreEqual('', Names);
end;

procedure TMiddlewareListTest.TestBackToAppReturnsTheServer;
begin
  Assert.AreSame(FServer, FServer.Middleware.Add(TFirstMiddleware).BackToApp as TObject);
end;

procedure TMiddlewareListTest.TestFactoryIsUsedWhenGiven;
var
  LBuilt: Boolean;
  LEntries: TMiddlewareEntries;
begin
  LBuilt := False;
  FServer.Middleware.Add(TFirstMiddleware,
    function : IMiddleware
    begin
      LBuilt := True;
      Result := TFirstMiddleware.Create;
    end);

  LEntries := FServer.Middleware.EntriesFor(IRequestMiddleware);
  Assert.AreEqual(1, Length(LEntries));

  Assert.IsNotNull(LEntries[0].CreateInstance);
  Assert.IsTrue(LBuilt, 'the factory must be what builds the instance');
end;

procedure TMiddlewareListTest.TestRemoveSharedStopsHandingItOut;
var
  LShared: TObject;
begin
  LShared := TStringList.Create;
  FServer.Middleware.AddShared(LShared);
  Assert.AreEqual(1, Length(FServer.Middleware.SharedObjects));

  FServer.Middleware.RemoveShared(LShared);

  // No longer handed out to the requests...
  Assert.AreEqual(0, Length(FServer.Middleware.SharedObjects));
  // ...but not freed either: a request already running could still hold it, so
  // it stays owned and goes down with the server. TearDown frees the server,
  // and the leak check of the suite is what proves it.
end;

procedure TMiddlewareListTest.TestClearLeavesSharedObjectsAlone;
begin
  FServer.Middleware
    .Add(TFirstMiddleware)
    .AddShared(TStringList.Create);

  FServer.Middleware.Clear;

  Assert.AreEqual(0, FServer.Middleware.Count, 'the chain is emptied');
  Assert.AreEqual(1, Length(FServer.Middleware.SharedObjects),
    'the shared objects are a different axis: only RemoveShared withdraws them');
end;

{ TMiddlewareChainTest }

procedure TMiddlewareChainTest.Setup;
begin
  FTrace := TMiddlewareTrace.Create();
  GTrace := FTrace;
end;

procedure TMiddlewareChainTest.TearDown;
begin
  GTrace := nil;
  FTrace.Free;
end;

procedure TMiddlewareChainTest.Terminal(AContext: TMiddlewareContext);
begin
  FTrace.Add('*');
end;

function TMiddlewareChainTest.BuildChain(const AEntries: TMiddlewareEntries): TRequestChain;
var
  LIndex: Integer;
  LChain: TArray<IRequestMiddleware>;
begin
  SetLength(LChain, Length(AEntries));
  for LIndex := 0 to High(AEntries) do
    Supports(AEntries[LIndex].CreateInstance, IRequestMiddleware, LChain[LIndex]);

  Result := TRequestChain.Create(LChain, Terminal);
end;

procedure TMiddlewareChainTest.TestEmptyChainCallsTerminal;
var
  LChain: TRequestChain;
begin
  LChain := TRequestChain.Create(nil, Terminal);
  LChain.Next(nil);

  Assert.AreEqual('*', FTrace.AsText);
end;

procedure TMiddlewareChainTest.TestChainNestsInAndOut;
var
  LServer: TJRPCServer;
  LChain: TRequestChain;
begin
  LServer := TJRPCServer.Create(nil);
  try
    LServer.Middleware
      .Add(TFirstMiddleware)
      .Add(TSecondMiddleware);

    LChain := BuildChain(LServer.Middleware.EntriesFor(IRequestMiddleware));
    LChain.Next(nil);

    // First registered is outermost: first going in, last coming out.
    Assert.AreEqual('A> B> * <B <A', FTrace.AsText);
  finally
    LServer.Free;
  end;
end;

procedure TMiddlewareChainTest.TestCursorIsReentrant;
var
  LMiddleware: IRequestMiddleware;
  LChain: TRequestChain;
  LTail: TArray<IRequestMiddleware>;
begin
  // The cursor is a record passed by value, so calling Next twice must walk the
  // same tail of the chain both times instead of running off the end.
  SetLength(LTail, 1);
  LTail[0] := TFirstMiddleware.Create;

  LMiddleware := TRetryMiddleware.Create;
  LChain := TRequestChain.Create(LTail, Terminal);

  LMiddleware.OnRequest(nil, LChain);

  Assert.AreEqual('A> * <A A> * <A', FTrace.AsText);
end;

initialization
  TDUnitX.RegisterTestFixture(TMiddlewareListTest);
  TDUnitX.RegisterTestFixture(TMiddlewareChainTest);

end.
