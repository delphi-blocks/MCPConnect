{******************************************************************************}
{                                                                              }
{  Delphi MCP Connect Library                                                  }
{                                                                              }
{  Copyright (c) Paolo Rossi <dev@paolorossi.net>                              }
{  All rights reserved.                                                        }
{                                                                              }
{  https://github.com/delphi-blocks/MCPConnect                                 }
{                                                                              }
{  Licensed under the MIT license                                              }
{                                                                              }
{******************************************************************************}
unit MCPConnect.Tests.MCP.SubscriptionRegistry;

interface

uses
  System.SysUtils, System.JSON, System.Generics.Collections, System.IOUtils,
  DUnitX.TestFramework,

  JRPC.Core,
  MCPConnect.MCP.Server,
  MCPConnect.Configuration.MCP,
  MCPConnect.MCP.Types.Base,
  MCPConnect.MCP.Types.Notifications,
  MCPConnect.MCP.Types.Subscriptions;

type
  /// <summary>
  ///   Exercises TMCPSubscriptionRegistry directly, with no transport and no
  ///   request: what a notification raised anywhere at all becomes when there
  ///   are streams to deliver it to.
  /// </summary>
  [TestFixture]
  TMCPSubscriptionRegistryTest = class(TObject)
  private
    FRegistry: TMCPSubscriptionRegistry;
    FOne: TMCPMessageQueue;
    FTwo: TMCPMessageQueue;

    /// <summary>
    ///   Everything this fixture opened, so that TearDown can free it.
    /// </summary>
    /// <remarks>
    ///   The registry deliberately does not free a subscription when it closes
    ///   it: the pump that owns the stream reads the State afterwards to decide
    ///   whether the client is owed the listen result, and frees it when it is
    ///   done. In production the pump does that; here the fixture does, or the
    ///   suite reports every subscription as a leak.
    ///
    ///   Owned, and the teardown closes each one *before* freeing it rather than
    ///   calling CloseAll: a close the test already made, followed by a second
    ///   one, is the double free the registry's own contract avoids.
    ///
    ///   Not an owning list, because the two are separate steps here: close,
    ///   then free. Owning would free on the list's destruction whatever the
    ///   close did or did not do.
    /// </remarks>
    FOpened: TList<TMCPSubscription>;

    /// <summary>
    ///   Extra queues a test made for itself, so that TearDown frees them after
    ///   the subscriptions that point at them. Not owned here, for the same
    ///   reason as FOpened: close, then free, in that order.
    /// </summary>
    FQueues: TList<TMCPMessageQueue>;

    /// <summary>
    ///   Opens a subscription on its own queue, owning everything it is given.
    /// </summary>
    function Subscribe(AId: TJSONValue; AFilter: TSubscriptionFilter;
      AQueue: TMCPMessageQueue): TMCPSubscription;

    /// <summary>A filter that asked for nothing at all.</summary>
    function EmptyFilter: TSubscriptionFilter;

    /// <summary>A filter that asked for the tool list.</summary>
    function ToolsFilter: TSubscriptionFilter;

    /// <summary>A filter that asked for one resource uri.</summary>
    function ResourceFilter(const AUri: string): TSubscriptionFilter;

    /// <summary>The subscription id carried in a notification's "_meta", as JSON.</summary>
    function SubscriptionIdOf(ANotification: TJRPCNotification): string;

    /// <summary>
    ///   Publishes a freshly built notification and frees it, answering how many
    ///   subscriptions were sent to.
    /// </summary>
    /// <remarks>
    ///   A TJRPCNotification is an object the caller owns - the registry clones
    ///   it per subscriber and leaves the original alone - so
    ///   "Publish(TMCPNotification.ToolListChanged)" leaks the template the
    ///   moment the call returns. This is the shape to write it in.
    /// </remarks>
    function Published(ANotification: TJRPCNotification): Integer;

    /// <summary>Dequeues everything and frees it, answering how many there were.</summary>
    function Drain(AQueue: TMCPMessageQueue): Integer;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestIdKeepsItsJsonType;
    [Test]
    procedure TestOpenRefusesADuplicateId;
    [Test]
    procedure TestFindAndCloseUntangleTheIds;
    [Test]
    procedure TestPublishClonesPerSubscriber;
    [Test]
    procedure TestPublishStampsEachSubscribersOwnId;
    [Test]
    procedure TestOptInIsPerType;
    [Test]
    procedure TestPublishLeavesTheTemplateAlone;
    [Test]
    procedure TestResourceUpdatesMatchTheirUriExactly;
    [Test]
    procedure TestPublishingToNobodyIsFree;
    [Test]
    procedure TestOverflowClosesTheSubscription;
  end;

  /// <summary>
  ///   The publishers of TMCPSubscriptionsConfig: what the Tools, Prompts and
  ///   Resources sections raise when they commit a change, and the two rules
  ///   the register/unregister path has to honour - opt-in, and one
  ///   notification per class rather than one per method.
  /// </summary>
  [TestFixture]
  TMCPSubscriptionsConfigTest = class(TObject)
  private
    FServer: TMCPServer;
    FConfig: IMCPConfig;
    FToolsQueue: TMCPMessageQueue;
    FPromptsQueue: TMCPMessageQueue;
    FResourcesQueue: TMCPMessageQueue;
    FReadmeQueue: TMCPMessageQueue;

    /// <summary>Everything this fixture opened, for TearDown to free.</summary>
    FOpened: TList<TMCPSubscription>;

    function SubscribeTo(AId: Int64; AFilter: TSubscriptionFilter;
      AQueue: TMCPMessageQueue): TMCPSubscription;

    /// <summary>Dequeues everything and frees it, answering how many there were.</summary>
    function Drain(AQueue: TMCPMessageQueue): Integer;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestRegisterClassAnnouncesOnceNotPerTool;
    [Test]
    procedure TestUnregisterToolAnnounces;
    [Test]
    procedure TestClearAllAnnouncesOnce;
    [Test]
    procedure TestUnregisterClassAnnounces;
    [Test]
    procedure TestResourceRegistrationAnnounces;
    [Test]
    procedure TestResourceUpdatedAnnouncesOnlyForAServedUri;
    [Test]
    procedure TestResourceUpdatedReachesOnlyTheUriSubscriber;
    [Test]
    procedure TestAPublisherDoesNotReachAMuteSubscription;
  end;

implementation

uses
  MCPConnect.MCP.Attributes;

type
  /// <summary>
  ///   Two tools, one resource and one prompt, so that a section's batch has
  ///   something to coalesce and the opt-in rules have something to filter.
  /// </summary>
  [McpScope('substest')]
  TSubscribedClass = class
  public
    [McpTool('ping', 'Ping')]
    function Ping: string;

    [McpTool('pong', 'Pong')]
    function Pong: string;

    [McpResource('readme', 'res://substest/readme', 'text/plain')]
    function Readme: string;

    [McpPrompt('greet', 'Greet', 'Greets someone')]
    function Greet: string;
  end;

function TSubscribedClass.Ping: string;
begin
  Result := 'pong';
end;

function TSubscribedClass.Pong: string;
begin
  Result := 'pong';
end;

function TSubscribedClass.Readme: string;
begin
  Result := 'hello';
end;

function TSubscribedClass.Greet: string;
begin
  Result := 'hello';
end;

{ TMCPSubscriptionRegistryTest }

procedure TMCPSubscriptionRegistryTest.Setup;
begin
  FRegistry := TMCPSubscriptionRegistry.Create;
  FOpened := TList<TMCPSubscription>.Create;
  FQueues := TList<TMCPMessageQueue>.Create;
  FOne := TMCPMessageQueue.Create;
  FTwo := TMCPMessageQueue.Create;
end;

procedure TMCPSubscriptionRegistryTest.TearDown;
begin
  // Close first and free second, exactly as the pump does: the id lives on the
  // subscription, so freeing it before closing it would hand Close a dangling
  // pointer to read the key from.
  for var LSubscription in FOpened do
  begin
    // A close the test already made is not an error: Close answers whether it
    // found the subscription, and ignores an id that is gone.
    FRegistry.Close(LSubscription.Id, False);
    LSubscription.Free;
  end;

  FRegistry.Free;
  FOpened.Free;

  // Messages a test published but did not read, and then the queues themselves:
  // a queue frees what it still holds when it is destroyed, so this is belt and
  // braces - it is what makes a leftover message show up as a mistake here
  // rather than as a leak at shutdown.
  Drain(FOne);
  Drain(FTwo);

  for var LQueue in FQueues do
  begin
    Drain(LQueue);
    LQueue.Free;
  end;

  FQueues.Free;

  FTwo.Free;
  FOne.Free;
end;

function TMCPSubscriptionRegistryTest.EmptyFilter: TSubscriptionFilter;
begin
  Result := TSubscriptionFilter.Create;
end;

function TMCPSubscriptionRegistryTest.ToolsFilter: TSubscriptionFilter;
begin
  Result := TSubscriptionFilter.Create;
  Result.ToolsListChanged := True;
end;

function TMCPSubscriptionRegistryTest.ResourceFilter(const AUri: string): TSubscriptionFilter;
begin
  Result := TSubscriptionFilter.Create;
  Result.ResourceSubscriptions := [AUri];
end;

function TMCPSubscriptionRegistryTest.Subscribe(AId: TJSONValue;
  AFilter: TSubscriptionFilter; AQueue: TMCPMessageQueue): TMCPSubscription;
begin
  Result := FRegistry.Open(AId, AFilter, AQueue);
  FOpened.Add(Result);
end;

function TMCPSubscriptionRegistryTest.SubscriptionIdOf(
  ANotification: TJRPCNotification): string;
var
  LParams, LMeta: TJSONObject;
begin
  LParams := ANotification.Params as TJSONObject;
  LMeta := LParams.GetValue('_meta') as TJSONObject;

  Result := LMeta.GetValue(MCP_META_SUBSCRIPTION_ID).ToJSON;
end;

function TMCPSubscriptionRegistryTest.Drain(AQueue: TMCPMessageQueue): Integer;
var
  LMessage: TJRPCMessage;
begin
  Result := 0;
  repeat
    LMessage := AQueue.Dequeue;
    if Assigned(LMessage) then
    begin
      Inc(Result);
      LMessage.Free;
    end;
  until not Assigned(LMessage);
end;

function TMCPSubscriptionRegistryTest.Published(
  ANotification: TJRPCNotification): Integer;
begin
  try
    Result := FRegistry.Publish(ANotification);
  finally
    ANotification.Free;
  end;
end;

procedure TMCPSubscriptionRegistryTest.TestIdKeepsItsJsonType;
var
  LNumber, LString: TMCPSubscription;
  LNumberId, LStringId: TJSONValue;
begin
  // The specification is explicit that the subscription id *is* the listen
  // request's id, so 7 and "7" are two different subscriptions - which is the
  // one thing AsString alone could never tell apart.
  LNumberId := TJSONNumber.Create(7);
  LStringId := TJSONString.Create('7');
  try
    LNumber := Subscribe(LNumberId, EmptyFilter, FOne);
    LString := Subscribe(LStringId, EmptyFilter, FTwo);
  finally
    LStringId.Free;
    LNumberId.Free;
  end;

  Assert.IsNotNull(LNumber, 'The integer id must open a subscription of its own');
  Assert.IsNotNull(LString, 'The string id must open a subscription of its own');
  Assert.AreNotEqual(LNumber, LString);
  Assert.AreEqual(2, FRegistry.Count, 'Two ids that differ in type are two subscriptions');
end;

procedure TMCPSubscriptionRegistryTest.TestOpenRefusesADuplicateId;
var
  LNewId, LOpenId: TJSONNumber;
  LNewFilter: TSubscriptionFilter;
begin
  LOpenId := TJSONNumber.Create(1);
  try
    Subscribe(LOpenId, EmptyFilter, FOne);
  finally
    LOpenId.Free;
  end;

  // Owned by the test: a refused Open must not take what it was handed
  LNewId := TJSONNumber.Create(1);
  LNewFilter := EmptyFilter;
  try
    Assert.WillRaise(
      procedure
      begin
        FRegistry.Open(LNewId, LNewFilter, FTwo);
      end,
      EMCPException);
  finally
    LNewFilter.Free;
    LNewId.Free;
  end;
end;

procedure TMCPSubscriptionRegistryTest.TestFindAndCloseUntangleTheIds;
var
  LNumber: TMCPSubscription;
  LNumberId, LUnknownId, LStringId: TJSONValue;
begin
  LNumberId := TJSONNumber.Create(1);
  LStringId := TJSONString.Create('1');
  LUnknownId := TJSONString.Create('2');
  try
    LNumber := Subscribe(LNumberId, EmptyFilter, FOne);
    Subscribe(LStringId, EmptyFilter, FTwo);

    Assert.AreEqual(LNumber, FRegistry.Find(LNumberId));
    Assert.IsNull(FRegistry.Find(LUnknownId), 'An unknown id has no subscription');

    Assert.IsTrue(FRegistry.Close(LNumberId, True));
    Assert.AreEqual(TMCPSubscriptionState.ClosedGracefully, LNumber.State);
    Assert.IsNull(FRegistry.Find(LNumberId));

    // The other subscription is untouched, and its type is what told the two apart
    Assert.AreEqual(1, FRegistry.Count);
    Assert.IsNotNull(FRegistry.Find(LStringId));
  finally
    LUnknownId.Free;
    LStringId.Free;
    LNumberId.Free;
  end;
end;

procedure TMCPSubscriptionRegistryTest.TestPublishClonesPerSubscriber;
var
  LOneId, LTwoId: TJSONValue;
begin
  LOneId := TJSONNumber.Create(1);
  LTwoId := TJSONNumber.Create(2);
  try
    Subscribe(LOneId, ToolsFilter, FOne);
    Subscribe(LTwoId, ToolsFilter, FTwo);
  finally
    LTwoId.Free;
    LOneId.Free;
  end;

  Assert.AreEqual(2, Published(TMCPNotification.ToolListChanged));

  Assert.AreEqual(1, FOne.Count);
  Assert.AreEqual(1, FTwo.Count);
  // Each queue owns its message, so both can be drained and freed without the
  // other's copy going with it - the double free a shared instance would cause
  Assert.AreEqual(1, Drain(FOne));
  Assert.AreEqual(1, Drain(FTwo));
end;

procedure TMCPSubscriptionRegistryTest.TestPublishStampsEachSubscribersOwnId;
var
  LNotification: TJRPCNotification;
  LStamped: TArray<string>;
begin
  var LNumberId := TJSONNumber.Create(7);
  var LStringId := TJSONString.Create('sub-a');
  try
    Subscribe(LNumberId, ToolsFilter, FOne);
    Subscribe(LStringId, ToolsFilter, FTwo);
  finally
    LStringId.Free;
    LNumberId.Free;
  end;

  // A notification built for no subscription in particular carries no "_meta"
  // at all - there would be nothing in it - so the stamp is what creates the
  // member, not what fills one in.
  LNotification := TMCPNotification.ToolListChanged;
  try
    Assert.IsNull((LNotification.Params as TJSONObject).GetValue('_meta'),
      'the template must not carry a "_meta": there is no subscription to name yet');

    FRegistry.Publish(LNotification);
    Assert.IsNull((LNotification.Params as TJSONObject).GetValue('_meta'),
      'the template must be left alone: the caller owns it and may publish it again');
  finally
    LNotification.Free;
  end;

  // Preconditions, asserted rather than dereferenced: a queue that came back
  // empty would otherwise fail as an access violation inside SubscriptionIdOf,
  // which says nothing about what went wrong.
  Assert.AreEqual(1, FOne.Count, 'The first subscription was not sent to');
  Assert.AreEqual(1, FTwo.Count, 'The second subscription was not sent to');

  // Indexed by the subscription, not by the registry's iteration order, so the
  // assertion is about the stamping and not about which one came first.
  var LMessage := FOne.Dequeue;
  try
    LStamped := LStamped + [SubscriptionIdOf(LMessage as TJRPCNotification)];
  finally
    LMessage.Free;
  end;

  LMessage := FTwo.Dequeue;
  try
    LStamped := LStamped + [SubscriptionIdOf(LMessage as TJRPCNotification)];
  finally
    LMessage.Free;
  end;

  Assert.AreEqual(2, Length(LStamped));
  Assert.AreEqual('7', LStamped[0], 'The integer id must go out as a number');
  Assert.AreEqual('"sub-a"', LStamped[1], 'The string id must go out as a string');
end;

procedure TMCPSubscriptionRegistryTest.TestOptInIsPerType;
var
  LOnlyToolsId, LSilentId: TJSONValue;
begin
  LOnlyToolsId := TJSONNumber.Create(1);
  LSilentId := TJSONNumber.Create(2);
  try
    Subscribe(LOnlyToolsId, ToolsFilter, FOne);
    Subscribe(LSilentId, EmptyFilter, FTwo);
  finally
    LSilentId.Free;
    LOnlyToolsId.Free;
  end;

  // A subscription that asked for nothing gets nothing, whatever is published.
  // Published, not Publish: a notification is an object the caller owns, and
  // handing the factory's result straight to Publish leaks it once the call
  // drops the only reference to it.
  Assert.AreEqual(1, Published(TMCPNotification.ToolListChanged),
    'Only the subscription that opted in may be sent to');
  Assert.AreEqual(1, FOne.Count);
  Assert.AreEqual(0, FTwo.Count);

  Assert.AreEqual(0, Published(TMCPNotification.PromptListChanged),
    'A type nobody asked for reaches nobody');
  Assert.AreEqual(0, Published(TMCPNotification.ResourceListChanged));
end;

procedure TMCPSubscriptionRegistryTest.TestPublishLeavesTheTemplateAlone;
var
  LTemplate: TJRPCNotification;
  LBefore: string;
  LId: TJSONValue;
begin
  LId := TJSONNumber.Create(1);
  try
    Subscribe(LId, ToolsFilter, FOne);
  finally
    LId.Free;
  end;

  LTemplate := TMCPNotification.ToolListChanged;
  try
    LBefore := LTemplate.Params.ToJSON;
    FRegistry.Publish(LTemplate);

    Assert.AreEqual(LBefore, LTemplate.Params.ToJSON,
      'The caller owns the template and may publish it again: no id may be stamped into it');
  finally
    LTemplate.Free;
    Drain(FOne);
  end;
end;

procedure TMCPSubscriptionRegistryTest.TestResourceUpdatesMatchTheirUriExactly;
var
  LId: TJSONValue;
begin
  LId := TJSONNumber.Create(1);
  try
    Subscribe(LId, ResourceFilter('res://a'), FOne);
  finally
    LId.Free;
  end;

  Assert.AreEqual(1, Published(TMCPNotification.ResourceUpdated('res://a')));

  // A uri is an identifier, not a label: a child resource and a differently
  // cased one are both different resources
  Assert.AreEqual(0, Published(TMCPNotification.ResourceUpdated('res://a/b')));
  Assert.AreEqual(0, Published(TMCPNotification.ResourceUpdated('res://AB')));
  Assert.AreEqual(1, FOne.Count);
end;

procedure TMCPSubscriptionRegistryTest.TestPublishingToNobodyIsFree;
begin
  Assert.AreEqual(0, Published(TMCPNotification.ToolListChanged));
end;

procedure TMCPSubscriptionRegistryTest.TestOverflowClosesTheSubscription;
var
  LSmallQueue: TMCPMessageQueue;
  LId: TJSONValue;
begin
  // A short queue stands in for the 64-message one a subscription gets: what is
  // under test is the policy, not the number. CloseOnDrop is what a subscription
  // sets on its queue, and what makes the cap a thing the pump can see.
  LSmallQueue := TMCPMessageQueue.Create(2);
  LSmallQueue.CloseOnDrop := True;
  FQueues.Add(LSmallQueue);

  LId := TJSONNumber.Create(1);
  try
    Subscribe(LId, ToolsFilter, LSmallQueue);
  finally
    LId.Free;
  end;

  // Two fit, and the third is the one that overflows. The base queue drops its
  // oldest message and closes itself, which is the pump's cue to end the
  // stream - a client that stopped reading has missed a change it will never
  // ask about again.
  Assert.AreEqual(1, Published(TMCPNotification.ToolListChanged));
  Assert.AreEqual(1, Published(TMCPNotification.ToolListChanged));
  Assert.IsFalse(LSmallQueue.Closed, 'The queue is not full yet');

  Assert.AreEqual(1, Published(TMCPNotification.ToolListChanged));
  Assert.IsTrue(LSmallQueue.Closed,
    'A queue at its cap must close, not silently drop: a missed invalidation is worse than a reconnect');

  // And it is the *oldest* message that went, which is what "cap and drop"
  // means: the three publishes left the two newest behind
  Assert.AreEqual(2, Drain(LSmallQueue));
end;

{ TMCPSubscriptionsConfigTest }

procedure TMCPSubscriptionsConfigTest.Setup;
begin
  FServer := TMCPServer.Create(nil);
  FConfig := FServer.Plugin.Configure<IMCPConfig>;
  FConfig.ApplyConfig;

  FOpened := TList<TMCPSubscription>.Create;
  FToolsQueue := TMCPMessageQueue.Create;
  FPromptsQueue := TMCPMessageQueue.Create;
  FResourcesQueue := TMCPMessageQueue.Create;
  FReadmeQueue := TMCPMessageQueue.Create;

  // One subscription per interest, so that a notification meant for one proves
  // it did not go to the others.
  var LToolsFilter := TSubscriptionFilter.Create;
  LToolsFilter.ToolsListChanged := True;
  SubscribeTo(1, LToolsFilter, FToolsQueue);

  var LPromptsFilter := TSubscriptionFilter.Create;
  LPromptsFilter.PromptsListChanged := True;
  SubscribeTo(2, LPromptsFilter, FPromptsQueue);

  var LResourcesFilter := TSubscriptionFilter.Create;
  LResourcesFilter.ResourcesListChanged := True;
  SubscribeTo(3, LResourcesFilter, FResourcesQueue);

  // A list filter and a resource subscription are not the same opt-in, which is
  // what makes this one a case worth having on its own
  var LReadmeFilter := TSubscriptionFilter.Create;
  LReadmeFilter.ResourceSubscriptions := ['res://substest/readme'];
  SubscribeTo(4, LReadmeFilter, FReadmeQueue);
end;

procedure TMCPSubscriptionsConfigTest.TearDown;
begin
  // The registry must not outlive the queues: closing first is the invariant
  // the transport's pump holds, and freeing afterwards is what the pump does
  // with the subscription once it has read the State.
  for var LSubscription in FOpened do
  begin
    FConfig.Subscriptions.Registry.Close(LSubscription.Id, False);
    LSubscription.Free;
  end;

  FOpened.Free;

  // Messages a test published but did not read. A queue frees what it holds
  // when it is destroyed, so this is belt and braces: it is what makes a
  // leftover message show up here rather than as a leak at shutdown.
  Drain(FReadmeQueue);
  Drain(FResourcesQueue);
  Drain(FPromptsQueue);
  Drain(FToolsQueue);

  FReadmeQueue.Free;
  FResourcesQueue.Free;
  FPromptsQueue.Free;
  FToolsQueue.Free;

  FConfig := nil;
  FServer.Free;
end;

function TMCPSubscriptionsConfigTest.SubscribeTo(AId: Int64;
  AFilter: TSubscriptionFilter; AQueue: TMCPMessageQueue): TMCPSubscription;
var
  LId: TJSONValue;
begin
  // Open copies the id and takes the filter, so the id is this fixture's to free
  LId := TJSONNumber.Create(AId);
  try
    Result := FConfig.Subscriptions.Registry.Open(LId, AFilter, AQueue);
  finally
    LId.Free;
  end;

  FOpened.Add(Result);
end;

function TMCPSubscriptionsConfigTest.Drain(AQueue: TMCPMessageQueue): Integer;
var
  LMessage: TJRPCMessage;
begin
  Result := 0;
  repeat
    LMessage := AQueue.Dequeue;
    if Assigned(LMessage) then
    begin
      Inc(Result);
      LMessage.Free;
    end;
  until not Assigned(LMessage);
end;

procedure TMCPSubscriptionsConfigTest.TestRegisterClassAnnouncesOnceNotPerTool;
begin
  FConfig.Tools.RegisterClass(TSubscribedClass);

  // Two tools, one notification: the batch is the point. A dozen tools would
  // otherwise mean a dozen identical "the tool list changed" messages.
  Assert.AreEqual(1, Drain(FToolsQueue),
    'Registering a class is one change to the list, however many methods it declares');
  Assert.AreEqual(0, Drain(FPromptsQueue), 'The prompt list did not change');
  Assert.AreEqual(0, Drain(FResourcesQueue), 'The resource list did not change');
end;

procedure TMCPSubscriptionsConfigTest.TestUnregisterToolAnnounces;
begin
  FConfig.Tools.RegisterClass(TSubscribedClass);
  Drain(FToolsQueue);

  FConfig.Tools.UnregisterTool('substest_ping');

  Assert.AreEqual(1, Drain(FToolsQueue));
end;

procedure TMCPSubscriptionsConfigTest.TestClearAllAnnouncesOnce;
begin
  FConfig.Tools.RegisterClass(TSubscribedClass);
  Drain(FToolsQueue);

  FConfig.Tools.ClearAll;

  // Two tools cleared is still one list, and one list change is one message
  Assert.AreEqual(1, Drain(FToolsQueue));
end;

procedure TMCPSubscriptionsConfigTest.TestUnregisterClassAnnounces;
begin
  FConfig.Tools.RegisterClass(TSubscribedClass);
  Drain(FToolsQueue);

  FConfig.Tools.UnregisterClass(TSubscribedClass);

  Assert.AreEqual(1, Drain(FToolsQueue));
end;

procedure TMCPSubscriptionsConfigTest.TestResourceRegistrationAnnounces;
begin
  FConfig.Resources.RegisterClass(TSubscribedClass);

  Assert.AreEqual(1, Drain(FResourcesQueue));
  Assert.AreEqual(0, Drain(FToolsQueue), 'The tool list did not change');
end;

procedure TMCPSubscriptionsConfigTest.TestResourceUpdatedAnnouncesOnlyForAServedUri;
begin
  FConfig.Resources.RegisterClass(TSubscribedClass);
  Drain(FReadmeQueue);

  // The uri the client subscribed to, and which this server serves
  FConfig.Subscriptions.ResourceUpdated('res://substest/readme');
  Assert.AreEqual(1, Drain(FReadmeQueue));

  // A uri nobody serves was never subscribable, and announcing it would point
  // the client at something it cannot read
  FConfig.Subscriptions.ResourceUpdated('res://substest/nothing');
  Assert.AreEqual(0, Drain(FReadmeQueue));
end;

procedure TMCPSubscriptionsConfigTest.TestResourceUpdatedReachesOnlyTheUriSubscriber;
begin
  FConfig.Resources.RegisterClass(TSubscribedClass);
  Drain(FReadmeQueue);
  Drain(FResourcesQueue);

  FConfig.Subscriptions.ResourceUpdated('res://substest/readme');

  // The subscription that asked for the resource list, and the ones that asked
  // about tools and prompts, are different opt-ins from this one
  Assert.AreEqual(1, Drain(FReadmeQueue));
  Assert.AreEqual(0, Drain(FResourcesQueue),
    'A list-changed subscription is not a resource subscription');
  Assert.AreEqual(0, Drain(FToolsQueue));
  Assert.AreEqual(0, Drain(FPromptsQueue));
end;

procedure TMCPSubscriptionsConfigTest.TestAPublisherDoesNotReachAMuteSubscription;
var
  LMuteQueue: TMCPMessageQueue;
begin
  // A subscription that acknowledged nothing - the shape a client gets when it
  // asks only for prompts, say, and then the tool list changes
  LMuteQueue := TMCPMessageQueue.Create;
  try
    SubscribeTo(99, TSubscriptionFilter.Create, LMuteQueue);

    FConfig.Subscriptions.ToolsChanged;
    FConfig.Subscriptions.PromptsChanged;
    FConfig.Subscriptions.ResourcesChanged;

    Assert.AreEqual(0, Drain(LMuteQueue),
      'The server MUST NOT send a notification type the client did not request');
  finally
    LMuteQueue.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TMCPSubscriptionRegistryTest);
  TDUnitX.RegisterTestFixture(TMCPSubscriptionsConfigTest);

end.
