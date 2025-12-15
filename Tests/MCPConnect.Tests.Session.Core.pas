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
unit MCPConnect.Tests.Session.Core;

interface

uses
  System.SysUtils, System.JSON, System.Rtti, System.DateUtils, System.SyncObjs,
  System.Generics.Collections,
  DUnitX.TestFramework,

  MCPConnect.Session.Core,
  MCPConnect.JRPC.Core;

type
  // Custom session class for testing
  TCustomTestSession = class(TSessionBase)
  private
    FUserName: string;
    FUserRole: string;
  public
    property UserName: string read FUserName write FUserName;
    property UserRole: string read FUserRole write FUserRole;
  end;

  [TestFixture]
  TSessionDataTest = class(TObject)
  private
    FSession: TSessionData;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    // Creation and destruction
    [Test]
    procedure TestCreate();
    [Test]
    procedure TestDataInitialized();

    // JSON data operations
    [Test]
    procedure TestAddSimpleValue();
    [Test]
    procedure TestAddMultipleValues();
    [Test]
    procedure TestRetrieveValue();
    [Test]
    procedure TestUpdateValue();
    [Test]
    procedure TestRemoveValue();
    [Test]
    procedure TestDataPersistence();

    // Complex data types
    [Test]
    procedure TestAddJSONObject();
    [Test]
    procedure TestAddJSONArray();
  end;

  [TestFixture]
  TSessionManagerTest = class(TObject)
  private
    FManager: TSessionManager;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    // Session creation
    [Test]
    procedure TestCreateSession();
    [Test]
    procedure TestCreateMultipleSessions();
    [Test]
    procedure TestSessionHasUniqueId();
    [Test]
    procedure TestSessionInitialTimestamps();

    // Session retrieval
    [Test]
    procedure TestGetSession();
    [Test]
    procedure TestTryGetSession();
    [Test]
    procedure TestGetSessionUpdatesLastAccess();
    [Test]
    procedure TestSessionExists();

    // Session destruction
    [Test]
    procedure TestDestroySession();
    [Test]
    procedure TestDestroyNonExistentSession();

    // Timeout and expiration
    [Test]
    procedure TestSessionTimeout();
    [Test]
    procedure TestExpiredSessionRemoved();
    [Test]
    procedure TestTimeoutConfiguration();

    // Custom session class
    [Test]
    procedure TestCustomSessionClass();

    // Error handling
    [Test]
    procedure TestGetNonExistentSessionRaisesException();
    [Test]
    procedure TestGetExpiredSessionRaisesException();
  end;

implementation

{ TSessionDataTest }

procedure TSessionDataTest.Setup;
begin
  FSession := TSessionData.Create;
end;

procedure TSessionDataTest.TearDown;
begin
  FSession.Free;
end;

{ Creation and Destruction Tests }

procedure TSessionDataTest.TestCreate;
begin
  Assert.IsNotNull(FSession, 'Session should be created');
  Assert.IsNotNull(FSession.Data, 'Data property should be initialized');
end;

procedure TSessionDataTest.TestDataInitialized;
begin
  Assert.IsTrue(FSession.Data is TJSONObject, 'Data should be a TJSONObject');
  Assert.AreEqual(0, FSession.Data.Count, 'Data should be empty initially');
end;

{ JSON Data Operations Tests }

procedure TSessionDataTest.TestAddSimpleValue;
begin
  FSession.Data.AddPair('username', 'john_doe');

  Assert.AreEqual(1, FSession.Data.Count, 'Data should contain one pair');
  Assert.AreEqual('john_doe', FSession.Data.GetValue<string>('username'), 'Username should match');
end;

procedure TSessionDataTest.TestAddMultipleValues;
begin
  FSession.Data.AddPair('username', 'john_doe');
  FSession.Data.AddPair('userId', TJSONNumber.Create(123));
  FSession.Data.AddPair('isActive', TJSONBool.Create(True));

  Assert.AreEqual(3, FSession.Data.Count, 'Data should contain three pairs');
  Assert.AreEqual('john_doe', FSession.Data.GetValue<string>('username'), 'Username should match');
  Assert.AreEqual(123, FSession.Data.GetValue<Integer>('userId'), 'UserId should match');
  Assert.AreEqual(True, FSession.Data.GetValue<Boolean>('isActive'), 'IsActive should be True');
end;

procedure TSessionDataTest.TestRetrieveValue;
var
  LValue: TJSONValue;
begin
  FSession.Data.AddPair('testKey', 'testValue');

  LValue := FSession.Data.GetValue('testKey');
  Assert.IsNotNull(LValue, 'Retrieved value should not be null');
  Assert.AreEqual('testValue', LValue.Value, 'Retrieved value should match');
end;

procedure TSessionDataTest.TestUpdateValue;
begin
  FSession.Data.AddPair('counter', TJSONNumber.Create(1));
  Assert.AreEqual(1, FSession.Data.GetValue<Integer>('counter'), 'Initial counter should be 1');

  // Update by removing and re-adding
  FSession.Data.RemovePair('counter').Free;
  FSession.Data.AddPair('counter', TJSONNumber.Create(2));

  Assert.AreEqual(2, FSession.Data.GetValue<Integer>('counter'), 'Updated counter should be 2');
end;

procedure TSessionDataTest.TestRemoveValue;
var
  LPair: TJSONPair;
begin
  FSession.Data.AddPair('toRemove', 'value');
  Assert.AreEqual(1, FSession.Data.Count, 'Data should have one pair');

  LPair := FSession.Data.RemovePair('toRemove');
  try
    Assert.IsNotNull(LPair, 'RemovePair should return the removed pair');
    Assert.AreEqual(0, FSession.Data.Count, 'Data should be empty after removal');
  finally
    LPair.Free;
  end;
end;

procedure TSessionDataTest.TestDataPersistence;
begin
  FSession.Data.AddPair('persistent', 'value');

  // Simulate multiple accesses
  Assert.AreEqual('value', FSession.Data.GetValue<string>('persistent'), 'First access should work');
  Assert.AreEqual('value', FSession.Data.GetValue<string>('persistent'), 'Second access should work');
  Assert.AreEqual(1, FSession.Data.Count, 'Data count should remain 1');
end;

{ Complex Data Types Tests }

procedure TSessionDataTest.TestAddJSONObject;
var
  LUserData: TJSONObject;
  LRetrieved: TJSONObject;
begin
  LUserData := TJSONObject.Create;
  LUserData.AddPair('name', 'John');
  LUserData.AddPair('age', TJSONNumber.Create(30));

  FSession.Data.AddPair('user', LUserData);

  Assert.AreEqual(1, FSession.Data.Count, 'Data should have one pair');

  LRetrieved := FSession.Data.GetValue<TJSONObject>('user');
  Assert.IsNotNull(LRetrieved, 'Retrieved object should not be null');
  Assert.AreEqual('John', LRetrieved.GetValue<string>('name'), 'User name should match');
  Assert.AreEqual(30, LRetrieved.GetValue<Integer>('age'), 'User age should match');
end;

procedure TSessionDataTest.TestAddJSONArray;
var
  LItems: TJSONArray;
  LRetrieved: TJSONArray;
begin
  LItems := TJSONArray.Create;
  LItems.Add('item1');
  LItems.Add('item2');
  LItems.Add('item3');

  FSession.Data.AddPair('items', LItems);

  LRetrieved := FSession.Data.GetValue<TJSONArray>('items');
  Assert.IsNotNull(LRetrieved, 'Retrieved array should not be null');
  Assert.AreEqual(3, LRetrieved.Count, 'Array should have 3 items');
  Assert.AreEqual('item1', LRetrieved.Items[0].Value, 'First item should match');
end;

{ TSessionManagerTest }

procedure TSessionManagerTest.Setup;
begin
  FManager := TSessionManager.Create;
  FManager.TimeoutMinutes := 30;
end;

procedure TSessionManagerTest.TearDown;
begin
  FManager.Free;
end;

{ Session Creation Tests }

procedure TSessionManagerTest.TestCreateSession;
var
  LSession: TSessionBase;
begin
  LSession := FManager.CreateSession;

  Assert.IsNotNull(LSession, 'Created session should not be null');
  Assert.IsNotEmpty(LSession.SessionId, 'Session should have an ID');
  Assert.IsTrue(LSession is TSessionData, 'Default session should be TSessionData');
end;

procedure TSessionManagerTest.TestCreateMultipleSessions;
var
  LSession1, LSession2: TSessionBase;
begin
  LSession1 := FManager.CreateSession;
  LSession2 := FManager.CreateSession;

  Assert.IsNotNull(LSession1, 'First session should not be null');
  Assert.IsNotNull(LSession2, 'Second session should not be null');
  Assert.AreNotEqual(LSession1.SessionId, LSession2.SessionId, 'Session IDs should be unique');
end;

procedure TSessionManagerTest.TestSessionHasUniqueId;
var
  LSessions: TList<TSessionBase>;
  I: Integer;
begin
  LSessions := TList<TSessionBase>.Create;
  try
    // Create 1000 sessions
    for I := 1 to 1000 do
      LSessions.Add(FManager.CreateSession);

    // Check all IDs are unique
    for I := 0 to LSessions.Count - 2 do
      for var J := I + 1 to LSessions.Count - 1 do
        Assert.AreNotEqual(LSessions[I].SessionId, LSessions[J].SessionId,
          Format('Session IDs %d and %d should be unique', [I, J]));
  finally
    LSessions.Free;
  end;
end;

procedure TSessionManagerTest.TestSessionInitialTimestamps;
var
  LSession: TSessionBase;
  LBefore, LAfter: TDateTime;
begin
  LBefore := Now;
  LSession := FManager.CreateSession;
  LAfter := Now;

  Assert.IsTrue(LSession.CreatedAt >= LBefore, 'CreatedAt should be >= before timestamp');
  Assert.IsTrue(LSession.CreatedAt <= LAfter, 'CreatedAt should be <= after timestamp');
  Assert.AreEqual(LSession.CreatedAt, LSession.LastAccessedAt, 'Initial timestamps should match');
end;

{ Session Retrieval Tests }

procedure TSessionManagerTest.TestGetSession;
var
  LCreated, LRetrieved: TSessionBase;
begin
  LCreated := FManager.CreateSession;
  LRetrieved := FManager.GetSession(LCreated.SessionId);

  Assert.IsNotNull(LRetrieved, 'Retrieved session should not be null');
  Assert.AreEqual(LCreated.SessionId, LRetrieved.SessionId, 'Session IDs should match');
  Assert.AreSame(LCreated, LRetrieved, 'Should return same session instance');
end;

procedure TSessionManagerTest.TestTryGetSession;
var
  LSession: TSessionBase;
  LCreated: TSessionBase;
  LFound: Boolean;
begin
  LCreated := FManager.CreateSession;

  LFound := FManager.TryGetSession(LCreated.SessionId, LSession);

  Assert.IsTrue(LFound, 'TryGetSession should return True for existing session');
  Assert.IsNotNull(LSession, 'Session should not be null');
  Assert.AreEqual(LCreated.SessionId, LSession.SessionId, 'Session IDs should match');
end;

procedure TSessionManagerTest.TestGetSessionUpdatesLastAccess;
var
  LSession: TSessionBase;
  LInitialAccess: TDateTime;
begin
  LSession := FManager.CreateSession;
  LInitialAccess := LSession.LastAccessedAt;

  // Wait a bit
  Sleep(100);

  // Retrieve session (should update LastAccessedAt)
  FManager.GetSession(LSession.SessionId);

  Assert.IsTrue(LSession.LastAccessedAt > LInitialAccess, 'LastAccessedAt should be updated');
end;

procedure TSessionManagerTest.TestSessionExists;
var
  LSession: TSessionBase;
begin
  LSession := FManager.CreateSession;

  Assert.IsTrue(FManager.SessionExists(LSession.SessionId), 'Session should exist');
  Assert.IsFalse(FManager.SessionExists('nonexistent-id'), 'Non-existent session should not exist');
end;

{ Session Destruction Tests }

procedure TSessionManagerTest.TestDestroySession;
var
  LSession: TSessionBase;
  LSessionId: string;
begin
  LSession := FManager.CreateSession;
  LSessionId := LSession.SessionId;

  Assert.IsTrue(FManager.SessionExists(LSessionId), 'Session should exist before destruction');

  FManager.DestroySession(LSessionId);

  Assert.IsFalse(FManager.SessionExists(LSessionId), 'Session should not exist after destruction');
end;

procedure TSessionManagerTest.TestDestroyNonExistentSession;
begin
  // Should not raise an exception
  FManager.DestroySession('nonexistent-id');
  Assert.IsTrue(True, 'Destroying non-existent session should not raise exception');
end;

{ Timeout and Expiration Tests }

procedure TSessionManagerTest.TestSessionTimeout;
var
  LSession: TSessionBase;
begin
  FManager.TimeoutMinutes := 1; // 1 minute timeout

  LSession := FManager.CreateSession;

  // Manually set LastAccessedAt to 2 minutes ago
  LSession.LastAccessedAt := IncMinute(Now, -2);

  // Try to get the session - should raise ESessionExpiredError
  Assert.WillRaise(
    procedure
    begin
      FManager.GetSession(LSession.SessionId);
    end,
    ESessionExpiredError,
    'Getting expired session should raise ESessionExpiredError'
  );
end;

procedure TSessionManagerTest.TestExpiredSessionRemoved;
var
  LSession: TSessionBase;
  LSessionId: string;
begin
  FManager.TimeoutMinutes := 1;

  LSession := FManager.CreateSession;
  LSessionId := LSession.SessionId;

  // Set to expired
  LSession.LastAccessedAt := IncMinute(Now, -2);

  Assert.IsTrue(FManager.SessionExists(LSessionId), 'Session should exist before expiration check');

  // Try to get - will remove the expired session
  try
    FManager.GetSession(LSessionId);
  except
    on E: ESessionExpiredError do
      ; // Expected
  end;

  Assert.IsFalse(FManager.SessionExists(LSessionId), 'Expired session should be removed after access attempt');
end;

procedure TSessionManagerTest.TestTimeoutConfiguration;
begin
  FManager.TimeoutMinutes := 60;
  Assert.AreEqual(60, FManager.TimeoutMinutes, 'Timeout should be configurable');

  FManager.TimeoutMinutes := 15;
  Assert.AreEqual(15, FManager.TimeoutMinutes, 'Timeout should be updated');
end;

{ Custom Session Class Tests }

procedure TSessionManagerTest.TestCustomSessionClass;
var
  LSession: TSessionBase;
  LCustomSession: TCustomTestSession;
begin
  FManager.SessionClass := TCustomTestSession;

  LSession := FManager.CreateSession;

  Assert.IsTrue(LSession is TCustomTestSession, 'Created session should be of custom type');

  LCustomSession := LSession as TCustomTestSession;
  LCustomSession.UserName := 'testuser';
  LCustomSession.UserRole := 'admin';

  Assert.AreEqual('testuser', LCustomSession.UserName, 'Custom property UserName should work');
  Assert.AreEqual('admin', LCustomSession.UserRole, 'Custom property UserRole should work');
end;

{ Error Handling Tests }

procedure TSessionManagerTest.TestGetNonExistentSessionRaisesException;
begin
  Assert.WillRaise(
    procedure
    begin
      FManager.GetSession('nonexistent-id');
    end,
    ESessionNotFoundError,
    'Getting non-existent session should raise ESessionNotFoundError'
  );
end;

procedure TSessionManagerTest.TestGetExpiredSessionRaisesException;
var
  LSession: TSessionBase;
begin
  FManager.TimeoutMinutes := 1;
  LSession := FManager.CreateSession;

  // Manually expire the session
  LSession.LastAccessedAt := IncMinute(Now, -5);

  Assert.WillRaise(
    procedure
    begin
      FManager.GetSession(LSession.SessionId);
    end,
    ESessionExpiredError,
    'Getting expired session should raise ESessionExpiredError'
  );
end;

initialization
  TDUnitX.RegisterTestFixture(TSessionDataTest);
  TDUnitX.RegisterTestFixture(TSessionManagerTest);

end.
