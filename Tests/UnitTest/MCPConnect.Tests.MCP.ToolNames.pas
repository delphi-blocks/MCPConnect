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
unit MCPConnect.Tests.MCP.ToolNames;

interface

uses
  System.SysUtils,
  DUnitX.TestFramework,

  MCPConnect.JRPC.Server,
  MCPConnect.Configuration.MCP,
  MCPConnect.MCP.Attributes,
  MCPConnect.MCP.Types.Base,
  MCPConnect.MCP.Types.Tool;

type
  TWellNamedTools = class
  public
    [McpTool('get_user', 'Fetches a user')]
    function GetUser: string;
  end;

  /// <summary>
  ///   A dot is legal in 2026-07-28 and was not before.
  /// </summary>
  TDottedTools = class
  public
    [McpTool('admin.tools.list', 'Lists admin tools')]
    function AdminList: string;
  end;

  [McpScope('auth')]
  TScopedTools = class
  public
    [McpTool('login', 'Logs in')]
    function Login: string;
  end;

  TBadlyNamedTools = class
  public
    [McpTool('get user', 'A space is not allowed')]
    function GetUser: string;
  end;

  [TestFixture]
  TMCPToolNameRuleTest = class(TObject)
  public
    [Test]
    [TestCase('simple', 'getUser,True')]
    [TestCase('underscored', 'DATA_EXPORT_v2,True')]
    [TestCase('dotted', 'admin.tools.list,True')]
    [TestCase('hyphenated', 'get-user,True')]
    [TestCase('single char', 'a,True')]
    [TestCase('digits', '123,True')]
    [TestCase('space', 'get user,False')]
    [TestCase('colon', 'auth:login,False')]
    [TestCase('slash', 'auth/login,False')]
    [TestCase('at sign', 'get@user,False')]
    procedure TestNameRule(const AName: string; AExpected: Boolean);

    [Test]
    procedure TestEmptyNameIsRejected;
    [Test]
    procedure TestCommaIsRejected;
    [Test]
    procedure TestNonAsciiLetterIsRejected;
    [Test]
    procedure TestLengthBounds;
  end;

  /// <summary>
  ///   The rule is enforced where a developer can act on it: at registration.
  /// </summary>
  [TestFixture]
  TMCPToolNameRegistrationTest = class(TObject)
  private
    FServer: TJRPCServer;
    FConfig: IMCPConfig;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestValidNameRegisters;
    [Test]
    procedure TestDottedNameRegisters;
    [Test]
    procedure TestScopedNameRegisters;
    [Test]
    procedure TestInvalidAttributeNameRaises;
    [Test]
    procedure TestInvalidProgrammaticNameRaises;
    [Test]
    procedure TestScopeSeparatorIsPartOfTheCheckedName;
  end;

implementation

{ tool classes }

function TWellNamedTools.GetUser: string;
begin
  Result := 'u';
end;

function TDottedTools.AdminList: string;
begin
  Result := 'a';
end;

function TScopedTools.Login: string;
begin
  Result := 'l';
end;

function TBadlyNamedTools.GetUser: string;
begin
  Result := 'u';
end;

{ TMCPToolNameRuleTest }

procedure TMCPToolNameRuleTest.TestNameRule(const AName: string; AExpected: Boolean);
begin
  Assert.AreEqual(AExpected, IsValidToolName(AName));
end;

procedure TMCPToolNameRuleTest.TestEmptyNameIsRejected;
begin
  Assert.IsFalse(IsValidToolName(''), 'A name is at least one character long');
end;

procedure TMCPToolNameRuleTest.TestCommaIsRejected;
begin
  // Spelled out rather than given as a [TestCase], which splits on commas
  Assert.IsFalse(IsValidToolName('get,user'));
end;

procedure TMCPToolNameRuleTest.TestNonAsciiLetterIsRejected;
begin
  // Only ASCII letters are allowed, so an accented one is out
  Assert.IsFalse(IsValidToolName('caff' + Char($E8)));
end;

procedure TMCPToolNameRuleTest.TestLengthBounds;
begin
  Assert.IsTrue(IsValidToolName(StringOfChar('a', MCP_TOOL_NAME_MAX_LENGTH)),
    '128 characters is the inclusive upper bound');
  Assert.IsFalse(IsValidToolName(StringOfChar('a', MCP_TOOL_NAME_MAX_LENGTH + 1)));

  // 2026-07-28 raised the limit from the 64 of earlier revisions
  Assert.IsTrue(IsValidToolName(StringOfChar('a', 65)));
end;

{ TMCPToolNameRegistrationTest }

procedure TMCPToolNameRegistrationTest.Setup;
begin
  FServer := TJRPCServer.Create(nil);
  FConfig := FServer.Plugin.Configure<IMCPConfig>;
end;

procedure TMCPToolNameRegistrationTest.TearDown;
begin
  FConfig := nil;
  FServer.Free;
end;

procedure TMCPToolNameRegistrationTest.TestValidNameRegisters;
begin
  FConfig.Tools.RegisterClass(TWellNamedTools);
  Assert.IsTrue(FConfig.Tools.Registry.ContainsKey('get_user'));
end;

procedure TMCPToolNameRegistrationTest.TestDottedNameRegisters;
begin
  // The dot became legal in 2026-07-28
  FConfig.Tools.RegisterClass(TDottedTools);
  Assert.IsTrue(FConfig.Tools.Registry.ContainsKey('admin.tools.list'));
end;

procedure TMCPToolNameRegistrationTest.TestScopedNameRegisters;
begin
  FConfig.Tools.RegisterClass(TScopedTools);
  Assert.IsTrue(FConfig.Tools.Registry.ContainsKey('auth_login'),
    'The default separator keeps the scoped name valid');
end;

procedure TMCPToolNameRegistrationTest.TestInvalidAttributeNameRaises;
begin
  Assert.WillRaise(
    procedure
    begin
      FConfig.Tools.RegisterClass(TBadlyNamedTools);
    end,
    EMCPException, 'A name a client would reject must be caught at registration');
end;

procedure TMCPToolNameRegistrationTest.TestInvalidProgrammaticNameRaises;
begin
  Assert.WillRaise(
    procedure
    begin
      FConfig.Tools
        .RegisterTool(TWellNamedTools, 'GetUser', 'get user', 'Bad name')
        .EndTool;
    end,
    EMCPException, 'The attribute-free path goes through the same check');
end;

procedure TMCPToolNameRegistrationTest.TestScopeSeparatorIsPartOfTheCheckedName;
begin
  // The scope prefix and its separator are part of the name the client sees,
  // so a separator the rule forbids has to be caught too
  FConfig.Server.SetScopeSeparator(':');

  Assert.WillRaise(
    procedure
    begin
      FConfig.Tools.RegisterClass(TScopedTools);
    end,
    EMCPException);
end;

initialization
  TDUnitX.RegisterTestFixture(TMCPToolNameRuleTest);
  TDUnitX.RegisterTestFixture(TMCPToolNameRegistrationTest);

end.
