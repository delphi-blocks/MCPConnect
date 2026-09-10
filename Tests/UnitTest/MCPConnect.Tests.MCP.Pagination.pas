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
///   Cursor pagination over the four list endpoints, and the deterministic
///   order it rests on. Paging itself is a MAY - which is why it is off until a
///   page size is configured - but two things are not: a list has to come back
///   in a stable order, and a cursor the server cannot have issued has to be
///   refused with -32602 rather than quietly ignored, or a client pages for
///   ever over the same first page.
/// </summary>
unit MCPConnect.Tests.MCP.Pagination;

interface

uses
  System.SysUtils, System.Classes, System.JSON,
  DUnitX.TestFramework,

  MCPConnect.Configuration.MCP,
  MCPConnect.MCP.Server,
  MCPConnect.MCP.Types.Base,
  MCPConnect.Transport.Base;

type
  /// <summary>
  ///   The cursor codec on its own: what it promises the server, as against
  ///   what it promises the client (nothing - it is opaque).
  /// </summary>
  [TestFixture]
  TMCPCursorTest = class(TObject)
  public
    [Test]
    procedure TestItRoundTripsEveryKind();
    [Test]
    procedure TestACursorForAnotherListIsRefused();
    [Test]
    procedure TestGarbageIsRefused();
    [Test]
    procedure TestBase64WithoutTheSeparatorIsRefused();
    [Test]
    procedure TestAnEmptyCursorIsRefused();
    [Test]
    procedure TestAKeyMayContainTheSeparator();
    [Test]
    procedure TestAnEmptyKeyIsAPosition();
    [Test]
    procedure TestItIsNotHumanReadable();
  end;

  /// <summary>A response writer that streams nothing.</summary>
  TSilentPageWriter = class(TInterfacedObject, IMCPTransportWriter)
  public
    procedure Write(const AValue: string);
    function Connected: Boolean;
    function SupportsStreaming: Boolean;
  end;

  [TestFixture]
  TPaginationTest = class(TObject)
  private
    FServer: TMCPServer;

    /// <summary>A server with five tools, five resources, two templates and five prompts.</summary>
    procedure ConfigureServer;

    /// <summary>The "result" of AMethod, or nil when it answered an error. The caller owns it.</summary>
    function Call(const AMethod: string; const ACursor: string = ''): TJSONObject;

    /// <summary>The JSON-RPC error code AMethod answered with, or 0.</summary>
    function CallForError(const AMethod: string; const ACursor: string): Integer;

    /// <summary>The names (or uris) in AResult's AMember array, in order.</summary>
    function KeysOf(AResult: TJSONObject; const AMember, AKeyName: string): TArray<string>;

    /// <summary>AResult's nextCursor, or '' when the member is absent.</summary>
    function NextCursorOf(AResult: TJSONObject): string;

    /// <summary>
    ///   Walks AMethod from the start, following every cursor, and returns
    ///   every key it was handed along with how many requests it took.
    /// </summary>
    function WalkAll(const AMethod, AMember, AKeyName: string;
      out APages: Integer): TArray<string>;
  public
    [TearDown]
    procedure TearDown();

    [Test]
    procedure TestUnpagedByDefaultTheWholeListComesBack();
    [Test]
    procedure TestUnpagedThereIsNoNextCursor();
    [Test]
    procedure TestTheOrderIsDeterministicEvenUnpaged();

    [Test]
    procedure TestAPageIsTheConfiguredSize();
    [Test]
    procedure TestAPageCarriesTheCursorOfTheNextOne();
    [Test]
    procedure TestFollowingTheCursorGivesTheNextPage();
    [Test]
    procedure TestTheLastPageCarriesNoCursor();
    [Test]
    procedure TestWalkingYieldsEveryItemExactlyOnce();
    [Test]
    procedure TestAPageSizeAtLeastTheListLengthPagesOnce();

    [Test]
    procedure TestASectionSizeOverridesTheServerSize();
    [Test]
    procedure TestResourcesAndPromptsPageToo();
    [Test]
    procedure TestTemplatesPageOnTheirOwnCursor();

    [Test]
    procedure TestACursorForAnotherListIsInvalidParams();
    [Test]
    procedure TestAGarbageCursorIsInvalidParams();
    [Test]
    procedure TestACursorSentToAnUnpagedServerIsInvalidParams();
    [Test]
    procedure TestACursorPastTheEndIsAnEmptyPage();
    [Test]
    procedure TestAnUnregisteredKeyStillNamesAPosition();
  end;

implementation

uses
  System.Generics.Collections,
  System.NetEncoding,

  MCPConnect.MCP.Attributes,
  MCPConnect.MCP.Types.Prompts;

type
  /// <summary>
  ///   Declared out of alphabetical order on purpose: the order a list comes
  ///   back in must be the sorted one, not the one the RTTI happens to report.
  /// </summary>
  TPagedFeatures = class(TObject)
  public
    [McpTool('echo', 'Answers something')]
    function Echo: string;
    [McpTool('delta', 'Answers something')]
    function Delta: string;
    [McpTool('alpha', 'Answers something')]
    function Alpha: string;
    [McpTool('charlie', 'Answers something')]
    function Charlie: string;
    [McpTool('bravo', 'Answers something')]
    function Bravo: string;

    [McpResource('e', 'res://e', 'text/plain', 'A resource')]
    function ResE: string;
    [McpResource('d', 'res://d', 'text/plain', 'A resource')]
    function ResD: string;
    [McpResource('a', 'res://a', 'text/plain', 'A resource')]
    function ResA: string;
    [McpResource('c', 'res://c', 'text/plain', 'A resource')]
    function ResC: string;
    [McpResource('b', 'res://b', 'text/plain', 'A resource')]
    function ResB: string;

    [McpTemplate('tz', 'res://t/{id}/z', 'text/plain', 'A template')]
    function TplZ([McpParam('id', 'The id')] const id: string): string;
    [McpTemplate('ta', 'res://t/{id}/a', 'text/plain', 'A template')]
    function TplA([McpParam('id', 'The id')] const id: string): string;

    [McpPrompt('p_echo', 'Echo', 'A prompt')]
    function PEcho: TPromptMessages;
    [McpPrompt('p_delta', 'Delta', 'A prompt')]
    function PDelta: TPromptMessages;
    [McpPrompt('p_alpha', 'Alpha', 'A prompt')]
    function PAlpha: TPromptMessages;
    [McpPrompt('p_charlie', 'Charlie', 'A prompt')]
    function PCharlie: TPromptMessages;
    [McpPrompt('p_bravo', 'Bravo', 'A prompt')]
    function PBravo: TPromptMessages;
  end;

/// <summary>
///   AValue as a JSON string literal, escaping included. A cursor is Base64,
///   so nothing in it needs escaping today - but a test that hand-rolls quotes
///   is a test that breaks the day a key does.
/// </summary>
function JsonQuoted(const AValue: string): string;
var
  LString: TJSONString;
begin
  LString := TJSONString.Create(AValue);
  try
    Result := LString.ToJSON;
  finally
    LString.Free;
  end;
end;

const
  ToolsMethod = 'tools/list';
  ResourcesMethod = 'resources/list';
  TemplatesMethod = 'resources/templates/list';
  PromptsMethod = 'prompts/list';

  /// <summary>The tool names of TPagedFeatures, in the order they must come back.</summary>
  SortedTools: array[0..4] of string = ('alpha', 'bravo', 'charlie', 'delta', 'echo');

function TPagedFeatures.Echo: string;    begin Result := 'e'; end;
function TPagedFeatures.Delta: string;   begin Result := 'd'; end;
function TPagedFeatures.Alpha: string;   begin Result := 'a'; end;
function TPagedFeatures.Charlie: string; begin Result := 'c'; end;
function TPagedFeatures.Bravo: string;   begin Result := 'b'; end;

function TPagedFeatures.ResE: string; begin Result := 'e'; end;
function TPagedFeatures.ResD: string; begin Result := 'd'; end;
function TPagedFeatures.ResA: string; begin Result := 'a'; end;
function TPagedFeatures.ResC: string; begin Result := 'c'; end;
function TPagedFeatures.ResB: string; begin Result := 'b'; end;

function TPagedFeatures.TplZ(const id: string): string; begin Result := id; end;
function TPagedFeatures.TplA(const id: string): string; begin Result := id; end;

function TPagedFeatures.PEcho: TPromptMessages;    begin Result := TPromptMessages.Create; end;
function TPagedFeatures.PDelta: TPromptMessages;   begin Result := TPromptMessages.Create; end;
function TPagedFeatures.PAlpha: TPromptMessages;   begin Result := TPromptMessages.Create; end;
function TPagedFeatures.PCharlie: TPromptMessages; begin Result := TPromptMessages.Create; end;
function TPagedFeatures.PBravo: TPromptMessages;   begin Result := TPromptMessages.Create; end;

{ TMCPCursorTest }

procedure TMCPCursorTest.TestItRoundTripsEveryKind;
var
  LKind: TMCPPageKind;
  LKey: string;
begin
  for LKind := Low(TMCPPageKind) to High(TMCPPageKind) do
  begin
    Assert.IsTrue(
      TMCPCursor.TryDecode(LKind, TMCPCursor.Encode(LKind, 'the-key'), LKey),
      TMCPCursor.KindNameOf(LKind));
    Assert.AreEqual('the-key', LKey, TMCPCursor.KindNameOf(LKind));
  end;
end;

procedure TMCPCursorTest.TestACursorForAnotherListIsRefused;
var
  LKey: string;
begin
  // The one thing a cursor is not opaque about to the *server*: which list it
  // came from. A client that pages prompts with a tools cursor is confused, and
  // answering it with prompts would hide that.
  Assert.IsFalse(
    TMCPCursor.TryDecode(TMCPPageKind.Prompts,
      TMCPCursor.Encode(TMCPPageKind.Tools, 'alpha'), LKey));
end;

procedure TMCPCursorTest.TestGarbageIsRefused;
var
  LKey: string;
begin
  Assert.IsFalse(TMCPCursor.TryDecode(TMCPPageKind.Tools, 'not base64 at all!', LKey));
  Assert.IsFalse(TMCPCursor.TryDecode(TMCPPageKind.Tools, '%%%%', LKey));
end;

procedure TMCPCursorTest.TestBase64WithoutTheSeparatorIsRefused;
var
  LKey: string;
begin
  Assert.IsFalse(
    TMCPCursor.TryDecode(TMCPPageKind.Tools,
      TNetEncoding.Base64String.Encode('tools-and-no-separator'), LKey));
end;

procedure TMCPCursorTest.TestAnEmptyCursorIsRefused;
var
  LKey: string;
begin
  // Not the same as an empty *key*: this is no cursor at all
  Assert.IsFalse(TMCPCursor.TryDecode(TMCPPageKind.Tools, '', LKey));
end;

procedure TMCPCursorTest.TestAKeyMayContainTheSeparator;
var
  LKey: string;
begin
  // Only the first separator delimits the list name, so a key carrying one
  // survives - a resource uri is free to contain anything
  Assert.IsTrue(
    TMCPCursor.TryDecode(TMCPPageKind.Resources,
      TMCPCursor.Encode(TMCPPageKind.Resources, 'res://a|b|c'), LKey));
  Assert.AreEqual('res://a|b|c', LKey);
end;

procedure TMCPCursorTest.TestAnEmptyKeyIsAPosition;
var
  LKey: string;
begin
  // "Everything after the empty string" is a legal, if unusual, position, and
  // the specification is explicit that an empty cursor value is still a cursor
  Assert.IsTrue(
    TMCPCursor.TryDecode(TMCPPageKind.Tools,
      TMCPCursor.Encode(TMCPPageKind.Tools, ''), LKey));
  Assert.AreEqual('', LKey);
end;

procedure TMCPCursorTest.TestItIsNotHumanReadable;
begin
  // Not a security property - it is not signed and does not need to be - but a
  // client that can read a cursor is a client that will parse one, which the
  // specification forbids
  Assert.DoesNotContain(TMCPCursor.Encode(TMCPPageKind.Tools, 'alpha'), 'alpha');
end;

{ TSilentPageWriter }

procedure TSilentPageWriter.Write(const AValue: string);
begin
  // Nothing streams in these tests.
end;

function TSilentPageWriter.Connected: Boolean;
begin
  Result := True;
end;

function TSilentPageWriter.SupportsStreaming: Boolean;
begin
  Result := False;
end;

{ TPaginationTest }

procedure TPaginationTest.TearDown;
begin
  FServer.Free;
  FServer := nil;
end;

procedure TPaginationTest.ConfigureServer;
begin
  FServer := TMCPServer.Create(nil);

  FServer.Plugin.Configure<IMCPConfig>
    .Server
      .SetName('pagination-test')
      .SetVersion('1.0.0')
    .BackToMCP
    .Security
      .SetHeaderValidation(TMCPValidationLevel.Off)
      .SetMetaValidation(TMCPValidationLevel.Off)
      .SetOriginPolicy(TMCPOriginPolicy.Off)
    .BackToMCP
    .Tools
      .RegisterClass(TPagedFeatures)
    .BackToMCP
    .Resources
      .RegisterClass(TPagedFeatures)
    .BackToMCP
    .Prompts
      .RegisterClass(TPagedFeatures)
    .BackToMCP
  .ApplyConfig;
end;

function TPaginationTest.Call(const AMethod, ACursor: string): TJSONObject;
var
  LHandler: TMCPTransportHandler;
  LContent, LParams: string;
  LValue, LResult: TJSONValue;
begin
  Result := nil;
  LContent := '';

  if ACursor.IsEmpty then
    LParams := '{}'
  else
    LParams := Format('{"cursor":%s}', [JsonQuoted(ACursor)]);

  LHandler := TMCPTransportHandler.Create(FServer, TSilentPageWriter.Create);
  try
    LHandler.ProcessRequest(
      procedure (ARequest: TMCPTransportRequest)
      begin
        ARequest.Url := '/';
        ARequest.Command := 'POST';
        ARequest.Protocol := TTransportProtocol.StreamableHTTP;
        ARequest.Accept := 'application/json';
        ARequest.Content := Format(
          '{"jsonrpc":"2.0","id":1,"method":"%s","params":%s}', [AMethod, LParams]);
      end,
      procedure (AResponse: TMCPTransportResponse)
      begin
        LContent := AResponse.Content;
      end);
  finally
    LHandler.Free;
  end;

  LValue := TJSONObject.ParseJSONValue(LContent);
  Assert.IsTrue(LValue is TJSONObject, AMethod + ' must answer JSON: ' + LContent);
  try
    LResult := TJSONObject(LValue).GetValue('result');
    if LResult is TJSONObject then
      Result := LResult.Clone as TJSONObject;
  finally
    LValue.Free;
  end;
end;

function TPaginationTest.CallForError(const AMethod, ACursor: string): Integer;
var
  LHandler: TMCPTransportHandler;
  LContent: string;
  LValue, LError, LCode: TJSONValue;
begin
  LContent := '';

  LHandler := TMCPTransportHandler.Create(FServer, TSilentPageWriter.Create);
  try
    LHandler.ProcessRequest(
      procedure (ARequest: TMCPTransportRequest)
      begin
        ARequest.Url := '/';
        ARequest.Command := 'POST';
        ARequest.Protocol := TTransportProtocol.StreamableHTTP;
        ARequest.Accept := 'application/json';
        ARequest.Content := Format(
          '{"jsonrpc":"2.0","id":1,"method":"%s","params":{"cursor":%s}}',
          [AMethod, JsonQuoted(ACursor)]);
      end,
      procedure (AResponse: TMCPTransportResponse)
      begin
        LContent := AResponse.Content;
      end);
  finally
    LHandler.Free;
  end;

  LValue := TJSONObject.ParseJSONValue(LContent);
  Assert.IsTrue(LValue is TJSONObject, 'must answer JSON: ' + LContent);
  try
    LError := TJSONObject(LValue).GetValue('error');
    Assert.IsTrue(LError is TJSONObject, 'must answer an error: ' + LContent);

    LCode := TJSONObject(LError).GetValue('code');
    Assert.IsTrue(LCode is TJSONNumber, 'the error must carry a code: ' + LContent);
    Result := TJSONNumber(LCode).AsInt;
  finally
    LValue.Free;
  end;
end;

function TPaginationTest.KeysOf(AResult: TJSONObject;
  const AMember, AKeyName: string): TArray<string>;
var
  LArray: TJSONValue;
  LEntry: TJSONValue;
  LKey: TJSONValue;
begin
  Result := [];
  LArray := AResult.GetValue(AMember);
  Assert.IsTrue(LArray is TJSONArray, 'the result must carry a ' + AMember + ' array');

  for LEntry in TJSONArray(LArray) do
  begin
    Assert.IsTrue(LEntry is TJSONObject, AMember + ' must hold objects');
    LKey := TJSONObject(LEntry).GetValue(AKeyName);
    Assert.IsTrue(LKey is TJSONString, 'each entry must carry a ' + AKeyName);
    Result := Result + [TJSONString(LKey).Value];
  end;
end;

function TPaginationTest.NextCursorOf(AResult: TJSONObject): string;
var
  LValue: TJSONValue;
begin
  Result := '';
  LValue := AResult.GetValue('nextCursor');
  if LValue is TJSONString then
    Result := TJSONString(LValue).Value;
end;

function TPaginationTest.WalkAll(const AMethod, AMember, AKeyName: string;
  out APages: Integer): TArray<string>;
var
  LResult: TJSONObject;
  LCursor: string;
begin
  Result := [];
  LCursor := '';
  APages := 0;

  repeat
    LResult := Call(AMethod, LCursor);
    Assert.IsNotNull(LResult, AMethod + ' answered no result on page ' + APages.ToString);
    try
      Inc(APages);
      Result := Result + KeysOf(LResult, AMember, AKeyName);
      LCursor := NextCursorOf(LResult);
    finally
      LResult.Free;
    end;

    // A walk that will not end is a bug worth failing on rather than hanging
    Assert.IsTrue(APages < 20, 'the walk did not terminate');
  until LCursor.IsEmpty;
end;

procedure TPaginationTest.TestUnpagedByDefaultTheWholeListComesBack;
var
  LResult: TJSONObject;
begin
  // Paging is a MAY, and a client that does not follow cursors would otherwise
  // be handed a truncated list with no way to tell
  ConfigureServer;

  LResult := Call(ToolsMethod);
  try
    Assert.AreEqual(Length(SortedTools), Length(KeysOf(LResult, 'tools', 'name')));
  finally
    LResult.Free;
  end;
end;

procedure TPaginationTest.TestUnpagedThereIsNoNextCursor;
var
  LResult: TJSONObject;
begin
  ConfigureServer;

  LResult := Call(ToolsMethod);
  try
    // Absent, not empty: an empty string is a cursor a client would follow
    Assert.IsNull(LResult.GetValue('nextCursor'),
      'a server that does not page issues no cursor');
  finally
    LResult.Free;
  end;
end;

procedure TPaginationTest.TestTheOrderIsDeterministicEvenUnpaged;
var
  LResult: TJSONObject;
  LNames: TArray<string>;
  LIndex: Integer;
begin
  // The registries are dictionaries, so the order they enumerate in is an
  // implementation detail. A list SHOULD come back in a deterministic order
  // whether it is paged or not - a client and a prompt cache both depend on it.
  ConfigureServer;

  LResult := Call(ToolsMethod);
  try
    LNames := KeysOf(LResult, 'tools', 'name');
    Assert.AreEqual(Length(SortedTools), Length(LNames));
    for LIndex := 0 to High(SortedTools) do
      Assert.AreEqual(SortedTools[LIndex], LNames[LIndex],
        'position ' + LIndex.ToString);
  finally
    LResult.Free;
  end;
end;

procedure TPaginationTest.TestAPageIsTheConfiguredSize;
var
  LResult: TJSONObject;
  LNames: TArray<string>;
begin
  ConfigureServer;
  FServer.Plugin.Configure<IMCPConfig>.Server.SetPageSize(2);

  LResult := Call(ToolsMethod);
  try
    LNames := KeysOf(LResult, 'tools', 'name');
    Assert.AreEqual(2, Length(LNames));
    Assert.AreEqual('alpha', LNames[0]);
    Assert.AreEqual('bravo', LNames[1]);
  finally
    LResult.Free;
  end;
end;

procedure TPaginationTest.TestAPageCarriesTheCursorOfTheNextOne;
var
  LResult: TJSONObject;
  LKey: string;
begin
  ConfigureServer;
  FServer.Plugin.Configure<IMCPConfig>.Server.SetPageSize(2);

  LResult := Call(ToolsMethod);
  try
    // It names the last item sent, which is what makes it stable: registering
    // a tool between two pages moves no position the cursor depends on
    Assert.IsTrue(
      TMCPCursor.TryDecode(TMCPPageKind.Tools, NextCursorOf(LResult), LKey));
    Assert.AreEqual('bravo', LKey);
  finally
    LResult.Free;
  end;
end;

procedure TPaginationTest.TestFollowingTheCursorGivesTheNextPage;
var
  LResult: TJSONObject;
  LCursor: string;
  LNames: TArray<string>;
begin
  ConfigureServer;
  FServer.Plugin.Configure<IMCPConfig>.Server.SetPageSize(2);

  LResult := Call(ToolsMethod);
  try
    LCursor := NextCursorOf(LResult);
  finally
    LResult.Free;
  end;

  LResult := Call(ToolsMethod, LCursor);
  try
    LNames := KeysOf(LResult, 'tools', 'name');
    Assert.AreEqual(2, Length(LNames));
    Assert.AreEqual('charlie', LNames[0]);
    Assert.AreEqual('delta', LNames[1]);
  finally
    LResult.Free;
  end;
end;

procedure TPaginationTest.TestTheLastPageCarriesNoCursor;
var
  LNames: TArray<string>;
  LPages: Integer;
begin
  ConfigureServer;
  FServer.Plugin.Configure<IMCPConfig>.Server.SetPageSize(2);

  // WalkAll stops when a page carries no cursor, so its returning at all is
  // the assertion; five tools at two a page is three pages
  LNames := WalkAll(ToolsMethod, 'tools', 'name', LPages);

  Assert.AreEqual(3, LPages);
  Assert.AreEqual(Length(SortedTools), Length(LNames));
end;

procedure TPaginationTest.TestWalkingYieldsEveryItemExactlyOnce;
var
  LNames: TArray<string>;
  LPages, LIndex: Integer;
begin
  ConfigureServer;
  FServer.Plugin.Configure<IMCPConfig>.Server.SetPageSize(2);

  LNames := WalkAll(ToolsMethod, 'tools', 'name', LPages);

  Assert.AreEqual(Length(SortedTools), Length(LNames),
    'no item is sent twice and none is skipped');
  for LIndex := 0 to High(SortedTools) do
    Assert.AreEqual(SortedTools[LIndex], LNames[LIndex], 'position ' + LIndex.ToString);
end;

procedure TPaginationTest.TestAPageSizeAtLeastTheListLengthPagesOnce;
var
  LResult: TJSONObject;
begin
  ConfigureServer;
  FServer.Plugin.Configure<IMCPConfig>.Server.SetPageSize(Length(SortedTools));

  LResult := Call(ToolsMethod);
  try
    Assert.AreEqual(Length(SortedTools), Length(KeysOf(LResult, 'tools', 'name')));
    Assert.IsNull(LResult.GetValue('nextCursor'),
      'a page that holds everything is the last page');
  finally
    LResult.Free;
  end;
end;

procedure TPaginationTest.TestASectionSizeOverridesTheServerSize;
var
  LResult: TJSONObject;
begin
  ConfigureServer;
  FServer.Plugin.Configure<IMCPConfig>
    .Server.SetPageSize(2)
    .BackToMCP
    .Tools.SetPageSize(4);

  LResult := Call(ToolsMethod);
  try
    Assert.AreEqual(4, Length(KeysOf(LResult, 'tools', 'name')),
      'the section that answered decides');
  finally
    LResult.Free;
  end;

  // And the sections that said nothing still follow the server
  LResult := Call(PromptsMethod);
  try
    Assert.AreEqual(2, Length(KeysOf(LResult, 'prompts', 'name')));
  finally
    LResult.Free;
  end;
end;

procedure TPaginationTest.TestResourcesAndPromptsPageToo;
var
  LUris, LNames: TArray<string>;
  LPages: Integer;
begin
  ConfigureServer;
  FServer.Plugin.Configure<IMCPConfig>.Server.SetPageSize(2);

  // Resources page on their uri, which is what resources/read is keyed by
  LUris := WalkAll(ResourcesMethod, 'resources', 'uri', LPages);
  Assert.AreEqual(3, LPages);
  Assert.AreEqual(5, Length(LUris));
  Assert.AreEqual('res://a', LUris[0]);
  Assert.AreEqual('res://e', LUris[4]);

  LNames := WalkAll(PromptsMethod, 'prompts', 'name', LPages);
  Assert.AreEqual(3, LPages);
  Assert.AreEqual(5, Length(LNames));
  Assert.AreEqual('p_alpha', LNames[0]);
end;

procedure TPaginationTest.TestTemplatesPageOnTheirOwnCursor;
var
  LResult: TJSONObject;
  LKey: string;
  LTemplates: TArray<string>;
begin
  ConfigureServer;
  FServer.Plugin.Configure<IMCPConfig>.Resources.SetPageSize(1);

  LResult := Call(TemplatesMethod);
  try
    LTemplates := KeysOf(LResult, 'resourceTemplates', 'uriTemplate');
    Assert.AreEqual(1, Length(LTemplates));
    Assert.AreEqual('res://t/{id}/a', LTemplates[0]);

    // Its own kind: the templates list and the resources list page apart, so
    // one's cursor is not the other's
    Assert.IsTrue(
      TMCPCursor.TryDecode(TMCPPageKind.Templates, NextCursorOf(LResult), LKey));
    Assert.IsFalse(
      TMCPCursor.TryDecode(TMCPPageKind.Resources, NextCursorOf(LResult), LKey));
  finally
    LResult.Free;
  end;
end;

procedure TPaginationTest.TestACursorForAnotherListIsInvalidParams;
begin
  ConfigureServer;
  FServer.Plugin.Configure<IMCPConfig>.Server.SetPageSize(2);

  Assert.AreEqual(-32602,
    CallForError(PromptsMethod, TMCPCursor.Encode(TMCPPageKind.Tools, 'alpha')),
    'a tools cursor is not a prompts position');
end;

procedure TPaginationTest.TestAGarbageCursorIsInvalidParams;
begin
  ConfigureServer;
  FServer.Plugin.Configure<IMCPConfig>.Server.SetPageSize(2);

  // Silently ignoring it is the one thing a server may not do: a client would
  // be handed the first page again and page for ever
  Assert.AreEqual(-32602, CallForError(ToolsMethod, 'not a cursor at all!'));
end;

procedure TPaginationTest.TestACursorSentToAnUnpagedServerIsInvalidParams;
begin
  // Well formed, but this server issues no cursor, so it cannot have issued
  // this one
  ConfigureServer;

  Assert.AreEqual(-32602,
    CallForError(ToolsMethod, TMCPCursor.Encode(TMCPPageKind.Tools, 'alpha')));
end;

procedure TPaginationTest.TestACursorPastTheEndIsAnEmptyPage;
var
  LResult: TJSONObject;
begin
  ConfigureServer;
  FServer.Plugin.Configure<IMCPConfig>.Server.SetPageSize(2);

  LResult := Call(ToolsMethod, TMCPCursor.Encode(TMCPPageKind.Tools, 'zulu'));
  try
    Assert.IsNotNull(LResult, 'a position past the end is a position, not an error');
    Assert.AreEqual(0, Length(KeysOf(LResult, 'tools', 'name')));
    Assert.IsNull(LResult.GetValue('nextCursor'));
  finally
    LResult.Free;
  end;
end;

procedure TPaginationTest.TestAnUnregisteredKeyStillNamesAPosition;
var
  LResult: TJSONObject;
  LNames: TArray<string>;
begin
  // The graceful half of "handle invalid cursors gracefully": a key that names
  // no item still names a place in the order, which is what a client holding a
  // cursor to a tool that has since been unregistered has
  ConfigureServer;
  FServer.Plugin.Configure<IMCPConfig>.Server.SetPageSize(2);

  LResult := Call(ToolsMethod, TMCPCursor.Encode(TMCPPageKind.Tools, 'bravo_gone'));
  try
    LNames := KeysOf(LResult, 'tools', 'name');
    Assert.AreEqual(2, Length(LNames));
    Assert.AreEqual('charlie', LNames[0], 'the walk resumes where the key sits');
  finally
    LResult.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TMCPCursorTest);
  TDUnitX.RegisterTestFixture(TPaginationTest);

end.
