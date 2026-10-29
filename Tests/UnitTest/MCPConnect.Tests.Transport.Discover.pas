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
///   What server/discover reports: the versions it speaks, what it can do -
///   inferred from what is registered, or exactly what the server said with
///   SetCapabilities - and the guidance it offers a model. The identity and the
///   cache hints of the same result belong to their own fixtures.
/// </summary>
unit MCPConnect.Tests.Transport.Discover;

interface

uses
  System.SysUtils, System.JSON,
  DUnitX.TestFramework,

  MCPConnect.Configuration.MCP,
  MCPConnect.MCP.Server,
  MCPConnect.MCP.Types.Base,
  MCPConnect.Transport.Base;

type
  /// <summary>
  ///   A response writer that writes nothing, and says whether its transport
  ///   could stream - which is what server/discover asks before promising
  ///   "listChanged".
  /// </summary>
  TSilentDiscoverWriter = class(TMCPTransportWriter)
  private
    FStreams: Boolean;
  public
    constructor Create(AStreams: Boolean = False);
    procedure Write(const AValue: string); override;
    function Connected: Boolean; override;
    function SupportsStreaming: Boolean; override;
  end;

  [TestFixture]
  TDiscoverTest = class(TObject)
  private
    FServer: TMCPServer;

    /// <summary>A server with nothing registered at all.</summary>
    procedure ConfigureBare;

    /// <summary>A server with a tool, a resource and a prompt.</summary>
    procedure ConfigureFull;

    /// <summary>
    ///   The "result" of server/discover, asked of a transport that can stream
    ///   or not. The caller owns it.
    /// </summary>
    function Discover(AStreams: Boolean = False): TJSONObject;

    /// <summary>
    ///   The "listChanged" of the capability AName, as JSON, or '' when absent.
    /// </summary>
    function ListChangedOf(AResult: TJSONObject; const AName: string): string;

    /// <summary>The "capabilities" of the result, or nil. Owned by AResult.</summary>
    function CapabilitiesOf(AResult: TJSONObject): TJSONObject;
  public
    [TearDown]
    procedure TearDown();

    [Test]
    procedure TestItReportsTheVersionsItSpeaks();

    [Test]
    procedure TestCapabilitiesFollowWhatIsRegistered();
    [Test]
    procedure TestABareServerAdvertisesNothing();
    [Test]
    procedure TestNoListChangedWhereNoStreamCanBeHeld();
    [Test]
    procedure TestListChangedWhereAStreamCanBeHeld();
    [Test]
    procedure TestSetListChangedFalseNeverPromisesIt();
    [Test]
    procedure TestSetListChangedDoesNotTouchExplicitCapabilities();

    [Test]
    procedure TestAnExplicitCapabilitySetIsUsedVerbatim();
    [Test]
    procedure TestAnExplicitSetCanAdvertiseLessThanIsRegistered();

    [Test]
    procedure TestTheAppExtensionIsAdvertisedWithAUiResource();
    [Test]
    procedure TestTheAppExtensionIsAbsentWithoutAppResources();
    [Test]
    procedure TestExplicitCapabilitiesGetNoAppExtension();

    [Test]
    procedure TestInstructionsAreReportedWhenSet();
    [Test]
    procedure TestInstructionsAreAbsentWhenNotSet();
    [Test]
    procedure TestTheDescriptionTravelsWithTheIdentity();
  end;

implementation

uses
  MCPConnect.MCP.Attributes,
  MCPConnect.MCP.Types.Prompts;

type
  TDiscoverTools = class(TObject)
  public
    [McpTool('ping', 'Answers something')]
    function Ping: string;

    [McpResource('doc', 'res://doc', 'text/plain', 'A resource')]
    function Doc: string;

    [McpPrompt('greet', 'Greet', 'Greets someone')]
    function Greet: TPromptMessages;
  end;

  TDiscoverApp = class(TObject)
  public
    [McpAppUI('fixture', 'ui://test/fixture', 'An MCP App page')]
    function Page: string;
  end;

function TDiscoverApp.Page: string;
begin
  Result := '<!DOCTYPE html><html><body>fixture</body></html>';
end;

function TDiscoverTools.Ping: string;
begin
  Result := 'pong';
end;

function TDiscoverTools.Doc: string;
begin
  Result := 'the doc';
end;

function TDiscoverTools.Greet: TPromptMessages;
begin
  Result := TPromptMessages.Create;
end;

{ TSilentDiscoverWriter }

constructor TSilentDiscoverWriter.Create(AStreams: Boolean);
begin
  inherited Create;
  FStreams := AStreams;
end;

procedure TSilentDiscoverWriter.Write(const AValue: string);
begin
  // Nothing streams in these tests: discover is asked for a document.
end;

function TSilentDiscoverWriter.Connected: Boolean;
begin
  Result := True;
end;

function TSilentDiscoverWriter.SupportsStreaming: Boolean;
begin
  Result := FStreams;
end;

{ TDiscoverTest }

procedure TDiscoverTest.TearDown;
begin
  FServer.Free;
  FServer := nil;
end;

procedure TDiscoverTest.ConfigureBare;
begin
  FServer := TMCPServer.Create(nil);

  FServer.Plugin.Configure<IMCPConfig>
    .Server
      .SetName('discover-test')
      .SetVersion('1.0.0')
    .BackToMCP
    .Security
      .SetHeaderValidation(TMCPValidationLevel.Off)
      .SetMetaValidation(TMCPValidationLevel.Off)
      .SetOriginPolicy(TMCPOriginPolicy.Off)
    .BackToMCP
  .ApplyConfig;
end;

procedure TDiscoverTest.ConfigureFull;
begin
  ConfigureBare();

  FServer.Plugin.Configure<IMCPConfig>
    .Tools
      .RegisterClass(TDiscoverTools)
    .BackToMCP
    .Resources
      .RegisterClass(TDiscoverTools)
    .BackToMCP
    .Prompts
      .RegisterClass(TDiscoverTools)
    .BackToMCP
  .ApplyConfig;
end;

function TDiscoverTest.Discover(AStreams: Boolean): TJSONObject;
var
  LHandler: TMCPTransportHandler;
  LContent: string;
  LValue, LResult: TJSONValue;
begin
  Result := nil;
  LContent := '';

  LHandler := TMCPTransportHandler.Create(FServer, TSilentDiscoverWriter.Create(AStreams));
  try
    LHandler.ProcessRequest(
      procedure (ARequest: TMCPTransportRequest)
      begin
        ARequest.Url := '/';
        ARequest.Command := 'POST';
        ARequest.Protocol := TTransportProtocol.StreamableHTTP;
        ARequest.Accept := 'application/json';
        ARequest.Content := '{"jsonrpc":"2.0","id":1,"method":"server/discover","params":{}}';
      end,
      procedure (AResponse: TMCPTransportResponse)
      begin
        LContent := AResponse.Content;
      end);
  finally
    LHandler.Free;
  end;

  LValue := TJSONObject.ParseJSONValue(LContent);
  Assert.IsTrue(LValue is TJSONObject, 'discover must answer JSON: ' + LContent);
  try
    LResult := TJSONObject(LValue).GetValue('result');
    Assert.IsTrue(LResult is TJSONObject, 'discover must answer a result: ' + LContent);
    Result := LResult.Clone as TJSONObject;
  finally
    LValue.Free;
  end;
end;

function TDiscoverTest.ListChangedOf(AResult: TJSONObject; const AName: string): string;
var
  LCaps: TJSONObject;
  LCapability, LFlag: TJSONValue;
begin
  Result := '';
  LCaps := CapabilitiesOf(AResult);
  if not Assigned(LCaps) then
    Exit;

  LCapability := LCaps.GetValue(AName);
  if not (LCapability is TJSONObject) then
    Exit;

  LFlag := TJSONObject(LCapability).GetValue('listChanged');
  if Assigned(LFlag) then
    Result := LFlag.ToJSON;
end;

function TDiscoverTest.CapabilitiesOf(AResult: TJSONObject): TJSONObject;
var
  LValue: TJSONValue;
begin
  Result := nil;
  LValue := AResult.GetValue('capabilities');
  if LValue is TJSONObject then
    Result := TJSONObject(LValue);
end;

procedure TDiscoverTest.TestItReportsTheVersionsItSpeaks;
var
  LResult: TJSONObject;
begin
  ConfigureBare();

  LResult := Discover;
  try
    Assert.Contains(LResult.ToJSON, MCP_PROTOCOL_VERSION_2026_07_28);
    Assert.Contains(LResult.ToJSON, 'supportedVersions');
  finally
    LResult.Free;
  end;
end;

procedure TDiscoverTest.TestCapabilitiesFollowWhatIsRegistered;
var
  LResult: TJSONObject;
  LCaps: TJSONObject;
begin
  ConfigureFull();

  LResult := Discover;
  try
    LCaps := CapabilitiesOf(LResult);
    Assert.IsNotNull(LCaps, LResult.ToJSON);

    // Presence is the declaration: what the server holds is what it says
    Assert.IsNotNull(LCaps.GetValue('tools'), LCaps.ToJSON);
    Assert.IsNotNull(LCaps.GetValue('resources'), LCaps.ToJSON);
    Assert.IsNotNull(LCaps.GetValue('prompts'), LCaps.ToJSON);
  finally
    LResult.Free;
  end;
end;

procedure TDiscoverTest.TestABareServerAdvertisesNothing;
var
  LResult: TJSONObject;
  LCaps: TJSONObject;
begin
  ConfigureBare();

  // It used to advertise tools, resources and prompts whatever it had - which
  // for a server with none of them was three promises it could not keep
  LResult := Discover;
  try
    LCaps := CapabilitiesOf(LResult);
    if Assigned(LCaps) then
    begin
      Assert.IsNull(LCaps.GetValue('tools'), LCaps.ToJSON);
      Assert.IsNull(LCaps.GetValue('resources'), LCaps.ToJSON);
      Assert.IsNull(LCaps.GetValue('prompts'), LCaps.ToJSON);
    end;
  finally
    LResult.Free;
  end;
end;

procedure TDiscoverTest.TestNoListChangedWhereNoStreamCanBeHeld;
var
  LResult: TJSONObject;
begin
  ConfigureFull();

  // Having tools and saying when they change are two statements. The second
  // needs the subscriptions/listen stream, which a transport that cannot stream
  // (WebBroker before 13.1, CGI) never holds open - so the flag says false
  // rather than promising notifications nothing could deliver.
  LResult := Discover(False);
  try
    Assert.AreEqual('false', ListChangedOf(LResult, 'tools'), LResult.ToJSON);
    Assert.AreEqual('false', ListChangedOf(LResult, 'resources'), LResult.ToJSON);
    Assert.AreEqual('false', ListChangedOf(LResult, 'prompts'), LResult.ToJSON);
  finally
    LResult.Free;
  end;
end;

procedure TDiscoverTest.TestListChangedWhereAStreamCanBeHeld;
var
  LResult: TJSONObject;
begin
  ConfigureFull();

  // The registry publishes every change it sees, so where a listen can be held
  // open the promise is one the server keeps. This request asked for a
  // document: the listen the promise is about is a request of its own.
  LResult := Discover(True);
  try
    Assert.AreEqual('true', ListChangedOf(LResult, 'tools'), LResult.ToJSON);
    Assert.AreEqual('true', ListChangedOf(LResult, 'resources'), LResult.ToJSON);
    Assert.AreEqual('true', ListChangedOf(LResult, 'prompts'), LResult.ToJSON);
  finally
    LResult.Free;
  end;
end;

procedure TDiscoverTest.TestSetListChangedFalseNeverPromisesIt;
var
  LResult: TJSONObject;
begin
  ConfigureFull();

  FServer.Plugin.Configure<IMCPConfig>
    .Server
      .SetListChanged(False)
    .BackToMCP
  .ApplyConfig;

  LResult := Discover(True);
  try
    Assert.AreEqual('false', ListChangedOf(LResult, 'tools'), LResult.ToJSON);
    Assert.AreEqual('false', ListChangedOf(LResult, 'prompts'), LResult.ToJSON);
  finally
    LResult.Free;
  end;
end;

procedure TDiscoverTest.TestSetListChangedDoesNotTouchExplicitCapabilities;
var
  LResult: TJSONObject;
begin
  ConfigureFull();

  // Explicit capabilities are reported as they were set: a server that set its
  // own says listChanged itself, or does not say it
  FServer.Plugin.Configure<IMCPConfig>
    .Server
      .SetListChanged(True)
      .SetCapabilities([TMCPCapability.Tools])
    .BackToMCP
  .ApplyConfig;

  LResult := Discover(True);
  try
    Assert.AreNotEqual('true', ListChangedOf(LResult, 'tools'), LResult.ToJSON);
  finally
    LResult.Free;
  end;
end;

procedure TDiscoverTest.TestAnExplicitCapabilitySetIsUsedVerbatim;
var
  LResult: TJSONObject;
  LCaps: TJSONObject;
begin
  ConfigureBare();

  // A server that registers late, or knows something the registry does not,
  // says so - and used to be ignored
  FServer.Plugin.Configure<IMCPConfig>
    .Server
      .SetCapabilities(
        procedure (ACapabilities: TServerCapabilities)
        begin
          ACapabilities.Tools.ListChanged := True;
          ACapabilities.Resources.Subscribe := True;
        end)
    .BackToMCP
  .ApplyConfig;

  LResult := Discover;
  try
    LCaps := CapabilitiesOf(LResult);
    Assert.IsNotNull(LCaps, LResult.ToJSON);
    Assert.Contains(LCaps.ToJSON, '"listChanged":true');
    Assert.Contains(LCaps.ToJSON, '"subscribe":true');
  finally
    LResult.Free;
  end;
end;

procedure TDiscoverTest.TestAnExplicitSetCanAdvertiseLessThanIsRegistered;
var
  LResult: TJSONObject;
  LCaps: TJSONObject;
begin
  ConfigureFull();

  // Saying less than it holds is the server's business: what it declares is
  // what it declared, not what the registries happen to contain
  FServer.Plugin.Configure<IMCPConfig>
    .Server
      .SetCapabilities([TMCPCapability.Tools])
    .BackToMCP
  .ApplyConfig;

  LResult := Discover;
  try
    LCaps := CapabilitiesOf(LResult);
    Assert.IsNotNull(LCaps.GetValue('tools'), LCaps.ToJSON);
    Assert.IsNull(LCaps.GetValue('prompts'), LCaps.ToJSON);
  finally
    LResult.Free;
  end;
end;

procedure TDiscoverTest.TestTheAppExtensionIsAdvertisedWithAUiResource;
var
  LResult: TJSONObject;
  LApp: TJSONValue;
  LMimeTypes: TJSONArray;
begin
  ConfigureBare();

  FServer.Plugin.Configure<IMCPConfig>
    .Resources
      .RegisterClass(TDiscoverApp)
    .BackToMCP
  .ApplyConfig;

  // MCP Apps is negotiated in both directions: a server with a ui:// page says
  // so, and the host learns it can render one
  LResult := Discover;
  try
    // Not FindValue: the extension identifier has dots of its own
    LApp := CapabilitiesOf(LResult).GetValue('extensions');
    if LApp is TJSONObject then
      LApp := TJSONObject(LApp).GetValue(MCP_APP_EXTENSION);
    Assert.IsTrue(LApp is TJSONObject, 'The MCP Apps extension should be advertised: ' + LResult.ToJSON);

    LMimeTypes := TJSONObject(LApp).GetValue('mimeTypes') as TJSONArray;
    Assert.IsNotNull(LMimeTypes, 'The extension settings should list the supported content types');
    Assert.AreEqual(1, LMimeTypes.Count, 'Exactly one content type is supported');
    Assert.AreEqual(MCP_APP_MIME_TYPE, LMimeTypes.Items[0].Value);
  finally
    LResult.Free;
  end;
end;

procedure TDiscoverTest.TestTheAppExtensionIsAbsentWithoutAppResources;
var
  LResult: TJSONObject;
begin
  ConfigureFull();

  LResult := Discover;
  try
    Assert.IsFalse(LResult.ToJSON.Contains(MCP_APP_EXTENSION),
      'A server without ui:// resources should not advertise MCP Apps: ' + LResult.ToJSON);
  finally
    LResult.Free;
  end;
end;

procedure TDiscoverTest.TestExplicitCapabilitiesGetNoAppExtension;
var
  LResult: TJSONObject;
begin
  ConfigureBare();

  // Explicit capabilities are reported verbatim, the extensions map included:
  // a server that sets its own declares the extension itself, or does not
  FServer.Plugin.Configure<IMCPConfig>
    .Server
      .SetCapabilities([TMCPCapability.Resources])
    .BackToMCP
    .Resources
      .RegisterClass(TDiscoverApp)
    .BackToMCP
  .ApplyConfig;

  LResult := Discover;
  try
    Assert.IsFalse(LResult.ToJSON.Contains(MCP_APP_EXTENSION), LResult.ToJSON);
  finally
    LResult.Free;
  end;
end;

procedure TDiscoverTest.TestInstructionsAreReportedWhenSet;
var
  LResult: TJSONObject;
begin
  ConfigureBare();

  FServer.Plugin.Configure<IMCPConfig>
    .Server
      .SetInstructions('Ask ping first; it answers pong.')
    .BackToMCP
  .ApplyConfig;

  LResult := Discover;
  try
    Assert.AreEqual('Ask ping first; it answers pong.',
      LResult.GetValue<string>('instructions', ''), LResult.ToJSON);
  finally
    LResult.Free;
  end;
end;

procedure TDiscoverTest.TestInstructionsAreAbsentWhenNotSet;
var
  LResult: TJSONObject;
begin
  ConfigureBare();

  LResult := Discover;
  try
    Assert.IsNull(LResult.GetValue('instructions'), LResult.ToJSON);
  finally
    LResult.Free;
  end;
end;

procedure TDiscoverTest.TestTheDescriptionTravelsWithTheIdentity;
var
  LResult: TJSONObject;
begin
  ConfigureBare();

  // What the server *is* goes with its identity, in the result _meta; what it
  // is good for goes in the instructions beside it
  FServer.Plugin.Configure<IMCPConfig>
    .Server
      .SetDescription('A server that answers pings')
    .BackToMCP
  .ApplyConfig;

  LResult := Discover;
  try
    Assert.Contains(LResult.ToJSON, 'A server that answers pings');
    Assert.Contains(LResult.ToJSON, 'serverInfo');
  finally
    LResult.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TDiscoverTest);

end.
