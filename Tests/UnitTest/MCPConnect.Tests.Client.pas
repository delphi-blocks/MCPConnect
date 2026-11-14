{******************************************************************************}
{                                                                              }
{  Delphi MCP Connect Library                                                  }
{                                                                              }
{  Copyright (c) Paolo Rossi <dev@paolorossi.net>                              }
{                Luca Minuti <code@lucaminuti.it>                              }
{                                                                              }
{  All rights reserved.                                                        }
{                                                                              }
{  https://github.com/delphi-blocks/MCPConnect                                 }
{                                                                              }
{  Licensed under the MIT license                                              }
{                                                                              }
{******************************************************************************}

/// <summary>
///   The MCP client core and the Streamable HTTP transport, against a real
///   Indy-hosted TMCPServer on loopback: discovery, tools/list paging, a tool
///   call with progress and log notifications, and a typed remote error.
/// </summary>
unit MCPConnect.Tests.Client;

interface

uses
  System.SysUtils, System.Classes, System.JSON,

  DUnitX.TestFramework,

  IdSocketHandle,

  JRPC.Core,
  JRPC.Classes,

  MCPConnect.Configuration.MCP,
  MCPConnect.MCP.Attributes,
  MCPConnect.MCP.Types.Base,
  MCPConnect.MCP.Types.Notifications,
  MCPConnect.MCP.Types.Tools,
  MCPConnect.MCP.Types.Resources,
  MCPConnect.MCP.Types.Prompts,
  MCPConnect.MCP.Types.Completion,
  MCPConnect.MCP.Types.Errors,
  MCPConnect.MCP.Response,
  MCPConnect.Transport.Indy,
  MCPConnect.Transport.Stdio,
  MCPConnect.Client,
  MCPConnect.Client.Transport.HTTP,
  MCPConnect.Client.Transport.Stdio;

/// <summary>
///   Runs this executable as a stdio MCP server: the client tests spawn it
///   with --stdio-server and talk to it over the pipes.
/// </summary>
procedure RunStdioServer;

type
  /// <summary>The structured content one test tool returns.</summary>
  TClientStructured = record
    Answer: Integer;
    LabelText: string;
  end;

  /// <summary>
  ///   A tool class the client tests call: one tool reports progress and logs,
  ///   one answers mixed content, one structured content, one a failure.
  /// </summary>
  [McpScope('clienttest')]
  TClientTestTools = class
  private
    [Context] FProgress: TMCPProgress;
    [Context] FLog: TMCPLog;
    [Context] FParams: TCallToolRequestParams;
    [Context] FConfig: TMCPConfig;
  public
    [McpTool('ping', 'Answers pong and reports progress')]
    function Ping: string;

    [McpTool('echo', 'Answers echo')]
    function Echo: string;

    [McpTool('echo_arg', 'Echoes the text it is given')]
    function EchoArg([McpParam('text', 'The text to echo')] const AText: string): string;

    [McpTool('mix', 'Answers every content type')]
    function Mix: TContentList;

    [McpTool('structured', 'Answers structured content', 'structured')]
    function Structured: TClientStructured;

    [McpTool('fail', 'Reports a failure to the model')]
    function Fail: TCallToolResult;

    [McpTool('confirm_delete', 'Asks for confirmation before answering')]
    function ConfirmDelete: TMCPResponse<string>;

    [McpTool('notify', 'Publishes a tools/list_changed notification')]
    function Notify: string;
  end;

  [McpScope('clienttest')]
  TClientTestResources = class
  public
    [McpResource('greeting', 'clienttest://greeting', 'text/plain', 'A greeting')]
    function Greeting: string;

    [McpResource('blob', 'clienttest://blob', 'application/octet-stream', 'A blob')]
    function Blob: TStream;

    [McpTemplate('item', 'clienttest://items/{id}', 'text/plain', 'One item')]
    function Item([McpParam('id', 'The item id')] const id: string): string;
  end;

  [McpScope('clienttest')]
  TClientTestPrompts = class
  public
    [McpPrompt('greet', 'Greeting', 'Greets someone')]
    function Greet([McpArgument('name', 'Who to greet')] const name: string): TPromptMessages;
  end;

  [McpScope('clienttest')]
  TClientTestCompletions = class
  public
    [McpComplete('greet', 'name')]
    function CompleteName(const AValue: string): TArray<string>;
  end;

  /// <summary>
  ///   A tool whose arguments are mirrored into Mcp-Param-* headers. The test
  ///   server validates headers Strict (the default), so a call that does not
  ///   carry them is refused with HeaderMismatch.
  /// </summary>
  [McpScope('routed')]
  TClientRoutedTools = class
  public
    [McpTool('query', 'Answers the arguments it was routed with')]
    function Query(
      [McpParam('region', 'The region to run in', 'header=Region')] const region: string;
      [McpParam('page', 'The page index', 'header=Page')] const page: Integer;
      [McpParam('flag', 'A switch', 'header=Flag')] const flag: Boolean;
      [McpParam('note', 'Not mirrored')] const note: string
    ): string;
  end;

  [TestFixture]
  TMCPClientTest = class
  private
    FServer: TMCPIndyServer;
    FTransport: TMCPHTTPTransport;
    FClient: TMCPClient;
    FURL: string;
  public
    [Setup] procedure Setup;
    [TearDown] procedure TearDown;

    [Test] procedure TestConnectDiscoversServer;
    [Test] procedure TestListToolsFollowsPaging;
    [Test] procedure TestCallReturnsRawResult;
    [Test] procedure TestProgressAndLogNotifications;
    [Test] procedure TestRemoteErrorIsTyped;
    [Test] procedure TestCallToolContentMapping;
    [Test] procedure TestCallToolArguments;
    [Test] procedure TestCallToolStructured;
    [Test] procedure TestCallToolIsError;
    [Test] procedure TestCallToolMrtrElicitation;
    [Test] procedure TestCallToolMrtrWithoutCapability;
    [Test] procedure TestListenReceivesNotification;
    [Test] procedure TestListenCancellation;
    [Test] procedure TestListResources;
    [Test] procedure TestReadResourceText;
    [Test] procedure TestReadResourceBlob;
    [Test] procedure TestListPrompts;
    [Test] procedure TestGetPrompt;
    [Test] procedure TestComplete;
    [Test] procedure TestErrorDataDecoding;
    [Test] procedure TestCallToolParamHeaders;
    [Test] procedure TestCallToolParamHeadersEncoded;
    [Test] procedure TestCallToolParamHeadersAfterListTools;
  end;

  /// <summary>
  ///   The same client over stdio: this executable is spawned with
  ///   --stdio-server and driven through a real child process and pipes.
  /// </summary>
  [TestFixture]
  TMCPStdioClientTest = class
  private
    FTransport: TMCPStdioTransport;
    FClient: TMCPClient;
  public
    [Setup] procedure Setup;
    [TearDown] procedure TearDown;

    [Test] procedure TestStdioConnect;
    [Test] procedure TestStdioListTools;
    [Test] procedure TestStdioCallToolWithMrtr;
    [Test] procedure TestStdioProgress;
  end;

implementation

uses
  System.Generics.Collections, System.SyncObjs,

  JRPC.Client,
  MCPConnect.Content.Writers.RTL,
  MCPConnect.MCP.Types.Subscriptions;

{ stdio server }

procedure RunStdioServer;
var
  LServer: TMCPStdioServer;
begin
  LServer := TMCPStdioServer.CreateMCPServer(nil);
  try
    LServer.MCPServer.Plugin.Configure<IMCPConfig>
      .Server
        .SetName('stdio-test')
        .SetVersion('1.0')
      .BackToMCP
      .Tools
        .RegisterClass(TClientTestTools)
      .BackToMCP
    .ApplyConfig;

    LServer.StartServerAndWait;
  finally
    LServer.Free;
  end;
end;

{ TClientTestTools }

function TClientTestTools.Ping: string;
begin
  FProgress.Report(1, 2, 'half way');
  FLog.Log(TMCPLogLevel.Warning, 'a warning from the tool');
  Result := 'pong';
end;

function TClientTestTools.Echo: string;
begin
  Result := 'echo';
end;

function TClientTestTools.EchoArg(const AText: string): string;
begin
  Result := AText;
end;

function TClientTestTools.Mix: TContentList;
var
  LEmbedded: TEmbeddedResourceText;
begin
  Result := TContentList.Create;
  Result.AddText('first');
  Result.AddImage('image/png', 'aGVsbG8=');       // base64 of 'hello'
  Result.AddAudio('audio/wav', 'YXVkaW8=');       // base64 of 'audio'
  Result.AddLink('text/html', 'https://example.com', 'Example');
  Result.AddBlob('application/octet-stream', 'YmxvYg==');  // base64 of 'blob'

  // The embedded resource has two shapes; the blob one came from AddBlob, this
  // is the text one.
  LEmbedded := TEmbeddedResourceText.Create;
  LEmbedded.Resource.Uri := 'resource://embedded';
  LEmbedded.Resource.MimeType := 'text/plain';
  LEmbedded.Resource.Text := 'embedded';
  Result.Add(LEmbedded);
end;

function TClientTestTools.Structured: TClientStructured;
begin
  Result.Answer := 42;
  Result.LabelText := 'structured';
end;

function TClientTestTools.Fail: TCallToolResult;
begin
  Result := TCallToolReply.Fail('boom');
end;

function TClientTestTools.ConfirmDelete: TMCPResponse<string>;
begin
  // No answer yet: ask, carrying a requestState the retry will echo back.
  if not FParams.InputResponses.Confirmed('confirm') then
  begin
    var LInput := TMCPInput.New('confirm-state');
    LInput.Confirm('confirm', 'Proceed?');
    Exit(TMCPResponse<string>.Needs(LInput));
  end;

  Result := TMCPResponse<string>.Value('confirmed');
end;

function TClientTestTools.Notify: string;
begin
  // Fans out to every open subscriptions/listen stream.
  FConfig.Subscriptions.ToolsChanged;
  Result := 'notified';
end;

{ TClientTestResources }

function TClientTestResources.Greeting: string;
begin
  Result := 'hello from the resource';
end;

function TClientTestResources.Blob: TStream;
var
  LBytes: TBytes;
begin
  LBytes := [1, 2, 3];
  Result := TBytesStream.Create(LBytes);
end;

function TClientTestResources.Item(const id: string): string;
begin
  Result := 'item ' + id;
end;

{ TClientTestPrompts }

function TClientTestPrompts.Greet(const name: string): TPromptMessages;
begin
  Result := TPromptMessages.Create;
  Result.AddText(TRole.User, 'Hello ' + name);
end;

{ TClientTestCompletions }

function TClientTestCompletions.CompleteName(const AValue: string): TArray<string>;
begin
  if (AValue = '') or SameText(AValue, 'a') then
    Result := ['Ada', 'Alan']
  else if SameText(AValue, 'ad') then
    Result := ['Ada']
  else
    Result := [];
end;

{ TClientRoutedTools }

function TClientRoutedTools.Query(const region: string; const page: Integer;
  const flag: Boolean; const note: string): string;
begin
  Result := Format('%s|%d|%s|%s', [region, page, BoolToStr(flag, True), note]);
end;

{ TMCPClientTest }

procedure TMCPClientTest.Setup;
var
  LBinding: TIdSocketHandle;
  LConfig: IMCPConfig;
begin
  FServer := TMCPIndyServer.CreateMCPServer(nil);
  LConfig := FServer.MCPServer.Plugin.Configure<IMCPConfig>;
  LConfig.Server
    .SetName('mcp-client-test')
    .SetVersion('1.0')
    // One tool per page, so tools/list has to follow the cursor.
    .SetPageSize(1)
    .RegisterWriter(TMCPStreamWriter);

  try
    LConfig.Tools.RegisterClass(TClientTestTools);
    LConfig.Tools.RegisterClass(TClientRoutedTools);
  except
    on E: Exception do
      raise Exception.Create('tools: ' + E.Message);
  end;

  try
    LConfig.Resources.RegisterClass(TClientTestResources);
  except
    on E: Exception do
      raise Exception.Create('resources: ' + E.Message);
  end;

  try
    LConfig.Prompts.RegisterClass(TClientTestPrompts);
  except
    on E: Exception do
      raise Exception.Create('prompts: ' + E.Message);
  end;

  try
    LConfig.Completions.RegisterClass(TClientTestCompletions);
  except
    on E: Exception do
      raise Exception.Create('completions: ' + E.Message);
  end;

  LConfig.ApplyConfig;

  LBinding := FServer.Bindings.Add;
  LBinding.IP := '127.0.0.1';
  LBinding.Port := 0;
  FServer.Active := True;

  FURL := Format('http://127.0.0.1:%d/mcp', [FServer.Bindings[0].Port]);

  FTransport := TMCPHTTPTransport.Create(FURL);
  FClient := TMCPClient.Create(FTransport);
  FClient.Connect;
end;

procedure TMCPClientTest.TearDown;
begin
  FClient.Free;
  // The client held the only interface reference to the transport.
  FTransport := nil;
  FServer.Active := False;
  FServer.Free;
end;

procedure TMCPClientTest.TestConnectDiscoversServer;
begin
  Assert.AreEqual('2026-07-28', FClient.ProtocolVersion);
  Assert.AreEqual('mcp-client-test', FClient.ServerInfo.Name);
  Assert.AreEqual('1.0', FClient.ServerInfo.Version);
end;

procedure TMCPClientTest.TestListToolsFollowsPaging;
var
  LTools: TMCPTools;
begin
  LTools := FClient.ListTools;
  try
    Assert.AreEqual(9, LTools.Count);
    Assert.AreEqual('clienttest_confirm_delete', LTools[0].Name);
    Assert.AreEqual('clienttest_echo', LTools[1].Name);
    Assert.AreEqual('clienttest_echo_arg', LTools[2].Name);
    Assert.AreEqual('clienttest_fail', LTools[3].Name);
    Assert.AreEqual('clienttest_mix', LTools[4].Name);
    Assert.AreEqual('clienttest_notify', LTools[5].Name);
    Assert.AreEqual('clienttest_ping', LTools[6].Name);
    Assert.AreEqual('clienttest_structured', LTools[7].Name);
    Assert.AreEqual('routed_query', LTools[8].Name);
  finally
    LTools.Free;
  end;
end;

procedure TMCPClientTest.TestCallReturnsRawResult;
var
  LParams: TCallToolRequestParams;
  LResult: TJSONValue;
begin
  LParams := TCallToolRequestParams.Create;
  try
    LParams.Name := 'clienttest_echo';

    LResult := FClient.Call('tools/call', LParams);
    try
      Assert.IsTrue(LResult.ToJSON.Contains('echo'), LResult.ToJSON);
    finally
      LResult.Free;
    end;
  finally
    LParams.Free;
  end;
end;

procedure TMCPClientTest.TestProgressAndLogNotifications;
var
  LParams: TCallToolRequestParams;
  LResult: TJSONValue;
  LMethods: TStringList;
begin
  LMethods := TStringList.Create;
  try
    FClient.OnNotification :=
      procedure(const ANotification: TJRPCNotification)
      begin
        LMethods.Add(ANotification.Method);
      end;

    LParams := TCallToolRequestParams.Create;
    try
      LParams.Name := 'clienttest_ping';
      // Both channels are opt-in per request.
      LParams.RequestMeta.SetProgressToken('progress-1');
      LParams.RequestMeta.LogLevel := TMCPLogLevel.Warning;

      LResult := FClient.Call('tools/call', LParams);
      try
        Assert.IsTrue(LResult.ToJSON.Contains('pong'), LResult.ToJSON);
      finally
        LResult.Free;
      end;
    finally
      LParams.Free;
    end;

    Assert.IsTrue(LMethods.IndexOf('notifications/progress') >= 0,
      'progress notification not seen: ' + LMethods.Text);
    Assert.IsTrue(LMethods.IndexOf('notifications/message') >= 0,
      'log notification not seen: ' + LMethods.Text);
  finally
    FClient.OnNotification := nil;
    LMethods.Free;
  end;
end;

procedure TMCPClientTest.TestRemoteErrorIsTyped;
var
  LParams: TCallToolRequestParams;
begin
  LParams := TCallToolRequestParams.Create;
  try
    LParams.Name := 'clienttest_missing';

    try
      FClient.Call('tools/call', LParams).Free;
      Assert.Fail('expected EJRPCRemoteError');
    except
      on E: EJRPCRemoteError do
        // The server answers an unknown tool with Invalid Params.
        Assert.AreEqual(-32602, E.Code, E.Message);
    end;
  finally
    LParams.Free;
  end;
end;

procedure TMCPClientTest.TestCallToolContentMapping;
var
  LResult: TCallToolResult;
begin
  LResult := FClient.CallTool('clienttest_mix');
  try
    Assert.AreEqual(6, LResult.Content.Count);

    Assert.IsTrue(LResult.Content[0] is TTextContent, LResult.Content[0].ClassName);
    Assert.AreEqual('first', TTextContent(LResult.Content[0]).Text);

    Assert.IsTrue(LResult.Content[1] is TImageContent, LResult.Content[1].ClassName);
    Assert.AreEqual('image/png', TImageContent(LResult.Content[1]).MimeType);
    Assert.AreEqual('aGVsbG8=', TImageContent(LResult.Content[1]).Data);

    Assert.IsTrue(LResult.Content[2] is TAudioContent, LResult.Content[2].ClassName);
    Assert.AreEqual('audio/wav', TAudioContent(LResult.Content[2]).MimeType);

    Assert.IsTrue(LResult.Content[3] is TResourceLink, LResult.Content[3].ClassName);
    Assert.AreEqual('https://example.com', TResourceLink(LResult.Content[3]).Uri);
    Assert.AreEqual('Example', TResourceLink(LResult.Content[3]).Description);

    Assert.IsTrue(LResult.Content[4] is TEmbeddedResourceBlob,
      LResult.Content[4].ClassName);
    Assert.IsTrue(TEmbeddedResourceBlob(LResult.Content[4]).Resource.MimeType =
      'application/octet-stream');
    Assert.IsTrue(TEmbeddedResourceBlob(LResult.Content[4]).Resource.Blob =
      'YmxvYg==');

    Assert.IsTrue(LResult.Content[5] is TEmbeddedResourceText,
      LResult.Content[5].ClassName);
    Assert.AreEqual('embedded', TEmbeddedResourceText(LResult.Content[5]).Resource.Text);
  finally
    LResult.Free;
  end;
end;

procedure TMCPClientTest.TestCallToolArguments;
var
  LArguments: TJSONObject;
  LResult: TCallToolResult;
begin
  LArguments := TJSONObject.Create;
  try
    LArguments.AddPair('text', 'hello');

    LResult := FClient.CallTool('clienttest_echo_arg', LArguments);
    try
      Assert.AreEqual(1, LResult.Content.Count);
      Assert.IsTrue(LResult.Content[0] is TTextContent, LResult.Content[0].ClassName);
      Assert.AreEqual('hello', TTextContent(LResult.Content[0]).Text);
    finally
      LResult.Free;
    end;

    // CallTool clones the arguments, so the caller still owns and holds them.
    Assert.IsNotNull(LArguments.GetValue('text'));
  finally
    LArguments.Free;
  end;
end;

procedure TMCPClientTest.TestCallToolStructured;
var
  LResult: TCallToolResult;
  LStructured: TJSONObject;
begin
  LResult := FClient.CallTool('clienttest_structured');
  try
    Assert.IsNotNull(LResult.StructuredContent);
    Assert.IsTrue(LResult.StructuredContent is TJSONObject,
      LResult.StructuredContent.ClassName);

    LStructured := TJSONObject(LResult.StructuredContent);
    Assert.AreEqual(42, LStructured.GetValue<Integer>('answer'));
    Assert.AreEqual('structured', LStructured.GetValue<string>('labelText'));
  finally
    LResult.Free;
  end;
end;

procedure TMCPClientTest.TestCallToolIsError;
var
  LResult: TCallToolResult;
begin
  LResult := FClient.CallTool('clienttest_fail');
  try
    Assert.IsTrue(LResult.IsError.HasValue, 'isError must be present');
    Assert.IsTrue(LResult.IsError.Value, 'the tool reported a failure');

    Assert.IsTrue(LResult.Content.Count >= 1);
    Assert.IsTrue(LResult.Content[0] is TTextContent, LResult.Content[0].ClassName);
    Assert.IsTrue(TTextContent(LResult.Content[0]).Text.Contains('boom'));
  finally
    LResult.Free;
  end;
end;

procedure TMCPClientTest.TestCallToolMrtrElicitation;
var
  LResult: TCallToolResult;
  LCalls: Integer;
begin
  LCalls := 0;
  FClient.OnElicitation :=
    function(const ARequest: TElicitRequestParams): TElicitResult
    begin
      Inc(LCalls);
      Assert.AreEqual('Proceed?', ARequest.Message);

      Result := TElicitResult.Create;
      Result.Action := TElicitAction.Accept;
      Result.Content := TJSONObject.ParseJSONValue('{"confirm":true}');
    end;

  try
    LResult := FClient.CallTool('clienttest_confirm_delete');
    try
      Assert.AreEqual(1, LResult.Content.Count);
      Assert.AreEqual('confirmed', TTextContent(LResult.Content[0]).Text);
    finally
      LResult.Free;
    end;

    Assert.AreEqual(1, LCalls, 'the interim round asked once');
  finally
    FClient.OnElicitation := nil;
  end;
end;

procedure TMCPClientTest.TestCallToolMrtrWithoutCapability;
var
  LCaps: TMCPClientCapabilities;
begin
  // No elicitation handler: the client declares no such capability, and the
  // server refuses the interim result with -32021 instead of asking.
  FClient.OnElicitation := nil;

  try
    FClient.CallTool('clienttest_confirm_delete').Free;
    Assert.Fail('expected EJRPCRemoteError');
  except
    on E: EJRPCRemoteError do
    begin
      Assert.AreEqual(MCP_MISSING_REQUIRED_CLIENT_CAPABILITY, E.Code, E.Message);
      Assert.IsTrue(TMCPClientErrors.TryRequiredCapabilities(E, LCaps));
      // No mode was declared at all, so the server asks for elicitation as a
      // whole rather than for the form mode specifically.
      Assert.IsTrue(TMCPClientCapability.Elicitation in LCaps);
    end;
  end;
end;

procedure TMCPClientTest.TestListenReceivesNotification;
var
  LFilter: TSubscriptionFilter;
  LSub: IMCPSubscription;
  LReceived: TEvent;
  LMethod: string;
begin
  FClient.TransportFactory :=
    function: IJRPCStreamingTransport
    begin
      // A dedicated transport for the stream: no read timeout, since a listen
      // stays open until it is closed.
      Result := TMCPHTTPTransport.Create(FURL, '', 0);
    end;

  LFilter := TSubscriptionFilter.Create;
  try
    LFilter.ToolsListChanged := True;

    LReceived := TEvent.Create(nil, True, False, '');
    try
      LMethod := '';
      LSub := FClient.Listen(LFilter,
        procedure(const ANotification: TJRPCNotification)
        begin
          LMethod := ANotification.Method;
          LReceived.SetEvent;
        end);
      try
        Assert.IsTrue(LSub.Acknowledged, 'Listen returns only after the ack');

        // A call on the client's own transport, while the listen stream is
        // open on its dedicated one.
        FClient.CallTool('clienttest_notify').Free;

        Assert.IsTrue(LReceived.WaitFor(5000) = wrSignaled,
          'the subscription notification must arrive');
        Assert.AreEqual('notifications/tools/list_changed', LMethod);
      finally
        LSub.Close;
        Assert.IsTrue(LSub.Wait(5000), 'the stream must end after Close');
        LSub := nil;
      end;
    finally
      LReceived.Free;
    end;
  finally
    LFilter.Free;
    FClient.TransportFactory := nil;
  end;
end;

procedure TMCPClientTest.TestListenCancellation;
var
  LFilter: TSubscriptionFilter;
  LSub: IMCPSubscription;
begin
  FClient.TransportFactory :=
    function: IJRPCStreamingTransport
    begin
      Result := TMCPHTTPTransport.Create(FURL, '', 0);
    end;

  LFilter := TSubscriptionFilter.Create;
  try
    LFilter.ResourcesListChanged := True;

    LSub := FClient.Listen(LFilter, nil);
    try
      Assert.IsTrue(LSub.Acknowledged);
    finally
      LSub.Close;
    end;

    Assert.IsTrue(LSub.Wait(5000), 'Close must end the stream');
    Assert.IsTrue(LSub.Completed);
  finally
    LFilter.Free;
    FClient.TransportFactory := nil;
  end;
end;

procedure TMCPClientTest.TestListResources;
var
  LResources: TMCPResources;
  LTemplates: TMCPTemplates;
begin
  LResources := FClient.ListResources;
  try
    Assert.AreEqual(2, LResources.Count);
    Assert.AreEqual('clienttest://blob', LResources[0].Uri);
    Assert.AreEqual('clienttest://greeting', LResources[1].Uri);
  finally
    LResources.Free;
  end;

  LTemplates := FClient.ListResourceTemplates;
  try
    Assert.AreEqual(1, LTemplates.Count);
    Assert.AreEqual('clienttest://items/{id}', LTemplates[0].UriTemplate.Value);
  finally
    LTemplates.Free;
  end;
end;

procedure TMCPClientTest.TestReadResourceText;
var
  LResult: TReadResourceResult;
  LItem: TReadResourceResult;
begin
  LResult := FClient.ReadResource('clienttest://greeting');
  try
    Assert.AreEqual(1, LResult.Contents.Count);
    Assert.IsTrue(LResult.Contents[0] is TTextResourceContents);
    Assert.AreEqual('hello from the resource',
      TTextResourceContents(LResult.Contents[0]).Text);
    Assert.AreEqual('text/plain', LResult.Contents[0].MimeType.Value);
    // The test server sets no hints, so the honest defaults travel.
    Assert.AreEqual(UInt64(0), LResult.TtlMs);
    Assert.AreEqual(TCacheScope.ScopePrivate, LResult.CacheScope);
  finally
    LResult.Free;
  end;

  // The template is read like any other resource.
  LItem := FClient.ReadResource('clienttest://items/7');
  try
    Assert.AreEqual('item 7', TTextResourceContents(LItem.Contents[0]).Text);
  finally
    LItem.Free;
  end;
end;

procedure TMCPClientTest.TestReadResourceBlob;
var
  LResult: TReadResourceResult;
begin
  LResult := FClient.ReadResource('clienttest://blob');
  try
    Assert.AreEqual(1, LResult.Contents.Count);
    Assert.IsTrue(LResult.Contents[0] is TBlobResourceContents);
    Assert.AreEqual('AQID', TBlobResourceContents(LResult.Contents[0]).Blob);
  finally
    LResult.Free;
  end;
end;

procedure TMCPClientTest.TestListPrompts;
var
  LPrompts: TMCPPrompts;
begin
  LPrompts := FClient.ListPrompts;
  try
    Assert.AreEqual(1, LPrompts.Count);
    Assert.AreEqual('clienttest_greet', LPrompts[0].Name);
  finally
    LPrompts.Free;
  end;
end;

procedure TMCPClientTest.TestGetPrompt;
var
  LArgs: TJSONObject;
  LResult: TGetPromptResult;
  LMessage: TPromptMessageBase;
begin
  LArgs := TJSONObject.ParseJSONValue('{"name":"Ada"}') as TJSONObject;
  try
    LResult := FClient.GetPrompt('clienttest_greet', LArgs);
    try
      Assert.AreEqual(1, LResult.Messages.Count);

      LMessage := LResult.Messages[0];
      Assert.IsTrue(LMessage is TPromptMessage<TTextContent>);
      Assert.AreEqual(TRole.User, LMessage.Role);
      Assert.AreEqual('Hello Ada',
        (LMessage as TPromptMessage<TTextContent>).Content.Text);
    finally
      LResult.Free;
    end;
  finally
    LArgs.Free;
  end;
end;

procedure TMCPClientTest.TestComplete;
var
  LRef: TMCPCompletionReference;
  LResult: TCompleteResult;
begin
  LRef := TMCPCompletionReference.Create;
  try
    LRef.&Type := MCP_REF_PROMPT;
    LRef.Name := 'clienttest_greet';

    LResult := FClient.Complete(LRef, 'name', 'A');
    try
      Assert.AreEqual(2, Length(LResult.Completion.Values));
      Assert.AreEqual('Ada', LResult.Completion.Values[0]);
      Assert.AreEqual('Alan', LResult.Completion.Values[1]);
    finally
      LResult.Free;
    end;
  finally
    LRef.Free;
  end;
end;

procedure TMCPClientTest.TestErrorDataDecoding;
var
  LError: EJRPCRemoteError;
  LData: TMCPUnsupportedVersionErrorData;
  LCaps: TMCPClientCapabilities;
begin
  LError := EJRPCRemoteError.Create(MCP_UNSUPPORTED_PROTOCOL_VERSION, 'unsupported',
    TJSONObject.ParseJSONValue(
      '{"requested":"2025-11-25","supported":["2026-07-28","2025-11-25"]}'));
  try
    Assert.IsTrue(TMCPClientErrors.TryUnsupportedVersion(LError, LData));
    try
      Assert.AreEqual('2025-11-25', LData.Requested);
      Assert.AreEqual(2, Length(LData.Supported));
      Assert.AreEqual('2026-07-28', LData.Supported[0]);
    finally
      LData.Free;
    end;
  finally
    LError.Free;
  end;

  LError := EJRPCRemoteError.Create(MCP_MISSING_REQUIRED_CLIENT_CAPABILITY, 'missing',
    TJSONObject.ParseJSONValue(
      '{"requiredCapabilities":{"elicitation":{"form":{}},"roots":{}}}'));
  try
    Assert.IsTrue(TMCPClientErrors.TryRequiredCapabilities(LError, LCaps));
    Assert.IsTrue(TMCPClientCapability.Elicitation in LCaps);
    Assert.IsTrue(TMCPClientCapability.ElicitationForm in LCaps);
    Assert.IsTrue(TMCPClientCapability.Roots in LCaps);
  finally
    LError.Free;
  end;
end;

/// <summary>
///   Calls routed_query and answers the text it returned. The tool is not
///   listed first: the call itself has to learn the annotations.
/// </summary>
function CallRouted(AClient: TMCPClient; const ARegion: string; APage: Integer;
  AFlag: Boolean): string;
var
  LArguments: TJSONObject;
  LResult: TCallToolResult;
begin
  LArguments := TJSONObject.Create;
  try
    LArguments.AddPair('region', ARegion);
    LArguments.AddPair('page', TJSONNumber.Create(APage));
    LArguments.AddPair('flag', TJSONBool.Create(AFlag));
    LArguments.AddPair('note', 'n');

    LResult := AClient.CallTool('routed_query', LArguments);
    try
      Assert.AreEqual(1, LResult.Content.Count);
      Result := (LResult.Content[0] as TTextContent).Text;
    finally
      LResult.Free;
    end;
  finally
    LArguments.Free;
  end;
end;

procedure TMCPClientTest.TestCallToolParamHeaders;
begin
  // The server validates Strict: without Mcp-Param-Region/-Page/-Flag the
  // call is refused with HeaderMismatch before the tool runs.
  Assert.AreEqual('eu-west|3|True|n', CallRouted(FClient, 'eu-west', 3, True));
end;

procedure TMCPClientTest.TestCallToolParamHeadersEncoded;
var
  LRegion: string;
begin
  // Non-ASCII, and a trailing blank: neither can travel as a plain header
  // value, so both go in the Base64 sentinel the server decodes.
  LRegion := 'citt' + #$00E0 + ' ';
  Assert.AreEqual(LRegion + '|0|False|n', CallRouted(FClient, LRegion, 0, False));

  // The empty string is a value, not an absent header
  Assert.AreEqual('|1|True|n', CallRouted(FClient, '', 1, True));
end;

procedure TMCPClientTest.TestCallToolParamHeadersAfterListTools;
begin
  // An explicit listing hands the annotations over too; the call after it
  // has nothing left to learn.
  FClient.ListTools.Free;
  Assert.AreEqual('us-east|7|True|n', CallRouted(FClient, 'us-east', 7, True));
end;

procedure TMCPStdioClientTest.Setup;
begin
  FTransport := TMCPStdioTransport.Create(
    Format('"%s" --stdio-server', [ParamStr(0)]));

  FClient := TMCPClient.Create(FTransport);
  FClient.CallTimeout := 15000;
  FClient.Connect;
end;

procedure TMCPStdioClientTest.TearDown;
begin
  FClient.Free;
  // The connection held the transport's only interface reference.
  FTransport := nil;
end;

procedure TMCPStdioClientTest.TestStdioConnect;
begin
  Assert.AreEqual('2026-07-28', FClient.ProtocolVersion);
  Assert.AreEqual('stdio-test', FClient.ServerInfo.Name);
  Assert.AreEqual('1.0', FClient.ServerInfo.Version);
end;

procedure TMCPStdioClientTest.TestStdioListTools;
var
  LTools: TMCPTools;
begin
  LTools := FClient.ListTools;
  try
    Assert.AreEqual(8, LTools.Count);
    Assert.AreEqual('clienttest_echo', LTools[1].Name);
  finally
    LTools.Free;
  end;
end;

procedure TMCPStdioClientTest.TestStdioCallToolWithMrtr;
var
  LResult: TCallToolResult;
begin
  FClient.OnElicitation :=
    function(const ARequest: TElicitRequestParams): TElicitResult
    begin
      Result := TElicitResult.Create;
      Result.Action := TElicitAction.Accept;
      Result.Content := TJSONObject.ParseJSONValue('{"confirm":true}');
    end;

  try
    LResult := FClient.CallTool('clienttest_confirm_delete');
    try
      Assert.AreEqual(1, LResult.Content.Count);
      Assert.AreEqual('confirmed', TTextContent(LResult.Content[0]).Text);
    finally
      LResult.Free;
    end;
  finally
    FClient.OnElicitation := nil;
  end;
end;

procedure TMCPStdioClientTest.TestStdioProgress;
var
  LParams: TCallToolRequestParams;
  LResult: TJSONValue;
  LMethods: TStringList;
begin
  LMethods := TStringList.Create;
  try
    FClient.OnNotification :=
      procedure(const ANotification: TJRPCNotification)
      begin
        LMethods.Add(ANotification.Method);
      end;

    LParams := TCallToolRequestParams.Create;
    try
      LParams.Name := 'clienttest_ping';
      LParams.RequestMeta.SetProgressToken('stdio-progress');
      LParams.RequestMeta.LogLevel := TMCPLogLevel.Warning;

      LResult := FClient.Call('tools/call', LParams);
      try
        Assert.IsTrue(LResult.ToJSON.Contains('pong'), LResult.ToJSON);
      finally
        LResult.Free;
      end;
    finally
      LParams.Free;
    end;

    Assert.IsTrue(LMethods.IndexOf('notifications/progress') >= 0, LMethods.Text);
    Assert.IsTrue(LMethods.IndexOf('notifications/message') >= 0, LMethods.Text);
  finally
    FClient.OnNotification := nil;
    LMethods.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TMCPClientTest);
  TDUnitX.RegisterTestFixture(TMCPStdioClientTest);

end.
