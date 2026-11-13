program MCPClient;

{
  MCPConnect console client demo.

  Connects to an MCP 2026-07-28 server and walks the client API: discovery,
  tools, resources, prompts, MRTR and (over HTTP) a subscriptions/listen
  stream. It is the reference usage of the units in Source/:

    * TMCPHTTPTransport / TMCPStdioTransport, both behind TMCPClient
    * typed results: TCallToolResult, TReadResourceResult, TGetPromptResult
    * MRTR handlers: OnElicitation, OnSampling, OnRoots
    * automatic paging in ListTools / ListResources / ListPrompts

  Run it against the Indy demo server:

    MCPServerIndy.exe            (listens on http://localhost:8080/)
    MCPClient.exe

  or against the stdio server:

    MCPClient.exe --stdio "...\MCPServerStdio.exe"

  See README.md for the options and what the default tour does.

  The demo answers MRTR requests automatically: a form is accepted with a
  plausible value for every field, sampling returns a canned message, and
  roots returns the current directory.
}

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Classes,
  System.JSON,
  System.Generics.Collections,
  System.IOUtils,
  System.SyncObjs,

  JRPC.Client,
  JRPC.Connection,
  JRPC.Core,

  MCPConnect.Client,
  MCPConnect.Client.Transport.HTTP,
  MCPConnect.Client.Transport.Stdio,
  MCPConnect.MCP.Types.Base,
  MCPConnect.MCP.Types.Tool,
  MCPConnect.MCP.Types.Tools,
  MCPConnect.MCP.Types.Resources,
  MCPConnect.MCP.Types.Prompts,
  MCPConnect.MCP.Types.Mrtr,
  MCPConnect.MCP.Types.Subscriptions;

type
  TDemoOptions = record
    URL: string;
    Token: string;
    StdioCommand: string;
    CallTool: string;
    CallArguments: string;
    CallArgumentsFile: string;
    ReadUri: string;
    PromptName: string;
    PromptArguments: string;
    ListenSeconds: Integer;
    ListOnly: Boolean;
    ShowHelp: Boolean;
  end;

function ParseOptions: TDemoOptions;
var
  I: Integer;
  LOption, LName: string;

  function NeedValue: string;
  begin
    Inc(I);
    if I > ParamCount then
      raise Exception.CreateFmt('%s needs a value', [LOption]);
    Result := ParamStr(I);
  end;

begin
  Result.URL := 'http://localhost:8080/mcp';
  Result.ListenSeconds := 0;

  I := 1;
  while I <= ParamCount do
  begin
    LOption := ParamStr(I);
    LName := LowerCase(LOption);

    if (LName = '--http') or (LName = '-u') then
      Result.URL := NeedValue
    else if (LName = '--stdio') or (LName = '-s') then
      Result.StdioCommand := NeedValue
    else if (LName = '--token') or (LName = '-t') then
      Result.Token := NeedValue
    else if (LName = '--call') or (LName = '-c') then
      Result.CallTool := NeedValue
    else if LName = '--args' then
      Result.CallArguments := NeedValue
    else if LName = '--args-file' then
      Result.CallArgumentsFile := NeedValue
    else if LName = '--read' then
      Result.ReadUri := NeedValue
    else if LName = '--prompt' then
      Result.PromptName := NeedValue
    else if LName = '--prompt-args' then
      Result.PromptArguments := NeedValue
    else if LName = '--listen' then
      Result.ListenSeconds := StrToIntDef(NeedValue, 0)
    else if LName = '--list' then
      Result.ListOnly := True
    else if (LName = '--help') or (LName = '-h') or (LName = '/?') then
      Result.ShowHelp := True
    else
      raise Exception.CreateFmt('Unknown option: %s (try --help)', [LOption]);

    Inc(I);
  end;
end;

procedure PrintHelp;
begin
  Writeln('MCPConnect console client demo.');
  Writeln;
  Writeln('Usage: MCPClient [options]');
  Writeln;
  Writeln('Connection:');
  Writeln('  --http URL          MCP endpoint (default http://localhost:8080/mcp)');
  Writeln('  --stdio COMMAND     launch COMMAND and speak MCP on its stdin/stdout');
  Writeln('  --token TOKEN       bearer token for HTTP');
  Writeln;
  Writeln('Actions (the default is a tour):');
  Writeln('  --list              list tools, resources, templates and prompts, then stop');
  Writeln('  --read URI          read a resource (default: the first one listed)');
  Writeln('  --call NAME         call a tool (default: list_tasks, or the first tool');
  Writeln('                      that takes no required arguments)');
  Writeln('  --args JSON         arguments for --call, e.g. --args ''{"task_id":1}''');
  Writeln('  --args-file FILE    read the arguments from a JSON file instead');
  Writeln('  --prompt NAME       get a prompt (default: the first one listed)');
  Writeln('  --prompt-args JSON  arguments for --prompt');
  Writeln('  --listen SECONDS    open a subscriptions/listen stream and report');
  Writeln('  --help              this text');
  Writeln;
  Writeln('Examples:');
  Writeln('  MCPClient');
  Writeln('  MCPClient --call delete_task --args ''{"id":1}''');
  Writeln('  MCPClient --read text://todo/summary');
  Writeln('  MCPClient --prompt weekly-review --prompt-args ''{"focus_area":"work"}''');
  Writeln('  MCPClient --stdio "C:\path\to\MCPServerStdio.exe"');
end;

function ArgsFromJSON(const AJSON: string): TJSONObject;
var
  LValue: TJSONValue;
begin
  if Trim(AJSON) = '' then
    Exit(TJSONObject.Create);

  LValue := TJSONObject.ParseJSONValue(AJSON);
  if not (LValue is TJSONObject) then
  begin
    LValue.Free;
    raise Exception.Create('Arguments must be a JSON object');
  end;

  Result := TJSONObject(LValue);
end;

function FindTool(ATools: TMCPTools; const AName: string): TMCPTool;
var
  LTool: TMCPTool;
begin
  Result := nil;
  for LTool in ATools do
    if LTool.Name = AName then
      Exit(LTool);
end;

function FirstToolWithoutRequiredArguments(ATools: TMCPTools): TMCPTool;
var
  LTool: TMCPTool;
  LRequired: TJSONValue;
begin
  Result := nil;
  for LTool in ATools do
  begin
    LRequired := nil;
    if Assigned(LTool.InputSchema) then
      LRequired := LTool.InputSchema.GetValue('required');

    if not (LRequired is TJSONArray) or (TJSONArray(LRequired).Count = 0) then
      Exit(LTool);
  end;
end;

function DefaultFormAnswer(ASchema: TJSONObject): TJSONObject;
var
  LProperties: TJSONObject;
  LPair: TJSONPair;
  LProperty: TJSONObject;
  LEnum: TJSONArray;
  LName, LType: string;
begin
  Result := TJSONObject.Create;

  if not Assigned(ASchema) then
    Exit;
  if not ASchema.TryGetValue<TJSONObject>('properties', LProperties) then
    Exit;

  for LPair in LProperties do
  begin
    if not (LPair.JsonValue is TJSONObject) then
      Continue;
    if not Assigned(LPair.JsonString) then
      Continue;

    LName := LPair.JsonString.Value;
    LProperty := TJSONObject(LPair.JsonValue);
    LType := LProperty.GetValue<string>('type', '');

    if LType = 'boolean' then
      // "confirm" is what lets the request proceed; anything else (such as
      // "dontAskAgain") defaults to false.
      Result.AddPair(LName, TJSONBool.Create(SameText(LName, 'confirm')))
    else if (LType = 'integer') or (LType = 'number') then
      Result.AddPair(LName, TJSONNumber.Create(0))
    else if LProperty.TryGetValue<TJSONArray>('enum', LEnum) and (LEnum.Count > 0) then
      Result.AddPair(LName, TJSONString.Create(LEnum.Items[0].Value))
    else
      Result.AddPair(LName, TJSONString.Create('demo'));
  end;
end;

procedure InstallHandlers(AClient: TMCPClient);
begin
  AClient.OnElicitation :=
    function(const ARequest: TElicitRequestParams): TElicitResult
    begin
      Writeln;
      Writeln('  [elicitation] ', ARequest.Message);
      if Assigned(ARequest.RequestedSchema) then
        Writeln('  [elicitation] schema: ', ARequest.RequestedSchema.ToJSON);

      Result := TElicitResult.Create;
      Result.Action := TElicitAction.Accept;
      Result.Content := DefaultFormAnswer(ARequest.RequestedSchema);

      Writeln('  [elicitation] answered ', Result.Content.ToJSON);
    end;

  AClient.OnSampling :=
    function(const ARequest: TCreateMessageRequestParams): TCreateMessageResult
    begin
      Writeln;
      Writeln(Format('  [sampling] %d message(s), maxTokens %d',
        [ARequest.Messages.Count, ARequest.MaxTokens]));

      Result := TCreateMessageResult.Create;
      Result.Role := TRole.Assistant;
      Result.Model := 'console-demo';
      Result.StopReason := 'endTurn';
      Result.AddText('This is a canned sampling answer from the console demo.');

      Writeln('  [sampling] answered');
    end;

  AClient.OnRoots :=
    function(const ARequest: TListRootsParams): TListRootsResult
    begin
      Writeln;
      Writeln('  [roots] the server asks for the client workspace');

      Result := TListRootsResult.Create;
      Result.Roots.Add(TRoot.Create);
      Result.Roots[0].Uri := 'file:///' +
        StringReplace(GetCurrentDir, '\', '/', [rfReplaceAll]);
      Result.Roots[0].Name := 'Current directory';

      Writeln('  [roots] answered ', Result.Roots[0].Uri);
    end;
end;

procedure PrintServer(AClient: TMCPClient);
begin
  Writeln;
  Writeln('Server   : ', AClient.ServerInfo.Name, ' ', AClient.ServerInfo.Version);
  if AClient.ServerInfo.Description.HasValue then
    Writeln('About    : ', AClient.ServerInfo.Description.Value);
  Writeln('Protocol : ', AClient.ProtocolVersion);
  if AClient.Instructions <> '' then
    Writeln('Guide    : ', AClient.Instructions);
end;

procedure PrintToolResult(const AName: string; AResult: TCallToolResult);
var
  LContent: TToolContent;
begin
  Writeln;
  Writeln('Call ', AName, ':');
  if AResult.IsError.HasValue and AResult.IsError.Value then
    Writeln('  isError: True');

  for LContent in AResult.Content do
  begin
    if LContent is TTextContent then
      Writeln('  text: ', TTextContent(LContent).Text)
    else if LContent is TImageContent then
      Writeln(Format('  image: %d base64 chars', [Length(TImageContent(LContent).Data)]))
    else if LContent is TAudioContent then
      Writeln(Format('  audio: %d base64 chars', [Length(TAudioContent(LContent).Data)]))
    else if LContent is TResourceLink then
      Writeln('  link: ', TResourceLink(LContent).Uri)
    else
      Writeln('  ', LContent.ClassName);
  end;

  if Assigned(AResult.StructuredContent) then
    Writeln('  structured: ', AResult.StructuredContent.ToJSON);
end;

procedure PrintReadResult(AResult: TReadResourceResult);
var
  LContent: TResourceContents;
begin
  Writeln;
  Writeln('Resource contents:');
  for LContent in AResult.Contents do
  begin
    if LContent is TTextResourceContents then
      Writeln(Format('  [text] %s: %s',
        [TTextResourceContents(LContent).Uri, TTextResourceContents(LContent).Text]))
    else if LContent is TBlobResourceContents then
      Writeln(Format('  [blob] %s: %d base64 chars',
        [TBlobResourceContents(LContent).Uri,
         Length(TBlobResourceContents(LContent).Blob)]))
    else
      Writeln('  [', LContent.ClassName, ']');
  end;
end;

procedure PrintPromptResult(AResult: TGetPromptResult);
var
  LMessage: TPromptMessageBase;
  LRole: string;
begin
  Writeln;
  if AResult.Description.HasValue then
    Writeln('Prompt: ', AResult.Description.Value);

  for LMessage in AResult.Messages do
  begin
    if LMessage.Role = TRole.User then
      LRole := 'user'
    else
      LRole := 'assistant';

    if LMessage is TPromptMessage<TTextContent> then
      Writeln(Format('  %s: %s', [LRole,
        TPromptMessage<TTextContent>(LMessage).Content.Text]))
    else
      Writeln(Format('  %s: <%s>', [LRole, LMessage.ClassName]));
  end;
end;

procedure RunListen(AClient: TMCPClient; ASeconds: Integer);
var
  LFilter: TSubscriptionFilter;
  LSubscription: IMCPSubscription;
  LArguments: TJSONObject;
begin
  Writeln;
  Writeln(Format('Listening for %d second(s) - change something on the server...',
    [ASeconds]));

  LFilter := TSubscriptionFilter.Create;
  try
    LFilter.ToolsListChanged := True;
    LFilter.PromptsListChanged := True;
    LFilter.ResourcesListChanged := True;
    // The demo server publishes this resource whenever the todo list changes.
    LFilter.ResourceSubscriptions := ['text://todo/summary'];

    try
      LSubscription := AClient.Listen(LFilter,
        procedure(const ANotification: TJRPCNotification)
        begin
          Writeln('  notification: ', ANotification.Method);
        end);
    except
      on E: Exception do
      begin
        Writeln('  listen unavailable: ', E.Message);
        Exit;
      end;
    end;

    try
      // Give the server something to publish, so the stream is not silent.
      try
        LArguments := TJSONObject.ParseJSONValue(
          '{"title":"Listen demo","description":"From the console demo"}')
          as TJSONObject;
        try
          AClient.CallTool('add_task', LArguments).Free;
          Writeln('  added a task to make the server publish a change');
        finally
          LArguments.Free;
        end;
      except
        on E: Exception do
          Writeln('  (could not trigger a change: ', E.Message, ')');
      end;

      Sleep(ASeconds * 1000);
    finally
      LSubscription.Close;
      LSubscription.Wait(5000);
      LSubscription := nil;
    end;
  finally
    LFilter.Free;
  end;
end;

var
  LOptions: TDemoOptions;
  LClient: TMCPClient;
  LStreaming: IJRPCStreamingTransport;
  LMessageTransport: IJRPCMessageTransport;
  LStdio: TMCPStdioTransport;
  LTools: TMCPTools;
  LResources: TMCPResources;
  LTemplates: TMCPTemplates;
  LPrompts: TMCPPrompts;
  LTool: TMCPTool;
  LResource: TMCPResource;
  LTemplate: TMCPResourceTemplate;
  LPrompt: TMCPPrompt;
  LArguments: TJSONObject;
  LToolResult: TCallToolResult;
  LReadResult: TReadResourceResult;
  LPromptResult: TGetPromptResult;
  LToolName, LReadUri, LPromptName, LArgsJSON: string;
begin
  ReportMemoryLeaksOnShutdown := True;

  try
    LOptions := ParseOptions;
    if LOptions.ShowHelp then
    begin
      PrintHelp;
      Exit;
    end;

    if LOptions.StdioCommand <> '' then
    begin
      Writeln('Launching: ', LOptions.StdioCommand);
      LStdio := TMCPStdioTransport.Create(LOptions.StdioCommand);
      LMessageTransport := LStdio;
      LClient := TMCPClient.Create(LMessageTransport);
      LClient.CallTimeout := 60000;
    end
    else
    begin
      Writeln('Endpoint : ', LOptions.URL);
      LStreaming := TMCPHTTPTransport.Create(LOptions.URL, LOptions.Token);
      LClient := TMCPClient.Create(LStreaming);

      // A subscription needs a transport of its own, and the stream must not
      // time out while it is open.
      LClient.TransportFactory :=
        function: IJRPCStreamingTransport
        begin
          Result := TMCPHTTPTransport.Create(LOptions.URL, LOptions.Token, 0);
        end;
    end;

    try
      LClient.Connect;
      InstallHandlers(LClient);
      PrintServer(LClient);

      // Tools
      Writeln;
      LTools := LClient.ListTools;
      try
        Writeln(Format('%d tool(s):', [LTools.Count]));
        for LTool in LTools do
        begin
          if LTool.Description.HasValue then
            Writeln('  ', LTool.Name, ' - ', LTool.Description.Value)
          else
            Writeln('  ', LTool.Name);
        end;

        LToolName := LOptions.CallTool;
        if LToolName = '' then
        begin
          if Assigned(FindTool(LTools, 'list_tasks')) then
            LToolName := 'list_tasks'
          else
          begin
            LTool := FirstToolWithoutRequiredArguments(LTools);
            if Assigned(LTool) then
              LToolName := LTool.Name;
          end;
        end;
      finally
        LTools.Free;
      end;

      // Resources and templates
      Writeln;
      LResources := LClient.ListResources;
      LTemplates := LClient.ListResourceTemplates;
      try
        Writeln(Format('%d resource(s):', [LResources.Count]));
        for LResource in LResources do
          if LResource.Name <> '' then
            Writeln('  ', LResource.Uri, ' - ', LResource.Name)
          else
            Writeln('  ', LResource.Uri);

        Writeln(Format('%d resource template(s):', [LTemplates.Count]));
        for LTemplate in LTemplates do
          Writeln('  ', LTemplate.UriTemplate.Value);

        LReadUri := LOptions.ReadUri;
        if (LReadUri = '') and (LResources.Count > 0) then
          LReadUri := LResources[0].Uri;
      finally
        LTemplates.Free;
        LResources.Free;
      end;

      // Prompts
      Writeln;
      LPrompts := LClient.ListPrompts;
      try
        Writeln(Format('%d prompt(s):', [LPrompts.Count]));
        for LPrompt in LPrompts do
          if LPrompt.Description.HasValue then
            Writeln('  ', LPrompt.Name, ' - ', LPrompt.Description.Value)
          else
            Writeln('  ', LPrompt.Name);

        LPromptName := LOptions.PromptName;
        if (LPromptName = '') and (LPrompts.Count > 0) then
          LPromptName := LPrompts[0].Name;
      finally
        LPrompts.Free;
      end;

      if LOptions.ListOnly then
        Exit;

      // Read
      if LReadUri <> '' then
      begin
        LReadResult := LClient.ReadResource(LReadUri);
        try
          Writeln;
          Writeln('Read ', LReadUri);
          PrintReadResult(LReadResult);
        finally
          LReadResult.Free;
        end;
      end;

      // Prompt
      if LPromptName <> '' then
      begin
        LArguments := ArgsFromJSON(LOptions.PromptArguments);
        try
          Writeln;
          Writeln('Get prompt ', LPromptName);
          LPromptResult := LClient.GetPrompt(LPromptName, LArguments);
          try
            PrintPromptResult(LPromptResult);
          finally
            LPromptResult.Free;
          end;
        finally
          LArguments.Free;
        end;
      end;

      // Call
      if LToolName <> '' then
      begin
        LArgsJSON := LOptions.CallArguments;
        if LOptions.CallArgumentsFile <> '' then
          LArgsJSON := TFile.ReadAllText(LOptions.CallArgumentsFile,
            TEncoding.UTF8);

        LArguments := ArgsFromJSON(LArgsJSON);
        try
          Writeln;
          Writeln('Calling ', LToolName, '...');
          LToolResult := LClient.CallTool(LToolName, LArguments);
          try
            PrintToolResult(LToolName, LToolResult);
          finally
            LToolResult.Free;
          end;
        finally
          LArguments.Free;
        end;
      end;

      if LOptions.ListenSeconds > 0 then
        RunListen(LClient, LOptions.ListenSeconds);

      Writeln;
      Writeln('Done.');
    finally
      LClient.Free;
      LMessageTransport := nil;
      LStreaming := nil;
    end;
  except
    on E: Exception do
    begin
      Writeln(ErrOutput, E.ClassName, ': ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
