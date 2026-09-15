unit MCPServer.Tools.Mrtr;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections, System.SyncObjs,
  System.JSON,

  Logify,
  Neon.Core.Attributes,
  Neon.Core.Persistence,
  Neon.Core.Persistence.JSON,
  Neon.Core.Persistence.JSON.Schema,

  JRPC.Classes,

  MCPConnect.Transport.Base,
  MCPConnect.MCP.Response,
  MCPConnect.MCP.Types.Base,
  MCPConnect.MCP.Types.Tools,
  MCPConnect.MCP.Types.Mrtr,
  MCPConnect.MCP.Types.Errors,
  MCPConnect.MCP.Attributes;

type
  TTaskStatus = (Pending, Completed);

  TTaskItem = class
  private
    FId: Integer;
    FTitle: string;
    FDescription: string;
    FStatus: TTaskStatus;
    FCreatedAt: TDateTime;
  public
    property Id: Integer read FId write FId;
    property Title: string read FTitle write FTitle;
    property Description: string read FDescription write FDescription;
    property Status: TTaskStatus read FStatus write FStatus;
    property CreatedAt: TDateTime read FCreatedAt write FCreatedAt;

    constructor Create(AId: Integer; const ATitle, ADescription: string);
  end;

  TTodoStore = class
  private
    FLock: TCriticalSection;
    FTasks: TObjectList<TTaskItem>;
    FNextId: Integer;
  public
    constructor Create();
    destructor Destroy(); override;

    function Add(const ATitle, ADescription: string): TTaskItem;
    function FindById(AId: Integer): TTaskItem;
    function Remove(AId: Integer): Boolean;
    function ToText(): string;
    function GetSummary(): string;
    function CountTasks(): Integer;

    procedure Lock();
    procedure Unlock();
  end;

  TTodoMrtrTools = class
  private
    class var FDontAskAgain: Boolean;
  private
    [Context] FParams: TCallToolRequestParams;
    [Context([TContextOption.Optional])] FCapabilities: TMCPDeclaredCapabilities;
    function DoDeleteTask(ATaskId: Integer): string;

    function SupportElicitationForm: Boolean;

    /// <summary>
    ///   The sampling request summarize_tasks and draft_day_plan send: the messages,
    ///   the budget and the model preferences, built in one place so the two
    ///   tools differ only in what they ask the model for.
    /// </summary>
    function BuildSamplingRequest(const ASystemPrompt, APrompt: string;
      AMaxTokens: Integer; AIntelligence: Double): TCreateMessageRequestParams;

    /// <summary>
    ///   Round two of import_tasks: the client has listed its roots, so the
    ///   files that can be imported are known and the user can be asked which.
    /// </summary>
    function AskImportFile: TMCPResponse<string>;

    /// <summary>
    ///   Round three of import_tasks: the user named a file inside AFolder,
    ///   which is the folder the *signed* state says was offered.
    /// </summary>
    function ReadImportFile(const AFolder: string): TMCPResponse<string>;
  public
    [McpTool('add_task', 'Add a new task to the todo list')]
    function AddTask(
      [McpParam('title', 'Title of the task')] const ATitle: string;
      [McpParam('description', 'Optional description of the task')] const ADescription: string = ''
    ): string;

    [McpTool('list_tasks', 'List all tasks in the todo list')]
    function ListTasks(): string;

    [McpTool('complete_task', 'Mark a task as completed')]
    function CompleteTask(
      [McpParam('task_id', 'ID of the task to complete')] ATaskId: Integer
    ): string;

    /// <summary>
    ///   Asks the user before it deletes anything: the first call answers with
    ///   an elicitation (MRTR), the retry carries the answer and does the work.
    /// </summary>
    [McpTool('delete_task', 'Delete a task from the todo list', 'destructive')]
    function DeleteTask(
      [McpParam('task_id', 'ID of the task to delete')] ATaskId: Integer
    ): TMCPResponse<string>;

    /// <summary>
    ///   Sampling (MRTR): the server holds the list but no model, so it asks
    ///   the client for one. Nothing here is asked of the *user* - the round
    ///   trip goes to the client's LLM and comes back as a TCreateMessageResult.
    /// </summary>
    [McpTool('summarize_tasks', 'Summarize the todo list, using the model the client offers', 'readonly')]
    function SummarizeTasks(
      [McpParam('style', 'How the summary should read')] const AStyle: string = 'three short bullet points'
    ): TMCPResponse<string>;

    /// <summary>
    ///   Roots then elicitation (MRTR), three rounds: a server may not guess at
    ///   a client's filesystem, so it asks for the roots, offers what it found
    ///   there, and imports the file the user picked. The step it is on travels
    ///   in a *signed* requestState, because that state names the folder that
    ///   will be read.
    /// </summary>
    [McpTool('import_tasks', 'Import tasks from a text file in one of the roots the client offers')]
    function ImportTasks(): TMCPResponse<string>;

    /// <summary>
    ///   Roots and sampling in a single interim result: the two requests are
    ///   independent, so there is no reason to spend two round trips on them.
    ///   Both answers come back in one map, under the keys chosen here.
    /// </summary>
    [McpTool('draft_day_plan', 'Draft a plan for today from the pending tasks and the client workspace', 'readonly')]
    function DraftDayPlan(): TMCPResponse<string>;

  end;

var
  TodoStore: TTodoStore;

implementation

uses
  System.Diagnostics,
  System.IOUtils,
  System.StrUtils,
  System.NetEncoding;

type
  /// <summary>
  ///   What the delete round trip carries in its requestState: which task the
  ///   user was asked about. TMCPRequestState writes it as Neon JSON and reads
  ///   it back on the retry, so the answer cannot be replayed against another
  ///   task. A record, so the round trip owns nothing.
  /// </summary>
  TDeleteContext = record
    TaskId: Integer;
  end;

  [NeonEnumNames('Undefined,Duplicate,No longer needed,Created by mistake,Other')]
  TDeleteReason = (
    Undefined,
    Duplicate,
    NoLongerNeeded,
    CreatedByMistake,
    Other
  );

  /// <summary>
  ///   What delete_task asks the user for. The form the client renders is
  ///   generated from this record's RTTI, and the answer reads straight back
  ///   into it - so the [JsonSchema] tags here are what the user sees, and
  ///   nothing spells the member names twice.
  /// </summary>
  TDeleteAsk = record
    [JsonSchema('title=Reason,description=Reason to delete the task')]
    Reason: TDeleteReason;

    [JsonSchema('title=Other reason,description=Custom reason, used when Reason is Other')]
    OtherReason: string;

    [JsonSchema('title=Don''t ask again,description=Skip the confirmation for future delete requests')]
    DontAskAgain: Boolean;

    function FullReason: string;
  end;

const
  /// <summary>
  ///   The key import_tasks signs its requestState with. A real server keeps a
  ///   secret out of its source and rotates it; what matters for the demo is
  ///   that the state names the folder the next round will read, so a client
  ///   editing it would be choosing what this server opens.
  /// </summary>
  IMPORT_SECRET = 'demo-import-state-secret';

  // The keys each round trip files its requests under. They are the server's to
  // choose, and the client answers under the same ones.
  SUMMARY_KEY = 'summary';
  IMPORT_ROOTS_KEY = 'where';
  IMPORT_FILE_KEY = 'file';
  IMPORT_FILE_PROPERTY = 'fileName';
  PLAN_ROOTS_KEY = 'where';
  PLAN_DRAFT_KEY = 'draft';

  // What import_tasks is willing to read out of a root
  IMPORT_EXTENSIONS: array [0 .. 1] of string = ('.md', '.txt');

type
  /// <summary>
  ///   What the summarize round trip carries in its requestState: how long the
  ///   list was when the model was asked. The retry compares, so a summary of a
  ///   list that has moved on can say so instead of pretending to be current.
  /// </summary>
  TSummaryContext = record
    TaskCount: Integer;
  end;

  /// <summary>
  ///   Which round of import_tasks the client is answering.
  /// </summary>
  TImportStep = (AskRoots, AskFile);

  /// <summary>
  ///   What the import round trips carry: the step, and - from round two on -
  ///   the folder the offered files came from. The folder is in the state and
  ///   not in the arguments precisely because it decides what gets opened: it
  ///   is signed, and a state that does not verify ends the call.
  /// </summary>
  TImportContext = record
    Step: TImportStep;
    Root: string;
  end;

/// <summary>
///   The local folder a root URI names, or an empty string when it names none
///   this server can use. Only file:// is admitted - the specification allows
///   nothing else yet - and a root naming a file is read as its folder.
/// </summary>
function RootToFolder(const AUri: string): string;
var
  LPath: string;
begin
  Result := '';
  if not AUri.StartsWith('file://', True) then
    Exit;

  LPath := TNetEncoding.URL.Decode(AUri.Substring(Length('file://')));
  LPath := LPath.Replace('/', PathDelim);

  // file:///C:/work becomes \C:\work, which is not a path anyone can open
  if (Length(LPath) > 2) and (LPath[1] = PathDelim) and (LPath[3] = ':') then
    LPath := LPath.Substring(1);

  if TDirectory.Exists(LPath) then
    Result := LPath
  else if TFile.Exists(LPath) then
    Result := TPath.GetDirectoryName(LPath);
end;

/// <summary>
///   The names of the files in AFolder this server would import, sorted.
/// </summary>
function ImportCandidates(const AFolder: string): TArray<string>;
var
  LFile: string;
  LNames: TStringList;
begin
  LNames := TStringList.Create;
  try
    LNames.Sorted := True;
    for LFile in TDirectory.GetFiles(AFolder) do
    begin
      if MatchText(TPath.GetExtension(LFile), IMPORT_EXTENSIONS) then
        LNames.Add(TPath.GetFileName(LFile));
    end;
    Result := LNames.ToStringArray;
  finally
    LNames.Free;
  end;
end;

/// <summary>
///   ALine as a task title: the markdown list and checkbox markers a todo file
///   is usually written with are not part of the task.
/// </summary>
function LineToTitle(const ALine: string): string;
begin
  Result := ALine.Trim;

  if Result.StartsWith('- ') or Result.StartsWith('* ') then
    Result := Result.Substring(2).Trim;

  if Result.StartsWith('[ ]') or Result.StartsWith('[x]', True) then
    Result := Result.Substring(3).Trim;
end;

{ TTaskItem }

constructor TTaskItem.Create(AId: Integer; const ATitle, ADescription: string);
begin
  inherited Create();
  FId := AId;
  FTitle := ATitle;
  FDescription := ADescription;
  FStatus := TTaskStatus.Pending;
  FCreatedAt := Now();
end;

{ TTodoStore }

constructor TTodoStore.Create();
begin
  inherited Create();
  FLock := TCriticalSection.Create();
  FTasks := TObjectList<TTaskItem>.Create(True);
  FNextId := 1;
end;

destructor TTodoStore.Destroy();
begin
  FTasks.Free();
  FLock.Free();
  inherited;
end;

procedure TTodoStore.Lock();
begin
  FLock.Enter();
end;

procedure TTodoStore.Unlock();
begin
  FLock.Leave();
end;

function TTodoStore.Add(const ATitle, ADescription: string): TTaskItem;
begin
  FLock.Enter();
  try
    Result := TTaskItem.Create(FNextId, ATitle, ADescription);
    Inc(FNextId);
    FTasks.Add(Result);
  finally
    FLock.Leave();
  end;
end;

function TTodoStore.FindById(AId: Integer): TTaskItem;
var
  LTask: TTaskItem;
begin
  Result := nil;
  for LTask in FTasks do
  begin
    if LTask.Id = AId then
      Exit(LTask);
  end;
end;

function TTodoStore.Remove(AId: Integer): Boolean;
var
  I: Integer;
begin
  Result := False;
  FLock.Enter();
  try
    for I := 0 to FTasks.Count - 1 do
    begin
      if FTasks[I].Id = AId then
      begin
        FTasks.Delete(I);
        Exit(True);
      end;
    end;
  finally
    FLock.Leave();
  end;
end;

function TTodoStore.ToText(): string;
var
  LTask: TTaskItem;
  LList: TStringList;
  LStatus: string;
begin
  FLock.Enter();
  try
    if FTasks.Count = 0 then
      Exit('No tasks in the list');

    LList := TStringList.Create();
    try
      LList.Add(Format('Todo List (%d tasks):', [FTasks.Count]));
      LList.Add('');
      for LTask in FTasks do
      begin
        if LTask.Status = TTaskStatus.Completed then
          LStatus := 'DONE'
        else
          LStatus := 'PENDING';
        LList.Add(Format('  #%d [%s] %s', [LTask.Id, LStatus, LTask.Title]));
        if not LTask.Description.IsEmpty() then
          LList.Add(Format('      %s', [LTask.Description]));
      end;
      Result := LList.Text;
    finally
      LList.Free();
    end;
  finally
    FLock.Leave();
  end;
end;

function TTodoStore.GetSummary(): string;
var
  LTask: TTaskItem;
  LPending: Integer;
  LCompleted: Integer;
  LList: TStringList;
begin
  FLock.Enter();
  try
    LPending := 0;
    LCompleted := 0;
    for LTask in FTasks do
    begin
      if LTask.Status = TTaskStatus.Completed then
        Inc(LCompleted)
      else
        Inc(LPending);
    end;

    LList := TStringList.Create();
    try
      LList.Add('=== Todo List Summary ===');
      LList.Add(Format('Total tasks: %d', [FTasks.Count]));
      LList.Add(Format('Pending: %d', [LPending]));
      LList.Add(Format('Completed: %d', [LCompleted]));

      if LPending > 0 then
      begin
        LList.Add('');
        LList.Add('--- Pending tasks ---');
        for LTask in FTasks do
        begin
          if LTask.Status = TTaskStatus.Pending then
            LList.Add(Format('  #%d %s', [LTask.Id, LTask.Title]));
        end;
      end;
      Result := LList.Text;
    finally
      LList.Free();
    end;
  finally
    FLock.Leave();
  end;
end;

function TTodoStore.CountTasks(): Integer;
begin
  FLock.Enter();
  try
    Result := FTasks.Count;
  finally
    FLock.Leave();
  end;
end;

{ TTodoMrtrTools }

function TTodoMrtrTools.AddTask(const ATitle: string; const ADescription: string): string;
var
  LTask: TTaskItem;
  LWatch: TStopwatch;
begin
  LWatch := TStopwatch.StartNew;
  try
    LTask := TodoStore.Add(ATitle, ADescription);
  finally
    LWatch.Stop;
  end;

  Result := Format('Task #%d "%s" added successfully', [LTask.Id, LTask.Title]);
end;

function TTodoMrtrTools.ListTasks(): string;
begin
  Result := TodoStore.ToText();
end;

function TTodoMrtrTools.SupportElicitationForm: Boolean;
begin
  Result := Assigned(FCapabilities) and (TMCPClientCapability.ElicitationForm in FCapabilities.Declared);
end;

function TTodoMrtrTools.CompleteTask(ATaskId: Integer): string;
var
  LTask: TTaskItem;
begin
  TodoStore.Lock();
  try
    LTask := TodoStore.FindById(ATaskId);
    if LTask = nil then
      Exit(Format('Task with ID %d not found', [ATaskId]));
    LTask.Status := TTaskStatus.Completed;
    Result := Format('Task #%d "%s" marked as completed', [LTask.Id, LTask.Title]);
  finally
    TodoStore.Unlock();
  end;
end;

function TTodoMrtrTools.DeleteTask(ATaskId: Integer): TMCPResponse<string>;
const
  // Elicitation key (each response is under a different key)
  DelKey = 'delete';
begin
  // The user ticked "do not ask again" on an earlier call, so skip the form
  if FDontAskAgain or not SupportElicitationForm then
    Exit(TMCPResponse<string>.Value(DoDeleteTask(ATaskId)));

  // The round-trip context: filled in below when asking, decoded back
  // out of the requestState when the answer returns
  var LContext := Default(TDeleteContext);

  // The client answered already? Absent on the first call, accepted /
  // declined / cancelled on the retry that carries the form back
  var LOutcome := FParams.InputResponses.Outcome(DelKey);

  // Deleting is destructive, so the first call asks rather than deletes: the
  // form is TDeleteAsk, the context travels as the requestState, and the client
  // retries with the user's answer under the key this server chose for it
  if LOutcome = TElicitationOutcome.Absent then
  begin
    // What this round trip is about: the task the answer will have to match
    LContext.TaskId := ATaskId;

    // Neon-serialized and Base64-encoded into the opaque token the client echoes back
    var LRequestState := TMCPRequestState.EncodeStruct<TDeleteContext>(LContext);

    // The builder of the interim result, carrying that state along
    var LInput := TMCPInput.New(LRequestState);

    // One form, its schema generated from the TDeleteAsk record, filed under DelKey
    LInput.Ask<TDeleteAsk>(DelKey, Format('Delete task #%d?', [ATaskId]));

    // Answer with the input-required result instead of a value: nothing is deleted yet
    Exit(TMCPResponse<string>.Needs(LInput));
  end;

  Logger.Log('User response for a previous InputRequest', TLogLevel.Debug);

  // An answer given to another question says nothing about this one: the state
  // is decoded back into the context rather than compared as text. Encode and
  // decode it with a secret when the context can influence authorization, so
  // a client cannot edit it.
  if not FParams.TryStateAsStruct<TDeleteContext>(LContext) or (LContext.TaskId <> ATaskId) then
    Exit(TMCPResponse<string>.Ready(
      TCallToolReply.Fail('This confirmation belongs to another request')));

  // A decline or a cancel: the user refused, so nothing is deleted
  if LOutcome <> TElicitationOutcome.Accepted then
    Exit(TMCPResponse<string>.Value('Operation aborted by the user!'));

  // The accepted answer, read back into the very record whose RTTI shaped the form
  var LAnswer := FParams.InputResponses.StructAs<TDeleteAsk>(DelKey);

  // The checkbox on that form: a class var, since the tool is built per call, so
  // later deletions skip the question for as long as the server runs
  FDontAskAgain := LAnswer.DontAskAgain;

  Logger.Log(Format('Task #%d deleted because: %s', [ATaskId, LAnswer.FullReason]), TLogLevel.Info);

  // Accepted, and about this very task: the deletion finally happens, answered
  // with the plain string this tool normally returns
  Result := TMCPResponse<string>.Value(DoDeleteTask(ATaskId));
end;

function TTodoMrtrTools.DoDeleteTask(ATaskId: Integer): string;
begin
  if TodoStore.Remove(ATaskId) then
    Result := Format('Task #%d deleted', [ATaskId])
  else
    Result := Format('Task with ID %d not found', [ATaskId]);
end;

function TTodoMrtrTools.BuildSamplingRequest(const ASystemPrompt, APrompt: string;
  AMaxTokens: Integer; AIntelligence: Double): TCreateMessageRequestParams;
var
  LHint: TModelHint;
  LMessage: TSamplingMessage;
  LTemperature: Currency;
begin
  Result := TCreateMessageRequestParams.Create;
  try
    // What the model is for, said once and outside the conversation
    Result.SystemPrompt := ASystemPrompt;

    // A budget the client can hold the model to: they are its tokens, not ours
    Result.MaxTokens := AMaxTokens;

    LTemperature := 0.2;
    Result.Temperature := LTemperature;

    // Everything this server needs the model to see travels in the message
    // below. Asking for the client's own conversation instead would need the
    // "context" sub-capability, which the capability gate checks separately
    Result.IncludeContext := TIncludeContext.None;

    // All of this is advisory - the client MAY ignore every line. The hint is
    // a substring of a model name and is evaluated first; the three priorities
    // are what break the ties
    LHint := TModelHint.Create;
    Result.ModelPreferences.Hints.Add(LHint);
    LHint.Name := 'claude';
    Result.ModelPreferences.SpeedPriority := 0.7;
    Result.ModelPreferences.CostPriority := 0.5;
    Result.ModelPreferences.IntelligencePriority := AIntelligence;

    LMessage := TSamplingMessage.Create;
    Result.Messages.Add(LMessage);
    LMessage.Role := TRole.User;
    LMessage.AddText(APrompt);
  except
    Result.Free;
    raise;
  end;
end;

function TTodoMrtrTools.SummarizeTasks(const AStyle: string): TMCPResponse<string>;
var
  LContext: TSummaryContext;
begin
  // The client's answer to the request an earlier call sent, or nil when there
  // was no earlier call: a sampling request has no "declined" to read back -
  // the message is either there or it is not
  var LReply := FParams.InputResponses.SamplingFor(SUMMARY_KEY);

  if LReply = nil then
  begin
    LContext.TaskCount := TodoStore.CountTasks;

    // Nothing to summarize, and a round trip would cost the client a model call
    if LContext.TaskCount = 0 then
      Exit(TMCPResponse<string>.Value('There is nothing to summarize: the list is empty'));

    // How long the list was when the model was asked, so the retry can notice
    // that it has moved on. Unsigned: it says nothing that decides access
    var LState := TMCPRequestState.EncodeStruct<TSummaryContext>(LContext);

    var LPrompt := Format('Summarize this todo list as %s.'#13#10#13#10'%s',
      [AStyle, TodoStore.ToText]);

    Exit(TMCPResponse<string>.Needs(
      TMCPInput.New(LState).Sample(SUMMARY_KEY,
        BuildSamplingRequest(
          'You summarize todo lists. Answer with the summary and nothing else.',
          LPrompt, 400, 0.3))));
  end;

  Logger.Log('The client sampled a model on our behalf', TLogLevel.Debug);

  // A sampling message carries a union of content blocks - text, image, audio,
  // tool use - and a summary that came back as anything else is not one
  var LText := '';
  if (LReply.ContentCount > 0) and (LReply.ContentTypeAt(0) = MCP_CONTENT_TEXT) then
    LText := LReply.AsText(0).Text;

  if LText.Trim.IsEmpty then
    Exit(TMCPResponse<string>.Ready(TCallToolReply.Fail(
      'The client answered the sampling request with no text')));

  var LAnswer := TStringList.Create;
  try
    LAnswer.Add(LText.Trim);
    LAnswer.Add('');

    // Which model answered, and why it stopped, are the client's to report -
    // this server chose neither
    LAnswer.Add(Format('-- summarized by %s (stop reason: %s)',
      [LReply.Model, LReply.StopReason.GetValueOrDefault('unknown')]));

    // The list is shared and a round trip lasts as long as a model does, so the
    // summary may already describe a list that has moved on
    if FParams.TryStateAsStruct<TSummaryContext>(LContext) and
       (LContext.TaskCount <> TodoStore.CountTasks) then
      LAnswer.Add(Format('-- note: %d tasks when the model was asked, %d now',
        [LContext.TaskCount, TodoStore.CountTasks]));

    Result := TMCPResponse<string>.Value(LAnswer.Text);
  finally
    LAnswer.Free;
  end;
end;

function TTodoMrtrTools.ImportTasks(): TMCPResponse<string>;
var
  LContext: TImportContext;
begin
  // Round one: nothing has been asked yet, so ask the client where this server
  // may read. A server does not go looking through a client's disk on its own
  if FParams.RequestState.GetValueOrDefault.IsEmpty then
  begin
    LContext.Step := TImportStep.AskRoots;
    LContext.Root := '';

    Exit(TMCPResponse<string>.Needs(
      TMCPInput.New(TMCPRequestState.EncodeStruct<TImportContext>(LContext, IMPORT_SECRET))
        .Roots(IMPORT_ROOTS_KEY)));
  end;

  // Signed, so a state that was edited on the way - or minted somewhere else -
  // fails here instead of pointing the next round at a folder of the client's
  // choosing. This is the case delete_task's comment recommends signing for
  if not FParams.TryStateAsStruct<TImportContext>(IMPORT_SECRET, LContext) then
    Exit(TMCPResponse<string>.Ready(TCallToolReply.Fail(
      'This continuation was not issued by this server')));

  // Which round this is, is the state's to say: the answers to a round are gone
  // by the next one, and nothing else on the server remembers
  case LContext.Step of
    TImportStep.AskRoots: Result := AskImportFile;
    TImportStep.AskFile:  Result := ReadImportFile(LContext.Root);
  else
    Result := TMCPResponse<string>.Ready(TCallToolReply.Fail('Unknown import step'));
  end;
end;

function TTodoMrtrTools.AskImportFile: TMCPResponse<string>;
var
  LContext: TImportContext;
  LFolder: string;
  LRoot: TRoot;
begin
  // The answer to round one: every root the client is willing to name
  var LRoots := FParams.InputResponses.RootsFor(IMPORT_ROOTS_KEY);
  if (LRoots = nil) or (LRoots.Roots.Count = 0) then
    Exit(TMCPResponse<string>.Ready(TCallToolReply.Fail(
      'The client listed no roots, so there is nowhere this server may read from')));

  // The first root that resolves to a folder on this machine: a client is free
  // to name roots that live on another host entirely
  LFolder := '';
  for LRoot in LRoots.Roots do
  begin
    LFolder := RootToFolder(LRoot.Uri);
    if not LFolder.IsEmpty then
      Break;
  end;

  if LFolder.IsEmpty then
    Exit(TMCPResponse<string>.Ready(TCallToolReply.Fail(Format(
      'None of the %d roots the client listed names a folder on this machine',
      [LRoots.Roots.Count]))));

  var LFiles := ImportCandidates(LFolder);
  if Length(LFiles) = 0 then
    Exit(TMCPResponse<string>.Ready(TCallToolReply.Fail(Format(
      'No .md or .txt file to import in %s', [LFolder]))));

  // Round two asks the user, and the folder travels signed with it: the answer
  // will be resolved against that folder and against nothing else
  LContext.Step := TImportStep.AskFile;
  LContext.Root := LFolder;
  var LState := TMCPRequestState.EncodeStruct<TImportContext>(LContext, IMPORT_SECRET);

  // The options are only known now, so this form is built at run time rather
  // than generated from a Delphi type the way delete_task's is
  Result := TMCPResponse<string>.Needs(
    TMCPInput.New(LState).AskText(IMPORT_FILE_KEY,
      Format('Import tasks from which file in %s? (%s)',
        [LFolder, string.Join(', ', LFiles)]),
      IMPORT_FILE_PROPERTY, 'File name'));
end;

function TTodoMrtrTools.ReadImportFile(const AFolder: string): TMCPResponse<string>;
var
  LLine: string;
  LTitle: string;
  LCount: Integer;
begin
  // Declined or cancelled: the user was asked and said no
  if not FParams.InputResponses.Accepted(IMPORT_FILE_KEY) then
    Exit(TMCPResponse<string>.Value('Import abandoned by the user'));

  var LName := FParams.InputResponses.FieldAsString(IMPORT_FILE_KEY, IMPORT_FILE_PROPERTY).Trim;

  // The question asked for a file name, not for a path: an answer of
  // "..\..\secrets.txt" answers something this server never asked
  if LName.IsEmpty or (TPath.GetFileName(LName) <> LName) or
     (not MatchText(TPath.GetExtension(LName), IMPORT_EXTENSIONS)) then
    Exit(TMCPResponse<string>.Ready(TCallToolReply.Fail(Format(
      '"%s" is not a file name this server offered', [LName]))));

  var LPath := TPath.Combine(AFolder, LName);
  if not TFile.Exists(LPath) then
    Exit(TMCPResponse<string>.Ready(TCallToolReply.Fail(Format(
      'There is no "%s" in %s', [LName, AFolder]))));

  LCount := 0;
  var LLines := TStringList.Create;
  try
    LLines.LoadFromFile(LPath, TEncoding.UTF8);
    for LLine in LLines do
    begin
      LTitle := LineToTitle(LLine);

      // Blank lines and markdown headings are not tasks
      if LTitle.IsEmpty or LTitle.StartsWith('#') then
        Continue;

      TodoStore.Add(LTitle, Format('Imported from %s', [LName]));
      Inc(LCount);
    end;
  finally
    LLines.Free;
  end;

  Logger.Log(Format('Imported %d task(s) from %s', [LCount, LPath]), TLogLevel.Info);

  Result := TMCPResponse<string>.Value(
    Format('Imported %d task(s) from %s', [LCount, LPath]));
end;

function TTodoMrtrTools.DraftDayPlan(): TMCPResponse<string>;
var
  LRoot: TRoot;
begin
  // Both answers come back in the one map, under the keys this tool chose when
  // it asked - which is what the keys are for
  var LRoots := FParams.InputResponses.RootsFor(PLAN_ROOTS_KEY);
  var LDraft := FParams.InputResponses.SamplingFor(PLAN_DRAFT_KEY);

  if (LRoots = nil) and (LDraft = nil) then
  begin
    if TodoStore.CountTasks = 0 then
      Exit(TMCPResponse<string>.Value('There is nothing to plan: the list is empty'));

    var LPrompt := Format(
      'Plan a working day around these tasks: order them and say why.'#13#10#13#10'%s',
      [TodoStore.ToText]);

    // Two requests in one interim result. They do not depend on each other, so
    // there is no reason to spend two round trips on them - and no requestState
    // either: nothing has to be remembered between the rounds, and the
    // arguments come back with the retry anyway.
    //
    // A client that declared only elicitation never sees this result: the
    // capability gate reads what an interim result asks for before it goes out,
    // and a missing capability is -32021 MissingRequiredClientCapability (400)
    Exit(TMCPResponse<string>.Needs(
      TMCPInput.New
        .Roots(PLAN_ROOTS_KEY)
        .Sample(PLAN_DRAFT_KEY,
          BuildSamplingRequest(
            'You plan a working day. Answer with the plan and nothing else.',
            LPrompt, 600, 0.9))));
  end;

  var LAnswer := TStringList.Create;
  try
    LAnswer.Add('=== Plan for today ===');
    LAnswer.Add('');

    // A client may fulfil one request and not the other: each key is answered
    // on its own terms, and an unanswered one is simply absent from the map
    if (LDraft <> nil) and (LDraft.ContentCount > 0) and
       (LDraft.ContentTypeAt(0) = MCP_CONTENT_TEXT) then
    begin
      LAnswer.Add(LDraft.AsText(0).Text.Trim);
      LAnswer.Add('');
      LAnswer.Add(Format('-- drafted by %s', [LDraft.Model]));
    end
    else
      LAnswer.Add('(the client fulfilled no sampling request, so there is no draft)');

    LAnswer.Add('');
    LAnswer.Add('Workspace the plan is about:');

    if (LRoots <> nil) and (LRoots.Roots.Count > 0) then
    begin
      for LRoot in LRoots.Roots do
        LAnswer.Add(Format('  %s -> %s',
          [LRoot.Name.GetValueOrDefault('(unnamed)'), LRoot.Uri]));
    end
    else
      LAnswer.Add('  (the client listed no roots)');

    Result := TMCPResponse<string>.Value(LAnswer.Text);
  finally
    LAnswer.Free;
  end;
end;

{ TDeleteAsk }

function TDeleteAsk.FullReason: string;
begin
  case Reason of
    TDeleteReason.Undefined: Result := 'undefined';
    TDeleteReason.Duplicate: Result := 'duplicate';
    TDeleteReason.NoLongerNeeded: Result := 'no longer needed';
    TDeleteReason.CreatedByMistake: Result := 'created by mistake';
    TDeleteReason.Other: Result := '';
  else
    Result := '';
  end;

  if not OtherReason.IsEmpty then
    Result := Result + ': ' + OtherReason;
end;

initialization
  TodoStore := TTodoStore.Create();

finalization
  TodoStore.Free();

end.
