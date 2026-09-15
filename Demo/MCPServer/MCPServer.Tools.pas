unit MCPServer.Tools;

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

  TTodoTool = class
  private
    class var FDontAskAgain: Boolean;
  private
    [Context] FParams: TCallToolRequestParams;
    [Context([TContextOption.Optional])] FCapabilities: TMCPDeclaredCapabilities;
    function DoDeleteTask(ATaskId: Integer): string;
    function SupportElicitationForm: Boolean;
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

  end;

var
  TodoStore: TTodoStore;

implementation

uses
  System.Diagnostics;

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

{ TTodoTool }

function TTodoTool.AddTask(const ATitle: string; const ADescription: string): string;
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

function TTodoTool.ListTasks(): string;
begin
  Result := TodoStore.ToText();
end;

function TTodoTool.SupportElicitationForm: Boolean;
begin
  Result := Assigned(FCapabilities) and (TMCPClientCapability.ElicitationForm in FCapabilities.Declared);
end;

function TTodoTool.CompleteTask(ATaskId: Integer): string;
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

function TTodoTool.DeleteTask(ATaskId: Integer): TMCPResponse<string>;
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

function TTodoTool.DoDeleteTask(ATaskId: Integer): string;
begin
  if TodoStore.Remove(ATaskId) then
    Result := Format('Task #%d deleted', [ATaskId])
  else
    Result := Format('Task with ID %d not found', [ATaskId]);
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
