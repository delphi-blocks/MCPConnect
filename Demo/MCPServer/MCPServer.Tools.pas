unit MCPServer.Tools;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections, System.SyncObjs,
  System.JSON,

  Logify,
  Neon.Core.Persistence,
  Neon.Core.Persistence.JSON,
  Neon.Core.Persistence.JSON.Schema,

  JRPC.Classes,

  MCPConnect.Transport.Base,
  MCPConnect.MCP.Response,
  MCPConnect.MCP.Types.Base,
  MCPConnect.MCP.Types.Tools,
  MCPConnect.MCP.Types.Mrtr,
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
    [Context] FParams: TCallToolRequestParams;
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

const
  STaskNotFound = 'Task with ID %d not found';

  /// <summary>The key delete_task asks its confirmation under.</summary>
  SElicitationDeleteKey = 'delete';

type
  /// <summary>
  ///   What the delete round trip carries in its requestState: which task the
  ///   user was asked about. TMCPRequestState writes it as Neon JSON and reads
  ///   it back on the retry, so the answer cannot be replayed against another
  ///   task.
  /// </summary>
  TDeleteContext = class
  public
    TaskId: Integer;
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

function TTodoTool.CompleteTask(ATaskId: Integer): string;
var
  LTask: TTaskItem;
begin
  TodoStore.Lock();
  try
    LTask := TodoStore.FindById(ATaskId);
    if LTask = nil then
      Exit(Format(STaskNotFound, [ATaskId]));
    LTask.Status := TTaskStatus.Completed;
    Result := Format('Task #%d "%s" marked as completed', [LTask.Id, LTask.Title]);
  finally
    TodoStore.Unlock();
  end;
end;

function TTodoTool.DeleteTask(ATaskId: Integer): TMCPResponse<string>;
var
  LTask: TTaskItem;
  LTitle, LResult: string;
  LContext: TDeleteContext;
begin
  // Deleting is destructive, so the first call asks rather than deletes: the
  // context travels as the requestState, and the client retries with the
  // user's answer under the key this server chose for it
  if FParams.InputResponses.Outcome(SElicitationDeleteKey) = TElicitationOutcome.Absent then
  begin
    LContext := TDeleteContext.Create;
    try
      LContext.TaskId := ATaskId;
      Exit(TMCPResponse<string>.Needs(
        TMCPInput.New(TMCPRequestState.Encode(LContext))
          .Confirm(SElicitationDeleteKey, Format('Delete task #%d?', [ATaskId]))));
    finally
      LContext.Free;
    end;
  end;

  Logger.Log('User response for a previous Input Request', TLogLevel.Debug);

  // An answer given to another question says nothing about this one: the state
  // is decoded back into the context rather than compared as text. Encode and
  // decode it with a secret when the context can influence authorization, so
  // a client cannot edit it.
  if not FParams.TryStateAs<TDeleteContext>(LContext) or (LContext.TaskId <> ATaskId) then
  begin
    LContext.Free;
    Exit(TMCPResponse<string>.Ready(
      TCallToolReply.Fail('This confirmation belongs to another request')));
  end;
  LContext.Free;

  if FParams.InputResponses.Outcome(SElicitationDeleteKey) <> TElicitationOutcome.Accepted then
    Exit(TMCPResponse<string>.Value(Format('Task #%d was not deleted', [ATaskId])));

  TodoStore.Lock();
  try
    LTask := TodoStore.FindById(ATaskId);
    if LTask = nil then
      raise Exception.CreateFmt(STaskNotFound, [ATaskId]);
    LTitle := LTask.Title;
  finally
    TodoStore.Unlock();
  end;

  if TodoStore.Remove(ATaskId) then
    LResult := Format('Task #%d "%s" deleted', [ATaskId, LTitle])
  else
    LResult := Format(STaskNotFound, [ATaskId]);

  Result := TMCPResponse<string>.Value(LResult);
end;

initialization
  TodoStore := TTodoStore.Create();

finalization
  TodoStore.Free();

end.
