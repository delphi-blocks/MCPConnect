unit MCPServer.Tools;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections, System.SyncObjs,

  Neon.Core.Persistence,

  JRPC.Classes,
  MCPConnect.Configuration.MCP,
  MCPConnect.MCP.Types.Base,
  MCPConnect.MCP.Types.Notifications,
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

    procedure Lock();
    procedure Unlock();
  end;

  TTodoTools = class
  private
    [Context] FCancel: TMCPCancellation;
    [Context] FProgress: TMCPProgress;
    [Context] FConfig: TMCPConfig;

    /// <summary>
    ///   Tells every client listening on "subscriptions/listen" for the summary
    ///   resource that it changed. Free when nobody listens.
    /// </summary>
    procedure SummaryChanged;
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

    [McpTool('delete_task', 'Delete a task from the todo list')]
    function DeleteTask(
      [McpParam('task_id', 'ID of the task to delete')] ATaskId: Integer
    ): string;

    [McpTool('summarize_tasks', 'Summarize the todo list, using the model the client offers', 'readonly')]
    function SummarizeTasks(): string;

    [McpTool('import_tasks', 'Import tasks from a text file in one of the roots the client offers')]
    function ImportTasks(
      [McpParam('task_list', 'List of task to import (text, each task on a line)')] const AText: string
    ): string;

    [McpTool('draft_day_plan', 'Draft a plan for today from the pending tasks and the client workspace', 'readonly')]
    function DraftDayPlan(): string;

    /// <summary>
    ///   The cancellation showcase: counts one a second, reporting progress, and
    ///   stops the moment the client cancels - closing the stream over HTTP, or
    ///   sending notifications/cancelled where the transport routes one.
    /// </summary>
    [McpTool('slow_count', 'Count up to a number, one a second; cancel the request to stop early', 'readonly')]
    function SlowCount(
      [McpParam('count_to', 'Where to stop counting, from 1 to 60')] ACountTo: Integer
    ): string;
  end;

var
  TodoStore: TTodoStore;

implementation

const
  STaskNotFound = 'Task with ID %d not found';

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

{ TTodoTools }

procedure TTodoTools.SummaryChanged;
begin
  // The uri of the resource in MCPServer.Resources. A client that subscribed to
  // it re-reads it; one that did not is never told
  FConfig.Subscriptions.ResourceUpdated('text://todo/summary');
end;

function TTodoTools.AddTask(const ATitle: string; const ADescription: string): string;
var
  LTask: TTaskItem;
begin
  LTask := TodoStore.Add(ATitle, ADescription);
  Result := Format('Task #%d "%s" added successfully', [LTask.Id, LTask.Title]);
  SummaryChanged;
end;

function TTodoTools.ListTasks(): string;
begin
  Result := TodoStore.ToText();
end;

function TTodoTools.SummarizeTasks: string;
begin
  Result := ListTasks;
end;

function TTodoTools.CompleteTask(ATaskId: Integer): string;
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
  SummaryChanged;
end;

function TTodoTools.DeleteTask(ATaskId: Integer): string;
var
  LTask: TTaskItem;
  LTitle: string;
begin
  TodoStore.Lock();
  try
    LTask := TodoStore.FindById(ATaskId);
    if LTask = nil then
      Exit(Format(STaskNotFound, [ATaskId]));
    LTitle := LTask.Title;
  finally
    TodoStore.Unlock();
  end;

  if TodoStore.Remove(ATaskId) then
  begin
    Result := Format('Task #%d "%s" deleted', [ATaskId, LTitle]);
    SummaryChanged;
  end
  else
    Result := Format(STaskNotFound, [ATaskId]);
end;

function TTodoTools.DraftDayPlan: string;
begin

end;

function TTodoTools.ImportTasks(const AText: string): string;
begin

end;

function TTodoTools.SlowCount(ACountTo: Integer): string;
var
  LCount: Integer;
begin
  if ACountTo < 1 then
    ACountTo := 1
  else if ACountTo > 60 then
    ACountTo := 60;

  for LCount := 1 to ACountTo do
  begin
    // A second's wait that ends early when the client cancels: WaitFor wakes at
    // once, and CheckCancelled then abandons the request - nothing, not even an
    // error, is sent back for it
    FCancel.WaitFor(1000);
    FCancel.CheckCancelled;

    FProgress.Report(LCount, ACountTo, Format('Counted to %d', [LCount]));
  end;

  Result := Format('Counted to %d', [ACountTo]);
end;

initialization
  TodoStore := TTodoStore.Create();

finalization
  TodoStore.Free();

end.
