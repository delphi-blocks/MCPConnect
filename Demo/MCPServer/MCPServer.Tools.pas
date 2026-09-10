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
  MCPConnect.MCP.Types.Base,
  MCPConnect.MCP.Types.Tools,
  MCPConnect.MCP.Types.Mrtr,
  MCPConnect.MCP.Attributes;

type
  TConfirm = class
  private
    FValue: Boolean;
  public
    property Value: Boolean read FValue write FValue;
  end;

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

    [McpTool('delete_task', 'Delete a task from the todo list')]
    function DeleteTask(
      [McpParam('task_id', 'ID of the task to delete')] ATaskId: Integer
    ): TBaseResult;

    [McpTool('metrics_report', 'Harvests the metrics the demo collects on each todo tool call')]
    function MetricsReport(): string;
  end;

var
  TodoStore: TTodoStore;

implementation

uses
  System.Diagnostics,

  MCPConnect.Metrics,
  MCPConnect.Metrics.Exporters;

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
  // One-liner measurements: a counter with a label, a duration histogram...
  TMetrics
    .Counter('todo.tool.calls', 'Todo tool invocations', 'calls')
    .Add(1, ['tool', 'add_task']);

  LWatch := TStopwatch.StartNew;
  try
    LTask := TodoStore.Add(ATitle, ADescription);
  finally
    LWatch.Stop;
  end;

  TMetrics
    .Histogram('todo.tool.duration_ms', 'Todo tool duration', 'ms')
    .Observe(LWatch.Elapsed.TotalMilliseconds, ['tool', 'add_task']);

  // ...and a gauge keeping the current list size
  TMetrics
    .Gauge('todo.tasks.total', 'Tasks currently in the list', 'tasks')
    .SetValue(TodoStore.CountTasks());

  Result := Format('Task #%d "%s" added successfully', [LTask.Id, LTask.Title]);
end;

function TTodoTool.ListTasks(): string;
begin
  TMetrics
    .Counter('todo.tool.calls', 'Todo tool invocations', 'calls')
    .Add(1, ['tool', 'list_tasks']);

  TMetrics
    .Gauge('todo.tasks.total', 'Tasks currently in the list', 'tasks')
    .SetValue(TodoStore.CountTasks());

  Result := TodoStore.ToText();
end;

function TTodoTool.CompleteTask(ATaskId: Integer): string;
var
  LTask: TTaskItem;
begin
  TMetrics
    .Counter('todo.tool.calls', 'Todo tool invocations', 'calls')
    .Add(1, ['tool', 'complete_task']);

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

function TTodoTool.DeleteTask(ATaskId: Integer): TBaseResult;
var
  LTask: TTaskItem;
  LTitle: string;
begin
  if FParams.InputResponses.Count > 0 then
    Logger.Log('User response for a previous Input Request', TLogLevel.Debug);

  TMetrics
    .Counter('todo.tool.calls', 'Todo tool invocations', 'calls')
    .Add(1, ['tool', 'delete_task']);

//  if AParams.RequestState <> 'TEST' then
//  begin
//    var LParams := TElicitRequestParams.Create;
//    LParams.RequestedSchema := TNeonSchemaGenerator.ClassToJSONSchema(TConfirm);
//    LParams.Message := 'Sei sicuro?';
//
//    var LRequiredResult := TInputRequiredResult.Create;
//    LRequiredResult.RequestState := 'TEST';
//    LRequiredResult.InputRequests.AddElicitation('123', LParams);
//    Exit(LRequiredResult);
//  end;
//
  var LResult := '';

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

  var LToolCallResult := TCallToolResult.Create;
  LToolCallResult.Content.AddText(LResult);
  Exit(LToolCallResult);
end;

function TTodoTool.MetricsReport(): string;
var
  LTarget: TStringList;
  LExporter: IMetricExporter;
begin
  // Harvesting "later": everything the tools recorded since the server
  // started is rendered through the sample text exporter and returned as a
  // report any MCP client can ask for.
  LTarget := TStringList.Create();
  try
    LExporter := TMetricTextExporter.Create(LTarget);
    LExporter.Export(TMetrics.Collect);
    Result := LTarget.Text;
  finally
    LTarget.Free();
  end;
end;

initialization
  TodoStore := TTodoStore.Create();

finalization
  TodoStore.Free();

end.
