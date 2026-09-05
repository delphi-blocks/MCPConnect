unit MCPServer.Prompts;

interface

uses
  System.Classes, System.SysUtils,

  MCPConnect.MCP.Types.Base,
  MCPConnect.MCP.Attributes;

type
  TTodoPrompts = class
  public
    [MCPPrompt('plan-day', 'Plan My Day',
      'Asks the model to organize and prioritize the pending tasks')]
    function PlanDay(): string;

    [MCPPrompt('weekly-review', 'Weekly Review',
      'Asks the model to review completed and pending tasks and suggest next steps')]
    function WeeklyReview(
      [MCPArgument('focus_area', 'Optional area to focus the review on')]
      const AFocusArea: string
    ): string;
  end;

implementation

uses
  System.StrUtils;

{ TTodoPrompts }

function TTodoPrompts.PlanDay(): string;
begin
  Result := 'Look at my current todo list and help me plan my day. ' +
    'Prioritize the pending tasks, suggest an order to tackle them, ' +
    'and estimate how long each might take.';
end;

function TTodoPrompts.WeeklyReview(const AFocusArea: string): string;
begin
  Result := 'Review my todo list for a weekly check-in. ' +
    'Summarize what has been completed, what is still pending, ' +
    'and suggest next steps or tasks that might be missing.';
  if not AFocusArea.IsEmpty() then
    Result := Result + Format(' Focus especially on: %s.', [AFocusArea]);
end;

end.
