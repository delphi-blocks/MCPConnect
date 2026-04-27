unit Test.Form.Misc;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,

  System.Generics.Collections,
  MCPConnect.JRPC.Core,
  MCPConnect.Session.Core;

type
  TfrmMisc = class(TForm)
    edtTemplate: TEdit;
    edtURI: TEdit;
    Button2: TButton;
    btnMatches: TButton;
    memoLog: TMemo;
    btnDelphiQueue: TButton;
    btnMCPQueue: TButton;
    btnPrintQueue: TButton;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnMatchesClick(Sender: TObject);
    procedure btnDelphiQueueClick(Sender: TObject);
    procedure btnMCPQueueClick(Sender: TObject);
    procedure btnPrintQueueClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    FCount: Integer;
    FDelphi: TThreadedQueue<Integer>;
    FMCP: TMCPMessageQueue<TJRPCNotification>;
  public
    { Public declarations }
  end;

var
  frmMisc: TfrmMisc;

implementation

uses
  MCPConnect.MCP.Types,
  MCPConnect.JRPC.Classes,

  System.RegularExpressions;

{$R *.dfm}

function ExtractParams(const ATemplate, AInput: string): TStringMap;
var
  LParamNames: TArray<string>;
  LRegexPattern: string;
  LMatch: TMatch;
  LName: string;
begin
  Result := [];
  LParamNames := [];

  var matches := TRegEx.Matches(ATemplate, '[^{\}]+(?=})');
  for var match in matches do
    LParamNames := LParamNames + [match.Value];

  // 1. Find all parameter names in the template (e.g., 'city', 'options')
  // Match anything between { and }
  (*
  LMatch := TRegEx.Match(ATemplate, '\{([a-zA-Z0-9_]+)\}');
  while LMatch.Success do
  begin
    LParamNames := LParamNames + [LMatch.Groups[1].Value];
    LMatch := LMatch.NextMatch;
  end;
  *)

  // 2. Convert Template to Regex Pattern
  // Escape the URL basics (like // and .) then swap {name} for a named group
  LRegexPattern := TRegEx.Escape(ATemplate);
  // We replace the escaped version of \{name\} with (?P<name>[^/]+)
  LRegexPattern := TRegEx.Replace(LRegexPattern, '\\\{([a-zA-Z0-9_]+)\\\}', '(?P<$1>[^/]+)');

  // 3. Execute the Match on the actual data
  LMatch := TRegEx.Match(AInput, '^' + LRegexPattern + '$');

  if not LMatch.Success then
    Exit;

  for LName in LParamNames do
    Result := Result + [TStringPair.Create(LName, LMatch.Groups[LName].Value)];
end;

procedure TfrmMisc.FormDestroy(Sender: TObject);
begin
  FDelphi.Free;
  FMCP.Free;
end;

procedure TfrmMisc.FormCreate(Sender: TObject);
begin
  FDelphi := TThreadedQueue<Integer>.Create(5);
  FMCP := TMCPMessageQueue<TJRPCNotification>.Create(5);
end;

procedure TfrmMisc.btnMatchesClick(Sender: TObject);
begin
  var res: TArray<string> := [];
  var matches := TRegEx.Matches('demo://weather/dynamic/{city}/{celsius}', '[^{\}]+(?=})');

  for var match in matches do
  begin
    res := res + [match.Value];
    memoLog.Lines.Add(match.Value);
  end;
end;

procedure TfrmMisc.btnDelphiQueueClick(Sender: TObject);
begin
  memoLog.Lines.Add(Format('Enqueing %d', [FCount]));
  FDelphi.Enqueue(FCount);
  Inc(FCount);

  memoLog.Lines.Add(Format('Queue has %d items', [FDelphi.Count]));

end;

procedure TfrmMisc.btnMCPQueueClick(Sender: TObject);
begin
  memoLog.Lines.Add(Format('Enqueing %d', [FCount]));

  var n := TJRPCNotification.Create;
  n.InternalId := FCount;
  n.Method := 'notification/log';
  n.AddNamedParam('level', 'Debug');
  FMCP.Enqueue(n);
  Inc(FCount);

  memoLog.Lines.Add(Format('Queue has %d items', [FMCP.Count]));
end;

procedure TfrmMisc.btnPrintQueueClick(Sender: TObject);
begin
  var n := FMCP.Dequeue;
  if not Assigned(n) then
  begin
    memoLog.Lines.Add('Nothing to deque');
    Exit;
  end;

  memoLog.Lines.Add(Format('Dequeuing [%d]', [n.InternalId]));
  n.Free;

  memoLog.Lines.Add(Format('Queue has %d items', [FMCP.Count]));
end;

procedure TfrmMisc.Button2Click(Sender: TObject);
begin
  var tpl := edtTemplate.Text;
  memoLog.Lines.Add(tpl);

  var uri := edtURI.Text;
  memoLog.Lines.Add(uri);

  var router := TRouteMatcher.Create;
  try
    if router.Match(tpl, uri) then
      for var pair in router.Params do
        memoLog.Lines.Add(pair.Key + ': ' + pair.Value)
    else
        memoLog.Lines.Add('URI is not a match for the template');
  finally
    router.Free;
  end;
end;

end.
