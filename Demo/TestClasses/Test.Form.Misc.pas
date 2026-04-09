unit Test.Form.Misc;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfrmMisc = class(TForm)
    edtTemplate: TEdit;
    edtURI: TEdit;
    Button2: TButton;
    btnMatches: TButton;
    memoLog: TMemo;
    procedure btnMatchesClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
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
