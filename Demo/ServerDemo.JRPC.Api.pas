unit ServerDemo.JRPC.Api;

interface

uses
  System.Classes, System.SysUtils, System.JSON,
  JSON.RPC,

  MCP.Types,
  MCP.Tools,
  MCP.Resources,
  MCP.Prompts;

type
  TPerson = class(TObject)
  private
    FName: string;
  public
    property Name: string read FName write FName;
    constructor Create(const AName: string);
  end;

  [JRPC('math')]
  TMathApi = class
  public
    [JRPCMethod('sum')]
    function Sum(
      [JRPCParam('a')] a,
      [JRPC('b')] b: Integer): Integer;

    [JRPCMethod('echo')]
    function EchoString(
      [JRPCParam('value')] const AValue: string): string;

    [JRPCMethod('timestamp')]
    function GetCurrentTimeStamp(): TDateTime;
  end;

  [JRPC('object')]
  TJRPObjectApi = class
  public
    [JRPC('person')]
    function GetPerson(): TPerson;
  end;

  [JRPC('tool')]
  TMCPToolApi = class
  public
    [JRPC('call')]
    function Call(
        [JRPCParam('name')] const AName: string;
        [JRPC('arguments')] AArguments: TJSONObject;
        [JRPC('_meta')] Meta: TJSONObject): TCallToolResult;
  end;

implementation

{ TMathApi }

function TMathApi.EchoString(const AValue: string): string;
begin
  Result := AValue;
end;

function TMathApi.GetCurrentTimeStamp: TDateTime;
begin
  Result := Now;
end;

function TMathApi.Sum(a, b: Integer): Integer;
begin
  Result := a + b;
end;

{ TJRPObjectApi }

function TJRPObjectApi.GetPerson: TPerson;
begin
  Result := TPerson.Create('Luca');
end;

{ TPerson }

constructor TPerson.Create(const AName: string);
begin
  inherited Create;
  FName := AName;
end;

{ TMCPToolApi }

function TMCPToolApi.Call(const AName: string; AArguments: TJSONObject; Meta: TJSONObject): TCallToolResult;
begin
  var t := TTextContent.Create;
  t.Text := 'This is the result => ';
  t.&Type := 'text';

  Result := TCallToolResult.Create;
  Result.SetContent(t);

  for var LPair in AArguments do
    t.Text := t.Text + LPair.JsonString.Value + ':' + LPair.JsonValue.ToString;
end;

initialization
  //TJRPCRegistry.Instance.NeonConfig := MCPNeonConfig;
  TJRPCRegistry.Instance.RegisterClass(TMathApi);
  TJRPCRegistry.Instance.RegisterClass(TJRPObjectApi);
  TJRPCRegistry.Instance.RegisterClass(TMCPToolApi, MCPNeonConfig);

end.
