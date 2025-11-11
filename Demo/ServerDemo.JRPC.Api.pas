unit ServerDemo.JRPC.Api;

interface

uses
  System.Classes, System.SysUtils, System.JSON,
  MCPConnect.JRPC.Core,

  MCPConnect.MCP.Types,
  MCPConnect.MCP.Tools,
  MCPConnect.MCP.Resources,
  MCPConnect.MCP.Prompts;

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

initialization
  TJRPCRegistry.Instance.RegisterClass(TMathApi);
  TJRPCRegistry.Instance.RegisterClass(TJRPObjectApi);

end.
