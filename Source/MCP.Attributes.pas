unit MCP.Attributes;

interface

uses
  System.SysUtils;

type
  MCPToolAttribute = class(TCustomAttribute)
  private
    FName: string;
    FDescription: string;
  public
    property Name: string read FName;
    property Description: string read FDescription;

    constructor Create(const AName, ADescription: string);
  end;

  MCPParamAttribute = class(TCustomAttribute)
  private
    FName: string;
    FDescription: string;
    FRequired: Boolean;
  public
    property Name: string read FName;
    property Description: string read FDescription;
    property Required: Boolean read FRequired;

    constructor Create(const AName, ADescription: string; ARequired: Boolean = True);
  end;

implementation

{ MCPParamAttribute }

constructor MCPParamAttribute.Create(const AName, ADescription: string; ARequired: Boolean);
begin
  inherited Create;
  FName := AName;
  FDescription := ADescription;
  FRequired := ARequired;
end;

{ MCPToolAttribute }

constructor MCPToolAttribute.Create(const AName, ADescription: string);
begin
  inherited Create;
  FName := AName;
  FDescription := ADescription;
end;

end.
