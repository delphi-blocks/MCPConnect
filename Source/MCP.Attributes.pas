unit MCP.Attributes;

interface

uses
  System.SysUtils;

type
  MCPToolAttribute = class(TCustomAttribute)

  end;

  MCPParamAttribute = class(TCustomAttribute)
    constructor Create(const AName, ADescription: string; ARequired: Boolean = True);
  end;

implementation

{ MCPParamAttribute }

constructor MCPParamAttribute.Create(const AName, ADescription: string; ARequired: Boolean);
begin

end;

end.
