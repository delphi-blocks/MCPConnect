{******************************************************************************}
{                                                                              }
{  Delphi MCP Connect Library                                                  }
{                                                                              }
{  Copyright (c) Paolo Rossi <dev@paolorossi.net>                              }
{                Luca Minuti <code@lucaminuti.it>                              }
{  All rights reserved.                                                        }
{                                                                              }
{  https://github.com/delphi-blocks/MCPConnect                                 }
{                                                                              }
{  Licensed under the MIT license                                              }
{                                                                              }
{******************************************************************************}
unit MCPConnect.MCP.Config;

interface

uses
  System.SysUtils, System.JSON, System.Generics.Collections,

  Neon.Core.Attributes,
  MCPConnect.MCP.Types;

type

  TMCPConfigServer = class
    &Type: string;
  end;

  TMCPConfigServerLocal = class(TMCPConfigServer)
    Command: string;
    [NeonInclude(IncludeIf.NotEmpty)] Args: TArray<string>;
    [NeonInclude(IncludeIf.NotEmpty)] Env: TDictionary<string, string>;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TMCPConfigServerRemote = class(TMCPConfigServer)
    Url: string;
    [NeonInclude(IncludeIf.NotEmpty)] Headers: TDictionary<string, string>;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TMCPConfigServers = class(TObjectDictionary<string, TMCPConfigServer>)

  end;

  TMCPConfig = class
  public
    Servers: TMCPConfigServers;
  public
    constructor Create;
    destructor Destroy; override;
  end;



implementation

{ TMCPConfig }

constructor TMCPConfig.Create;
begin
  Servers := TMCPConfigServers.Create([doOwnsValues]);
end;

destructor TMCPConfig.Destroy;
begin
  Servers.Free;
  inherited;
end;

{ TMCPConfigServerLocal }

constructor TMCPConfigServerLocal.Create;
begin
  Env := TDictionary<string, string>.Create;
end;

destructor TMCPConfigServerLocal.Destroy;
begin
  Env.Free;
  inherited;
end;

{ TMCPConfigServerRemote }

constructor TMCPConfigServerRemote.Create;
begin
  Headers := TDictionary<string, string>.Create;
end;

destructor TMCPConfigServerRemote.Destroy;
begin
  Headers.Free;
  inherited;
end;

end.
