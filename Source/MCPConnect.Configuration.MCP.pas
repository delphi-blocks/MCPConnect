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
unit MCPConnect.Configuration.MCP;

interface

uses
  System.Classes, System.SysUtils,

  MCPConnect.Content.Writers,
  MCPConnect.Configuration.Core;

type
  IMCPConfig = interface(IJRPCConfiguration)
  ['{B8BBD257-2FE1-479A-8D63-5331164CF5E5}']
    function SetToolClass(AClass: TClass): IMCPConfig;
    function SetServerName(const AName: string): IMCPConfig;
    function SetServerVersion(const AVersion: string): IMCPConfig;
    function RegisterWriter(AClass: TCustomWriterClass): IMCPConfig;
    function GetWriters: TMCPWriterRegistry;
  end;

  [Implements(IMCPConfig)]
  TMCPConfig = class(TJRPCConfiguration, IMCPConfig)
  private
    FWriterRegistry: TMCPWriterRegistry;
    FToolClass: TClass;
    FServerVersion: string;
    FServerName: string;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure AfterConstruction; override;

    function SetToolClass(AClass: TClass): IMCPConfig;
    function SetServerName(const AName: string): IMCPConfig;
    function SetServerVersion(const AVersion: string): IMCPConfig;
    function RegisterWriter(AClass: TCustomWriterClass): IMCPConfig;
    function GetWriters: TMCPWriterRegistry;

    function CreateDefaultTool: TObject;
    function GetDefaultToolClass: TClass;

    property ToolClass: TClass read FToolClass write FToolClass;
    property ServerName: string read FServerName write FServerName;
    property ServerVersion: string read FServerVersion write FServerVersion;
  end;

implementation

uses
  Neon.Core.Utils,
  MCPConnect.JRPC.Core;

{ TMCPConfig }

constructor TMCPConfig.Create;
begin
  inherited;
  FWriterRegistry := TMCPWriterRegistry.Create;
end;

procedure TMCPConfig.AfterConstruction;
begin
  inherited;
  FServerName := 'MCPServer';
  FServerVersion := '1.0';
end;

destructor TMCPConfig.Destroy;
begin
   FWriterRegistry.Free;
  inherited;
end;

function TMCPConfig.CreateDefaultTool: TObject;
begin
  if not Assigned(FToolClass) then
    raise EJRPCException.Create('Default tool not found');

  Result := TRttiUtils.CreateInstance(FToolClass);
end;

function TMCPConfig.GetDefaultToolClass: TClass;
begin
  if not Assigned(FToolClass) then
    raise EJRPCException.Create('Default tool not found');

  Result := FToolClass;
end;

function TMCPConfig.GetWriters: TMCPWriterRegistry;
begin
  Result := FWriterRegistry;
end;

function TMCPConfig.RegisterWriter(AClass: TCustomWriterClass): IMCPConfig;
begin
  FWriterRegistry.RegisterWriter(AClass);
  Result := Self;
end;

function TMCPConfig.SetServerName(const AName: string): IMCPConfig;
begin
  FServerName := AName;
  Result := Self;
end;

function TMCPConfig.SetServerVersion(const AVersion: string): IMCPConfig;
begin
  FServerVersion := AVersion;
  Result := Self;
end;

function TMCPConfig.SetToolClass(AClass: TClass): IMCPConfig;
begin
  FToolClass := AClass;
  Result := Self;
end;

initialization
  TJRPCConfigClassRegistry.Instance.RegisterConfigClass(TMCPConfig);

end.
