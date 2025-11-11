unit JRPC.Configuration.MCP;

interface

uses
  System.Classes, System.SysUtils,

  JRPC.Configuration.Core;

type
  IMCPConfig = interface(IJRPCConfiguration)
  ['{B8BBD257-2FE1-479A-8D63-5331164CF5E5}']
    function SetToolClass(AClass: TClass): IMCPConfig;
    function SetServerName(const AName: string): IMCPConfig;
    function SetServerVersion(const AVersion: string): IMCPConfig;
  end;

  [Implements(IMCPConfig)]
  TMCPConfig = class(TJRPCConfiguration, IMCPConfig)
  private
    FToolClass: TClass;
    FServerVersion: string;
    FServerName: string;
  public
    function SetToolClass(AClass: TClass): IMCPConfig;
    function SetServerName(const AName: string): IMCPConfig;
    function SetServerVersion(const AVersion: string): IMCPConfig;

    function CreateDefaultTool: TObject;
    function GetDefaultToolClass: TClass;

    property ToolClass: TClass read FToolClass write FToolClass;
    property ServerName: string read FServerName write FServerName;
    property ServerVersion: string read FServerVersion write FServerVersion;

    procedure AfterConstruction; override;

  end;

implementation

uses
  Neon.Core.Utils;

{ TMCPConfig }

procedure TMCPConfig.AfterConstruction;
begin
  inherited;
  FServerName := 'MCPServer';
  FServerVersion := '1.0';
end;

function TMCPConfig.CreateDefaultTool: TObject;
begin
  if not Assigned(FToolClass) then
    raise Exception.Create('Default tool not found');

  Result := TRttiUtils.CreateInstance(FToolClass);
end;

function TMCPConfig.GetDefaultToolClass: TClass;
begin
  if not Assigned(FToolClass) then
    raise Exception.Create('Default tool not found');

  Result := FToolClass;
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
