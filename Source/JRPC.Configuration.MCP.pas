unit JRPC.Configuration.MCP;

interface

uses
  System.Classes, System.SysUtils,

  JRPC.Configuration.Core;

type
  IJRPCMCPConfig = interface(IJRPCConfiguration)
    ['{B8BBD257-2FE1-479A-8D63-5331164CF5E5}']
    function SetToolClass(AClass: TClass): IJRPCMCPConfig;
    function SetServerName(const AName: string): IJRPCMCPConfig;
    function SetServerVersion(const AVersion: string): IJRPCMCPConfig;
  end;

  [Implements(IJRPCMCPConfig)]
  TJRPCMCPConfig = class(TJRPCConfiguration, IJRPCMCPConfig)
  private
    FToolClass: TClass;
    FServerVersion: string;
    FServerName: string;
  public
    function SetToolClass(AClass: TClass): IJRPCMCPConfig;
    function SetServerName(const AName: string): IJRPCMCPConfig;
    function SetServerVersion(const AVersion: string): IJRPCMCPConfig;

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

{ TJRPCMCPConfig }

procedure TJRPCMCPConfig.AfterConstruction;
begin
  inherited;
  FServerName := 'MCPServer';
  FServerVersion := '1.0';
end;

function TJRPCMCPConfig.CreateDefaultTool: TObject;
begin
  if not Assigned(FToolClass) then
    raise Exception.Create('Default tool not found');

  Result := TRttiUtils.CreateInstance(FToolClass);
end;

function TJRPCMCPConfig.GetDefaultToolClass: TClass;
begin
  if not Assigned(FToolClass) then
    raise Exception.Create('Default tool not found');

  Result := FToolClass;
end;

function TJRPCMCPConfig.SetServerName(const AName: string): IJRPCMCPConfig;
begin
  FServerName := AName;
  Result := Self;
end;

function TJRPCMCPConfig.SetServerVersion(const AVersion: string): IJRPCMCPConfig;
begin
  FServerVersion := AVersion;
  Result := Self;
end;

function TJRPCMCPConfig.SetToolClass(AClass: TClass): IJRPCMCPConfig;
begin
  FToolClass := AClass;
  Result := Self;
end;

initialization
  TJRPCConfigClassRegistry.Instance.RegisterConfigClass(TJRPCMCPConfig);

end.
