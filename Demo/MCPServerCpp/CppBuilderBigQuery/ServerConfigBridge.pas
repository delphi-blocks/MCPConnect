unit ServerConfigBridge;

interface

uses
  System.Classes,
  MCPConnect.JRPC.Server;

procedure ConfigureBigQueryServer(AServer: TJRPCServer; AToolClass: TClass);
procedure UnregisterBigQueryTools(AServer: TJRPCServer; AToolClass: TClass);

implementation

uses
  System.SysUtils,
  MCPConnect.Configuration.Core,
  MCPConnect.Configuration.MCP,
  MCPConnect.MCP.Server.Api;

procedure ConfigureBigQueryServer(AServer: TJRPCServer; AToolClass: TClass);
var
  LConfig: IMCPConfig;
begin
  if not Assigned(AServer) then
    raise EArgumentNilException.Create('AServer');
  if not Assigned(AToolClass) then
    raise EArgumentNilException.Create('AToolClass');

  { Keep the Delphi generic Configure<T> call on the Delphi side. The C++
    application supplies its RTTI-enabled tool class as a metaclass value. }
  LConfig := AServer.Plugin.Configure<IMCPConfig>;

  LConfig.Server
    .SetName('cppbuilder-bigquery-mcp')
    .SetDescription('Read-only BigQuery metadata sample hosted by C++Builder')
    .SetVersion('1.0.0');

  LConfig.Security
    .SetCORS(False)
    .SetAllowedMethods(['POST']);

  { Do not depend on package-global initialization in this mixed Delphi/C++
    host. Populate the server-local registry when the user clicks Start so
    initialize, tools/*, ping, and the other standard MCP methods resolve
    against the same IMCPConfig instance as the BigQuery tools. }
  LConfig.MessageHandling
    .RegisterApi(TMCPInitializeApi)
    .RegisterApi(TMCPToolsApi)
    .RegisterApi(TMCPPromptsApi)
    .RegisterApi(TMCPResourcesApi)
    .RegisterApi(TMCPNotificationsApi)
    .RegisterApi(TMCPLoggingApi)
    .RegisterApi(TMCPPingApi);

  LConfig.Tools
    .RegisterTool(AToolClass, 'ListDatasets', 'bq_list_datasets',
      'Lists the first 50 BigQuery datasets and reports truncation')
      .WithParam('AProjectId', 'project_id', 'Google Cloud project ID')
      .EndTool
    .RegisterTool(AToolClass, 'DescribeDataset', 'bq_describe_dataset',
      'Lists up to 20 BigQuery tables and returns each BASIC schema')
      .WithParam('AProjectId', 'project_id', 'Google Cloud project ID')
      .WithParam('ADatasetId', 'dataset_id', 'BigQuery dataset ID')
      .EndTool;

  LConfig.ApplyConfig;
end;

procedure UnregisterBigQueryTools(AServer: TJRPCServer; AToolClass: TClass);
var
  LConfig: IMCPConfig;
begin
  if not Assigned(AServer) or not Assigned(AToolClass) then
    Exit;

  LConfig := AServer.Plugin.Configure<IMCPConfig>;
  LConfig.Tools.UnregisterClass(AToolClass);
  LConfig.ApplyConfig;
end;

end.
