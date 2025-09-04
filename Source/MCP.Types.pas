unit MCP.Types;

interface

{$SCOPEDENUMS ON}

uses
  System.SysUtils, System.Generics.Collections, System.JSON, System.Rtti,

  Neon.Core.Types,
  Neon.Core.Attributes,
  Neon.Core.Nullables,
  Neon.Core.Persistence,
  Neon.Core.Persistence.JSON,
  Neon.Core.Serializers.RTL;

const
  // Initiates connection and negotiates protocol capabilities.
  MethodInitialize = 'initialize';

  // Verifies connection liveness between client and server.
  MethodPing = 'ping';

  // Lists all available server resources.
  MethodResourcesList = 'resources/list';

  // Provides URI templates for constructing resource URIs.
  MethodResourcesTemplatesList = 'resources/templates/list';

  // Retrieves content of a specific resource by URI.
  MethodResourcesRead = 'resources/read';

  // Lists all available prompt templates.
  MethodPromptsList = 'prompts/list';

  // Retrieves a specific prompt template with filled parameters.
  MethodPromptsGet = 'prompts/get';

  // Lists all available executable tools.
  MethodToolsList = 'tools/list';

  // Ivokes a specific tool with provided parameters.
  MethodToolsCall = 'tools/call';

  // Configures the minimum log level for client
  MethodSetLogLevel = 'logging/setLevel';

  // Notifies when the list of available resources changes.
  MethodNotificationResourcesListChanged = 'notifications/resources/list_changed';

  // Notifies when the resources are updated.
  MethodNotificationResourceUpdated = 'notifications/resources/updated';

  // Notifies when the list of available prompt templates changes.
  MethodNotificationPromptsListChanged = 'notifications/prompts/list_changed';

  // Notifies when the list of available tools changes.
  MethodNotificationToolsListChanged = 'notifications/tools/list_changed';


type
  TAnyMap = class(TDictionary<string, TValue>);

  TAnyMapOwned = class(TDictionary<string, TValue>)
  public
    destructor Destroy; override;
  end;

  TJSONMap = class(TObjectDictionary<string, TJSONValue>)
  public
    constructor Create;
  end;

  TJSONObjectMap = class(TObjectDictionary<string, TJSONObject>)
  public
    constructor Create;
  end;

  TMeta = TJSONMap;

  TRequestParams = class
    /// <summary>
    /// Meta is a metadata object that is reserved by MCP for storing additional information
    /// </summary>
    [NeonProperty('_meta'), NeonInclude(IncludeIf.NotEmpty)] Meta: TMeta;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TPaginatedParams = record
    // An opaque token representing the current pagination position.
    // If provided, the server should return results starting after this cursor.
    [NeonProperty('cursor')] Cursor: NullString;
  end;


  /// <summary>
  /// TImplementation describes the name and version of an MCP implementation.
  /// </summary>
  TImplementation = record
    /// <summary>
    /// Name is the name of the implementation.
    /// </summary>
    [NeonProperty('name')] Name: string;

    /// <summary>
    /// Version is the version of the implementation.
    /// </summary>
    [NeonProperty('version')] Version: string;
  end;

  /// <summary>
  /// TRootsCapability is present if the client supports listing roots.
  /// </summary>
  TRootsCapability = record
    /// <summary>
    /// Whether the client supports notifications for changes to the roots list.
    /// </summary>
    [NeonProperty('listChanged'), NeonInclude(IncludeIf.NotEmpty)] ListChanged: NullBoolean;
  end;

  /// <summary>
  /// TClientCapabilities represents capabilities a client may support.
  /// </summary>
  TClientCapabilities = class

    /// <summary>
    /// Experimental, non-standard capabilities that the client supports.
    /// </summary>
    [NeonProperty('experimental'), NeonInclude(IncludeIf.NotEmpty)] &Experimental: TJSONObject;

    /// <summary>
    /// Present if the client supports listing roots.
    /// </summary>
    [NeonProperty('roots'), NeonInclude(IncludeIf.NotEmpty)] Roots: TRootsCapability;

    /// <summary>
    /// Present if the client supports sampling from an LLM.
    /// </summary>
    [NeonProperty('sampling'), NeonInclude(IncludeIf.NotEmpty)] Sampling: TJSONObject;

  public
    constructor Create;
    destructor Destroy; override;
  end;


  /// <summary>
  /// TServerCapabilities represents capabilities that a server may support.
  /// </summary>
  TServerCapabilities = class

    /// <summary>
    /// TPromptsCapability is present if the server offers any prompt templates.
    /// </summary>
    public type TPromptsCapability = record
      /// <summary>
      /// Whether this server supports notifications for changes to the prompt list.
      /// </summary>
      [NeonProperty('listChanged'), NeonInclude(IncludeIf.NotEmpty)] ListChanged: NullBoolean;
    end;

    /// <summary>
    /// TResourcesCapability is present if the server offers any resources to read.
    /// </summary>
    public type TResourcesCapability = record
      /// <summary>
      /// Whether this server supports subscribing to resource updates.
      /// </summary>
      [NeonProperty('subscribe'), NeonInclude(IncludeIf.NotEmpty)] Subscribe: NullBoolean;

      /// <summary>
      /// Whether this server supports notifications for changes to the resource list.
      /// </summary>
      [NeonProperty('listChanged'), NeonInclude(IncludeIf.NotEmpty)] ListChanged: NullBoolean;
    end;

    /// <summary>
    /// TToolsCapability is present if the server offers any tools to call.
    /// </summary>
    public type TToolsCapability = record
      /// <summary>
      /// Whether this server supports notifications for changes to the tool list.
      /// </summary>
      [NeonProperty('listChanged'), NeonInclude(IncludeIf.NotEmpty)] ListChanged: NullBoolean;
    end;
  public
    /// <summary>
    /// Experimental, non-standard capabilities that the server supports.
    /// </summary>
    [NeonProperty('experimental'), NeonInclude(IncludeIf.NotEmpty)] &Experimental: TJSONObject;

    /// <summary>
    /// Present if the server supports sending log messages to the client.
    /// </summary>
    [NeonProperty('logging'), NeonInclude(IncludeIf.NotEmpty)] Logging: TJSONObject;

    /// <summary>
    /// Present if the server offers any prompt templates.
    /// </summary>
    [NeonProperty('prompts'), NeonInclude(IncludeIf.NotEmpty)] Prompts: TPromptsCapability;

    /// <summary>
    /// Present if the server offers any resources to read.
    /// </summary>
    [NeonProperty('resources'), NeonInclude(IncludeIf.NotEmpty)] Resources: TResourcesCapability;

    /// <summary>
    /// Present if the server supports sending sampling requests to clients.
    /// </summary>
    [NeonProperty('sampling'), NeonInclude(IncludeIf.NotEmpty)] Sampling: TJSONObject;

    /// <summary>
    /// Present if the server offers any tools to call.
    /// </summary>
    [NeonProperty('tools'), NeonInclude(IncludeIf.NotEmpty)] Tools: TToolsCapability;

  public
    constructor Create;
    destructor Destroy; override;
  end;

  /// <summary>
  /// TInitializeParams is sent from the client to the server when it first connects.
  /// </summary>
  TInitializeParams = record
    /// <summary>
    /// The latest version of the Model Context Protocol that the client supports.
    /// </summary>
    [NeonProperty('protocolVersion')] ProtocolVersion: string;

    /// <summary>
    /// Client capabilities.
    /// </summary>
    [NeonProperty('capabilities')] Capabilities: TClientCapabilities;

    /// <summary>
    /// Client implementation information.
    /// </summary>
    [NeonProperty('clientInfo')] ClientInfo: TImplementation;
  end;

  /// <summary>
  /// TInitializeResult is sent after receiving an initialize request from the
  /// client.
  /// </summary>
  TInitializeResult = class
    // Parent "Result" type is not defined in the source, assuming it contains no serialized fields.
    /// <summary>
    /// The version of the Model Context Protocol that the server wants to use.
    /// </summary>
    [NeonProperty('protocolVersion')] ProtocolVersion: string;

    /// <summary>
    /// Server capabilities.
    /// </summary>
    [NeonProperty('capabilities')] Capabilities: TServerCapabilities;

    /// <summary>
    /// Server implementation information.
    /// </summary>
    [NeonProperty('serverInfo')] ServerInfo: TImplementation;

    /// <summary>
    /// Instructions describing how to use the server and its features.
    /// </summary>
    [NeonProperty('instructions'), NeonInclude(IncludeIf.NotEmpty)] Instructions: NullString;

  public
    constructor Create;
    destructor Destroy; override;

    function ToJSON(APrettyPrint: Boolean = False): string;
  end;

  /// <summary>
  /// TInitializedNotification is sent from the client to the server after
  /// initialization has finished.
  /// </summary>
  TInitializedNotification = record
    // Parent "Notification" type is not defined in the source, assuming it contains no serialized fields.
  end;

function GetNeonConfig: INeonConfiguration;


implementation


function GetNeonConfig: INeonConfiguration;
begin
  Result := TNeonConfiguration.Camel
    .SetMembers([TNeonMembers.Fields]);

  Result.GetSerializers.RegisterSerializer(TJSONValueSerializer);
end;


{ TInitializeResult }

constructor TInitializeResult.Create;
begin
  Capabilities := TServerCapabilities.Create;
end;

destructor TInitializeResult.Destroy;
begin
  Capabilities.Free;
  inherited;
end;

function TInitializeResult.ToJSON(APrettyPrint: Boolean): string;
begin
  Result := TNeon.ObjectToJSONString(Self, GetNeonConfig.SetPrettyPrint(APrettyPrint));
end;

{ TClientCapabilities }

constructor TClientCapabilities.Create;
begin
  &Experimental := TJSONObject.Create;
  Sampling := TJSONObject.Create;
end;

destructor TClientCapabilities.Destroy;
begin
  Sampling.Free;
  &Experimental.Free;
  inherited;
end;

{ TServerCapabilities }

constructor TServerCapabilities.Create;
begin
  &Experimental := TJSONObject.Create;
  Logging := TJSONObject.Create;
  Sampling := TJSONObject.Create;
end;

destructor TServerCapabilities.Destroy;
begin
  Sampling.Free;
  Logging.Free;
  &Experimental.Free;
  inherited;
end;

{ TRequestParams }

constructor TRequestParams.Create;
begin
  Meta := TMeta.Create;
end;

destructor TRequestParams.Destroy;
begin
  Meta.Free;
  inherited;
end;

{ TJSONMap }

constructor TJSONMap.Create;
begin
  inherited Create([doOwnsValues]);
end;

{ TAnyMapOwned }

destructor TAnyMapOwned.Destroy;
var
  LPair: TPair<string, TValue>;
begin
  for LPair in Self do
  begin
    if LPair.Value.IsObject then
      LPair.Value.AsObject.Free;
  end;

  inherited;
end;

{ TJSONObjectMap }

constructor TJSONObjectMap.Create;
begin
  inherited Create([doOwnsValues]);
end;

end.
