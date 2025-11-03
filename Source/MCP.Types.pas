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

  TMetaClass = class
    /// <summary>
    /// Meta is a metadata object that is reserved by MCP for storing additional information
    /// </summary>
    [NeonProperty('_meta'), NeonInclude(IncludeIf.NotEmpty)] Meta: TJSONObject;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TRequestParams = class(TMetaClass)
  end;

  TPaginatedParams = record
    // An opaque token representing the current pagination position.
    // If provided, the server should return results starting after this cursor.
    [NeonProperty('cursor')] Cursor: NullString;
  end;

  // Otional annotations for the client.
  // The client can use annotations to inform how objects are used or displayed
  TAnnotations = class
    // Describes who the intended customer of this object or data is.
    //
    // It can include multiple entries to indicate content useful for multiple
    // audiences (e.g., `["user", "assistant"]`).
    [NeonInclude(IncludeIf.NotEmpty)] Audience: TArray<string>;

    // Describes how important this data is for operating the server.
    //
    // A value of 1 means "most important," and indicates that the data is
    // effectively required, while 0 means "least important," and indicates that
    // the data is entirely optional.

    [NeonInclude(IncludeIf.NotEmpty)] Priority: Currency;
  end;


  /// <summary>
  /// TImplementation describes the name and version of an MCP implementation.
  /// </summary>
  TImplementation = class
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
  TInitializeParams = class
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

  public
    constructor Create;
    destructor Destroy; override;
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
  TInitializedNotificationParams = class(TMetaClass)
  end;

  /// <summary>
  ///   When a party wants to cancel an in-progress request, it sends a notifications/cancelled *
  ///   The ID of the request to cancel * An optional reason string that can be logged or displayed
  /// </summary>
  TCancelledNotificationParams = record

    /// <summary>
    ///   A uniquely identifying ID for a request in JSON-RPC.
    /// </summary>
    [NeonProperty('requestId')] RequestId: Integer;

    /// <summary>
    ///   An optional string describing the reason for the cancellation. This MAY be logged or
    ///   presented to the user.
    /// </summary>
    [NeonProperty('reason')] Reason: NullString;
  end;

  { ************ Contents ************ }

  /// <summary>
  ///   Base class for the content(s)
  /// </summary>
  TBaseContent = class(TMetaClass)
  public
    [NeonProperty('annotations'), NeonInclude(IncludeIf.NotEmpty)] Annotations: TAnnotations;
    [NeonProperty('type')] &Type: string;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  /// <summary>
  ///   Text provided to or from an LLM.
  /// </summary>
  TTextContent = class(TBaseContent)
  public
    [NeonProperty('text')] Text: string;
    constructor Create(const AText: string = '');
  end;

  /// <summary>
  ///   An image provided to or from an LLM.
  /// </summary>
  TImageContent = class(TBaseContent)
  public
    [NeonProperty('data')] Data: string;
    [NeonProperty('mimeType')] MIMEType: string;
  end;

  /// <summary>
  ///   Audio provided to or from an LLM.
  /// </summary>
  TAudioContent = class(TBaseContent)
  public
    [NeonProperty('data')] Data: string;
    [NeonProperty('mimeType')] MIMEType: string;
  end;

  /// <summary>
  ///   The contents of a specific resource or sub-resource.
  /// </summary>
  TResourceContents = class(TMetaClass)
  public
    /// <summary>
    /// The URI of this resource.
    /// </summary>
    [NeonProperty('uri')] URI: string;

    /// <summary>
    /// The MIME type of this resource, if known.
    /// </summary>
    [NeonProperty('mimeType')] MIMEType: NullString;
  end;

  /// <summary>
  /// Represents a text-based resource.
  /// </summary>
  TTextResourceContents = class(TResourceContents)
  public
    /// <summary>
    /// The text of the item.
    /// </summary>
    /// <remarks>This must only be set if the item can actually be represented as text (not binary data).</remarks>
    [NeonProperty('text')] Text: string;
  end;

  /// <summary>
  /// Represents a binary-based resource.
  /// </summary>
  TBlobResourceContents = class(TResourceContents)
  public
    /// <summary>
    /// A base64-encoded string representing the binary data of the item.
    /// </summary>
    [NeonProperty('blob')] Blob: string;
  end;

  /// <summary>
  ///   A resource that the server is capable of reading, included in a prompt or tool call result.
  /// </summary>
  /// <remarks>
  ///   Note: resource links returned by tools are not guaranteed to appear in the results of
  ///   `resources/list` requests.
  /// </remarks>
  TResourceLink = class(TBaseContent)
  public
    [NeonProperty('uri')] URI: string;
    [NeonProperty('name')] Name: string;
    [NeonProperty('description')] Description: string;
    [NeonProperty('mimeType')] MIMEType: string;
  end;

  /// <summary>
  ///   The contents of a resource, embedded into a prompt or tool call result. It is up to the
  ///   client how best to render embedded resources for the benefit of the LLM and/or the user.
  /// </summary>
  TEmbeddedResource = class(TBaseContent)
  public
    [NeonProperty('resource')] Resource: TResourceContents;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  /// <summary>
  ///   Base class for all the Content-based MCP classes
  /// </summary>
  TContentClass = class
  public

   /// <summary>
   ///   Can be TextContent, ImageContent, AudioContent, ResourceLink, or
   ///   EmbeddedResource
   /// </summary>
	  [NeonProperty('content')] Content: TBaseContent;

  public
    constructor Create;
    destructor Destroy; override;

    procedure SetContent(AContent: TBaseContent);

    function GetContentAsText: TTextContent;
    function GetContentAsImage: TImageContent;
    function GetContentAsAudio: TAudioContent;
    function GetContentAsResource: TResourceLink;
    function GetContentAsEmbedded: TEmbeddedResource;
  end;



function MCPNeonConfig: INeonConfiguration;


implementation


function MCPNeonConfig: INeonConfiguration;
begin
  Result := TNeonConfiguration.Camel
    .SetMembers([TNeonMembers.Fields]);

  Result.GetSerializers.RegisterSerializer(TJSONValueSerializer);
end;


{ TInitializeResult }

constructor TInitializeResult.Create;
begin
  Capabilities := TServerCapabilities.Create;
  ServerInfo := TImplementation.Create;
end;

destructor TInitializeResult.Destroy;
begin
  ServerInfo.Free;
  Capabilities.Free;
  inherited;
end;

function TInitializeResult.ToJSON(APrettyPrint: Boolean): string;
begin
  Result := TNeon.ObjectToJSONString(Self, MCPNeonConfig.SetPrettyPrint(APrettyPrint));
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

{ TInitializeParams }

constructor TInitializeParams.Create;
begin
  Capabilities := TClientCapabilities.Create;
  ClientInfo := TImplementation.Create;
end;

destructor TInitializeParams.Destroy;
begin
  Capabilities.Free;
  ClientInfo.Free;
  inherited;
end;

{ TBaseContent }

constructor TBaseContent.Create;
begin
  inherited;
  Annotations := TAnnotations.Create;
end;

destructor TBaseContent.Destroy;
begin
  Annotations.Free;
  inherited;
end;

{ TEmbeddedResource }

constructor TEmbeddedResource.Create;
begin
  Resource := TResourceContents.Create;
end;

destructor TEmbeddedResource.Destroy;
begin
  Resource.Free;
  inherited;
end;

{ TContentClass }

constructor TContentClass.Create;
begin
  Content := TBaseContent.Create;
end;

destructor TContentClass.Destroy;
begin
  Content.Free;
  inherited;
end;

function TContentClass.GetContentAsAudio: TAudioContent;
begin
  if not (Content is TAudioContent) then
  begin
    Content.Free;
    Content := TAudioContent.Create;
  end;

  Result := Content as TAudioContent;
end;

function TContentClass.GetContentAsEmbedded: TEmbeddedResource;
begin
  if not (Content is TEmbeddedResource) then
  begin
    Content.Free;
    Content := TEmbeddedResource.Create;
  end;

  Result := Content as TEmbeddedResource;
end;

function TContentClass.GetContentAsImage: TImageContent;
begin
  if not (Content is TImageContent) then
  begin
    Content.Free;
    Content := TImageContent.Create;
  end;

  Result := Content as TImageContent;
end;

function TContentClass.GetContentAsResource: TResourceLink;
begin
  if not (Content is TResourceLink) then
  begin
    Content.Free;
    Content := TResourceLink.Create;
  end;

  Result := Content as TResourceLink;
end;

function TContentClass.GetContentAsText: TTextContent;
begin
  if not (Content is TTextContent) then
  begin
    Content.Free;
    Content := TTextContent.Create;
  end;

  Result := Content as TTextContent;
end;

procedure TContentClass.SetContent(AContent: TBaseContent);
begin
  if (AContent <> nil) and (AContent <> Content) then
  begin
    Content.Free;
    Content := AContent;
  end;
end;

{ TMetaClass }

constructor TMetaClass.Create;
begin
  Meta := TJSONObject.Create;
end;

destructor TMetaClass.Destroy;
begin
  Meta.Free;
  inherited;
end;

{ TTextContent }

constructor TTextContent.Create(const AText: string);
begin
  inherited Create;
  Text := AText;
  &Type := 'text';
end;

end.
