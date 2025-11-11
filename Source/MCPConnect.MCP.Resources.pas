unit MCPConnect.MCP.Resources;

interface

{$SCOPEDENUMS ON}

uses
  System.SysUtils, System.Generics.Collections, System.JSON, System.Rtti,

  Neon.Core.Types,
  Neon.Core.Attributes,
  Neon.Core.Nullables,
  Neon.Core.Persistence,
  Neon.Core.Persistence.JSON,
  Neon.Core.Serializers.RTL,

  MCPConnect.MCP.Types;

type
  /// <summary>
  /// Represents a paginated request.
  /// </summary>
  TPaginatedRequest = record
  end;

  /// <summary>
  /// Represents a paginated result.
  /// </summary>
  TPaginatedResult = record
  end;

  /// <summary>
  /// Represents a base result.
  /// </summary>
  TResult = record
  end;

  TNotificationParams = record
    // This parameter name is reserved by MCP to allow clients and
    // servers to attach additional metadata to their notifications.
    [NeonProperty('_meta'), NeonInclude(IncludeIf.NotEmpty)] Meta: TJSONObject;

    // Additional fields can be added to this map
    [NeonIgnore] AdditionalFields: TJSONObject;
  end;

  TMCPAnnotation = record
  
    // Describes who the intended customer of this object or data is.
    //
    // It can include multiple entries to indicate content useful for multiple
    // audiences (e.g., `["user", "assistant"]`).
    [NeonProperty('audience'), NeonInclude(IncludeIf.NotEmpty)]Audience: TArray<string>;

    // Describes how important this data is for operating the server.
    //
    // A value of 1 means "most important," and indicates that the data is
    // effectively required, while 0 means "least important," and indicates that
    // the data is entirely optional.
    [NeonProperty('priority')] Priority: Nullable<Currency>;
  end;

  /// <summary>
  /// Represents a known resource that the server is capable of reading.
  /// </summary>
  TMCPResource = class
  public
    [NeonProperty('annotation'), NeonInclude(IncludeIf.NotEmpty)] Annotated: TMCPAnnotation;
    /// <summary>
    /// Metadata object reserved by MCP for storing additional information.
    /// </summary>
    [NeonProperty('_meta'), NeonInclude(IncludeIf.NotEmpty)] Meta: TJSONObject;
    /// <summary>
    /// The URI of this resource.
    /// </summary>
    [NeonProperty('uri')] URI: string;
    /// <summary>
    /// A human-readable name for this resource.
    /// </summary>
    /// <remarks>This can be used by clients to populate UI elements.</remarks>
    [NeonProperty('name')] Name: string;
    /// <summary>
    /// A description of what this resource represents.
    /// </summary>
    /// <remarks>This can be used by clients to improve the LLM's understanding of available resources. It can be thought of like a 'hint' to the model.</remarks>
    [NeonProperty('description')] Description: NullString;
    /// <summary>
    /// The MIME type of this resource, if known.
    /// </summary>
    [NeonProperty('mimeType')] MIMEType: NullString;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TMCPResources = class(TObjectList<TMCPResource>);


  /// <summary>
  /// Represents a template description for resources available on the server.
  /// </summary>
  TMCPResourceTemplate = class
  public
    [NeonProperty('annotations'), NeonInclude(IncludeIf.NotEmpty)] Annotation: TMCPAnnotation;
    /// <summary>
    /// Metadata object reserved by MCP for storing additional information.
    /// </summary>
    [NeonProperty('_meta'), NeonInclude(IncludeIf.NotEmpty)] Meta: TJSONObject;
    /// <summary>
    /// A URI template (according to RFC 6570) that can be used to construct resource URIs.
    /// </summary>
    [NeonProperty('uriTemplate')] URITemplate: NullString;
    /// <summary>
    /// A human-readable name for the type of resource this template refers to.
    /// </summary>
    /// <remarks>This can be used by clients to populate UI elements.</remarks>
    [NeonProperty('name')] Name: string;
    /// <summary>
    /// A description of what this template is for.
    /// </summary>
    /// <remarks>This can be used by clients to improve the LLM's understanding of available resources. It can be thought of like a 'hint' to the model.</remarks>
    [NeonProperty('description')] Description: NullString;
    /// <summary>
    /// The MIME type for all resources that match this template.
    /// </summary>
    /// <remarks>This should only be included if all resources matching this template have the same type.</remarks>
    [NeonProperty('mimeType')] MIMEType: NullString;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TMCPResourceTemplates = class(TObjectList<TMCPResourceTemplate>);


  /// <summary>
  /// Sent from the client to request a list of resources the server has.
  /// </summary>
  TListResourcesRequest = record
  public
    [NeonProperty('PaginatedRequest')] PaginatedRequest: TPaginatedRequest;
  end;

  /// <summary>
  /// The server's response to a resources/list request from the client.
  /// </summary>
  TListResourcesResult = class
  public
    //[NeonProperty('PaginatedResult')] PaginatedResult: TPaginatedResult;
    /// <summary>
    /// A list of available resources.
    /// </summary>
    [NeonProperty('resources')] Resources: TMCPResources;
  public
    constructor Create;
    destructor Destroy; override;

    function AddResource(const AName, AURI, AType: string): TMCPResource;
  end;

  /// <summary>
  /// Sent from the client to request a list of resource templates the server has.
  /// </summary>
  TListResourceTemplatesRequest = record
  public
    [NeonProperty('PaginatedRequest')] PaginatedRequest: TPaginatedRequest;
  end;

  /// <summary>
  /// The server's response to a resources/templates/list request from the client.
  /// </summary>
  [NeonProperty('ListResourceTemplatesResult')]
  TListResourceTemplatesResult = class
  public
    [NeonProperty('PaginatedResult')] PaginatedResult: TPaginatedResult;
    /// <summary>
    /// A list of available resource templates.
    /// </summary>
    [NeonProperty('resourceTemplates')] ResourceTemplates: TMCPResourceTemplates;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  /// <summary>
  /// Represents the parameters for a resources/read request.
  /// </summary>
  TReadResourceParams = class
  public

    /// <summary>
    /// The URI of the resource to read.
    /// </summary>
    /// <remarks>The URI can use any protocol; it is up to the server how to interpret it.</remarks>
    [NeonProperty('uri')] URI: string;

    /// <summary>
    /// Arguments to pass to the resource handler.
    /// </summary>
    [NeonProperty('arguments')] [NeonInclude(IncludeIf.NotEmpty)] Arguments: TJSONObject;

  public
    constructor Create;
    destructor Destroy; override;
  end;

  /// <summary>
  /// The server's response to a resources/read request from the client.
  /// </summary>
  TReadResourceResult = class
  public
    [NeonProperty('Result')] Result: TResult;
    /// <summary>
    /// The contents of the resource. Can be either a TTextResourceContents or TBlobResourceContents.
    /// </summary>
    [NeonProperty('contents')] Contents: TJSONObject;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  /// <summary>
  /// Represents the parameters for a resources/subscribe request.
  /// </summary>
  TSubscribeParams = record
  public
    /// <summary>
    /// The URI of the resource to subscribe to.
    /// </summary>
    /// <remarks>The URI can use any protocol; it is up to the server how to interpret it.</remarks>
    [NeonProperty('uri')] URI: string;
  end;

  /// <summary>
  /// Represents the parameters for a resources/unsubscribe request.
  /// </summary>
  TUnsubscribeParams = record
  public
    /// <summary>
    /// The URI of the resource to unsubscribe from.
    /// </summary>
    [NeonProperty('uri')] URI: string;
  end;

  /// <summary>
  /// Represents the parameters for a resources/updated notification.
  /// </summary>
  TResourceUpdatedNotificationParams = record
  public
    /// <summary>
    /// The URI of the resource that has been updated.
    /// </summary>
    /// <remarks>This might be a sub-resource of the one that the client actually subscribed to.</remarks>
    [NeonProperty('uri')] URI: string;
  end;

implementation

{ TMCPResource }

constructor TMCPResource.Create;
begin
  Meta := TJSONObject.Create;
end;

destructor TMCPResource.Destroy;
begin
  Meta.Free;
  inherited;
end;

{ TMCPResourceTemplate }

constructor TMCPResourceTemplate.Create;
begin
  Meta := TJSONObject.Create;
end;

destructor TMCPResourceTemplate.Destroy;
begin
  Meta.Free;
  inherited;
end;

{ TReadResourceParams }

constructor TReadResourceParams.Create;
begin
  Arguments := TJSONObject.Create;
end;

destructor TReadResourceParams.Destroy;
begin
  Arguments.Free;
  inherited;
end;

{ TReadResourceResult }

constructor TReadResourceResult.Create;
begin
  Contents := TJSONObject.Create;
end;

destructor TReadResourceResult.Destroy;
begin
  Contents.Free;
  inherited;
end;

{ TListResourcesResult }

function TListResourcesResult.AddResource(const AName, AURI, AType: string): TMCPResource;
begin
  Result := TMCPResource.Create;
  Result.Name := AName;
  Result.URI := AURI;
  Result.MIMEType := AType;
  Resources.Add(Result);
end;

constructor TListResourcesResult.Create;
begin
  Resources := TMCPResources.Create;
end;

destructor TListResourcesResult.Destroy;
begin
  Resources.Free;
  inherited;
end;

{ TListResourceTemplatesResult }

constructor TListResourceTemplatesResult.Create;
begin
  ResourceTemplates := TMCPResourceTemplates.Create;
end;

destructor TListResourceTemplatesResult.Destroy;
begin
  ResourceTemplates.Free;
  inherited;
end;

end.
