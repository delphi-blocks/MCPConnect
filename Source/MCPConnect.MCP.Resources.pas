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
    [NeonInclude(IncludeIf.NotEmpty)] Audience: TArray<string>;


    /// <summary>
    ///   The moment the resource was last modified, as an ISO 8601 formatted string <br />
    /// </summary>
    /// <example>
    ///   Last activity timestamp in an open file, timestamp when the resource was attached, etc.
    /// </example>
    LastModified: NullDateTime;

    // Describes how important this data is for operating the server.
    //
    // A value of 1 means "most important," and indicates that the data is
    // effectively required, while 0 means "least important," and indicates that
    // the data is entirely optional.
    Priority: Nullable<Currency>;
  end;

  /// <summary>
  /// Represents a known resource that the server is capable of reading.
  /// </summary>
  TMCPResource = class(TMetaClass)
  public
    [NeonInclude(IncludeIf.NotEmpty)] Annotations: TMCPAnnotation;

    /// <summary>
    /// The URI of this resource.
    /// </summary>
    Uri: string;

    /// <summary>
    /// A human-readable name for this resource.
    /// </summary>
    /// <remarks>This can be used by clients to populate UI elements.</remarks>
    Name: string;

    /// <summary>
    /// A description of what this resource represents.
    /// </summary>
    /// <remarks>This can be used by clients to improve the LLM's understanding of available resources. It can be thought of like a 'hint' to the model.</remarks>
    Description: NullString;

    /// <summary>
    /// The MIME type of this resource, if known.
    /// </summary>
    MIMEType: NullString;

    /// <summary>
    ///   Optional set of sized icons that the client can display in a user interface
    /// </summary>
    [NeonInclude(IncludeIf.NotEmpty)] Icons: TIconList;
  end;

  TMCPResources = class(TObjectList<TMCPResource>);


  /// <summary>
  /// Represents a template description for resources available on the server.
  /// </summary>
  TMCPResourceTemplate = class(TMetaClass)
  public
    [NeonInclude(IncludeIf.NotEmpty)] Annotations: TMCPAnnotation;

    /// <summary>
    /// A URI template (according to RFC 6570) that can be used to construct resource URIs.
    /// </summary>
    URITemplate: NullString;

    /// <summary>
    /// A human-readable name for the type of resource this template refers to.
    /// </summary>
    /// <remarks>This can be used by clients to populate UI elements.</remarks>
    Name: string;

    /// <summary>
    /// A description of what this template is for.
    /// </summary>
    /// <remarks>This can be used by clients to improve the LLM's understanding of available resources. It can be thought of like a 'hint' to the model.</remarks>
    Description: NullString;

    /// <summary>
    /// The MIME type for all resources that match this template.
    /// </summary>
    /// <remarks>This should only be included if all resources matching this template have the same type.</remarks>
    MIMEType: NullString;

    /// <summary>
    ///   Optional set of sized icons that the client can display in a user interface
    /// </summary>
    [NeonInclude(IncludeIf.NotEmpty)] Icons: TIconList;
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
  TListResourcesResult = class(TMetaClass)
  public
    //[NeonProperty('PaginatedResult')] PaginatedResult: TPaginatedResult;
    /// <summary>
    /// A list of available resources.
    /// </summary>
    Resources: TMCPResources;

    /// <summary>
    ///   An opaque token representing the pagination position after the last returned result. If present, there may be more results available
    /// </summary>
    NextCursor: NullString;

  public
    constructor Create;
    destructor Destroy; override;

    function AddResource(const AName, AUri, AType: string): TMCPResource;
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
  TListResourceTemplatesResult = class(TMetaClass)
  public
    [NeonProperty('PaginatedResult')] PaginatedResult: TPaginatedResult;
    /// <summary>
    /// A list of available resource templates.
    /// </summary>
    ResourceTemplates: TMCPResourceTemplates;

    /// <summary>
    ///   An opaque token representing the pagination position after the last returned result. If present, there may be more results available
    /// </summary>
    NextCursor: NullString;

  public
    constructor Create;
    destructor Destroy; override;
  end;

  /// <summary>
  /// Represents the parameters for a resources/read request.
  /// </summary>
  TReadResourceParams = class(TMetaClass)
  public
    /// <summary>
    /// The URI of the resource to read.
    /// </summary>
    /// <remarks>The URI can use any protocol; it is up to the server how to interpret it.</remarks>
    Uri: string;
  end;

  /// <summary>
  /// The server's response to a resources/read request from the client.
  /// </summary>
  TReadResourceResult = class(TMetaClass)
  public
    /// <summary>
    /// The contents of the resource. Can be either a TTextResourceContents or TBlobResourceContents.
    /// </summary>
    Contents: TResourceContentsList;
  public
    constructor Create;
    destructor Destroy; override;


    procedure AddContent(AContent: TResourceContents);
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
    Uri: string;
  end;

  /// <summary>
  /// Represents the parameters for a resources/unsubscribe request.
  /// </summary>
  TUnsubscribeParams = record
  public
    /// <summary>
    /// The URI of the resource to unsubscribe from.
    /// </summary>
    Uri: string;
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
    Uri: string;
  end;

implementation

{ TReadResourceResult }

procedure TReadResourceResult.AddContent(AContent: TResourceContents);
begin
  Contents.Add(AContent);
end;

constructor TReadResourceResult.Create;
begin
  Contents := TResourceContentsList.Create;
end;

destructor TReadResourceResult.Destroy;
begin
  Contents.Free;
  inherited;
end;

{ TListResourcesResult }

function TListResourcesResult.AddResource(const AName, AUri, AType: string): TMCPResource;
begin
  Result := TMCPResource.Create;
  Result.Name := AName;
  Result.Uri := AUri;
  Result.MIMEType := AType;
  Resources.Add(Result);
end;

constructor TListResourcesResult.Create;
begin
  inherited;
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
  inherited;
  ResourceTemplates := TMCPResourceTemplates.Create;
end;

destructor TListResourceTemplatesResult.Destroy;
begin
  ResourceTemplates.Free;
  inherited;
end;

end.
