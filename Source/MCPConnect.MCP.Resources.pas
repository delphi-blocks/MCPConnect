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
  System.SysUtils, System.Classes, System.Generics.Collections, System.JSON, System.Rtti,

  Neon.Core.Types,
  Neon.Core.Attributes,
  Neon.Core.Nullables,
  Neon.Core.Persistence,
  Neon.Core.Persistence.JSON,
  Neon.Core.Persistence.JSON.Schema,
  Neon.Core.Serializers.RTL,
  Neon.Core.Utils,


  MCPConnect.MCP.Attributes,
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
  TMCPResourceBase = class(TMetaClass)
    [NeonInclude(IncludeIf.NotEmpty)] Annotations: TMCPAnnotation;

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
    MimeType: NullString;

    /// <summary>
    ///   Optional set of sized icons that the client can display in a user interface
    /// </summary>
    [NeonInclude(IncludeIf.NotEmpty)] Icons: TIconList;
  end;


  TMimeEncoding = (Plain, Base64);

  /// <summary>
  /// Represents a known resource that the server is capable of reading.
  /// </summary>
  TMCPResource = class(TMCPResourceBase)
  public
    [NeonIgnore] FileName: string;
    [NeonIgnore] Classe: TClass;
    [NeonIgnore] Method: TRttiMethod;
    [NeonIgnore] Category: string;
    [NeonIgnore] Disabled: Boolean;
  public
    /// <summary>
    /// The URI of this resource.
    /// </summary>
    Uri: string;
  end;

  TMCPResources = class(TObjectList<TMCPResource>);
  TMCPResourceRegistry = class(TObjectDictionary<string, TMCPResource>);
  TMCPResourceConfigurator = reference to procedure(AResource: TMCPResource);
  TMCPResourceFilterFunc = reference to function (ATool: TMCPResource): Boolean;

  /// <summary>
  /// Represents a template description for resources available on the server.
  /// </summary>
  TMCPResourceTemplate = class(TMCPResourceBase)
  public
    /// <summary>
    /// A URI template (according to RFC 6570) that can be used to construct resource URIs.
    /// </summary>
    UriTemplate: NullString;
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
    //[NeonProperty('PaginatedRequest')] PaginatedRequest: TPaginatedRequest;
  end;

  /// <summary>
  /// The server's response to a resources/templates/list request from the client.
  /// </summary>
  TListResourceTemplatesResult = class(TMetaClass)
  public
    //[NeonProperty('PaginatedResult')] PaginatedResult: TPaginatedResult;

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

    function AddResource(const AName, AUriTemplate, AType: string): TMCPResourceTemplate;
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
    procedure AddTextContent(const AUri, AMime, AText: string);
    procedure AddBlobContent(const AUri, AMime, AText: string);
    procedure AddBase64Content(const AUri, AMime, ABase64: string);
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


  /// <summary>
  ///   MCP Resources List Generator
  /// </summary>
  TMCPResourcesListGenerator = class
  protected
    /// <summary>
    ///   Writer for a method's params
    /// </summary>
    procedure WriteParams(AMethod: TRttiMethod; AProps: TJSONObject; ARequired: TJSONArray);
  public
    /// <summary>
    ///   Serialize a Delphi method as a MCP Resource
    ///   The Delphi method must be marked with the MCP attributes
    /// </summary>
    class function MethodToResource(AMethod: TRttiMethod): TMCPResource;

    /// <summary>
    ///   Serialize a Delphi method as a MCP Resource Template
    ///   The Delphi method must be marked with the MCP attributes
    /// </summary>
    class function MethodToTemplate(AMethod: TRttiMethod): TMCPResourceTemplate;

    /// <summary>
    ///   Loops through the methods of a class/record and populate a structure
    ///   in response to the resources/list from a LLM client
    /// </summary>
    class procedure ListResources(AClass: TClass; AList: TListResourcesResult); overload;

    /// <summary>
    ///   Loops through the methods of a class/record and populate a structure
    ///   in response to the resources/templates/list from a LLM client
    /// </summary>
    class procedure ListTemplates(AClass: TClass; AList: TListResourceTemplatesResult); overload;
  end;



implementation

uses
  System.NetEncoding,
  MCPConnect.JRPC.Core;

{ TReadResourceResult }

procedure TReadResourceResult.AddContent(AContent: TResourceContents);
begin
  Contents.Add(AContent);
end;

procedure TReadResourceResult.AddBase64Content(const AUri, AMime, ABase64: string);
begin
  var blob := TBlobResourceContents.Create;
  blob.Uri := AUri;
  blob.MimeType := AMime;
  blob.Blob := ABase64;
  Contents.Add(blob);
end;

procedure TReadResourceResult.AddBlobContent(const AUri, AMime, AText: string);
begin
  var blob := TBlobResourceContents.Create;
  blob.Uri := AUri;
  blob.MimeType := AMime;
  blob.Blob := TNetEncoding.Base64String.Encode(AText);
  Contents.Add(blob);
end;

procedure TReadResourceResult.AddTextContent(const AUri, AMime, AText: string);
begin
  var text := TTextResourceContents.Create;
  text.Uri := AUri;
  text.MimeType := AMime;
  text.text := AText;
  Contents.Add(text);
end;

constructor TReadResourceResult.Create;
begin
  inherited;
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
  Resources := TMCPResources.Create(False);
end;

destructor TListResourcesResult.Destroy;
begin
  Resources.Free;
  inherited;
end;

{ TListResourceTemplatesResult }

function TListResourceTemplatesResult.AddResource(const AName, AUriTemplate, AType: string): TMCPResourceTemplate;
begin
  Result := TMCPResourceTemplate.Create;
  Result.Name := AName;
  Result.UriTemplate := AUriTemplate;
  Result.MIMEType := AType;
  ResourceTemplates.Add(Result);
end;

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

{ TMCPResourcesListGenerator }

class procedure TMCPResourcesListGenerator.ListResources(AClass: TClass; AList: TListResourcesResult);
var
  LMethod: TRttiMethod;
  LMethods: TArray<TRttiMethod>;
  LResource: TMCPResource;
begin
  LMethods := TRttiUtils.Context.GetType(AClass).GetMethods;
  for LMethod in LMethods do
    if Assigned(LMethod.GetAttribute(MCPResourceAttribute)) then
    begin
      LResource := MethodToResource(LMethod);

      AList.Resources.Add(LResource);
    end;
end;

class procedure TMCPResourcesListGenerator.ListTemplates(AClass: TClass; AList: TListResourceTemplatesResult);
var
  LMethod: TRttiMethod;
  LMethods: TArray<TRttiMethod>;
  LTemplate: TMCPResourceTemplate;
begin
  LMethods := TRttiUtils.Context.GetType(AClass).GetMethods;
  for LMethod in LMethods do
    if Assigned(LMethod.GetAttribute(MCPTemplateAttribute)) then
    begin
      LTemplate := MethodToTemplate(LMethod);
      AList.ResourceTemplates.Add(LTemplate);
    end;
end;

class function TMCPResourcesListGenerator.MethodToResource(AMethod: TRttiMethod): TMCPResource;
var
  LAttr: MCPResourceAttribute;
begin
  LAttr := AMethod.GetAttribute<MCPResourceAttribute>;
  if not Assigned(LAttr) then
    raise EMCPException.CreateFmt('Attribute [MCPResource] not found in method %s', [AMethod.Name]);

  Result := TMCPResource.Create;
  try
    Result.Name := LAttr.Name;
    Result.Uri := LAttr.Uri;
    Result.MIMEType := LAttr.MimeType;
    Result.Description := LAttr.Description;
  except
    Result.Free;
    raise;
  end;
end;

class function TMCPResourcesListGenerator.MethodToTemplate(AMethod: TRttiMethod): TMCPResourceTemplate;
var
  LAttr: MCPTemplateAttribute;
begin
  LAttr := AMethod.GetAttribute<MCPTemplateAttribute>;
  if not Assigned(LAttr) then
    raise EMCPException.CreateFmt('Attribute [MCPTemplate] not found in method %s', [AMethod.Name]);

  // Controllo UriTemplate e parametri?

  Result := TMCPResourceTemplate.Create;
  try
    Result.Name := LAttr.Name;
    Result.UriTemplate := LAttr.UriTemplate;
    Result.MIMEType := LAttr.MimeType;
    Result.Description := LAttr.Description;
  except
    Result.Free;
    raise;
  end;
end;

procedure TMCPResourcesListGenerator.WriteParams(AMethod: TRttiMethod; AProps: TJSONObject; ARequired: TJSONArray);
var
  LJSONObj: TJSONObject;
  LParam: TRttiParameter;
  LAttr: MCPParamAttribute;
begin
  for LParam in AMethod.GetParameters do
  begin
    LAttr := LParam.GetAttribute<MCPParamAttribute>;
      if not Assigned(LAttr) then
        raise EJRPCException.Create('Non-annotated params are not permitted');

    LJSONObj := TNeonSchemaGenerator.TypeToJSONSchema(LParam.ParamType, MCPNeonConfig);

    LJSONObj.AddPair('description', TJSONString.Create(LAttr.Description));
    AProps.AddPair(LAttr.Name, LJSONObj);
    ARequired.Add(LAttr.Name);
  end;
end;

end.
