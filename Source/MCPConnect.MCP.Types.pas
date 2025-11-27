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
unit MCPConnect.MCP.Types;

interface

{$SCOPEDENUMS ON}

uses
  System.SysUtils, System.Classes, System.Generics.Collections, System.JSON, System.Rtti,

  Neon.Core.Types,
  Neon.Core.Attributes,
  Neon.Core.Nullables,
  Neon.Core.Persistence,
  Neon.Core.Persistence.JSON,
  Neon.Core.Serializers.RTL, MCPConnect.Core.Utils;

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

    /// <summary>
    ///   An opaque token representing the current pagination position. If provided, the server
    ///   should return results starting after this cursor.
    /// </summary>
    Cursor: NullString;
  end;

  /// <summary>
  ///   Optional annotations for the client. The client can use annotations to inform how objects
  ///   are used or displayed <br />
  /// </summary>
  TAnnotations = class

    /// <summary>
    ///   Describes who the intended customer of this object or data is.
    /// </summary>
    /// <remarks>
    ///   It can include multiple entries to indicate content useful for multiple audiences (e.g.,
    ///   `["user", "assistant"]`).
    /// </remarks>
    [NeonInclude(IncludeIf.NotEmpty)] Audience: TArray<string>;

    // Examples: last activity timestamp in an open file, timestamp when the resource was attached, etc.
    /// <summary>
    ///   The moment the resource was last modified, as an ISO 8601 formatted string.
    /// </summary>
    /// <example>
    ///   Last activity timestamp in an open file, timestamp when the resource was attached, etc.
    /// </example>
    [NeonInclude(IncludeIf.NotEmpty)] LastModified: TDateTime;

    /// <summary>
    ///   Describes how important this data is for operating the server. A value of 1 means "most
    ///   important," and indicates that the data is effectively required, while 0 means "least
    ///   important," and indicates that the data is entirely optional.
    /// </summary>
    [NeonInclude(IncludeIf.NotDefault)] Priority: Currency;
  end;


  /// <summary>
  ///   TImplementation describes the name and version of an MCP implementation.
  /// </summary>
  TImplementation = class

    /// <summary>
    ///   Name is the name of the implementation.
    /// </summary>
    Name: string;

    /// <summary>
    ///   Version is the version of the implementation.
    /// </summary>
    Version: string;
  end;

  /// <summary>
  ///   TRootsCapability is present if the client supports listing roots.
  /// </summary>
  TRootsCapability = record

    /// <summary>
    ///   Whether the client supports notifications for changes to the roots list.
    /// </summary>
    ListChanged: NullBoolean;
  end;

  /// <summary>
  ///   TClientCapabilities represents capabilities a client may support.
  /// </summary>
  TClientCapabilities = class

    /// <summary>
    ///   Experimental, non-standard capabilities that the client supports.
    /// </summary>
    [NeonProperty('experimental'), NeonInclude(IncludeIf.NotEmpty)] &Experimental: TJSONObject;

    /// <summary>
    ///   Present if the client supports listing roots.
    /// </summary>
    [NeonInclude(IncludeIf.NotEmpty)] Roots: TRootsCapability;

    /// <summary>
    ///   Present if the client supports sampling from an LLM.
    /// </summary>
    [NeonInclude(IncludeIf.NotEmpty)] Sampling: TJSONObject;

  public
    constructor Create;
    destructor Destroy; override;
  end;


  /// <summary>
  /// T  ServerCapabilities represents capabilities that a server may support.
  /// </summary>
  TServerCapabilities = class

    /// <summary>
    /// T  PromptsCapability is present if the server offers any prompt templates.
    /// </summary>
    public type TPromptsCapability = record

      /// <summary>
      ///   Whether this server supports notifications for changes to the prompt list.
      /// </summary>
      ListChanged: NullBoolean;
    end;

    /// <summary>
    /// TResourcesCapability is present if the server offers any resources to read.
    /// </summary>
    public type TResourcesCapability = record

      /// <summary>
      ///   Whether this server supports subscribing to resource updates.
      /// </summary>
      Subscribe: NullBoolean;

      /// <summary>
      ///   Whether this server supports notifications for changes to the resource list.
      /// </summary>
      ListChanged: NullBoolean;
    end;

    /// <summary>
    /// TToolsCapability is present if the server offers any tools to call.
    /// </summary>
    public type TToolsCapability = record

      /// <summary>
      ///   Whether this server supports notifications for changes to the tool list.
      /// </summary>
      ListChanged: NullBoolean;
    end;
  public

    /// <summary>
    ///   Experimental, non-standard capabilities that the server supports.
    /// </summary>
    [NeonProperty('experimental'), NeonInclude(IncludeIf.NotEmpty)] &Experimental: TJSONObject;

    /// <summary>
    ///   Present if the server supports sending log messages to the client.
    /// </summary>
    [NeonInclude(IncludeIf.NotEmpty)] Logging: TJSONObject;

    /// <summary>
    ///   Present if the server offers any prompt templates.
    /// </summary>
    [NeonInclude(IncludeIf.NotEmpty)] Prompts: TPromptsCapability;

    /// <summary>
    ///   Present if the server offers any resources to read.
    /// </summary>
    [NeonInclude(IncludeIf.NotEmpty)] Resources: TResourcesCapability;

    /// <summary>
    ///   Present if the server supports sending sampling requests to clients.
    /// </summary>
    [NeonInclude(IncludeIf.NotEmpty)] Sampling: TJSONObject;

    /// <summary>
    ///   Present if the server offers any tools to call.
    /// </summary>
    [NeonInclude(IncludeIf.NotEmpty)] Tools: TToolsCapability;

  public
    constructor Create;
    destructor Destroy; override;
  end;

  /// <summary>
  ///   TInitializeParams is sent from the client to the server when it first connects.
  /// </summary>
  TInitializeParams = class

    /// <summary>
    /// T  he latest version of the Model Context Protocol that the client supports.
    /// </summary>
    ProtocolVersion: string;

    /// <summary>
    ///   Client capabilities.
    /// </summary>
    Capabilities: TClientCapabilities;

    /// <summary>
    ///   Client implementation information.
    /// </summary>
    ClientInfo: TImplementation;

  public
    constructor Create;
    destructor Destroy; override;
  end;

  /// <summary>
  ///   TInitializeResult is sent after receiving an initialize request from the
  ///   client.
  /// </summary>
  TInitializeResult = class

    /// <summary>
    ///   The version of the Model Context Protocol that the server wants to use.
    /// </summary>
    ProtocolVersion: string;

    /// <summary>
    ///   Server capabilities.
    /// </summary>
    Capabilities: TServerCapabilities;

    /// <summary>
    ///   Server implementation information.
    /// </summary>
    ServerInfo: TImplementation;

    /// <summary>
    ///   Instructions describing how to use the server and its features.
    /// </summary>
    [NeonInclude(IncludeIf.NotEmpty)] Instructions: NullString;

  public
    constructor Create;
    destructor Destroy; override;

    function ToJSON(APrettyPrint: Boolean = False): string;
  end;

  /// <summary>
  ///   TInitializedNotification is sent from the client to the server after
  ///   initialization has finished.
  /// </summary>
  TInitializedNotificationParams = class(TMetaClass)
  end;

  /// <summary>
  ///   To cancel an in-progress request, send a notifications/cancelled:<br />
  ///   *The ID of the request to cancel <br />
  ///   *An optional reason string that can be logged or displayed
  /// </summary>
  TCancelledNotificationParams = class

    /// <summary>
    ///   A uniquely identifying ID for a request in JSON-RPC.
    /// </summary>
    RequestId: Integer;

    /// <summary>
    ///   An optional string describing the reason for the cancellation. This MAY be logged or
    ///   presented to the user.
    /// </summary>
    Reason: NullString;
  end;

  { ************ Contents ************ }

  /// <summary>
  ///   Base class for the content(s)
  /// </summary>
  TBaseContent = class(TMetaClass)
  public

    /// <summary>
    ///   Optional annotations for the client. The client can use annotations to inform how objects
    ///   are used or displayed
    /// </summary>
    [NeonInclude(IncludeIf.NotEmpty)] Annotations: TAnnotations;

    /// <summary>
    ///   Content type: can be text, image, audio, etc...
    /// </summary>
    [NeonProperty('type')] &Type: string;
  public
    constructor Create;
    destructor Destroy; override;

    function DataFromStream(AStream: TStream): string;
  end;

  /// <summary>
  ///   Text provided to or from an LLM.
  /// </summary>
  TTextContent = class(TBaseContent)
  public

    /// <summary>
    ///   The text content of the message.
    /// </summary>
    Text: string;
  public
    constructor Create(const AText: string = ''); overload;
  end;

  /// <summary>
  ///   An image provided to or from an LLM.
  /// </summary>
  TImageContent = class(TBaseContent)
  public

    /// <summary>
    ///   The base64-encoded image data.
    /// </summary>
    /// <value>
    ///   Format: byte
    /// </value>
    Data: string;

    /// <summary>
    ///   The MIME type of the image. Different providers may support different image types.
    /// </summary>
    MimeType: string;

    constructor Create; overload;
  end;

  /// <summary>
  ///   Audio provided to or from an LLM.
  /// </summary>
  TAudioContent = class(TBaseContent)
  public

    /// <summary>
    ///   The base64-encoded audio data.
    /// </summary>
    /// <value>
    ///   Format: byte
    /// </value>
    Data: string;

    /// <summary>
    ///   The MIME type of the audio. Different providers may support different audio types.
    /// </summary>
    MimeType: string;

  public
    constructor Create;
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

    /// <summary>
    ///   The URI of this resource.
    /// </summary>
    /// <value>
    ///   Format: uri
    /// </value>
    Uri: string;

    /// <summary>
    ///   Intended for programmatic or logical use, but used as a display name in past specs or
    ///   fallback (if title isn't present).
    /// </summary>
    Name: string;

    /// <summary>
    ///   The size of the raw resource content, in bytes (i.e., before base64 encoding or any
    ///   tokenization), if known.
    /// </summary>
    /// <remarks>
    ///   This can be used by Hosts to display file sizes and estimate context window usage.
    /// </remarks>
    [NeonInclude(IncludeIf.NotEmpty)] Size: Integer;

    /// <summary>
    ///   A description of what this resource represents. This can be used by clients to improve the
    ///   LLM's understanding of available resources. It can be thought of like a "hint" to the
    ///   model.
    /// </summary>
    [NeonInclude(IncludeIf.NotEmpty)] Description: string;

    /// <summary>
    ///   Intended for UI and end-user contexts, optimized to be human-readable and easily
    ///   understood, even by those unfamiliar with domain-specific terminology.
    /// </summary>
    [NeonInclude(IncludeIf.NotEmpty)] Title: string;

    /// <summary>
    ///   The MIME type of this resource, if known.
    /// </summary>
    [NeonInclude(IncludeIf.NotEmpty)] MimeType: string;

    constructor Create;
  end;

  /// <summary>
  ///   The contents of a specific resource or sub-resource.
  /// </summary>
  TResourceContents = class(TMetaClass)
  public

    /// <summary>
    /// The URI of this resource.
    /// </summary>
    Uri: string;

    /// <summary>
    /// The MIME type of this resource, if known.
    /// </summary>
    MimeType: NullString;
  end;

  /// <summary>
  /// Represents a text-based resource.
  /// </summary>
  TTextResourceContents = class(TResourceContents)
  public

    /// <summary>
    ///   The text of the item.
    /// </summary>
    /// <remarks>
    ///   This must only be set if the item can actually be represented as text (not binary data).
    /// </remarks>
    Text: string;
  end;

  /// <summary>
  /// Represents a binary-based resource.
  /// </summary>
  TBlobResourceContents = class(TResourceContents)
  public

    /// <summary>
    ///   A base64-encoded string representing the binary data of the item.
    /// </summary>
    /// <value>
    ///   Format: byte
    /// </value>
    Blob: string;
  end;

  /// <summary>
  ///   The contents of a resource, embedded into a prompt or tool call result. It is up to the
  ///   client how best to render embedded resources for the benefit of the LLM and/or the user.
  /// </summary>
  /// <remarks>
  ///   Defaults to TBlobResourceContents
  /// </remarks>
  TEmbeddedResourceBlob = class(TBaseContent)
  public

    /// <summary>
    ///   The embedded (blob) resource
    /// </summary>
    Resource: TBlobResourceContents;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  /// <summary>
  ///   The contents of a resource, embedded into a prompt or tool call result. It is up to the
  ///   client how best to render embedded resources for the benefit of the LLM and/or the user.
  /// </summary>
  /// <remarks>
  ///   Defaults to TTextResourceContents
  /// </remarks>
  TEmbeddedResourceText = class(TBaseContent)
  public

    /// <summary>
    ///   The embedded (blob) resource
    /// </summary>
    Resource: TTextResourceContents;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  /// <summary>
  ///   List of Contents. Used in CallToolResult
  /// </summary>
  TContentList = class(TObjectList<TBaseContent>)
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
   Content: TBaseContent;

  public
    constructor Create;
    destructor Destroy; override;

    procedure SetContent(AContent: TBaseContent);

    function GetContentAsText: TTextContent;
    function GetContentAsImage: TImageContent;
    function GetContentAsAudio: TAudioContent;
    function GetContentAsResource: TResourceLink;
    function GetContentAsEmbedded: TEmbeddedResourceBlob;
  end;

  /// <summary>
  ///   Interface for the CallToolResult array
  /// </summary>
  IToolResultBuilder = interface
  ['{62895467-E05D-4B86-A26D-E057F3684267}']
    function AddText(const AText: string): IToolResultBuilder;
    function AddImage(const AMime: string; AImage: TStream): IToolResultBuilder;
    function AddAudio(const AMime: string; AAudio: TStream): IToolResultBuilder;
    function AddLink(const AMime, AURL, ADescription: string): IToolResultBuilder;
    function AddBlob(const AMime: string; ABlob: TStream): IToolResultBuilder;

    function Build(): TContentList;
  end;

  /// <summary>
  ///   Implementation for the CallToolResult array. To be called inside a tool method.
  /// </summary>
  TToolResultBuilder = class(TInterfacedObject, IToolResultBuilder)
  private
    FContents: TContentList;
    FOwnItems: Boolean;
  public
    class function CreateInstance: IToolResultBuilder; static;
  public
    constructor Create;
    destructor Destroy; override;

    function AddText(const AText: string): IToolResultBuilder;
    function AddImage(const AMime: string; AImage: TStream): IToolResultBuilder;
    function AddAudio(const AMime: string; AAudio: TStream): IToolResultBuilder;
    function AddLink(const AMime, AUri, ADescription: string): IToolResultBuilder;
    function AddBlob(const AMime: string; ABlob: TStream): IToolResultBuilder;

    function Build(): TContentList;
  end;

function MCPNeonConfig: INeonConfiguration;


implementation

uses
  System.NetEncoding;


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

function TBaseContent.DataFromStream(AStream: TStream): string;
var
  LBase64 :TBase64Encoding;
  LData :TStringStream;
begin
  Result := '';
  LBase64 := TBase64Encoding.Create;
  LData := TStringStream.Create;
  try
    LBase64.Encode(AStream, LData);
    Result := LData.DataString;
  finally
    LBase64.Free;
    LData.Free;
  end;

end;

destructor TBaseContent.Destroy;
begin
  Annotations.Free;
  inherited;
end;

{ TEmbeddedResourceBlob }

constructor TEmbeddedResourceBlob.Create;
begin
  inherited Create;
  &Type := 'resource';
  Resource := TBlobResourceContents.Create;
end;

destructor TEmbeddedResourceBlob.Destroy;
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

function TContentClass.GetContentAsEmbedded: TEmbeddedResourceBlob;
begin
  if not (Content is TEmbeddedResourceBlob) then
  begin
    Content.Free;
    Content := TEmbeddedResourceBlob.Create;
  end;

  Result := Content as TEmbeddedResourceBlob;
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
  &Type := 'text';
  Text := AText;
end;

{ TToolResultBuilder }

function TToolResultBuilder.AddAudio(const AMime: string; AAudio: TStream): IToolResultBuilder;
var
  LContent: TAudioContent;
begin
  LContent := TAudioContent.Create;
  LContent.Data := LContent.DataFromStream(AAudio);
  LContent.MimeType := AMime;
  FContents.Add(LContent);
  Result := Self;
end;

function TToolResultBuilder.AddBlob(const AMime: string; ABlob: TStream): IToolResultBuilder;
var
  LBlob: TBlobResourceContents;
  LContent: TEmbeddedResourceBlob;
begin
  LContent := TEmbeddedResourceBlob.Create;
  LBlob := LContent.Resource as TBlobResourceContents;
  LBlob.MimeType := AMime;
  LBlob.Blob := LContent.DataFromStream(ABlob);
  FContents.Add(LContent);
  Result := Self;
end;

function TToolResultBuilder.AddImage(const AMime: string; AImage: TStream): IToolResultBuilder;
var
  LContent: TImageContent;
begin
  LContent := TImageContent.Create;
  LContent.Data := LContent.DataFromStream(AImage);
  LContent.MimeType := AMime;
  FContents.Add(LContent);
  Result := Self;
end;

function TToolResultBuilder.AddLink(const AMime, AUri, ADescription: string): IToolResultBuilder;
begin
  var LResource := TResourceLink.Create;
  LResource.URI := AUri;
  LResource.MimeType := AMime;
  LResource.Description := ADescription;
  FContents.Add(LResource);
  Result := Self;
end;

function TToolResultBuilder.AddText(const AText: string): IToolResultBuilder;
begin
  FContents.Add(TTextContent.Create(AText));
  Result := Self;
end;

function TToolResultBuilder.Build: TContentList;
begin
  Result := TContentList.Create;
  for var LItem in FContents do
  begin
    Result.Add(LItem);
  end;
  FOwnItems := False;
end;

constructor TToolResultBuilder.Create;
begin
  FContents := TContentList.Create(False);
  FOwnItems := True;
end;

destructor TToolResultBuilder.Destroy;
var
  LItem: TBaseContent;
begin
  if FOwnItems then
  begin
    for LItem in FContents do
      LItem.Free;
  end;

  FContents.Free;
  inherited;
end;

class function TToolResultBuilder.CreateInstance: IToolResultBuilder;
begin
  Result := TToolResultBuilder.Create;
end;

{ TImageContent }

constructor TImageContent.Create;
begin
  inherited Create;
  &Type := 'image';
end;

{ TAudioContent }

constructor TAudioContent.Create;
begin
  inherited Create;
  &Type := 'audio';
end;

{ TResourceLink }

constructor TResourceLink.Create;
begin
  inherited Create;
  &Type := 'resource_link';
end;

{ TEmbeddedResourceText }

constructor TEmbeddedResourceText.Create;
begin
  inherited Create;
  &Type := 'resource';
  Resource := TTextResourceContents.Create;
end;

destructor TEmbeddedResourceText.Destroy;
begin
  Resource.Free;
  inherited;
end;

end.
