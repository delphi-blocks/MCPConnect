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
  Neon.Core.Serializers.RTL,
  MCPConnect.Core.Utils;

type
  EMCPException = class(Exception);

  TMCPURIBuilder = class
  end;


  TStringPair = TPair<string, string>;
  TStringMap = TArray<TStringPair>;

  TAnyMap = class(TDictionary<string, TValue>);

  TAnyMapOwned = class(TDictionary<string, TValue>)
  public
    destructor Destroy; override;
  end;

  TMetaClass = class
  public
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
  ///   are used or displayed
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

    /// <summary>
    ///   The moment the resource was last modified, as an ISO 8601 formatted string.
    /// </summary>
    /// <example>
    ///   Last activity timestamp in an open file, timestamp when the resource was attached, etc.
    /// </example>
    LastModified: NullDateTime;

    /// <summary>
    ///   Describes how important this data is for operating the server. A value of 1 means "most
    ///   important," and indicates that the data is effectively required, while 0 means "least
    ///   important," and indicates that the data is entirely optional.
    /// </summary>
    Priority: Nullable<Currency>;
  end;

  /// <summary>
  ///   An optionally-sized icon that can be displayed in a user interface
  /// </summary>
  TMCPIcon = record
  private
    function ReadMWord(AStream: TFileStream): Word;
    procedure GetPNGSize(const AFileName: string; var AWidth, AHeight: Word);
    function GetPNGSizeString(const AFileName: string): string;
  public

    /// <summary>
    ///   A standard URI pointing to an icon resource. May be an HTTP/HTTPS URL or a data:
    ///   URI with Base64-encoded image data. <br />
    ///   <br />
    ///   Consumers SHOULD takes steps to ensure URLs serving icons are from the same domain as the
    ///   client/server or a trusted domain.
    /// </summary>
    /// <remarks>
    ///   Format: uri
    /// </remarks>
    Src: string;

    /// <summary>
    ///   Optional MIME type override if the source MIME type is missing or generic.
    /// </summary>
    /// <example>
    ///   "image/png", "image/jpeg", or "image/svg+xml"
    /// </example>
    MimeType: NullString;

    /// <summary>
    ///   Optional array of strings that specify sizes at which the icon can be used. Each string
    ///   should be in WxH format (e.g., "48x48", "96x96") or "any" for scalable formats like SVG
    /// </summary>
    /// <remarks>
    ///   If not provided, the client should assume that the icon can be used at any size
    /// </remarks>
    [NeonInclude(IncludeIf.NotEmpty)] Sizes: TArray<string>;

    /// <summary>
    ///   Optional specifier for the theme this icon is designed for. light indicates the icon is
    ///   designed to be used with a light background, and dark indicates the icon is designed to be
    ///   used with a dark background. If not provided, the client should assume the icon can be
    ///   used with any theme
    /// </summary>
    /// <remarks>
    ///   Valid values: dark, light
    /// </remarks>
    Theme: NullString;


    procedure FromFile(const AIcon: string);
  end;

  TMCPIconList = TArray<TMCPIcon>;


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

    /// <summary>
    ///   An optional human-readable description of what this implementation does. This can be used
    ///   by clients or servers to provide context about their purpose and capabilities. For
    ///   example, a server might describe the types of resources or tools it provides, while a
    ///   client might describe its intended use case.
    /// </summary>
    Description: NullString;

    /// <summary>
    ///   Intended for UI and end-user contexts — optimized to be human-readable and easily
    ///   understood, even by those unfamiliar with domain-specific terminology
    /// </summary>
    /// <remarks>
    ///   If not provided, the name should be used for display (except for Tool, where
    ///   annotations.title should be given precedence over using name, if present)
    /// </remarks>
    Title: NullString;

    /// <summary>
    ///   An optional URL of the website for this implementation
    /// </summary>
    /// <remarks>
    ///   Format: uri
    /// </remarks>
    WebsiteUrl: NullString;

    /// <summary>
    ///   Optional set of sized icons that the client can display in a user interface
    /// </summary>
    [NeonInclude(IncludeIf.NotEmpty)] Icons: TMCPIconList;
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
    ///   Present if the server supports argument autocompletion suggestions
    /// </summary>
    [NeonInclude(IncludeIf.NotEmpty)] Completions: TJSONObject;

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
    ///   Capabilities that a server may support. Known capabilities are defined here, in this
    ///   schema, but this is not a closed set: any server can define its own, additional
    ///   capabilities.
    /// </summary>
    Capabilities: TServerCapabilities;

    /// <summary>
    ///   Server implementation information.
    /// </summary>
    ServerInfo: TImplementation;

    /// <summary>
    ///   Instructions describing how to use the server and its features.
    ///   This can be used by clients to improve the LLM's understanding of available
    ///   tools, resources, etc. It can be thought of like a "hint" to the model.
    ///   For example, this information MAY be added to the system prompt.
    /// </summary>
    Instructions: NullString;

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


  TLogSetLevel = (Alert, Critical, Debug, Emergency, Error, Info, Notice, Warning);
  TSetLevelRequestParams = class(TMetaClass)

    /// <summary>
    ///   The severity of a log message. These map to syslog message severities, as specified in
    ///   RFC-5424: <see href="https://datatracker.ietf.org/doc/html/rfc5424#section-6.2.1" />
    /// </summary>
    Level: TLogSetLevel;
  end;

  TSetLevelResult = class
    
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
  public
    constructor Create;
    destructor Destroy; override;

    function DataFromStream(AStream: TStream): string;
  end;

  TToolContent = class(TBaseContent)
  public
    /// <summary>
    ///   Content type: can be text, image, audio, etc...
    /// </summary>
    [NeonProperty('type')] &Type: string;
  end;

  /// <summary>
  ///   Text provided to or from an LLM.
  /// </summary>
  TTextContent = class(TToolContent)
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
  TImageContent = class(TToolContent)
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
  TAudioContent = class(TToolContent)
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
  TResourceLink = class(TToolContent)
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
  ///   The contents of a specific resource or sub-resource (TextResourceContents, BlobResourceContents)
  /// </summary>
  TResourceContents = class(TBaseContent)
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
  ///   List of Resource Contents. Used in ReadResourceResult
  /// </summary>
  TResourceContentsList = class(TObjectList<TResourceContents>);

  /// <summary>
  ///   The contents of a resource, embedded into a prompt or tool call result. It is up to the
  ///   client how best to render embedded resources for the benefit of the LLM and/or the user.
  /// </summary>
  /// <remarks>
  ///   Defaults to TBlobResourceContents
  /// </remarks>
  TEmbeddedResourceBlob = class(TToolContent)
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
  TEmbeddedResourceText = class(TToolContent)
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
  TContentList = class(TObjectList<TToolContent>);

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


  TMimeList = (Standard, Complete);
  TMimeEncoding = (Plain, Base64);
  TMCPMimeTypes = class
  private type
    TMimeInfo = record
      Ext: string;
      Mime: string;
      Encoding: TMimeEncoding;
    end;
  private
    FList: TList<TMimeInfo>;
    function ExtExists(const AExtension, AList: string): Boolean;
  public
    constructor Create(AList: TMimeList = TMimeList.Standard);
    destructor Destroy; override;

    procedure SetStandard;
    procedure SetComplete;

    procedure AddMime(AEncoding: TMimeEncoding; const AMime: string; const AExt: string = '');
    function MediaByExtension(const AExtension: string): string;
    function EncodingByMedia(const AMime: string): Nullable<TMimeEncoding>;
    function DataUri(const AFileName: string): string; overload;
    function DataUri(const AFileName, AMimeType: string): string; overload;

    function Count: NativeInt;
  end;

function MCPNeonConfig: INeonConfiguration;


implementation

uses
  system.IOUtils,
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
  Completions := TJSONObject.Create;
  &Experimental := TJSONObject.Create;
  Logging := TJSONObject.Create;
  Sampling := TJSONObject.Create;
end;

destructor TServerCapabilities.Destroy;
begin
  Sampling.Free;
  Logging.Free;
  &Experimental.Free;
  Completions.Free;
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

{ TMCPIcon }

procedure TMCPIcon.FromFile(const AIcon: string);
begin

  var mm := TMCPMimeTypes.Create;
  try
    var ext := TPath.GetExtension(AIcon);
    MimeType := mm.MediaByExtension(ext);
    if MimeType.Value.IsEmpty then
      raise EMCPException.CreateFmt('MediaType for extension [%s] not found', [ext]);

    Src := mm.DataUri(AIcon, MimeType);
    if MimeType = 'image/png' then
      Sizes := Sizes + [GetPNGSizeString(AIcon)];

  finally
    mm.Free;
  end;

end;

procedure TMCPIcon.GetPNGSize(const AFileName: string; var AWidth, AHeight: Word);
type
  TPNGSig = array[0..7] of Byte;
const
  VALID_SIG: TPNGSig = (137, 80, 78, 71, 13, 10, 26, 10);
var
  LSig: TPNGSig;
  LStream: tFileStream;
  LIndex: integer;
begin
  FillChar(LSig, SizeOf(LSig), #0);
  LStream := TFileStream.Create(AFileName, fmOpenRead);
  try
    LStream.Read(LSig[0], SizeOf(LSig));

    for LIndex := Low(LSig) to High(LSig) do
      if LSig[LIndex] <> VALID_SIG[LIndex] then
        Exit;

      LStream.Seek(18, 0);
      AWidth := ReadMWord(LStream);
      LStream.Seek(22, 0);
      AHeight := ReadMWord(LStream);
  finally
    LStream.Free;
  end;
end;

function TMCPIcon.GetPNGSizeString(const AFileName: string): string;
begin
  var width, height: Word;
  GetPNGSize(AFileName, width, height);

  Result := width.ToString + 'x' + height.ToString;
end;

function TMCPIcon.ReadMWord(AStream: TFileStream): Word;

type
  TMotorolaWord = record
  case Byte of
    0: (Value: word);
    1: (Byte1, Byte2: byte);
  end;

var
  LWord: TMotorolaWord;
begin
  // It would probably be better to just read these two bytes in normally and
  // then do a small ASM routine to swap them. But we aren't talking about
  // reading entire files, so I doubt the performance gain would be worth the trouble.
  AStream.Read(LWord.Byte2, SizeOf(Byte));
  AStream.Read(LWord.Byte1, SizeOf(Byte));
  Result := LWord.Value;
end;

{ TMCPMimeTypes }

procedure TMCPMimeTypes.SetComplete;
begin
  AddMime(TMimeEncoding.Plain, 'text/calendar', '.ics,.ifb');
  AddMime(TMimeEncoding.Plain, 'text/css', '.css');
  AddMime(TMimeEncoding.Plain, 'text/csv', '.csv');
  AddMime(TMimeEncoding.Plain, 'text/html', '.htm,.html');
  AddMime(TMimeEncoding.Plain, 'text/javascript', '.js');
  AddMime(TMimeEncoding.Plain, 'text/markdown', '.md,.markdown,.mdown,.markdn');
  AddMime(TMimeEncoding.Plain, 'text/mathml', '.mathml,.mml');
  AddMime(TMimeEncoding.Plain, 'text/plain', '.txt,.text,.ini,.conf,.def,.diff,.list,.log');
  AddMime(TMimeEncoding.Plain, 'text/prs.lines.tag', '.dsc');
  AddMime(TMimeEncoding.Plain, 'text/richtext', '.rtx');
  AddMime(TMimeEncoding.Plain, 'text/sgml', '.sgm,.sgml');
  AddMime(TMimeEncoding.Plain, 'text/tab-separated-values', '.tsv');
  AddMime(TMimeEncoding.Plain, 'text/troff', '.man,.me,.ms,.roff,.t,.tr');
  AddMime(TMimeEncoding.Plain, 'text/uri-list', '.uri,.uris,.urls');
  AddMime(TMimeEncoding.Plain, 'text/vnd.curl', '.curl');
  AddMime(TMimeEncoding.Plain, 'text/vnd.graphviz', '.gv');
  AddMime(TMimeEncoding.Plain, 'text/vnd.wap.wml', '.wml');
  AddMime(TMimeEncoding.Plain, 'text/x-asm', '.asm,.s');
  AddMime(TMimeEncoding.Plain, 'text/x-c', '.c,.cc,.cpp,.cxx,.dic,.h,.hh');
  AddMime(TMimeEncoding.Plain, 'text/x-fortran', '.f,.f77,.f90,.for');
  AddMime(TMimeEncoding.Plain, 'text/x-java-source', '.java');
  AddMime(TMimeEncoding.Plain, 'text/x-pascal', '.p,.pas,.pp,.inc');
  AddMime(TMimeEncoding.Plain, 'text/x-python', '.py,.pyc,.pyo,.pyd,.whl');
  AddMime(TMimeEncoding.Plain, 'text/x-setext', '.etx');
  AddMime(TMimeEncoding.Plain, 'text/x-uuencode', '.uu');
  AddMime(TMimeEncoding.Plain, 'text/x-vcalendar', '.vcs');
  AddMime(TMimeEncoding.Plain, 'text/x-vcard', '.vcf');

  AddMime(TMimeEncoding.Plain, 'application/json', '.json');
  AddMime(TMimeEncoding.Plain, 'application/xml', '.xml');
  AddMime(TMimeEncoding.Plain, 'application/yaml', '.yaml,.yml');
  AddMime(TMimeEncoding.Plain, 'application/toml', '.toml');
  AddMime(TMimeEncoding.Plain, 'application/rss+xml', '.rss');
  AddMime(TMimeEncoding.Plain, 'application/x-shellscript', '.sh');
  AddMime(TMimeEncoding.Plain, 'application/xml', '.xml,.xpdl,.xsl');
  AddMime(TMimeEncoding.Plain, 'application/xml-dtd', '.dtd');
  AddMime(TMimeEncoding.Plain, 'application/xop+xml', '.xop');
  AddMime(TMimeEncoding.Plain, 'application/xslt+xml', '.xslt');
  AddMime(TMimeEncoding.Plain, 'application/xspf+xml', '.xspf');

  AddMime(TMimeEncoding.Base64, 'application/pdf', '.pdf');
  AddMime(TMimeEncoding.Base64, 'application/octet-stream', '.dat,.a,.bin,.bpk,.deploy,.dist,.dmg,.dms,.dump,.lha,.lrf,.lzh,.o,.obj,.pkg,.so');
  AddMime(TMimeEncoding.Base64, 'application/pgp-encrypted', '.pgp');
  AddMime(TMimeEncoding.Base64, 'application/pgp-signature', '.asc,.sig');
  AddMime(TMimeEncoding.Base64, 'application/pkcs10', '.p10');
  AddMime(TMimeEncoding.Base64, 'application/pkcs7-mime', '.p7c,.p7m');
  AddMime(TMimeEncoding.Base64, 'application/pkcs7-signature', '.p7s');
  AddMime(TMimeEncoding.Base64, 'application/postscript', '.ai,.eps,.ps');
  AddMime(TMimeEncoding.Base64, 'application/rtf', '.rtf');
  AddMime(TMimeEncoding.Base64, 'application/vnd.amazon.ebook', '.azw');
  AddMime(TMimeEncoding.Base64, 'application/vnd.android.package-archive', '.apk');
  AddMime(TMimeEncoding.Base64, 'application/vnd.lotus-1-2-3', '.123');
  AddMime(TMimeEncoding.Base64, 'application/vnd.ms-excel', '.xla,.xlb,.xlc,.xlm,.xls,.xlt,.xlw');
  AddMime(TMimeEncoding.Base64, 'application/vnd.ms-excel.addin.macroenabled.12', '.xlam');
  AddMime(TMimeEncoding.Base64, 'application/vnd.ms-excel.sheet.binary.macroenabled.12', '.xlsb');
  AddMime(TMimeEncoding.Base64, 'application/vnd.ms-excel.sheet.macroenabled.12', '.xlsm');
  AddMime(TMimeEncoding.Base64, 'application/vnd.ms-excel.template.macroenabled.12', '.xltm');
  AddMime(TMimeEncoding.Base64, 'application/vnd.ms-htmlhelp', '.chm');
  AddMime(TMimeEncoding.Base64, 'application/vnd.ms-powerpoint', '.pot,.ppa,.pps,.ppt,.pwz');
  AddMime(TMimeEncoding.Base64, 'application/vnd.ms-powerpoint.addin.macroenabled.12', '.ppam');
  AddMime(TMimeEncoding.Base64, 'application/vnd.ms-project', '.mpp,.mpt');
  AddMime(TMimeEncoding.Base64, 'application/vnd.oasis.opendocument.chart', '.odc');
  AddMime(TMimeEncoding.Base64, 'application/vnd.oasis.opendocument.chart-template', '.otc');
  AddMime(TMimeEncoding.Base64, 'application/vnd.oasis.opendocument.database', '.odb');
  AddMime(TMimeEncoding.Base64, 'application/vnd.oasis.opendocument.formula', '.odf');
  AddMime(TMimeEncoding.Base64, 'application/vnd.oasis.opendocument.formula-template', '.odft');
  AddMime(TMimeEncoding.Base64, 'application/vnd.oasis.opendocument.graphics', '.odg');
  AddMime(TMimeEncoding.Base64, 'application/vnd.oasis.opendocument.graphics-template', '.otg');
  AddMime(TMimeEncoding.Base64, 'application/vnd.oasis.opendocument.image', '.odi');
  AddMime(TMimeEncoding.Base64, 'application/vnd.oasis.opendocument.image-template', '.oti');
  AddMime(TMimeEncoding.Base64, 'application/vnd.oasis.opendocument.presentation', '.odp');
  AddMime(TMimeEncoding.Base64, 'application/vnd.oasis.opendocument.presentation-template', '.otp');
  AddMime(TMimeEncoding.Base64, 'application/vnd.oasis.opendocument.spreadsheet', '.ods');
  AddMime(TMimeEncoding.Base64, 'application/vnd.oasis.opendocument.spreadsheet-template', '.ots');
  AddMime(TMimeEncoding.Base64, 'application/vnd.oasis.opendocument.text', '.odt');
  AddMime(TMimeEncoding.Base64, 'application/vnd.oasis.opendocument.text-master', '.otm');
  AddMime(TMimeEncoding.Base64, 'application/vnd.oasis.opendocument.text-template', '.ott');
  AddMime(TMimeEncoding.Base64, 'application/vnd.oasis.opendocument.text-web', '.oth');
  AddMime(TMimeEncoding.Base64, 'application/vnd.openxmlformats-officedocument.presentationml.presentation', '.pptx');
  AddMime(TMimeEncoding.Base64, 'application/vnd.openxmlformats-officedocument.presentationml.slide', '.sldx');
  AddMime(TMimeEncoding.Base64, 'application/vnd.openxmlformats-officedocument.presentationml.slideshow', '.ppsx');
  AddMime(TMimeEncoding.Base64, 'application/vnd.openxmlformats-officedocument.presentationml.template', '.potx');
  AddMime(TMimeEncoding.Base64, 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet', '.xlsx');
  AddMime(TMimeEncoding.Base64, 'application/vnd.openxmlformats-officedocument.spreadsheetml.template', '.xltx');
  AddMime(TMimeEncoding.Base64, 'application/vnd.openxmlformats-officedocument.wordprocessingml.document', '.docx');
  AddMime(TMimeEncoding.Base64, 'application/vnd.openxmlformats-officedocument.wordprocessingml.template', '.dotx');
  AddMime(TMimeEncoding.Base64, 'application/vnd.sqlite3', '.db,.sqlite,.sqlite3,.db-wal,.sqlite-wal,.db-shm,.sqlite-shm');
  AddMime(TMimeEncoding.Base64, 'application/wasm', '.wasm');
  AddMime(TMimeEncoding.Base64, 'application/x-7z-compressed', '.7z');
  AddMime(TMimeEncoding.Base64, 'application/x-ace-compressed', '.ace');
  AddMime(TMimeEncoding.Base64, 'application/x-bittorrent', '.torrent');
  AddMime(TMimeEncoding.Base64, 'application/x-bzip', '.bz');
  AddMime(TMimeEncoding.Base64, 'application/x-bzip2', '.boz,.bz2');
  AddMime(TMimeEncoding.Base64, 'application/x-debian-package', '.deb,.udeb');
  AddMime(TMimeEncoding.Base64, 'application/x-font-bdf', '.bdf');
  AddMime(TMimeEncoding.Base64, 'application/x-font-ghostscript', '.gsf');
  AddMime(TMimeEncoding.Base64, 'application/x-font-linux-psf', '.psf');
  AddMime(TMimeEncoding.Base64, 'application/x-font-otf', '.otf');
  AddMime(TMimeEncoding.Base64, 'application/x-font-pcf', '.pcf');
  AddMime(TMimeEncoding.Base64, 'application/x-font-snf', '.snf');
  AddMime(TMimeEncoding.Base64, 'application/x-font-ttf', '.ttc,.ttf');
  AddMime(TMimeEncoding.Base64, 'application/x-font-type1', '.afm,.pfa,.pfb,.pfm');
  AddMime(TMimeEncoding.Base64, 'application/x-latex', '.latex');
  AddMime(TMimeEncoding.Base64, 'application/x-msaccess', '.mdb');
  AddMime(TMimeEncoding.Base64, 'application/x-mspublisher', '.pub');
  AddMime(TMimeEncoding.Base64, 'application/x-pkcs12', '.p12,.pfx');
  AddMime(TMimeEncoding.Base64, 'application/x-pkcs7-certificates', '.p7b,.spc');
  AddMime(TMimeEncoding.Base64, 'application/x-pkcs7-certreqresp', '.p7r');
  AddMime(TMimeEncoding.Base64, 'application/x-rar-compressed', '.rar');
  AddMime(TMimeEncoding.Base64, 'application/x-rpm', '.rpm');
  AddMime(TMimeEncoding.Base64, 'application/zip', '.zip');

  AddMime(TMimeEncoding.Base64, 'audio/3gpp2', '.3g2');
  AddMime(TMimeEncoding.Base64, 'audio/aac', '.aac,.m4a');
  AddMime(TMimeEncoding.Base64, 'audio/aacp', '.aacp');
  AddMime(TMimeEncoding.Base64, 'audio/adpcm', '.adp');
  AddMime(TMimeEncoding.Base64, 'audio/aiff', '.aiff,.aif,.aff');
  AddMime(TMimeEncoding.Base64, 'audio/basic', '.au,.snd');
  AddMime(TMimeEncoding.Base64, 'audio/flac', '.flac');
  AddMime(TMimeEncoding.Base64, 'audio/midi', '.kar,.mid,.midi,.rmi');
  AddMime(TMimeEncoding.Base64, 'audio/mp4', '.mp4,.m4a,.m4b,.m4p,.m4r,.m4v,.mp4v,.3gp,.3g2,.3ga,.3gpa,.3gpp,.3gpp2,.3gp2');
  AddMime(TMimeEncoding.Base64, 'audio/mpeg', '.m2a,.m3a,.mp2,.mp2a,.mp3,.mpga');
  AddMime(TMimeEncoding.Base64, 'audio/ogg', '.oga,.ogg,.spx');
  AddMime(TMimeEncoding.Base64, 'audio/vnd.wav', '.wav');
  AddMime(TMimeEncoding.Base64, 'audio/webm', '.weba');
  AddMime(TMimeEncoding.Base64, 'audio/x-matroska', '.mka');
  AddMime(TMimeEncoding.Base64, 'audio/x-mpegurl', '.m3u');
  AddMime(TMimeEncoding.Base64, 'audio/x-ms-wax', '.wax');
  AddMime(TMimeEncoding.Base64, 'audio/x-ms-wma', '.wma');
  AddMime(TMimeEncoding.Base64, 'font/otf', '.otf');
  AddMime(TMimeEncoding.Base64, 'font/woff', '.woff');
  AddMime(TMimeEncoding.Base64, 'font/woff2', '.woff2');
  AddMime(TMimeEncoding.Base64, 'image/avif', '.avif');
  AddMime(TMimeEncoding.Base64, 'image/bmp', '.bmp');
  AddMime(TMimeEncoding.Base64, 'image/cgm', '.cgm');
  AddMime(TMimeEncoding.Base64, 'image/gif', '.gif');
  AddMime(TMimeEncoding.Base64, 'image/jpeg', '.jpe,.jpeg,.jpg,.pjpg,.jfif,.jfif-tbnl,.jif');
  AddMime(TMimeEncoding.Base64, 'image/png', '.png');
  AddMime(TMimeEncoding.Base64, 'image/svg+xml', '.svg,.svgz');
  AddMime(TMimeEncoding.Base64, 'image/tiff', '.tif,.tiff');
  AddMime(TMimeEncoding.Base64, 'image/vnd.adobe.photoshop', '.psd');
  AddMime(TMimeEncoding.Base64, 'image/vnd.djvu', '.djv,.djvu');
  AddMime(TMimeEncoding.Base64, 'image/vnd.dwg', '.dwg');
  AddMime(TMimeEncoding.Base64, 'image/vnd.dxf', '.dxf');
  AddMime(TMimeEncoding.Base64, 'image/vnd.wap.wbmp', '.wbmp');
  AddMime(TMimeEncoding.Base64, 'image/vnd.xiff', '.xif');
  AddMime(TMimeEncoding.Base64, 'image/webp', '.webp');
  AddMime(TMimeEncoding.Base64, 'image/x-icns', '.icns');
  AddMime(TMimeEncoding.Base64, 'image/x-icon', '.ico');
  AddMime(TMimeEncoding.Base64, 'image/x-pcx', '.pcx');
  AddMime(TMimeEncoding.Base64, 'image/x-pict', '.pct,.pic');

  AddMime(TMimeEncoding.Base64, 'video/3gpp', '.3gp');
  AddMime(TMimeEncoding.Base64, 'video/3gpp2', '.3g2');
  AddMime(TMimeEncoding.Base64, 'video/h261', '.h261');
  AddMime(TMimeEncoding.Base64, 'video/h263', '.h263');
  AddMime(TMimeEncoding.Base64, 'video/h264', '.h264');
  AddMime(TMimeEncoding.Base64, 'video/jpeg', '.jpgv');
  AddMime(TMimeEncoding.Base64, 'video/jpm', '.jpgm,.jpm');
  AddMime(TMimeEncoding.Base64, 'video/mj2', '.mj2,.mjp2');
  AddMime(TMimeEncoding.Base64, 'video/mp4', '.mp4,.mp4v,.mpg4');
  AddMime(TMimeEncoding.Base64, 'video/mpeg', '.m1v,.m2v,.mpa,.mpe,.mpeg,.mpg');
  AddMime(TMimeEncoding.Base64, 'video/ogg', '.ogv');
  AddMime(TMimeEncoding.Base64, 'video/quicktime', '.mov,.qt');
  AddMime(TMimeEncoding.Base64, 'video/webm', '.webm');
  AddMime(TMimeEncoding.Base64, 'video/x-f4v', '.f4v');
  AddMime(TMimeEncoding.Base64, 'video/x-fli', '.fli');
  AddMime(TMimeEncoding.Base64, 'video/x-flv', '.flv');
  AddMime(TMimeEncoding.Base64, 'video/x-m4v', '.m4v');
  AddMime(TMimeEncoding.Base64, 'video/x-matroska', '.mkv');
  AddMime(TMimeEncoding.Base64, 'video/x-ms-asf', '.asf,.asx');
  AddMime(TMimeEncoding.Base64, 'video/x-ms-wm', '.wm');
  AddMime(TMimeEncoding.Base64, 'video/x-ms-wmv', '.wmv');
  AddMime(TMimeEncoding.Base64, 'video/x-ms-wmx', '.wmx');
  AddMime(TMimeEncoding.Base64, 'video/x-ms-wvx', '.wvx');
  AddMime(TMimeEncoding.Base64, 'video/x-msvideo', '.avi');
  AddMime(TMimeEncoding.Base64, 'video/x-sgi-movie', '.movie');
end;

function TMCPMimeTypes.Count: NativeInt;
begin
  Result := FList.Count;
end;

constructor TMCPMimeTypes.Create(AList: TMimeList = TMimeList.Standard);
begin
  FList := TList<TMimeInfo>.Create;
  case AList of
    TMimeList.Standard: SetStandard;
    TMimeList.Complete: SetComplete;
  end;
end;

function TMCPMimeTypes.DataUri(const AFileName: string): string;
begin
  var ext := TPath.GetExtension(AFileName);
  var mime := MediaByExtension(ext);
  if mime.IsEmpty then
    raise EMCPException.CreateFmt('MediaType for extension [%s] not found', [ext]);

  Result := DataUri(AFileName, mime);
end;

function TMCPMimeTypes.DataUri(const AFileName, AMimeType: string): string;
begin
  if not TFile.Exists(AFileName) then
    raise EMCPException.CreateFmt('Building data uri: file [%s] not found', [AFileName]);

  var input := TFileStream.Create(AFileName, fmOpenRead);
  var output := TStringStream.Create;
  try
    TNetEncoding.Base64.Encode(input, output);
    Result := 'data:' + AMimeType + ';base64,' + output.DataString;
  finally
    input.Free;
    output.Free;
  end;

end;

destructor TMCPMimeTypes.Destroy;
begin
  FList.Free;
  inherited;
end;

procedure TMCPMimeTypes.AddMime(AEncoding: TMimeEncoding; const AMime, AExt: string);
var
  LInfo: TMimeInfo;
begin
  LInfo.Encoding := AEncoding;
  LInfo.Mime := AMime;
  LInfo.Ext := AExt;

  FList.Add(LInfo);
end;

function TMCPMimeTypes.EncodingByMedia(const AMime: string): Nullable<TMimeEncoding>;
begin
  var LMime := AMime;
  var LMimeParts := LMime.Split([';']);
  if Length(LMimeParts) > 1 then
    LMime := LMimeParts[0];
  Result := TMimeEncoding.Base64;
  for var m in FList do
  begin
    if LMime = m.Mime then
      Exit(m.Encoding);
  end;
end;

function TMCPMimeTypes.ExtExists(const AExtension, AList: string): Boolean;
begin
  Result := False;
  var lst := AList.Split([',']);
  for var ext in lst do
    if SameText(AExtension, ext) then
      Exit(True);
end;

procedure TMCPMimeTypes.SetStandard;
begin
  FList.Clear;
  AddMime(TMimeEncoding.Plain, 'text/css', '.css');
  AddMime(TMimeEncoding.Plain, 'text/csv', '.csv');
  AddMime(TMimeEncoding.Plain, 'text/html', '.htm,.html');
  AddMime(TMimeEncoding.Plain, 'text/javascript', '.js');
  AddMime(TMimeEncoding.Plain, 'text/markdown', '.md,.markdown,.mdown,.markdn');
  AddMime(TMimeEncoding.Plain, 'text/plain', '.txt,.text,.ini,.conf,.def,.diff,.list,.log');

  AddMime(TMimeEncoding.Plain, 'application/json', '.json');
  AddMime(TMimeEncoding.Plain, 'application/xml', '.xml');
  AddMime(TMimeEncoding.Plain, 'application/yaml', '.yaml,.yml');
  AddMime(TMimeEncoding.Plain, 'application/toml', '.toml');
  AddMime(TMimeEncoding.Plain, 'application/rss+xml', '.rss');
  AddMime(TMimeEncoding.Plain, 'application/xml', '.xml,.xsl');
  AddMime(TMimeEncoding.Plain, 'application/xslt+xml', '.xslt');

  AddMime(TMimeEncoding.Base64, 'application/octet-stream', '.dat,.a,.bin,.bpk,.dist,.dmg,.dms,.dump,.o,.obj,.pkg,.so');
  AddMime(TMimeEncoding.Base64, 'application/pdf', '.pdf');
  AddMime(TMimeEncoding.Base64, 'application/rtf', '.rtf');
  AddMime(TMimeEncoding.Base64, 'application/x-msaccess', '.mdb');
  AddMime(TMimeEncoding.Base64, 'application/vnd.ms-excel', '.xla,.xlb,.xlc,.xlm,.xls,.xlt,.xlw');
  AddMime(TMimeEncoding.Base64, 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet', '.xlsx');
  AddMime(TMimeEncoding.Base64, 'application/vnd.openxmlformats-officedocument.wordprocessingml.document', '.docx');
  AddMime(TMimeEncoding.Base64, 'application/vnd.sqlite3', '.db,.sqlite,.sqlite3');
  AddMime(TMimeEncoding.Base64, 'application/x-7z-compressed', '.7z');
  AddMime(TMimeEncoding.Base64, 'application/x-rar-compressed', '.rar');
  AddMime(TMimeEncoding.Base64, 'application/zip', '.zip');

  AddMime(TMimeEncoding.Base64, 'audio/flac', '.flac');
  AddMime(TMimeEncoding.Base64, 'audio/mp4', '.mp4,.m4a,.m4b,.m4p,.m4r,.m4v,.mp4v,.3gp,.3g2,.3ga,.3gpa,.3gpp,.3gpp2,.3gp2');
  AddMime(TMimeEncoding.Base64, 'audio/mpeg', '.m2a,.m3a,.mp2,.mp2a,.mp3,.mpga');
  AddMime(TMimeEncoding.Base64, 'audio/ogg', '.oga,.ogg,.spx');
  AddMime(TMimeEncoding.Base64, 'audio/vnd.wav', '.wav');
  AddMime(TMimeEncoding.Base64, 'audio/webm', '.weba');
  AddMime(TMimeEncoding.Base64, 'audio/x-matroska', '.mka');

  AddMime(TMimeEncoding.Base64, 'image/avif', '.avif');
  AddMime(TMimeEncoding.Base64, 'image/bmp', '.bmp');
  AddMime(TMimeEncoding.Base64, 'image/cgm', '.cgm');
  AddMime(TMimeEncoding.Base64, 'image/gif', '.gif');
  AddMime(TMimeEncoding.Base64, 'image/jpeg', '.jpe,.jpeg,.jpg,.pjpg');
  AddMime(TMimeEncoding.Base64, 'image/png', '.png');
  AddMime(TMimeEncoding.Base64, 'image/svg+xml', '.svg,.svgz');
  AddMime(TMimeEncoding.Base64, 'image/tiff', '.tif,.tiff');
  AddMime(TMimeEncoding.Base64, 'image/vnd.adobe.photoshop', '.psd');
  AddMime(TMimeEncoding.Base64, 'image/vnd.djvu', '.djv,.djvu');
  AddMime(TMimeEncoding.Base64, 'image/vnd.dwg', '.dwg');
  AddMime(TMimeEncoding.Base64, 'image/vnd.dxf', '.dxf');
  AddMime(TMimeEncoding.Base64, 'image/vnd.wap.wbmp', '.wbmp');
  AddMime(TMimeEncoding.Base64, 'image/vnd.xiff', '.xif');
  AddMime(TMimeEncoding.Base64, 'image/webp', '.webp');
  AddMime(TMimeEncoding.Base64, 'image/x-icns', '.icns');
  AddMime(TMimeEncoding.Base64, 'image/x-icon', '.ico');
  AddMime(TMimeEncoding.Base64, 'image/x-pcx', '.pcx');
  AddMime(TMimeEncoding.Base64, 'image/x-pict', '.pct,.pic');

  AddMime(TMimeEncoding.Base64, 'video/mp4', '.mp4,.mp4v,.mpg4');
  AddMime(TMimeEncoding.Base64, 'video/mpeg', '.m1v,.m2v,.mpa,.mpe,.mpeg,.mpg');
  AddMime(TMimeEncoding.Base64, 'video/ogg', '.ogv');
  AddMime(TMimeEncoding.Base64, 'video/quicktime', '.mov,.qt');
  AddMime(TMimeEncoding.Base64, 'video/webm', '.webm');
  AddMime(TMimeEncoding.Base64, 'video/x-matroska', '.mkv');
  AddMime(TMimeEncoding.Base64, 'video/x-ms-wmv', '.wmv');
  AddMime(TMimeEncoding.Base64, 'video/x-msvideo', '.avi');
  AddMime(TMimeEncoding.Base64, 'video/x-sgi-movie', '.movie');
end;

function TMCPMimeTypes.MediaByExtension(const AExtension: string): string;
begin
  Result := '';
  for var m in FList do
  begin
    if ExtExists(AExtension, m.Ext) then
      Exit(m.Mime);
  end;
end;

end.
