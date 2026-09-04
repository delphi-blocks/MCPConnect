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
unit MCPConnect.MCP.Types.Resources;

interface

{$I 'MCPConnect.inc' }
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
  MCPConnect.MCP.Types.Base,
  MCPConnect.MCP.Types.Mrtr;

type
  /// <summary>
  /// Represents a known resource that the server is capable of reading.
  /// </summary>
  TMCPResourceBase = class(TMetaClass)
    /// <summary>Registration-time grouping, from the "category=" tag.</summary>
    [NeonIgnore] Category: string;

    /// <summary>Kept out of the listings, from the "disabled" tag.</summary>
    [NeonIgnore] Disabled: Boolean;

    [NeonInclude(IncludeIf.NotEmpty)] Annotations: TAnnotations;

    /// <summary>
    /// A human-readable name for this resource.
    /// </summary>
    /// <remarks>This can be used by clients to populate UI elements.</remarks>
    Name: string;

    /// <summary>
    ///   Intended for UI and end-user contexts, optimized to be human-readable
    ///   and easily understood even by those unfamiliar with domain-specific
    ///   terminology. Falls back to Name when absent.
    /// </summary>
    /// <remarks>
    ///   Set it with the "title=" tag on [McpResource] / [McpTemplate] /
    ///   [McpAppUI], or through the ATags argument of the programmatic
    ///   registration methods.
    /// </remarks>
    Title: NullString;

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
    [NeonInclude(IncludeIf.NotEmpty)] Icons: TMCPIconList;

  public
    constructor Create;
    destructor Destroy; override;
  end;


  /// <summary>
  /// Represents a known resource that the server is capable of reading.
  /// </summary>
  TMCPResource = class(TMCPResourceBase)
  public
    [NeonIgnore] FileName: string;
    [NeonIgnore] ResourceClass: TClass;
    [NeonIgnore] Method: TRttiMethod;
  public
    /// <summary>
    /// The URI of this resource.
    /// </summary>
    Uri: string;

    /// <summary>
    ///   The size of the raw resource content in bytes, before base64 encoding
    ///   or any tokenization, when it is known. Hosts use it to display file
    ///   sizes and to estimate context window usage.
    /// </summary>
    /// <remarks>
    ///   Set automatically for the static files registered with RegisterFile.
    ///   A resource template has no size, which is why this lives here rather
    ///   than on TMCPResourceBase.
    /// </remarks>
    [NeonInclude(IncludeIf.NotDefault)] Size: Int64;
  end;

  TMCPResources = class(TObjectList<TMCPResource>);
  {$IFDEF HAS_ORDERED_DICTIONARY}
  TMCPResourceRegistry = class(TObjectOrderedDictionary<string, TMCPResource>);
  {$ELSE}
  TMCPResourceRegistry = class(TObjectDictionary<string, TMCPResource>);
  {$ENDIF}
  TMCPResourceFilterFunc = reference to function (AResource: TMCPResource): Boolean;

  /// <summary>
  ///   A single URI-template placeholder configured for a manually-registered
  ///   resource template (i.e. registered without relying on [McpParam] attributes).
  /// </summary>
  TMCPResTemplateParam = class(TMetaClass)
  public
    Param: TRttiParameter;
    ParamName: string;
    Name: string;
    Description: string;
  end;

  /// <summary>
  /// Represents a template description for resources available on the server.
  /// </summary>
  TMCPResourceTemplate = class(TMCPResourceBase)
  public
    [NeonIgnore] ResourceClass: TClass;
    [NeonIgnore] Method: TRttiMethod;
    [NeonIgnore] MethodParams: TObjectList<TMCPResTemplateParam>;
  public
    /// <summary>
    /// A URI template (according to RFC 6570) that can be used to construct resource URIs.
    /// </summary>
    UriTemplate: NullString;
  public
    constructor Create;
    destructor Destroy; override;

    function FindMCPParam(const AName: string): TMCPResTemplateParam;
  end;

  TMCPTemplates = class(TObjectList<TMCPResourceTemplate>);
  TMCPTemplateRegistry = class(TObjectDictionary<string, TMCPResourceTemplate>);
  TMCPTemplateFilterFunc = reference to function (ATemplate: TMCPResourceTemplate): Boolean;


  TUIResourceCSP = class
  public
    [NeonInclude(IncludeIf.NotEmpty)] ConnectDomains: TArray<string>;
    [NeonInclude(IncludeIf.NotEmpty)] ResourceDomains: TArray<string>;
    [NeonInclude(IncludeIf.NotEmpty)] FrameDomains: TArray<string>;
    [NeonInclude(IncludeIf.NotEmpty)] BaseUriDomains: TArray<string>;
  public
    procedure AddSiteException(const ASite: string);
  end;

  TUIResourcePermissions = class
  public
    [NeonInclude(IncludeIf.NotEmpty)] Camera: TJSONObject;
    [NeonInclude(IncludeIf.NotEmpty)] Microphone: TJSONObject;
    [NeonInclude(IncludeIf.NotEmpty)] Geolocation: TJSONObject;
    [NeonInclude(IncludeIf.NotEmpty)] ClipboardWrite: TJSONObject;
  public
    constructor Create;
    destructor Destroy; override;

  end;

  TUIResourceUI = class
  public
    [NeonInclude(IncludeIf.NotEmpty)] Csp: TUIResourceCSP;
    [NeonInclude(IncludeIf.NotEmpty)] Permissions: TUIResourcePermissions;
    Domain: NullString;
    PrefersBorder: NullBoolean;
  public
    constructor Create;
    destructor Destroy; override;

    function ToJSON: TJSONObject;
  end;

  TMCPUIResourceConfigurator = reference to procedure(AResource: TMCPResource; AUI: TUIResourceUI);


  /// <summary>
  /// The server's response to a resources/list request from the client.
  /// </summary>
  TListResourcesResult = class(TCachedResult)
  public
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
  /// The server's response to a resources/templates/list request from the client.
  /// </summary>
  TListResourceTemplatesResult = class(TCachedResult)
  public

    /// <summary>
    /// A list of available resource templates.
    /// </summary>
    ResourceTemplates: TMCPTemplates;

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
  TReadResourceParams = class(TInputRequestParams)
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
  TReadResourceResult = class(TCachedResult)
  public
    /// <summary>
    /// The contents of the resource. Can be either a TTextResourceContents or TBlobResourceContents.
    /// </summary>
    Contents: TResourceContentsList;
  public
    constructor Create; overload;
    constructor Create(AContents: TResourceContentsList); overload;
    destructor Destroy; override;

    procedure AddContent(AContent: TResourceContents);
    procedure AddTextContent(const AUri, AMime, AText: string);
    procedure AddBlobContent(const AUri, AMime, AText: string);
    procedure AddBase64Content(const AUri, AMime, ABase64: string);
  end;

  // resources/subscribe and resources/unsubscribe were removed in 2026-07-28:
  // a client subscribes through the resourceSubscriptions filter of a
  // subscriptions/listen request (MCPConnect.MCP.Types.Subscriptions).


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

constructor TReadResourceResult.Create(AContents: TResourceContentsList);
begin
  Assert(Assigned(AContents), ClassName + ': AContents cannot be nil');

  inherited Create;
  Contents := AContents;
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
  ResourceTemplates := TMCPTemplates.Create(False);
end;

destructor TListResourceTemplatesResult.Destroy;
begin
  ResourceTemplates.Free;
  inherited;
end;

{ TMCPResourceTemplate }

constructor TMCPResourceTemplate.Create;
begin
  inherited;
  MethodParams := TObjectList<TMCPResTemplateParam>.Create(True);
end;

destructor TMCPResourceTemplate.Destroy;
begin
  MethodParams.Free;
  inherited;
end;

function TMCPResourceTemplate.FindMCPParam(const AName: string): TMCPResTemplateParam;
begin
  Result := nil;
  for var par in MethodParams do
    if SameText(AName, par.ParamName) then
      Exit(par);
end;

constructor TUIResourcePermissions.Create;
begin
  Camera := TJSONObject.Create;
  Microphone := TJSONObject.Create;
  Geolocation := TJSONObject.Create;
  ClipboardWrite := TJSONObject.Create;
end;

destructor TUIResourcePermissions.Destroy;
begin
  Camera.Free;
  Microphone.Free;
  Geolocation.Free;
  ClipboardWrite.Free;

  inherited;
end;

{ TUIResourceUI }

constructor TUIResourceUI.Create;
begin
  Csp := TUIResourceCSP.Create;
  Permissions := TUIResourcePermissions.Create;
end;

destructor TUIResourceUI.Destroy;
begin
  Permissions.Free;
  Csp.Free;

  inherited;
end;

function TUIResourceUI.ToJSON: TJSONObject;
begin
  Result := TNeon.ObjectToJSON(Self, MCPNeonConfig) as TJSONObject;
end;

{ TUIResourceCSP }

procedure TUIResourceCSP.AddSiteException(const ASite: string);
begin
  ConnectDomains := ConnectDomains + [ASite];
  ResourceDomains := ResourceDomains + [ASite];
  FrameDomains := FrameDomains + [ASite];
  BaseUriDomains := BaseUriDomains + [ASite];
end;

{ TMCPResourceBase }

constructor TMCPResourceBase.Create;
begin
  inherited;
  Annotations := TAnnotations.Create;
end;

destructor TMCPResourceBase.Destroy;
begin
  Annotations.Free;
  inherited;
end;

end.
