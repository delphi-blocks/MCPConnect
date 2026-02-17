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
unit MCPConnect.Configuration.MCP;

interface

uses
  System.Classes, System.SysUtils,
  System.Generics.Defaults,
  System.Generics.Collections,
  System.Rtti,

  Neon.Core.Nullables,

  MCPConnect.JRPC.Core,
  MCPConnect.MCP.Types,
  MCPConnect.MCP.Tools,
  MCPConnect.MCP.Prompts,
  MCPConnect.MCP.Resources,

  MCPConnect.MCP.Attributes,
  MCPConnect.Content.Writers,
  MCPConnect.Configuration.Core, System.JSON;

type
  /// <summary>
  ///   Represents a tools/resources/prompts class registration with its namespace.
  /// </summary>
  TMCPClassInfo = record
    Scope: string;
    MCPClass: TClass;
  end;

  TMCPBaseConfig = class;
  TMCPListConfig = class;
  TMCPToolConfig = class;
  TMCPServerConfig = class;
  TMCPResourceConfig = class;
  
  /// <summary>
  ///   Primary configuration interface for Model Context Protocol (MCP) servers.
  ///   Configures server metadata, tool classes, and custom content writers for
  ///   handling complex return types. This is the main entry point for setting
  ///   up an MCP server using the MCPConnect framework.
  /// </summary>
  /// <remarks>
  ///   This configuration is required for all MCP servers. At minimum, you must:
  ///   1. Set a tool class using SetToolClass (contains methods marked with [McpTool])
  ///   2. Optionally set server name/version for client identification
  ///   3. Register custom writers if returning complex types (images, streams, etc.)
  ///
  ///   The tool class contains all methods exposed as MCP tools via the [McpTool]
  ///   attribute. Methods are automatically discovered and registered via RTTI.
  ///
  ///   Custom writers handle conversion of complex Delphi types to MCP content
  ///   formats. Built-in writers exist for common types (TStream, TStringList),
  ///   and VCL-specific writers handle TPicture, TBitmap, TImage, etc.
  /// </remarks>
  /// <example>
  ///   <code>
  ///   // Basic MCP server configuration:
  ///   FJRPCServer.Plugin.Configure&lt;IMCPConfig&gt;
  ///     .SetServerName('my-mcp-server')
  ///     .SetServerVersion('1.0.0')
  ///     .SetToolClass(TMyMCPTools)
  ///     .ApplyConfig;
  ///
  ///   // With custom writers for VCL image types:
  ///   FJRPCServer.Plugin.Configure&lt;IMCPConfig&gt;
  ///     .SetServerName('image-processor')
  ///     .SetServerVersion('2.1.0')
  ///     .SetToolClass(TImageTools)
  ///     .RegisterWriter(TMCPPictureWriter)
  ///     .RegisterWriter(TMCPBitmapWriter)
  ///     .RegisterWriter(TMCPImageWriter)
  ///     .ApplyConfig;
  ///
  ///   // Now tool methods can return TPicture/TBitmap directly:
  ///   [McpTool('get_image', 'Returns an image')]
  ///   function GetImage: TPicture;  // Automatically converted to base64 PNG
  ///   </code>
  /// </example>
  IMCPConfig = interface(IJRPCConfiguration)
  ['{B8BBD257-2FE1-479A-8D63-5331164CF5E5}']

    /// <summary>
    ///   Server configuration
    /// </summary>
    function Server: TMCPServerConfig;

    /// <summary>
    ///   Tools configuration
    /// </summary>
    function Tools: TMCPToolConfig;

    /// <summary>
    ///   Resources configuration
    /// </summary>
    function Resources: TMCPResourceConfig;

    /// <summary>
    ///   Prompts configuration
    /// </summary>
    function Prompts: TMCPListConfig;
  end;

  TMCPBaseConfig = class
  protected
    FConfig: IMCPConfig;
  public
    constructor Create(AConfig: IMCPConfig);

    function BackToMCP: IMCPConfig;
  end;

  TMCPCapability = (Tools, Resources, Prompts, Tasks, Logging, Completions);
  TMCPCapabilities = set of TMCPCapability;

  TMCPServerConfig = class(TMCPBaseConfig)
  public
    ScopeSeparator: string;
    Name: string;
    Description: string;
    Version: string;
    Capabilities: TMCPCapabilities;
    WriterRegistry: TMCPWriterRegistry;
  public
    constructor Create(AConfig: IMCPConfig);
    destructor Destroy; override;
  public
    /// <summary>
    ///   Sets the separator character/string used between scope and tool name.
    ///   Default is '_' (underscore), resulting in names like "auth_login".
    /// </summary>
    /// <param name="ASeparator">
    ///   Separator string. Must be MCP-compliant (only a-zA-Z0-9_-).
    ///   Common values: '_' (default), '-'
    /// </param>
    /// <returns>Self for fluent chaining</returns>
    /// <remarks>
    ///   IMPORTANT: MCP requires tool names to match ^[a-zA-Z0-9_-]{1,64}$
    ///   Do NOT use '.', ':', or other special characters.
    /// </remarks>
    function SetScopeSeparator(const ASeparator: string): TMCPServerConfig;

    /// <summary>
    ///   Sets the server name returned in the MCP initialize response.
    ///   Identifies the server to MCP clients (Claude Desktop, etc.).
    /// </summary>
    /// <param name="AName">Human-readable server name (default: 'MCPServer')</param>
    /// <returns>Self for fluent chaining</returns>
    function SetName(const AName: string): TMCPServerConfig;

    /// <summary>
    ///   Sets the server description returned in the MCP initialize response.
    /// </summary>
    /// <param name="ADescription">Description for the server (default: '')</param>
    /// <returns>Self for fluent chaining</returns>
    function SetDescription(const ADescription: string): TMCPServerConfig;

    /// <summary>
    ///   Sets the server version returned in the MCP initialize response.
    ///   Helps clients identify server capabilities and compatibility.
    /// </summary>
    /// <param name="AVersion">Semantic version string (default: '1.0')</param>
    /// <returns>Self for fluent chaining</returns>
    function SetVersion(const AVersion: string): TMCPServerConfig;

    function SetCapabilities(ACapabilities: TMCPCapabilities): TMCPServerConfig;

    /// <summary>
    ///   Registers a custom content writer for handling complex return types.
    ///   Writers convert Delphi types (TPicture, TStream, etc.) to MCP content
    ///   formats (base64 images, text, embedded resources).
    /// </summary>
    /// <param name="AClass">
    ///   Content writer class (must inherit from TMCPCustomWriter). Examples:
    ///   - TMCPPictureWriter (VCL TPicture -> base64 PNG)
    ///   - TMCPStreamWriter (TStream -> base64 or text)
    ///   - TMCPStringListWriter (TStringList -> text)
    /// </param>
    /// <returns>Self for fluent chaining</returns>
    /// <remarks>
    ///   Writers are checked in registration order. First matching writer handles
    ///   the conversion. Built-in writers for basic types are always available.
    /// </remarks>
    function RegisterWriter(AClass: TCustomWriterClass): TMCPServerConfig;
  end;

  TMCPListConfig = class(TMCPBaseConfig)
  private
    FClasses: TObjectDictionary<string, TClass>;
  public
    constructor Create(AConfig: IMCPConfig);
    destructor Destroy; override;

    function RegisterClass(AClass: TClass): TMCPListConfig; overload;
    function GetClasses: TArray<TMCPClassInfo>;

    /// <summary>
    ///   Creates an instance of a class by namespace.
    ///   Used internally by the framework to instantiate tools.
    /// </summary>
    /// <param name="ANamespace">Namespace of the tool class to instantiate</param>
    /// <returns>New instance of the tool class</returns>
    /// <exception cref="EJRPCException">Raised if namespace not found</exception>
    function CreateInstance(const ANamespace: string): TObject;
  end;


  TMCPToolConfig = class(TMCPBaseConfig)
  private
    procedure WriteInputSchema(ATool: TMCPTool);
    procedure WriteParams(AMethod: TRttiMethod; AProps: TJSONObject; ARequired: TJSONArray);
    procedure WriteTool(ATool: TMCPTool; AAttr: MCPToolAttribute);
  public
    ToolRegistry: TMCPToolRegistry;
  public
    constructor Create(AConfig: IMCPConfig);
    destructor Destroy; override;

    function RegisterClass(AClass: TClass): TMCPToolConfig;

    /// <summary>
    ///   Creates an instance of a class by namespace.
    ///   Used internally by the framework to instantiate tools.
    /// </summary>
    /// <param name="ANamespace">Namespace of the tool class to instantiate</param>
    /// <returns>New instance of the tool class</returns>
    /// <exception cref="EJRPCException">Raised if namespace not found</exception>
    function CreateInstance(const ATool: string): TObject;

    function ListComplete: TListToolsResult;
    function ListEnabled: TListToolsResult;

    procedure FilterList(AList: TListToolsResult; AFilter: TMCPToolFilterFunc);
  end;

  TMCPStaticResource = class
  public
    class procedure GetResource(AConfig: IMCPConfig; AResource: TMCPResource; AResult: TReadResourceResult);
  end;

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
    constructor Create;
    destructor Destroy; override;

    procedure SetStandard;
    procedure SetComplete;

    procedure AddMime(AEncoding: TMimeEncoding; const AMime: string; const AExt: string = '');
    function MediaByExtension(const AExtension: string): string;
    function EncodingByMedia(const AMime: string): Nullable<TMimeEncoding>;

    function Count: NativeInt;
  end;


  TMCPResourceConfig = class(TMCPBaseConfig)
  public
    Registry: TMCPResourceRegistry;
    MimeTypes: TMCPMimeTypes;
    Schemes: TDictionary<string, string>;
    BasePath: string;
  private
  public
    constructor Create(AConfig: IMCPConfig);
    destructor Destroy; override;

    function SetBasePath(const APath: string): TMCPResourceConfig;
    function RegisterScheme(const AScheme, APath: string): TMCPResourceConfig;

    function RegisterClass(AClass: TClass): TMCPResourceConfig;
    function RegisterStatic(const AFileName, AMime, ADescription: string): TMCPResourceConfig;
    //function RegisterStatic(const AScheme, APath, AMime, ADescription: string): TMCPResourceConfig;

    function RegisterResource(AClass: TClass; const AMethod, AUri: string; AConfig: TMCPResourceConfigurator): TMCPResourceConfig;

    /// <summary>
    ///   Creates an instance of a class by namespace.
    ///   Used internally by the framework to instantiate tools.
    /// </summary>
    /// <param name="ANamespace">Namespace of the tool class to instantiate</param>
    /// <returns>New instance of the tool class</returns>
    /// <exception cref="EJRPCException">Raised if namespace not found</exception>
    function CreateInstance(const AUri: string): TObject;

    function ListEnabled: TListResourcesResult;
  end;


  [Implements(IMCPConfig)]
  TMCPConfig = class(TJRPCConfiguration, IMCPConfig)
  private
    FServer: TMCPServerConfig;
    FTools: TMCPToolConfig;
    FResources: TMCPResourceConfig;
    FTemplates: TMCPListConfig;
    FPrompts: TMCPListConfig;
  public
    constructor Create(AApp: IJRPCApplication); override;
    destructor Destroy; override;

    { IMCPConfig }
    function Server: TMCPServerConfig;
    function Tools: TMCPToolConfig;
    function Resources: TMCPResourceConfig;
    function Templates: TMCPListConfig;
    function Prompts: TMCPListConfig;
  end;


implementation

uses
  System.IOUtils,
  Neon.Core.Utils,
  Neon.Core.Persistence.JSON.Schema;

constructor TMCPConfig.Create(AApp: IJRPCApplication);
begin
  inherited;

  FServer := TMCPServerConfig.Create(Self);
  FTools := TMCPToolConfig.Create(Self);
  FResources := TMCPResourceConfig.Create(Self);
  FTemplates := TMCPListConfig.Create(Self);
  FPrompts := TMCPListConfig.Create(Self);
end;

destructor TMCPConfig.Destroy;
begin
  FPrompts.Free;
  FTemplates.Free;
  FResources.Free;
  FTools.Free;
  FServer.Free;

  inherited;
end;

function TMCPConfig.Prompts: TMCPListConfig;
begin
  Result := FPrompts;
end;

function TMCPConfig.Resources: TMCPResourceConfig;
begin
  Result := FResources;
end;

function TMCPConfig.Templates: TMCPListConfig;
begin
  Result := FTemplates;
end;

function TMCPConfig.Tools: TMCPToolConfig;
begin
  Result := FTools;
end;

function TMCPConfig.Server: TMCPServerConfig;
begin
  Result := FServer;
end;

{ TMCPListConfig }

constructor TMCPListConfig.Create(AConfig: IMCPConfig);
begin
  inherited;
  FClasses := TObjectDictionary<string, TClass>.Create;
end;

destructor TMCPListConfig.Destroy;
begin
  FClasses.Free;
  inherited;
end;

function TMCPListConfig.GetClasses: TArray<TMCPClassInfo>;
var
  LPair: TPair<string, TClass>;
  LIndex: Integer;
begin
  SetLength(Result, FClasses.Count);
  LIndex := 0;
  for LPair in FClasses do
  begin
    Result[LIndex].Scope := LPair.Key;
    Result[LIndex].MCPClass := LPair.Value;
    Inc(LIndex);
  end;
end;

function TMCPListConfig.RegisterClass(AClass: TClass): TMCPListConfig;
begin
  FClasses.AddOrSetValue(AClass.ClassName, AClass);
  Result := Self;
end;

function TMCPListConfig.CreateInstance(const ANamespace: string): TObject;
var
  LClass: TClass;
begin
  if not FClasses.TryGetValue('', LClass) then
    raise EJRPCException.CreateFmt('Tool class not found for namespace "%s"', [ANamespace]);

  Result := TRttiUtils.CreateInstance(LClass);
end;

constructor TMCPToolConfig.Create(AConfig: IMCPConfig);
begin
  inherited;
  ToolRegistry := TMCPToolRegistry.Create([doOwnsValues]);
end;

function TMCPToolConfig.CreateInstance(const ATool: string): TObject;
var
  LTool: TMCPTool;
begin
  if not ToolRegistry.TryGetValue(ATool, LTool) then
    raise EMCPException.CreateFmt('Tool [%s] not found', [ATool]);

  Result := TRttiUtils.CreateInstance(LTool.Classe);
end;

destructor TMCPToolConfig.Destroy;
begin
  ToolRegistry.Free;

  inherited;
end;

function TMCPToolConfig.ListEnabled: TListToolsResult;
begin
  Result := TListToolsResult.Create;
  for var pair in ToolRegistry do
    if not pair.Value.Disabled then
      Result.Tools.Add(pair.Value);
end;

function TMCPToolConfig.RegisterClass(AClass: TClass): TMCPToolConfig;
var
  LScope: string;
  LClassType: TRttiType;
  LToolAttr: MCPToolAttribute;
  LScopeAttr: MCPScopeAttribute;
  LTool: TMCPTool;
begin
  LScope := '';
  LClassType := TRttiUtils.Context.GetType(AClass);
  LScopeAttr := TRttiUtils.FindAttribute<MCPScopeAttribute>(LClassType);
  if Assigned(LScopeAttr) then
    LScope := LScopeAttr.Name + FConfig.Server.ScopeSeparator;

  // Registers all the tools found in AClass
  for var LMethod in LClassType.GetMethods do
  begin
    LToolAttr := TRttiUtils.FindAttribute<MCPToolAttribute>(LMethod);
    if not Assigned(LToolAttr) then
      Continue;

    LTool := TMCPTool.Create;
    try
      LTool.Name := LScope + LToolAttr.Name;
      LTool.Description := LToolAttr.Description;
      LTool.Classe := AClass;
      LTool.Method := LMethod;

      WriteTool(LTool, LToolAttr);


      ToolRegistry.Add(LTool.Name, LTool);
    except
      LTool.Free;
      raise;
    end;
  end;
  Result := Self;
end;

function TMCPToolConfig.ListComplete: TListToolsResult;
begin
  Result := TListToolsResult.Create;
  for var pair in ToolRegistry do
    Result.Tools.Add(pair.Value);
end;

procedure TMCPToolConfig.FilterList(AList: TListToolsResult; AFilter: TMCPToolFilterFunc);
begin
  for var pair in ToolRegistry do
    if AFilter(pair.Value) then
      AList.Tools.Add(pair.Value);
end;

procedure TMCPToolConfig.WriteTool(ATool: TMCPTool; AAttr: MCPToolAttribute);
begin
  ATool.AddIcon(AAttr.Tags.GetValueAs<string>('icon'));
  ATool.Category := AAttr.Tags.GetValueAs<string>('category');
  ATool.Disabled := AAttr.Tags.GetBoolValue('disabled');

  if AAttr.Tags.Exists('meta.ui') then
    ATool.UI.ResourceUri := AAttr.Tags.GetValueAs<string>('meta.ui');

  if AAttr.Tags.Exists('readonly') then
    ATool.Annotations.ReadOnlyHint := AAttr.Tags.GetBoolValue('readonly');
  if AAttr.Tags.Exists('destructive') then
    ATool.Annotations.DestructiveHint := AAttr.Tags.GetBoolValue('destructive');
  if AAttr.Tags.Exists('idempotent') then
    ATool.Annotations.IdempotentHint := AAttr.Tags.GetBoolValue('idempotent');
  if AAttr.Tags.Exists('openworld') then
    ATool.Annotations.OpenWorldHint := AAttr.Tags.GetBoolValue('openworld');

  WriteInputSchema(ATool);
end;

procedure TMCPToolConfig.WriteInputSchema(ATool: TMCPTool);
var
  LProps, LInputSchema: TJSONObject;
  LRequired: TJSONArray;
begin
  LProps := TJSONObject.Create;
  LRequired := TJSONArray.Create;
  try
    WriteParams(ATool.Method, LProps, LRequired);
  except
    LProps.Free;
    LRequired.Free;
    raise;
  end;

  LInputSchema := TJSONObject.Create
    .AddPair('type', 'object')
    .AddPair('properties', LProps);
    //.AddPair('additionalProperties', False);
    //.AddPair('$schema', 'http://json-schema.org/draft-07/schema#');

  if LRequired.Count > 0 then
    LInputSchema.AddPair('required', LRequired)
  else
    LRequired.Free;

  ATool.ExchangeInputSchema(LInputSchema);
end;

procedure TMCPToolConfig.WriteParams(AMethod: TRttiMethod; AProps: TJSONObject; ARequired: TJSONArray);
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

constructor TMCPServerConfig.Create(AConfig: IMCPConfig);
begin
  inherited;
  WriterRegistry := TMCPWriterRegistry.Create;

  ScopeSeparator := '_';  // Default separator (MCP requires ^[a-zA-Z0-9_-]{1,64}$)
  Capabilities := [TMCPCapability.Tools, TMCPCapability.Resources, TMCPCapability.Prompts];
end;

destructor TMCPServerConfig.Destroy;
begin
  WriterRegistry.Free;
  inherited;
end;

function TMCPServerConfig.RegisterWriter(AClass: TCustomWriterClass): TMCPServerConfig;
begin
  WriterRegistry.RegisterWriter(AClass);
  Result := Self;
end;

function TMCPServerConfig.SetCapabilities(ACapabilities: TMCPCapabilities): TMCPServerConfig;
begin
  Capabilities := ACapabilities;
  Result := Self;
end;

function TMCPServerConfig.SetDescription(const ADescription: string): TMCPServerConfig;
begin
  Description := ADescription;
  Result := Self;
end;

function TMCPServerConfig.SetName(const AName: string): TMCPServerConfig;
begin
  Name := AName;
  Result := Self;
end;

function TMCPServerConfig.SetScopeSeparator(const ASeparator: string): TMCPServerConfig;
begin
  ScopeSeparator := ASeparator;
  Result := Self;
end;

function TMCPServerConfig.SetVersion(const AVersion: string): TMCPServerConfig;
begin
  Version := AVersion;
  Result := Self;
end;

{ TMCPBaseConfig }

function TMCPBaseConfig.BackToMCP: IMCPConfig;
begin
  Result := FConfig;
end;

constructor TMCPBaseConfig.Create(AConfig: IMCPConfig);
begin
  FConfig := AConfig;
end;

{ TMCPResourceConfig }

constructor TMCPResourceConfig.Create(AConfig: IMCPConfig);
begin
  inherited;
  BasePath := GetCurrentDir + '\data';
  ForceDirectories(BasePath);
  MimeTypes := TMCPMimeTypes.Create;
  MimeTypes.SetStandard;
  Registry := TMCPResourceRegistry.Create([doOwnsValues]);
end;

function TMCPResourceConfig.CreateInstance(const AUri: string): TObject;
var
  LResource: TMCPResource;
begin
  if not Registry.TryGetValue(AUri, LResource) then
    raise EMCPException.CreateFmt('Resource [%s] not found', [AUri]);

  Result := TRttiUtils.CreateInstance(LResource.Classe);
end;

destructor TMCPResourceConfig.Destroy;
begin
  MimeTypes.Free;
  Registry.Free;
  inherited;
end;

function TMCPResourceConfig.ListEnabled: TListResourcesResult;
begin
  Result := TListResourcesResult.Create;
  for var pair in Registry do
    if not pair.Value.Disabled then
      Result.Resources.Add(pair.Value);
end;

function TMCPResourceConfig.RegisterClass(AClass: TClass): TMCPResourceConfig;
var
  LClassType: TRttiType;
  LRes: TMCPResource;
  LResAttr: MCPResourceAttribute;
begin
  LClassType := TRttiUtils.Context.GetType(AClass);

  // Registers all the resources found in AClass
  for var LMethod in LClassType.GetMethods do
  begin
    LResAttr := TRttiUtils.FindAttribute<MCPResourceAttribute>(LMethod);
    if not Assigned(LResAttr) then
      Continue;

    if Length(LMethod.GetParameters) > 0 then
      raise EMCPException.CreateFmt('Standard method for resource [%s] cannot have parameters', [LResAttr.Name]);

    LRes := TMCPResource.Create;
    try
      LRes.Name := LResAttr.Name;
      LRes.Uri := LResAttr.Uri;
      LRes.MimeType := LResAttr.MimeType;
      LRes.Description := LResAttr.Description;
      LRes.Classe := AClass;
      LRes.Method := LMethod;

      Registry.Add(LRes.Uri, LRes);
    except
      LRes.Free;
      raise;
    end;
  end;
  Result := Self;
end;

function TMCPResourceConfig.RegisterResource(AClass: TClass; const AMethod, AUri: string;
  AConfig: TMCPResourceConfigurator): TMCPResourceConfig;
var
  LClassType: TRttiType;
  LRes: TMCPResource;
  LMethod: TRttiMethod;
begin
  LClassType := TRttiUtils.Context.GetType(AClass);
  LMethod := LClassType.GetMethod(AMethod);
  if not Assigned(LMethod) then
    raise EMCPException.CreateFmt('Method [%s] not found in class [%s]', [AMethod, AClass.ClassName]);

  if Length(LMethod.GetParameters) > 0 then
    raise EMCPException.Create('Resource''s method cannot have parameters');

  LRes := TMCPResource.Create;
  try
    Registry.Add(AUri, LRes);

    AConfig(LRes);
  except
    LRes.Free;
    raise;
  end;

  Result := Self;
end;

function TMCPResourceConfig.RegisterScheme(const AScheme, APath: string): TMCPResourceConfig;
begin
  Schemes.Add(AScheme, APath);
  Result := Self;
end;

function TMCPResourceConfig.RegisterStatic(const AFileName, AMime, ADescription: string): TMCPResourceConfig;
const
  RES_CLASS: TClass = TMCPStaticResource;
  RES_METHOD = 'GetResource';
var
  LClassType: TRttiType;
  LRes: TMCPResource;
  LMethod: TRttiMethod;
begin
  LClassType := TRttiUtils.Context.GetType(RES_CLASS);
  LMethod := LClassType.GetMethod(RES_METHOD);
  if not Assigned(LMethod) then
    raise EMCPException.CreateFmt('Method [%s] not found in class [%s]', [RES_METHOD, RES_CLASS.ClassName]);

  LRes := TMCPResource.Create;
  try
    LRes.FileName := AFileName;
    LRes.Name :=   ExtractFileName(AFileName);

    { TODO -opaolo -c : Customize the URI (URI Schemes?) 16/02/2026 13:01:25 }
    LRes.Uri := 'res://' + LRes.Name;
    LRes.MimeType := AMime;
    LRes.Description := ADescription;
    LRes.Classe := RES_CLASS;
    LRes.Method := LMethod;

    Registry.Add(LRes.Uri, LRes);
  except
    LRes.Free;
    raise;
  end;

  Result := Self;
end;

function TMCPResourceConfig.SetBasePath(const APath: string): TMCPResourceConfig;
begin
  BasePath := APath;
  Result := Self;
end;

{ TMCPStaticResource }

class procedure TMCPStaticResource.GetResource(AConfig: IMCPConfig; AResource:
    TMCPResource; AResult: TReadResourceResult);
var
  LFileName: string;
begin
  if AResource.FileName.IsEmpty then
    raise EMCPException.CreateFmt('No filename specified for static resource [%s]', [AResource.Name]);

  LFileName := TPath.Combine(AConfig.Resources.BasePath, AResource.FileName);

  if not FileExists(LFileName) then
    raise EMCPException.CreateFmt('File [%s] not found for resource [%s]', [LFileName, AResource.Name]);

  { TODO -opaolo -c : check the mime type and serve accordingly 16/02/2026 12:44:47 }

  AResult.AddTextContent(AResource.Uri, AResource.MimeType, TFile.ReadAllText(LFileName));
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
  AddMime(TMimeEncoding.Plain, 'text/plain', '.conf,.def,.diff,.in,.ksh,.list,.log,.pl,.text,.txt');
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

constructor TMCPMimeTypes.Create;
begin
  FList := TList<TMimeInfo>.Create;
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
  AddMime(TMimeEncoding.Plain, 'text/css', '.css');
  AddMime(TMimeEncoding.Plain, 'text/csv', '.csv');
  AddMime(TMimeEncoding.Plain, 'text/html', '.htm,.html');
  AddMime(TMimeEncoding.Plain, 'text/javascript', '.js');
  AddMime(TMimeEncoding.Plain, 'text/markdown', '.md,.markdown,.mdown,.markdn');
  AddMime(TMimeEncoding.Plain, 'text/plain', '.conf,.def,.diff,.in,.ksh,.list,.log,.pl,.text,.txt');

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

initialization
  TJRPCConfigClassRegistry.Instance.RegisterConfigClass(TMCPConfig);

end.
