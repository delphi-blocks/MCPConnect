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

  MCPConnect.Content.Writers,
  MCPConnect.Configuration.Core;

type
  /// <summary>
  ///   Represents a tool class registration with its namespace.
  /// </summary>
  TToolClassInfo = record
    Namespace: string;
    ToolClass: TClass;
  end;

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
    ///   Registers a tool class with an optional namespace.
    ///   Tool methods marked with [McpTool] will be exposed with the format:
    ///   namespace + separator + tool_name (if namespace is not empty).
    /// </summary>
    /// <param name="ANamespace">
    ///   Namespace prefix for tools in this class. Use empty string for no namespace.
    /// </param>
    /// <param name="AClass">Class containing [McpTool] methods</param>
    /// <returns>Self for fluent chaining</returns>
    /// <remarks>
    ///   If multiple classes are registered with the same namespace, the last one
    ///   overwrites previous registrations (no error is raised).
    /// </remarks>
    function RegisterToolClass(const ANamespace: string; AClass: TClass): IMCPConfig; overload;

    /// <summary>
    ///   Registers a tool class without namespace (backward compatible).
    ///   Equivalent to RegisterToolClass('', AClass).
    /// </summary>
    /// <param name="AClass">Class containing [McpTool] methods</param>
    /// <returns>Self for fluent chaining</returns>
    function RegisterToolClass(AClass: TClass): IMCPConfig; overload;

    /// <summary>
    ///   DEPRECATED: Use RegisterToolClass instead.
    ///   Sets a single tool class (legacy method, maintained for backward compatibility).
    /// </summary>
    /// <param name="AClass">Class containing [McpTool] methods</param>
    /// <returns>Self for fluent chaining</returns>
    function SetToolClass(AClass: TClass): IMCPConfig; deprecated 'Use RegisterToolClass instead';

    /// <summary>
    ///   Sets the separator character/string used between namespace and tool name.
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
    /// <example>
    ///   <code>
    ///   .SetNamespaceSeparator('_')  // Results in: auth_login (default)
    ///   .SetNamespaceSeparator('-')  // Results in: auth-login
    ///   </code>
    /// </example>
    function SetNamespaceSeparator(const ASeparator: string): IMCPConfig;

    /// <summary>
    ///   Sets the server name returned in the MCP initialize response.
    ///   Identifies the server to MCP clients (Claude Desktop, etc.).
    /// </summary>
    /// <param name="AName">Human-readable server name (default: 'MCPServer')</param>
    /// <returns>Self for fluent chaining</returns>
    function SetServerName(const AName: string): IMCPConfig;

    /// <summary>
    ///   Sets the server version returned in the MCP initialize response.
    ///   Helps clients identify server capabilities and compatibility.
    /// </summary>
    /// <param name="AVersion">Semantic version string (default: '1.0')</param>
    /// <returns>Self for fluent chaining</returns>
    function SetServerVersion(const AVersion: string): IMCPConfig;

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
    function RegisterWriter(AClass: TCustomWriterClass): IMCPConfig;

    /// <summary>
    ///   Returns the registry of all registered content writers.
    ///   Typically used internally by the framework during result processing.
    /// </summary>
    /// <returns>Writer registry containing all registered writers</returns>
    function GetWriters: TMCPWriterRegistry;

    /// <summary>
    ///   Returns all registered tool classes with their namespaces.
    /// </summary>
    /// <returns>Array of tool class registrations</returns>
    function GetToolClasses: TArray<TToolClassInfo>;

    /// <summary>
    ///   Returns the configured namespace separator.
    /// </summary>
    /// <returns>Separator string (default: '_')</returns>
    function GetNamespaceSeparator: string;
  end;

  [Implements(IMCPConfig)]
  TMCPConfig = class(TJRPCConfiguration, IMCPConfig)
  private
    FWriterRegistry: TMCPWriterRegistry;
    FToolClasses: TObjectDictionary<string, TClass>;
    FNamespaceSeparator: string;    FServerVersion: string;
    FServerName: string;
  public
    constructor Create(AApp: IJRPCApplication); override;
    destructor Destroy; override;
    procedure AfterConstruction; override;

    { IMCPConfig }
    function RegisterToolClass(const ANamespace: string; AClass: TClass): IMCPConfig; overload;
    function RegisterToolClass(AClass: TClass): IMCPConfig; overload;
    function SetToolClass(AClass: TClass): IMCPConfig; // deprecated
    function SetNamespaceSeparator(const ASeparator: string): IMCPConfig;
    function SetServerName(const AName: string): IMCPConfig;
    function SetServerVersion(const AVersion: string): IMCPConfig;
    function RegisterWriter(AClass: TCustomWriterClass): IMCPConfig;
    function GetWriters: TMCPWriterRegistry;
    function GetToolClasses: TArray<TToolClassInfo>;
    function GetNamespaceSeparator: string;

    /// <summary>
    ///   Creates an instance of a tool class by namespace.
    ///   Used internally by the framework to instantiate tools.
    /// </summary>
    /// <param name="ANamespace">Namespace of the tool class to instantiate</param>
    /// <returns>New instance of the tool class</returns>
    /// <exception cref="EJRPCException">Raised if namespace not found</exception>
    function CreateToolInstance(const ANamespace: string): TObject;

    /// <summary>
    ///   Finds the namespace for a fully qualified tool name.
    ///   Splits the tool name by separator and looks up the namespace part.
    /// </summary>
    /// <param name="AFullToolName">Full tool name (e.g., "auth.login")</param>
    /// <param name="ANamespace">Output: namespace found (e.g., "auth")</param>
    /// <param name="AToolName">Output: tool name without namespace (e.g., "login")</param>
    /// <returns>True if namespace found, False if tool has no namespace</returns>
    function FindNamespaceForTool(const AFullToolName: string; out ANamespace, AToolName: string): Boolean;

    property ServerName: string read FServerName write FServerName;
    property ServerVersion: string read FServerVersion write FServerVersion;
  end;

implementation

uses
  Neon.Core.Utils,
  MCPConnect.JRPC.Core;

{ TMCPConfig }

constructor TMCPConfig.Create(AApp: IJRPCApplication);
begin
  inherited;
  FWriterRegistry := TMCPWriterRegistry.Create;
  FToolClasses := TObjectDictionary<string, TClass>.Create;
end;

procedure TMCPConfig.AfterConstruction;
begin
  inherited;
  FServerName := 'MCPServer';
  FServerVersion := '1.0';
  FNamespaceSeparator := '_';  // Default separator (MCP requires ^[a-zA-Z0-9_-]{1,64}$)
end;

destructor TMCPConfig.Destroy;
begin
  FToolClasses.Free;
  FWriterRegistry.Free;
  inherited;
end;

function TMCPConfig.RegisterToolClass(const ANamespace: string; AClass: TClass): IMCPConfig;
begin
  // AddOrSetValue: if namespace exists, replaces the class (last wins)
  FToolClasses.AddOrSetValue(ANamespace, AClass);
  Result := Self;
end;

function TMCPConfig.RegisterToolClass(AClass: TClass): IMCPConfig;
begin
  // Register without namespace (empty string key)
  Result := RegisterToolClass('', AClass);
end;

function TMCPConfig.GetWriters: TMCPWriterRegistry;
begin
  Result := FWriterRegistry;
end;

function TMCPConfig.RegisterWriter(AClass: TCustomWriterClass): IMCPConfig;
begin
  FWriterRegistry.RegisterWriter(AClass);
  Result := Self;
end;

function TMCPConfig.SetServerName(const AName: string): IMCPConfig;
begin
  FServerName := AName;
  Result := Self;
end;

function TMCPConfig.SetServerVersion(const AVersion: string): IMCPConfig;
begin
  FServerVersion := AVersion;
  Result := Self;
end;

function TMCPConfig.SetToolClass(AClass: TClass): IMCPConfig;
begin
  // Legacy method - redirects to RegisterToolClass
  Result := RegisterToolClass('', AClass);
end;

function TMCPConfig.SetNamespaceSeparator(const ASeparator: string): IMCPConfig;
begin
  FNamespaceSeparator := ASeparator;
  Result := Self;
end;

function TMCPConfig.GetToolClasses: TArray<TToolClassInfo>;
var
  LPair: TPair<string, TClass>;
  LIndex: Integer;
begin
  SetLength(Result, FToolClasses.Count);
  LIndex := 0;
  for LPair in FToolClasses do
  begin
    Result[LIndex].Namespace := LPair.Key;
    Result[LIndex].ToolClass := LPair.Value;
    Inc(LIndex);
  end;
end;

function TMCPConfig.GetNamespaceSeparator: string;
begin
  Result := FNamespaceSeparator;
end;

function TMCPConfig.CreateToolInstance(const ANamespace: string): TObject;
var
  LClass: TClass;
begin
  if not FToolClasses.TryGetValue(ANamespace, LClass) then
    raise EJRPCException.CreateFmt('Tool class not found for namespace "%s"', [ANamespace]);

  Result := TRttiUtils.CreateInstance(LClass);
end;

function TMCPConfig.FindNamespaceForTool(const AFullToolName: string;
  out ANamespace, AToolName: string): Boolean;
var
  LNamespaceList: TList<string>;
  LNamespace: string;
  LPrefix: string;
begin
  Result := False;
  ANamespace := '';
  AToolName := AFullToolName;

  LNamespaceList := TList<string>.Create(FToolClasses.Keys);
  try
    // Sort namespaces by length (longest first) to match most specific namespace
    LNamespaceList.Sort(TComparer<string>.Construct(
      function(const A, B: string): Integer
      begin
        Result := Length(B) - Length(A);  // Descending order
      end
    ));

    // Try each namespace (longest first)
    for LNamespace in LNamespaceList do
    begin
      if LNamespace = '' then
        Continue; // Skip empty namespace for now

      // Check if tool name starts with "namespace_"
      LPrefix := LNamespace + FNamespaceSeparator;
      if AFullToolName.StartsWith(LPrefix) then
      begin
        ANamespace := LNamespace;
        AToolName := Copy(AFullToolName, Length(LPrefix) + 1, MaxInt);
        Result := True;
        Exit;
      end;
    end;

    // If no namespace matched, try default (empty namespace)
    if FToolClasses.ContainsKey('') then
    begin
      ANamespace := '';
      AToolName := AFullToolName;
      Result := True;
    end;
  finally
    LNamespaceList.Free;
  end;
end;

initialization
  TJRPCConfigClassRegistry.Instance.RegisterConfigClass(TMCPConfig);

end.
