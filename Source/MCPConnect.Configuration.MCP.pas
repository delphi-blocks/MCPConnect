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

  MCPConnect.Content.Writers,
  MCPConnect.Configuration.Core;

type
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
    ///   Sets the tool class containing MCP tool methods. This class contains
    ///   methods marked with [McpTool] attributes that are exposed as callable
    ///   tools to MCP clients.
    /// </summary>
    /// <param name="AClass">
    ///   Class reference containing [McpTool] methods. The class will be
    ///   instantiated for each request, with dependencies injected via [Context].
    /// </param>
    /// <returns>Self for fluent chaining</returns>
    /// <remarks>
    ///   This is REQUIRED for MCP servers. The framework will raise an exception
    ///   if no tool class is configured when processing MCP requests.
    /// </remarks>
    function SetToolClass(AClass: TClass): IMCPConfig;

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
  end;

  [Implements(IMCPConfig)]
  TMCPConfig = class(TJRPCConfiguration, IMCPConfig)
  private
    FWriterRegistry: TMCPWriterRegistry;
    FToolClass: TClass;
    FServerVersion: string;
    FServerName: string;
  public
    constructor Create(AApp: IJRPCApplication); override;
    destructor Destroy; override;
    procedure AfterConstruction; override;

    function SetToolClass(AClass: TClass): IMCPConfig;
    function SetServerName(const AName: string): IMCPConfig;
    function SetServerVersion(const AVersion: string): IMCPConfig;
    function RegisterWriter(AClass: TCustomWriterClass): IMCPConfig;
    function GetWriters: TMCPWriterRegistry;

    function CreateDefaultTool: TObject;
    function GetDefaultToolClass: TClass;

    property ToolClass: TClass read FToolClass write FToolClass;
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
end;

procedure TMCPConfig.AfterConstruction;
begin
  inherited;
  FServerName := 'MCPServer';
  FServerVersion := '1.0';
end;

destructor TMCPConfig.Destroy;
begin
   FWriterRegistry.Free;
  inherited;
end;

function TMCPConfig.CreateDefaultTool: TObject;
begin
  if not Assigned(FToolClass) then
    raise EJRPCException.Create('Default tool not found');

  Result := TRttiUtils.CreateInstance(FToolClass);
end;

function TMCPConfig.GetDefaultToolClass: TClass;
begin
  if not Assigned(FToolClass) then
    raise EJRPCException.Create('Default tool not found');

  Result := FToolClass;
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
  FToolClass := AClass;
  Result := Self;
end;

initialization
  TJRPCConfigClassRegistry.Instance.RegisterConfigClass(TMCPConfig);

end.
