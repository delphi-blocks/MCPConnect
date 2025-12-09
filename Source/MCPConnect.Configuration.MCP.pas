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

    function SetServerName(const AName: string): IMCPConfig;
    function SetServerVersion(const AVersion: string): IMCPConfig;
    function RegisterWriter(AClass: TCustomWriterClass): IMCPConfig;
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
    FNamespaceSeparator: string;
    FServerVersion: string;
    FServerName: string;
  public
    constructor Create; override;
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

constructor TMCPConfig.Create;
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

function TMCPConfig.GetNamespaceSeparator: string;
begin
  Result := FNamespaceSeparator;
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

initialization
  TJRPCConfigClassRegistry.Instance.RegisterConfigClass(TMCPConfig);

end.
