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
unit MCPConnect.Configuration.Neon;

interface

uses
  System.Classes, System.SysUtils,
  Neon.Core.Persistence,

  MCPConnect.Configuration.Core;

type
  /// <summary>
  ///   Configuration interface for customizing Neon serialization behavior in
  ///   JSON-RPC servers. Allows you to configure JSON serialization settings
  ///   such as naming conventions, visibility, type handling, and custom serializers.
  /// </summary>
  /// <remarks>
  ///   The Neon library (https://github.com/paolo-rossi/delphi-neon) is used
  ///   internally for all JSON serialization/deserialization in MCPConnect.
  ///   Use this configuration to customize how Delphi objects are converted
  ///   to/from JSON, including:
  ///   - Property naming conventions (camelCase, PascalCase, snake_case)
  ///   - Member visibility (public only, published, etc.)
  ///   - Custom type converters and serializers
  ///   - Date/time formatting
  ///   - Enum handling
  /// </remarks>
  /// <example>
  ///   <code>
  ///   // Configure Neon to use camelCase for JSON properties:
  ///   var
  ///     LNeonConfig: INeonConfiguration;
  ///   begin
  ///     LNeonConfig := TNeonConfiguration.Default;
  ///     LNeonConfig.SetMemberCase(TNeonCase.CamelCase);
  ///     LNeonConfig.SetVisibility([mvPublic, mvPublished]);
  ///
  ///     FJRPCServer.Plugin.Configure&lt;IJRPCNeonConfig&gt;
  ///       .SetNeonConfig(LNeonConfig)
  ///       .ApplyConfig;
  ///   end;
  ///
  ///   // Now all JSON serialization will use camelCase:
  ///   // Delphi property: FirstName -> JSON: "firstName"
  ///   </code>
  /// </example>
  IJRPCNeonConfig = interface(IJRPCConfiguration)
    ['{DAA7F996-C465-4D01-8B70-0C94F85C013A}']
    /// <summary>
    ///   Sets the Neon configuration to use for JSON serialization/deserialization
    ///   in the JSON-RPC server. This configuration applies to all JSON operations
    ///   including request parsing, response generation, and parameter marshaling.
    /// </summary>
    /// <param name="ANeonConfig">
    ///   Neon configuration instance (INeonConfiguration). Use TNeonConfiguration.Default
    ///   as a starting point and customize via its fluent methods (SetMemberCase,
    ///   SetVisibility, RegisterSerializer, etc.)
    /// </param>
    /// <returns>Self for fluent chaining</returns>
    function SetNeonConfig(ANeonConfig: INeonConfiguration): IJRPCNeonConfig;
  end;

  [Implements(IJRPCNeonConfig)]
  TJRPCNeonConfig = class(TJRPCConfiguration, IJRPCNeonConfig)
  private
    FNeonConfig: INeonConfiguration;
  public
    function SetNeonConfig(ANeonConfig: INeonConfiguration): IJRPCNeonConfig;

    property NeonConfig: INeonConfiguration read FNeonConfig write FNeonConfig;
  end;

implementation

{ TJRPCNeonConfig }

function TJRPCNeonConfig.SetNeonConfig(ANeonConfig: INeonConfiguration): IJRPCNeonConfig;
begin
  FNeonConfig := ANeonConfig;
  Result := Self;
end;

initialization
  TJRPCConfigClassRegistry.Instance.RegisterConfigClass(TJRPCNeonConfig);

end.
