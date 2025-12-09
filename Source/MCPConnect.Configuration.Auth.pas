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
unit MCPConnect.Configuration.Auth;

interface

uses
  System.Classes, System.SysUtils,
  MCPConnect.Configuration.Core;

{$SCOPEDENUMS ON}

type
  /// <summary>
  ///   Specifies where the authentication token should be extracted from
  ///   in incoming HTTP requests.
  /// </summary>
  TAuthTokenLocation = (
    /// <summary>HTTP Authorization header with Bearer scheme (e.g., "Authorization: Bearer token")</summary>
    Bearer,
    /// <summary>HTTP Cookie (requires SetTokenCustomHeader to specify cookie name)</summary>
    Cookie,
    /// <summary>Custom HTTP header (requires SetTokenCustomHeader to specify header name)</summary>
    Header
  );

  /// <summary>
  ///   Configuration interface for token-based authentication in JSON-RPC servers.
  ///   Supports Bearer tokens, cookie-based authentication, and custom header tokens.
  ///   Inherits fluent API methods (ApplyConfig, BackToApp) from IJRPCConfiguration.
  /// </summary>
  /// <remarks>
  ///   Token authentication is checked before processing JSON-RPC requests.
  ///   If the token doesn't match, the request is rejected with an authentication error.
  ///   This is a simple token comparison - for more complex authentication schemes,
  ///   implement custom authentication logic in your transport layer.
  /// </remarks>
  /// <example>
  ///   <code>
  ///   // Bearer token authentication:
  ///   FJRPCServer.Plugin.Configure&lt;IAuthTokenConfig&gt;
  ///     .SetToken('my-secret-token-12345')
  ///     .SetTokenLocation(TAuthTokenLocation.Bearer)
  ///     .ApplyConfig;
  ///   // Client must send: Authorization: Bearer my-secret-token-12345
  ///
  ///   // Cookie-based authentication:
  ///   FJRPCServer.Plugin.Configure&lt;IAuthTokenConfig&gt;
  ///     .SetToken('session-value')
  ///     .SetTokenLocation(TAuthTokenLocation.Cookie)
  ///     .SetTokenCustomHeader('SessionId')
  ///     .ApplyConfig;
  ///   // Client must send: Cookie: SessionId=session-value
  ///
  ///   // Custom header authentication:
  ///   FJRPCServer.Plugin.Configure&lt;IAuthTokenConfig&gt;
  ///     .SetToken('api-key-xyz')
  ///     .SetTokenLocation(TAuthTokenLocation.Header)
  ///     .SetTokenCustomHeader('X-API-Key')
  ///     .ApplyConfig;
  ///   // Client must send: X-API-Key: api-key-xyz
  ///   </code>
  /// </example>
  IAuthTokenConfig = interface(IJRPCConfiguration)
  ['{5E98537E-1A8F-44D9-8100-CB3CCC7C0BFC}']
    /// <summary>
    ///   Sets the authentication token that will be compared against incoming requests.
    /// </summary>
    /// <param name="AToken">The token string to match (case-sensitive comparison)</param>
    /// <returns>Self for fluent chaining</returns>
    function SetToken(const AToken: string): IAuthTokenConfig;

    /// <summary>
    ///   Specifies where to extract the authentication token from HTTP requests.
    /// </summary>
    /// <param name="ALocation">Token location: Bearer (default), Cookie, or Header</param>
    /// <returns>Self for fluent chaining</returns>
    function SetTokenLocation(ALocation: TAuthTokenLocation): IAuthTokenConfig;

    /// <summary>
    ///   Sets the custom header or cookie name when using Cookie or Header locations.
    ///   Required when SetTokenLocation is Cookie or Header, ignored for Bearer.
    /// </summary>
    /// <param name="ACustomHeader">
    ///   For Cookie: the cookie name to extract
    ///   For Header: the HTTP header name to extract (e.g., 'X-API-Key')
    /// </param>
    /// <returns>Self for fluent chaining</returns>
    function SetTokenCustomHeader(const ACustomHeader: string): IAuthTokenConfig;
  end;

  [Implements(IAuthTokenConfig)]
  TAuthTokenConfig = class(TJRPCConfiguration, IAuthTokenConfig)
  private
    FToken: string;
    FLocation: TAuthTokenLocation;
    FCustomHeader: string;
  public
    function SetToken(const AToken: string): IAuthTokenConfig;
    function SetTokenLocation(ALocation: TAuthTokenLocation): IAuthTokenConfig;
    function SetTokenCustomHeader(const ACustomHeader: string): IAuthTokenConfig;

    property Token: string read FToken write FToken;
    property Location: TAuthTokenLocation read FLocation write FLocation;
    property CustomHeader: string read FCustomHeader write FCustomHeader;

  end;

implementation

{ TAuthTokenConfig }

function TAuthTokenConfig.SetToken(
  const AToken: string): IAuthTokenConfig;
begin
  FToken := AToken;
  Result := Self;
end;

function TAuthTokenConfig.SetTokenCustomHeader(
  const ACustomHeader: string): IAuthTokenConfig;
begin
  FCustomHeader := ACustomHeader;
  Result := Self;
end;

function TAuthTokenConfig.SetTokenLocation(
  ALocation: TAuthTokenLocation): IAuthTokenConfig;
begin
  FLocation := ALocation;
  Result := Self;
end;

initialization
  TJRPCConfigClassRegistry.Instance.RegisterConfigClass(TAuthTokenConfig);

end.
