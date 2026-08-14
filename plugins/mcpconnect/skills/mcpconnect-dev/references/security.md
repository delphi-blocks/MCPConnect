# Security: tokens, OAuth and CORS

An MCP server exposes executable methods to a language model on someone's behalf, so who is calling matters. MCPConnect offers two levels: a static shared token for closed setups, and a full OAuth 2.1 protected-resource mode for anything reachable from the outside. Contents:

- [CORS and origin checks](#cors-and-origin-checks)
- [Static bearer token](#static-bearer-token)
- [OAuth 2.1 protected resource](#oauth-21-protected-resource)
- [Token validators](#token-validators)
- [Using the caller's identity](#using-the-callers-identity)

Note that stdio servers have no network surface: the client spawns the process and owns it, so none of this applies there. Everything below is about the HTTP transports.

## CORS and origin checks

In the `.Security` section of `IMCPConfig`:

```pascal
.Security
  .SetCORS(True)
  .SetAllowedMethods(['GET', 'POST', 'OPTIONS'])
  .SetAllowedOrigins(['https://app.example.com'])
  .SetCookieSecure(True)          // only if session ids travel in a cookie
.BackToMCP
```

A request whose `Origin` is not allowed is rejected with 403 before any MCP work happens. Leaving `SetAllowedOrigins` unset means no origin restriction — acceptable while developing on localhost, not for a deployed server. The demos make this explicit by restricting origins only outside `DEBUG` builds.

## Static bearer token

The simplest scheme: one shared secret, checked on every request.

```pascal
uses
  MCPConnect.Configuration.Auth;

AServer
  .Plugin.Configure<IAuthTokenConfig>
    .SetToken(GetEnvironmentVariable('MCP_TOKEN'))
    .SetTokenLocation(TAuthTokenLocation.Bearer)   // Bearer | Cookie | Header
    .SetTokenCustomHeader('X-Api-Key')             // only with Location = Header
  .ApplyConfig
```

This suits a server behind a trusted boundary — a private network, a single known client. It identifies nobody: every caller is the same caller, so nothing downstream can make per-user decisions. Read the token from the environment or a config file; a literal in source ends up in version control.

## OAuth 2.1 protected resource

For a network-reachable server, act as an OAuth protected resource: an external authorization server issues access tokens, and MCPConnect validates them and publishes the discovery metadata clients need to find that authorization server.

```pascal
uses
  MCPConnect.Configuration.Auth,
  MCPConnect.Security.Token;

AServer
  .Plugin.Configure<IOAuthConfig>
    .SetResource('https://mcp.example.com')          // this server's canonical URL
    .AddAuthorizationServer('https://login.example.com')
    .AddTrustedIssuer('https://login.example.com/v2.0')
    .SetTokenValidatorClass(TJoseTokenValidator)
    .SetAudience('api://mcp-server')
    .AddRequiredScope('mcp.invoke')
    .AddScopesSupported('openid')
    .AddScopesSupported('email')
    .SetClockSkew(60)                                 // seconds
    .SetKeyCacheTTL(3600)                             // JWKS cache lifetime
  .ApplyConfig
```

This publishes `/.well-known/oauth-protected-resource` describing the resource and its authorization servers, and answers unauthenticated requests with a 401 carrying the appropriate `WWW-Authenticate` challenge, which is how a compliant MCP client discovers where to send the user to log in.

`EnableMetadataProxy(upstreamIssuer)` additionally exposes the upstream authorization server's metadata through `/oauth-proxy`, for clients that cannot reach the issuer's own discovery endpoint directly.

Each configuration knob does real work, so set them deliberately:

- **`SetAudience`** — reject tokens minted for a different API. Without it, any token from a trusted issuer is accepted, including tokens issued to other applications.
- **`AddRequiredScope`** — reject genuine tokens that lack the scope this server needs; the caller gets `insufficient_scope`.
- **`AddTrustedIssuer`** — the set of issuers whose signatures are trusted. Everything else is rejected regardless of how well formed it is.
- **`SetClockSkew`** — tolerance on `exp` / `nbf`; 60 seconds is the default and is usually right.

Configuration is validated at startup and warns loudly about the two dangerous states: OAuth enabled with no validator registered (every token rejected), and a decode-only validator registered (signatures not checked at all).

Keep issuer URLs, audiences and client ids out of source. The OAuth demo (`Demo/MCPOAuthServer`) reads them from a `.env` file through a small `TEnvironment` helper.

## Token validators

A validator implements `ITokenValidator` (`MCPConnect.Security.Token`):

```pascal
function Validate(AContext: TJRPCContext; const AToken: string;
  AAccessToken: TMCPAccessToken): TTokenValidationResult;
```

One instance is built per request through RTTI and released at the end of it, so the class needs a parameterless constructor, must be reference counted (descend from `TInterfacedObject`, or from `TTokenValidatorBase` which handles it), and must not keep state between requests. Fill `AAccessToken` **only** when validation succeeds — everything downstream treats a populated access token as trusted. Report failures by returning `TTokenValidationResult.Fail(...)`, not by raising: expired and forged tokens are normal outcomes, not exceptional ones. The `ErrorDescription` reaches the client, so keep internal details out of it.

Three implementations ship with the framework:

| Class | Behaviour |
|-------|-----------|
| `TDecodeOnlyTokenValidator` | Decodes the token and checks nothing — no signature, issuer, audience or expiry. **Development only**; it accepts a token anyone can forge. |
| `TClaimsTokenValidator` | Verifies issuer, audience, scopes and expiry, and resolves the signing key from the issuer's JWKS, but leaves `CheckSignature` to a descendant. |
| `TJoseTokenValidator` | `TClaimsTokenValidator` plus real signature verification via [delphi-jose-jwt](https://github.com/paolo-rossi/delphi-jose-jwt). This is the one to use in production. |

`TJoseTokenValidator` lives in `MCPConnect.Security.Token.JOSE` and compiles only when `DELPHI_JOSE_JWT` is defined in `MCPConnect.inc` (it is, by default) and the JOSE library is present under `Libs\JOSE`. Guard the choice if the project must build without that dependency:

```pascal
{$IFDEF DELPHI_JOSE_JWT}
.SetTokenValidatorClass(TJoseTokenValidator)
{$ELSE}
.SetTokenValidatorClass(TClaimsTokenValidator)
{$ENDIF}
```

Writing a custom validator is worthwhile for opaque tokens or introspection endpoints; for ordinary JWTs, derive from `TClaimsTokenValidator` and override only what differs rather than reimplementing claim checking.

## Using the caller's identity

Once OAuth is configured, the decoded claims are available to any tool, resource or prompt class through `[Context]`:

```pascal
uses
  MCPConnect.MCP.Types;

type
  TAccountTool = class
  private
    [Context] FToken: TMCPAccessToken;
  public
    [McpTool('my_orders', 'List the orders belonging to the authenticated user', 'readonly')]
    function MyOrders: TOrders;
  end;

function TAccountTool.MyOrders: TOrders;
begin
  Result := LoadOrdersFor(FToken.Subject);
end;
```

`TMCPAccessToken` exposes `Subject`, `Name`, `EMail`, `EmailVerified`, `PreferredUsername`, `GivenName`, `FamilyName`, `Scope`, `Issuer`, `Audience`, `ClientId`, `Expiration`, `IssuedAt`, `NotBefore`, and `Payload` for any claim without a named property.

Scope the data, don't just read the identity: a tool that takes a customer id as a parameter and returns that customer's data will happily return *anyone's* data, because the model chooses the argument. Derive the identity from `FToken.Subject` and treat parameters as filters within what that subject is already allowed to see.
