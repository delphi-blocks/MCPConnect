# OAuth Support in MCPConnect

MCPConnect can protect an MCP server with OAuth 2.1 bearer tokens, following the
[MCP Authorization specification](https://modelcontextprotocol.io/specification/draft/basic/authorization). This
document explains:

1. What MCPConnect does (and does not do) for OAuth.
2. How to configure it (`IOAuthConfig`).
3. How to debug an OAuth-protected server locally using MCPJam Inspector, a public
   authorization server (e.g. an OpenID Connect provider), and a Cloudflare Tunnel.

## 1. Overview

MCPConnect acts as an OAuth 2.1 **resource server** — it does **not** implement an authorization
server (no `/authorize`, `/token`, or `/register` endpoints, no user login, no consent screen).
Authentication is delegated to an external authorization server (e.g. Microsoft Entra ID, Auth0,
Keycloak, Okta, or any OpenID Connect provider).

When OAuth is enabled, MCPConnect's HTTP transport (`TMCPTransportHandler` in
`MCPConnect.Transport.Base`) does the following on every incoming request:

- Serves the [RFC 9728](https://www.rfc-editor.org/rfc/rfc9728) Protected Resource Metadata
  document — the resource URL, the configured authorization server(s), and supported scopes — at
  the URL RFC 9728 §3.1 prescribes: the well-known segment goes *between* the host and the
  resource's path, so `SetResource('https://mcp.example.com/mcp')` publishes at
  `GET /.well-known/oauth-protected-resource/mcp`. A resource that is just an origin publishes at
  the bare `/.well-known/oauth-protected-resource`, which is also still served as a fallback.
- Rejects unauthenticated requests with `401 Unauthorized` and a `WWW-Authenticate: Bearer
  realm="...", resource_metadata="..."` header, per the MCP Authorization spec.
- Accepts requests carrying `Authorization: Bearer <token>` **when a registered token validator
  accepts them** (see [Section 3.2](#32-token-validation)), and injects the validated claims into the
  request context as `TMCPAccessToken`.
- Optionally proxies and patches the authorization server's discovery document (see
  [Section 4](#4-the-metadata-proxy)).

All of this is wired into `CheckOAuth`, called from `ProcessRequest` before any JSON-RPC handling
takes place. CORS headers (see [Section 3.3](#33-cors)) are injected **before** these checks, so
that error responses (401, metadata documents) are readable by browser-based clients too.

## 2. Basic Configuration

OAuth is configured through the `IOAuthConfig` fluent interface, obtained via
`TJRPCServer.Plugin.Configure<IOAuthConfig>`:

```delphi
uses
  MCPConnect.Configuration.Auth;

// ...

AServer
  .Plugin.Configure<IOAuthConfig>
    .SetResource('https://mcp.example.com/mcp')
    .SetRealm('mcp')
    .AddAuthorizationServer('https://auth.example.com')
    .AddScopesSupported('openid')
    .AddScopesSupported('profile')
  .ApplyConfig
  // ... other plugins
;
```

| Method | Purpose |
|---|---|
| `SetResource(AUrl)` | The canonical, public URL of this MCP server (the `resource` in RFC 8707 terms). **Must be called first** — other methods derive URLs from it. |
| `SetRealm(ARealm)` | The `realm` value sent in the `WWW-Authenticate` header. Defaults to `'mcp'`. |
| `AddAuthorizationServer(AUrl)` | Registers an external authorization server. Can be called multiple times; all are listed in the protected resource metadata. |
| `AddScopesSupported(AScope)` | Advertises a supported OAuth scope. Can be called multiple times. |
| `EnableMetadataProxy(AUpstreamIssuer)` | See [Section 4](#4-the-metadata-proxy). Registers a local proxy URL as the authorization server instead of `AUpstreamIssuer` directly. |
| `SetTokenValidatorClass(AClass)` | Registers the class that validates bearer tokens. Without it the server rejects every bearer token — see [Section 3.2](#32-token-validation). |
| `SetAudience(AAudience)` | Value the token's `aud` claim must contain. Defaults to `SetResource`. |
| `AddRequiredScope(AScope)` | Scope the token must carry, else `insufficient_scope`. Can be called multiple times. |
| `SetClockSkew(ASeconds)` | Tolerance on `exp`/`nbf`, in seconds. Defaults to 60. |
| `SetKeyCacheTTL(ASeconds)` | Lifetime of the cached JWKS, in seconds. Defaults to 3600. |

If `AuthorizationServers` is empty, OAuth enforcement is fully disabled — `CheckOAuth` short-circuits
and every request is allowed through, regardless of `Authorization` headers. This lets you enable
OAuth only when at least one authorization server has been configured.

### 2.1 Reading the token

Once a request carries an `Authorization: Bearer <token>` that the registered validator accepted, the
claims are available in any `[Context]`-injected parameter as `TMCPAccessToken`
(`MCPConnect.MCP.Types`), e.g.:

```delphi
[McpTool('whoami')]
function WhoAmI([Context] AToken: TMCPAccessToken): string;
begin
  Result := AToken.Subject; // maps to the JWT "sub" claim
end;
```

### 2.2 Combining with token/cookie authentication

`IOAuthConfig` and `IAuthTokenConfig` (simple static-token / cookie authentication) are independent
plugins and can coexist, but are generally alternatives for different deployment scenarios — most
servers will use one or the other.

## 3. Production Considerations

### 3.1 HTTPS

`SetResource` must be set to the **real, publicly reachable URL** of your server. OAuth 2.1 resource
indicators (RFC 8707) require this to be the exact URL MCP clients connect to. In production this
must be an `https://` URL — see [Section 5](#5-debugging-with-mcpjam-entra-id-and-a-cloudflare-tunnel)
for why `http://localhost` does not work against most real-world authorization servers, even for
local testing.

### 3.2 Token validation

Token validation is delegated to a class you register in the configuration:

```delphi
uses
  MCPConnect.Security.Token;

AServer
  .Plugin.Configure<IOAuthConfig>
    .SetResource('https://mcp.example.com/mcp')
    .AddAuthorizationServer('https://auth.example.com')
    .SetTokenValidatorClass(TMyTokenValidator)
  .ApplyConfig;
```

Any class implementing `ITokenValidator` qualifies — there is no base class to derive from. It needs
a parameterless constructor and must be reference counted, so in practice:

```delphi
type
  TMyTokenValidator = class(TInterfacedObject, ITokenValidator)
  public
    function Validate(AContext: TJRPCContext; const AToken: string;
      AAccessToken: TMCPAccessToken): TTokenValidationResult;
  end;
```

Everything the validator needs it reads from `AContext`: the `TJRPCServer`, and through it the
`TOAuthConfig` carrying the trusted issuers, the audience, the required scopes, the clock skew and
the metadata provider.

Two things follow from this, and both matter before exposing a server:

- **Without a registered validator the server is fail-closed**: every request carrying a bearer token
  is answered with `401`. This is a change from earlier versions, which accepted any well-formed JWT.
- **Three validators come in the box**, and only one of them proves a token is genuine:
  - `TJoseTokenValidator` (`MCPConnect.Security.Token.JOSE`) — everything below **plus the
    signature**, verified with the [Delphi JOSE library](https://github.com/paolo-rossi/delphi-jose-jwt)
    against the key the identity provider publishes. This is the one to register in production. It
    needs the JOSE library at compile time (under `Libs\JWT`, switched by the `DELPHI_JOSE_JWT`
    define in `Source/MCPConnect.inc`) and the OpenSSL libraries at run time.
  - `TClaimsTokenValidator` — checks `iss`, `aud`, `exp`/`nbf`, the required scopes, rejects
    `"alg": "none"`, and verifies that the `kid` names a key the issuer actually publishes. It stops
    expired, foreign and unsigned tokens, but **not** a forged one: its `CheckSignature` hook is
    empty.
  - `TDecodeOnlyTokenValidator` — decodes the payload and verifies nothing. Local development only.

The framework supplies the pieces around the signature check too: an `IOAuthMetadataProvider` that
fetches and caches the authorization server's discovery document and JWKS (with TTLs, key-rotation
refresh and stale-if-error), the issuer/audience/scope/clock-skew options, and the `401` challenge
plumbing. See [`token-validation.md`](token-validation.md) for the full design and for how to write
a validator.

### 3.3 CORS

If your MCP server will be accessed from a browser-based client (like MCPJam Inspector), enable
CORS via `IMCPConfig`:

```delphi
AServer
  .Plugin.Configure<IMCPConfig>
    .Security
      .SetCORS(True)
      .SetAllowedMethods(['GET', 'POST', 'OPTIONS'])
      .SetAllowedOrigins(['https://your-debugger-origin'])
    .BackToMCP
  .ApplyConfig;
```

CORS headers, including `Access-Control-Expose-Headers` for `WWW-Authenticate` (needed by browser
JavaScript to read the challenge on a 401 response) and the session header, are injected on every
response — including error responses and the OAuth well-known endpoints — so the browser-based
OAuth discovery flow works correctly.

### 3.4 WebBroker routing

The Indy and STDIO transports hand every request to `TMCPTransportHandler`, so the OAuth
endpoints are served without further setup. WebBroker is different: it routes by path, and a
`TJRPCDispatcher` only receives requests matching its own `PathInfo` mask. A dispatcher mounted at
`/mcp` therefore never sees `/.well-known/...`, and the metadata document — and the
[metadata proxy](#4-the-metadata-proxy) — would 404.

Give the well-known paths a route of their own, pointing at the same server:

```delphi
procedure TWebModule1.WebModuleCreate(Sender: TObject);
begin
  FJRPCServer := TJRPCServer.Create(Self);
  TServerConfigurator.ConfigureServer(FJRPCServer);

  // The MCP endpoint itself
  FJRPCDispatcher := TJRPCDispatcher.Create(Self);
  FJRPCDispatcher.PathInfo := '/mcp';
  FJRPCDispatcher.Server := FJRPCServer;

  // The OAuth discovery endpoints, served by the same server: it picks the right one
  // from the request path
  FWellKnownDispatcher := TJRPCDispatcher.Create(Self);
  FWellKnownDispatcher.PathInfo := '/.well-known/*';
  FWellKnownDispatcher.Server := FJRPCServer;
end;
```

`PathInfo` is a `TMask`, so `*` covers every well-known path in one route. Only add this second
dispatcher when OAuth is configured: without an authorization server, `CheckOAuth` lets requests
straight through and a `.well-known` request would fall into normal MCP request handling and fail.

## 4. The Metadata Proxy

Some authorization servers support PKCE in practice but do not advertise it (an empty or missing
`code_challenge_methods_supported` field in their `/.well-known/openid-configuration` document).
Microsoft Entra ID is a well-known example of this. Recent revisions of the MCP Authorization spec
require PKCE support to be advertised, so strict MCP clients refuse to proceed against such
authorization servers, even though the actual authorization flow would work fine.

`EnableMetadataProxy` works around this without modifying the external authorization server:

```delphi
AServer
  .Plugin.Configure<IOAuthConfig>
    .SetResource('https://mcp.example.com/mcp')
    .EnableMetadataProxy('https://auth.example.com')  // upstream issuer, e.g. an Entra ID tenant
    .AddScopesSupported('openid')
  .ApplyConfig;
```

This call:

1. Stores the upstream issuer URL (`https://auth.example.com` in the example).
2. Registers a **local** URL — `<resource-origin>/oauth-proxy` (e.g.
   `https://mcp.example.com/oauth-proxy`) — as the authorization server, instead of the upstream
   URL directly (equivalent to calling `AddAuthorizationServer` with the local proxy URL).

At runtime, when a client requests the authorization server's metadata (trying any of the
well-known URL shapes a spec-compliant client may use — path-insertion or path-appending, for
both `oauth-authorization-server` and `openid-configuration`), MCPConnect:

1. Fetches `<upstream-issuer>/.well-known/openid-configuration` server-side.
2. If `code_challenge_methods_supported` is missing or empty, adds `["S256"]`.
3. Rewrites `issuer` to the local proxy URL — see [the trade-off](#41-the-issuer-rewrite) below.
4. Passes everything else through untouched — `authorization_endpoint`, `token_endpoint` and every
   other field — so the authorization and token exchanges still happen directly against the real
   upstream authorization server. Only the discovery document is patched.

### 4.1 The `issuer` rewrite

RFC 8414 §3.3 (and OpenID Connect Discovery §4.3) require a client to check that the `issuer` in a
metadata document matches the URL it fetched the document from. Serving the upstream document
unchanged from a local URL fails that check, which is what the rewrite fixes — and without it,
serving the document locally would be pointless for any client that performs the check.

The cost is that the upstream authorization server keeps its own identity everywhere else. Under
[RFC 9207](https://www.rfc-editor.org/rfc/rfc9207), which OAuth 2.1 requires, the authorization
response carries an `iss` parameter — minted by the upstream server, so it will *not* match the
`issuer` the client just discovered. The same goes for the `iss` of an OpenID Connect ID token.

**A proxy that only patches the document cannot make both checks pass**, and which one your client
performs decides whether this workaround helps or hurts:

| Client validates | Without the rewrite | With the rewrite (current behaviour) |
|---|---|---|
| Document `issuer` vs fetch URL (RFC 8414 §3.3) | rejects | accepts |
| Authorization response `iss` (RFC 9207) | accepts | rejects |

If your client fails with an issuer mismatch *after* the redirect back from the authorization
server, this is why — and the answer is not the proxy but fixing the upstream document, either by
having the authorization server advertise `code_challenge_methods_supported` or by pointing
`AddAuthorizationServer` straight at it and dropping `EnableMetadataProxy`.

MCPConnect logs a warning at startup whenever the proxy is enabled, so this is visible before a
flow is attempted rather than only from the client's error.

Access tokens are unaffected either way: they carry the **upstream** `iss`, and MCPConnect already
expects that — with the proxy enabled, `TrustedIssuers` resolves to the upstream issuer, not to the
local proxy URL.

### 4.2 Other notes and limitations

- This is deliberately a "just patch the document" proxy, not a full reverse proxy of the OAuth
  flow. If the upstream server is unreachable or returns a non-200 response, MCPConnect returns
  `502 Bad Gateway`.
- The upstream document is fetched from `openid-configuration` only. An authorization server that
  publishes solely the RFC 8414 `oauth-authorization-server` document cannot currently be proxied
  (signing-key discovery, which is a separate path, does try both).
- On WebBroker, the `.well-known` paths need a route of their own — see
  [Section 3.4](#34-webbroker-routing).

## 5. Debugging with MCPJam, Entra ID, and a Cloudflare Tunnel

This section walks through setting up a full local test loop:
[MCPJam Inspector](https://github.com/MCPJam/inspector)'s OAuth Debugger as the MCP client, an
OpenID Connect provider (Microsoft Entra ID in this example) as the authorization server, and a
[Cloudflare Tunnel](https://developers.cloudflare.com/cloudflare-one/connections/connect-networks/)
to expose your local MCPConnect server over HTTPS with a domain you control.

All values below (`example.com`, `mcp.example.com`, `contoso`, client/tenant IDs, etc.) are
placeholders — replace them with your own domain and tenant details.

### 5.1 Why `http://localhost` is not enough

Per RFC 8707 (Resource Indicators), MCP clients send a `resource` parameter to the authorization
server, set to the exact URL of the MCP server they are connecting to. Real-world authorization
servers validate this value against a registered API identifier:

- Microsoft Entra ID requires an **Application ID URI** to match, and refuses to accept
  `http://` (non-TLS) URIs for it — it must be `https://` on a domain your tenant has verified, or
  the `api://` scheme.
- A bare DNS `CNAME` to a tunnel hostname is not enough either: the tunnel's TLS certificate is
  issued for its own hostname, not for your custom domain, so a browser or Microsoft's own
  validation will see a certificate mismatch.

The reliable fix is to expose your local server over HTTPS on a **domain you actually own**, using
a tunnel that terminates TLS for that domain — Cloudflare Tunnel does this automatically when your
domain's DNS is managed by Cloudflare.

### 5.2 Set up the tunnel

```bash
# Install cloudflared
winget install --id Cloudflare.cloudflared   # or see https://developers.cloudflare.com/cloudflare-one

cloudflared tunnel login                      # authorize against your Cloudflare account/domain
cloudflared tunnel create mcp-tunnel
cloudflared tunnel route dns mcp-tunnel mcp.example.com
```

Create `%USERPROFILE%\.cloudflared\config.yml`:

```yaml
tunnel: mcp-tunnel
credentials-file: C:\Users\<you>\.cloudflared\<tunnel-id>.json
ingress:
  - hostname: mcp.example.com
    service: http://localhost:8080
  - service: http_status:404
```

Run it:

```bash
cloudflared tunnel run mcp-tunnel
```

`https://mcp.example.com` now forwards to your local server on port 8080, with a valid TLS
certificate for your own domain.

### 5.3 Register an application in Entra ID

1. **Custom domain verification** (one-time, tenant-wide): Microsoft Entra ID → **Custom domain
   names** → **Add custom domain name** → enter `example.com` → add the given TXT record to your
   domain's DNS → **Verify**. This is required before Entra ID will accept an Application ID URI
   under that domain.

2. **Create (or reuse) an App Registration**, then configure:

   - **Authentication** → **Add a platform** → **Web** → redirect URI:
     `http://localhost:6274/oauth/callback` (MCPJam Inspector's default callback, adjust the port
     to match your instance).

     > Use the **Web** platform, not **Single-page application**. MCPJam Inspector's OAuth Debugger
     > redeems the authorization code through its own backend process rather than a direct
     > browser-to-authorization-server request. Microsoft Entra ID only allows that redemption
     > pattern for confidential ("Web") clients; SPA-registered clients are restricted to
     > browser-only, cross-origin token redemption, and will fail with
     > `AADSTS9002327` (see the [troubleshooting table](#56-troubleshooting) below) against a
     > backend-proxied client like this one.

   - **Certificates & secrets** → **New client secret** → copy the value (shown once).

   - **Expose an API**:
     - **Application ID URI** → set it to your resource URL: `https://mcp.example.com/mcp`.
     - **+ Add a scope** → name `access_as_user`, consent `Admins and users`, state `Enabled`.
       This produces the full scope `https://mcp.example.com/mcp/access_as_user`.
     - **Authorized client applications** → **+ Add a client application** → enter this same app's
       Client ID (found on the **Overview** page) → check the `access_as_user` scope → **Add
       application**. This pre-authorizes the client for that scope without a separate consent
       screen (equivalent to adding the permission under **API permissions** and granting admin
       consent).

### 5.4 Configure MCPConnect

```delphi
AServer
  .Plugin.Configure<IOAuthConfig>
    .SetResource('https://mcp.example.com/mcp')
    .EnableMetadataProxy(GetEnvironmentVariable('OIDC_AUTH_SERVER'))  // e.g. https://login.microsoftonline.com/<tenant-id>/v2.0
    .AddTrustedIssuer(GetEnvironmentVariable('OIDC_TOKEN_ISSUER'))    // e.g. https://sts.windows.net/<tenant-id>/
    .SetTokenValidatorClass(TClaimsTokenValidator)
    .AddScopesSupported('openid')
    .AddScopesSupported('email')
    .AddScopesSupported('profile')
    .AddScopesSupported('https://mcp.example.com/mcp/access_as_user')
  .ApplyConfig;
```

Set the `OIDC_AUTH_SERVER` environment variable to your tenant's **v2.0** endpoint —
`https://login.microsoftonline.com/<tenant-id>/v2.0` — not the legacy v1.0 endpoint, which has a
different discovery document shape.

`OIDC_TOKEN_ISSUER` covers a mismatch that catches everyone once. An exposed API left at the default
`requestedAccessTokenVersion` receives **v1.0** access tokens, and their `iss` is
`https://sts.windows.net/<tenant-id>/` — not the v2.0 URL you just discovered against, even though
the whole flow ran there. Decode a token and look at `ver`: `1.0` means you need this line. See
[token-validation.md §3.3.1](token-validation.md#331-when-iss-is-not-the-discovery-url) for the
alternative (switching the API to v2.0 tokens) and for why that one also forces you to set the
audience.

Start (or restart) your MCPConnect server so it's listening on `localhost:8080`, forwarded by the
tunnel to `https://mcp.example.com`.

### 5.5 Run MCPJam Inspector

1. Open MCPJam Inspector (e.g. `http://localhost:6274/servers`).
2. Add a server with URL `https://mcp.example.com/mcp`.
3. In the OAuth Debugger, select protocol version **2025-11-25 (Latest)**.
4. Registration mode: **pre-registered** — enter the Client ID and Client Secret from
   [Section 5.3](#53-register-an-application-in-entra-id).
5. Run the flow. It should walk through: initial request → `401` → resource metadata →
   authorization server metadata (served by MCPConnect's proxy, patched with
   `code_challenge_methods_supported`) → redirect to Entra ID → login/consent → redirect back to
   MCPJam → token exchange → authenticated `initialize` call.

### 5.6 Troubleshooting

| Symptom | Cause | Fix |
|---|---|---|
| Request stays "pending" in the browser-based client forever, breakpoints in your server code never hit, and it stays "pending" even with the server turned off | The client-side OAuth tool failed silently (blocked `fetch`, popup blocked, or a client-side bug) before ever reaching the network — check the browser DevTools Console/Network tabs directly instead of trusting the tool's status UI | Verify CORS headers are present on **every** response, including `401`s and well-known endpoints (fixed by injecting CORS before any auth check in `ProcessRequest`); check for a blocked-popup icon in the browser address bar; verify the exact server URL/port configured in the client |
| `401` response (or the `.well-known/oauth-protected-resource` document) is unreadable by browser JavaScript / blocked by CORS | `Access-Control-Allow-Origin` (and, for reading `WWW-Authenticate`, `Access-Control-Expose-Headers`) missing on error/metadata responses | Already handled by MCPConnect (CORS is injected before any OAuth check) — make sure `SetCORS(True)` is called and your client's origin is allowed |
| `PKCE is REQUIRED for 2025-11-25 protocol, but authorization server does not advertise code_challenge_methods_supported` | The authorization server's discovery document doesn't include `code_challenge_methods_supported`, even though it supports PKCE (common with Entra ID) | Use [`EnableMetadataProxy`](#4-the-metadata-proxy) |
| Redirect goes to `http://<your-server>/authorize` (a path your MCP server doesn't implement) instead of the real authorization server | A "legacy" protocol version was selected (e.g. 2025-03-26) in the debugging client, which does not support delegating to an external authorization server — it assumes the MCP server itself is the authorization server | Select the current protocol version (e.g. 2025-11-25) in the client, not a legacy one |
| Every authenticated request gets `401` with `error="invalid_token"`, and the server log says *"The token issuer is not trusted (expected: https://login.microsoftonline.com/&lt;tid&gt;/v2.0, found: https://sts.windows.net/&lt;tid&gt;/)"* | The exposed API issues **v1.0** access tokens (`"ver": "1.0"`), whose `iss` is the `sts.windows.net` form, while discovery runs against the v2.0 endpoint | Add the token's issuer with `AddTrustedIssuer`, or set `requestedAccessTokenVersion: 2` on the API and then also set `SetAudience` to its client ID — see [token-validation.md §3.3.1](token-validation.md#331-when-iss-is-not-the-discovery-url) |
| A required scope is never satisfied although the token clearly carries it | The `scp` claim holds **bare** scope names (`access_as_user`); the full `https://.../access_as_user` form belongs in the authorization request, not in the claim | Pass the bare name to `AddRequiredScope` |
| `AADSTS9010010: The resource parameter provided in the request doesn't match with the requested scopes` | The `resource` parameter (your MCP server's URL) doesn't correspond to any registered API in the authorization server, so it can't be reconciled with the requested scopes | Expose your resource as an API in the authorization server ([Section 5.3](#53-register-an-application-in-entra-id)) and request a scope that belongs to it, alongside any OIDC scopes |
| `AADSTS500011: The resource principal named <url> was not found in the tenant` | The Application ID URI hasn't been set (or doesn't exactly match the `resource` value) on the authorization server side yet | Complete the "Expose an API" step, matching the URI **exactly** (scheme, host, path, trailing slash) |
| Entra ID refuses to save the Application ID URI: *"You must use a verified domain of the organization"* | Application ID URIs must be on a domain your tenant has verified, or use `api://...` — `http://localhost`, raw IPs, and third-party tunnel domains you don't own never qualify | Use a domain you own, verify it in Entra ID, and expose your server over HTTPS on that domain (a Cloudflare Tunnel, as described above, is one way) |
| `AADSTS9002327: Tokens issued for the 'Single-Page Application' client-type may only be redeemed via cross-origin requests` | The app is registered as a **Single-page application** platform, but the OAuth debugging client redeems the authorization code through its own backend process rather than a genuine direct browser-to-authorization-server request | Register the redirect URI under the **Web** platform instead, and configure a client secret in the debugging client, as described in [Section 5.3](#53-register-an-application-in-entra-id) |
| `Invalid client ID during token exchange. Please verify the client ID is correctly registered` | A generic message some debugging clients show for *any* `invalid_client` error from the authorization server — not necessarily a wrong client ID | Inspect the raw token endpoint response in DevTools Network tab for the actual `error`/`error_description` (or `AADSTS...` code) instead of trusting the paraphrased message |
