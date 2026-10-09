# Authorizing Tools, Resources and Prompts

Authentication tells you who the caller is; authorization decides what that caller can see and use. Checking the scopes by hand inside every tool works, but it is easy to forget one, and the tool still shows up in the list the client hands to the model. MCPConnect lets you declare the requirement once, next to the code it protects.

::: info
The scope-based authorization described here (`McpRequiredScope`, `RequireScope` and the custom authorizers) is available only in the latest version of MCPConnect on [GitHub](https://github.com/delphi-blocks/MCPConnect).
:::

## Declaring the Required Scopes

The `McpRequiredScope` attribute lists the scopes the caller's token must **all** carry, separated by commas or semicolons. It works on tools, resources, resource templates, App UIs and prompts:

```pascal
uses
  MCPConnect.MCP.Attributes;

type
  [McpRequiredScope('orders:read')]
  TOrderTools = class
  public
    [McpTool('list_orders', 'Lists the orders')]
    function ListOrders: TArray<TOrder>;

    [McpTool('delete_order', 'Deletes an order')]
    [McpRequiredScope('orders:write, orders:admin')]
    function DeleteOrder([McpParam('id', 'Order id')] const AId: string): Boolean;
  end;
```

On a class the attribute applies to everything the class registers; on a method it adds to the class requirements. Here `list_orders` needs `orders:read`, while `delete_order` needs `orders:read`, `orders:write` and `orders:admin`. Items without the attribute stay open to every caller, exactly as before.

Not to be confused with `[McpScope]`, which namespaces the tool names of a class and has nothing to do with OAuth.

The scopes come from the token the request was authenticated with: the `scope` claim of the JWT with OAuth (or `scp`, for Microsoft Entra ID), or whatever an API key validator wrote into `AIdentity.Scope` (see [OAuth Support](./oauth.md) and the `IAuthTokenConfig.SetTokenValidator` plugin). The same attribute works with both. Scopes are compared whole and case-sensitively: `orders` does not satisfy `orders:read`.

## What the Client Sees

The check runs on every MCP method that exposes an item:

| Method | A denied item... |
|---|---|
| `tools/list`, `resources/list`, `resources/templates/list`, `prompts/list` | is left out of the list |
| `tools/call`, `resources/read`, `prompts/get` | gets the same error as a missing item |
| `completion/complete` | gets the same error as a missing prompt or resource |
| `subscriptions/listen` | is left out of the acknowledged `resourceSubscriptions`, like an unknown uri |

Three choices are worth noting:

- **A denied item looks like a missing one.** A client that is not allowed to call `delete_order` gets exactly the error it would get if the tool did not exist — Invalid Params (`-32602`), `Tool [delete_order] not found` — so the server does not reveal what it hides. The real reason, with the scopes required and the scopes the token carried, goes to the server log, where you need it when a caller reports that a tool "disappeared".
- **The check is fail-closed.** A caller without a token, such as a STDIO client or any client of a server with no authentication configured, cannot see or use any item that requires a scope.
- **Lists are filtered before they are paged.** A page never comes back short because of a hidden item, and no cursor points at one.

The check lives in the MCP API itself, not in a middleware: a middleware can be removed, which would silently open everything it closed, and `resources/templates/list`, `completion/complete` and `subscriptions/listen` run no chain at all. It applies the same way to clients served through the legacy compatibility plugin.

::: warning Caching
A list filtered per caller is only valid for that caller. Results default to `ScopePrivate`, which is safe; a server that calls `SetCacheHints(..., TCacheScope.ScopePublic)` on a section with scoped items lets a shared cache serve one caller's list to another.
:::

## Registering Without Attributes

Classes registered programmatically — for example from C++Builder, which cannot carry Delphi attributes — get the same protection with `RequireScope`, on the tool builder or, after registration, on the section that holds the item, by name (tools and prompts) or by uri (resources, App UIs and uri templates):

```pascal
FJRPCServer.Plugin.Configure<IMCPConfig>
  .Tools
    .RegisterTool(TOrderService, 'DeleteOrder', 'delete_order', 'Deletes an order')
      .WithParam('AId', 'id', 'Order id')
      .RequireScope('orders:write')
    .EndTool
    .RequireScope('list_orders', 'orders:read')
  .BackToMCP
  .Resources
    .RequireScope('res://orders/{id}', 'orders:read')
  .BackToMCP
  .Prompts
    .RequireScope('summarize_orders', 'orders:read')
  .BackToMCP
  .ApplyConfig;
```

`RequireScope` always adds to the scopes already declared, and raises `EMCPException` when the item does not exist. Attributes found on a class are honored even when it is registered programmatically, so registering a class by hand never drops its restrictions.

## Custom Rules

Scopes are the recommended way, but not the only one. With `SetAuthorizer` you replace the default check with your own rule. It is called for every item of a list and for the item a call asks for, and returns `True` to allow access. It receives:

- the request context;
- a `TMCPAuthItem` describing the item: its `Kind` (`Tool`, `Resource`, `Template`, `UI`, `Prompt`), its name, its uri (for a template, the uri template when listing and the uri actually requested when reading), the declared `RequiredScopes` and the `Tags`;
- the caller's identity, never `nil`: without authentication it is an empty token.

A custom rule replaces the default one, so to keep the scope check and add to it, call `TMCPScopeAuthorizer.Check` first. Tags make it easy to mark items for your own rules without new attributes:

```pascal
uses
  MCPConnect.MCP.Authorization;

FJRPCServer.Plugin.Configure<IMCPConfig>
  .Security
    .SetAuthorizer(
      function (AContext: TJRPCContext; const AItem: TMCPAuthItem;
        AIdentity: TMCPAccessToken): Boolean
      begin
        Result := TMCPScopeAuthorizer.Check(AItem, AIdentity);
        // Tools tagged "admin", e.g. [McpTool('purge', 'Purges the archive', 'admin')]
        if Result and AItem.Tags.Exists('admin') then
          Result := AdminRepository.IsAdmin(AIdentity.Subject);
      end)
  .BackToMCP
  .ApplyConfig;
```

The function is called concurrently from every request thread and must be thread-safe; an exception raised inside it is logged and counts as a denial. From C++Builder, register a class implementing `IMCPAuthorizer` with `SetAuthorizerClass`: one instance is created per request, not per item, so it needs a parameterless constructor and must be reference counted (descending from `TInterfacedObject`). The two registrations replace each other, and passing `nil` to either one goes back to the default check.

## Checking Scopes Inside a Tool

When a tool needs finer decisions than "may call it or not" — for example returning less data to some callers — it can read the identity through `[Context]` and test the scopes itself:

```pascal
TOrderTools = class
private
  [Context]
  FIdentity: TMCPAccessToken;
  ...
end;

if FIdentity.HasScope('orders:admin') then
  ...
```

`HasScope` matches whole, case-sensitive scopes; `HasScopes` requires them all and `MissingScopes` returns the ones the token lacks.
