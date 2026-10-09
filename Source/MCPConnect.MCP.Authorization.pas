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
unit MCPConnect.MCP.Authorization;

interface

{$SCOPEDENUMS ON}

uses
  System.SysUtils,

  Neon.Core.Tags,
  JRPC.Core,

  MCPConnect.MCP.Types.Base,
  MCPConnect.MCP.Types.Tool,
  MCPConnect.MCP.Types.Resources,
  MCPConnect.MCP.Types.Prompts;

resourcestring
  SMCPAuthorizerFailedFmt = 'Authorizer failed on %s, access denied';

type
  /// <summary>What kind of MCP item an authorizer is asked about.</summary>
  TMCPAuthItemKind = (Tool, Resource, Template, UI, Prompt);

  /// <summary>
  ///   Describes the MCP item (tool, resource, template, App UI or prompt) an
  ///   authorizer is asked about.
  /// </summary>
  TMCPAuthItem = record
  public
    Kind: TMCPAuthItemKind;

    /// <summary>MCP-facing name of the item.</summary>
    Name: string;

    /// <summary>
    ///   Uri of a resource or App UI. For a template it is the uri template when
    ///   listing and the concrete uri requested when reading. Empty for tools and
    ///   prompts.
    /// </summary>
    Uri: string;

    /// <summary>Scopes declared with [McpRequiredScope] or RequireScope.</summary>
    RequiredScopes: TArray<string>;

    /// <summary>
    ///   Tags of the item, so that custom rules (e.g. "role=admin") need no new
    ///   attribute. Owned by the item: do not keep it past the call.
    /// </summary>
    Tags: TAttributeTags;

    class function FromTool(ATool: TMCPTool): TMCPAuthItem; static;
    class function FromResource(AResource: TMCPResource): TMCPAuthItem; static;
    class function FromTemplate(ATemplate: TMCPResourceTemplate; const AUri: string = ''): TMCPAuthItem; static;
    class function FromPrompt(APrompt: TMCPPrompt): TMCPAuthItem; static;

    /// <summary>Short description for logs, e.g. "tool [get_orders]".</summary>
    function ToString: string;
  end;

  /// <summary>
  ///   Decides whether the caller can see and use an MCP item. Called by the list
  ///   operations for every item, and by tools/call, resources/read, prompts/get,
  ///   completion/complete and subscriptions/listen for the requested one.
  /// </summary>
  /// <param name="AContext">Context of the request.</param>
  /// <param name="AItem">The item asked about.</param>
  /// <param name="AIdentity">
  ///   The caller's identity as filled in by the token validator. Never nil: when there
  ///   is no authentication it carries no claims.
  /// </param>
  /// <returns>
  ///   True to allow. A denied item is left out of the lists, and using it gets the
  ///   same error as an item that does not exist.
  /// </returns>
  /// <remarks>
  ///   Called concurrently from every request thread: whatever it captures must be
  ///   thread-safe. An exception is logged and treated as a denial.
  ///   TMCPScopeAuthorizer.Check holds the default rule, to combine with custom ones.
  /// </remarks>
  TMCPAuthorizerFunc = reference to function(AContext: TJRPCContext;
    const AItem: TMCPAuthItem; AIdentity: TMCPAccessToken): Boolean;

  /// <summary>
  ///   Class-based alternative to TMCPAuthorizerFunc, for code that cannot pass an
  ///   anonymous method (e.g. C++Builder). Register it with
  ///   IMCPConfig.Security.SetAuthorizerClass.
  /// </summary>
  /// <remarks>
  ///   One instance is built per request through RTTI and released at the end of it: the
  ///   class needs a parameterless constructor and must be reference counted (descending
  ///   from TInterfacedObject is the usual way).
  /// </remarks>
  IMCPAuthorizer = interface
  ['{4B7E2C19-8D3A-4F56-9A1E-6C0D52B8E7F3}']
    /// <summary>Same contract as TMCPAuthorizerFunc.</summary>
    function Authorize(AContext: TJRPCContext; const AItem: TMCPAuthItem;
      AIdentity: TMCPAccessToken): Boolean;
  end;

  /// <summary>
  ///   The default authorizer: the caller's token must carry every scope the item
  ///   requires. An item that requires none is always allowed; one that requires some
  ///   is denied to a caller without a token (fail-closed).
  /// </summary>
  TMCPScopeAuthorizer = class(TInterfacedObject, IMCPAuthorizer)
  public
    /// <summary>The default rule, to reuse inside a custom authorizer.</summary>
    class function Check(const AItem: TMCPAuthItem; AIdentity: TMCPAccessToken): Boolean; static;

    function Authorize(AContext: TJRPCContext; const AItem: TMCPAuthItem;
      AIdentity: TMCPAccessToken): Boolean;
  end;

  /// <summary>Adapts a TMCPAuthorizerFunc to IMCPAuthorizer.</summary>
  TMCPFuncAuthorizer = class(TInterfacedObject, IMCPAuthorizer)
  private
    FFunc: TMCPAuthorizerFunc;
  public
    constructor Create(const AFunc: TMCPAuthorizerFunc);

    function Authorize(AContext: TJRPCContext; const AItem: TMCPAuthItem;
      AIdentity: TMCPAccessToken): Boolean;
  end;

  /// <summary>
  ///   Asks an authorizer about the items of one request, resolving the caller's
  ///   identity once. Used by the MCP API; an exception raised by the authorizer is
  ///   logged and counted as a denial.
  /// </summary>
  TMCPAccessGuard = class
  private
    FContext: TJRPCContext;
    FAuthorizer: IMCPAuthorizer;
    FIdentity: TMCPAccessToken;
    FOwnedIdentity: TMCPAccessToken;
  public
    constructor Create(AContext: TJRPCContext; const AAuthorizer: IMCPAuthorizer);
    destructor Destroy; override;

    function IsAllowed(const AItem: TMCPAuthItem): Boolean;

    property Identity: TMCPAccessToken read FIdentity;
  end;

implementation

uses
  Logify;

const
  UI_SCHEME = 'ui://';

{ TMCPAuthItem }

class function TMCPAuthItem.FromTool(ATool: TMCPTool): TMCPAuthItem;
begin
  Result := Default(TMCPAuthItem);
  Result.Kind := TMCPAuthItemKind.Tool;
  Result.Name := ATool.Name;
  Result.RequiredScopes := ATool.RequiredScopes;
  Result.Tags := ATool.Tags;
end;

class function TMCPAuthItem.FromResource(AResource: TMCPResource): TMCPAuthItem;
begin
  Result := Default(TMCPAuthItem);
  if AResource.Uri.StartsWith(UI_SCHEME, True) then
    Result.Kind := TMCPAuthItemKind.UI
  else
    Result.Kind := TMCPAuthItemKind.Resource;
  Result.Name := AResource.Name;
  Result.Uri := AResource.Uri;
  Result.RequiredScopes := AResource.RequiredScopes;
  Result.Tags := AResource.Tags;
end;

class function TMCPAuthItem.FromTemplate(ATemplate: TMCPResourceTemplate;
  const AUri: string): TMCPAuthItem;
begin
  Result := Default(TMCPAuthItem);
  Result.Kind := TMCPAuthItemKind.Template;
  Result.Name := ATemplate.Name;
  if AUri.IsEmpty then
    Result.Uri := ATemplate.UriTemplate.GetValueOrDefault
  else
    Result.Uri := AUri;
  Result.RequiredScopes := ATemplate.RequiredScopes;
  Result.Tags := ATemplate.Tags;
end;

class function TMCPAuthItem.FromPrompt(APrompt: TMCPPrompt): TMCPAuthItem;
begin
  Result := Default(TMCPAuthItem);
  Result.Kind := TMCPAuthItemKind.Prompt;
  Result.Name := APrompt.Name;
  Result.RequiredScopes := APrompt.RequiredScopes;
  Result.Tags := APrompt.Tags;
end;

function TMCPAuthItem.ToString: string;
const
  KIND_NAMES: array[TMCPAuthItemKind] of string = ('tool', 'resource', 'template', 'ui', 'prompt');
begin
  if Uri.IsEmpty then
    Result := Format('%s [%s]', [KIND_NAMES[Kind], Name])
  else
    Result := Format('%s [%s]', [KIND_NAMES[Kind], Uri]);
end;

{ TMCPScopeAuthorizer }

class function TMCPScopeAuthorizer.Check(const AItem: TMCPAuthItem;
  AIdentity: TMCPAccessToken): Boolean;
begin
  if Length(AItem.RequiredScopes) = 0 then
    Exit(True);

  Result := Assigned(AIdentity) and AIdentity.HasScopes(AItem.RequiredScopes);
end;

function TMCPScopeAuthorizer.Authorize(AContext: TJRPCContext;
  const AItem: TMCPAuthItem; AIdentity: TMCPAccessToken): Boolean;
begin
  Result := Check(AItem, AIdentity);
end;

{ TMCPFuncAuthorizer }

constructor TMCPFuncAuthorizer.Create(const AFunc: TMCPAuthorizerFunc);
begin
  inherited Create;
  FFunc := AFunc;
end;

function TMCPFuncAuthorizer.Authorize(AContext: TJRPCContext;
  const AItem: TMCPAuthItem; AIdentity: TMCPAccessToken): Boolean;
begin
  Result := FFunc(AContext, AItem, AIdentity);
end;

{ TMCPAccessGuard }

constructor TMCPAccessGuard.Create(AContext: TJRPCContext;
  const AAuthorizer: IMCPAuthorizer);
begin
  inherited Create;
  FContext := AContext;
  FAuthorizer := AAuthorizer;

  if Assigned(FContext) then
    FIdentity := FContext.FindContextDataAs<TMCPAccessToken>;

  // The transports always add a token; anything else building a context may not,
  // and the authorizer is promised a non-nil identity.
  if not Assigned(FIdentity) then
  begin
    FOwnedIdentity := TMCPAccessToken.Create;
    FIdentity := FOwnedIdentity;
  end;
end;

destructor TMCPAccessGuard.Destroy;
begin
  FOwnedIdentity.Free;
  inherited;
end;

function TMCPAccessGuard.IsAllowed(const AItem: TMCPAuthItem): Boolean;
begin
  try
    Result := FAuthorizer.Authorize(FContext, AItem, FIdentity);
  except
    on E: Exception do
    begin
      Logger.LogError(E, Format(SMCPAuthorizerFailedFmt, [AItem.ToString]));
      Result := False;
    end;
  end;
end;

end.
