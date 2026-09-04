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
unit MCPConnect.MCP.Types.Completion;

interface

{$SCOPEDENUMS ON}

uses
  System.SysUtils, System.Rtti, System.JSON, System.Generics.Collections,

  Neon.Core.Types,
  Neon.Core.Attributes,
  Neon.Core.Nullables,
  Neon.Core.Persistence,
  Neon.Core.Persistence.JSON,

  MCPConnect.MCP.Types.Base;

const
  /// <summary>
  ///   Discriminator of a PromptReference: the completion targets an argument
  ///   of the prompt named by the reference.
  /// </summary>
  MCP_REF_PROMPT = 'ref/prompt';

  /// <summary>
  ///   Discriminator of a ResourceTemplateReference: the completion targets a
  ///   placeholder of the resource URI or URI template named by the reference.
  /// </summary>
  MCP_REF_RESOURCE = 'ref/resource';

  /// <summary>
  ///   A completion response carries at most 100 values (schema: maxItems).
  /// </summary>
  MCP_COMPLETION_MAX_VALUES = 100;

resourcestring
  // MCPConnect.MCP.Types.Completion
  SMCPCompletionRefUnknownFmt = 'Completion reference type [%s] is not supported';
  SMCPCompletionRefMissing = 'Completion reference is missing its "type"';

type
  /// <summary>
  ///   Which of the two reference shapes a completion request carries.
  /// </summary>
  TMCPCompletionRefKind = (Unknown, Prompt, ResourceTemplate);

  /// <summary>
  ///   The subject of a "completion/complete" request: the schema's
  ///   PromptReference | ResourceTemplateReference union.
  /// </summary>
  /// <remarks>
  ///   Modelled as one class rather than two: the union is discriminated by
  ///   "type" and its two field sets are disjoint, so a single class both
  ///   deserializes either shape - which two classes could not, Neon having no
  ///   polymorphic reader - and serializes back to the shape it came from,
  ///   the unused fields being dropped as empty.
  /// </remarks>
  TMCPCompletionReference = class
  public
    /// <summary>
    ///   REQUIRED. MCP_REF_PROMPT or MCP_REF_RESOURCE.
    /// </summary>
    [NeonProperty('type')] &Type: string;

    /// <summary>
    ///   [ref/prompt] REQUIRED. The name of the prompt being completed.
    /// </summary>
    [NeonInclude(IncludeIf.NotEmpty)] Name: string;

    /// <summary>
    ///   [ref/prompt] The prompt's human-readable title.
    /// </summary>
    [NeonInclude(IncludeIf.NotEmpty)] Title: string;

    /// <summary>
    ///   [ref/resource] REQUIRED. The URI or URI template being completed.
    /// </summary>
    [NeonInclude(IncludeIf.NotEmpty)] Uri: string;
  public
    /// <summary>
    ///   The reference shape, from the "type" discriminator.
    /// </summary>
    function Kind: TMCPCompletionRefKind;

    /// <summary>
    ///   What the reference points at: the prompt name for a ref/prompt, the
    ///   uri for a ref/resource, an empty string for anything else.
    /// </summary>
    function Target: string;
  end;

  /// <summary>
  ///   The argument whose value the client is completing.
  /// </summary>
  TMCPCompletionArgument = class
  public
    /// <summary>REQUIRED. The name of the argument.</summary>
    Name: string;

    /// <summary>REQUIRED. The value typed so far; may be empty.</summary>
    Value: string;
  end;

  /// <summary>
  ///   Additional, optional context for a completion request.
  /// </summary>
  TMCPCompletionContext = class
  public
    /// <summary>
    ///   Arguments of the same prompt or template the client has already
    ///   resolved, as a map of argument name to value.
    /// </summary>
    [NeonInclude(IncludeIf.NotEmpty)] Arguments: TJSONObject;
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    ///   The value already resolved for AName, or an empty string.
    /// </summary>
    function ArgumentValue(const AName: string): string;
  end;

  /// <summary>
  ///   Parameters of a "completion/complete" request.
  /// </summary>
  TCompleteRequestParams = class(TRequestMetaParams)
  public
    /// <summary>REQUIRED. The prompt or resource template being completed.</summary>
    Ref: TMCPCompletionReference;

    /// <summary>REQUIRED. The argument being completed.</summary>
    Argument: TMCPCompletionArgument;

    /// <summary>Arguments already resolved by the client, if any.</summary>
    [NeonInclude(IncludeIf.NotEmpty)] Context: TMCPCompletionContext;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  /// <summary>
  ///   The completion payload of a CompleteResult.
  /// </summary>
  TMCPCompletion = class
  public
    /// <summary>
    ///   REQUIRED. The suggestions, ranked by relevance, at most
    ///   MCP_COMPLETION_MAX_VALUES of them.
    /// </summary>
    Values: TArray<string>;

    /// <summary>
    ///   The total number of matches available, which may exceed the number of
    ///   values returned.
    /// </summary>
    Total: NullInteger;

    /// <summary>
    ///   Whether more matches exist beyond the values returned.
    /// </summary>
    HasMore: NullBoolean;
  end;

  /// <summary>
  ///   The server's response to a "completion/complete" request.
  /// </summary>
  /// <remarks>
  ///   A plain Result, not a CacheableResult: completions are keystroke-scoped
  ///   and the schema gives them no ttlMs/cacheScope.
  /// </remarks>
  TCompleteResult = class(TBaseResult)
  public
    /// <summary>REQUIRED. The suggestions for the requested argument.</summary>
    Completion: TMCPCompletion;
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    ///   Fills the result from a full candidate list: keeps the first
    ///   MCP_COMPLETION_MAX_VALUES, and reports the rest through total/hasMore.
    /// </summary>
    procedure SetValues(const AValues: TArray<string>);
  end;

  /// <summary>
  ///   A registered completion provider: the method that supplies suggestions
  ///   for one argument of one prompt, or one placeholder of one resource
  ///   template.
  /// </summary>
  TMCPCompletionProvider = class
  public
    ProviderClass: TClass;
    Method: TRttiMethod;
    Category: string;
    Disabled: Boolean;
  public
    /// <summary>Whether the target is a prompt or a resource template.</summary>
    RefKind: TMCPCompletionRefKind;

    /// <summary>The prompt name, or the resource uri / uri template.</summary>
    RefTarget: string;

    /// <summary>The argument (or uri placeholder) this provider completes.</summary>
    Argument: string;
  public
    /// <summary>
    ///   The registry key for a (reference, argument) pair. Prompt names, URIs
    ///   and argument names are all case-sensitive in MCP, so the key is too.
    /// </summary>
    class function KeyFor(ARefKind: TMCPCompletionRefKind;
      const ATarget, AArgument: string): string; static;

    function Key: string;
  end;

  TMCPCompletionRegistry = class(TObjectDictionary<string, TMCPCompletionProvider>);
  TMCPCompletionFilterFunc = reference to function (AProvider: TMCPCompletionProvider): Boolean;

implementation

{ TMCPCompletionReference }

function TMCPCompletionReference.Kind: TMCPCompletionRefKind;
begin
  // The discriminator is a fixed string in the schema, so it is compared
  // case-sensitively, as JSON member values are everywhere else.
  if &Type = MCP_REF_PROMPT then
    Result := TMCPCompletionRefKind.Prompt
  else if &Type = MCP_REF_RESOURCE then
    Result := TMCPCompletionRefKind.ResourceTemplate
  else
    Result := TMCPCompletionRefKind.Unknown;
end;

function TMCPCompletionReference.Target: string;
begin
  case Kind of
    TMCPCompletionRefKind.Prompt:           Result := Name;
    TMCPCompletionRefKind.ResourceTemplate: Result := Uri;
  else
    Result := '';
  end;
end;

{ TMCPCompletionContext }

constructor TMCPCompletionContext.Create;
begin
  Arguments := TJSONObject.Create;
end;

destructor TMCPCompletionContext.Destroy;
begin
  Arguments.Free;
  inherited;
end;

function TMCPCompletionContext.ArgumentValue(const AName: string): string;
begin
  if not Assigned(Arguments) then
    Exit('');

  Result := Arguments.GetValue<string>(AName, '');
end;

{ TCompleteRequestParams }

constructor TCompleteRequestParams.Create;
begin
  inherited;
  Ref := TMCPCompletionReference.Create;
  Argument := TMCPCompletionArgument.Create;
  Context := TMCPCompletionContext.Create;
end;

destructor TCompleteRequestParams.Destroy;
begin
  Context.Free;
  Argument.Free;
  Ref.Free;
  inherited;
end;

{ TCompleteResult }

constructor TCompleteResult.Create;
begin
  inherited;
  Completion := TMCPCompletion.Create;
  ResultType := TResultType.Complete;
end;

destructor TCompleteResult.Destroy;
begin
  Completion.Free;
  inherited;
end;

procedure TCompleteResult.SetValues(const AValues: TArray<string>);
var
  LCount: Integer;
begin
  LCount := Length(AValues);

  Completion.Total := LCount;
  Completion.HasMore := LCount > MCP_COMPLETION_MAX_VALUES;

  if LCount > MCP_COMPLETION_MAX_VALUES then
    LCount := MCP_COMPLETION_MAX_VALUES;

  Completion.Values := Copy(AValues, 0, LCount);
end;

{ TMCPCompletionProvider }

class function TMCPCompletionProvider.KeyFor(ARefKind: TMCPCompletionRefKind;
  const ATarget, AArgument: string): string;
var
  LPrefix: string;
begin
  case ARefKind of
    TMCPCompletionRefKind.Prompt:           LPrefix := MCP_REF_PROMPT;
    TMCPCompletionRefKind.ResourceTemplate: LPrefix := MCP_REF_RESOURCE;
  else
    LPrefix := '';
  end;

  // '|' cannot appear in a reference type, so the three parts stay unambiguous
  Result := LPrefix + '|' + ATarget + '|' + AArgument;
end;

function TMCPCompletionProvider.Key: string;
begin
  Result := KeyFor(RefKind, RefTarget, Argument);
end;

end.
