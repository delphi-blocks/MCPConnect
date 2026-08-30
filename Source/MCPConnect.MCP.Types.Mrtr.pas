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
unit MCPConnect.MCP.Types.Mrtr;

interface

{$SCOPEDENUMS ON}

uses
  System.SysUtils, System.Classes, System.Generics.Collections, System.JSON, System.Rtti,

  Neon.Core.Tags,
  Neon.Core.Types,
  Neon.Core.Attributes,
  Neon.Core.Nullables,
  Neon.Core.Persistence,
  Neon.Core.Persistence.JSON,
  Neon.Core.Serializers.RTL,
  MCPConnect.JRPC.Core,
  MCPConnect.JRPC.Classes,
  MCPConnect.MCP.Types,
  MCPConnect.MCP.Types.Tool;

type

  /// <summary>
  ///   The sender or recipient of messages and data in a conversation.
  /// </summary>
  TMCPRole = (
    Assistant,
    User
  );

  /// <summary>
  ///   Controls tool selection behavior for sampling requests.
  /// </summary>
  TMCPToolChoice = (
    /// <summary>
    ///   Model decides whether to use tools (default)
    /// </summary>
    Auto,
    /// <summary>
    ///   Model MUST use at least one tool before completing.
    /// </summary>
    Required,
    /// <summary>
    ///   Model MUST NOT use any tools.
    /// </summary>
    None
  );


  /// <summary>
  ///   Represents a root directory or file that the server can operate on.
  /// </summary>
  TMCPRoot = class(TMetaClass)
  public

    /// <summary>
    ///   An optional name for the root. This can be used to provide a
    ///   human-readable identifier for the root, which may be useful for
    ///   display purposes or for referencing the root in other parts of the
    ///   application.
    /// </summary>
    Name: NullString;

    /// <summary>
    ///   The URI identifying the root.
    /// </summary>
    /// <remarks>
    ///   <para>
    ///     This must start with file:// for now.
    ///   </para>
    ///   <para>
    ///     This restriction may be relaxed in future versions of the protocol
    ///     toallow other URI schemes.
    ///   </para>
    /// </remarks>
    Uri: string
  end;

  TMCPRoots = class(TObjectList<TMCPRoot>);

  TListRootsResult = class
  public

    /// <summary>
    ///   Array of Root objects, each representing a root
    ///   directory or file that the server can operate on.
    /// </summary>
    Roots: TMCPRoots;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  { TODO -opaolo -c : Hot to free the object(s) in TValue 29/08/2026 11:11:54 }
  TSamplingMessage = class(TMetaClass)
  public
    /// <summary>
    ///   The sender or recipient of messages and data in a conversation.
    /// </summary>
    Role: TMCPRole;

    { TODO -opaolo -c : perhaps a TJSONValue so I can deserialize the object and free it? 29/08/2026 11:13:49 }
    Content: TValue; // anyOf [TextContent, ImageContent, AudioContent, ToolUseContent, ToolResultContent, Array<anyOf [TextContent, ImageContent, AudioContent, ToolUseContent, ToolResultContent]>]
  end;


  /// <summary>
  ///   Hints to use for model selection.
  ///
  ///   Keys not declared here are currently left unspecified by the spec and are
  ///   up to the client to interpret.
  /// </summary>
  TModelHint = class(TMetaClass)
  public
    /// <summary>
    ///   A hint for a model name.
    ///
    ///   The client SHOULD treat this as a substring of a model name; for example:
    ///   - `claude-3-5-sonnet` should match `claude-3-5-sonnet-20241022`
    ///   - `sonnet` should match `claude-3-5-sonnet-20241022`, `claude-3-sonnet-20240229`, etc.
    ///   - `claude` should match any Claude model
    ///
    ///   The client MAY also map the string to a different provider's model name
    ///   or a different model family, as long as it fills a similar niche; for
    ///   example:
    ///   - `gemini-1.5-flash` could match `claude-3-haiku-20240307`
    /// </summary>
    Name: NullString;
  end;

  /// <summary>
  ///   The server's preferences for model selection, requested of the client
  ///   during sampling.
  ///
  ///   Because LLMs can vary along multiple dimensions, choosing the "best" model
  ///   is rarely straightforward. Different models excel in different areas — some
  ///   are faster but less capable, others are more capable but more expensive,
  ///   and so on. This interface allows servers to express their priorities across
  ///   multiple dimensions to help clients make an appropriate selection for their
  ///   use case.
  ///
  ///   These preferences are always advisory. The client MAY ignore them. It is
  ///   also up to the client to decide how to interpret these preferences and how
  ///   to balance them against other considerations.
  /// </summary>
  TModelPreferences = class(TMetaClass)
  public
    /// <summary>
    ///   Optional hints to use for model selection.
    ///
    ///   If multiple hints are specified, the client MUST evaluate them in order
    ///   (such that the first match is taken).
    ///
    ///   The client SHOULD prioritize these hints over the numeric priorities, but
    ///   MAY still use the priorities to select from ambiguous matches.
    /// </summary>
    [NeonInclude(IncludeIf.NotEmpty)]
    Hints: TObjectList<TModelHint>;

    /// <summary>
    ///   How much to prioritize cost when selecting a model. A value of 0 means
    ///   cost is not important, while a value of 1 means cost is the most
    ///   important factor.
    /// </summary>
    CostPriority: NullDouble;

    /// <summary>
    ///   How much to prioritize sampling speed (latency) when selecting a model.
    ///   A value of 0 means speed is not important, while a value of 1 means speed
    ///   is the most important factor.
    /// </summary>
    SpeedPriority: NullDouble;

    /// <summary>
    ///   How much to prioritize intelligence and capabilities when selecting a
    ///   model. A value of 0 means intelligence is not important, while a value
    ///   of 1 means intelligence is the most important factor.
    /// </summary>
    IntelligencePriority: NullDouble;
  public
    constructor Create;
    destructor Destroy; override;
  end;


  TCreateMessageResult = class(TSamplingMessage)
  public

    /// <summary>
    ///   The name of the model that generated the message.
    /// </summary>
    Model: string;

    /// <summary>
    ///   The reason why sampling stopped, if known.
    /// </summary>
    StopReason: NullString;
  end;


  /// <summary>
  ///   Parameters for the sampling/createMessage request.
  /// </summary>
  TCreateMessageParams = class(TMetaClass)
  public
    /// <summary>
    ///   The messages to sample.
    /// </summary>
    Messages: TObjectList<TSamplingMessage>;

    /// <summary>
    ///   The server's preferences for which model to select.
    /// </summary>
    [NeonInclude(IncludeIf.NotEmpty)]
    ModelPreferences: TModelPreferences;

    /// <summary>
    ///   An optional system prompt the server wants to use for sampling.
    /// </summary>
    //SystemPrompt: NullString;

    /// <summary>
    ///   How much of the conversation history to include.
    /// </summary>
    IncludeContext: NullString;

    /// <summary>
    ///   The temperature to use for sampling.
    /// </summary>
    Temperature: NullCurrency;

    /// <summary>
    ///   The maximum number of tokens to sample.
    /// </summary>
    MaxTokens: Integer;

    /// <summary>
    ///   Sequences the model should stop generating at.
    /// </summary>
    [NeonInclude(IncludeIf.NotEmpty)]
    StopSequences: TArray<string>;

    /// <summary>
    ///   Request-scoped metadata.
    /// </summary>
    [NeonInclude(IncludeIf.NotEmpty)]
    Metadata: TJSONObject;

    /// <summary>
    ///   Tools the model may use during generation.
    /// </summary>
    [NeonInclude(IncludeIf.NotEmpty)]
    Tools: TArray<TMCPTool>;

    /// <summary>
    ///   Controls how the model uses tools during generation.
    /// </summary>
    ToolChoice: Nullable<TMCPToolChoice>;
  public
    constructor Create;
    destructor Destroy; override;
  end;




  TInputRequest = class

  end;

  TInputRequests = class(TObjectList<TInputRequest>);


  TInputResponse = class

    // Elicitation answers an elicitation/create input request.
    Elicitation: TElicitResult;

    // Sampling answers a sampling/createMessage input request.
    Sampling: TCreateMessageResult;

    // Roots answers a roots/list input request.
    Roots: TListRootsResult;

    // raw preserves the original JSON so that a response can be round-tripped
    // and decoded against the method of the request it answers.
    Raw: TJSONValue;
  end;

  TInputResponses = class(TObjectDictionary<string, TInputResponse>);



implementation


{ TModelPreferences }

constructor TModelPreferences.Create;
begin
  inherited;
  Hints := TObjectList<TModelHint>.Create(True);
end;

destructor TModelPreferences.Destroy;
begin
  Hints.Free;
  inherited;
end;

{ TCreateMessageParams }

constructor TCreateMessageParams.Create;
begin
  inherited;
  Messages := TObjectList<TSamplingMessage>.Create(True);
end;

destructor TCreateMessageParams.Destroy;
begin
  Messages.Free;
  inherited;
end;

{ TListRootsResult }

constructor TListRootsResult.Create;
begin
  Roots := TMCPRoots.Create;
end;

destructor TListRootsResult.Destroy;
begin
  Roots.Free;
  inherited;
end;



end.
