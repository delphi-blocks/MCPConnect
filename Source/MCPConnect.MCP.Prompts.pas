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
unit MCPConnect.MCP.Prompts;

interface

uses
  System.SysUtils,
  System.Rtti,
  System.JSON,
  System.Types,
  System.Generics.Collections,

  Neon.Core.Types,
  Neon.Core.Nullables,
  Neon.Core.Attributes,
  Neon.Core.Persistence,
  Neon.Core.Persistence.JSON,
  Neon.Core.Persistence.JSON.Schema,
  Neon.Core.Serializers.RTL,
  Neon.Core.Utils,

  MCPConnect.MCP.Types,
  MCPConnect.MCP.Attributes;


type
  TPromptArgument = record
    /// <summary>
    /// The name of the argument.
    /// </summary>
    Name: string;

    /// <summary>
    /// A human-readable description of the argument.
    /// </summary>
    Description: NullString;

    /// <summary>
    /// Whether this argument must be provided.
    /// If true, clients must include this argument when calling prompts/get.
    /// </summary>
    Required: NullBoolean;

    /// <summary>
    ///   Intended for UI and end-user contexts — optimized to be human-readable and easily
    ///   understood, even by those unfamiliar with domain-specific terminology
    /// </summary>
    /// <remarks>
    ///   If not provided, the name should be used for display (except for Tool, where
    ///   annotations.title should be given precedence over using name, if present)
    /// </remarks>
    Title: NullString;
  end;

  TPromptArguments = TArray<TPromptArgument>;


  /// <summary>
  /// Represents a known resource that the server is capable of reading.
  /// </summary>
  TMCPPrompt = class(TMetaClass)
  public

    /// <summary>
    /// A human-readable name for this resource.
    /// </summary>
    /// <remarks>This can be used by clients to populate UI elements.</remarks>
    Name: string;

    /// <summary>
    /// A description of what this resource represents.
    /// </summary>
    /// <remarks>This can be used by clients to improve the LLM's understanding of available resources. It can be thought of like a 'hint' to the model.</remarks>
    Description: NullString;

    /// <summary>
    ///   Optional set of sized icons that the client can display in a user interface. Clients that
    ///   support rendering icons MUST support at least the following MIME types:
    /// </summary>
    [NeonInclude(IncludeIf.NotEmpty)] Icons: TMCPIconList;

    /// <summary>
    ///   A list of arguments to use for templating the prompt
    /// </summary>
    Arguments: TPromptArguments;

    /// <summary>
    ///   Intended for UI and end-user contexts — optimized to be human-readable and easily
    ///   understood, even by those unfamiliar with domain-specific terminology
    /// </summary>
    /// <remarks>
    ///   If not provided, the name should be used for display (except for Tool, where
    ///   annotations.title should be given precedence over using name, if present)
    /// </remarks>
    Title: NullString;
  end;

  TMCPPrompts = TObjectList<TMCPPrompt>;


  TGetPromptParams = class(TMetaClass)
    /// <summary>
    ///  The name of the prompt or prompt template.
    /// </summary>
    [NeonProperty('name')] Name: string;

    /// <summary>
    /// Arguments to use for templating the prompt.
    /// </summary>
    [NeonProperty('arguments')] [NeonInclude(IncludeIf.NotEmpty)] Arguments: TJSONObject;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  /// <summary>
  ///   Used by the client to get a prompt provided by the server.
  /// </summary>
  TGetPromptRequest = class
    [NeonProperty('method')] Method: string;

    [NeonProperty('params')] Params: TGetPromptParams;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  /// <summary>
  /// PromptMessage describes a message returned as part of a prompt.
  /// This is similar to `SamplingMessage`, but also supports the embedding of
  /// resources from the MCP server.
  /// </summary>
  TPromptMessage = class(TToolContent)
  public
    /// <summary>
    ///   The sender or recipient of messages and data in a conversation.
    /// </summary>
    /// <remarks>
    ///   Valid values: `assistant`, `user`
    /// </remarks>
    [NeonProperty('rule')] Role: string;
  end;
  TPromptMessages = TObjectList<TPromptMessage>;


  /// <summary>
  ///  The server's response to a prompts/get request from the client.
  /// </summary>
  TGetPromptResult = class(TMetaClass)
  public

    /// <summary>
    /// A description of what this resource represents.
    /// </summary>
    /// <remarks>This can be used by clients to improve the LLM's understanding of available resources. It can be thought of like a 'hint' to the model.</remarks>
    [NeonProperty('description')] Description: NullString;

    /// <summary>
    /// The MIME type of this resource, if known.
    /// </summary>
    [NeonProperty('messages ')] Messages: TPromptMessages;
  public
    constructor Create;
    destructor Destroy; override;
  end;


  /// <summary>
  /// The server's response to a resources/list request from the client.
  /// </summary>
  TListPromptsResult = class(TMetaClass)
  public
    //[NeonProperty('PaginatedResult')] PaginatedResult: TPaginatedResult;
    /// <summary>
    /// A list of available resources.
    /// </summary>
    Prompts: TMCPPrompts;

    /// <summary>
    ///   An opaque token representing the pagination position after the last returned result. If present, there may be more results available
    /// </summary>
    NextCursor: NullString;

  public
    constructor Create;
    destructor Destroy; override;

    function AddPrompt(const AName, ADescription: string): TMCPPrompt;
  end;


implementation

{ TGetPromptResult }

constructor TGetPromptResult.Create;
begin
  Messages := TPromptMessages.Create;
end;

destructor TGetPromptResult.Destroy;
begin
  Messages.Free;
  inherited;
end;

{ TGetPromptParams }

constructor TGetPromptParams.Create;
begin
  Arguments := TJSONObject.Create;
end;

destructor TGetPromptParams.Destroy;
begin
  Arguments.Free;
  inherited;
end;

{ TGetPromptRequest }

constructor TGetPromptRequest.Create;
begin
  Params := TGetPromptParams.Create;
end;

destructor TGetPromptRequest.Destroy;
begin
  Params.Free;
  inherited;
end;

{ TListPromptsResult }

function TListPromptsResult.AddPrompt(const AName, ADescription: string): TMCPPrompt;
begin
  Result := TMCPPrompt.Create;
  Result.Name := AName;
  Result.Description := ADescription;
  Prompts.Add(Result);
end;

constructor TListPromptsResult.Create;
begin
  inherited;
  Prompts := TMCPPrompts.Create;
end;

destructor TListPromptsResult.Destroy;
begin
  Prompts.Free;
  inherited;
end;

end.
