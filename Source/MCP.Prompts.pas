unit MCP.Prompts;

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

  MCP.Types,
  MCP.Attributes;


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
  end;

  TPromptArguments = TArray<TPromptArgument>;


  /// <summary>
  /// Represents a known resource that the server is capable of reading.
  /// </summary>
  TMCPPrompt = class
  public

    /// <summary>
    /// Metadata object reserved by MCP for storing additional information.
    /// </summary>
    [NeonProperty('_meta'), NeonInclude(IncludeIf.NotEmpty)] Meta: TJSONObject;

    /// <summary>
    /// A human-readable name for this resource.
    /// </summary>
    /// <remarks>This can be used by clients to populate UI elements.</remarks>
    [NeonProperty('name')] Name: string;

    /// <summary>
    /// A description of what this resource represents.
    /// </summary>
    /// <remarks>This can be used by clients to improve the LLM's understanding of available resources. It can be thought of like a 'hint' to the model.</remarks>
    [NeonProperty('description')] Description: NullString;

    /// <summary>
    /// The MIME type of this resource, if known.
    /// </summary>
    [NeonProperty('mimeType')] Arguments: TPromptArguments;
  public
    constructor Create;
    destructor Destroy; override;
  end;


  TGetPromptParams = class
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
  TPromptMessage = class(TContentClass)
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
  TGetPromptResult = class
  public

    /// <summary>
    /// Metadata object reserved by MCP for storing additional information.
    /// </summary>
    [NeonProperty('_meta'), NeonInclude(IncludeIf.NotEmpty)] Meta: TJSONObject;

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



implementation

{ TMCPPrompt }

constructor TMCPPrompt.Create;
begin
  Meta := TJSONObject.Create;
end;

destructor TMCPPrompt.Destroy;
begin
  Meta.Free;
  inherited;
end;

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

end.
