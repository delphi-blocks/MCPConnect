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
  System.Classes,
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

    class function New(const AName, ADescription: string): TPromptArgument; static;
  end;

  TPromptArguments = TArray<TPromptArgument>;


  /// <summary>
  /// Represents a known resource that the server is capable of reading.
  /// </summary>
  TMCPPrompt = class(TMetaClass)
  public
    [NeonIgnore] Classe: TClass;
    [NeonIgnore] Method: TRttiMethod;
    [NeonIgnore] Category: string;
    [NeonIgnore] Disabled: Boolean;
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
  TMCPPromptRegistry = class(TObjectDictionary<string, TMCPPrompt>);


  TGetPromptParams = class(TMetaClass)
    /// <summary>
    ///  The name of the prompt or prompt template.
    /// </summary>
    Name: string;

    /// <summary>
    /// Arguments to use for templating the prompt.
    /// </summary>
    [NeonInclude(IncludeIf.NotEmpty)] Arguments: TJSONObject;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  /// <summary>
  ///   Used by the client to get a prompt provided by the server.
  /// </summary>
  TGetPromptRequest = class
    Method: string;

    Params: TGetPromptParams;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TPromptMessageBase = class
  public
    /// <summary>
    ///   The sender or recipient of messages and data in a conversation.
    /// </summary>
    /// <remarks>
    ///   Valid values: `assistant`, `user`
    /// </remarks>
    Role: string;

    //[NeonIgnore] ContentType: TResultContentType;

    constructor Create;
  end;

  /// <summary>
  ///   PromptMessage with a variable content
  /// </summary>
  TPromptMessage<T: TToolContent, constructor> = class(TPromptMessageBase)
  public
    /// <summary>
    ///   Can be TextContent, ImageContent, AudioContent, ResourceLink or EmbeddedResource
    /// </summary>
    Content: T;

    constructor Create; overload;
    constructor Create(const ARole: string); overload;
    destructor Destroy; override;
  end;

  TPromptMessages = class(TObjectList<TPromptMessageBase>)
  public
    function AddText(const ARole, AText: string): TTextContent;

    procedure AddImage(const ARole, AMime, ABase64: string); overload;
    procedure AddImage(const ARole, AMime: string; AImage: TStream); overload;

    procedure AddAudio(const ARole, AMime, ABase64: string); overload;
    procedure AddAudio(const ARole, AMime: string; AAudio: TStream); overload;

    procedure AddLink(const ARole, AMime, AUri, ADescription: string);

    procedure AddBlob(const ARole, AMime, ABase64: string); overload;
    procedure AddBlob(const ARole, AMime: string; ABlob: TStream); overload;
  end;

  /// <summary>
  ///  The server's response to a prompts/get request from the client.
  /// </summary>
  TGetPromptResult = class(TMetaClass)
  public

    /// <summary>
    /// A description of what this resource represents.
    /// </summary>
    /// <remarks>This can be used by clients to improve the LLM's understanding of available resources. It can be thought of like a 'hint' to the model.</remarks>
    Description: NullString;

    /// <summary>
    /// The MIME type of this resource, if known.
    /// </summary>
    Messages: TPromptMessages;
  public
    constructor Create; overload;
    constructor Create(AMessages: TPromptMessages); overload;
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

constructor TGetPromptResult.Create(AMessages: TPromptMessages);
begin
  Assert(Assigned(AMessages), ClassName + ': AMessages cannot be nil');

  inherited Create;
  Messages := AMessages;
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
  Prompts := TMCPPrompts.Create(False);
end;

destructor TListPromptsResult.Destroy;
begin
  Prompts.Free;
  inherited;
end;

{ TPromptArgument }

class function TPromptArgument.New(const AName, ADescription: string): TPromptArgument;
begin
  Result.Name := AName;
  Result.Description := ADescription;
end;

{ TPromptMessage<T> }

constructor TPromptMessage<T>.Create(const ARole: string);
begin
  inherited Create;

  Content := T.Create;

  if not ARole.IsEmpty then
    Role := ARole;
end;

constructor TPromptMessage<T>.Create;
begin
  inherited;
  Content := T.Create;
end;

destructor TPromptMessage<T>.Destroy;
begin
  Content.Free;
  inherited;
end;

{ TPromptMessages }

procedure TPromptMessages.AddImage(const ARole, AMime, ABase64: string);
begin
  var p := TPromptMessage<TImageContent>.Create(ARole);

  p.Content.Data := ABase64;
  p.Content.MimeType := AMime;
  Self.Add(p);
end;

procedure TPromptMessages.AddBlob(const ARole, AMime, ABase64: string);
begin
  var p := TPromptMessage<TEmbeddedResourceBlob>.Create(ARole);

  p.Content.Resource.MimeType := AMime;
  p.Content.Resource.Blob := ABase64;
  Self.Add(p);
end;

procedure TPromptMessages.AddImage(const ARole, AMime: string; AImage: TStream);
begin
  var p := TPromptMessage<TImageContent>.Create(ARole);

  p.Content.Data := p.Content.DataFromStream(AImage);
  p.Content.MimeType := AMime;
  Self.Add(p);
end;

procedure TPromptMessages.AddLink(const ARole, AMime, AUri, ADescription: string);
begin
  var p := TPromptMessage<TResourceLink>.Create(ARole);

  p.Content.Uri := AUri;
  p.Content.MimeType := AMime;
  p.Content.Description := ADescription;
  Self.Add(p);
end;

function TPromptMessages.AddText(const ARole, AText: string): TTextContent;
begin
  var p := TPromptMessage<TTextContent>.Create(ARole);

  p.Content.Text := AText;
  Self.Add(p);

  Result := p.Content;
end;

procedure TPromptMessages.AddAudio(const ARole, AMime: string; AAudio: TStream);
begin
  var p := TPromptMessage<TAudioContent>.Create(ARole);

  p.Content.Data := p.Content.DataFromStream(AAudio);
  p.Content.MimeType := AMime;
  Self.Add(p);
end;

procedure TPromptMessages.AddAudio(const ARole, AMime, ABase64: string);
begin
  var p := TPromptMessage<TAudioContent>.Create(ARole);

  p.Content.Data := ABase64;
  p.Content.MimeType := AMime;
  Self.Add(p);
end;

procedure TPromptMessages.AddBlob(const ARole, AMime: string; ABlob: TStream);
begin
  var p := TPromptMessage<TEmbeddedResourceBlob>.Create(ARole);

  p.Content.Resource.MimeType := AMime;
  p.Content.Resource.Blob := p.Content.DataFromStream(ABlob);
  Self.Add(p);
end;

{ TPromptMessageBase }

constructor TPromptMessageBase.Create;
begin
  Role := 'user';
end;

end.
