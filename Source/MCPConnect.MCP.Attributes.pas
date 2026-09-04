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
unit MCPConnect.MCP.Attributes;

interface

uses
  System.SysUtils, Neon.Core.Tags;

type
  McpBaseAttribute = class(TCustomAttribute)
  protected
    FAdditionalTags: string;
    FTags: TAttributeTags;
    function GetTags: TAttributeTags;
  public
    property AdditionalTags: string read FAdditionalTags write FAdditionalTags;
    property Tags: TAttributeTags read GetTags write FTags;

    constructor Create;
    destructor Destroy; override;
  end;

  McpScopeAttribute = class(McpBaseAttribute)
  private
    FName: string;
  public
    property Name: string read FName;
    constructor Create(const AName: string; const AAdditionalTags: string = '');
  end;

  McpAttribute = class(McpBaseAttribute)
  private
    FName: string;
    FDescription: string;
  public
    property Name: string read FName;
    property Description: string read FDescription;

    constructor Create(const AName, ADescription: string; const AAdditionalTags: string = '');
  end;

  MCPToolAttribute = class(McpAttribute);
  MCPParamAttribute = class(McpAttribute);

  MCPAppAttribute = class(McpBaseAttribute)
  private
    FUI: string;
  public
    property UI: string read FUI;

    constructor Create(const AUI: string; const AAdditionalTags: string = '');
  end;

  MCPResourceBaseAttribute = class(McpBaseAttribute)
  private
    FMimeType: string;
    FName: string;
    FDescription: string;
  public
    property Name: string read FName;
    property MimeType: string read FMimeType;
    property Description: string read FDescription;
  end;

  MCPResourceAttribute = class(MCPResourceBaseAttribute)
  private
    FUri: string;
  public
    property Uri: string read FUri;
    constructor Create(const AName, AUri: string; const AMime: string = ''; const ADescription: string = ''; const AAdditionalTags: string = '');
  end;

  MCPAppUIAttribute = class(McpBaseAttribute)
  private
    FName: string;
    FDescription: string;
    FUri: string;
  public
    property Uri: string read FUri;
    property Name: string read FName;
    property Description: string read FDescription;

    constructor Create(const AName, AUri: string; const ADescription: string = ''; const AAdditionalTags: string = '');
  end;

  MCPTemplateAttribute = class(MCPResourceBaseAttribute)
  private
    FUriTemplate: string;
  public
    property UriTemplate: string read FUriTemplate;
    constructor Create(const AName, AUriTemplate: string; const AMime: string = ''; const ADescription: string = ''; const AAdditionalTags: string = '');
  end;

  MCPTemplateParamAttribute = class(McpBaseAttribute)
  private
    FName: string;
  public
    property Name: string read FName;
    constructor Create(const AName: string);
  end;

  MCPPromptAttribute = class(McpAttribute)
  private
    FTitle: string;
  public
    property Title: string read FTitle;

    constructor Create(const AName, ATitle, ADescription: string; const AAdditionalTags: string = '');
  end;

  MCPArgumentAttribute = MCPParamAttribute;

  /// <summary>
  ///   Common part of the completion attributes: the argument whose value the
  ///   decorated method suggests values for.
  /// </summary>
  MCPCompleteBaseAttribute = class(McpBaseAttribute)
  private
    FArgument: string;
  public
    property Argument: string read FArgument;
  end;

  /// <summary>
  ///   Marks a method as the source of "completion/complete" suggestions for
  ///   one argument of a prompt. The prompt name is scoped like the prompt
  ///   itself, so [McpScope] applies to it too.
  /// </summary>
  /// <example>
  ///   <code>
  ///   [McpComplete('code_review', 'language')]
  ///   function CompleteLanguage(const AValue: string): TArray&lt;string&gt;;
  ///   </code>
  /// </example>
  MCPCompleteAttribute = class(MCPCompleteBaseAttribute)
  private
    FName: string;
  public
    property Name: string read FName;
    constructor Create(const AName, AArgument: string; const AAdditionalTags: string = '');
  end;

  /// <summary>
  ///   Marks a method as the source of "completion/complete" suggestions for
  ///   one placeholder of a resource template (or of a plain resource uri).
  ///   The uri is matched verbatim and is never scoped.
  /// </summary>
  /// <example>
  ///   <code>
  ///   [McpCompleteTemplate('file:///{path}', 'path')]
  ///   function CompletePath(const AValue: string): TArray&lt;string&gt;;
  ///   </code>
  /// </example>
  MCPCompleteTemplateAttribute = class(MCPCompleteBaseAttribute)
  private
    FUriTemplate: string;
  public
    property UriTemplate: string read FUriTemplate;
    constructor Create(const AUriTemplate, AArgument: string; const AAdditionalTags: string = '');
  end;

  //MCPToolNoteAttribute = class(TCustomAttribute);

  TAttributes = TArray<TCustomAttribute>;

implementation

{ McpAttribute }

constructor McpAttribute.Create(const AName, ADescription, AAdditionalTags: string);
begin
  inherited Create;
  FName := AName;
  FDescription := ADescription;
  FAdditionalTags := AAdditionalTags;
end;

{ McpBaseAttribute }

constructor McpBaseAttribute.Create;
begin
  FTags := TAttributeTags.Create();
end;

destructor McpBaseAttribute.Destroy;
begin
  FTags.Free;
  inherited;
end;

function McpBaseAttribute.GetTags: TAttributeTags;
begin
  if (FTags.Count = 0) and not FAdditionalTags.IsEmpty then
    FTags.Parse(FAdditionalTags);

  Result := FTags;
end;

{ MCPResourceAttribute }

constructor MCPResourceAttribute.Create(const AName, AUri, AMime, ADescription, AAdditionalTags: string);
begin
  inherited Create;
  FName := AName;
  FUri := AUri;
  FMimeType := AMime;
  FDescription := ADescription;
  FAdditionalTags := AAdditionalTags;
end;

{ MCPTemplateAttribute }

constructor MCPTemplateAttribute.Create(const AName, AUriTemplate, AMime, ADescription, AAdditionalTags: string);
begin
  inherited Create;
  FName := AName;
  FUriTemplate := AUriTemplate;
  FMimeType := AMime;
  FDescription := ADescription;
  FAdditionalTags := AAdditionalTags;
end;

{ MCPTemplateParamAttribute }

constructor MCPTemplateParamAttribute.Create(const AName: string);
begin
  FName := AName;
end;

{ McpScopeAttribute }

constructor McpScopeAttribute.Create(const AName, AAdditionalTags: string);
begin
  FName := AName;
  FAdditionalTags := AAdditionalTags;
end;

{ MCPAppUIAttribute }

constructor MCPAppUIAttribute.Create(const AName, AUri, ADescription, AAdditionalTags: string);
begin
  inherited Create;
  FName := AName;
  FUri := AUri;
  FDescription := ADescription;
  FAdditionalTags := AAdditionalTags;
end;

{ MCPAppAttribute }

constructor MCPAppAttribute.Create(const AUI, AAdditionalTags: string);
begin
  inherited Create;
  FUI := AUI;
end;

{ MCPPromptAttribute }

constructor MCPPromptAttribute.Create(const AName, ATitle, ADescription, AAdditionalTags: string);
begin
  inherited Create(AName, ADescription, AAdditionalTags);
  FTitle := ATitle;
end;

{ MCPCompleteAttribute }

constructor MCPCompleteAttribute.Create(const AName, AArgument, AAdditionalTags: string);
begin
  inherited Create;
  FName := AName;
  FArgument := AArgument;
  FAdditionalTags := AAdditionalTags;
end;

{ MCPCompleteTemplateAttribute }

constructor MCPCompleteTemplateAttribute.Create(const AUriTemplate, AArgument, AAdditionalTags: string);
begin
  inherited Create;
  FUriTemplate := AUriTemplate;
  FArgument := AArgument;
  FAdditionalTags := AAdditionalTags;
end;

end.
