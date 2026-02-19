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

  MCPAppAttribute = class(McpBaseAttribute)
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

  MCPPromptAttribute = class(McpAttribute);

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

{ MCPAppAttribute }

constructor MCPAppAttribute.Create(const AName, AUri, ADescription, AAdditionalTags: string);
begin
  inherited Create;
  FName := AName;
  FUri := AUri;
  FDescription := ADescription;
  FAdditionalTags := AAdditionalTags;
end;

end.
