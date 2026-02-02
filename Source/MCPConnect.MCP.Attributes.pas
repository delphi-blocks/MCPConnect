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

  MCPResourceAttribute = class(McpBaseAttribute)
  private
    FMimeType: string;
    FUri: string;
    FName: string;
    FDescription: string;
  public
    property Name: string read FName;
    property Uri: string read FUri;
    property MimeType: string read FMimeType;
    property Description: string read FDescription;

    constructor Create(const AName, AUri, AMime, ADescription: string; const AAdditionalTags: string = '');
  end;

  MCPResourceTemplateAttribute = class(MCPResourceAttribute)

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

end.
