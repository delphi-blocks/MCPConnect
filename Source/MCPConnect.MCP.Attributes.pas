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
  McpAttribute = class(TCustomAttribute)
  private
    FName: string;
    FDescription: string;
    FAdditionalTags: string;
    FAdditional: string;
    FTags: TAttributeTags;
    function GetTags: TAttributeTags;
  public
    property Name: string read FName;
    property Description: string read FDescription;
    property AdditionalTags: string read FAdditional write FAdditional;

    property Tags: TAttributeTags read GetTags write FTags;

    constructor Create(const AName, ADescription: string; const AAdditionalTags: string = '');
    destructor Destroy; override;
  end;

  MCPToolAttribute = class(McpAttribute);
  MCPParamAttribute = class(McpAttribute);

  MCPResourceAttribute = class(McpAttribute);
  MCPResourceTemplateAttribute = class(McpAttribute);

  MCPPromptAttribute = class(McpAttribute);

  //MCPToolNoteAttribute = class(TCustomAttribute);

  TAttributes = TArray<TCustomAttribute>;

implementation

{ McpAttribute }

constructor McpAttribute.Create(const AName, ADescription, AAdditionalTags: string);
begin
  FName := AName;
  FDescription := ADescription;
  FAdditionalTags := AAdditionalTags;

  FTags := TAttributeTags.Create();
end;

destructor McpAttribute.Destroy;
begin
  FTags.Free;
  inherited;
end;

function McpAttribute.GetTags: TAttributeTags;
begin
  if (FTags.Count = 0) and not FAdditionalTags.IsEmpty then
    FTags.Parse(FAdditionalTags);

  Result := FTags;
end;

end.
