unit MCP.Attributes;

interface

uses
  System.SysUtils, Attribute.Tags;

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

  //MCPToolNoteAttribute = class(TCustomAttribute);

  MCPParamAttribute = class(McpAttribute);

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
