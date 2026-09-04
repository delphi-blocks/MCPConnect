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
unit MCPConnect.MCP.Types.Tools;

interface

{$I 'MCPConnect.inc' }

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

  MCPConnect.MCP.Types.Base,
  MCPConnect.MCP.Types.Tool,
  MCPConnect.MCP.Types.Mrtr,

  MCPConnect.MCP.Attributes;

type
  /// <summary>
  /// Parameters for CallToolRequest
  /// </summary>
  TCallToolRequestParams = class(TMrtrRequestParams);


  TMCPTools = class(TObjectList<TMCPTool>);
  {$IFDEF HAS_ORDERED_DICTIONARY}
  TMCPToolRegistry = class(TObjectOrderedDictionary<string, TMCPTool>);
  {$ELSE}
  TMCPToolRegistry = class(TObjectDictionary<string, TMCPTool>);
  {$ENDIF}
  TMCPToolConfigurator = reference to procedure(ATool: TMCPTool);
  TMCPToolFilterFunc = reference to function (ATool: TMCPTool): Boolean;

  /// <summary>
  ///   The result returned by the server for a tools/list request.
  /// </summary>
  TListToolsResult = class(TCachedResult)
  public

    /// <summary>
    ///   Tool list
    /// </summary>
    Tools: TMCPTools;

    /// <summary>
    ///   An opaque token representing the pagination position after the last returned result. If present, there may be more results available
    /// </summary>
    NextCursor: NullString;
  public
    constructor Create;
    destructor Destroy; override;

    function ToJSON(APrettyPrint: Boolean = False): string;
  end;

  /// <summary>
  ///   The result returned by the server for a tools/call request.
  /// </summary>
  TCallToolResult = class(TBaseResult)
  public

   /// <summary>
   ///   Can be TextContent, ImageContent, AudioContent, ResourceLink, or
   ///   EmbeddedResource.
   /// </summary>
	  Content: TContentList;

    /// <summary>
    ///   Structured content returned as a JSON object in the structuredContent
    ///   field of a result. For backwards compatibility, a tool that returns
    ///   structured content SHOULD also return functionally equivalent
    ///   unstructured content.
    /// </summary>

    { TODO -opaolo -c : Lift the object constraint 29/08/2026 09:37:25 }
    [NeonInclude(IncludeIf.NotEmpty)] StructuredContent: TJSONValue;

    /// <summary>
    ///   Whether the tool call ended in an error. If not set, this is assumed
    ///   to be false (the call was successful).
    /// </summary>
    IsError: Nullable<Boolean>;

  public
    constructor Create; overload;
    constructor Create(AContent: TContentList); overload;

    destructor Destroy; override;

    procedure AddContent(AContent: TToolContent);
  end;

  TCallToolResultResponse = class
  private
    InputRequiredResult: TInputRequiredResult;
    CallToolResult: TCallToolResult;

  public
    [NeonUnwrapped] Raw: TJSONObject;
  end;

  TCallToolResponse<T> = record
  private

  public

  end;

implementation

uses
  MCPConnect.JRPC.Core;

{ TListToolsResult }

constructor TListToolsResult.Create;
begin
  inherited;
  Tools := TMCPTools.Create(False);
end;

destructor TListToolsResult.Destroy;
begin
  Tools.Free;
  inherited;
end;

function TListToolsResult.ToJSON(APrettyPrint: Boolean = False): string;
begin
  Result := TNeon.ObjectToJSONString(Self, MCPNeonConfig.SetPrettyPrint(APrettyPrint));
end;

{ TCallToolResult }

constructor TCallToolResult.Create;
begin
  inherited;
  Content := TContentList.Create;
  //StructuredContent := TJSONNull.Create;
  ResultType := TResultType.Complete;
end;

constructor TCallToolResult.Create(AContent: TContentList);
begin
  Assert(Assigned(AContent), ClassName + ': AContent cannot be nil');

  inherited Create;
  Content := AContent;
  //StructuredContent := TJSONNull.Create;
end;

destructor TCallToolResult.Destroy;
begin
  Content.Free;
  StructuredContent.Free;
  inherited;
end;

procedure TCallToolResult.AddContent(AContent: TToolContent);
begin
  Content.Add(AContent);
end;

end.
