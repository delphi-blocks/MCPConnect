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
unit MCPConnect.MCP.Invoker;

interface

uses
  System.SysUtils, System.Rtti, System.Classes, System.Generics.Collections,
  System.TypInfo, System.JSON,

  Neon.Core.Utils,
  Neon.Core.Persistence,
  Neon.Core.Persistence.JSON,

  MCPConnect.Core.Utils,

  MCPConnect.JRPC.Core,
  MCPConnect.Configuration.Core,
  MCPConnect.Configuration.MCP,

  MCPConnect.MCP.Attributes,
  MCPConnect.MCP.Types,
  MCPConnect.MCP.Tools,
  MCPConnect.MCP.Prompts,
  MCPConnect.MCP.Resources;

type
  /// <summary>
  ///   This interface define a standard method to handle MCP call
  /// </summary>
  IMCPInvokable = interface
  ['{3E475D9B-6863-4260-8789-74014419A52B}']
    function InvokeTool(AParams: TCallToolParams; AResult: TCallToolResult): Boolean;
    function InvokeResource(AParams: TReadResourceParams; AResult: TReadResourceResult): Boolean;
  end;

  TMCPMethodInvoker = class(TInterfacedObject, IMCPInvokable)
  private
    //[Context] FContext: TJRPCContext;
    [Context] FConfig: TMCPConfig;
    [Context] FGarbageCollector: IGarbageCollector;
  private
    FInstance: TObject;
    FMethod: TRttiMethod;
    function ArgumentsToRttiParams(AArguments: TJSONObject): TArray<TValue>;
    function GetParamName(LParam: TRttiParameter): string;
  protected
    procedure ResultToTool(const AToolResult: TValue; AContentList: TContentList);
    procedure ResultToResource(const AMethodResult: TValue; AContentList: TResourceContentsList);
  public
    { IMCPInvokable }
    function InvokeTool(AParams: TCallToolParams; AResult: TCallToolResult): Boolean;
    function InvokeResource(AParams: TReadResourceParams; AResult: TReadResourceResult): Boolean;

    constructor Create(AInstance: TObject; AMethod: TRttiMethod);
  end;

implementation

uses
  MCPConnect.Content.Writers;

{ TMCPMethodInvoker }

constructor TMCPMethodInvoker.Create(AInstance: TObject; AMethod: TRttiMethod);
begin
  inherited Create;
  FInstance := AInstance;
  FMethod := AMethod;
end;

function TMCPMethodInvoker.InvokeResource(AParams: TReadResourceParams; AResult: TReadResourceResult): Boolean;
var
  LResult: TValue;
begin
  Result := True;



  LResult := FMethod.Invoke(FInstance, []);
  try
    // If the result is already a TResourceContents just assign it
    if LResult.IsType<TResourceContents> then
    begin
      AResult.Contents.Free;
      AResult.Contents := TResourceContentsList(LResult.AsObject);
      LResult := nil;
    end
    else
      ResultToResource(LResult, AResult.Contents);
  finally
    FGarbageCollector.Add(LResult);
  end;
end;

function TMCPMethodInvoker.InvokeTool(AParams: TCallToolParams; AResult: TCallToolResult): Boolean;
var
  LArgs: TArray<TValue>;
  LResult: TValue;
begin
  Result := True;
  LArgs := ArgumentsToRttiParams(AParams.Arguments);
  FGarbageCollector.Add(LArgs);
  LResult := FMethod.Invoke(FInstance, LArgs);
  try
    // If the result is already a TContentList just assign it
    if LResult.IsType<TContentList> then
    begin
      AResult.Content.Free;
      AResult.Content := TContentList(LResult.AsObject);
      LResult := nil;
    end
    else
      ResultToTool(LResult, AResult.Content);
  finally
    FGarbageCollector.Add(LResult);
  end;
end;

procedure TMCPMethodInvoker.ResultToTool(const AToolResult: TValue; AContentList: TContentList);
var
  LResult: string;
  LWriter: TMCPCustomWriter;
  LContext: TMCPToolContext;
  LContent: TToolContent;
  LText: TTextContent absolute LContent;
  LResText: TEmbeddedResourceText absolute LContent;
  LResBlob: TEmbeddedResourceBlob absolute LContent;
begin
  { TODO -opaolo -c : Change the Neon configuration!!! 14/11/2025 10:25:55 }
  LResult := TNeon.ValueToJSONString(AToolResult, TNeonConfiguration.Default);

  LWriter := FConfig.Server.WriterRegistry.GetWriter(AToolResult);
  if Assigned(LWriter) then
  begin
    LContext.Result := AContentList;
    LContext.Attributes := FMethod.GetAttributes;

    LWriter.WriteTool(AToolResult, LContext);
    Exit;
  end;

  case AToolResult.Kind of

    // As it is
    tkInt64,
    tkInteger,
    tkFloat: LText := TTextContent.Create(LResult);

    // Dequote
    tkEnumeration,
    tkChar,
    tkWChar,
    tkString,
    tkLString,
    tkWString,
    tkUString: LText := TTextContent.Create(LResult.DeQuotedString('"'));

    // JSON response
    tkSet,
    tkClass,
    tkRecord, tkMRecord:
    begin
      // Check if the tool is configured to return an embedded resource
      var LMCPTool := TRttiUtils.FindAttribute<MCPToolAttribute>(FMethod);
      if Assigned(LMCPTool) and (LMCPTool.Tags.Exists('embedded')) then
      begin
        LResText := TEmbeddedResourceText.Create;
        LResText.Resource.MIMEType := 'application/json';
        LResText.Resource.URI := '';
        LResText.Resource.Text := LResult;
      end
      else
      begin
        LText := TTextContent.Create(LResult);
      end;
    end;

    tkArray, tkDynArray:
    begin
      LResBlob := TEmbeddedResourceBlob.Create;
      LResBlob.Resource.Blob := LResult;

      if AToolResult.TypeInfo = TypeInfo(TBytes) then
        LResBlob.Resource.MIMEType := 'application/octect-stream'
      else
        LResBlob.Resource.MIMEType := 'application/json';
    end;

  else
    LText := TTextContent.Create(LResult);
  end;

  AContentList.Add(LContent);
end;

procedure TMCPMethodInvoker.ResultToResource(const AMethodResult: TValue; AContentList: TResourceContentsList);
var
  LResult: string;
  LWriter: TMCPCustomWriter;
  LContext: TMCPResourceContext;

  LContent: TResourceContents;
  LResText: TTextResourceContents absolute LContent;
  LResBlob: TBlobResourceContents absolute LContent;
begin

  LResult := TNeon.ValueToJSONString(AMethodResult, TNeonConfiguration.Default);
  LWriter := FConfig.Server.WriterRegistry.GetWriter(AMethodResult);
  if Assigned(LWriter) then
  begin
    LContext.Result := AContentList;
    LContext.Attributes := FMethod.GetAttributes;

    LWriter.WriteResource(AMethodResult, LContext);
    Exit;
  end;

  case AMethodResult.Kind of

    // As it is
    tkInt64,
    tkInteger,
    tkFloat:
    begin
      LResText := TTextResourceContents.Create();
      LResText.Text := LResult;
      LResText.MimeType := 'text/text';
      LResText.Uri := FInstance.ClassName;
    end;

    // Dequote
    tkEnumeration,
    tkChar,
    tkWChar,
    tkString,
    tkLString,
    tkWString,
    tkUString:
    begin
      LResText := TTextResourceContents.Create;
      LResText.Text := LResult.DeQuotedString('"');
      LResText.MimeType := 'text/text';
      LResText.Uri := FInstance.ClassName;
    end;
    {
    // JSON response
    tkSet,
    tkClass,
    tkRecord, tkMRecord:
    begin
      // Check if the tool is configured to return an embedded resource
      var LMCPTool := TRttiUtils.FindAttribute<MCPToolAttribute>(FMethod);
      if Assigned(LMCPTool) and (LMCPTool.Tags.Exists('embedded')) then
      begin
        LResText := TEmbeddedResourceText.Create;
        LResText.Resource.MIMEType := 'application/json';
        LResText.Resource.URI := '';
        LResText.Resource.Text := LResult;
      end
      else
      begin
        LContent := TTextContent.Create(LResult);
      end;
    end;

    tkArray, tkDynArray:
    begin
      LResBlob := TEmbeddedResourceBlob.Create;
      LResBlob.Resource.Blob := LResult;

      if AMethodResult.TypeInfo = TypeInfo(TBytes) then
        LResBlob.Resource.MIMEType := 'application/octect-stream'
      else
        LResBlob.Resource.MIMEType := 'application/json';
    end;

  else
    LContent := TTextContent.Create(LResult);
    }
  end;

  AContentList.Add(LContent);
end;

function TMCPMethodInvoker.GetParamName(LParam: TRttiParameter): string;
var
  LParamAttrib: MCPParamAttribute;
begin
  LParamAttrib := TRttiUtils.FindAttribute<MCPParamAttribute>(LParam);
  if Assigned(LParamAttrib) then
    Result := LParamAttrib.Name
  else
    Result := LParam.Name;
end;

function TMCPMethodInvoker.ArgumentsToRttiParams(AArguments: TJSONObject): TArray<TValue>;

  function CastJSONValue(AParam: TRttiParameter; AValue: TJSONValue): TValue;
  begin
    if not Assigned(AValue) then
    begin
      Result := CreateNewValue(AParam.ParamType);
      Exit;
    end;

    //CheckCompatibility(AParam, AValue);
    if AParam.ParamType.IsInstance then
      Result := TNeon.JSONToObject(AParam.ParamType, AValue)
    else
      Result := TNeon.JSONToValue(AParam.ParamType, AValue);
  end;

  function CastParamValue(AParam: TRttiParameter; AValue: TValue): TValue;
  begin
    if AValue.IsObject and (AValue.AsObject is TJSONValue) then
      Result := CastJSONValue(AParam, TJSONValue(AValue.AsObject))
    else
      Result := AValue.Cast(AParam.ParamType.Handle);
  end;

var
  LParam: TRttiParameter;
  LParamJSON: TJSONValue;
  LRttiParams: TArray<TRttiParameter>;
begin
  Result := [];

  LRttiParams := FMethod.GetParameters;

  for LParam in LRttiParams do
  begin
    LParamJSON := AArguments.GetValue(GetParamName(LParam));
    Result := Result + [CastJSONValue(LParam, LParamJSON)];
  end;
end;


end.
