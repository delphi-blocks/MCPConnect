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
  TMCPInvoker = class
  protected
    FInstance: TObject;

    [Context] FConfig: TMCPConfig;
    [Context] FGC: IGarbageCollector;

    function GetParamName(LParam: TRttiParameter): string;
    function ArgumentsToRttiParams(AArguments: TJSONObject; const AParams: TArray<TRttiParameter>): TArray<TValue>;

    constructor Create(AInstance: TObject);
  end;

  TMCPToolInvoker = class(TMCPInvoker)
  protected
    FTool: TMCPTool;
    procedure ResultToTool(const AToolResult: TValue; AContentList: TContentList);
  public
    constructor Create(AInstance: TObject; ATool: TMCPTool);

    function Invoke(AParams: TCallToolParams; AResult: TCallToolResult): Boolean;
  end;

  TMCPResourceInvoker = class(TMCPInvoker)
  protected
    FResource: TMCPResource;
    procedure ResultToResource(const AMethodResult: TValue; AContentList: TResourceContentsList; AResource: TMCPResource);
  public
    constructor Create(AInstance: TObject; AResource: TMCPResource);

    function Invoke(AParams: TReadResourceParams; AResult: TReadResourceResult): Boolean;
  end;



implementation

uses
  System.StrUtils,
  System.NetEncoding,
  MCPConnect.Content.Writers;

function TMCPInvoker.ArgumentsToRttiParams(AArguments: TJSONObject; const AParams: TArray<TRttiParameter>): TArray<TValue>;

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
begin
  Result := [];

  for LParam in AParams do
  begin
    LParamJSON := AArguments.GetValue(GetParamName(LParam));
    Result := Result + [CastJSONValue(LParam, LParamJSON)];
  end;
end;

constructor TMCPInvoker.Create(AInstance: TObject);
begin
  FInstance := AInstance;
end;

function TMCPInvoker.GetParamName(LParam: TRttiParameter): string;
var
  LParamAttrib: MCPParamAttribute;
begin
  LParamAttrib := TRttiUtils.FindAttribute<MCPParamAttribute>(LParam);
  if Assigned(LParamAttrib) then
    Result := LParamAttrib.Name
  else
    Result := LParam.Name;
end;

constructor TMCPToolInvoker.Create(AInstance: TObject; ATool: TMCPTool);
begin
  inherited Create(AInstance);
  FTool := ATool;
end;

function TMCPToolInvoker.Invoke(AParams: TCallToolParams; AResult: TCallToolResult): Boolean;
var
  LArgs: TArray<TValue>;
  LResult: TValue;
begin
  Result := True;
  LArgs := ArgumentsToRttiParams(AParams.Arguments, FTool.Method.GetParameters);
  FGC.Add(LArgs);
  LResult := FTool.Method.Invoke(FInstance, LArgs);
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
    FGC.Add(LResult);
  end;
end;

procedure TMCPToolInvoker.ResultToTool(const AToolResult: TValue; AContentList: TContentList);
var
  LWriter: TMCPCustomWriter;
  LContext: TMCPToolContext;
  LContent: TToolContent;
  LText: TTextContent absolute LContent;
  LResText: TEmbeddedResourceText absolute LContent;
  LResBlob: TEmbeddedResourceBlob absolute LContent;
begin
  LWriter := FConfig.Server.WriterRegistry.GetWriter(AToolResult);
  if Assigned(LWriter) then
  begin
    LContext.Result := AContentList;
    LContext.Attributes := FTool.Method.GetAttributes;

    LWriter.WriteTool(AToolResult, LContext);
    Exit;
  end;

  case AToolResult.Kind of

    // As it is
    tkInt64,
    tkInteger,
    tkFloat: LText := TTextContent.Create(AToolResult.ToString);

    // Dequote
    tkEnumeration,
    tkChar,
    tkWChar,
    tkString,
    tkLString,
    tkWString,
    tkUString: LText := TTextContent.Create(AToolResult.ToString);

    // JSON response
    tkSet,
    tkClass,
    tkRecord, tkMRecord:
    begin
      // Check if the tool is configured to return an embedded resource
      //  { TODO -opaolo -c : Change the Neon configuration!!! 14/11/2025 10:25:55 }
      var LResult := TNeon.ValueToJSONString(AToolResult, TNeonConfiguration.Default);
      var LMCPTool := TRttiUtils.FindAttribute<MCPToolAttribute>(FTool.Method);
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

      if AToolResult.TypeInfo = TypeInfo(TBytes) then
      begin
        LResBlob.Resource.Blob := TNetEncoding.Base64String.EncodeBytesToString(AToolResult.AsType<TBytes>);
        LResBlob.Resource.MIMEType := 'application/octect-stream'
      end
      else
      begin
        var LResult := TNeon.ValueToJSONString(AToolResult, TNeonConfiguration.Default);
        LResBlob.Resource.MIMEType := 'application/json';
        LResBlob.Resource.Blob := LResult;
    end;
    end;

  else
    raise EMCPException.Create('Type kind not supported');
  end;

  AContentList.Add(LContent);
end;


{ TMCPResourceInvoker }

constructor TMCPResourceInvoker.Create(AInstance: TObject; AResource: TMCPResource);
begin
  inherited Create(AInstance);
  FResource := AResource;
end;

function TMCPResourceInvoker.Invoke(AParams: TReadResourceParams; AResult: TReadResourceResult): Boolean;
var
  LResult: TValue;
begin
  Result := True;

  LResult := FResource.Method.Invoke(FInstance, []);
  try
    // If the result is already a TResourceContents just assign it
    if LResult.IsType<TResourceContents> then
    begin
      AResult.Contents.Free;
      AResult.Contents := TResourceContentsList(LResult.AsObject);
      LResult := nil;
    end
    else
    begin
      ResultToResource(LResult, AResult.Contents, FResource);
    end;
  finally
    FGC.Add(LResult);
  end;
end;

procedure TMCPResourceInvoker.ResultToResource(const AMethodResult: TValue;
    AContentList: TResourceContentsList; AResource: TMCPResource);
var
  LMime: string;
  LEncoding: TMimeEncoding;
  LResult: string;
  LWriter: TMCPCustomWriter;
  LContext: TMCPResourceContext;

  LContent: TResourceContents;
  LResText: TTextResourceContents absolute LContent;
  LResBlob: TBlobResourceContents absolute LContent;
begin
  LMime := AResource.MimeType;
  LEncoding := TMimeEncoding.Plain;

  if not LMime.IsEmpty then
    LEncoding := FConfig.Resources.MimeTypes.EncodingByMedia(LMime);

  LWriter := FConfig.Server.WriterRegistry.GetWriter(AMethodResult);
  if Assigned(LWriter) then
  begin
    LContext.Result := AContentList;
    LContext.Attributes := FResource.Method.GetAttributes;

    LWriter.WriteResource(AMethodResult, LContext);
    Exit;
  end;

  case AMethodResult.Kind of

    // As it is
    tkInt64,
    tkInteger,
    tkFloat:
    begin
      if LEncoding = TMimeEncoding.Plain then
      begin
        LResText := TTextResourceContents.Create();
        LResText.Uri := AResource.Uri;
        LResText.MimeType := IfThen(LMime.IsEmpty, 'text/plain', LMime);
        LResText.Text := AMethodResult.ToString;
      end
      else
      begin
        LResBlob := TBlobResourceContents.Create();
        LResBlob.Uri := AResource.Uri;
        LResBlob.MimeType := LMime;
        LResBlob.Blob := TNetEncoding.Base64.Encode(AMethodResult.ToString);
      end
    end;

    // Dequote
    tkChar,
    tkWChar,
    tkString,
    tkLString,
    tkWString,
    tkUString:
    begin
      if LEncoding = TMimeEncoding.Plain then
      begin
        LResText := TTextResourceContents.Create();
        LResText.Uri := AResource.Uri;
        LResText.MimeType := IfThen(LMime.IsEmpty, 'text/plain', LMime);
        LResText.Text := AMethodResult.ToString;
      end
      else
      begin
        LResBlob := TBlobResourceContents.Create();
        LResBlob.Uri := AResource.Uri;
        LResBlob.MimeType := LMime;
        LResBlob.Blob := TNetEncoding.Base64.Encode(AMethodResult.ToString);
      end;
    end;

    // JSON response
    tkEnumeration,
    tkSet,
    tkClass,
    tkRecord, tkMRecord,
    tkArray, tkDynArray:
    begin
      LResult := TNeon.ValueToJSONString(AMethodResult, TNeonConfiguration.Default);
      if LEncoding = TMimeEncoding.Plain then
      begin
        LResText := TTextResourceContents.Create();
        LResText.Uri := AResource.Uri;
        LResText.MimeType := IfThen(LMime.IsEmpty, 'application/json', LMime);
        LResText.Text := LResult;
      end
      else
      begin
        LResBlob := TBlobResourceContents.Create();
        LResBlob.Uri := AResource.Uri;
        LResBlob.MimeType := LMime;
        LResBlob.Blob := TNetEncoding.Base64.Encode(LResult);
      end;
    end;

  else
    raise EMCPException.Create('Type kind not supported');

  end;

  AContentList.Add(LContent);
end;

end.
