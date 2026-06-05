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

  MCPConnect.JRPC.Classes,
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
    procedure ResultToTool(const AToolResult: TValue; AResult: TCallToolResult);
  public
    constructor Create(AInstance: TObject; ATool: TMCPTool);

    function Invoke(AParams: TCallToolParams): TCallToolResult;
  end;

  TMCPResourceInvoker = class(TMCPInvoker)
  protected
    FResource: TMCPResource;
    procedure ResultToResource(const AMethodResult: TValue; AResult: TReadResourceResult);
  public
    constructor Create(AInstance: TObject; AResource: TMCPResource);

    function Invoke(AParams: TReadResourceParams): TReadResourceResult;
  end;

  TMCPTemplateInvoker = class(TMCPInvoker)
  protected
    FTemplate: TMCPResourceTemplate;
    function BuildTemplateParams(const AUri: string; const AParams: TArray<TRttiParameter>): TArray<TValue>;
    procedure ResultToResource(const AMethodResult: TValue; AResult: TReadResourceResult);
  public
    constructor Create(AInstance: TObject; ATemplate: TMCPResourceTemplate);

    function Invoke(AParams: TReadResourceParams): TReadResourceResult;
  end;

  TMCPPromptInvoker = class(TMCPInvoker)
  protected
    FPrompt: TMCPPrompt;
    procedure ResultToPrompt(const APromptResult: TValue; AResult: TGetPromptResult);
  public
    constructor Create(AInstance: TObject; APrompt: TMCPPrompt);

    function Invoke(AParams: TGetPromptParams): TGetPromptResult;
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

procedure TMCPToolInvoker.ResultToTool(const AToolResult: TValue; AResult: TCallToolResult);
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
    LContext.Result := AResult;
    LContext.Attributes := FTool.Method.GetAttributes;

    LWriter.WriteTool(AToolResult, LContext);
    Exit;
  end;

  var LMCPAttr := TRttiUtils.FindAttribute<MCPToolAttribute>(FTool.Method);
  case AToolResult.Kind of

    // As it is
    tkInt64,
    tkInteger,
    tkFloat: LText := TTextContent.CreateWithText(AToolResult.ToString);

    // As it is
    tkEnumeration,
    tkChar,
    tkWChar,
    tkString,
    tkLString,
    tkWString,
    tkUString: LText := TTextContent.CreateWithText(AToolResult.ToString);

    // JSON response
    tkSet,
    tkClass,
    tkRecord, tkMRecord:
    begin
      var LJSON := TNeon.ValueToJSON(AToolResult, FConfig.Tools.NeonConfig);
      try
        // Check if the tool is configured to return a structured content
        if Assigned(LMCPAttr) and LMCPAttr.Tags.Exists('structured') then
          AResult.StructuredContent := LJSON.Clone as TJSONValue;

        if Assigned(LMCPAttr) and (LMCPAttr.Tags.Exists('embedded')) then
        begin
          LResText := TEmbeddedResourceText.Create;
          LResText.Resource.MIMEType := 'application/json';
          LResText.Resource.URI := '';
          LResText.Resource.Text := LJSON.ToJSON;
        end
        else
        begin
          LText := TTextContent.CreateWithText(LJSON.ToJSON);
        end;
      finally
        LJSON.Free;
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
        var LJSON := TNeon.ValueToJSON(AToolResult, FConfig.Tools.NeonConfig);
        try
          // Check if the tool is configured to return a structured content
          if Assigned(LMCPAttr) and (LMCPAttr.Tags.Exists('structured')) then
            AResult.StructuredContent := LJSON.Clone as TJSONValue;

          LResBlob.Resource.MIMEType := 'application/json';
          LResBlob.Resource.Blob := LJSON.ToJSON;
        finally
          LJSON.Free;
        end;
      end;
    end;

  else
    raise EMCPException.Create('Type kind not supported');
  end;

  AResult.Content.Add(LContent);
end;

function TMCPToolInvoker.Invoke(AParams: TCallToolParams): TCallToolResult;
var
  LArgs: TArray<TValue>;
  LMethodResult: TValue;
begin
  LArgs := ArgumentsToRttiParams(AParams.Arguments, FTool.Method.GetParameters);
  FGC.Add(LArgs);
  LMethodResult := FTool.Method.Invoke(FInstance, LArgs);

  if LMethodResult.IsType<TCallToolResult> then
  begin
    Result := TCallToolResult(LMethodResult.AsObject);
    Exit;
  end;

  if LMethodResult.IsType<TContentList> then
  begin
    Result := TCallToolResult.Create(TContentList(LMethodResult.AsObject));
    Exit;
  end;

  FGC.Add(LMethodResult);
  Result := TCallToolResult.Create;
  ResultToTool(LMethodResult, Result);
end;


{ TMCPResourceInvoker }

constructor TMCPResourceInvoker.Create(AInstance: TObject; AResource: TMCPResource);
begin
  inherited Create(AInstance);
  FResource := AResource;
end;

function TMCPResourceInvoker.Invoke(AParams: TReadResourceParams): TReadResourceResult;
var
  LMethodResult: TValue;
begin
  LMethodResult := FResource.Method.Invoke(FInstance, []);

  // If the result is already a TReadResourceResult just assign it
  if LMethodResult.IsType<TReadResourceResult> then
    Exit(LMethodResult.AsObject as TReadResourceResult);

  // If the result is already a TResourceContents just assign it
  if LMethodResult.IsType<TResourceContents> then
    Exit(TReadResourceResult.Create(LMethodResult.AsObject as TResourceContentsList));

  FGC.Add(LMethodResult);
  Result := TReadResourceResult.Create;
  ResultToResource(LMethodResult, Result);
end;

procedure TMCPResourceInvoker.ResultToResource(const AMethodResult: TValue; AResult: TReadResourceResult);
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
  LMime := FResource.MimeType;
  LEncoding := TMimeEncoding.Plain;

  if not LMime.IsEmpty then
    LEncoding := FConfig.Resources.MimeTypes.EncodingByMedia(LMime);

  LWriter := FConfig.Server.WriterRegistry.GetWriter(AMethodResult);
  if Assigned(LWriter) then
  begin
    LContext.Result := AResult;
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
        LResText.Uri := FResource.Uri;
        LResText.MimeType := IfThen(LMime.IsEmpty, 'text/plain', LMime);
        LResText.Text := AMethodResult.ToString;
      end
      else
      begin
        LResBlob := TBlobResourceContents.Create();
        LResBlob.Uri := FResource.Uri;
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
        LResText.Uri := FResource.Uri;
        LResText.MimeType := IfThen(LMime.IsEmpty, 'text/plain', LMime);
        LResText.Text := AMethodResult.ToString;
      end
      else
      begin
        LResBlob := TBlobResourceContents.Create();
        LResBlob.Uri := FResource.Uri;
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
        LResText.Uri := FResource.Uri;
        LResText.MimeType := IfThen(LMime.IsEmpty, 'application/json', LMime);
        LResText.Text := LResult;
      end
      else
      begin
        LResBlob := TBlobResourceContents.Create();
        LResBlob.Uri := FResource.Uri;
        LResBlob.MimeType := LMime;
        LResBlob.Blob := TNetEncoding.Base64.Encode(LResult);
      end;
    end;

  else
    raise EMCPException.Create('Type kind not supported');

  end;

  AResult.Contents.Add(LContent);
end;

{ TMCPTemplateInvoker }

function TMCPTemplateInvoker.BuildTemplateParams(const AUri: string;
  const AParams: TArray<TRttiParameter>): TArray<TValue>;
var
  LParam: TRttiParameter;
  LParamName, LParamUri: string;
begin
  Result := [];

  var router := TRouteMatcher.Create;
  try
    if not router.Match(FTemplate.UriTemplate, AUri) then
      raise EMCPException.Create('URI not compatible with the template');

    if router.Params.Count <> Length(AParams) then
      raise EMCPException.Create('Parameters count from method and URI are different');

    for LParam in AParams do
    begin
      LParamName := GetParamName(LParam);
      if router.Params.TryGetValue(LParamName, LParamUri) then
        Result := Result + [LParamUri];
    end;

  finally
    router.Free;
  end;
end;

constructor TMCPTemplateInvoker.Create(AInstance: TObject; ATemplate: TMCPResourceTemplate);
begin
  inherited Create(AInstance);
  FTemplate := ATemplate;
end;

function TMCPTemplateInvoker.Invoke(AParams: TReadResourceParams): TReadResourceResult;
var
  LArgs: TArray<TValue>;
  LResult: TValue;
begin
  LArgs := BuildTemplateParams(AParams.Uri, FTemplate.Method.GetParameters);
  FGC.Add(LArgs);
  LResult := FTemplate.Method.Invoke(FInstance, LArgs);

  // If the result is already a TReadResourceResult just assign it
  if LResult.IsType<TReadResourceResult> then
    Exit(LResult.AsObject as TReadResourceResult);

  // If the result is already a TContentList just use it
  if LResult.IsType<TResourceContentsList> then
    Exit(TReadResourceResult.Create(LResult.AsObject as TResourceContentsList));

  FGC.Add(LResult);
  Result := TReadResourceResult.Create;
  ResultToResource(LResult, Result);
end;

procedure TMCPTemplateInvoker.ResultToResource(const AMethodResult: TValue; AResult: TReadResourceResult);
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
  LMime := FTemplate.MimeType;
  LEncoding := TMimeEncoding.Plain;

  if not LMime.IsEmpty then
    LEncoding := FConfig.Resources.MimeTypes.EncodingByMedia(LMime);

  LWriter := FConfig.Server.WriterRegistry.GetWriter(AMethodResult);
  if Assigned(LWriter) then
  begin
    LContext.Result := AResult;
    LContext.Attributes := FTemplate.Method.GetAttributes;

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
        LResText.Uri := FTemplate.UriTemplate;
        LResText.MimeType := IfThen(LMime.IsEmpty, 'text/plain', LMime);
        LResText.Text := AMethodResult.ToString;
      end
      else
      begin
        LResBlob := TBlobResourceContents.Create();
        LResBlob.Uri := FTemplate.UriTemplate;
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
        LResText.Uri := FTemplate.UriTemplate;
        LResText.MimeType := IfThen(LMime.IsEmpty, 'text/plain', LMime);
        LResText.Text := AMethodResult.ToString;
      end
      else
      begin
        LResBlob := TBlobResourceContents.Create();
        LResBlob.Uri := FTemplate.UriTemplate;
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
        LResText.Uri := FTemplate.UriTemplate;
        LResText.MimeType := IfThen(LMime.IsEmpty, 'application/json', LMime);
        LResText.Text := LResult;
      end
      else
      begin
        LResBlob := TBlobResourceContents.Create();
        LResBlob.Uri := FTemplate.UriTemplate;
        LResBlob.MimeType := LMime;
        LResBlob.Blob := TNetEncoding.Base64.Encode(LResult);
      end;
    end;

  else
    raise EMCPException.Create('Type kind not supported');

  end;

  AResult.Contents.Add(LContent);
end;

{ TMCPPromptInvoker }

constructor TMCPPromptInvoker.Create(AInstance: TObject; APrompt: TMCPPrompt);
begin
  inherited Create(AInstance);
  FPrompt := APrompt;
end;

function TMCPPromptInvoker.Invoke(AParams: TGetPromptParams): TGetPromptResult;
var
  LArgs: TArray<TValue>;
  LMethodResult: TValue;
begin
  LArgs := ArgumentsToRttiParams(AParams.Arguments, FPrompt.Method.GetParameters);
  FGC.Add(LArgs);
  LMethodResult := FPrompt.Method.Invoke(FInstance, LArgs);

  // If the result is already a TGetPromptResult just assign it
  if LMethodResult.IsType<TGetPromptResult> then
    Exit(LMethodResult.AsObject as TGetPromptResult);

  // If the result is already a TPromptMessages just use it
  if LMethodResult.IsType<TPromptMessages> then
    Exit(TGetPromptResult.Create(LMethodResult.AsObject as TPromptMessages));

  FGC.Add(LMethodResult);
  Result := TGetPromptResult.Create;
  ResultToPrompt(LMethodResult, Result);
end;

procedure TMCPPromptInvoker.ResultToPrompt(const APromptResult: TValue; AResult: TGetPromptResult);
var
  LWriter: TMCPCustomWriter;
  LContext: TMCPPromptContext;
begin
  LWriter := FConfig.Server.WriterRegistry.GetWriter(APromptResult);
  if Assigned(LWriter) then
  begin
    LContext.Result := AResult;
    LContext.Attributes := FPrompt.Method.GetAttributes;

    LWriter.WritePrompt(APromptResult, LContext);
    Exit;
  end;

  case APromptResult.Kind of

    // As it is
    tkInt64,
    tkInteger,
    tkFloat: AResult.Messages.AddText('user', APromptResult.ToString);

    // Dequote
    tkEnumeration,
    tkChar,
    tkWChar,
    tkString,
    tkLString,
    tkWString,
    tkUString: AResult.Messages.AddText('user', APromptResult.ToString);

    // JSON response
    tkSet,
    tkClass,
    tkRecord, tkMRecord:
    begin
      // Check if the tool is configured to return an embedded resource
      //  { TODO -opaolo -c : Change the Neon configuration!!! 14/11/2025 10:25:55 }
      var LResult := TNeon.ValueToJSONString(APromptResult, TNeonConfiguration.Default);
      var LMCPPrompt := TRttiUtils.FindAttribute<MCPToolAttribute>(FPrompt.Method);
      if Assigned(LMCPPrompt) and (LMCPPrompt.Tags.Exists('embedded')) then
        AResult.Messages.AddBlob('user', 'application/json', LResult)
      else
        AResult.Messages.AddText('user', LResult);
    end;


    tkArray, tkDynArray:
    begin
      var LBlob, LMime: string;
      if APromptResult.TypeInfo = TypeInfo(TBytes) then
      begin
        LBlob := TNetEncoding.Base64String.EncodeBytesToString(APromptResult.AsType<TBytes>);
        LMime := 'application/octect-stream';
      end
      else
      begin
        LBlob := TNeon.ValueToJSONString(APromptResult, TNeonConfiguration.Default);
        LMime := 'application/json';
      end;
      AResult.Messages.AddBlob('user', LMime, LBlob);
    end;

  else
    raise EMCPException.Create('Type kind not supported');

  end;

end;

end.
