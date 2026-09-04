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
  MCPConnect.MCP.Types.Base,
  MCPConnect.MCP.Types.Tool,
  MCPConnect.MCP.Types.Tools,
  MCPConnect.MCP.Types.Prompts,
  MCPConnect.MCP.Types.Resources,
  MCPConnect.MCP.Types.Completion,
  MCPConnect.MCP.Types.Mrtr;

type
  TMCPInvoker = class
  protected
    FInstance: TObject;

    [Context] FConfig: TMCPConfig;
    [Context] FGC: IGarbageCollector;

    function GetParamName(AParam: TRttiParameter): string; virtual;
    function ArgumentsToRttiParams(AArguments: TJSONObject; const AParams: TArray<TRttiParameter>): TArray<TValue>;

    constructor Create(AInstance: TObject);
  end;

  TMCPToolInvoker = class(TMCPInvoker)
  protected
    FTool: TMCPTool;
    function GetParamName(AParam: TRttiParameter): string; override;
    procedure ResultToTool(const AToolResult: TValue; AResult: TCallToolResult);  public
    constructor Create(AInstance: TObject; ATool: TMCPTool);

    /// <summary>
    ///   Returns a TCallToolResult, or a TInputRequiredResult when the tool
    ///   needs more from the client before it can finish (MRTR).
    /// </summary>
    function Invoke(AParams: TCallToolRequestParams): TBaseResult;
  end;

  TMCPResourceInvoker = class(TMCPInvoker)
  protected
    FResource: TMCPResource;
    procedure ResultToResource(const AMethodResult: TValue; AResult: TReadResourceResult);
  public
    constructor Create(AInstance: TObject; AResource: TMCPResource);

    /// <summary>
    ///   Returns a TReadResourceResult, or a TInputRequiredResult (MRTR).
    /// </summary>
    function Invoke(AParams: TReadResourceParams): TBaseResult;
  end;

  TMCPTemplateInvoker = class(TMCPInvoker)
  protected
    FTemplate: TMCPResourceTemplate;
    function GetParamName(AParam: TRttiParameter): string; override;
    function BuildTemplateParams(const AUri: string; const AParams: TArray<TRttiParameter>): TArray<TValue>;
    procedure ResultToResource(const AMethodResult: TValue; AResult: TReadResourceResult);
  public
    constructor Create(AInstance: TObject; ATemplate: TMCPResourceTemplate);

    /// <summary>
    ///   Returns a TReadResourceResult, or a TInputRequiredResult (MRTR).
    /// </summary>
    function Invoke(AParams: TReadResourceParams): TBaseResult;
  end;

  TMCPPromptInvoker = class(TMCPInvoker)
  protected
    FPrompt: TMCPPrompt;
    function GetParamName(AParam: TRttiParameter): string; override;
    procedure ResultToPrompt(const APromptResult: TValue; AResult: TGetPromptResult);
  public
    constructor Create(AInstance: TObject; APrompt: TMCPPrompt);

    /// <summary>
    ///   Returns a TGetPromptResult, or a TInputRequiredResult (MRTR).
    /// </summary>
    function Invoke(AParams: TGetPromptRequestParams): TBaseResult;
  end;

  /// <summary>
  ///   Invokes a registered completion provider for a "completion/complete"
  ///   request and turns whatever it returns into a TCompleteResult.
  /// </summary>
  TMCPCompletionInvoker = class(TMCPInvoker)
  protected
    FProvider: TMCPCompletionProvider;
    function BuildArgs(AParams: TCompleteRequestParams): TArray<TValue>;
    procedure ResultToCompletion(const AMethodResult: TValue; AResult: TCompleteResult);
  public
    constructor Create(AInstance: TObject; AProvider: TMCPCompletionProvider);

    function Invoke(AParams: TCompleteRequestParams): TCompleteResult;
  end;



implementation

uses
  System.StrUtils,
  System.NetEncoding,
  System.Diagnostics,
  Logify,
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

function TMCPInvoker.GetParamName(AParam: TRttiParameter): string;
var
  LParamAttrib: MCPParamAttribute;
begin
  LParamAttrib := TRttiUtils.FindAttribute<MCPParamAttribute>(AParam);
  if Assigned(LParamAttrib) then
    Result := LParamAttrib.Name
  else
    Result := AParam.Name;
end;

constructor TMCPToolInvoker.Create(AInstance: TObject; ATool: TMCPTool);
begin
  inherited Create(AInstance);
  FTool := ATool;
end;

function TMCPToolInvoker.GetParamName(AParam: TRttiParameter): string;
var
  LParam: TMCPToolParam;
begin
  LParam := FTool.FindMCPParam(AParam.Name);
  if Assigned(LParam) then
    Result := LParam.Name
  else
    Result := AParam.Name;
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
        if FTool.Tags.Exists('structured') then
          AResult.StructuredContent := LJSON.Clone as TJSONObject;

        if FTool.Tags.Exists('embedded') then
        begin
          LResText := TEmbeddedResourceText.Create;
          LResText.Resource.MIMEType := TMediaType.Json;
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
        LResBlob.Resource.MIMEType := TMediaType.OctectStream;
      end
      else
      begin
        var LJSON := TNeon.ValueToJSON(AToolResult, FConfig.Tools.NeonConfig);
        try
          // outputSchema and structuredContent are (for now) limited to a JSON Object
          // See: https://github.com/modelcontextprotocol/php-sdk/issues/357

          // Check if the tool is configured to return a structured content
          if FTool.Tags.Exists('structured') then
            raise EMCPException.Create(SMCPStructuredContentMustBeObject);

          LResBlob.Resource.MIMEType := TMediaType.Json;
          LResBlob.Resource.Blob := LJSON.ToJSON;
        finally
          LJSON.Free;
        end;
      end;
    end;

  else
    raise EMCPException.Create(SMCPTypeKindNotSupported);
  end;

  AResult.Content.Add(LContent);
end;

function TMCPToolInvoker.Invoke(AParams: TCallToolRequestParams): TBaseResult;
var
  LArgs: TArray<TValue>;
  LMethodResult: TValue;
  LToolResult: TCallToolResult;
  LStopwatch: TStopwatch;
begin
  LStopwatch := TStopwatch.StartNew;
  LArgs := ArgumentsToRttiParams(AParams.Arguments, FTool.Method.GetParameters);
  FGC.Add(LArgs);
  Logger.LogDebug('[PERF] Tool [%s] ArgumentsToRttiParams: %d ms', [FTool.Name, LStopwatch.ElapsedMilliseconds]);

  LStopwatch := TStopwatch.StartNew;
  LMethodResult := FTool.Method.Invoke(FInstance, LArgs);
  Logger.LogDebug('[PERF] Tool [%s] Method.Invoke (business logic): %d ms', [FTool.Name, LStopwatch.ElapsedMilliseconds]);

  if LMethodResult.IsType<TCallToolResult> then
  begin
    Result := TCallToolResult(LMethodResult.AsObject);
    Exit;
  end;

  // A tool that cannot finish yet answers with the requests it needs filled
  if LMethodResult.IsType<TInputRequiredResult> then
  begin
    Result := TInputRequiredResult(LMethodResult.AsObject);
    Exit;
  end;

  if LMethodResult.IsType<TContentList> then
  begin
    Result := TCallToolResult.Create(TContentList(LMethodResult.AsObject));
    Exit;
  end;

  FGC.Add(LMethodResult);

  LToolResult := TCallToolResult.Create;
  Result := LToolResult;

  LStopwatch := TStopwatch.StartNew;
  ResultToTool(LMethodResult, LToolResult);
  Logger.LogDebug('[PERF] Tool [%s] ResultToTool (result serialization): %d ms', [FTool.Name, LStopwatch.ElapsedMilliseconds]);
end;


{ TMCPResourceInvoker }

constructor TMCPResourceInvoker.Create(AInstance: TObject; AResource: TMCPResource);
begin
  inherited Create(AInstance);
  FResource := AResource;
end;

function TMCPResourceInvoker.Invoke(AParams: TReadResourceParams): TBaseResult;
var
  LMethodResult: TValue;
  LResourceResult: TReadResourceResult;
  LStopwatch: TStopwatch;
begin
  LStopwatch := TStopwatch.StartNew;
  LMethodResult := FResource.Method.Invoke(FInstance, []);
  Logger.LogDebug('[PERF] Resource [%s] Method.Invoke (business logic): %d ms', [FResource.Uri, LStopwatch.ElapsedMilliseconds]);

  // If the result is already a TReadResourceResult just assign it
  if LMethodResult.IsType<TReadResourceResult> then
    Exit(LMethodResult.AsObject as TReadResourceResult);

  // If the result is already a TResourceContents just assign it
  if LMethodResult.IsType<TResourceContents> then
    Exit(TReadResourceResult.Create(LMethodResult.AsObject as TResourceContentsList));

  // A resource that cannot be served without more input answers with the
  // requests it needs filled (MRTR)
  if LMethodResult.IsType<TInputRequiredResult> then
    Exit(TInputRequiredResult(LMethodResult.AsObject));

  FGC.Add(LMethodResult);

  LResourceResult := TReadResourceResult.Create;
  Result := LResourceResult;

  LStopwatch := TStopwatch.StartNew;
  ResultToResource(LMethodResult, LResourceResult);
  Logger.LogDebug('[PERF] Resource [%s] ResultToResource (result serialization): %d ms', [FResource.Uri, LStopwatch.ElapsedMilliseconds]);
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
        LResText.MimeType := IfThen(LMime.IsEmpty, TMediaType.Text, LMime);
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
        LResText.MimeType := IfThen(LMime.IsEmpty, TMediaType.Text, LMime);
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
        LResText.MimeType := IfThen(LMime.IsEmpty, TMediaType.Json, LMime);
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
    raise EMCPException.Create(SMCPTypeKindNotSupported);

  end;

  AResult.Contents.Add(LContent);
end;

{ TMCPTemplateInvoker }

function TMCPTemplateInvoker.GetParamName(AParam: TRttiParameter): string;
var
  LParam: TMCPResTemplateParam;
begin
  LParam := FTemplate.FindMCPParam(AParam.Name);
  if Assigned(LParam) then
    Result := LParam.Name
  else
    Result := AParam.Name;
end;

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
      raise EMCPException.Create(SMCPUriNotCompatibleWithTemplate);

    if router.Params.Count <> Length(AParams) then
      raise EMCPException.Create(SMCPParamsCountMismatch);

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

function TMCPTemplateInvoker.Invoke(AParams: TReadResourceParams): TBaseResult;
var
  LArgs: TArray<TValue>;
  LResult: TValue;
  LResourceResult: TReadResourceResult;
  LStopwatch: TStopwatch;
begin
  LStopwatch := TStopwatch.StartNew;
  LArgs := BuildTemplateParams(AParams.Uri, FTemplate.Method.GetParameters);
  FGC.Add(LArgs);
  Logger.LogDebug('[PERF] Template [%s] BuildTemplateParams: %d ms', [FTemplate.UriTemplate.Value, LStopwatch.ElapsedMilliseconds]);

  LStopwatch := TStopwatch.StartNew;
  LResult := FTemplate.Method.Invoke(FInstance, LArgs);
  Logger.LogDebug('[PERF] Template [%s] Method.Invoke (business logic): %d ms', [FTemplate.UriTemplate.Value, LStopwatch.ElapsedMilliseconds]);

  // If the result is already a TReadResourceResult just assign it
  if LResult.IsType<TReadResourceResult> then
    Exit(LResult.AsObject as TReadResourceResult);

  // If the result is already a TContentList just use it
  if LResult.IsType<TResourceContentsList> then
    Exit(TReadResourceResult.Create(LResult.AsObject as TResourceContentsList));

  // A template that cannot be served without more input answers with the
  // requests it needs filled (MRTR)
  if LResult.IsType<TInputRequiredResult> then
    Exit(TInputRequiredResult(LResult.AsObject));

  FGC.Add(LResult);

  LResourceResult := TReadResourceResult.Create;
  Result := LResourceResult;

  LStopwatch := TStopwatch.StartNew;
  ResultToResource(LResult, LResourceResult);
  Logger.LogDebug('[PERF] Template [%s] ResultToResource (result serialization): %d ms', [FTemplate.UriTemplate.Value, LStopwatch.ElapsedMilliseconds]);
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
        LResText.MimeType := IfThen(LMime.IsEmpty, TMediaType.Text, LMime);
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
        LResText.MimeType := IfThen(LMime.IsEmpty, TMediaType.Text, LMime);
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
        LResText.MimeType := IfThen(LMime.IsEmpty, TMediaType.Json, LMime);
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
    raise EMCPException.Create(SMCPTypeKindNotSupported);

  end;

  AResult.Contents.Add(LContent);
end;

{ TMCPPromptInvoker }

constructor TMCPPromptInvoker.Create(AInstance: TObject; APrompt: TMCPPrompt);
begin
  inherited Create(AInstance);
  FPrompt := APrompt;
end;

function TMCPPromptInvoker.GetParamName(AParam: TRttiParameter): string;
var
  LParam: TMCPPromptParam;
begin
  LParam := FPrompt.FindMCPParam(AParam.Name);
  if Assigned(LParam) then
    Result := LParam.Name
  else
    Result := AParam.Name;
end;

function TMCPPromptInvoker.Invoke(AParams: TGetPromptRequestParams): TBaseResult;
var
  LArgs: TArray<TValue>;
  LMethodResult: TValue;
  LPromptResult: TGetPromptResult;
  LStopwatch: TStopwatch;
begin
  LStopwatch := TStopwatch.StartNew;
  LArgs := ArgumentsToRttiParams(AParams.Arguments, FPrompt.Method.GetParameters);
  FGC.Add(LArgs);
  Logger.LogDebug('[PERF] Prompt [%s] ArgumentsToRttiParams: %d ms', [FPrompt.Name, LStopwatch.ElapsedMilliseconds]);

  LStopwatch := TStopwatch.StartNew;
  LMethodResult := FPrompt.Method.Invoke(FInstance, LArgs);
  Logger.LogDebug('[PERF] Prompt [%s] Method.Invoke (business logic): %d ms', [FPrompt.Name, LStopwatch.ElapsedMilliseconds]);

  // If the result is already a TGetPromptResult just assign it
  if LMethodResult.IsType<TGetPromptResult> then
    Exit(LMethodResult.AsObject as TGetPromptResult);

  // If the result is already a TPromptMessages just use it
  if LMethodResult.IsType<TPromptMessages> then
    Exit(TGetPromptResult.Create(LMethodResult.AsObject as TPromptMessages));

  // A prompt that cannot be built without more input answers with the
  // requests it needs filled (MRTR)
  if LMethodResult.IsType<TInputRequiredResult> then
    Exit(TInputRequiredResult(LMethodResult.AsObject));

  FGC.Add(LMethodResult);

  LPromptResult := TGetPromptResult.Create;
  Result := LPromptResult;

  LStopwatch := TStopwatch.StartNew;
  ResultToPrompt(LMethodResult, LPromptResult);
  Logger.LogDebug('[PERF] Prompt [%s] ResultToPrompt (result serialization): %d ms', [FPrompt.Name, LStopwatch.ElapsedMilliseconds]);
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
    tkFloat: AResult.Messages.AddText(TRole.User, APromptResult.ToString);

    // Dequote
    tkEnumeration,
    tkChar,
    tkWChar,
    tkString,
    tkLString,
    tkWString,
    tkUString: AResult.Messages.AddText(TRole.User, APromptResult.ToString);

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
        AResult.Messages.AddBlob(TRole.User, TMediaType.Json, LResult)
      else
        AResult.Messages.AddText(TRole.User, LResult);
    end;


    tkArray, tkDynArray:
    begin
      var LBlob, LMime: string;
      if APromptResult.TypeInfo = TypeInfo(TBytes) then
      begin
        LBlob := TNetEncoding.Base64String.EncodeBytesToString(APromptResult.AsType<TBytes>);
        LMime := TMediaType.OctectStream;
      end
      else
      begin
        LBlob := TNeon.ValueToJSONString(APromptResult, TNeonConfiguration.Default);
        LMime := TMediaType.Json;
      end;
      AResult.Messages.AddBlob(TRole.User, LMime, LBlob);
    end;

  else
    raise EMCPException.Create(SMCPTypeKindNotSupported);

  end;

end;

{ TMCPCompletionInvoker }

constructor TMCPCompletionInvoker.Create(AInstance: TObject; AProvider: TMCPCompletionProvider);
begin
  inherited Create(AInstance);
  FProvider := AProvider;
end;

function TMCPCompletionInvoker.BuildArgs(AParams: TCompleteRequestParams): TArray<TValue>;
var
  LParams: TArray<TRttiParameter>;
begin
  // A provider says how much of the request it wants by its arity: nothing, the
  // value typed so far, or that value plus the already-resolved arguments.
  LParams := FProvider.Method.GetParameters;

  if Length(LParams) > 2 then
    raise EMCPException.CreateFmt(SCompletionMethodSignatureFmt, [FProvider.Method.Name]);

  if (Length(LParams) >= 1) and (LParams[0].ParamType.TypeKind <> tkUString) then
    raise EMCPException.CreateFmt(SCompletionMethodSignatureFmt, [FProvider.Method.Name]);

  if (Length(LParams) = 2) and (LParams[1].ParamType.Handle <> TypeInfo(TMCPCompletionContext)) then
    raise EMCPException.CreateFmt(SCompletionMethodSignatureFmt, [FProvider.Method.Name]);

  case Length(LParams) of
    1: Result := [TValue.From<string>(AParams.Argument.Value)];
    2: Result := [TValue.From<string>(AParams.Argument.Value),
                  TValue.From<TMCPCompletionContext>(AParams.Context)];
  else
    Result := [];
  end;
end;

function TMCPCompletionInvoker.Invoke(AParams: TCompleteRequestParams): TCompleteResult;
var
  LMethodResult: TValue;
  LStopwatch: TStopwatch;
begin
  LStopwatch := TStopwatch.StartNew;
  LMethodResult := FProvider.Method.Invoke(FInstance, BuildArgs(AParams));
  Logger.LogDebug('[PERF] Completion [%s/%s] Method.Invoke (business logic): %d ms',
    [FProvider.RefTarget, FProvider.Argument, LStopwatch.ElapsedMilliseconds]);

  // A provider that builds the whole result itself keeps control of total/hasMore
  if LMethodResult.IsType<TCompleteResult> then
    Exit(LMethodResult.AsObject as TCompleteResult);

  FGC.Add(LMethodResult);
  Result := TCompleteResult.Create;
  try
    ResultToCompletion(LMethodResult, Result);
  except
    Result.Free;
    raise;
  end;
end;

procedure TMCPCompletionInvoker.ResultToCompletion(const AMethodResult: TValue;
  AResult: TCompleteResult);
begin
  if AMethodResult.IsType<TArray<string>> then
  begin
    AResult.SetValues(AMethodResult.AsType<TArray<string>>);
    Exit;
  end;

  if AMethodResult.IsObject and (AMethodResult.AsObject is TStrings) then
  begin
    AResult.SetValues(TStrings(AMethodResult.AsObject).ToStringArray);
    Exit;
  end;

  raise EMCPException.CreateFmt(SCompletionResultTypeFmt, [FProvider.Method.Name]);
end;

end.
