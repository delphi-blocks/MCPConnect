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

  MCPConnect.JRPC.Core,
  MCPConnect.Configuration.Core,
  MCPConnect.Configuration.MCP,

  MCPConnect.MCP.Attributes,
  MCPConnect.MCP.Types,
  MCPConnect.MCP.Tools;

type
  /// <summary>
  ///   This interface define a standard method to handle MCP call
  /// </summary>
  IMCPInvokable = interface
  ['{3E475D9B-6863-4260-8789-74014419A52B}']
    function Invoke(const AName: string; AArguments: TJSONObject; Meta: TJSONObject; AResult: TCallToolResult): Boolean;
  end;

  /// <summary>
  ///   This class invoke a specific method on a given instance.
  ///   The method to be invoked is reached through RTTI
  ///   using the MCP specific attributes.
  /// </summary>
  TMCPObjectInvoker = class(TInterfacedObject, IMCPInvokable)
  private
    [Context] FContext: TJRPCContext;
    //[Context] FConfig: TMCPConfig;
  private
    FInstance: TObject;
    FRttiType: TRttiType;
    function FindMethod(const AName: string): TRttiMethod;
  public
    { IMCPInvokable }
    function Invoke(const AName: string; AArguments: TJSONObject; Meta: TJSONObject; AResult: TCallToolResult): Boolean;

    constructor Create(AInstance: TObject);
  end;

  TMCPMethodInvoker = class(TInterfacedObject, IMCPInvokable)
  private
    //[Context] FContext: TJRPCContext;
    [Context] FConfig: TMCPConfig;
  private
    FInstance: TObject;
    FMethod: TRttiMethod;
    function ArgumentsToRttiParams(AArguments: TJSONObject): TArray<TValue>;
    procedure ResultToContents(const AToolResult: TValue; AContentList: TContentList);
    function GetParamName(LParam: TRttiParameter): string;
  public
    { IMCPInvokable }
    function Invoke(const AName: string; AArguments: TJSONObject; Meta: TJSONObject; AResult: TCallToolResult): Boolean;

    constructor Create(AInstance: TObject; AMethod: TRttiMethod);
  end;

implementation

uses
  MCPConnect.Content.Writers,
  MCPConnect.Core.Utils;

{ TMCPObjectInvoker }

constructor TMCPObjectInvoker.Create(AInstance: TObject);
begin
  inherited Create;
  FInstance := AInstance;
  FRttiType := TRttiUtils.GetType(AInstance);
end;

function TMCPObjectInvoker.FindMethod(const AName: string): TRttiMethod;
var
  LMethod: TRttiMethod;
  LMethodName: string;
  LMPCToolAttrib: MCPToolAttribute;
begin
  Result := nil;
  for LMethod in FRttiType.GetMethods do
  begin
    LMPCToolAttrib := TRttiUtils.FindAttribute<MCPToolAttribute>(LMethod);
    if Assigned(LMPCToolAttrib) then
      LMethodName := LMPCToolAttrib.Name
    else
      LMethodName := '';

    if SameText(AName, LMethodName) then
      Exit(LMethod);
  end;
end;

function TMCPObjectInvoker.Invoke(const AName: string; AArguments,
  Meta: TJSONObject; AResult: TCallToolResult): Boolean;
var
  LMethod: TRttiMethod;
  LMethodInvoker: IMCPInvokable;
begin
  LMethod := FindMethod(AName);
  if not Assigned(LMethod) then
  begin
    AResult.IsError := True;
    Exit(False);
  end;

  LMethodInvoker := TMCPMethodInvoker.Create(FInstance, LMethod);
  FContext.Inject(LMethodInvoker);

  Result := LMethodInvoker.Invoke(AName, AArguments, Meta, AResult);
end;

{ TMCPMethodInvoker }

constructor TMCPMethodInvoker.Create(AInstance: TObject; AMethod: TRttiMethod);
begin
  inherited Create;
  FInstance := AInstance;
  FMethod := AMethod;
end;

function TMCPMethodInvoker.Invoke(const AName: string; AArguments,
  Meta: TJSONObject; AResult: TCallToolResult): Boolean;
var
  LArgs: TArray<TValue>;
  LResult: TValue;
  LGarbageCollector: IGarbageCollector;
begin
  Result := True;
  LGarbageCollector := TGarbageCollector.CreateInstance;
  LArgs := ArgumentsToRttiParams(AArguments);
  try
    LGarbageCollector.Add(LArgs);
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
        ResultToContents(LResult, AResult.Content);
    finally
      if LResult.IsObject then
        LResult.AsObject.Free;
    end;
  except
    on E: Exception do
    begin
      AResult.IsError := True;
    end;
  end;
end;

procedure TMCPMethodInvoker.ResultToContents(const AToolResult: TValue; AContentList: TContentList);
var
  LResult: string;
  LWriter: TMCPCustomWriter;
  LContext: TMCPWriterContext;
  LContent: TBaseContent;
  LText: TTextContent absolute LContent;
  LResText: TEmbeddedResourceText absolute LContent;
  LResBlob: TEmbeddedResourceBlob absolute LContent;
begin
  { TODO -opaolo -c : Change the Neon configuration!!! 14/11/2025 10:25:55 }
  LResult := TNeon.ValueToJSONString(AToolResult, TNeonConfiguration.Default);

  LWriter := FConfig.GetWriters.GetWriter(AToolResult);
  if Assigned(LWriter) then
  begin
    LContext.ContentList := AContentList;
    LContext.ToolAttributes := FMethod.GetAttributes;

    LWriter.Write(AToolResult, LContext);
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
