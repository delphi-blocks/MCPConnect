unit JSON.RPC;

interface

uses
  System.SysUtils, System.Rtti, System.Classes, System.Generics.Collections,
  System.TypInfo, System.JSON,

  Neon.Core.Utils,
  Neon.Core.Nullables,
  Neon.Core.Attributes,
  Neon.Core.Persistence,
  Neon.Core.Persistence.JSON,
  Neon.Core.Serializers.RTL;

const
  // Standard JSON-RPC error codes
	JRPC_PARSE_ERROR      = -32700; // Invalid JSON was received by the server.
	JRPC_INVALID_REQUEST  = -32600; // The JSON sent is not a valid Request object.
	JRPC_METHOD_NOT_FOUND = -32601; // The method does not exist / is not available.
	JRPC_INVALID_PARAMS   = -32602; // Invalid method parameter(s).
	JRPC_INTERNAL_ERROR   = -32603; // Internal error.	Internal JSON-RPC error.

type
  EJSONRPCException = class(Exception);

  TJRPCID = record
  private
    [NeonInclude] id: TValue;
  public
    class operator Implicit(ASource: Integer): TJRPCID;
    class operator Implicit(ASource: string): TJRPCID;

    class operator Implicit(const ASource: TJRPCID): Integer;
    class operator Implicit(const ASource: TJRPCID): string;
  end;

  TJRPCEnvelope = class
  public const
	  JSONRPC_VERSION = '2.0';
  protected
    FId: TJRPCID;
    FJsonRpc: string;
    function GetNeonConfig: INeonConfiguration;
  public
    constructor Create;

    [NeonProperty('jsonrpc')]
    property JsonRpc: string read FJsonRpc write FJsonRpc;

    [NeonProperty('id'), NeonUnwrapped]
    property Id: TJRPCID read FId write FId;
  end;

  TJRPCParamList = class(TObjectList<TJSONValue>)
  public
    function ToJson: TJSONArray;
  end;

  TJRPCParamDictionary = class(TObjectDictionary<string, TJSONValue>)
  public
    function ToJson: TJSONObject;
  end;

  TJRPCParamsType = (ByPos, ByName, Null);
  TJRPCParams = class
  private
    FByName: TJRPCParamDictionary;
    FByPos: TJRPCParamList;
    function GetCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddParam(const AValue: TJSONValue); overload;
    procedure AddParam(const AName: string; const AValue: TJSONValue); overload;
    function ParamsType: TJRPCParamsType;

    property ByPos: TJRPCParamList read FByPos write FByPos;
    property ByName: TJRPCParamDictionary read FByName write FByName;
    property Count: Integer read GetCount;
  end;

  TJRPCError = class
  private
    FCode: NullInteger;
    FData: TValue;
    FMessage: NullString;
  public
    [NeonProperty('code')]
    property Code: NullInteger read FCode write FCode;

    [NeonProperty('message')]
    property Message: NullString read FMessage write FMessage;

    [NeonProperty('data'), NeonInclude(IncludeIf.NotNull)]
    property Data: TValue read FData write FData;
  end;

  TJRPCRequest = class(TJRPCEnvelope)
  protected
    FMethod: string;
    FParams: TJSONValue;

    function GetPositionParams: TJSONArray;
    function GetNamedParams: TJSONObject;
  public
    constructor Create;
    destructor Destroy; override;

    function ParamsType: TJRPCParamsType;
    function ParamsCount: Integer;

    procedure AddPositionParam(const AValue: TValue);
    procedure AddNamedParam(const AName: string; const AValue: TValue);
    procedure SetParams(AValue: TJSONValue);

    function ToJson: string;

    [NeonProperty('method')]
    property Method: string read FMethod write FMethod;

    [NeonProperty('params')]
    property Params: TJSONValue read FParams write FParams;
  end;

  TJRPCResponse = class(TJRPCEnvelope)
  protected
    FError: TJRPCError;
    FResult: TValue;
  public
    constructor Create;
    destructor Destroy; override;

    function IsError: Boolean;
    procedure SetResult(AValue: TValue);

    [NeonProperty('error')]
    property Error: TJRPCError read FError write FError;

    [NeonProperty('result')]
    property Result: TValue read FResult write FResult;
  end;


  /// <summary>
  ///   Custom serializer for the TJRPCRequest class.
  /// </summary>
  TJRequestSerializer = class(TCustomSerializer)
  protected
    class function GetTargetInfo: PTypeInfo; override;
    class function CanHandle(AType: PTypeInfo): Boolean; override;
  public
    function Serialize(const AValue: TValue; ANeonObject: TNeonRttiObject; AContext: ISerializerContext): TJSONValue; override;
    function Deserialize(AValue: TJSONValue; const AData: TValue; ANeonObject: TNeonRttiObject; AContext: IDeserializerContext): TValue; override;
  end;

  /// <summary>
  ///   Custom serializer for the TJRPCResponse class.
  /// </summary>
  TJResponseSerializer = class(TCustomSerializer)
  protected
    class function GetTargetInfo: PTypeInfo; override;
    class function CanHandle(AType: PTypeInfo): Boolean; override;
  public
    function Serialize(const AValue: TValue; ANeonObject: TNeonRttiObject; AContext: ISerializerContext): TJSONValue; override;
    function Deserialize(AValue: TJSONValue; const AData: TValue; ANeonObject: TNeonRttiObject; AContext: IDeserializerContext): TValue; override;
  end;

  /// <summary>
  ///   Custom serializer for the TValue record.
  /// </summary>
  TJValueSerializer = class(TCustomSerializer)
  protected
    class function GetTargetInfo: PTypeInfo; override;
    class function CanHandle(AType: PTypeInfo): Boolean; override;
  public
    function Serialize(const AValue: TValue; ANeonObject: TNeonRttiObject; AContext: ISerializerContext): TJSONValue; override;
    function Deserialize(AValue: TJSONValue; const AData: TValue; ANeonObject: TNeonRttiObject; AContext: IDeserializerContext): TValue; override;
  end;

function JRPCNeonConfig: INeonConfiguration;

implementation


function JRPCNeonConfig: INeonConfiguration;
begin
  Result := TNeonConfiguration.Default
    .RegisterSerializer(TJSONValueSerializer)
    .RegisterSerializer(TJValueSerializer)
    .RegisterSerializer(TJRequestSerializer)
    .RegisterSerializer(TJResponseSerializer);
end;


{ TJRPCEnvelope }

constructor TJRPCEnvelope.Create;
begin
  FJsonRpc := JSONRPC_VERSION;
end;

function TJRPCEnvelope.GetNeonConfig: INeonConfiguration;
begin
  Result := TNeonConfiguration.Default
    .RegisterSerializer(TJSONValueSerializer)
    .RegisterSerializer(TJValueSerializer)
    .RegisterSerializer(TJValueSerializer)
    .RegisterSerializer(TJResponseSerializer);
end;

constructor TJRPCRequest.Create;
begin
  inherited;
  FParams := TJSONNull.Create;
end;

destructor TJRPCRequest.Destroy;
begin
  FParams.Free;
  inherited;
end;

function TJRPCRequest.GetNamedParams: TJSONObject;
begin
  if FParams is TJSONArray then
    raise EJSONRPCException.Create('Only named params are allowed');

  if FParams is TJSONObject then
    Exit(FParams as TJSONObject);

  if FParams is TJSONNull then
  begin
    FParams.Free;
    Result := TJSONObject.Create;
    FParams := Result;
  end;
end;

function TJRPCRequest.GetPositionParams: TJSONArray;
begin
  if FParams is TJSONObject then
    raise EJSONRPCException.Create('Only position params are allowed');

  if FParams is TJSONArray then
    Exit(FParams as TJSONArray);

  if FParams is TJSONNull then
  begin
    FParams.Free;
    Result := TJSONArray.Create;
    FParams := Result;
  end;
end;

function TJRPCRequest.ParamsCount: Integer;
begin
  Result := 0;

  if FParams is TJSONNull then
    Exit(0);

  if FParams is TJSONArray then
    Exit((FParams as TJSONArray).Count);

  if FParams is TJSONObject then
    Exit((FParams as TJSONObject).Count);
end;

function TJRPCRequest.ParamsType: TJRPCParamsType;
begin
  Result := TJRPCParamsType.Null;

  if FParams is TJSONNull then
    Exit(TJRPCParamsType.Null);

  if FParams is TJSONArray then
    Exit(TJRPCParamsType.ByPos);

  if FParams is TJSONObject then
    Exit(TJRPCParamsType.ByName);
end;

procedure TJRPCRequest.AddNamedParam(const AName: string; const AValue: TValue);
var
  LParam: TJSONValue;
begin
  LParam := TNeon.ValueToJSON(AValue, GetNeonConfig);
  if Assigned(LParam) then
    GetNamedParams.AddPair(AName, LParam);
end;

procedure TJRPCRequest.AddPositionParam(const AValue: TValue);
var
  LParam: TJSONValue;
begin
  LParam := TNeon.ValueToJSON(AValue, JRPCNeonConfig);
  if Assigned(LParam) then
    GetPositionParams.AddElement(LParam);
end;

procedure TJRPCRequest.SetParams(AValue: TJSONValue);
begin
  if AValue is TJSONObject then
  begin
    FParams.Free;
    FParams := AValue;
  end;
end;

function TJRPCRequest.ToJson: string;
begin
  Result := TNeon.ObjectToJSONString(Self, JRPCNeonConfig);
end;

{ TJRPCParams }

procedure TJRPCParams.AddParam(const AName: string; const AValue: TJSONValue);
begin
  if ByPos.Count > 0 then
    raise Exception.Create('Only position params are allowed');

  ByName.Add(AName, AValue);
end;

constructor TJRPCParams.Create;
begin
  FByName := TJRPCParamDictionary.Create([doOwnsValues]);
  FByPos := TJRPCParamList.Create(True);
end;

destructor TJRPCParams.Destroy;
begin
  FByName.Free;
  FByPos.Free;
  inherited;
end;

function TJRPCParams.GetCount: Integer;
begin
  Result := 0;

  if (FByPos.Count = 0) and (FByName.Count = 0)  then
    Exit(0);

  if (FByPos.Count = 0)  then
    Exit(FByName.Count);

  if (FByName.Count = 0)  then
    Exit(FByPos.Count);
end;

function TJRPCParams.ParamsType: TJRPCParamsType;
begin
  Result := TJRPCParamsType.Null;

  if (FByPos.Count = 0) and (FByName.Count = 0)  then
    Exit(TJRPCParamsType.Null);

  if (FByPos.Count = 0)  then
    Exit(TJRPCParamsType.ByName);

  if (FByName.Count = 0)  then
    Exit(TJRPCParamsType.ByPos);
end;

procedure TJRPCParams.AddParam(const AValue: TJSONValue);
begin
  if ByName.Count > 0 then
    raise Exception.Create('Only named params are allowed');

  ByPos.Add(AValue);
end;

{ TJRPCID }

class operator TJRPCID.Implicit(ASource: Integer): TJRPCID;
begin
  Result.id := ASource;
end;

class operator TJRPCID.Implicit(ASource: string): TJRPCID;
begin
  Result.id := ASource;
end;

class operator TJRPCID.Implicit(const ASource: TJRPCID): string;
begin
  if ASource.Id.IsType<Integer> then
    raise Exception.Create('The Id is an integer');

  Result := ASource.Id.AsString;
end;

class operator TJRPCID.Implicit(const ASource: TJRPCID): Integer;
begin
  if ASource.Id.IsType<string> then
    raise Exception.Create('The Id is a string');

  Result := ASource.Id.AsInteger;
end;

{ TJResponseSerializer }

class function TJResponseSerializer.CanHandle(AType: PTypeInfo): Boolean;
begin
  Result := TypeInfoIs(AType);
end;

class function TJResponseSerializer.GetTargetInfo: PTypeInfo;
begin
  Result := TJRPCResponse.ClassInfo;
end;

function TJResponseSerializer.Deserialize(AValue: TJSONValue;
  const AData: TValue; ANeonObject: TNeonRttiObject;
  AContext: IDeserializerContext): TValue;
var
  LRes: TJRPCResponse;
begin
  LRes := AData.AsType<TJRPCResponse>;

  var j := AValue.FindValue('result');
  if Assigned(j) then
    LRes.Result := j;

  var id := AValue.FindValue('id');
  if id is TJSONNumber then
    LRes.Id := id.GetValue<Integer>
  else
    LRes.Id := id.GetValue<string>;

  LRes.JsonRpc := AValue.FindValue('jsonrpc').Value;

  var err := AValue.FindValue('error');
  if Assigned(err) then
  begin
    var typ := TRttiUtils.Context.GetType(LRes.Error.ClassType);
    AContext.ReadDataMember(err, typ, LRes.Error, True);
  end;

  Result := TValue.From<TJRPCResponse>(LRes);
end;

function TJResponseSerializer.Serialize(const AValue: TValue;
  ANeonObject: TNeonRttiObject; AContext: ISerializerContext): TJSONValue;
var
  LRes: TJRPCResponse;
begin
  LRes := AValue.AsObject as TJRPCResponse;

  if LRes.IsError then
    AContext.GetConfiguration.Rules.ForClass<TJRPCResponse>
      .AddIgnoreMembers(['Result'])
  else
    AContext.GetConfiguration.Rules.ForClass<TJRPCResponse>
      .AddIgnoreMembers(['Error']);

  Result := AContext.WriteDataMember(AValue, False);
end;


{ TJRPCResponse }

constructor TJRPCResponse.Create;
begin
  inherited;

  FError := TJRPCError.Create;
end;

destructor TJRPCResponse.Destroy;
begin
  FError.Free;

  inherited;
end;

{ TJRPCResponse }

function TJRPCResponse.IsError: Boolean;
begin
  Result := True;

  if FError.Code.IsNull or FError.Message.IsNull then
    Result := False;
end;

procedure TJRPCResponse.SetResult(AValue: TValue);
begin

end;

{ TJRPCParamList }

function TJRPCParamList.ToJson: TJSONArray;
var
  LValue: TJSONValue;
begin
  Result := TJSONArray.Create;
  try
    for LValue in Self do
    begin
      Result.AddElement(LValue.Clone as TJSONValue);
    end;
  except
    Result.Free;
    raise;
  end;
end;

{ TJRPCParamDictionary }

function TJRPCParamDictionary.ToJson: TJSONObject;
var
  LValue: TPair<string, TJSONValue>;
begin
  Result := TJSONObject.Create;
  try
    for LValue in Self do
    begin
      Result.AddPair(LValue.Key, LValue.Value.Clone as TJSONValue);
    end;
  except
    Result.Free;
    raise;
  end;
end;

{ TJValueSerializer }

class function TJValueSerializer.CanHandle(AType: PTypeInfo): Boolean;
begin
  Result := AType = GetTargetInfo;
end;

function TJValueSerializer.Deserialize(AValue: TJSONValue; const AData: TValue;
  ANeonObject: TNeonRttiObject; AContext: IDeserializerContext): TValue;
var
  LType: TRttiType;
  LValue: TValue;
begin
  LValue := TValue.Empty;

  if AValue is TJSONNumber then
  begin
    if (AValue as TJSONNumber).Value.Contains('.') then
      LType := TRttiUtils.Context.GetType(TypeInfo(Integer))
    else
      LType := TRttiUtils.Context.GetType(TypeInfo(Double));

    LValue := AContext.ReadDataMember(AValue, LType, AData, False);
  end
  else if AValue is TJSONString then
  begin
    LType := TRttiUtils.Context.GetType(TypeInfo(string));
    LValue := AContext.ReadDataMember(AValue, LType, AData, False);
  end
  else if TJSONUtils.IsBool(AValue) then
  begin
    LType := TRttiUtils.Context.GetType(TypeInfo(Boolean));
    LValue := AContext.ReadDataMember(AValue, LType, AData, False);
  end;

  Result := LValue;
end;

class function TJValueSerializer.GetTargetInfo: PTypeInfo;
begin
  Result := TypeInfo(TValue);
end;

function TJValueSerializer.Serialize(const AValue: TValue;
  ANeonObject: TNeonRttiObject; AContext: ISerializerContext): TJSONValue;
begin
  Result := nil;

  if AValue.TypeInfo = TypeInfo(TValue) then
  begin
    var vv := AValue.AsType<TValue>;
    if not Assigned(TRttiUtils.Context.GetType(vv.TypeInfo)) then
      Exit(nil);

    Result := AContext.WriteDataMember(AValue.AsType<TValue>, False)
  end;

  {
  if AValue.Kind = tkRecord then
    Result := AContext.WriteDataMember(AValue.AsType<TValue>, False)
  else
    Result := nil;
  }
end;

{ TJRequestSerializer }

class function TJRequestSerializer.CanHandle(AType: PTypeInfo): Boolean;
begin
  Result := TypeInfoIs(AType);
end;

function TJRequestSerializer.Deserialize(AValue: TJSONValue;
  const AData: TValue; ANeonObject: TNeonRttiObject;
  AContext: IDeserializerContext): TValue;
var
  LId: TJSONValue;
  LReq: TJRPCRequest;
  LParams: TJSONValue;
begin
  LReq := AData.AsType<TJRPCRequest>;

  LReq.Method := AValue.GetValue<string>('method');
  LReq.JsonRpc := AValue.GetValue<string>('jsonrpc');
  LId := AValue.GetValue<TJSONValue>('id');

  if LId is TJSONString then
    LReq.Id := LId.Value
  else
    LReq.Id := LId.AsType<Integer>;

  LParams := AValue.GetValue<TJSONValue>('params');

  // Position parameters
  if LParams is TJSONArray then
  begin
    LReq.Params.Free;
    LReq.Params := LParams.Clone as TJSONArray;
  end;

  // Named parameters
  if LParams is TJSONObject then
  begin
    LReq.Params.Free;
    LReq.Params := LParams.Clone as TJSONObject;
  end;

  Result := TValue.From<TJRPCRequest>(LReq);
end;

class function TJRequestSerializer.GetTargetInfo: PTypeInfo;
begin
  Result := TJRPCRequest.ClassInfo;
end;

function TJRequestSerializer.Serialize(const AValue: TValue;
  ANeonObject: TNeonRttiObject; AContext: ISerializerContext): TJSONValue;
begin
  Result := AContext.WriteDataMember(AValue, False);
end;

end.
