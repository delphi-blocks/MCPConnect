unit JRPC.Classes;

interface

uses
  System.SysUtils, System.Rtti, System.Classes, System.Generics.Collections,
  System.TypInfo, System.JSON,

  Neon.Core.Utils,
  Neon.Core.Nullables,
  Neon.Core.Attributes,
  Neon.Core.Persistence;

type
  TJRPCID = record
  private
    [NeonInclude]
    id: TValue;
  public
    class operator Implicit(ASource: Integer): TJRPCID;
    class operator Implicit(ASource: string): TJRPCID;
  end;

  TJRPCEnvelope = class
  public const
	  JSONRPC_VERSION = '2.0';
  private
    FId: TJRPCID;
    FJsonRpc: string;
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
  private
    FMethod: string;
    FParams: TJRPCParams;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddPositionParam(const AValue: TJSONValue); overload;
    procedure AddPositionParam(AValue: Integer); overload;
    procedure AddPositionParam(AValue: Double); overload;
    procedure AddPositionParam(const AValue: string); overload;

    procedure AddNamedParam(const AName: string; const AValue: TJSONValue); overload;
    procedure AddNamedParam(const AName: string; AValue: Integer); overload;
    procedure AddNamedParam(const AName: string; AValue: Double); overload;
    procedure AddNamedParam(const AName, AValue: string); overload;

    [NeonProperty('method')]
    property Method: string read FMethod write FMethod;

    [NeonProperty('params')]
    property Params: TJRPCParams read FParams write FParams;
  end;

  TJRPCResponse = class(TJRPCEnvelope)
  private
    FError: TJRPCError;
    FResult: TValue;
  public
    constructor Create;
    destructor Destroy; override;

    function IsError: Boolean;

    [NeonProperty('error')]
    property Error: TJRPCError read FError write FError;

    [NeonProperty('result')]
    property Result: TValue read FResult write FResult;
  end;


  /// <summary>
  ///   Custom serializer for the TJParams class.
  /// </summary>
  TJParamsSerializer = class(TCustomSerializer)
  protected
    class function GetTargetInfo: PTypeInfo; override;
    class function CanHandle(AType: PTypeInfo): Boolean; override;
  public
    function Serialize(const AValue: TValue; ANeonObject: TNeonRttiObject; AContext: ISerializerContext): TJSONValue; override;
    function Deserialize(AValue: TJSONValue; const AData: TValue; ANeonObject: TNeonRttiObject; AContext: IDeserializerContext): TValue; override;
  end;

  /// <summary>
  ///   Custom serializer for the TJResponse class.
  /// </summary>
  TJResponseSerializer = class(TCustomSerializer)
  protected
    class function GetTargetInfo: PTypeInfo; override;
    class function CanHandle(AType: PTypeInfo): Boolean; override;
  public
    function Serialize(const AValue: TValue; ANeonObject: TNeonRttiObject; AContext: ISerializerContext): TJSONValue; override;
    function Deserialize(AValue: TJSONValue; const AData: TValue; ANeonObject: TNeonRttiObject; AContext: IDeserializerContext): TValue; override;
  end;

implementation

{ TJRPCEnvelope }

constructor TJRPCEnvelope.Create;
begin
  FJsonRpc := JSONRPC_VERSION;
end;

{ TJRPCRequest }

procedure TJRPCRequest.AddNamedParam(const AName: string; AValue: Integer);
begin
  AddNamedParam(AName, TJSONNumber.Create(AValue));
end;

procedure TJRPCRequest.AddPositionParam(AValue: Integer);
begin
  AddPositionParam(TJSONNumber.Create(AValue));
end;

constructor TJRPCRequest.Create;
begin
  inherited;
  FParams := TJRPCParams.Create;
end;

destructor TJRPCRequest.Destroy;
begin
  FParams.Free;

  inherited;
end;

procedure TJRPCRequest.AddPositionParam(const AValue: TJSONValue);
begin
  FParams.AddParam(AValue);
end;

procedure TJRPCRequest.AddNamedParam(const AName: string; const AValue: TJSONValue);
begin
  FParams.AddParam(AName, AValue);
end;

procedure TJRPCRequest.AddNamedParam(const AName, AValue: string);
begin
  AddNamedParam(AName, TJSONString.Create(AValue));
end;

procedure TJRPCRequest.AddNamedParam(const AName: string; AValue: Double);
begin
  AddNamedParam(AName, TJSONNumber.Create(AValue));
end;

procedure TJRPCRequest.AddPositionParam(AValue: Double);
begin
  AddPositionParam(TJSONNumber.Create(AValue));
end;

procedure TJRPCRequest.AddPositionParam(const AValue: string);
begin
  AddPositionParam(TJSONString.Create(AValue));
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

{ TJParamsSerializer }

class function TJParamsSerializer.CanHandle(AType: PTypeInfo): Boolean;
begin
  Result := TypeInfoIs(AType);
end;

class function TJParamsSerializer.GetTargetInfo: PTypeInfo;
begin
  Result := TJRPCParams.ClassInfo;
end;

function TJParamsSerializer.Serialize(const AValue: TValue;
  ANeonObject: TNeonRttiObject; AContext: ISerializerContext): TJSONValue;
var
  LParams: TJRPCParams;
begin
  Result := nil;
  LParams := AValue.AsObject as TJRPCParams;

  case LParams.ParamsType of
    ByPos:  Result := LParams.ByPos.ToJson;
    ByName: Result := LParams.ByName.ToJson;
    Null:   Result := TJSONObject.Create;
  end;

end;

function TJParamsSerializer.Deserialize(AValue: TJSONValue; const AData: TValue;
  ANeonObject: TNeonRttiObject; AContext: IDeserializerContext): TValue;
var
  LParams: TJRPCParams;
  LArray: TJSONArray;
  LItem: TJSONValue;

  LObject: TJSONObject;
  LPair: TJSONPair;
begin
  LParams := AData.AsType<TJRPCParams>;

  if AValue is TJSONArray then
  begin
    LArray := AValue as TJSONArray;
    for LItem in LArray do
      LParams.AddParam(LItem.Clone as TJSONValue);
  end;

  // Is a dictionary
  if AValue is TJSONObject then
  begin
    LObject := AValue as TJSONObject;
    for LPair in LObject do
      LParams.AddParam(LPair.JsonString.Value, LPair.JsonValue.Clone as TJSONValue);
  end;

  Result := TValue.From<TJRPCParams>(LParams);
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

end.
