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


  TJRPCParamsType = (ByPos, ByName, Null);
  TJRPCParams = class
  private
    FByName: TDictionary<string, TValue>;
    FByPos: TArray<TValue>;
    function GetCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddParam(const AValue: TValue); overload;
    procedure AddParam(const AName: string; const AValue: TValue); overload;
    function ParamsType: TJRPCParamsType;

    property ByPos: TArray<TValue> read FByPos write FByPos;
    property ByName: TDictionary<string, TValue> read FByName write FByName;
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

    procedure AddPositionParam(const AValue: TValue);
    procedure AddNamedParam(const AName: string; const AValue: TValue);

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

procedure TJRPCRequest.AddPositionParam(const AValue: TValue);
begin
  FParams.AddParam(AValue);
end;

procedure TJRPCRequest.AddNamedParam(const AName: string; const AValue: TValue);
begin
  FParams.AddParam(AName, AValue);
end;

{ TJRPCParams }

procedure TJRPCParams.AddParam(const AName: string; const AValue: TValue);
begin
  if Length(ByPos) > 0 then
    raise Exception.Create('Only position params are allowed');

  ByName.Add(AName, AValue);
end;

constructor TJRPCParams.Create;
begin
  FByName := TDictionary<string, TValue>.Create;
end;

destructor TJRPCParams.Destroy;
begin
  FByName.Free;

  inherited;
end;

function TJRPCParams.GetCount: Integer;
begin
  Result := 0;

  if (Length(FByPos) = 0) and (FByName.Count = 0)  then
    Exit(0);

  if (Length(FByPos) = 0)  then
    Exit(FByName.Count);

  if (FByName.Count = 0)  then
    Exit(Length(FByPos));
end;

function TJRPCParams.ParamsType: TJRPCParamsType;
begin
  Result := TJRPCParamsType.Null;

  if (Length(FByPos) = 0) and (FByName.Count = 0)  then
    Exit(TJRPCParamsType.Null);

  if (Length(FByPos) = 0)  then
    Exit(TJRPCParamsType.ByName);

  if (FByName.Count = 0)  then
    Exit(TJRPCParamsType.ByPos);
end;

procedure TJRPCParams.AddParam(const AValue: TValue);
begin
  if ByName.Count > 0 then
    raise Exception.Create('Only named params are allowed');

  ByPos := ByPos + [AValue];
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
    ByPos:  Result := AContext.WriteDataMember(TValue.From<TArray<TValue>>(LParams.ByPos));
    ByName: Result := AContext.WriteDataMember(LParams.ByName);
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
      LParams.AddParam(LItem);
  end;

  // Is a dictionary
  if AValue is TJSONObject then
  begin
    LObject := AValue as TJSONObject;
    for LPair in LObject do
      LParams.AddParam(LPair.JsonString.Value, LPair.JsonValue);
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

end.
