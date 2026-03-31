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
unit MCPConnect.JRPC.Core;

interface

{$SCOPEDENUMS ON}

uses
  System.SysUtils, System.Rtti, System.Classes, System.Generics.Collections,
  System.TypInfo, System.JSON,

  Neon.Core.Utils,
  Neon.Core.Tags,
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
  /// <summary>
  ///   Base class for all JSON-RPC related exceptions
  /// </summary>
  EJRPCException = class(Exception)
  protected
    FCode: Integer;
    FData: TValue;
  public
    procedure AfterConstruction; override;

    function ToJSON: string;

    /// <summary>
    ///   The JSON-RPC error code.
    /// </summary>
    property Code: Integer read FCode;

    /// <summary>
    ///   Optional data associated with the error.
    /// </summary>
    property Data: TValue read FData;
  end;

  /// <summary>
  ///   Exception raised when invalid JSON was received by the server.
  /// </summary>
  EJRPCParseError = class(EJRPCException)
  public
    procedure AfterConstruction; override;
  end;

  /// <summary>
  /// Exception raised when the JSON sent is not a valid Request object.
  /// </summary>
  EJRPCInvalidRequestError = class(EJRPCException)
  public
    procedure AfterConstruction; override;
  end;

  /// <summary>
  /// Exception raised when the method does not exist / is not available.
  /// </summary>
  EJRPCMethodNotFoundError = class(EJRPCException)
  public
    procedure AfterConstruction; override;
  end;

  /// <summary>
  /// Exception raised when the method parameter(s) are invalid.
  /// </summary>
  EJRPCInvalidParamsError = class(EJRPCException)
  public
    procedure AfterConstruction; override;
  end;

  TJRPCContext = class;
  TJRPCError = class;

  /// <summary>
  ///   Type of JSON-RPC parameters: by position, by name, or null
  /// </summary>
  TJRPCParamsType = (ByPos, ByName, Null);

  /// <summary>
  ///   Attribute used to mark a method as a JSON-RPC notification
  /// </summary>
  JRPCNotificationAttribute = class(TCustomAttribute);

  /// <summary>
  ///   Attribute used to mark a field for context injection
  /// </summary>
  ContextAttribute = class(TCustomAttribute);

  /// <summary>
  ///   Base attribute for JSON-RPC related metadata
  /// </summary>
  JRPCAttribute = class(TCustomAttribute)
  private
    FName: string;
    FAdditionalTags: string;
    FAdditional: string;
    FTags: TAttributeTags;
    function GetTags: TAttributeTags;
  public
    property Name: string read FName;
    property AdditionalTags: string read FAdditional write FAdditional;

    /// <summary>
    ///   Parsed tags from AdditionalTags
    /// </summary>
    property Tags: TAttributeTags read GetTags write FTags;

    constructor Create(const AName: string; const AAdditionalTags: string = '');
    destructor Destroy; override;
  end;

  /// <summary>
  ///   Attribute used to specify a JSON-RPC path prefix.
  /// </summary>
  JRPCPathAttribute = class(JRPCAttribute);

  /// <summary>
  ///   Attribute used to specify a JSON-RPC method name.
  /// </summary>
  JRPCMethodAttribute = class(JRPCAttribute);

  /// <summary>
  ///   Attribute used to specify a JSON-RPC parameter name.
  /// </summary>
  JRPCParamAttribute = class(JRPCAttribute);

  /// <summary>
  ///   Attribute used to specify multiple JSON-RPC parameters.
  /// </summary>
  JRPCParamsAttribute = class(TCustomAttribute);

  /// <summary>
  ///   Represents a JSON-RPC ID, which can be either an integer or a string.
  /// </summary>
  TJRPCID = record
  private
    [NeonInclude] id: TValue;
  public
    class operator Implicit(ASource: Integer): TJRPCID;
    class operator Implicit(ASource: string): TJRPCID;
    class operator Implicit(Value: TObject): TJRPCID;

    class operator Implicit(const ASource: TJRPCID): Integer;
    class operator Implicit(const ASource: TJRPCID): string;

    function IsNull: Boolean;
  end;

  /// <summary>
  ///   Type of JSON-RPC message: Request, Response, or Notification.
  /// </summary>
  TJRPCMessageType = (Request, Response, Error, Notification);
  TJRPCMessageTypes = set of TJRPCMessageType;

  /// <summary>
  ///   Base class for all JSON-RPC classes.
  /// </summary>
  TJRPCMessage = class
  public const
	  JSONRPC_VERSION = '2.0';
  protected
    FJsonRpc: string;
    function GetNeonConfig: INeonConfiguration;
  public
    constructor Create;

    function GetType: TJRPCMessageType; virtual; abstract;

    /// <summary>
    ///   Serializes the message to a JSON string.
    /// </summary>
    function ToJson: string; virtual;

    /// <summary>
    ///   Deserializes the message from a JSON string.
    /// </summary>
    procedure FromJson(const AJSON: string); virtual;

    /// <summary>
    ///   The JSON-RPC version (defaults to "2.0")
    /// </summary>
    [NeonProperty('jsonrpc')]
    property JsonRpc: string read FJsonRpc write FJsonRpc;
  end;

  TJRPCMessageClass = class of TJRPCMessage;

  /// <summary>
  ///   Manages a collection of JSON-RPC messages.
  /// </summary>
  TJRPCMessages = class
  private
    FList: TObjectList<TJRPCMessage>;
    FTypes: TJRPCMessageTypes;
    FSingle: Boolean;

    procedure FromJsonSingle(const AJSON: TJSONObject);
    function GetMessageType(AMessage: TJSONObject): TJRPCMessageType;
    function GetMessageTypes(ARequestJSON: TJSONValue): TJRPCMessageTypes;
    function GetCount: NativeInt;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddMessage(AMsg: TJRPCMessage);

    function ToJson: string;
    procedure FromJson(const AJSON: string); overload;
    procedure FromJson(AStream: TStream); overload;
    procedure FromJson(AValue: TJSONValue); overload;

    property Single: Boolean read FSingle write FSingle;
    property List: TObjectList<TJRPCMessage> read FList;
    property Types: TJRPCMessageTypes read FTypes;
    property Count: NativeInt read GetCount;
  public
    class function CreateFromJson(const AJSON: string): TJRPCMessages;
  end;

  /// <summary>
  /// Represents a JSON-RPC error object.
  /// </summary>
  TJRPCErrorDetails = class
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

  /// <summary>
  ///   Abstract base class for JSON-RPC methods (Requests and Notifications).
  /// </summary>
  TJRPCMethod = class abstract(TJRPCMessage)
  protected
    FMethod: string;
    FParams: TJSONValue;

    function GetPositionParams: TJSONArray;
    function GetNamedParams: TJSONObject;
    procedure SetParams(AValue: TJSONValue);
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    ///   Returns the type of parameters (ByPos, ByName, or Null).
    /// </summary>
    function ParamsType: TJRPCParamsType;

    /// <summary>
    ///   Returns the number of parameters.
    /// </summary>
    function ParamsCount: Integer;

    /// <summary>
    ///   Adds a parameter by position.
    /// </summary>
    procedure AddPositionParam(const AValue: TValue);

    /// <summary>
    ///   Adds a parameter by name.
    /// </summary>
    procedure AddNamedParam(const AName: string; const AValue: TValue);

    /// <summary>
    ///   The name of the method to be invoked.
    /// </summary>
    [NeonProperty('method')]
    property Method: string read FMethod write FMethod;

    /// <summary>
    ///   The parameters for the method call.
    /// </summary>
    [NeonProperty('params')]
    property Params: TJSONValue read FParams write SetParams;

  public
    /// <summary>
    ///   Creates a TJRPCMethod instance from a JSON string.
    /// </summary>
    class function CreateFromJson(const AJSON: string): TJRPCMethod;
  end;

  /// <summary>
  ///   Class representing a JSON-RPC notification.
  ///   https://json-rpc.dev/learn/examples/notifications
  /// </summary>
  TJRPCNotification = class(TJRPCMethod)
  public
    function GetType: TJRPCMessageType; override;
  end;

  /// <summary>
  /// Class representing a JSON-RPC request.
  /// </summary>
  TJRPCRequest = class(TJRPCMethod)
  private
    FId: TJRPCID;
  public
    function GetType: TJRPCMessageType; override;

    /// <summary>
    ///   The unique ID of the request.
    /// </summary>
    [NeonProperty('id'), NeonUnwrapped]
    property Id: TJRPCID read FId write FId;
  public
    /// <summary>
    ///   Creates a TJRPCRequest instance from a JSON string.
    /// </summary>
    class function CreateFromJson(const AJSON: string): TJRPCRequest;
  end;


  /// <summary>
  ///   Class representing a JSON-RPC response.
  /// </summary>
  TJRPCResponse = class(TJRPCMessage)
  private
    FId: TJRPCID;
    FResult: TJSONValue;
    procedure SetResult(AValue: TJSONValue);
  public
    constructor Create;
    destructor Destroy; override;

    function GetType: TJRPCMessageType; override;

    /// <summary>
    ///   The result of the method invocation.
    /// </summary>
    [NeonProperty('result'), NeonInclude(IncludeIf.Always)]
    property Result: TJSONValue read FResult write SetResult;

    /// <summary>
    ///   The ID of the corresponding request.
    /// </summary>
    [NeonProperty('id'), NeonUnwrapped]
    property Id: TJRPCID read FId write FId;
  public
    /// <summary>
    ///   Creates a TJRPCResponse instance from a JSON string.
    /// </summary>
    class function CreateFromJson(const AJSON: string): TJRPCResponse;
  end;

  /// <summary>
  ///   Class representing a JSON-RPC error.
  /// </summary>
  TJRPCError = class(TJRPCMessage)
  private
    FId: TJRPCID;
    FError: TJRPCErrorDetails;
    FRequest: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    function GetType: TJRPCMessageType; override;
    function Clone: TJRPCError;

    /// <summary>
    ///   The error object, if any.
    /// </summary>
    [NeonProperty('error')]
    property Error: TJRPCErrorDetails read FError write FError;

    /// <summary>
    ///   The ID of the corresponding request.
    /// </summary>
    [NeonProperty('id'), NeonUnwrapped]
    property Id: TJRPCID read FId write FId;

    /// <summary>
    ///   Specifies if the exception is from a JRPC request
    /// </summary>
    [NeonIgnore]
    property Request: Boolean read FRequest write FRequest;
  public
    /// <summary>
    ///   Creates a TJRPCError instance
    /// </summary>
    class function CreateFromJson(const AJSON: string): TJRPCError;
    class function CreateFromException(E: Exception; AId: TJRPCID): TJRPCError; overload;
    class function CreateFromException(E: Exception; AJSON: TJSONObject): TJRPCError; overload;
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
  ///   Custom serializer for the TJRPCError class.
  /// </summary>
  TJErrorSerializer = class(TCustomSerializer)
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

  /// <summary>
  /// Proxy class for creating instances of a specific class using a custom constructor function.
  /// </summary>
  TJRPCConstructorProxy = class
  private
    FConstructorFunc: TFunc<TObject>;
    FTypeTClass: TClass;
    FNeonConfig: INeonConfiguration;
  public
    constructor Create(AClass: TClass; const AConstructorFunc: TFunc<TObject>; ANeonConfig: INeonConfiguration);

    property TypeTClass: TClass read FTypeTClass;
    property NeonConfig: INeonConfiguration read FNeonConfig;
    property ConstructorFunc: TFunc<TObject> read FConstructorFunc write FConstructorFunc;
    function Clone: TJRPCConstructorProxy;
  end;

  /// <summary>
  ///   Registry for JSON-RPC classes and their constructor proxies.
  /// </summary>
  TJRPCRegistry = class(TObjectDictionary<string, TJRPCConstructorProxy>)
  private
    class var FInstance: TJRPCRegistry;
  protected
    class function GetInstance: TJRPCRegistry; static; inline;
  private
    FSeparator: string;
    function GetClassName(AClass: TClass): string;
    procedure AddClass(AClass: TClass; AProxy: TJRPCConstructorProxy);
    procedure RemoveClass(AClass: TClass);
  public
    constructor Create; virtual;

    /// <summary>
    ///   Registers a class with its default constructor.
    /// </summary>
    function RegisterClass(AClass: TClass): TJRPCConstructorProxy; overload;

    /// <summary>
    ///   Registers a class with a custom Neon configuration.
    /// </summary>
    function RegisterClass(AClass: TClass; ANeonConfig: INeonConfiguration): TJRPCConstructorProxy; overload;

    /// <summary>
    ///   Registers a class of type T.
    /// </summary>
    function RegisterClass<T: class>: TJRPCConstructorProxy; overload;

    /// <summary>
    ///   Registers a class of type T with a custom Neon configuration.
    /// </summary>
    function RegisterClass<T: class>(ANeonConfig: INeonConfiguration): TJRPCConstructorProxy; overload;

    /// <summary>
    ///   Registers a class of type T with a custom constructor function.
    /// </summary>
    function RegisterClass<T: class>(const AConstructorFunc: TFunc<TObject>): TJRPCConstructorProxy; overload;

    /// <summary>
    ///   Registers a class of type T with a custom constructor function and
    ///   Neon configuration.
    /// </summary>
    function RegisterClass<T: class>(const AConstructorFunc: TFunc<TObject>; ANeonConfig: INeonConfiguration): TJRPCConstructorProxy; overload;

    function ClassExists(AClass: TClass): Boolean; overload;
    function ClassExists<T: class>: Boolean; overload;

    /// <summary>
    ///   Unregisters a class.
    /// </summary>
    procedure UnregisterClass(AClass: TClass);

    /// <summary>
    ///   Creates an instance of the class registered with the specified name.
    /// </summary>
    function GetClassInstance(const AName: string; out Value: TObject): Boolean;

    /// <summary>
    ///   Retrieves the constructor proxy for the specified class name.
    /// </summary>
    function GetConstructorProxy(const AName: string; out Value: TJRPCConstructorProxy): Boolean;

    /// <summary>
    ///   The character used to separate class and method names (default is "/").
    /// </summary>
    property Separator: string read FSeparator write FSeparator;

    /// <summary>
    ///   Singleton instance of the registry.
    /// </summary>
    class property Instance: TJRPCRegistry read GetInstance;

    class constructor Create;
    class destructor Destroy;
  end;

  TJRPCContextRegistry = class(TDictionary<TClass, TObject>);

  /// <summary>
  ///   Represents the context of a JSON-RPC CurrentRequest, including the CurrentRequest
  ///   itself, the Responses, and any additional data associated with the context.
  /// </summary>
  TJRPCContext = class(TObject)
  private
    FCurrentRequest: TJRPCRequest;
    FResponses: TJRPCMessages;
    FContextData: TJRPCContextRegistry;
  protected
    function GetCurrentRequest: TJRPCRequest;
    function GetResponses: TJRPCMessages;
    procedure AddConfigurations(AObject: TObject);
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    ///   Adds an object to the context data.
    /// </summary>
    procedure AddContent(AObject: TObject); overload;

    /// <summary>
    ///   Adds an interface implementation to the context data.
    /// </summary>
    procedure AddContent(AInterface: IInterface); overload;

    /// <summary>
    ///   Retrieves context data as a specific class type (raises exception if not found)
    /// </summary>
    function GetContextDataAs<T: class>: T; overload;

    /// <summary>
    ///   Retrieves context data as a specific class type (raises exception if not found)
    /// </summary>
    function GetContextDataAs(AClass: TClass): TObject; overload;

    /// <summary>
    ///   Retrieves context data as a specific interface (raises exception if not found)
    /// </summary>
    function GetContextDataAs(AInterface: TGUID): IInterface; overload;

    /// <summary>
    ///   Finds context data as a specific class type (returns nil if not found)
    /// </summary>
    function FindContextDataAs<T: class>: T; overload;

    /// <summary>
    ///   Finds context data as a specific class type (returns nil if not found)
    /// </summary>
    function FindContextDataAs(AClass: TClass): TObject; overload;

    /// <summary>
    ///   Finds context data as a specific interface (returns nil if not found)
    /// </summary>
    function FindContextDataAs(AInterface: TGUID): IInterface; overload;

    /// <summary>
    ///   Injects context data into the fields of the specified object.
    /// </summary>
    procedure Inject(AObject: TObject); overload;

    /// <summary>
    ///   Injects context data into the fields of the specified interface implementation.
    /// </summary>
    procedure Inject(AInterface: IInterface); overload;

    /// <summary>
    ///   The JSON-RPC CurrentRequest associated with this context.
    /// </summary>
    property CurrentRequest: TJRPCRequest read GetCurrentRequest;

    /// <summary>
    ///   The JSON-RPC Responses associated with this context.
    /// </summary>
    property Responses: TJRPCMessages read GetResponses;
  end;

/// <summary>
///   Returns the default Neon configuration for JSON-RPC serialization.
/// </summary>
function JRPCNeonConfig: INeonConfiguration;

implementation

uses
  MCPConnect.Configuration.Core;

function JRPCNeonConfig: INeonConfiguration;
begin
  Result := TNeonConfiguration.Camel
    .SetPrettyPrint(True)
    .RegisterSerializer(TJSONValueSerializer)
    .RegisterSerializer(TJValueSerializer)
    .RegisterSerializer(TJRequestSerializer)
    .RegisterSerializer(TJErrorSerializer)
  ;
end;


{ TJRPCMessage }

constructor TJRPCMessage.Create;
begin
  FJsonRpc := JSONRPC_VERSION;
end;

procedure TJRPCMessage.FromJson(const AJSON: string);
begin
  TNeon.JSONToObject(Self, AJSON, JRPCNeonConfig);
end;

function TJRPCMessage.GetNeonConfig: INeonConfiguration;
begin
  Result := TNeonConfiguration.Default
    .RegisterSerializer(TJSONValueSerializer)
    .RegisterSerializer(TJValueSerializer)
    .RegisterSerializer(TJValueSerializer);
end;

function TJRPCMessage.ToJson: string;
begin
  Result := TNeon.ObjectToJSONString(Self, JRPCNeonConfig);
end;

constructor TJRPCMethod.Create;
begin
  inherited;
  FParams := TJSONNull.Create;
end;

class function TJRPCMethod.CreateFromJson(const AJSON: string): TJRPCMethod;
begin
  Result := Self.Create;
  try
    Result.FromJson(AJSON);
  except
    Result.Free;
    raise;
  end;
end;

destructor TJRPCMethod.Destroy;
begin
  FParams.Free;
  inherited;
end;

function TJRPCMethod.GetNamedParams: TJSONObject;
begin
  if FParams is TJSONArray then
    raise EJRPCException.Create('Only named params are allowed');

  if FParams is TJSONObject then
    Exit(FParams as TJSONObject);

  if FParams is TJSONNull then
  begin
    FParams.Free;
    Result := TJSONObject.Create;
    FParams := Result;
    Exit(FParams as TJSONObject);
  end;
  // This should never happen
  raise EJRPCException.Create('Not a valid type for named allowed');
end;

function TJRPCMethod.GetPositionParams: TJSONArray;
begin
  if FParams is TJSONObject then
    raise EJRPCException.Create('Only position params are allowed');

  if FParams is TJSONArray then
    Exit(FParams as TJSONArray);

  if FParams is TJSONNull then
  begin
    FParams.Free;
    Result := TJSONArray.Create;
    FParams := Result;
    Exit(FParams as TJSONArray);
  end;
  // This should never happen
  raise EJRPCException.Create('Not a valid type for position allowed');
end;

function TJRPCMethod.ParamsCount: Integer;
begin
  Result := 0;

  if FParams is TJSONNull then
    Exit(0);

  if FParams is TJSONArray then
    Exit((FParams as TJSONArray).Count);

  if FParams is TJSONObject then
    Exit((FParams as TJSONObject).Count);
end;

function TJRPCMethod.ParamsType: TJRPCParamsType;
begin
  Result := TJRPCParamsType.Null;

  if FParams is TJSONNull then
    Exit(TJRPCParamsType.Null);

  if FParams is TJSONArray then
    Exit(TJRPCParamsType.ByPos);

  if FParams is TJSONObject then
    Exit(TJRPCParamsType.ByName);
end;

procedure TJRPCMethod.AddNamedParam(const AName: string; const AValue: TValue);
var
  LParam: TJSONValue;
begin
  LParam := TNeon.ValueToJSON(AValue, GetNeonConfig);
  if Assigned(LParam) then
    GetNamedParams.AddPair(AName, LParam);
end;

procedure TJRPCMethod.AddPositionParam(const AValue: TValue);
var
  LParam: TJSONValue;
begin
  LParam := TNeon.ValueToJSON(AValue, JRPCNeonConfig);
  if Assigned(LParam) then
    GetPositionParams.AddElement(LParam);
end;

procedure TJRPCMethod.SetParams(AValue: TJSONValue);
begin
  if Assigned(FParams) and (FParams <> AValue) then
  begin
    FParams.Free;
    FParams := AValue;
  end;
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

class operator TJRPCID.Implicit(Value: TObject): TJRPCID;
begin
  Result.id := nil;
end;

class operator TJRPCID.Implicit(const ASource: TJRPCID): string;
begin
  if ASource.Id.IsType<Integer> then
    raise EJRPCParseError.Create('The Id is an integer');

  Result := ASource.Id.AsString;
end;

function TJRPCID.IsNull: Boolean;
begin
  Result := id.IsEmpty;
end;

class operator TJRPCID.Implicit(const ASource: TJRPCID): Integer;
begin
  if ASource.Id.IsType<string> then
    raise EJRPCParseError.Create('The Id is a string');

  Result := ASource.Id.AsInteger;
end;


{ TJRPCResponse }

constructor TJRPCResponse.Create;
begin
  inherited;
  FResult := TJSONNull.Create;
end;

destructor TJRPCResponse.Destroy;
begin
  FResult.Free;
  inherited;
end;

class function TJRPCResponse.CreateFromJson(const AJSON: string): TJRPCResponse;
begin
  Result := Self.Create;
  try
    Result.FromJson(AJSON);
  except
    Result.Free;
    raise;
  end;
end;

function TJRPCResponse.GetType: TJRPCMessageType;
begin
  Result := TJRPCMessageType.Response;
end;

procedure TJRPCResponse.SetResult(AValue: TJSONValue);
begin
  if FResult <> AValue then
  begin
    if Assigned(FResult) then
      FResult.Free;

    FResult := AValue;
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
var
  LValue: TValue;
begin
  Result := nil;

  if AValue.TypeInfo = TypeInfo(TValue) then
  begin
    LValue := AValue.AsType<TValue>;
    if not Assigned(TRttiUtils.Context.GetType(LValue.TypeInfo)) then
      Exit(nil);

    Result := AContext.WriteDataMember(AValue.AsType<TValue>, False)
  end;
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
  LIdValue: TJSONValue;
  LReq: TJRPCRequest;
  LParams: TJSONValue;
begin
  LReq := AData.AsType<TJRPCRequest>;

  LReq.Method := AValue.GetValue<string>('method');
  LReq.JsonRpc := AValue.GetValue<string>('jsonrpc');
  LIdValue := AValue.GetValue<TJSONValue>('id', nil);
  if not Assigned(LIdValue) then
    LReq.Id := ''
  else if LIdValue is TJSONNumber then
    LReq.Id := LIdValue.AsType<Integer>
  else
    LReq.Id := LIdValue.Value;

  // "params" is optional
  if AValue.TryGetValue<TJSONValue>('params', LParams) then
  begin
    // Position parameters
    if LParams is TJSONArray then
      LReq.Params := LParams.Clone as TJSONArray;

    // Named parameters
    if LParams is TJSONObject then
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

{ JRPCAttribute }

constructor JRPCAttribute.Create(const AName, AAdditionalTags: string);
begin
  FName := AName;
  FAdditionalTags := AAdditionalTags;

  FTags := TAttributeTags.Create();
end;

destructor JRPCAttribute.Destroy;
begin
  FTags.Free;
  inherited;
end;

function JRPCAttribute.GetTags: TAttributeTags;
begin
  if (FTags.Count = 0) and not FAdditionalTags.IsEmpty then
    FTags.Parse(FAdditionalTags);

  Result := FTags;
end;

{ TJRPCRegistry }

procedure TJRPCRegistry.AddClass(AClass: TClass; AProxy: TJRPCConstructorProxy);
begin
  Self.Add(GetClassName(AClass), AProxy);
end;

function TJRPCRegistry.ClassExists(AClass: TClass): Boolean;
var
  LItem: TJRPCConstructorProxy;
begin
  Result := TryGetValue(GetClassName(AClass), LItem);
end;

function TJRPCRegistry.ClassExists<T>: Boolean;
begin
  Result := ClassExists(TClass(T));
end;

class constructor TJRPCRegistry.Create;
begin
  FInstance := nil;
end;

constructor TJRPCRegistry.Create;
begin
  inherited Create([doOwnsValues]);
  FSeparator := '/';
end;

function TJRPCRegistry.GetClassInstance(const AName: string;
  out Value: TObject): Boolean;
var
  LConstructorProxy: TJRPCConstructorProxy;
begin
  Result := False;
  if GetConstructorProxy(AName, LConstructorProxy) then
    Value := LConstructorProxy.ConstructorFunc();
end;

function TJRPCRegistry.GetClassName(AClass: TClass): string;
var
  LJRPCAttribute: JRPCAttribute;
begin
  LJRPCAttribute := TRttiUtils.FindAttribute<JRPCAttribute>(TRttiUtils.Context.GetType(AClass));
  if Assigned(LJRPCAttribute) then
  begin
    Result := LJRPCAttribute.Name;
    if LJRPCAttribute.GetTags.Exists('separator') then
      FSeparator := LJRPCAttribute.GetTags.GetValueAs<string>('separator');
  end
  else
    Result := AClass.QualifiedClassName;
end;

function TJRPCRegistry.GetConstructorProxy(const AName: string; out Value: TJRPCConstructorProxy): Boolean;
var
  LClassName: string;
  LSeparatorIndex: Integer;
begin
  LSeparatorIndex := Pos(FSeparator, AName);
  if LSeparatorIndex > 0 then
    LClassName := Copy(AName, 0, LSeparatorIndex - 1)
  else
    LClassName := AName;

  Value := nil;
  Result := Self.TryGetValue(LClassName, Value);
end;

class function TJRPCRegistry.GetInstance: TJRPCRegistry;
begin
  if not Assigned(FInstance) then
    FInstance := TJRPCRegistry.Create;
  Result := FInstance;
end;

function TJRPCRegistry.RegisterClass(AClass: TClass): TJRPCConstructorProxy;
begin
  Result := TJRPCConstructorProxy.Create(AClass, nil, nil);
  Self.AddClass(AClass, Result);
end;

function TJRPCRegistry.RegisterClass<T>: TJRPCConstructorProxy;
begin
  Result := RegisterClass<T>(nil, nil);
end;

function TJRPCRegistry.RegisterClass(AClass: TClass;
  ANeonConfig: INeonConfiguration): TJRPCConstructorProxy;
begin
  Result := TJRPCConstructorProxy.Create(AClass, nil, ANeonConfig);
  Self.AddClass(AClass, Result);
end;

function TJRPCRegistry.RegisterClass<T>(ANeonConfig: INeonConfiguration): TJRPCConstructorProxy;
begin
  Result := RegisterClass<T>(nil, ANeonConfig);
end;

function TJRPCRegistry.RegisterClass<T>(const AConstructorFunc: TFunc<TObject>;
  ANeonConfig: INeonConfiguration): TJRPCConstructorProxy;
begin
  Result := TJRPCConstructorProxy.Create(TClass(T), AConstructorFunc, ANeonConfig);
  AddClass(TClass(T), Result);
end;

function TJRPCRegistry.RegisterClass<T>(const AConstructorFunc: TFunc<TObject>): TJRPCConstructorProxy;
begin
  Result := TJRPCConstructorProxy.Create(TClass(T), AConstructorFunc, nil);
  AddClass(TClass(T), Result);
end;

procedure TJRPCRegistry.RemoveClass(AClass: TClass);
begin
  Self.Remove(GetClassName(AClass));
end;

procedure TJRPCRegistry.UnregisterClass(AClass: TClass);
begin
  Self.RemoveClass(AClass);
end;

class destructor TJRPCRegistry.Destroy;
begin
  FInstance.Free;
end;

{ TJRPCConstructorProxy }

function TJRPCConstructorProxy.Clone: TJRPCConstructorProxy;
begin
  Result := TJRPCConstructorProxy.Create(FTypeTClass, FConstructorFunc, FNeonConfig);
end;

constructor TJRPCConstructorProxy.Create(AClass: TClass;
  const AConstructorFunc: TFunc<TObject>; ANeonConfig: INeonConfiguration);
begin
  inherited Create;
  FConstructorFunc := AConstructorFunc;
  FTypeTClass := AClass;
  FNeonConfig := ANeonConfig;

  // Default constructor function
  if not Assigned(FConstructorFunc) then
    FConstructorFunc :=
      function: TObject
      begin
        Result := TRttiUtils.CreateInstance(FTypeTClass);
      end;
end;

{ TJRPCContext }

procedure TJRPCContext.AddConfigurations(AObject: TObject);
var
  LApplication: IJRPCApplication;
  LConfig: TJRPCConfiguration;
begin
  if Supports(AObject, IJRPCApplication, LApplication) then
  begin
    for LConfig in LApplication.GetConfigurations do
    begin
      AddContent(LConfig);
    end;
  end;
end;

procedure TJRPCContext.AddContent(AObject: TObject);
begin
  if AObject = nil then
    Exit;

  if AObject is TJRPCRequest then
    FCurrentRequest := TJRPCRequest(AObject)
  else if AObject is TJRPCMessages then
    FResponses := TJRPCMessages(AObject)
  else
  begin
    FContextData.Add(AObject.ClassType, AObject);
    AddConfigurations(AObject);
  end;
end;

procedure TJRPCContext.AddContent(AInterface: IInterface);
begin
  AddContent(AInterface as TObject);
end;

constructor TJRPCContext.Create;
begin
  inherited Create;
  FContextData := TJRPCContextRegistry.Create;
  AddContent(Self);
end;

destructor TJRPCContext.Destroy;
begin
  FContextData.Free;
  inherited;
end;

function TJRPCContext.FindContextDataAs(AClass: TClass): TObject;
begin
  if not FContextData.TryGetValue(AClass, Result) then
    Result := nil;
end;

function TJRPCContext.FindContextDataAs(AInterface: TGUID): IInterface;
var
  LObject: TObject;
begin
  Result := nil;
  for LObject in FContextData.Values do
  begin
    if Supports(LObject, AInterface, Result) then
      Exit;
  end;
end;

function TJRPCContext.FindContextDataAs<T>: T;
begin
  Result := FindContextDataAs(TClass(T)) as T;
end;

function TJRPCContext.GetContextDataAs(AClass: TClass): TObject;
begin
  Result := FindContextDataAs(AClass);
  if not Assigned(Result) then
    raise EJRPCException.CreateFmt('Context: object "%s" not found', [AClass.ClassName]);
end;

function TJRPCContext.GetContextDataAs(AInterface: TGUID): IInterface;
begin
  Result := FindContextDataAs(AInterface);
  if not Assigned(Result) then
    raise EJRPCException.CreateFmt('Context: interface "%s" not found', [AInterface.ToString]);
end;

function TJRPCContext.GetContextDataAs<T>: T;
begin
  Result := GetContextDataAs(TClass(T)) as T;
end;

function TJRPCContext.GetCurrentRequest: TJRPCRequest;
begin
  if not Assigned(FCurrentRequest) then
    raise EJRPCException.Create('CurrentRequest not found');

  Result := FCurrentRequest;
end;

function TJRPCContext.GetResponses: TJRPCMessages;
begin
  if not Assigned(FResponses) then
    raise EJRPCException.Create('Responses not found');

  Result := FResponses;
end;

procedure TJRPCContext.Inject(AInterface: IInterface);
begin
  Inject(AInterface as TObject);
end;

procedure TJRPCContext.Inject(AObject: TObject);
var
  LRttiType: TRttiType;
begin
  LRttiType := TRttiUtils.Context.GetType(AObject.ClassType);
  TRttiUtils.ForEachFieldWithAttribute<ContextAttribute>(LRttiType,
    function (AField: TRttiField; AAttr: ContextAttribute): Boolean
    var
      LValue: TValue;
    begin
      if AField.FieldType is TRttiInstanceType then
      begin
        LValue := GetContextDataAs(TRttiInstanceType(AField.FieldType).MetaclassType);
        AField.SetValue(AObject, LValue);
        Result := True;
      end
      else if AField.FieldType is TRttiInterfaceType then
      begin
        LValue := GetContextDataAs(TRttiInterfaceType(AField.FieldType).GUID) as TObject;
        AField.SetValue(AObject, LValue);
        Result := True;
      end
      else
        raise EJRPCException.Create('Context variables should be an object or interface');
    end);
end;

{ EJRPCException }

procedure EJRPCException.AfterConstruction;
begin
  inherited;
  FCode := JRPC_INTERNAL_ERROR;
end;

function EJRPCException.ToJSON: string;
var
  LErrorNeonConfig: INeonConfiguration;
begin
  LErrorNeonConfig := JRPCNeonConfig.AddIgnoreMembers(['BaseException', 'InnerException', 'StackTrace', 'StackInfo', 'HelpContext']);
  Result := TNeon.ObjectToJSONString(Self, LErrorNeonConfig);
end;

{ EJRPCInvalidRequestError }

procedure EJRPCInvalidRequestError.AfterConstruction;
begin
  inherited;
  FCode := JRPC_INVALID_REQUEST;
end;

{ EJRPCMethodNotFoundError }

procedure EJRPCMethodNotFoundError.AfterConstruction;
begin
  inherited;
  FCode := JRPC_METHOD_NOT_FOUND;
end;

{ EJRPCInvalidParamsError }

procedure EJRPCInvalidParamsError.AfterConstruction;
begin
  inherited;
  FCode := JRPC_INVALID_PARAMS;
end;

{ EJRPCParseError }

procedure EJRPCParseError.AfterConstruction;
begin
  inherited;
  FCode := JRPC_PARSE_ERROR;
end;

{ TJRPCMessages }

procedure TJRPCMessages.AddMessage(AMsg: TJRPCMessage);
begin
  FList.Add(AMsg);
  FTypes := FTypes + [AMsg.GetType];
end;

constructor TJRPCMessages.Create;
begin
  FList := TObjectList<TJRPCMessage>.Create;
end;

class function TJRPCMessages.CreateFromJson(const AJSON: string): TJRPCMessages;
begin
  Result := TJRPCMessages.Create;
  try
    Result.FromJson(AJSON);
  except
    Result.Free;
    raise;
  end;
end;

destructor TJRPCMessages.Destroy;
begin
  FList.Free;
  inherited;
end;

procedure TJRPCMessages.FromJson(const AJSON: string);
var
  LJSON: TJSONValue;
begin
  LJSON := TJSONObject.ParseJSONValue(AJSON);
  if not Assigned(LJSON) then
    raise EJRPCParseError.Create('An invalid JSON was received by the server');
  try
    FromJson(LJSON);
  finally
    LJSON.Free;
  end;
end;

procedure TJRPCMessages.FromJson(AStream: TStream);
var
  LJSON: TJSONValue;
  LBuf: TBytes;
begin
  AStream.Read(LBuf, AStream.Size);
  LJSON := TJSONObject.ParseJSONValue(LBuf, 0, Length(LBuf));
  try
    FromJson(LJSON);
  finally
    LJSON.Free;
  end;
end;

procedure TJRPCMessages.FromJsonSingle(const AJSON: TJSONObject);
var
  LMsg: TJRPCMessage;
begin
  try
    case GetMessageType(AJSON) of
      TJRPCMessageType.Request:
      begin
        LMsg := TNeon.JSONToObject<TJRPCRequest>(AJSON, JRPCNeonConfig);
      end;
      TJRPCMessageType.Response:
      begin
        LMsg := TNeon.JSONToObject<TJRPCResponse>(AJSON, JRPCNeonConfig);
      end;
      TJRPCMessageType.Error:
      begin
        LMsg := TNeon.JSONToObject<TJRPCError>(AJSON, JRPCNeonConfig);
        (LMsg as TJRPCError).Request := True;
      end;
      TJRPCMessageType.Notification:
      begin
        LMsg := TNeon.JSONToObject<TJRPCNotification>(AJSON, JRPCNeonConfig);
      end
    else
      LMsg := nil;
    end;
  except
    on E: Exception do
      LMsg := TJRPCError.CreateFromException(E, AJSON);
  end;
  AddMessage(LMsg);
end;

function TJRPCMessages.GetCount: NativeInt;
begin
  Result := FList.Count;
end;

function TJRPCMessages.GetMessageType(AMessage: TJSONObject): TJRPCMessageType;
var
  LId, LMethod, LResult, LError: TJSONValue;
begin
  LMethod := AMessage.GetValue('method');
  LId := AMessage.GetValue('id');
  LResult := AMessage.GetValue('result');
  LError := AMessage.GetValue('error');

  if Assigned(LMethod) and Assigned(LId) then
    Exit(TJRPCMessageType.Request);

  if Assigned(LMethod) and not Assigned(LId) then
    Exit(TJRPCMessageType.Notification);

  if Assigned(LResult) then
    Exit(TJRPCMessageType.Response);

  if Assigned(LError) then
    Exit(TJRPCMessageType.Error);

  raise EJRPCInvalidRequestError.Create('Invalid JRPC Request');
end;

function TJRPCMessages.GetMessageTypes(ARequestJSON: TJSONValue): TJRPCMessageTypes;
begin
  Result := [];

  if ARequestJSON is TJSONObject then
    Exit(Result + [GetMessageType(ARequestJSON as TJSONObject)]);

  if ARequestJSON is TJSONArray then
    for var LReq in (ARequestJSON as TJSONArray) do
      Result := Result + [GetMessageType(LReq as TJSONObject)];
end;

function TJRPCMessages.ToJson: string;
var
  LRes: TJSONArray;
  LMsg: TJRPCMessage;
begin
  if FList.Count = 0 then
    Exit('');

  if FList.Count = 1 then
    Exit(TNeon.ObjectToJSONString(FList[0], JRPCNeonConfig));

  LRes := TJSONArray.Create;
  try
    for LMsg in FList do
    begin
      {
      if LMSg is TJRPCError then
        Continue;
      }
      LRes.AddElement(TNeon.ObjectToJSON(LMsg, JRPCNeonConfig));
    end;

    Result := TNeon.Print(LRes, True);
  finally
    LRes.Free;
  end;
end;

procedure TJRPCMessages.FromJson(AValue: TJSONValue);
begin
  if AValue is TJSONObject then
  begin
    FSingle := True;
    FromJsonSingle(AValue as TJSONObject);
  end
  else if AValue is TJSONArray then
    for var LItem in AValue as TJSONArray do
      FromJsonSingle(LItem as TJSONObject);
end;

{ TJRPCRequest }

class function TJRPCRequest.CreateFromJson(const AJSON: string): TJRPCRequest;
begin
  Result := Self.Create;
  try
    Result.FromJson(AJSON);
  except
    Result.Free;
    raise;
  end;
end;

function TJRPCRequest.GetType: TJRPCMessageType;
begin
  Result := TJRPCMessageType.Request;
end;

{ TJRPCNotification }

function TJRPCNotification.GetType: TJRPCMessageType;
begin
  Result := TJRPCMessageType.Notification;
end;

{ TJRPCError }

function TJRPCError.Clone: TJRPCError;
begin
  Result := TJRPCError.Create;
  Result.Id := FId;
  Result.Error.Code := FError.Code;
  Result.Error.Message := FError.Message;
  Result.Error.Data := FError.Data;
end;

constructor TJRPCError.Create;
begin
  inherited;
  FError := TJRPCErrorDetails.Create;
end;

destructor TJRPCError.Destroy;
begin
  FError.Free;
  inherited;
end;

function TJRPCError.GetType: TJRPCMessageType;
begin
  Result := TJRPCMessageType.Error;
end;

class function TJRPCError.CreateFromJson(const AJSON: string): TJRPCError;
begin
  Result := Self.Create;
  try
    Result.FromJson(AJSON);
  except
    Result.Free;
    raise;
  end;
end;

class function TJRPCError.CreateFromException(E: Exception; AJSON: TJSONObject): TJRPCError;
var
  LValue: TJSONValue;
  LId: TJRPCID;
begin
  LId := nil;
  LValue := AJSON.GetValue('id');
  if Assigned(LValue) then
  begin
    if LValue is TJSONNumber then
      LId := LValue.AsType<Integer>
    else if LValue is TJSONString then
      LId := LValue.AsType<string>
  end;

  Result := CreateFromException(E, LId);
end;

class function TJRPCError.CreateFromException(E: Exception; AId: TJRPCID): TJRPCError;
begin
  Result := TJRPCError.Create;

  if E is EJRPCException then
  begin
    Result.Id := AId;
    Result.Error.Code := EJRPCException(E).Code;
    Result.Error.Message := E.Message;
  end
  else if E is EJSONParseException then
  begin
    Result.Id := AId;
    Result.Error.Code := JRPC_PARSE_ERROR;
    Result.Error.Message := E.Message;
  end
  else
  begin
    Result.Id := AId;
    Result.Error.Code := JRPC_INVALID_REQUEST;
    Result.Error.Message := E.Message;
    Result.Error.Data := E.ClassName;
  end;
end;

{ TJErrorSerializer }

class function TJErrorSerializer.CanHandle(AType: PTypeInfo): Boolean;
begin
  Result := TypeInfoIs(AType);
end;

function TJErrorSerializer.Deserialize(AValue: TJSONValue; const AData: TValue;
  ANeonObject: TNeonRttiObject; AContext: IDeserializerContext): TValue;
var
  LType: TRttiType;
begin
  LType := TRttiUtils.Context.GetType(TJRPCError);
  Result := AContext.ReadDataMember(AValue, LType, AData, False);
end;

class function TJErrorSerializer.GetTargetInfo: PTypeInfo;
begin
  Result := TJRPCError.ClassInfo;
end;

function TJErrorSerializer.Serialize(const AValue: TValue;
  ANeonObject: TNeonRttiObject; AContext: ISerializerContext): TJSONValue;
var
  LError: TJRPCError;
begin
  LError := AValue.AsType<TJRPCError>;

  Result := AContext.WriteDataMember(LError, False);
  if LError.Id.IsNull then
    (Result as TJSONObject).AddPair('id', TJSONNull.Create);
end;

end.
