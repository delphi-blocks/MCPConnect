unit MCP.Tools;

interface

uses
  System.SysUtils,
  System.Rtti,
  System.JSON,
  System.Types,
  System.Generics.Collections,
  Neon.Core.Attributes,
  Neon.Core.Nullables;

type
  TMeta = class

  end;

  /// <summary>
  /// A JSON Schema object defining the expected parameters for the tool.
  /// </summary>
  TToolInputSchema = class
    /// <summary>
    /// Definitions of types used in the schema.
    /// </summary>
    [NeonProperty('$defs'), NeonInclude(IncludeIf.NotEmpty)] Defs: TDictionary<string, TValue>;

    /// <summary>
    /// The type of the schema (e.g., "object").
    /// </summary>
    [NeonProperty('type')] &Type: string;

    /// <summary>
    /// A map of property names to their schema definitions.
    /// </summary>
    [NeonProperty('properties'), NeonInclude(IncludeIf.NotEmpty)] Properties: TDictionary<string, TValue>;

    /// <summary>
    /// A list of required property names.
    /// </summary>
    [NeonProperty('required'), NeonInclude(IncludeIf.NotEmpty)] Required: TArray<string>;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  /// <summary>
  /// Optional properties describing tool behavior
  /// </summary>
  TToolAnnotation = class
    /// <summary>
    /// Human-readable title for the tool
    /// </summary>
    [NeonProperty('title')] Title: Nullable<string>;

    /// <summary>
    /// If true, the tool does not modify its environment
    /// </summary>
    [NeonProperty('readOnlyHint')] ReadOnlyHint: Nullable<Boolean>;

    /// <summary>
    /// If true, the tool may perform destructive updates
    /// </summary>
    [NeonProperty('destructiveHint')] DestructiveHint: Nullable<Boolean>;

    /// <summary>
    /// If true, repeated calls with same args have no additional effect
    /// </summary>
    [NeonProperty('idempotentHint')] IdempotentHint: Nullable<Boolean>;

    /// <summary>
    /// If true, tool interacts with external entities
    /// </summary>
    [NeonProperty('openWorldHint')] OpenWorldHint: Nullable<Boolean>;
  end;

  /// <summary>
  /// Tool represents the definition for a tool the client can call.
  /// </summary>
  TTool = class
    /// <summary>
    /// Meta is a metadata object that is reserved by MCP for storing additional information
    /// </summary>
    [NeonProperty('_meta'), NeonInclude(IncludeIf.NotEmpty)] Meta: TMeta;

    /// <summary>
    /// The name of the tool
    /// </summary>
    [NeonProperty('name')] Name: string;

    /// <summary>
    /// A human-readable description of the tool
    /// </summary>
    [NeonProperty('description')] Description: Nullable<string>;

    /// <summary>
    /// A JSON Schema object defining the expected parameters for the tool
    /// </summary>
    [NeonProperty('inputSchema')] InputSchema: TToolInputSchema;

    /// <summary>
    /// Alternative to InputSchema - allows arbitrary JSON Schema to be provided
    /// </summary>
    [NeonIgnore] RawInputSchema: TJSONObject;

    /// <summary>
    /// Optional JSON Schema defining expected output structure
    /// </summary>
    [NeonIgnore] RawOutputSchema: TJSONObject;

    /// <summary>
    /// Optional properties describing tool behavior
    /// </summary>
    [NeonProperty('annotations')] Annotations: TToolAnnotation;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TToolInputSchema }

constructor TToolInputSchema.Create;
begin
  Defs := TDictionary<string, TValue>.Create;
  Properties := TDictionary<string, TValue>.Create;
end;

destructor TToolInputSchema.Destroy;
begin
  Properties.Free;
  Defs.Free;

  inherited;
end;

{ TTool }

constructor TTool.Create;
begin
  Meta := TMeta.Create;
  InputSchema := TToolInputSchema.Create;
  Annotations := TToolAnnotation.Create;
  RawInputSchema := TJSONObject.Create;
  RawOutputSchema := TJSONObject.Create;
end;

destructor TTool.Destroy;
begin
  Meta.Free;
  InputSchema.Free;
  Annotations.Free;
  RawInputSchema.Free;
  RawOutputSchema.Free;

  inherited;
end;

end.
