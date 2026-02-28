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
unit MCPConnect.Core.Utils;

interface

uses
  System.Classes, System.SysUtils, System.Rtti, System.TypInfo,
  System.Generics.Collections, System.RegularExpressions;

type
  /// <summary>
  ///   Anonymous method type that defines a custom disposal action.
  ///   Used by the garbage collector to execute custom cleanup logic
  ///   when disposing of tracked objects.
  /// </summary>
  /// <remarks>
  ///   Use this when objects require special cleanup beyond the default
  ///   Free method (e.g., releasing external resources, closing handles).
  /// </remarks>
  /// <example>
  ///   <code>
  ///     FGC.Add(TValue.From(MyObject), procedure begin
  ///       MyObject.CloseConnection;
  ///       MyObject.Free;
  ///     end);
  ///   </code>
  /// </example>
  TDisposeAction = reference to procedure;

  /// <summary>
  ///   IGarbageCollector defines an interface for managing and disposing
  ///   of objects or values that require explicit cleanup. It allows adding
  ///   values with optional custom disposal actions, and provides a method
  ///   to collect and dispose all tracked garbage.
  /// </summary>
  /// <remarks>
  ///   The garbage collector is typically injected via [Context] attribute
  ///   in MCP tool classes, providing automatic memory management for
  ///   objects created during request processing.
  /// </remarks>
  IGarbageCollector = interface
    ['{407C168F-A81C-4E41-96B2-BFEA94C58B0D}']
    /// <summary>
    ///   Adds a value to the garbage collector for automatic disposal.
    ///   Uses default disposal logic (calls Free for objects, recursive
    ///   cleanup for arrays).
    /// </summary>
    /// <param name="AValue">The value to track. Typically a TValue.From(Object)</param>
    procedure Add(const AValue: TValue); overload;

    /// <summary>
    ///   Adds a value with a custom disposal action.
    /// </summary>
    /// <param name="AValue">The value to track</param>
    /// <param name="AAction">Custom cleanup procedure to execute instead of default disposal</param>
    procedure Add(const AValue: TValue; AAction: TDisposeAction); overload;

    /// <summary>
    ///   Adds multiple values at once for automatic disposal.
    /// </summary>
    /// <param name="AValues">Array of values to track</param>
    procedure Add(const AValues: TArray<TValue>); overload;

    /// <summary>
    ///   Adds multiple values with a shared custom disposal action.
    /// </summary>
    /// <param name="AValues">Array of values to track</param>
    /// <param name="AAction">Custom cleanup procedure applied to all values</param>
    procedure Add(const AValues: TArray<TValue>; AAction: TDisposeAction); overload;

    /// <summary>
    ///   Immediately disposes all tracked values and clears the collection.
    ///   Called automatically on destruction, but can be invoked manually
    ///   for early cleanup.
    /// </summary>
    procedure CollectGarbage();
  end;

  /// <summary>
  /// TGarbageCollector implements IGarbageCollector and provides a
  /// mechanism to track and dispose of objects or values. It supports
  /// custom disposal actions and recursive cleanup of arrays and class
  /// instances. Garbage is collected either on demand or automatically
  /// on destruction.
  /// </summary>
  /// <remarks>
  /// When the garbage collector is destroyed, it will automatically
  /// collect and dispose all tracked garbage. If you get the instance through
  /// the *CreateInstance* method, then it will be destroyed automatically
  /// when it goes out of scope.
  /// </remarks>
  /// <example>
  ///   <code>
  ///   // Typical usage in an MCP tool class:
  ///   TMyTool = class
  ///   private
  ///     [Context] FGC: IGarbageCollector;
  ///   public
  ///     [McpTool('get_data', 'Returns data')]
  ///     function GetData: TContentList;
  ///     var
  ///       LList: TStringList;
  ///       LBuilder: TToolResultBuilder;
  ///     begin
  ///       LList := TStringList.Create;
  ///       FGC.Add(TValue.From(LList)); // Auto-freed after request
  ///
  ///       LList.Add('Item 1');
  ///       LList.Add('Item 2');
  ///
  ///       LBuilder := TToolResultBuilder.CreateInstance;
  ///       LBuilder.AddText(LList.Text);
  ///       Result := LBuilder.Build;
  ///     end;
  ///   end;
  ///   </code>
  /// </example>
  TGarbageCollector = class(TInterfacedObject, IGarbageCollector)
  private
    FGarbage: TDictionary<TObject, TDisposeAction>;

    /// <summary>
    ///   Internal method that performs the actual disposal of a single value.
    ///   Handles different type kinds: tkClass (calls Free), tkArray/tkDynArray
    ///   (recursive cleanup of elements).
    /// </summary>
    /// <param name="AValue">The value to dispose</param>
    procedure CollectGarbageValue(const AValue: TValue);

    /// <summary>
    ///   Internal method that processes a single garbage entry. If a custom
    ///   disposal action is defined, executes it; otherwise, uses default
    ///   disposal logic via CollectGarbageValue.
    /// </summary>
    /// <param name="AGarbage">A pair containing the value and its optional disposal action</param>
    procedure CollectSingleGarbage(AGarbage: TPair<TObject, TDisposeAction>);
  public
    /// <summary>
    ///   Creates a new garbage collector instance as an interface reference.
    ///   Recommended for automatic memory management (destroyed when out of scope).
    /// </summary>
    /// <returns>IGarbageCollector interface reference</returns>
    class function CreateInstance: IGarbageCollector; static;
  public
    /// <summary>
    ///   Initializes the garbage collector and internal dictionary.
    /// </summary>
    constructor Create; virtual;

    /// <summary>
    ///   Destroys the garbage collector. Automatically calls CollectGarbage
    ///   to dispose all tracked objects before destruction.
    /// </summary>
    destructor Destroy; override;

    /// <summary>
    ///   Adds a value to the garbage collector for automatic disposal.
    ///   Uses default disposal logic (calls Free for objects).
    /// </summary>
    /// <param name="AValue">The value to track</param>
    procedure Add(const AValue: TValue); overload;

    /// <summary>
    ///   Adds a value with a custom disposal action.
    /// </summary>
    /// <param name="AValue">The value to track</param>
    /// <param name="AAction">Custom cleanup procedure</param>
    procedure Add(const AValue: TValue; AAction: TDisposeAction); overload;

    /// <summary>
    ///   Adds multiple values at once for automatic disposal.
    /// </summary>
    /// <param name="AValues">Array of values to track</param>
    procedure Add(const AValues: TArray<TValue>); overload;

    /// <summary>
    ///   Adds multiple values with a shared custom disposal action.
    /// </summary>
    /// <param name="AValues">Array of values to track</param>
    /// <param name="AAction">Custom cleanup procedure for all values</param>
    procedure Add(const AValues: TArray<TValue>; AAction: TDisposeAction); overload;

    /// <summary>
    ///   Immediately disposes all tracked values and clears the collection.
    ///   Called automatically on destruction.
    /// </summary>
    procedure CollectGarbage();
  end;


  /// <summary>
  ///   TRouteMatcher provides URI template matching and parameter extraction.
  ///   It supports curly-bracket style templates (e.g., '/users/{id}') and
  ///   automatically extracts and URL-decodes matched parameters.
  /// </summary>
  TRouteMatcher = class
  public type
    TRouteParams = TDictionary<string, string>;
  private
    FParams: TRouteParams;
    FParamNames: TList<string>;
    /// <summary>
    ///   Converts a URI template into a regular expression with named capture groups.
    /// </summary>
    /// <param name="ATemplate">The URI template to convert (e.g., '/users/{id}')</param>
    /// <returns>A regex string suitable for TRegEx matching</returns>
    function BuildRegex(const ATemplate: string): string;
  public
    /// <summary>
    ///   Initializes a new instance of the TRouteMatcher class.
    /// </summary>
    constructor Create;
    /// <summary>
    ///   Cleans up internal resources used by the matcher.
    /// </summary>
    destructor Destroy; override;
  
    /// <summary>
    ///   Attempts to match an input string against a URI template.
    ///   If successful, extracts parameters into the Params dictionary.
    /// </summary>
    /// <param name="ATemplate">The URI template (e.g., '/users/{id}')</param>
    /// <param name="AInput">The actual URI or string to match</param>
    /// <returns>True if the input matches the template; otherwise False</returns>
    function Match(const ATemplate, AInput: string): Boolean;
  
    /// <summary>
    ///   Dictionary containing the parameters extracted during the last
    ///   successful Match operation. Keys are parameter names from the
    ///   template, and values are URL-decoded strings from the input.
    /// </summary>
    property Params: TRouteParams read FParams;
  end;


  /// <summary>
  ///   Creates a new TValue initialized with the default value for the specified
  ///   RTTI type. Supports common Delphi type kinds including integers, floats,
  ///   strings, classes, and records.
  /// </summary>
  /// <param name="AType">RTTI type information for which to create a default value</param>
  /// <returns>
  ///   A TValue containing the default/zero value for the type:
  ///   - Integers: 0
  ///   - Floats: 0.0
  ///   - Strings: empty string
  ///   - Classes: nil
  ///   - Records: zero-initialized memory
  /// </returns>
  /// <remarks>
  ///   Used internally by the JRPC invoker to create default parameter values
  ///   when arguments are omitted in JSON-RPC requests. For record types,
  ///   allocates and zero-initializes memory temporarily to create the TValue.
  /// </remarks>
  /// <exception cref="EJRPCException">
  ///   Raised if the type kind is not supported (e.g., interfaces, pointers, methods)
  /// </exception>
  function CreateNewValue(AType: TRttiType): TValue;



implementation

uses
  System.NetEncoding,
  MCPConnect.JRPC.Core;

function CreateNewValue(AType: TRttiType): TValue;
var
  LAllocatedMem: Pointer;
begin
  case AType.TypeKind of
    tkInteger: Result := TValue.From<Integer>(0);
    tkInt64:   Result := TValue.From<Int64>(0);
    tkChar:    Result := TValue.From<UTF8Char>(#0);
    tkWChar:   Result := TValue.From<Char>(#0);
    tkFloat:   Result := TValue.From<Double>(0);
    tkString:  Result := TValue.From<UTF8String>('');
    tkWString: Result := TValue.From<string>('');
    tkLString: Result := TValue.From<UTF8String>('');
    tkUString: Result := TValue.From<string>('');
    tkClass:   Result := nil;
    tkRecord:
    begin
      LAllocatedMem := AllocMem(AType.TypeSize);
      try
        TValue.Make(LAllocatedMem, AType.Handle, Result);
      finally
        FreeMem(LAllocatedMem);
      end;
    end;
  else
    raise EJRPCException.CreateFmt('Error creating type: %s', [AType.Name]);
  end;
end;


{ TGarbageCollector }

procedure TGarbageCollector.Add(const AValue: TValue);
begin
  if AValue.IsObject then
    Add(AValue, nil);
end;

procedure TGarbageCollector.Add(const AValue: TValue; AAction: TDisposeAction);
begin
  if FGarbage.ContainsKey(AValue.AsObject) then
    Exit;
  FGarbage.Add(AValue.AsObject, AAction);
end;

procedure TGarbageCollector.Add(const AValues: TArray<TValue>);
begin
  Add(AValues, nil);
end;

procedure TGarbageCollector.Add(const AValues: TArray<TValue>; AAction: TDisposeAction);
var
  LValue: TValue;
begin
  for LValue in AValues do
    if LValue.IsObject then
      Add(LValue, AAction);
end;

procedure TGarbageCollector.CollectGarbage;
var
  LGarbage: TPair<TObject, TDisposeAction>;
begin
  for LGarbage in FGarbage do
    CollectSingleGarbage(LGarbage);
  FGarbage.Clear;
end;

procedure TGarbageCollector.CollectGarbageValue(const AValue: TValue);
var
  LIndex: Integer;
begin
  case AValue.Kind of
    tkClass:
    begin
      if (AValue.AsObject <> nil) then
        //if not TRttiUtils.HasAttribute<SingletonAttribute>(AValue.AsObject.ClassType) then
        AValue.AsObject.Free;
    end;

    tkArray,
    tkDynArray:
    begin
      for LIndex := 0 to AValue.GetArrayLength - 1 do
        CollectGarbageValue(AValue.GetArrayElement(LIndex));
    end;
  end;
end;

procedure TGarbageCollector.CollectSingleGarbage(AGarbage: TPair<TObject, TDisposeAction>);
begin
  if Assigned(AGarbage.Value) then
    AGarbage.Value()
  else
    CollectGarbageValue(AGarbage.Key);
end;

constructor TGarbageCollector.Create;
begin
  inherited;
  FGarbage := TDictionary<TObject, TDisposeAction>.Create;
end;

class function TGarbageCollector.CreateInstance: IGarbageCollector;
begin
  Result := TGarbageCollector.Create;
end;

destructor TGarbageCollector.Destroy;
begin
  CollectGarbage;
  FGarbage.Free;
  inherited;
end;

{ TRouteMatcher }

constructor TRouteMatcher.Create;
begin
  FParams := TRouteParams.Create;
  FParamNames := TList<string>.Create;
end;

destructor TRouteMatcher.Destroy;
begin
  FParams.Free;
  FParamNames.Free;
  inherited;
end;

function TRouteMatcher.BuildRegex(const ATemplate: string): string;
var
  LMatch: TMatch;
begin
  FParamNames.Clear;

  // 1. Find the names inside { } before we escape the string
  LMatch := TRegEx.Match(ATemplate, '\{([a-zA-Z0-9_]+)\}');
  while LMatch.Success do
  begin
    FParamNames.Add(LMatch.Groups[1].Value);
    LMatch := LMatch.NextMatch;
  end;

  // 2. Escape the template for Regex safety
  Result := TRegEx.Escape(ATemplate);

  // 3. Replace the escaped \{name\} with the named capture group (?P<name>[^/]+)
  Result := TRegEx.Replace(Result, '\\\{([a-zA-Z0-9_]+)\\\}', '(?P<$1>[^/]+)');

  Result := '^' + Result + '$';
end;

function TRouteMatcher.Match(const ATemplate, AInput: string): Boolean;
var
  LRegex: string;
  LMatch: TMatch;
  LName: string;
begin
  FParams.Clear;
  LRegex := BuildRegex(ATemplate);

  LMatch := TRegEx.Match(AInput, LRegex, [roIgnoreCase]);
  Result := LMatch.Success;

  if not Result then
    Exit;

  for LName in FParamNames do
    if LMatch.Groups[LName].Success then
      FParams.Add(LName, TNetEncoding.URL.Decode(LMatch.Groups[LName].Value));
end;

end.
