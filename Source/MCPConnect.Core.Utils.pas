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
  System.Generics.Collections;

type
  // With this anonymous method, you can define custom disposal actions
  TDisposeAction = reference to procedure;

  /// <summary>
  ///   IGarbageCollector defines an interface for managing and disposing
  ///   of objects or values that require explicit cleanup. It allows adding
  ///   values with optional custom disposal actions, and provides a method
  ///   to collect and dispose all tracked garbage.
  /// </summary>
  IGarbageCollector = interface
    ['{407C168F-A81C-4E41-96B2-BFEA94C58B0D}']
    procedure Add(const AValue: TValue); overload;
    procedure Add(const AValue: TValue; AAction: TDisposeAction); overload;
    procedure Add(const AValues: TArray<TValue>); overload;
    procedure Add(const AValues: TArray<TValue>; AAction: TDisposeAction); overload;
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
  TGarbageCollector = class(TInterfacedObject, IGarbageCollector)
  private
    FGarbage: TDictionary<TValue, TDisposeAction>;
    procedure CollectGarbageValue(const AValue: TValue);
    procedure CollectSingleGarbage(AGarbage: TPair<TValue, TDisposeAction>);
  public
    class function CreateInstance: IGarbageCollector; static;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure Add(const AValue: TValue); overload;
    procedure Add(const AValue: TValue; AAction: TDisposeAction); overload;
    procedure Add(const AValues: TArray<TValue>); overload;
    procedure Add(const AValues: TArray<TValue>; AAction: TDisposeAction); overload;
    procedure CollectGarbage();
  end;

function CreateNewValue(AType: TRttiType): TValue;

implementation

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
    raise Exception.CreateFmt('Error creating type: %s', [AType.Name]);
  end;
end;


{ TGarbageCollector }

procedure TGarbageCollector.Add(const AValue: TValue);
begin
  Add(AValue, nil);
end;

procedure TGarbageCollector.Add(const AValue: TValue; AAction: TDisposeAction);
begin
  if FGarbage.ContainsKey(AValue) then
    Exit;
  FGarbage.Add(AValue, AAction);
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
    Add(LValue, AAction);
end;

procedure TGarbageCollector.CollectGarbage;
var
  LGarbage: TPair<TValue, TDisposeAction>;
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

procedure TGarbageCollector.CollectSingleGarbage(
  AGarbage: TPair<TValue, TDisposeAction>);
begin
  if Assigned(AGarbage.Value) then
    AGarbage.Value()
  else
    CollectGarbageValue(AGarbage.Key);
end;

constructor TGarbageCollector.Create;
begin
  inherited;
  FGarbage := TDictionary<TValue, TDisposeAction>.Create;
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

end.
