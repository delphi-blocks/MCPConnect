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
unit MCPConnect.Content.Writers;

interface

uses
  System.SysUtils, System.TypInfo, System.Rtti, System.SyncObjs, System.Generics.Collections,

  MCPConnect.MCP.Attributes,
  MCPConnect.MCP.Types,
  MCPConnect.MCP.Tools;


type
  TMCPWriterContext = record
    ContentList: TContentList;
    ToolAttributes: TAttributes;
  end;

  /// <summary>
  ///   Base class for a Custom Writer
  /// </summary>
  TMCPCustomWriter = class abstract(TObject)
  protected
    class function ClassDistance: Integer;
    class function ClassIs(AClass: TClass): Boolean;
    class function TypeInfoIs(AInfo: PTypeInfo): Boolean;
    class function TypeInfoIsClass(AInfo: PTypeInfo): Boolean;
  protected
    class function GetTargetInfo: PTypeInfo; virtual;
    class function CanHandle(AType: PTypeInfo): Boolean; virtual; abstract;
  public
    procedure Write(const AValue: TValue; AContext: TMCPWriterContext); virtual; abstract;
  end;
  TCustomWriterClass = class of TMCPCustomWriter;

  /// <summary>
  ///   Global registry for custom writers
  /// </summary>
  TMCPWriterRegistry = class
  public type
    TWriterInfo = record
    public
      WriterClass: TCustomWriterClass;
      Distance: Integer;
    public
      class function FromWriter(AWriterClass: TCustomWriterClass): TWriterInfo; static;
    end;
  private type
    WriterCacheRegistry = class(TObjectDictionary<PTypeInfo, TMCPCustomWriter>);
    WriterClassRegistry = class(TList<TWriterInfo>);
  private
    FRegistryClass: WriterClassRegistry;
    FRegistryCache: WriterCacheRegistry;
    FRegistryCacheLock: TCriticalSection;
    function GetCount: Integer;

    function InternalGetWriter(ATypeInfo: PTypeInfo): TMCPCustomWriter;
  public
    constructor Create;
    destructor Destroy; override;
  public
    procedure Clear;
    procedure ClearCache;
    procedure Assign(ARegistry: TMCPWriterRegistry);

    function RegisterWriter(AWriterClass: TCustomWriterClass): TMCPWriterRegistry; overload;
    procedure UnregisterWriter(AWriterClass: TCustomWriterClass);

    function GetWriter<T>: TMCPCustomWriter; overload;
    function GetWriter(AValue: TValue): TMCPCustomWriter; overload;
    function GetWriter(ATargetClass: TClass): TMCPCustomWriter; overload;
    function GetWriter(ATargetInfo: PTypeInfo): TMCPCustomWriter; overload;
  public
    property Count: Integer read GetCount;
  end;





implementation

uses
  Neon.Core.Utils;

procedure TMCPWriterRegistry.Assign(ARegistry: TMCPWriterRegistry);
var
  LWriterInfo: TWriterInfo;
begin
  {for LWriterInfo in ARegistry.FRegistry do
    FRegistry.Add(LWriterInfo);}
end;

constructor TMCPWriterRegistry.Create;
begin
  FRegistryClass := WriterClassRegistry.Create();
  FRegistryCache := WriterCacheRegistry.Create([doOwnsValues]);
  FRegistryCacheLock := TCriticalSection.Create;
end;

destructor TMCPWriterRegistry.Destroy;
begin
  FRegistryClass.Free;
  FRegistryCache.Free;
  FRegistryCacheLock.Free;
  inherited;
end;

procedure TMCPWriterRegistry.Clear;
begin
  FRegistryClass.Clear;
  FRegistryCacheLock.Enter;
  try
    FRegistryCache.Clear;
  finally
    FRegistryCacheLock.Leave
  end;
end;

procedure TMCPWriterRegistry.ClearCache;
begin
  FRegistryCacheLock.Enter;
  try
    FRegistryCache.Clear;
  finally
    FRegistryCacheLock.Leave
  end;
end;

function TMCPWriterRegistry.GetCount: Integer;
begin
  Result := FRegistryClass.Count;
end;

function TMCPWriterRegistry.GetWriter(ATargetClass: TClass): TMCPCustomWriter;
begin
  Result := InternalGetWriter(ATargetClass.ClassInfo);
end;

function TMCPWriterRegistry.GetWriter(ATargetInfo: PTypeInfo): TMCPCustomWriter;
begin
  Result := InternalGetWriter(ATargetInfo);
end;

function TMCPWriterRegistry.GetWriter(AValue: TValue): TMCPCustomWriter;
begin
  Result := InternalGetWriter(AValue.TypeInfo);
end;

function TMCPWriterRegistry.GetWriter<T>: TMCPCustomWriter;
begin
  Result := InternalGetWriter(TypeInfo(T));
end;

function TMCPWriterRegistry.InternalGetWriter(ATypeInfo: PTypeInfo): TMCPCustomWriter;
var
  LInfo: TWriterInfo;
  LClass: TCustomWriterClass;
  LDistanceMax: Integer;
begin
  Result := nil;
  LClass := nil;
  LDistanceMax := 0;

  FRegistryCacheLock.Enter;
  try
    if FRegistryCache.TryGetValue(ATypeInfo, Result) then
      Exit(Result);
  finally
    FRegistryCacheLock.Leave
  end;

  for LInfo in FRegistryClass do
  begin
    if LInfo.WriterClass.CanHandle(ATypeInfo) then
    begin
      if LInfo.Distance = -1 then
      begin
        LClass := LInfo.WriterClass;
        Break;
      end
      else
      begin
        if LInfo.Distance > LDistanceMax then
        begin
          LDistanceMax := LInfo.Distance;
          LClass := LInfo.WriterClass;
        end;
      end;
    end;
  end;

  if Assigned(LClass) then
  begin
    FRegistryCacheLock.Enter;
    try
      if FRegistryCache.TryGetValue(ATypeInfo, Result) then
        Exit(Result);

      Result := LClass.Create;
      FRegistryCache.Add(ATypeInfo, Result);
    finally
      FRegistryCacheLock.Leave
    end;
  end;
end;

function TMCPWriterRegistry.RegisterWriter(AWriterClass: TCustomWriterClass): TMCPWriterRegistry;
begin
  FRegistryClass.Add(TWriterInfo.FromWriter(AWriterClass));
  Result := Self;
end;

procedure TMCPWriterRegistry.UnregisterWriter(AWriterClass: TCustomWriterClass);
var
  LIndex: Integer;
begin
  for LIndex := 0 to FRegistryClass.Count - 1 do
    if FRegistryClass[LIndex].WriterClass = AWriterClass then
    begin
      FRegistryClass.Delete(LIndex);
      ClearCache;
      Break;
    end;
end;

{ TMCPCustomWriter }

class function TMCPCustomWriter.ClassDistance: Integer;
begin
  Result := TRttiUtils.ClassDistanceFromRoot(GetTargetInfo);
end;

class function TMCPCustomWriter.ClassIs(AClass: TClass): Boolean;
var
  LType: TRttiType;
begin
  Result := False;

  LType := TRttiUtils.Context.GetType(GetTargetInfo);
  if Assigned(LType) and (LType.TypeKind = tkClass) then
    Result := AClass.InheritsFrom(LType.AsInstance.MetaclassType);
end;

class function TMCPCustomWriter.GetTargetInfo: PTypeInfo;
begin
  Result := nil;
end;

class function TMCPCustomWriter.TypeInfoIs(AInfo: PTypeInfo): Boolean;
var
  LType: TRttiType;
begin
  Result := False;
  LType := TRttiUtils.Context.GetType(AInfo);
  if Assigned(LType) and (LType.TypeKind = tkClass) then
    Result := ClassIs(LType.AsInstance.MetaclassType);
end;

class function TMCPCustomWriter.TypeInfoIsClass(AInfo: PTypeInfo): Boolean;
var
  LType: TRttiType;
begin
  Result := False;
  LType := TRttiUtils.Context.GetType(AInfo);
  if Assigned(LType) and (LType.TypeKind = tkClass) then
    Result := True;
end;

{ TMCPWriterRegistry.TWriterInfo }

class function TMCPWriterRegistry.TWriterInfo.FromWriter(AWriterClass: TCustomWriterClass): TWriterInfo;
begin
  Result.WriterClass := AWriterClass;
  Result.Distance := AWriterClass.ClassDistance;
end;

end.
