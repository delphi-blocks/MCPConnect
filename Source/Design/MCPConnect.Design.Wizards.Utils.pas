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
unit MCPConnect.Design.Wizards.Utils;

interface

uses
  Winapi.Windows,
  System.Classes, System.SysUtils,
  ToolsAPI;

/// <summary>
///   Returns the current Project Group opened in the IDE
/// </summary>
function ActiveProjectGroup: IOTAProjectGroup;

/// <summary>
///   Returns the current Project active in the Project Manager
/// </summary>
function ActiveProject: IOTAProject;

type
  /// <summary>
  ///   Simple implementation of the IOTAFile interface. The constructor
  ///   parameter is returned by the GetSource method.
  /// </summary>
  TMCPSourceFile = class(TInterfacedObject, IOTAFile)
  private
    FSource: string;
  public
    constructor Create(const ASource: string);

    function GetAge: TDateTime;
    function GetSource: string;
  end;

  /// <summary>
  ///   Builds source code by replacing %PLACEHOLDER% markers in a template.
  ///   Use the class through this interface to avoid manual memory management.
  /// </summary>
  ISourceBuilder = interface
    ['{2B2C0A3E-5A1E-4F0C-9C5E-6E7B1D0A4F21}']
    function Add(const AName, AValue: string): ISourceBuilder;
    function Build: string;
  end;

  TSourceBuilder = class(TInterfacedObject, ISourceBuilder)
  private type
    TPlaceholder = record
      Name: string;
      Value: string;
    end;
  private
    FValues: TArray<TPlaceholder>;
    FSource: string;
  public
    constructor Create(const ASource: string);
    destructor Destroy; override;

    class function FromResource(const AResourceName: string): ISourceBuilder;
    class function FromString(const ASource: string): ISourceBuilder;

    function Add(const AName, AValue: string): ISourceBuilder;
    function Build: string;
  end;

/// <summary>
///   Returns a file name (with full path) not used by any file on disk nor by
///   any module currently open in the IDE.
/// </summary>
function GetNewModuleFileName(const APrefix, AOptionalDirectory,
  AOptionalFileName: string; AUseDefaultFileExt: Boolean;
  out ASuffix: string; const AExtensions: string = ''): string;

/// <summary>
///   Loads a RCDATA resource linked into the design-time package
/// </summary>
function LoadStringResource(const AResourceName: string): string;

implementation

function GetNewModuleFileName(const APrefix, AOptionalDirectory,
  AOptionalFileName: string; AUseDefaultFileExt: Boolean;
  out ASuffix: string; const AExtensions: string): string;
var
  LServices: IOTAModuleServices;

  function CheckFileExists(const AFileName: string): Boolean;
  begin
    Result := FileExists(AFileName) or
      ((BorlandIDEServices as IOTAModuleServices).FindModule(AFileName) <> nil);
  end;

  function CheckExtensions(const AFileName: string; const AExtensionList: TStrings): Boolean;
  var
    LIndex: Integer;
  begin
    Result := False;
    for LIndex := 0 to AExtensionList.Count - 1 do
    begin
      Result := CheckFileExists(ChangeFileExt(AFileName, AExtensionList[LIndex]));
      if Result then
        Break;
    end;
  end;

  function ModuleOrFileExists(const AFileName: string; const AExtensionList: TStrings): Boolean;
  begin
    Result := CheckFileExists(AFileName);
    if not Result and Assigned(AExtensionList) then
      Result := CheckExtensions(AFileName, AExtensionList);
  end;

  function CanFormatFileName(const AFileName: string): Boolean;
  begin
    Result := (Pos('%d', LowerCase(AFileName)) >= 1) or
      (Pos('%0:d', LowerCase(AFileName)) >= 1);
  end;

  function FindNextAvailableFileName(const AFileName: string; out ANameSuffix: string;
    const AExtensionList: TStrings): string;
  var
    LIndex: Integer;
    LFileNameFormat: string;
  begin
    LFileNameFormat := AFileName;
    if not CanFormatFileName(LFileNameFormat) then
      LFileNameFormat := ExtractFilePath(LFileNameFormat) +
        ChangeFileExt(ExtractFileName(LFileNameFormat), '') + '%d' +
        ExtractFileExt(LFileNameFormat);

    LIndex := 1;
    Result := Format(LFileNameFormat, [LIndex]);
    while ModuleOrFileExists(Result, AExtensionList) do
    begin
      Inc(LIndex);
      Result := Format(LFileNameFormat, [LIndex]);
    end;
    ANameSuffix := IntToStr(LIndex);
  end;

  function GetDefaultFileExt: string;
  var
    LNewTextFileIdent, LNewClassName, LNewFileName: string;
  begin
    LServices.GetNewModuleAndClassName(APrefix, LNewTextFileIdent, LNewClassName, LNewFileName);
    Result := ExtractFileExt(LNewFileName);
  end;

  function GetDefaultDirectory: string;
  var
    LNewTextFileIdent, LNewClassName, LNewFileName: string;
  begin
    LServices.GetNewModuleAndClassName(APrefix, LNewTextFileIdent, LNewClassName, LNewFileName);
    Result := ExtractFilePath(LNewFileName);
  end;

var
  LFileName: string;
  LDirectory: string;
  LExtensions: TStrings;
begin
  LExtensions := nil;
  try
    if AExtensions <> '' then
    begin
      LExtensions := TStringList.Create;
      LExtensions.Delimiter := ';';
      LExtensions.StrictDelimiter := True;
      LExtensions.DelimitedText := AExtensions;
    end;

    ASuffix := '';
    LServices := BorlandIDEServices as IOTAModuleServices;

    if AOptionalFileName = '' then
      LFileName := ChangeFileExt(APrefix + '%d', GetDefaultFileExt)
    else
    begin
      LFileName := AOptionalFileName;
      if AUseDefaultFileExt then
        LFileName := ChangeFileExt(LFileName, GetDefaultFileExt);
    end;

    if AOptionalDirectory <> '' then
      LDirectory := ExtractFilePath(AOptionalDirectory)
    else
      LDirectory := GetDefaultDirectory;

    if not CanFormatFileName(LFileName) then
    begin
      Result := LDirectory + LFileName;
      if ModuleOrFileExists(Result, LExtensions) then
        Result := FindNextAvailableFileName(Result, ASuffix, LExtensions);
    end
    else
      Result := FindNextAvailableFileName(LDirectory + LFileName, ASuffix, LExtensions);
  finally
    LExtensions.Free;
  end;
end;

function ActiveProjectGroup: IOTAProjectGroup;
var
  LModuleServices: IOTAModuleServices;
begin
  LModuleServices := BorlandIDEServices as IOTAModuleServices;
  Result := LModuleServices.MainProjectGroup;
end;

function ActiveProject: IOTAProject;
var
  LModuleServices: IOTAModuleServices;
begin
  LModuleServices := BorlandIDEServices as IOTAModuleServices;
  Result := LModuleServices.GetActiveProject;
end;

function LoadStringResource(const AResourceName: string): string;
var
  LResourceStream: TResourceStream;
  LValue: TStrings;
begin
  LResourceStream := TResourceStream.Create(HInstance, AResourceName, RT_RCDATA);
  try
    if LResourceStream.Size = 0 then
      raise Exception.CreateFmt('Resource %s is empty', [AResourceName]);

    LValue := TStringList.Create;
    try
      LResourceStream.Position := 0;
      LValue.LoadFromStream(LResourceStream);
      Result := LValue.Text;
    finally
      LValue.Free;
    end;
  finally
    LResourceStream.Free;
  end;
end;

{ TMCPSourceFile }

constructor TMCPSourceFile.Create(const ASource: string);
begin
  inherited Create;
  FSource := ASource;
end;

function TMCPSourceFile.GetAge: TDateTime;
begin
  Result := -1;
end;

function TMCPSourceFile.GetSource: string;
begin
  Result := FSource;
end;

{ TSourceBuilder }

constructor TSourceBuilder.Create(const ASource: string);
begin
  inherited Create;
  FSource := ASource;
end;

destructor TSourceBuilder.Destroy;
begin
  FValues := nil;
  inherited;
end;

class function TSourceBuilder.FromResource(const AResourceName: string): ISourceBuilder;
begin
  Result := TSourceBuilder.Create(LoadStringResource(AResourceName));
end;

class function TSourceBuilder.FromString(const ASource: string): ISourceBuilder;
begin
  Result := TSourceBuilder.Create(ASource);
end;

function TSourceBuilder.Add(const AName, AValue: string): ISourceBuilder;
var
  LPlaceholder: TPlaceholder;
begin
  LPlaceholder.Name := AName;
  LPlaceholder.Value := AValue;
  FValues := FValues + [LPlaceholder];
  Result := Self;
end;

function TSourceBuilder.Build: string;
var
  LPlaceholder: TPlaceholder;
begin
  Result := FSource;
  for LPlaceholder in FValues do
  begin
    Result := StringReplace(Result, '%' + LPlaceholder.Name + '%',
      LPlaceholder.Value, [rfReplaceAll, rfIgnoreCase]);
  end;
end;

end.
