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
unit MCPConnect.Design.Wizards.Modules;

interface

uses
  ToolsAPI,
  MCPConnect.Design.Wizards.Types;

type
  /// <summary>
  ///   Common behaviour of every module (unit / form) created by the wizard
  /// </summary>
  TMCPModuleCreator = class abstract (TInterfacedObject, IOTACreator, IOTAModuleCreator)
  private
    FConfig: TMCPProjectConfig;
    FFileName: string;
    FUnitName: string;
  protected
    property Config: TMCPProjectConfig read FConfig;
  public
    constructor Create(const AConfig: TMCPProjectConfig; const AFileNamePrefix: string);

    // IOTACreator
    function GetCreatorType: string; virtual;
    function GetExisting: Boolean;
    function GetFileSystem: string;
    function GetOwner: IOTAModule;
    function GetUnnamed: Boolean;

    // IOTAModuleCreator
    function GetAncestorName: string; virtual;
    function GetImplFileName: string;
    function GetIntfFileName: string;
    function GetFormName: string; virtual;
    function GetMainForm: Boolean; virtual;
    function GetShowForm: Boolean; virtual;
    function GetShowSource: Boolean; virtual;
    function NewFormFile(const FormIdent, AncestorIdent: string): IOTAFile; virtual;
    function NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile; virtual; abstract;
    function NewIntfSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
    procedure FormCreated(const FormEditor: IOTAFormEditor);

    /// <summary>
    ///   Name of the generated unit, without path nor extension
    /// </summary>
    property GeneratedUnitName: string read FUnitName;
  end;

  /// <summary>
  ///   Main form of an Indy hosted server: it owns the TJRPCIndyServer and
  ///   carries the whole MCP configuration
  /// </summary>
  TMCPIndyFormCreator = class(TMCPModuleCreator)
  private
    FToolsUnit: string;
  public
    constructor Create(const AConfig: TMCPProjectConfig; const AToolsUnit: string);

    function GetCreatorType: string; override;
    function GetAncestorName: string; override;
    function GetFormName: string; override;
    function GetMainForm: Boolean; override;
    function NewFormFile(const FormIdent, AncestorIdent: string): IOTAFile; override;
    function NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile; override;
  end;

  /// <summary>
  ///   Main form of a WebBroker hosted server: it only drives the
  ///   TIdHTTPWebBrokerBridge, the MCP configuration lives in the WebModule
  /// </summary>
  TMCPWebBrokerFormCreator = class(TMCPModuleCreator)
  public
    constructor Create(const AConfig: TMCPProjectConfig);

    function GetCreatorType: string; override;
    function GetAncestorName: string; override;
    function GetFormName: string; override;
    function GetMainForm: Boolean; override;
    function NewFormFile(const FormIdent, AncestorIdent: string): IOTAFile; override;
    function NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile; override;
  end;

  /// <summary>
  ///   WebModule hosting the TJRPCServer and the TJRPCDispatcher
  /// </summary>
  TMCPWebModuleCreator = class(TMCPModuleCreator)
  private
    FToolsUnit: string;
  public
    constructor Create(const AConfig: TMCPProjectConfig; const AToolsUnit: string);

    function GetCreatorType: string; override;
    function GetAncestorName: string; override;
    function GetFormName: string; override;
    function GetMainForm: Boolean; override;
    function GetShowForm: Boolean; override;
    function NewFormFile(const FormIdent, AncestorIdent: string): IOTAFile; override;
    function NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile; override;
  end;

  /// <summary>
  ///   TService descendant hosting the server. With the Indy transport it also
  ///   carries the MCP configuration; with WebBroker it only owns the HTTP
  ///   listener and the configuration stays in the WebModule.
  /// </summary>
  TMCPServiceModuleCreator = class(TMCPModuleCreator)
  private
    FToolsUnit: string;
    FWebModuleUnit: string;
  public
    constructor Create(const AConfig: TMCPProjectConfig;
      const AToolsUnit, AWebModuleUnit: string);

    function GetCreatorType: string; override;
    function GetAncestorName: string; override;
    function GetFormName: string; override;
    function GetMainForm: Boolean; override;
    function GetShowForm: Boolean; override;
    function NewFormFile(const FormIdent, AncestorIdent: string): IOTAFile; override;
    function NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile; override;
  end;

  /// <summary>
  ///   Plain unit with a sample tool, resource and prompt class
  /// </summary>
  TMCPToolsUnitCreator = class(TMCPModuleCreator)
  public
    constructor Create(const AConfig: TMCPProjectConfig);

    function GetCreatorType: string; override;
    function GetAncestorName: string; override;
    function GetFormName: string; override;
    function GetMainForm: Boolean; override;
    function GetShowForm: Boolean; override;
    function NewFormFile(const FormIdent, AncestorIdent: string): IOTAFile; override;
    function NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile; override;
  end;

resourcestring
  SIndyFormSrcResource = 'MCPIndyFormSRC';
  SIndyFormDfmResource = 'MCPIndyFormDFM';
  SWebBrokerFormSrcResource = 'MCPWBFormSRC';
  SWebBrokerFormDfmResource = 'MCPWBFormDFM';
  SWebModuleSrcResource = 'MCPWebModuleSRC';
  SWebModuleDfmResource = 'MCPWebModuleDFM';
  SServiceIndySrcResource = 'MCPServiceIndySRC';
  SServiceWebBrokerSrcResource = 'MCPServiceWBSRC';
  SServiceDfmResource = 'MCPServiceDFM';
  SToolsUnitResource = 'MCPToolsUnit';

  SFormUnitPrefix = 'FormUnit';
  SWebModuleUnitPrefix = 'WebModuleUnit';
  SServiceUnitPrefix = 'ServiceUnit';
  SToolsUnitPrefix = 'ToolsUnit';

implementation

uses
  System.SysUtils,
  MCPConnect.Design.Wizards.Utils,
  MCPConnect.Design.Wizards.CodeGen;

const
  MainFormName = 'frmMain';
  WebModuleFormName = 'WebModule1';
  ServiceModuleFormName = 'ServiceModule';
  ConfigIndentLevel = 1;

  // A console host must not pull the VCL in through the designer class group
  VclClassGroup = 'Vcl.Controls.TControl';
  ConsoleClassGroup = 'System.Classes.TPersistent';

function ClassGroupForAppKind(AAppKind: TMCPAppKind): string;
begin
  if AAppKind = TMCPAppKind.Console then
    Result := ConsoleClassGroup
  else
    Result := VclClassGroup;
end;

{ TMCPModuleCreator }

constructor TMCPModuleCreator.Create(const AConfig: TMCPProjectConfig;
  const AFileNamePrefix: string);
var
  LSuffix: string;
begin
  inherited Create;
  FConfig := AConfig;
  FFileName := GetNewModuleFileName(AFileNamePrefix, '', '', False, LSuffix);
  FUnitName := ExtractFileName(ChangeFileExt(FFileName, ''));
end;

function TMCPModuleCreator.GetCreatorType: string;
begin
  Result := sUnit;
end;

function TMCPModuleCreator.GetExisting: Boolean;
begin
  Result := False;
end;

function TMCPModuleCreator.GetFileSystem: string;
begin
  Result := '';
end;

function TMCPModuleCreator.GetOwner: IOTAModule;
begin
  Result := ActiveProject;
end;

function TMCPModuleCreator.GetUnnamed: Boolean;
begin
  Result := True;
end;

function TMCPModuleCreator.GetAncestorName: string;
begin
  Result := '';
end;

function TMCPModuleCreator.GetImplFileName: string;
begin
  Result := FFileName;
end;

function TMCPModuleCreator.GetIntfFileName: string;
begin
  Result := '';
end;

function TMCPModuleCreator.GetFormName: string;
begin
  Result := '';
end;

function TMCPModuleCreator.GetMainForm: Boolean;
begin
  Result := False;
end;

function TMCPModuleCreator.GetShowForm: Boolean;
begin
  Result := True;
end;

function TMCPModuleCreator.GetShowSource: Boolean;
begin
  Result := True;
end;

function TMCPModuleCreator.NewFormFile(const FormIdent, AncestorIdent: string): IOTAFile;
begin
  Result := nil;
end;

function TMCPModuleCreator.NewIntfSource(const ModuleIdent, FormIdent,
  AncestorIdent: string): IOTAFile;
begin
  Result := nil;
end;

procedure TMCPModuleCreator.FormCreated(const FormEditor: IOTAFormEditor);
begin
end;

{ TMCPIndyFormCreator }

constructor TMCPIndyFormCreator.Create(const AConfig: TMCPProjectConfig;
  const AToolsUnit: string);
begin
  inherited Create(AConfig, SFormUnitPrefix);
  FToolsUnit := AToolsUnit;
end;

function TMCPIndyFormCreator.GetCreatorType: string;
begin
  Result := sForm;
end;

function TMCPIndyFormCreator.GetAncestorName: string;
begin
  Result := 'TForm';
end;

function TMCPIndyFormCreator.GetFormName: string;
begin
  Result := MainFormName;
end;

function TMCPIndyFormCreator.GetMainForm: Boolean;
begin
  Result := True;
end;

function TMCPIndyFormCreator.NewFormFile(const FormIdent, AncestorIdent: string): IOTAFile;
begin
  Result := TMCPSourceFile.Create(
    TSourceBuilder.FromResource(SIndyFormDfmResource)
      .Add('SERVER_PORT', IntToStr(Config.ServerPort))
      .Build
  );
end;

function TMCPIndyFormCreator.NewImplSource(const ModuleIdent, FormIdent,
  AncestorIdent: string): IOTAFile;
begin
  Result := TMCPSourceFile.Create(
    TSourceBuilder.FromResource(SIndyFormSrcResource)
      .Add('UNIT_NAME', GeneratedUnitName)
      .Add('EXTRA_UNITS', TMCPCodeGen.BuildUsesList(Config, FToolsUnit))
      .Add('CONFIG_CODE', TMCPCodeGen.BuildConfigCode(Config, 'FServer.JRPCServer', ConfigIndentLevel))
      .Build
  );
end;

{ TMCPWebBrokerFormCreator }

constructor TMCPWebBrokerFormCreator.Create(const AConfig: TMCPProjectConfig);
begin
  inherited Create(AConfig, SFormUnitPrefix);
end;

function TMCPWebBrokerFormCreator.GetCreatorType: string;
begin
  Result := sForm;
end;

function TMCPWebBrokerFormCreator.GetAncestorName: string;
begin
  Result := 'TForm';
end;

function TMCPWebBrokerFormCreator.GetFormName: string;
begin
  Result := MainFormName;
end;

function TMCPWebBrokerFormCreator.GetMainForm: Boolean;
begin
  Result := True;
end;

function TMCPWebBrokerFormCreator.NewFormFile(const FormIdent, AncestorIdent: string): IOTAFile;
begin
  Result := TMCPSourceFile.Create(
    TSourceBuilder.FromResource(SWebBrokerFormDfmResource)
      .Add('SERVER_PORT', IntToStr(Config.ServerPort))
      .Build
  );
end;

function TMCPWebBrokerFormCreator.NewImplSource(const ModuleIdent, FormIdent,
  AncestorIdent: string): IOTAFile;
begin
  Result := TMCPSourceFile.Create(
    TSourceBuilder.FromResource(SWebBrokerFormSrcResource)
      .Add('UNIT_NAME', GeneratedUnitName)
      .Build
  );
end;

{ TMCPWebModuleCreator }

constructor TMCPWebModuleCreator.Create(const AConfig: TMCPProjectConfig;
  const AToolsUnit: string);
begin
  inherited Create(AConfig, SWebModuleUnitPrefix);
  FToolsUnit := AToolsUnit;
end;

function TMCPWebModuleCreator.GetCreatorType: string;
begin
  Result := sForm;
end;

function TMCPWebModuleCreator.GetAncestorName: string;
begin
  Result := 'TWebModule';
end;

function TMCPWebModuleCreator.GetFormName: string;
begin
  Result := WebModuleFormName;
end;

function TMCPWebModuleCreator.GetMainForm: Boolean;
begin
  Result := False;
end;

function TMCPWebModuleCreator.GetShowForm: Boolean;
begin
  Result := False;
end;

function TMCPWebModuleCreator.NewFormFile(const FormIdent, AncestorIdent: string): IOTAFile;
begin
  Result := TMCPSourceFile.Create(
    TSourceBuilder.FromResource(SWebModuleDfmResource).Build
  );
end;

function TMCPWebModuleCreator.NewImplSource(const ModuleIdent, FormIdent,
  AncestorIdent: string): IOTAFile;
begin
  Result := TMCPSourceFile.Create(
    TSourceBuilder.FromResource(SWebModuleSrcResource)
      .Add('UNIT_NAME', GeneratedUnitName)
      .Add('SERVER_NAME', Config.ServerName)
      .Add('MCP_PATH', Config.McpPath)
      .Add('CLASS_GROUP', ClassGroupForAppKind(Config.AppKind))
      .Add('EXTRA_UNITS', TMCPCodeGen.BuildUsesList(Config, FToolsUnit))
      .Add('CONFIG_CODE', TMCPCodeGen.BuildConfigCode(Config, 'JRPCServer', ConfigIndentLevel))
      .Build
  );
end;

{ TMCPServiceModuleCreator }

constructor TMCPServiceModuleCreator.Create(const AConfig: TMCPProjectConfig;
  const AToolsUnit, AWebModuleUnit: string);
begin
  inherited Create(AConfig, SServiceUnitPrefix);
  FToolsUnit := AToolsUnit;
  FWebModuleUnit := AWebModuleUnit;
end;

function TMCPServiceModuleCreator.GetCreatorType: string;
begin
  Result := sForm;
end;

function TMCPServiceModuleCreator.GetAncestorName: string;
begin
  Result := 'TService';
end;

function TMCPServiceModuleCreator.GetFormName: string;
begin
  Result := ServiceModuleFormName;
end;

function TMCPServiceModuleCreator.GetMainForm: Boolean;
begin
  Result := True;
end;

function TMCPServiceModuleCreator.GetShowForm: Boolean;
begin
  Result := False;
end;

function TMCPServiceModuleCreator.NewFormFile(const FormIdent, AncestorIdent: string): IOTAFile;
begin
  Result := TMCPSourceFile.Create(
    TSourceBuilder.FromResource(SServiceDfmResource).Build
  );
end;

function TMCPServiceModuleCreator.NewImplSource(const ModuleIdent, FormIdent,
  AncestorIdent: string): IOTAFile;
var
  LBuilder: ISourceBuilder;
begin
  if Config.Transport = TMCPTransportKind.WebBroker then
  begin
    LBuilder := TSourceBuilder.FromResource(SServiceWebBrokerSrcResource)
      .Add('WEBMODULE_UNIT', FWebModuleUnit);
  end
  else
  begin
    LBuilder := TSourceBuilder.FromResource(SServiceIndySrcResource)
      .Add('EXTRA_UNITS', TMCPCodeGen.BuildUsesList(Config, FToolsUnit))
      .Add('CONFIG_CODE', TMCPCodeGen.BuildConfigCode(Config, 'FServer.JRPCServer',
        ConfigIndentLevel));
  end;

  Result := TMCPSourceFile.Create(
    LBuilder
      .Add('UNIT_NAME', GeneratedUnitName)
      .Add('SERVICE_NAME', Config.ServiceName)
      .Add('SERVER_NAME', Config.ServerName)
      .Add('SERVER_PORT', IntToStr(Config.ServerPort))
      .Build
  );
end;

{ TMCPToolsUnitCreator }

constructor TMCPToolsUnitCreator.Create(const AConfig: TMCPProjectConfig);
begin
  inherited Create(AConfig, SToolsUnitPrefix);
end;

function TMCPToolsUnitCreator.GetCreatorType: string;
begin
  Result := sUnit;
end;

function TMCPToolsUnitCreator.GetAncestorName: string;
begin
  Result := '';
end;

function TMCPToolsUnitCreator.GetFormName: string;
begin
  Result := '';
end;

function TMCPToolsUnitCreator.GetMainForm: Boolean;
begin
  Result := False;
end;

function TMCPToolsUnitCreator.GetShowForm: Boolean;
begin
  Result := False;
end;

function TMCPToolsUnitCreator.NewFormFile(const FormIdent, AncestorIdent: string): IOTAFile;
begin
  Result := nil;
end;

function TMCPToolsUnitCreator.NewImplSource(const ModuleIdent, FormIdent,
  AncestorIdent: string): IOTAFile;
begin
  Result := TMCPSourceFile.Create(
    TSourceBuilder.FromResource(SToolsUnitResource)
      .Add('UNIT_NAME', GeneratedUnitName)
      .Build
  );
end;

end.
