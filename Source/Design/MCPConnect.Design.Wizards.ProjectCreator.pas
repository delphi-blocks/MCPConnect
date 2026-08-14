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
unit MCPConnect.Design.Wizards.ProjectCreator;

interface

uses
  ToolsAPI,
  MCPConnect.Design.Wizards.Types;

type
  TMCPServerProjectCreator = class(TInterfacedObject, IOTACreator, IOTAProjectCreator,
    IOTAProjectCreator50, IOTAProjectCreator80, IOTAProjectCreator160
    {$IF CompilerVersion >= 32.0}, IOTAProjectCreator190{$ENDIF})
  private
    FConfig: TMCPProjectConfig;
    FFileName: string;
    FProjectName: string;

    function GetProjectTemplateName: string;
    /// <summary>
    ///   True when the .dpr itself hosts the fluent configuration, i.e. when
    ///   there is neither a main form nor a WebModule to put it in
    /// </summary>
    function ProjectSourceHostsConfig: Boolean;
  public
    constructor Create(const AConfig: TMCPProjectConfig);

    // IOTACreator
    function GetCreatorType: string;
    function GetExisting: Boolean;
    function GetFileSystem: string;
    function GetOwner: IOTAModule;
    function GetUnnamed: Boolean;

    // IOTAProjectCreator
    function GetFileName: string;
    function GetOptionFileName: string;
    function GetShowSource: Boolean;
    procedure NewDefaultModule;
    function NewOptionSource(const ProjectName: string): IOTAFile;
    procedure NewProjectResource(const Project: IOTAProject);
    function NewProjectSource(const ProjectName: string): IOTAFile;

    // IOTAProjectCreator50
    procedure NewDefaultProjectModule(const Project: IOTAProject);

    // IOTAProjectCreator80
    function GetProjectPersonality: string;

    // IOTAProjectCreator160
    function GetFrameworkType: string;
    function GetPlatforms: TArray<string>;
    function GetPreferredPlatform: string;
    procedure SetInitialOptions(const NewProject: IOTAProject);

    {$IF CompilerVersion >= 32.0}
    // IOTAProjectCreator190
    function GetSupportedPlatforms: TArray<string>;
    {$ENDIF}
  end;

resourcestring
  SStdioProjectResource = 'MCPProjectStdio';
  SIndyProjectResource = 'MCPProjectIndy';
  SWebBrokerProjectResource = 'MCPProjectWebBroker';
  SConsoleIndyProjectResource = 'MCPProjectConsoleIndy';
  SConsoleWebBrokerProjectResource = 'MCPProjectConsoleWebBroker';
  SServiceProjectResource = 'MCPProjectService';

  SProjectFileNamePrefix = 'Project';

implementation

uses
  System.SysUtils,
  PlatformAPI,
  MCPConnect.Design.Wizards.Utils,
  MCPConnect.Design.Wizards.CodeGen,
  MCPConnect.Design.Wizards.Modules;

const
  ProjectExtensions = '.bdsproj;.dproj;.dpr;.dpk;.cbproj';
  // The configuration sits inside a try block of a procedure in the .dpr
  ProjectSourceIndentLevel = 2;

{ TMCPServerProjectCreator }

constructor TMCPServerProjectCreator.Create(const AConfig: TMCPProjectConfig);
var
  LSuffix: string;
begin
  inherited Create;
  FConfig := AConfig;
  FFileName := GetNewModuleFileName(SProjectFileNamePrefix, '', '', False,
    LSuffix, ProjectExtensions);
  FProjectName := ExtractFileName(ChangeFileExt(FFileName, ''));
end;

function TMCPServerProjectCreator.GetProjectTemplateName: string;
begin
  if FConfig.AppKind = TMCPAppKind.Service then
    Result := SServiceProjectResource
  else if FConfig.AppKind = TMCPAppKind.Console then
  begin
    case FConfig.Transport of
      TMCPTransportKind.Stdio:     Result := SStdioProjectResource;
      TMCPTransportKind.Indy:      Result := SConsoleIndyProjectResource;
      TMCPTransportKind.WebBroker: Result := SConsoleWebBrokerProjectResource;
    else
      Assert(False, 'Unhandled MCP transport');
      Result := SStdioProjectResource;
    end;
  end
  else
  begin
    case FConfig.Transport of
      TMCPTransportKind.Indy:      Result := SIndyProjectResource;
      TMCPTransportKind.WebBroker: Result := SWebBrokerProjectResource;
    else
      // STDIO needs the console streams, the wizard never offers it for VCL
      Assert(False, 'Unsupported VCL transport');
      Result := SIndyProjectResource;
    end;
  end;
end;

function TMCPServerProjectCreator.ProjectSourceHostsConfig: Boolean;
begin
  Result := (FConfig.AppKind = TMCPAppKind.Console) and
    (FConfig.Transport in [TMCPTransportKind.Stdio, TMCPTransportKind.Indy]);
end;

{$REGION 'IOTACreator'}

function TMCPServerProjectCreator.GetCreatorType: string;
begin
  if FConfig.AppKind = TMCPAppKind.Console then
    Result := sConsole
  else
    Result := sApplication;
end;

function TMCPServerProjectCreator.GetExisting: Boolean;
begin
  Result := False;
end;

function TMCPServerProjectCreator.GetFileSystem: string;
begin
  Result := '';
end;

function TMCPServerProjectCreator.GetOwner: IOTAModule;
begin
  Result := ActiveProjectGroup;
end;

function TMCPServerProjectCreator.GetUnnamed: Boolean;
begin
  Result := True;
end;

{$ENDREGION}
{$REGION 'IOTAProjectCreator'}

function TMCPServerProjectCreator.GetFileName: string;
begin
  Result := FFileName;
end;

function TMCPServerProjectCreator.GetOptionFileName: string;
begin
  Result := '';
end;

function TMCPServerProjectCreator.GetShowSource: Boolean;
begin
  Result := True;
end;

procedure TMCPServerProjectCreator.NewDefaultModule;
begin
end;

function TMCPServerProjectCreator.NewOptionSource(const ProjectName: string): IOTAFile;
begin
  Result := nil;
end;

procedure TMCPServerProjectCreator.NewProjectResource(const Project: IOTAProject);
begin
end;

function TMCPServerProjectCreator.NewProjectSource(const ProjectName: string): IOTAFile;
var
  LBuilder: ISourceBuilder;
begin
  LBuilder := TSourceBuilder.FromResource(GetProjectTemplateName)
    .Add('PROJECT_NAME', FProjectName)
    .Add('SERVER_PORT', IntToStr(FConfig.ServerPort))
    .Add('MCP_PATH', FConfig.McpPath);

  // A console STDIO or Indy project has no host module, so the fluent
  // configuration goes straight into the .dpr. The sample unit is added to the
  // uses clause by the IDE when the module is created, hence it is not listed
  // here.
  if ProjectSourceHostsConfig then
  begin
    LBuilder
      .Add('EXTRA_UNITS', TMCPCodeGen.BuildUsesList(FConfig, ''))
      .Add('CONFIG_CODE', TMCPCodeGen.BuildConfigCode(FConfig,
        'LServer.JRPCServer', ProjectSourceIndentLevel));
  end;

  Result := TMCPSourceFile.Create(LBuilder.Build);
end;

{$ENDREGION}
{$REGION 'IOTAProjectCreator50'}

procedure TMCPServerProjectCreator.NewDefaultProjectModule(const Project: IOTAProject);
var
  LModuleServices: IOTAModuleServices;
  LToolsCreator: TMCPToolsUnitCreator;
  LToolsUnit: string;
  LWebModuleCreator: TMCPWebModuleCreator;
  LWebModuleUnit: string;
begin
  LModuleServices := BorlandIDEServices as IOTAModuleServices;

  // The tools unit is created first so that the host module can reference it
  // in its uses clause, but it is registered with the IDE last so that the
  // main form keeps being the first module of the project.
  LToolsCreator := nil;
  LToolsUnit := '';
  if FConfig.CreateSampleUnit then
  begin
    LToolsCreator := TMCPToolsUnitCreator.Create(FConfig);
    LToolsUnit := LToolsCreator.GeneratedUnitName;
  end;

  // A WebBroker host always gets its WebModule, and the service module has to
  // know its unit name to reference WebModuleClass, so it is built first
  LWebModuleCreator := nil;
  LWebModuleUnit := '';
  if FConfig.Transport = TMCPTransportKind.WebBroker then
  begin
    LWebModuleCreator := TMCPWebModuleCreator.Create(FConfig, LToolsUnit);
    LWebModuleUnit := LWebModuleCreator.GeneratedUnitName;
  end;

  // A VCL host gets its main form, a service host its TService descendant.
  // A console STDIO or Indy project gets neither: the .dpr is the host module.
  case FConfig.AppKind of
    TMCPAppKind.Console:
      ;

    TMCPAppKind.VCL:
      case FConfig.Transport of
        TMCPTransportKind.Indy:
          LModuleServices.CreateModule(TMCPIndyFormCreator.Create(FConfig, LToolsUnit));

        TMCPTransportKind.WebBroker:
          LModuleServices.CreateModule(TMCPWebBrokerFormCreator.Create(FConfig));
      else
        Assert(False, 'Unsupported VCL transport');
      end;

    TMCPAppKind.Service:
      LModuleServices.CreateModule(
        TMCPServiceModuleCreator.Create(FConfig, LToolsUnit, LWebModuleUnit));
  else
    Assert(False, 'Unhandled MCP application kind');
  end;

  if Assigned(LWebModuleCreator) then
    LModuleServices.CreateModule(LWebModuleCreator);

  if Assigned(LToolsCreator) then
    LModuleServices.CreateModule(LToolsCreator);
end;

{$ENDREGION}
{$REGION 'IOTAProjectCreator80'}

function TMCPServerProjectCreator.GetProjectPersonality: string;
begin
  Result := sDelphiPersonality;
end;

{$ENDREGION}
{$REGION 'IOTAProjectCreator160'}

function TMCPServerProjectCreator.GetFrameworkType: string;
begin
  if FConfig.AppKind = TMCPAppKind.Console then
    Result := sFrameworkTypeNone
  else
    Result := sFrameworkTypeVCL;
end;

function TMCPServerProjectCreator.GetPlatforms: TArray<string>;
begin
  Result := [cWin32Platform, cWin64Platform];
end;

function TMCPServerProjectCreator.GetPreferredPlatform: string;
begin
  Result := cWin32Platform;
end;

procedure TMCPServerProjectCreator.SetInitialOptions(const NewProject: IOTAProject);
begin
end;

{$ENDREGION}
{$REGION 'IOTAProjectCreator190'}

{$IF CompilerVersion >= 32.0}
function TMCPServerProjectCreator.GetSupportedPlatforms: TArray<string>;
begin
  Result := [GetPreferredPlatform];
end;
{$ENDIF}

{$ENDREGION}

end.
