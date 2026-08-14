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
unit MCPConnect.Design.Wizards;

interface

uses
  ToolsAPI;

resourcestring
  SWizardName = 'MCP Server Application Wizard';
  SWizardComment = 'Creates a new MCP (Model Context Protocol) server application';
  SWizardAuthor = 'MCPConnect Development Team';
  SGalleryCategory = 'MCPConnect';
  SGalleryCategoryId = 'MCPConnect.Wizards';
  SWizardIconResource = 'MCPConnectWizardIcon';

type
  TMCPServerProjectWizard = class(TNotifierObject, IOTAWizard, IOTARepositoryWizard,
    IOTARepositoryWizard60, IOTARepositoryWizard80, IOTAProjectWizard, IOTAProjectWizard100)
  public
    constructor Create;

    // IOTAWizard
    function GetIDString: string;
    function GetName: string;
    function GetState: TWizardState;
    procedure Execute;

    // IOTARepositoryWizard
    function GetAuthor: string;
    function GetComment: string;
    function GetPage: string;
    function GetGlyph: {$IFDEF WIN32}Cardinal{$ELSE}UInt64{$ENDIF};

    // IOTARepositoryWizard60
    function GetDesigner: string;

    // IOTARepositoryWizard80
    function GetGalleryCategory: IOTAGalleryCategory;
    function GetPersonality: string;

    // IOTAProjectWizard100
    function IsVisible(Project: IOTAProject): Boolean;
  end;

implementation

uses
  Winapi.Windows,
  MCPConnect.Design.Wizards.Types,
  MCPConnect.Design.Wizards.Dialog,
  MCPConnect.Design.Wizards.ProjectCreator;

{ TMCPServerProjectWizard }

constructor TMCPServerProjectWizard.Create;
var
  LCategoryServices: IOTAGalleryCategoryManager;
begin
  inherited Create;
  LCategoryServices := BorlandIDEServices as IOTAGalleryCategoryManager;
  LCategoryServices.AddCategory(LCategoryServices.FindCategory(sCategoryRoot),
    SGalleryCategoryId, SGalleryCategory);
end;

procedure TMCPServerProjectWizard.Execute;
var
  LConfig: TMCPProjectConfig;
  LModuleServices: IOTAModuleServices;
begin
  LConfig := TMCPProjectConfig.CreateDefault;
  if not TformMCPProjectWizard.FindConfig(LConfig) then
    Exit;

  LModuleServices := BorlandIDEServices as IOTAModuleServices;
  LModuleServices.CreateModule(TMCPServerProjectCreator.Create(LConfig));
end;

function TMCPServerProjectWizard.GetIDString: string;
begin
  Result := SGalleryCategoryId + '.Server';
end;

function TMCPServerProjectWizard.GetName: string;
begin
  Result := SWizardName;
end;

function TMCPServerProjectWizard.GetState: TWizardState;
begin
  Result := [wsEnabled];
end;

function TMCPServerProjectWizard.GetAuthor: string;
begin
  Result := SWizardAuthor;
end;

function TMCPServerProjectWizard.GetComment: string;
begin
  Result := SWizardComment;
end;

function TMCPServerProjectWizard.GetPage: string;
begin
  Result := SGalleryCategory;
end;

function TMCPServerProjectWizard.GetGlyph: {$IFDEF WIN32}Cardinal{$ELSE}UInt64{$ENDIF};
begin
  Result := LoadIcon(HInstance, PChar(SWizardIconResource));
end;

function TMCPServerProjectWizard.GetDesigner: string;
begin
  Result := dAny;
end;

function TMCPServerProjectWizard.GetGalleryCategory: IOTAGalleryCategory;
begin
  Result := (BorlandIDEServices as IOTAGalleryCategoryManager).FindCategory(SGalleryCategoryId);
end;

function TMCPServerProjectWizard.GetPersonality: string;
begin
  Result := sDelphiPersonality;
end;

function TMCPServerProjectWizard.IsVisible(Project: IOTAProject): Boolean;
begin
  Result := True;
end;

end.
