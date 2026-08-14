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
unit MCPConnect.Design.Register;

interface

procedure Register;

implementation

uses
  System.SysUtils,
  Winapi.Windows,
  ToolsAPI,
  MCPConnect.Design.Wizards;

resourcestring
  SMCPConnectCaption = 'MCPConnect';
  SMCPConnectLicense = 'MIT License';
  SMCPConnectDescription = 'MCPConnect - Model Context Protocol server library for Delphi' + sLineBreak +
    'https://github.com/delphi-blocks/MCPConnect';
  SSplashResource = 'MCPConnectSplash';

var
  FAboutBoxIndex: Integer = -1;

procedure RegisterSplashScreen;
var
  LProductImage: HBITMAP;
begin
  if not Assigned(SplashScreenServices) then
    Exit;

  LProductImage := LoadBitmap(FindResourceHInstance(HInstance), PChar(SSplashResource));
  if LProductImage <> 0 then
    SplashScreenServices.AddPluginBitmap(SMCPConnectCaption, LProductImage,
      False, SMCPConnectLicense);
end;

procedure RegisterAboutBox;
var
  LAboutBoxServices: IOTAAboutBoxServices;
  LProductImage: HBITMAP;
begin
  if FAboutBoxIndex <> -1 then
    Exit;

  if not Supports(BorlandIDEServices, IOTAAboutBoxServices, LAboutBoxServices) then
    Exit;

  LProductImage := LoadBitmap(FindResourceHInstance(HInstance), PChar(SSplashResource));
  if LProductImage = 0 then
    Exit;

  FAboutBoxIndex := LAboutBoxServices.AddPluginInfo(SMCPConnectCaption,
    SMCPConnectDescription, LProductImage, False);
end;

procedure UnregisterAboutBox;
var
  LAboutBoxServices: IOTAAboutBoxServices;
begin
  if FAboutBoxIndex = -1 then
    Exit;

  if Supports(BorlandIDEServices, IOTAAboutBoxServices, LAboutBoxServices) then
    LAboutBoxServices.RemovePluginInfo(FAboutBoxIndex);

  FAboutBoxIndex := -1;
end;

procedure Register;
begin
  RegisterPackageWizard(TMCPServerProjectWizard.Create);

  RegisterSplashScreen;
  RegisterAboutBox;
end;

initialization

finalization
  UnregisterAboutBox;

end.
