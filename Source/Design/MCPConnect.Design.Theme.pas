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
unit MCPConnect.Design.Theme;

interface

uses
  System.SysUtils,
  System.Classes,
  Winapi.Windows,
  Vcl.Graphics,
  Vcl.Forms,
  Vcl.Themes,
  ToolsAPI;

type
  /// <summary>
  ///   Wrapper around IOTAIDEThemingServices used to keep the design-time
  ///   wizards in sync with the active IDE theme (light or dark). Every helper
  ///   degrades gracefully: when the IDE does not expose the theming service,
  ///   or theming is turned off, the calls are no-ops and the forms keep their
  ///   design-time colors.
  /// </summary>
  TMCPIDETheme = class
  strict private
    class var FRegisteredForms: TArray<TCustomFormClass>;

    class function GetServices: IOTAIDEThemingServices;
    class function IsRegistered(const AFormClass: TCustomFormClass): Boolean;
  public
    /// <summary>
    ///   True when the IDE exposes the theming service and theming is enabled.
    /// </summary>
    class function Active: Boolean;
    /// <summary>
    ///   True when the active IDE theme is a dark one.
    /// </summary>
    class function IsDark: Boolean;
    /// <summary>
    ///   Resolves a system color against the active IDE theme; without a theme
    ///   the color is returned unchanged.
    /// </summary>
    class function GetColor(const AColor: TColor): TColor;
    /// <summary>
    ///   Applies the active IDE theme to a form and registers its class for the
    ///   IDE style hooks (once), so the wizard draws like an inbuilt IDE dialog.
    /// </summary>
    class procedure ApplyTheme(AForm: TCustomForm);
  end;

implementation

{ TMCPIDETheme }

class function TMCPIDETheme.Active: Boolean;
var
  LServices: IOTAIDEThemingServices;
begin
  LServices := GetServices;
  Result := Assigned(LServices) and LServices.IDEThemingEnabled;
end;

class function TMCPIDETheme.GetColor(const AColor: TColor): TColor;
var
  LServices: IOTAIDEThemingServices;
begin
  LServices := GetServices;
  if Assigned(LServices) and LServices.IDEThemingEnabled then
    Result := LServices.StyleServices.GetSystemColor(AColor)
  else
    Result := AColor;
end;

class function TMCPIDETheme.GetServices: IOTAIDEThemingServices;
begin
  if not Supports(BorlandIDEServices, IOTAIDEThemingServices, Result) then
    Result := nil;
end;

class function TMCPIDETheme.IsDark: Boolean;
var
  LServices: IOTAIDEThemingServices;
  LColor: TColor;
begin
  LServices := GetServices;
  if Assigned(LServices) and LServices.IDEThemingEnabled then
  begin
    LColor := LServices.StyleServices.GetSystemColor(clWindow);
    Result := (GetRValue(LColor) + GetGValue(LColor) + GetBValue(LColor)) div 3 < 128;
  end
  else
    Result := False;
end;

class function TMCPIDETheme.IsRegistered(const AFormClass: TCustomFormClass): Boolean;
var
  LIndex: Integer;
begin
  Result := False;
  for LIndex := Low(FRegisteredForms) to High(FRegisteredForms) do
    if FRegisteredForms[LIndex] = AFormClass then
      Exit(True);
end;

class procedure TMCPIDETheme.ApplyTheme(AForm: TCustomForm);
var
  LServices: IOTAIDEThemingServices;
  LFormClass: TCustomFormClass;
begin
  if not Assigned(AForm) then
    Exit;

  LServices := GetServices;
  if (not Assigned(LServices)) or (not LServices.IDEThemingEnabled) then
    Exit;

  // Enable the IDE style hooks for the form class once, so every instance
  // draws like an inbuilt IDE dialog (e.g. the frameless group boxes of the
  // Options dialog)
  LFormClass := TCustomFormClass(AForm.ClassType);
  if not IsRegistered(LFormClass) then
  begin
    LServices.RegisterFormClass(LFormClass);
    FRegisteredForms := FRegisteredForms + [LFormClass];
  end;

  // Synchronize Color / Font.Color of the controls without style hooks
  LServices.ApplyTheme(AForm);
end;

end.