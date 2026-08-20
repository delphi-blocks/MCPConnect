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
  Vcl.Controls,
  Vcl.ExtCtrls,
  Vcl.Forms,
  Vcl.Themes,
  ToolsAPI,
  BrandingAPI,
  IDETheme.Utils;

type
  TMCPIDETheme = class
  public
    class function Active: Boolean;
    class function IsDark: Boolean;
    class function GetColor(const AColor: TColor): TColor;
    class procedure ApplyTheme(AForm: TCustomForm);
    class procedure ApplyToPanel(APanel: TWinControl; ABackgroundColor: TColor = clBtnFace);
  end;

implementation

{ TMCPIDETheme }

class function TMCPIDETheme.Active: Boolean;
begin
  Result := IDEThemeAvailable;
end;

class function TMCPIDETheme.GetColor(const AColor: TColor): TColor;
begin
  if IDEThemeAvailable then
    Result := ThemeProperties.StyleServices.GetSystemColor(AColor)
  else
    Result := AColor;
end;

class function TMCPIDETheme.IsDark: Boolean;
var
  LColor: TColor;
begin
  if IDEThemeAvailable then
  begin
    LColor := ThemeProperties.StyleServices.GetSystemColor(clWindow);
    Result := (GetRValue(LColor) + GetGValue(LColor) + GetBValue(LColor)) div 3 < 128;
  end
  else
    Result := False;
end;

class procedure TMCPIDETheme.ApplyTheme(AForm: TCustomForm);
var
  LStyle: TCustomStyleServices;
begin
  if not Assigned(AForm) then
    Exit;

  if not IDEThemeAvailable then
    Exit;

  LStyle := ThemeProperties.StyleServices;
  AForm.StyleElements := AForm.StyleElements - [seClient];
  AForm.Color := LStyle.GetSystemColor(clWindow);

  IDEThemeManager.RegisterFormClass(TCustomFormClass(AForm.ClassType));
  ThemeProperties.ApplyTheme(AForm);
end;

class procedure TMCPIDETheme.ApplyToPanel(APanel: TWinControl; ABackgroundColor: TColor);
var
  LStyle: TCustomStyleServices;
begin
  if not Assigned(APanel) then
    Exit;

  if not IDEThemeAvailable then
    Exit;

  if APanel is TCustomForm then
    Exit;

  LStyle := ThemeProperties.StyleServices;
  if APanel is TPanel then
  begin
    TPanel(APanel).StyleElements := TPanel(APanel).StyleElements - [seClient];
    TPanel(APanel).ParentBackground := False;
    TPanel(APanel).Color := LStyle.GetSystemColor(ABackgroundColor);
  end;
end;

end.
