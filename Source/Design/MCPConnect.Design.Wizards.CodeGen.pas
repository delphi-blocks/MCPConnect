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
unit MCPConnect.Design.Wizards.CodeGen;

interface

uses
  MCPConnect.Design.Wizards.Types;

type
  /// <summary>
  ///   Builds the source code fragments injected into the generated host
  ///   module: the extra uses entries and the fluent configuration chain.
  /// </summary>
  TMCPCodeGen = class
  public const
    SampleToolClass = 'TSampleTool';
    SampleResourceClass = 'TSampleResource';
    SamplePromptsClass = 'TSamplePrompts';
  private
    class function QuotedPascalString(const AValue: string): string; static;
    class function SplitList(const AValue: string): TArray<string>; static;
    class function ArrayLiteral(const AValue: string): string; static;
    class function IndentLines(const AText: string; ALevel: Integer): string; static;
    class function TokenLocationLiteral(ALocation: TMCPTokenLocation): string; static;
  public
    /// <summary>
    ///   Returns the MCPConnect uses entries needed by the generated
    ///   configuration code, one per line, terminated by a semicolon.
    /// </summary>
    class function BuildUsesList(const AConfig: TMCPProjectConfig;
      const AToolsUnit: string): string; static;

    /// <summary>
    ///   Returns the whole fluent configuration chain, indented by AIndent
    ///   levels (2 spaces each) and terminated by a semicolon.
    /// </summary>
    class function BuildConfigCode(const AConfig: TMCPProjectConfig;
      const AReceiver: string; AIndent: Integer): string; static;
  end;

implementation

uses
  System.Classes, System.SysUtils;

const
  IndentSize = 2;

{ TMCPCodeGen }

class function TMCPCodeGen.IndentLines(const AText: string; ALevel: Integer): string;
var
  LLines: TStrings;
  LIndex: Integer;
  LIndent: string;
begin
  if AText = '' then
    Exit('');

  LIndent := StringOfChar(' ', ALevel * IndentSize);
  LLines := TStringList.Create;
  try
    LLines.Text := AText;
    for LIndex := 0 to LLines.Count - 1 do
    begin
      if LLines[LIndex] <> '' then
        LLines[LIndex] := LIndent + LLines[LIndex];
    end;
    Result := LLines.Text;
  finally
    LLines.Free;
  end;
end;

class function TMCPCodeGen.TokenLocationLiteral(ALocation: TMCPTokenLocation): string;
begin
  case ALocation of
    TMCPTokenLocation.Bearer: Result := 'TAuthTokenLocation.Bearer';
    TMCPTokenLocation.Cookie: Result := 'TAuthTokenLocation.Cookie';
    TMCPTokenLocation.Header: Result := 'TAuthTokenLocation.Header';
  else
    Assert(False, 'Unhandled MCP token location');
    Result := 'TAuthTokenLocation.Bearer';
  end;
end;

class function TMCPCodeGen.QuotedPascalString(const AValue: string): string;
begin
  Result := QuotedStr(AValue);
end;

class function TMCPCodeGen.SplitList(const AValue: string): TArray<string>;
var
  LItem: string;
begin
  Result := [];
  for LItem in AValue.Split([',', ';']) do
  begin
    if not LItem.Trim.IsEmpty then
      Result := Result + [LItem.Trim];
  end;
end;

class function TMCPCodeGen.ArrayLiteral(const AValue: string): string;
var
  LItems: TArray<string>;
  LIndex: Integer;
begin
  LItems := SplitList(AValue);
  for LIndex := Low(LItems) to High(LItems) do
    LItems[LIndex] := QuotedPascalString(LItems[LIndex]);

  Result := '[' + string.Join(', ', LItems) + ']';
end;

class function TMCPCodeGen.BuildUsesList(const AConfig: TMCPProjectConfig;
  const AToolsUnit: string): string;
var
  LUnits: TArray<string>;
  LUnitName: string;
begin
  // MCPConnect.MCP.Server.Api registers the standard MCP methods (initialize,
  // tools/*, resources/*, prompts/*, ...) from its initialization section, so
  // it has to be linked in even though nothing references it explicitly
  LUnits := ['MCPConnect.MCP.Server.Api', 'MCPConnect.Configuration.MCP',
    'MCPConnect.Content.Writers.RTL'];

  if AConfig.IsHttpTransport then
  begin
    case AConfig.AuthKind of
      TMCPAuthKind.StaticToken:
        LUnits := LUnits + ['MCPConnect.Configuration.Auth'];

      TMCPAuthKind.OAuth:
        LUnits := LUnits + ['MCPConnect.Configuration.Auth', 'MCPConnect.Security.Token'];
    end;
  end;

  if AConfig.CreateSampleUnit and not AToolsUnit.IsEmpty then
    LUnits := LUnits + [AToolsUnit];

  Result := '';
  for LUnitName in LUnits do
  begin
    if not Result.IsEmpty then
      Result := Result + ',' + sLineBreak;
    Result := Result + '  ' + LUnitName;
  end;
  Result := Result + ';';
end;

class function TMCPCodeGen.BuildConfigCode(const AConfig: TMCPProjectConfig;
  const AReceiver: string; AIndent: Integer): string;
var
  LCode: TStringBuilder;
  LScope: string;
begin
  LCode := TStringBuilder.Create;
  try
    LCode.AppendLine(AReceiver);

    if AConfig.IsHttpTransport then
    begin
      case AConfig.AuthKind of
        TMCPAuthKind.None:
          ;

        TMCPAuthKind.StaticToken:
          begin
            LCode.AppendLine('  .Plugin.Configure<IAuthTokenConfig>');
            LCode.AppendLine('    .SetToken(' + QuotedPascalString(AConfig.TokenValue) + ')');
            LCode.AppendLine('    .SetTokenLocation(' + TokenLocationLiteral(AConfig.TokenLocation) + ')');
            if AConfig.TokenNeedsCustomHeader then
              LCode.AppendLine('    .SetTokenCustomHeader(' + QuotedPascalString(AConfig.TokenCustomHeader) + ')');
            LCode.AppendLine('  .ApplyConfig');
            LCode.AppendLine('');
          end;

        TMCPAuthKind.OAuth:
          begin
            LCode.AppendLine('  .Plugin.Configure<IOAuthConfig>');
            LCode.AppendLine('    .SetResource(' + QuotedPascalString(AConfig.OAuthResource) + ')');
            LCode.AppendLine('    .AddAuthorizationServer(' + QuotedPascalString(AConfig.OAuthAuthServer) + ')');
            LCode.AppendLine('    .AddTrustedIssuer(' + QuotedPascalString(AConfig.OAuthTrustedIssuer) + ')');
            // Switch to TJoseTokenValidator (MCPConnect.Security.Token.JOSE) to also
            // verify the token signature against the JWKS of the issuer
            LCode.AppendLine('    .SetTokenValidatorClass(TClaimsTokenValidator)');
            for LScope in SplitList(AConfig.OAuthScopes) do
              LCode.AppendLine('    .AddScopesSupported(' + QuotedPascalString(LScope) + ')');
            LCode.AppendLine('  .ApplyConfig');
            LCode.AppendLine('');
          end;
      else
        Assert(False, 'Unhandled MCP authentication kind');
      end;
    end;

    LCode.AppendLine('  .Plugin.Configure<IMCPConfig>');
    LCode.AppendLine('    .Server');
    LCode.AppendLine('      .SetName(' + QuotedPascalString(AConfig.ServerName) + ')');
    LCode.AppendLine('      .SetVersion(' + QuotedPascalString(AConfig.ServerVersion) + ')');
    LCode.AppendLine('      .RegisterWriter(TMCPStreamWriter)');
    LCode.AppendLine('      .RegisterWriter(TMCPStringListWriter)');
    LCode.AppendLine('    .BackToMCP');
    LCode.AppendLine('');

    if AConfig.UseCORS and AConfig.IsHttpTransport then
    begin
      LCode.AppendLine('    .Security');
      LCode.AppendLine('      .SetCORS(True)');
      LCode.AppendLine('      .SetAllowedMethods(' + ArrayLiteral(AConfig.CORSAllowedMethods) + ')');
      if not AConfig.CORSAllowedOrigins.Trim.IsEmpty then
        LCode.AppendLine('      .SetAllowedOrigins(' + ArrayLiteral(AConfig.CORSAllowedOrigins) + ')');
      LCode.AppendLine('    .BackToMCP');
      LCode.AppendLine('');
    end;

    if AConfig.CreateSampleUnit then
    begin
      LCode.AppendLine('    .Tools');
      LCode.AppendLine('      .RegisterClass(' + SampleToolClass + ')');
      LCode.AppendLine('    .BackToMCP');
      LCode.AppendLine('');
      LCode.AppendLine('    .Resources');
      LCode.AppendLine('      .RegisterClass(' + SampleResourceClass + ')');
      LCode.AppendLine('    .BackToMCP');
      LCode.AppendLine('');
      LCode.AppendLine('    .Prompts');
      LCode.AppendLine('      .RegisterClass(' + SamplePromptsClass + ')');
      LCode.AppendLine('    .BackToMCP');
      LCode.AppendLine('');
    end;

    // TrimRight drops the blank line left by the last section, so that the
    // closing semicolon sits right below it
    Result := LCode.ToString.TrimRight + sLineBreak + '  ;';
    Result := IndentLines(Result, AIndent).TrimRight;
  finally
    LCode.Free;
  end;
end;

end.
