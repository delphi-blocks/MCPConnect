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
unit MCPConnect.Content.Writers.RTL;

interface

uses
  System.SysUtils, System.Classes, System.TypInfo, System.Rtti, System.Generics.Collections,

  MCPConnect.MCP.Types,
  MCPConnect.MCP.Tools,
  MCPConnect.MCP.Attributes,
  MCPConnect.Content.Writers;

type
  TMCPStringListWriter = class(TMCPCustomWriter)
  private
    function ValueAsList(const AValue: TValue): TStringList;
  protected
    class function GetTargetInfo: PTypeInfo; override;
    class function CanHandle(AType: PTypeInfo): Boolean; override;
  public
    procedure WriteTool(const AValue: TValue; AContext: TMCPToolContext); override;
    procedure WritePrompt(const AValue: TValue; AContext: TMCPPromptContext); override;
    procedure WriteResource(const AValue: TValue; AContext: TMCPresourceContext); override;
  end;

  TMCPStreamWriter = class(TMCPCustomWriter)
  private
    function StreamToBase64(AValue: TValue): string;
  protected
    class function GetTargetInfo: PTypeInfo; override;
    class function CanHandle(AType: PTypeInfo): Boolean; override;
  public
    procedure WriteTool(const AValue: TValue; AContext: TMCPToolContext); override;
    procedure WritePrompt(const AValue: TValue; AContext: TMCPPromptContext); override;
    procedure WriteResource(const AValue: TValue; AContext: TMCPresourceContext); override;
  end;

implementation

uses
  Neon.Core.Utils;

{ TMCPStringListWriter }

class function TMCPStringListWriter.CanHandle(AType: PTypeInfo): Boolean;
begin
  Result := TypeInfoIs(AType);
end;

class function TMCPStringListWriter.GetTargetInfo: PTypeInfo;
begin
  Result := TStringList.ClassInfo;
end;

function TMCPStringListWriter.ValueAsList(const AValue: TValue): TStringList;
begin
  Result := AValue.AsObject as TStringList;
end;

procedure TMCPStringListWriter.WritePrompt(const AValue: TValue; AContext: TMCPPromptContext);
begin
  AContext.Result.Messages.AddText('user', ValueAsList(AValue).CommaText);
end;

procedure TMCPStringListWriter.WriteResource(const AValue: TValue; AContext: TMCPresourceContext);
var
  LMime: string;
  LMCP: MCPResourceAttribute;
begin
  LMCP := TRttiUtils.FindAttribute<MCPResourceAttribute>(AContext.Attributes);
  if Assigned(LMCP) then
    LMime := LMCP.MimeType
  else
    LMime := 'text/plain';

  AContext.Result.Contents.AddText(LMCP.Uri, LMime, ValueAsList(AValue).CommaText);
end;

procedure TMCPStringListWriter.WriteTool(const AValue: TValue; AContext: TMCPToolContext);
begin
  AContext.Result.Content.AddText(ValueAsList(AValue).CommaText);
end;

{ TMCPStreamWriter }

class function TMCPStreamWriter.CanHandle(AType: PTypeInfo): Boolean;
begin
  Result := TypeInfoIs(AType);
end;

class function TMCPStreamWriter.GetTargetInfo: PTypeInfo;
begin
  Result := TStream.ClassInfo;
end;

function TMCPStreamWriter.StreamToBase64(AValue: TValue): string;
var
  LStream: TStream;
begin
  LStream := AValue.AsObject as TStream;
  LStream.Position := soFromBeginning;
  Result := TBase64.Encode(LStream);
end;

procedure TMCPStreamWriter.WritePrompt(const AValue: TValue; AContext: TMCPPromptContext);
begin
  AContext.Result.Messages.AddBlob('user', 'application/octect-stream', StreamToBase64(AValue));
end;

procedure TMCPStreamWriter.WriteResource(const AValue: TValue; AContext: TMCPresourceContext);
var
  LStream: TStream;
  LBase64: string;
  LBlob: TBlobResourceContents;
  LMCP: MCPResourceAttribute;
begin
  LStream := AValue.AsObject as TStream;
  LStream.Position := soFromBeginning;
  LBase64 := TBase64.Encode(LStream);

  LBlob := TBlobResourceContents.Create;

  // Read Attributes
  LMCP := TRttiUtils.FindAttribute<MCPResourceAttribute>(AContext.Attributes);
  if Assigned(LMCP) then
    LBlob.MimeType := LMCP.MimeType
  else
    LBlob.MimeType := 'application/octect-stream';

  LBlob.Blob := LBase64;
  AContext.Result.Contents.Add(LBlob);
end;

procedure TMCPStreamWriter.WriteTool(const AValue: TValue; AContext: TMCPToolContext);
begin
  AContext.Result.Content.AddBlob('application/octect-stream', StreamToBase64(AValue));
end;

end.
