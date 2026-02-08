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
  protected
    class function GetTargetInfo: PTypeInfo; override;
    class function CanHandle(AType: PTypeInfo): Boolean; override;
  public
    procedure WriteTool(const AValue: TValue; AContext: TMCPToolContext); override;
    procedure WriteResource(const AValue: TValue; AContext: TMCPresourceContext); override;
  end;

  TMCPStreamWriter = class(TMCPCustomWriter)
  protected
    class function GetTargetInfo: PTypeInfo; override;
    class function CanHandle(AType: PTypeInfo): Boolean; override;
  public
    procedure WriteTool(const AValue: TValue; AContext: TMCPToolContext); override;
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

procedure TMCPStringListWriter.WriteResource(const AValue: TValue; AContext: TMCPresourceContext);
var
  LList: TStringList;
  LContent: TTextResourceContents;
  LMCP: MCPResourceAttribute;
begin
  LList := AValue.AsObject as TStringList;

  LContent := TTextResourceContents.Create;

  // Read Attributes
  LMCP := TRttiUtils.FindAttribute<MCPResourceAttribute>(AContext.Attributes);
  if Assigned(LMCP) then
    LContent.MimeType := LMCP.MimeType
  else
    LContent.MimeType := 'text/plain';

  LContent.Text := LList.CommaText;
  AContext.Result.Add(LContent);
end;

procedure TMCPStringListWriter.WriteTool(const AValue: TValue; AContext: TMCPToolContext);
var
  LList: TStringList;
  LContent: TTextContent;
begin
  LList := AValue.AsObject as TStringList;

  LContent := TTextContent.Create;
  LContent.Text := LList.CommaText;
  AContext.Result.Add(LContent);
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
  AContext.Result.Add(LBlob);
end;

procedure TMCPStreamWriter.WriteTool(const AValue: TValue; AContext: TMCPToolContext);
var
  LStream: TStream;
  LBase64: string;
  LBlob: TEmbeddedResourceBlob;
begin
  LStream := AValue.AsObject as TStream;
  LStream.Position := soFromBeginning;
  LBase64 := TBase64.Encode(LStream);

  LBlob := TEmbeddedResourceBlob.Create;
  LBlob.Resource.MIMEType := 'application/octect-stream';
  LBlob.Resource.Blob := LBase64;
  AContext.Result.Add(LBlob);
end;

end.
