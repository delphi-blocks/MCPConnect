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
    procedure Write(const AValue: TValue; AContext: TMCPWriterContext); override;
  end;

  TMCPStreamWriter = class(TMCPCustomWriter)
  protected
    class function GetTargetInfo: PTypeInfo; override;
    class function CanHandle(AType: PTypeInfo): Boolean; override;
  public
    procedure Write(const AValue: TValue; AContext: TMCPWriterContext); override;
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

procedure TMCPStringListWriter.Write(const AValue: TValue; AContext: TMCPWriterContext);
var
  LList: TStringList;
  LContent: TTextContent;
begin
  LList := AValue.AsObject as TStringList;

  LContent := TTextContent.Create;
  LContent.Text := LList.CommaText;
  AContext.ContentList.Add(LContent);
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

procedure TMCPStreamWriter.Write(const AValue: TValue; AContext: TMCPWriterContext);
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
  AContext.ContentList.Add(LBlob);
end;

end.
