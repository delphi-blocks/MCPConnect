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
unit MCPConnect.Content.Writers.VCL;

interface

uses
  System.SysUtils, System.Classes, System.TypInfo, System.Rtti, System.SyncObjs, System.Generics.Collections,

  MCPConnect.MCP.Types,
  MCPConnect.MCP.Tools,
  MCPConnect.MCP.Attributes,
  MCPConnect.Content.Writers;

type
  TMCPImageWriter = class(TMCPCustomWriter)
  protected
    class function GetTargetInfo: PTypeInfo; override;
    class function CanHandle(AType: PTypeInfo): Boolean; override;
  public
    procedure WriteTool(const AValue: TValue; AContext: TMCPToolContext); override;
  end;

  TMCPPictureWriter = class(TMCPCustomWriter)
  protected
    class function GetTargetInfo: PTypeInfo; override;
    class function CanHandle(AType: PTypeInfo): Boolean; override;
  public
    procedure WriteTool(const AValue: TValue; AContext: TMCPToolContext); override;
    procedure WriteResource(const AValue: TValue; AContext: TMCPresourceContext); override;
  end;

implementation

uses
  Vcl.Graphics,
  Vcl.ExtCtrls,
  Neon.Core.Utils;

{ TMCPImageWriter }

class function TMCPImageWriter.CanHandle(AType: PTypeInfo): Boolean;
begin
  Result := TypeInfoIs(AType);
end;

class function TMCPImageWriter.GetTargetInfo: PTypeInfo;
begin
  Result := TImage.ClassInfo;
end;

procedure TMCPImageWriter.WriteTool(const AValue: TValue; AContext: TMCPToolContext);
var
  LImage: TImage;
  LStream: TMemoryStream;
  LBase64: string;
  LContent: TImageContent;
begin
  LImage := AValue.AsObject as TImage;

  LStream := TMemoryStream.Create;
  try
{$IF CompilerVersion >= 30}
    LImage.Picture.SaveToStream(LStream);
{$ELSE}
    LImage.Picture.Bitmap.SaveToStream(LStream);
{$ENDIF}
    LStream.Position := soFromBeginning;
    LBase64 := TBase64.Encode(LStream);
  finally
    LStream.Free;
  end;

  LContent := TImageContent.Create;
  LContent.Data := LBase64;
  LContent.MIMEType := 'image';
  AContext.Result.Add(LContent);
end;

{ TMCPPictureWriter }

class function TMCPPictureWriter.CanHandle(AType: PTypeInfo): Boolean;
begin
  Result := TypeInfoIs(AType);
end;

class function TMCPPictureWriter.GetTargetInfo: PTypeInfo;
begin
  Result := TPicture.ClassInfo;
end;

procedure TMCPPictureWriter.WriteResource(const AValue: TValue; AContext: TMCPresourceContext);
var
  LPicture: TPicture;
  LStream: TMemoryStream;
  LBase64: string;
  LBlob: TBlobResourceContents;
  LMCP: MCPResourceAttribute;
begin
  LPicture := AValue.AsObject as TPicture;
  LStream := TMemoryStream.Create;
  try
{$IF CompilerVersion >= 30}
    LPicture.SaveToStream(LStream);
{$ELSE}
    LPicture.Bitmap.SaveToStream(LStream);
{$ENDIF}
    LStream.Position := soFromBeginning;
    LBase64 := TBase64.Encode(LStream);
  finally
    LStream.Free;
  end;

  LBlob := TBlobResourceContents.Create;

  // Read Attributes
  LMCP := TRttiUtils.FindAttribute<MCPResourceAttribute>(AContext.Attributes);
  if Assigned(LMCP) then
    LBlob.MimeType := LMCP.MimeType
  else
    LBlob.MimeType := 'image/png';

  LBlob.Blob := LBase64;

  AContext.Result.Add(LBlob);
end;

procedure TMCPPictureWriter.WriteTool(const AValue: TValue; AContext: TMCPToolContext);
var
  LPicture: TPicture;
  LStream: TMemoryStream;
  LBase64: string;
  LContent: TImageContent;
begin
  LPicture := AValue.AsObject as TPicture;
  LStream := TMemoryStream.Create;
  try
{$IF CompilerVersion >= 30}
    LPicture.SaveToStream(LStream);
{$ELSE}
    LPicture.Bitmap.SaveToStream(LStream);
{$ENDIF}
    LStream.Position := soFromBeginning;
    LBase64 := TBase64.Encode(LStream);
  finally
    LStream.Free;
  end;

  LContent := TImageContent.Create;
  LContent.Data := LBase64;
  LContent.MIMEType := 'image';
  AContext.Result.Add(LContent);
end;

end.
