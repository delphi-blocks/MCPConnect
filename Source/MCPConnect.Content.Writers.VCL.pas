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
    procedure Write(const AValue: TValue; AContext: TMCPWriterContext); override;
  end;

  TMCPPictureWriter = class(TMCPCustomWriter)
  protected
    class function GetTargetInfo: PTypeInfo; override;
    class function CanHandle(AType: PTypeInfo): Boolean; override;
  public
    procedure Write(const AValue: TValue; AContext: TMCPWriterContext); override;
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

procedure TMCPImageWriter.Write(const AValue: TValue; AContext: TMCPWriterContext);
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
  AContext.ContentList.Add(LContent);
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

procedure TMCPPictureWriter.Write(const AValue: TValue; AContext: TMCPWriterContext);
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
  AContext.ContentList.Add(LContent);
end;

end.
