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
  System.SysUtils, System.Classes, System.TypInfo, System.Rtti,

  MCPConnect.MCP.Types,
  MCPConnect.MCP.Tools,
  MCPConnect.MCP.Prompts,
  MCPConnect.MCP.Attributes,
  MCPConnect.Content.Writers;

type
  TMCPGraphicWriter = class(TMCPCustomWriter)
  protected
    function ValueToBase64(AValue: TValue): string;
  public
    procedure WriteTool(const AValue: TValue; AContext: TMCPToolContext); override;
    procedure WritePrompt(const AValue: TValue; AContext: TMCPPromptContext); override;
    procedure WriteResource(const AValue: TValue; AContext: TMCPresourceContext); override;
  end;

  TMCPImageWriter = class(TMCPGraphicWriter)
  protected
    class function GetTargetInfo: PTypeInfo; override;
    class function CanHandle(AType: PTypeInfo): Boolean; override;
  end;

  TMCPPictureWriter = class(TMCPGraphicWriter)
  protected
    class function GetTargetInfo: PTypeInfo; override;
    class function CanHandle(AType: PTypeInfo): Boolean; override;
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

{ TMCPPictureWriter }

class function TMCPPictureWriter.CanHandle(AType: PTypeInfo): Boolean;
begin
  Result := TypeInfoIs(AType);
end;

class function TMCPPictureWriter.GetTargetInfo: PTypeInfo;
begin
  Result := TPicture.ClassInfo;
end;

{ TMCPGraphicWriter }

function TMCPGraphicWriter.ValueToBase64(AValue: TValue): string;
var
  LGraphic: TObject;
  LPicture: TPicture;
  LStream: TMemoryStream;
begin
  LGraphic := AValue.AsObject;

  if LGraphic is TImage then
    LPicture := (LGraphic as TImage).Picture
  else if LGraphic is TPicture then
    LPicture := LGraphic as TPicture
  else
    raise EMCPException.CreateFmt('Type %s not supported', [LGraphic.ClassName]);

  LStream := TMemoryStream.Create;
  try
{$IF CompilerVersion >= 30}
    LPicture.SaveToStream(LStream);
{$ELSE}
    LPicture.Bitmap.SaveToStream(LStream);
{$ENDIF}
    LStream.Position := soFromBeginning;
    Result := TBase64.Encode(LStream);
  finally
    LStream.Free;
  end;
end;

procedure TMCPGraphicWriter.WritePrompt(const AValue: TValue; AContext: TMCPPromptContext);
begin
  AContext.Result.Messages.AddImage('user', 'image', ValueToBase64(AValue));
end;

procedure TMCPGraphicWriter.WriteResource(const AValue: TValue; AContext: TMCPresourceContext);
var
  LBlob: TBlobResourceContents;
  LMCPAttr: MCPResourceAttribute;
begin
  LBlob := TBlobResourceContents.Create;

  // Read Attributes
  LMCPAttr := TRttiUtils.FindAttribute<MCPResourceAttribute>(AContext.Attributes);
  if Assigned(LMCPAttr) then
    LBlob.MimeType := LMCPAttr.MimeType
  else
    LBlob.MimeType := 'image/png';

  LBlob.Blob := ValueToBase64(AValue);

  AContext.Result.Contents.Add(LBlob);
end;

procedure TMCPGraphicWriter.WriteTool(const AValue: TValue; AContext: TMCPToolContext);
begin
  AContext.Result.Content.AddImage('image', ValueToBase64(AValue));
end;

end.
