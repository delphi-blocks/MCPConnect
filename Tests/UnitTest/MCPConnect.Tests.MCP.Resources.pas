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
unit MCPConnect.Tests.MCP.Resources;

interface

uses
  System.SysUtils, System.IOUtils, System.JSON,
  DUnitX.TestFramework,

  Neon.Core.Persistence.JSON,

  MCPConnect.JRPC.Server,
  MCPConnect.Configuration.MCP,
  MCPConnect.MCP.Attributes,
  MCPConnect.MCP.Types.Base,
  MCPConnect.MCP.Types.Resources;

type
  TTitledResources = class
  public
    [McpResource('report', 'res://report', 'text/plain', 'A report', 'title=Quarterly Report')]
    function Report: string;

    [McpResource('plain', 'res://plain', 'text/plain', 'No title tag')]
    function Plain: string;

    [McpResource('hidden', 'res://hidden', 'text/plain', 'Not listed', 'disabled')]
    function Hidden: string;

    [McpTemplate('file', 'file:///{path}', 'text/plain', 'A file', 'title=Any File')]
    function GetFile([McpParam('path', 'The path')] const path: string): string;

    function Manual: string;
  end;

  /// <summary>
  ///   Resource "title" and "size", the two fields 2026-07-28 adds to Resource.
  /// </summary>
  [TestFixture]
  TMCPResourceFieldsTest = class(TObject)
  private
    FServer: TJRPCServer;
    FConfig: IMCPConfig;
    FBasePath: string;

    function ListResources: TJSONArray;
    function ListTemplates: TJSONArray;
    function FindByUri(AList: TJSONArray; const AUri: string): TJSONObject;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestTitleFromAttributeTag;
    [Test]
    procedure TestTitleAbsentWhenNotTagged;
    [Test]
    procedure TestTitleOnTemplateFromAttributeTag;
    [Test]
    procedure TestTitleFromProgrammaticRegistration;
    [Test]
    procedure TestDisabledTagWorksOnTheAttributePath;

    [Test]
    procedure TestStaticFileReportsItsSize;
    [Test]
    procedure TestSizeAbsentWhenUnknown;
  end;

implementation

{ TTitledResources }

function TTitledResources.Report: string;
begin
  Result := 'r';
end;

function TTitledResources.Plain: string;
begin
  Result := 'p';
end;

function TTitledResources.Hidden: string;
begin
  Result := 'h';
end;

function TTitledResources.GetFile(const path: string): string;
begin
  Result := path;
end;

function TTitledResources.Manual: string;
begin
  Result := 'm';
end;

{ TMCPResourceFieldsTest }

procedure TMCPResourceFieldsTest.Setup;
begin
  FBasePath := TPath.Combine(TPath.GetTempPath, 'mcpconnect-restest');
  TDirectory.CreateDirectory(FBasePath);
  TFile.WriteAllText(TPath.Combine(FBasePath, 'readme.md'), 'hello');

  FServer := TJRPCServer.Create(nil);
  FConfig := FServer.Plugin.Configure<IMCPConfig>;
  FConfig.Resources
    .SetBasePath(FBasePath)
    .RegisterClass(TTitledResources)
    .RegisterFile('readme.md', 'The readme')
    .RegisterResource(TTitledResources, 'Manual', 'manual', 'res://manual',
      'text/plain', 'The manual', 'title=User Manual');
end;

procedure TMCPResourceFieldsTest.TearDown;
begin
  FConfig := nil;
  FServer.Free;

  TFile.Delete(TPath.Combine(FBasePath, 'readme.md'));
  TDirectory.Delete(FBasePath);
end;

function TMCPResourceFieldsTest.ListResources: TJSONArray;
var
  LResult: TListResourcesResult;
  LJson: TJSONObject;
begin
  LResult := TListResourcesResult.Create;
  try
    FServer.GetConfiguration<TMCPConfig>.Resources.ResourceList(LResult);

    LJson := TNeon.ObjectToJSON(LResult, MCPNeonConfig) as TJSONObject;
    try
      Result := LJson.GetValue('resources').Clone as TJSONArray;
    finally
      LJson.Free;
    end;
  finally
    LResult.Free;
  end;
end;

function TMCPResourceFieldsTest.ListTemplates: TJSONArray;
var
  LResult: TListResourceTemplatesResult;
  LJson: TJSONObject;
begin
  LResult := TListResourceTemplatesResult.Create;
  try
    FServer.GetConfiguration<TMCPConfig>.Resources.TemplateList(LResult);

    LJson := TNeon.ObjectToJSON(LResult, MCPNeonConfig) as TJSONObject;
    try
      Result := LJson.GetValue('resourceTemplates').Clone as TJSONArray;
    finally
      LJson.Free;
    end;
  finally
    LResult.Free;
  end;
end;

function TMCPResourceFieldsTest.FindByUri(AList: TJSONArray; const AUri: string): TJSONObject;
var
  LItem: TJSONValue;
  LEntry: TJSONObject;
begin
  for LItem in AList do
  begin
    LEntry := LItem as TJSONObject;
    if (LEntry.GetValue<string>('uri', '') = AUri) or
       (LEntry.GetValue<string>('uriTemplate', '') = AUri) then
      Exit(LEntry);
  end;

  Result := nil;
end;

procedure TMCPResourceFieldsTest.TestTitleFromAttributeTag;
var
  LList: TJSONArray;
begin
  LList := ListResources;
  try
    Assert.AreEqual('Quarterly Report',
      FindByUri(LList, 'res://report').GetValue<string>('title'));
  finally
    LList.Free;
  end;
end;

procedure TMCPResourceFieldsTest.TestTitleAbsentWhenNotTagged;
var
  LList: TJSONArray;
begin
  LList := ListResources;
  try
    // An untagged resource must not emit an empty title: clients fall back to name
    Assert.IsNull(FindByUri(LList, 'res://plain').GetValue('title'));
  finally
    LList.Free;
  end;
end;

procedure TMCPResourceFieldsTest.TestTitleOnTemplateFromAttributeTag;
var
  LList: TJSONArray;
begin
  LList := ListTemplates;
  try
    Assert.AreEqual('Any File',
      FindByUri(LList, 'file:///{path}').GetValue<string>('title'));
  finally
    LList.Free;
  end;
end;

procedure TMCPResourceFieldsTest.TestTitleFromProgrammaticRegistration;
var
  LList: TJSONArray;
begin
  LList := ListResources;
  try
    Assert.AreEqual('User Manual',
      FindByUri(LList, 'res://manual').GetValue<string>('title'));
  finally
    LList.Free;
  end;
end;

procedure TMCPResourceFieldsTest.TestDisabledTagWorksOnTheAttributePath;
var
  LList: TJSONArray;
begin
  // The attribute path parsed no tags at all before, so "disabled" was ignored
  LList := ListResources;
  try
    Assert.IsNull(FindByUri(LList, 'res://hidden'), 'A disabled resource must not be listed');
  finally
    LList.Free;
  end;
end;

procedure TMCPResourceFieldsTest.TestStaticFileReportsItsSize;
var
  LList: TJSONArray;
  LEntry: TJSONObject;
begin
  LList := ListResources;
  try
    LEntry := FindByUri(LList, 'res://readme.md');
    Assert.IsNotNull(LEntry, 'The static file should be listed');
    Assert.AreEqual(5, LEntry.GetValue<Integer>('size'), 'readme.md holds "hello"');
  finally
    LList.Free;
  end;
end;

procedure TMCPResourceFieldsTest.TestSizeAbsentWhenUnknown;
var
  LList: TJSONArray;
begin
  LList := ListResources;
  try
    // A method-backed resource has no size until it is read, and 0 is a value
    // a client would read as "empty file"
    Assert.IsNull(FindByUri(LList, 'res://report').GetValue('size'));
  finally
    LList.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TMCPResourceFieldsTest);

end.
