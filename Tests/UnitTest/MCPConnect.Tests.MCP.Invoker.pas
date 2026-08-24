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
unit MCPConnect.Tests.MCP.Invoker;

interface

uses
  System.SysUtils, System.JSON, System.Rtti, System.Generics.Collections,
  DUnitX.TestFramework,

  MCPConnect.JRPC.Core,
  MCPConnect.Core.Utils,
  MCPConnect.MCP.Invoker,
  MCPConnect.MCP.Tools,
  MCPConnect.MCP.Types,
  MCPConnect.MCP.Attributes,
  MCPConnect.Configuration.MCP;

type
  // Test record for complex parameters
  TPersonInfo = record
    Name: string;
    Age: Integer;
  end;

  // Test helper class with MCP-annotated methods
  TTestMCPService = class
  public
    [McpTool('echo', 'Echo back the input')]
    function Echo(
      [McpParam('message', 'Message to echo')] const AMessage: string
    ): string;

    [McpTool('add', 'Add two numbers')]
    function Add(
      [McpParam('a', 'First number')] AFirst: Integer;
      [McpParam('b', 'Second number')] ASecond: Integer
    ): Integer;

    [McpTool('multiply', 'Multiply two numbers')]
    function Multiply(
      [McpParam('x', 'First number')] AX: Double;
      [McpParam('y', 'Second number')] AY: Double
    ): Double;

    [McpTool('get_info', 'Get service info')]
    function GetInfo: string;

    [McpTool('get_array', 'Return an array of integers')]
    function GetArray: TArray<Integer>;

    [McpTool('get_person', 'Get person info', 'embedded')]
    function GetPerson(
      [McpParam('name', 'Person name')] const AName: string;
      [McpParam('age', 'Person age')] AAge: Integer
    ): TPersonInfo;

    [McpTool('error_test', 'Test error handling')]
    function ErrorTest: string;
  end;

implementation

{ TTestMCPService }

function TTestMCPService.Echo(const AMessage: string): string;
begin
  Result := AMessage;
end;

function TTestMCPService.Add(AFirst, ASecond: Integer): Integer;
begin
  Result := AFirst + ASecond;
end;

function TTestMCPService.Multiply(AX, AY: Double): Double;
begin
  Result := AX * AY;
end;

function TTestMCPService.GetInfo: string;
begin
  Result := 'TestMCPService v1.0';
end;

function TTestMCPService.GetArray: TArray<Integer>;
begin
  Result := [1, 2, 3, 4, 5];
end;

function TTestMCPService.GetPerson(const AName: string; AAge: Integer): TPersonInfo;
begin
  Result.Name := AName;
  Result.Age := AAge;
end;

function TTestMCPService.ErrorTest: string;
begin
  raise Exception.Create('Test error');
end;

end.
