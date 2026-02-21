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
program MCPConnect.Tests.Framework;

{$IFNDEF DEBUG}
{$IFNDEF TESTINSIGHT}
{$APPTYPE CONSOLE}
{$ENDIF}
{$ENDIF}
{$STRONGLINKTYPES ON}
uses
  System.SysUtils,
  {$IFDEF TESTINSIGHT}
  TestInsight.DUnitX,
  {$ENDIF }
  {$IFDEF DEBUG}
  DUnitX.Loggers.GUI.VCL,
  {$ENDIF }
  DUnitX.Loggers.Console,
  DUnitX.Loggers.Xml.NUnit,
  DUnitX.TestFramework,
  MCPConnect.Configuration.Auth in '..\Source\MCPConnect.Configuration.Auth.pas',
  MCPConnect.Configuration.Core in '..\Source\MCPConnect.Configuration.Core.pas',
  MCPConnect.Configuration.MCP in '..\Source\MCPConnect.Configuration.MCP.pas',
  MCPConnect.Configuration.Neon in '..\Source\MCPConnect.Configuration.Neon.pas',
  MCPConnect.Configuration.Session in '..\Source\MCPConnect.Configuration.Session.pas',
  MCPConnect.Core.Utils in '..\Source\MCPConnect.Core.Utils.pas',
  MCPConnect.JRPC.Core in '..\Source\MCPConnect.JRPC.Core.pas',
  MCPConnect.JRPC.Invoker in '..\Source\MCPConnect.JRPC.Invoker.pas',
  MCPConnect.JRPC.Server in '..\Source\MCPConnect.JRPC.Server.pas',
  MCPConnect.MCP.Attributes in '..\Source\MCPConnect.MCP.Attributes.pas',
  MCPConnect.MCP.Invoker in '..\Source\MCPConnect.MCP.Invoker.pas',
  MCPConnect.MCP.Prompts in '..\Source\MCPConnect.MCP.Prompts.pas',
  MCPConnect.MCP.Resources in '..\Source\MCPConnect.MCP.Resources.pas',
  MCPConnect.MCP.Server.Api in '..\Source\MCPConnect.MCP.Server.Api.pas',
  MCPConnect.MCP.Tools in '..\Source\MCPConnect.MCP.Tools.pas',
  MCPConnect.MCP.Types in '..\Source\MCPConnect.MCP.Types.pas',
  MCPConnect.Session.Core in '..\Source\MCPConnect.Session.Core.pas',
  MCPConnect.Content.Writers in '..\Source\MCPConnect.Content.Writers.pas',
  MCPConnect.Tests.JRPC.Core in 'MCPConnect.Tests.JRPC.Core.pas',
  MCPConnect.Tests.MCP.Tools in 'MCPConnect.Tests.MCP.Tools.pas',
  MCPConnect.Tests.JRPC.Invoker in 'MCPConnect.Tests.JRPC.Invoker.pas',
  MCPConnect.Tests.Session.Core in 'MCPConnect.Tests.Session.Core.pas';

var
  runner : ITestRunner;
  results : IRunResults;
  logger : ITestLogger;
  nunitLogger : ITestLogger;
begin
  ReportMemoryLeaksOnShutdown := True;
{$IFDEF TESTINSIGHT}
  TestInsight.DUnitX.RunRegisteredTests;
  Exit;
{$ENDIF}
{$IFDEF DEBUG}
  DUnitX.Loggers.GUI.VCL.Run;
  Exit;
{$ENDIF}
  try
    //Check command line options, will exit if invalid
    TDUnitX.CheckCommandLine;
    //Create the test runner
    runner := TDUnitX.CreateRunner;
    //Tell the runner to use RTTI to find Fixtures
    runner.UseRTTI := True;
    //tell the runner how we will log things
    //Log to the console window
    logger := TDUnitXConsoleLogger.Create(true);
    runner.AddLogger(logger);
    //Generate an NUnit compatible XML File
    nunitLogger := TDUnitXXMLNUnitFileLogger.Create(TDUnitX.Options.XMLOutputFile);
    runner.AddLogger(nunitLogger);
    runner.FailsOnNoAsserts := False; //When true, Assertions must be made during tests;

    //Run tests
    results := runner.Execute;
    if not results.AllPassed then
      System.ExitCode := EXIT_ERRORS;

    {$IFNDEF CI}
    //We don't want this happening when running under CI.
    if TDUnitX.Options.ExitBehavior = TDUnitXExitBehavior.Pause then
    begin
      System.Write('Done.. press <Enter> key to quit.');
      System.Readln;
    end;
    {$ENDIF}
  except
    on E: Exception do
      System.Writeln(E.ClassName, ': ', E.Message);
  end;
end.
