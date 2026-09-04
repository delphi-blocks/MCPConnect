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
  MCPConnect.Configuration.Auth in '..\..\Source\MCPConnect.Configuration.Auth.pas',
  MCPConnect.Configuration.Core in '..\..\Source\MCPConnect.Configuration.Core.pas',
  MCPConnect.Configuration.MCP in '..\..\Source\MCPConnect.Configuration.MCP.pas',
  MCPConnect.Configuration.Neon in '..\..\Source\MCPConnect.Configuration.Neon.pas',
  MCPConnect.Configuration.Session in '..\..\Source\MCPConnect.Configuration.Session.pas',
  MCPConnect.JRPC.Core in '..\..\Source\MCPConnect.JRPC.Core.pas',
  MCPConnect.JRPC.Invoker in '..\..\Source\MCPConnect.JRPC.Invoker.pas',
  MCPConnect.JRPC.Server in '..\..\Source\MCPConnect.JRPC.Server.pas',
  MCPConnect.MCP.Attributes in '..\..\Source\MCPConnect.MCP.Attributes.pas',
  MCPConnect.MCP.Invoker in '..\..\Source\MCPConnect.MCP.Invoker.pas',
  MCPConnect.MCP.Types.Prompts in '..\..\Source\MCPConnect.MCP.Types.Prompts.pas',
  MCPConnect.MCP.Types.Resources in '..\..\Source\MCPConnect.MCP.Types.Resources.pas',
  MCPConnect.MCP.Types.Tools in '..\..\Source\MCPConnect.MCP.Types.Tools.pas',
  MCPConnect.MCP.Types.Base in '..\..\Source\MCPConnect.MCP.Types.Base.pas',
  MCPConnect.MCP.Types.Completion in '..\..\Source\MCPConnect.MCP.Types.Completion.pas',
  MCPConnect.MCP.Types.Errors in '..\..\Source\MCPConnect.MCP.Types.Errors.pas',
  MCPConnect.MCP.Server.Api in '..\..\Source\MCPConnect.MCP.Server.Api.pas',
  MCPConnect.Security.Jwks in '..\..\Source\MCPConnect.Security.Jwks.pas',
  MCPConnect.Security.Token in '..\..\Source\MCPConnect.Security.Token.pas',
  MCPConnect.Security.Token.JOSE in '..\..\Source\MCPConnect.Security.Token.JOSE.pas',
  MCPConnect.Session.Core in '..\..\Source\MCPConnect.Session.Core.pas',
  MCPConnect.Content.Writers in '..\..\Source\MCPConnect.Content.Writers.pas',
  MCPConnect.Transport.AcceptParser in '..\..\Source\MCPConnect.Transport.AcceptParser.pas',
  MCPConnect.Transport.MediaType in '..\..\Source\MCPConnect.Transport.MediaType.pas',
  MCPConnect.Transport.Base in '..\..\Source\MCPConnect.Transport.Base.pas',
  MCPConnect.Tests.JRPC.Core in 'MCPConnect.Tests.JRPC.Core.pas',
  MCPConnect.Tests.MCP.Tools in 'MCPConnect.Tests.MCP.Tools.pas',
  MCPConnect.Tests.JRPC.Invoker in 'MCPConnect.Tests.JRPC.Invoker.pas',
  MCPConnect.Tests.Session.Core in 'MCPConnect.Tests.Session.Core.pas',
  MCPConnect.Tests.Core.Utils in 'MCPConnect.Tests.Core.Utils.pas',
  MCPConnect.Tests.MCP.Configuration in 'MCPConnect.Tests.MCP.Configuration.pas',
  MCPConnect.Tests.MCP.Server.Api in 'MCPConnect.Tests.MCP.Server.Api.pas',
  MCPConnect.Tests.Security.Token in 'MCPConnect.Tests.Security.Token.pas',
  MCPConnect.Tests.Security.Token.JOSE in 'MCPConnect.Tests.Security.Token.JOSE.pas',
  MCPConnect.Tests.Transport.OAuth in 'MCPConnect.Tests.Transport.OAuth.pas',
  MCPConnect.Tests.Transport.Headers in 'MCPConnect.Tests.Transport.Headers.pas',
  MCPConnect.Tests.MCP.Errors in 'MCPConnect.Tests.MCP.Errors.pas',
  MCPConnect.Tests.MCP.Completion in 'MCPConnect.Tests.MCP.Completion.pas';

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
