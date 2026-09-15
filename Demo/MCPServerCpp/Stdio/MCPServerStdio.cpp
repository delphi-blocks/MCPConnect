#include <iostream>
#include <tchar.h>

//Adicionado ao projeto
#include <System.hpp>
#include <System.SysUtils.hpp>
#include <MCPConnect.Configuration.Core.hpp>
#include <MCPConnect.Configuration.MCP.hpp>
#include <cstdio>
#include <exception>

#include "MCPServer.Tools.h"
#include "MCPConnect.Transport.Stdio.hpp"

#pragma hdrstop

#pragma argsused


int _tmain(int argc, _TCHAR* argv[])
{
    Mcpconnect::Transport::Stdio::TJRPCStdioServer *Server = nullptr;

    try
    {
        Server =
            new Mcpconnect::Transport::Stdio::TJRPCStdioServer(nullptr);

        auto MCPConfig =
            static_cast<Mcpconnect::Configuration::Mcp::TMCPConfig*>(
                Server->JRPCServer->GetConfigByClassRef(
                    __classid(Mcpconnect::Configuration::Mcp::TMCPConfig)
                )
            );

        MCPConfig->Server()
            ->SetName(L"cpp-mcp-server")
            ->SetVersion(L"1.0.0");

        MCPConfig->Tools()
            ->RegisterTool(
                __classid(TRegisterToolTest),
                L"RandomNumber",
                L"random",
                L"Generates random numbers within a specified range",
                L""
            )
            ->WithParam(
                L"AMax",
                L"range",
                L"Range parameter for Random"
            )
            ->EndTool();

        MCPConfig->ApplyConfig();

        Server->StartServerAndWait();

        delete Server;
        Server = nullptr;
    }
    catch (const System::Sysutils::Exception &E)
    {
        fprintf(
            stderr,
            "Delphi exception: %ls\n",
            E.Message.c_str()
        );

        delete Server;
        return 1;
    }
    catch (const std::exception &E)
    {
        fprintf(
            stderr,
            "C++ exception: %s\n",
            E.what()
        );

        delete Server;
        return 2;
    }
    catch (...)
    {
        fprintf(
            stderr,
            "Unknown exception\n"
        );

        delete Server;
        return 3;
    }

    return 0;
}
