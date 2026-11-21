//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "MCPServerIndyMainForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
   FServer =
        Mcpconnect::Transport::Indy::TJRPCIndyServer::CreateMCPServer(this);

    FServer->DefaultPort = 8080;
	FServer->Active = true;

    auto MCPConfig =
    static_cast<Mcpconnect::Configuration::Mcp::TMCPConfig*>(
        FServer->JRPCServer->GetConfigByClassRef(
            __classid(Mcpconnect::Configuration::Mcp::TMCPConfig)
        )
    );

	MCPConfig->Server()
	 ->SetName(L"cpp-mcp-indy-server")
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
}
//---------------------------------------------------------------------------
