 
//---------------------------------------------------------------------------
#include "MCPServerWebBrokerWebModule.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"

TComponentClass WebModuleClass = __classid(TWebModule1);
//---------------------------------------------------------------------------
__fastcall TWebModule1::TWebModule1(TComponent* Owner)
	: TWebModule(Owner)
{
    FJRPCServer = new Mcpconnect::Jrpc::Server::TJRPCServer(this);

    FJRPCDispatcher =
        new Mcpconnect::Transport::Webbroker::TJRPCDispatcher(this);

    FJRPCDispatcher->PathInfo = L"/mcp";
	FJRPCDispatcher->Server = FJRPCServer;

    auto MCPConfig =
    static_cast<Mcpconnect::Configuration::Mcp::TMCPConfig*>(
        FJRPCServer->GetConfigByClassRef(
            __classid(Mcpconnect::Configuration::Mcp::TMCPConfig)
        )
    );

	MCPConfig->Server()
	->SetName(L"cpp-mcp-webbroker-server")
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

void __fastcall TWebModule1::WebModule1DefaultHandlerAction(TObject *Sender, TWebRequest *Request,
          TWebResponse *Response, bool &Handled)
{
  Response->Content =
    "<html>"
    "<head><title>Web Server Application</title></head>"
    "<body>Web Server Application</body>"
    "</html>";
}
//---------------------------------------------------------------------------


