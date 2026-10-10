 
//---------------------------------------------------------------------------
#ifndef MCPServerWebBrokerWebModuleH
#define MCPServerWebBrokerWebModuleH
//---------------------------------------------------------------------------
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <Web.HTTPApp.hpp>

//Adicionado
#include <MCPConnect.JRPC.Server.hpp>
#include <MCPConnect.Transport.WebBroker.hpp>
#include <MCPConnect.Configuration.MCP.hpp>

#include "MCPServer.Tools.h"

//---------------------------------------------------------------------------
class TWebModule1 : public TWebModule
{
__published:	// IDE-managed Components
	void __fastcall WebModule1DefaultHandlerAction(TObject *Sender, TWebRequest *Request,
          TWebResponse *Response, bool &Handled);
private:	// User declarations

    Mcpconnect::Jrpc::Server::TJRPCServer *FJRPCServer;
	Mcpconnect::Transport::Webbroker::TJRPCDispatcher *FJRPCDispatcher;
public:		// User declarations
	__fastcall TWebModule1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TWebModule1 *WebModule1;
//---------------------------------------------------------------------------
#endif


