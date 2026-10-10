//---------------------------------------------------------------------------
#ifndef MCPServerIndyMainFormH
#define MCPServerIndyMainFormH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>


//Adicionado
#include <MCPConnect.Transport.Indy.hpp>
#include <MCPConnect.Configuration.MCP.hpp>

#include "MCPServer.Tools.h"

//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
private:	// User declarations
   Mcpconnect::Transport::Indy::TJRPCIndyServer *FServer;

public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
