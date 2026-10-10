 
//---------------------------------------------------------------------------

#ifndef MCPServerWebBrokerMainFormH
#define MCPServerWebBrokerMainFormH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.AppEvnts.hpp>
#include <IdHTTPWebBrokerBridge.hpp>
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TButton *ButtonOpenBrowser;
	TApplicationEvents *ApplicationEvents1;
	TEdit *EditPort;
	TButton *ButtonStop;
	TButton *ButtonStart;
	TLabel *Label1;
	void __fastcall ButtonStartClick(TObject *Sender);
	void __fastcall ButtonStopClick(TObject *Sender);
	void __fastcall ButtonOpenBrowserClick(TObject *Sender);
	void __fastcall ApplicationEvents1Idle(TObject *Sender, bool &Done);
	void __fastcall FormCreate(TObject *Sender);
private:	// User declarations
	TIdHTTPWebBrokerBridge *FServer;
    
	void __fastcall StartServer();
public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif


