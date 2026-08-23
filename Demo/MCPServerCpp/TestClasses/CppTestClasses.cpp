//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
#include <tchar.h>
//---------------------------------------------------------------------------
USEPACKAGE("rtl.bpi");
USEPACKAGE("vcl.bpi");
USEPACKAGE("dbrtl.bpi");
USEPACKAGE("inet.bpi");
USEPACKAGE("IndySystem.bpi");
USEPACKAGE("IndyProtocols.bpi");
USEPACKAGE("IndyCore.bpi");
USEPACKAGE("Neon.bpi");
USEPACKAGE("Logify.bpi");
USEPACKAGE("JOSE.bpi");
USEPACKAGE("MCPConnect.bpi");

#pragma link "rtl.bpi"
#pragma link "vcl.bpi"
#pragma link "dbrtl.bpi"
#pragma link "inet.bpi"
#pragma link "IndySystem.bpi"
#pragma link "IndyProtocols.bpi"
#pragma link "IndyCore.bpi"
#pragma link "Neon.bpi"
#pragma link "Logify.bpi"
#pragma link "JOSE.bpi"
#pragma link "MCPConnect.bpi"
//---------------------------------------------------------------------------
USEFORM("FormMain.cpp", frmMain);
USEFORM("FormMisc.cpp", frmMisc);
USEFORM("FormSnippets.cpp", frmSnippets);
//---------------------------------------------------------------------------
#include "FormMain.h"
#include "FormMisc.h"
#include "FormSnippets.h"
//---------------------------------------------------------------------------
int WINAPI _tWinMain(HINSTANCE, HINSTANCE, LPTSTR, int)
{
	try
	{
		System::ReportMemoryLeaksOnShutdown = true;
		Application->Initialize();
		Application->MainFormOnTaskBar = true;
		Application->CreateForm(__classid(TfrmMain), &frmMain);
		Application->CreateForm(__classid(TfrmMisc), &frmMisc);
		Application->CreateForm(__classid(TfrmSnippets), &frmSnippets);
		Application->Run();
	}
	catch (Exception &exception)
	{
		Application->ShowException(&exception);
	}
	catch (...)
	{
		try
		{
			throw Exception("");
		}
		catch (Exception &exception)
		{
			Application->ShowException(&exception);
		}
	}
	return 0;
}
//---------------------------------------------------------------------------
