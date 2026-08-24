//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
#include <tchar.h>
//---------------------------------------------------------------------------
// Keep the runtime-package dependency closure in source. C++Builder may
// regenerate LinkPackageImports in the .cbproj when the active platform
// changes. USEPACKAGE exposes the dependencies to the IDE; pragma link is
// the compiler-level fallback used by both Win32 and Win64x linkers.
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
USEFORM("FormMain.cpp", FormMain);
//---------------------------------------------------------------------------
int WINAPI _tWinMain(HINSTANCE, HINSTANCE, LPTSTR, int)
{
	try
	{
		Application->Initialize();
		Application->MainFormOnTaskBar = true;
		Application->CreateForm(__classid(TFormMain), &FormMain);
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
