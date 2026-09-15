 
//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "MCPServerWebBrokerMainForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm1::ButtonStartClick(TObject *Sender)
{
  StartServer();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::ButtonStopClick(TObject *Sender)
{
  FServer->Active = false;
  FServer->Bindings->Clear();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::ButtonOpenBrowserClick(TObject *Sender)
{
  StartServer();
#if defined(_Windows)
  String url;
  url.sprintf(L"http://localhost:%s", EditPort->Text.c_str());
  ShellExecuteW(0,
        NULL,
        url.c_str(),
        NULL, NULL, SW_SHOWNOACTIVATE);
#endif
}
//---------------------------------------------------------------------------
void __fastcall TForm1::ApplicationEvents1Idle(TObject *Sender, bool &Done)
{
  ButtonStart->Enabled = !FServer->Active;
  ButtonStop->Enabled = FServer->Active;
  EditPort->Enabled = !FServer->Active;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::StartServer()
{
  if (!FServer->Active)
  {
    FServer->Bindings->Clear();
    FServer->DefaultPort = StrToInt(EditPort->Text);
    FServer->Active = true;
  }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormCreate(TObject *Sender)
{
  FServer = new TIdHTTPWebBrokerBridge(this);
}
//---------------------------------------------------------------------------


