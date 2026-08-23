//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "FormMisc.h"
#include "CppTestClassesBridge.hpp"
#include <MCPConnect.MCP.Types.hpp>
#include <MCPConnect.JRPC.Classes.hpp>
#include <System.RegularExpressions.hpp>
#include <System.SysUtils.hpp>

//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TfrmMisc *frmMisc = NULL;
//---------------------------------------------------------------------------
__fastcall TfrmMisc::TfrmMisc(TComponent* Owner)
	: TForm(Owner), FCount(0), FDelphi(NULL), FMCP(NULL)
{
}
//---------------------------------------------------------------------------
void __fastcall TfrmMisc::FormCreate(TObject *Sender)
{
	FCount = 0;
	FDelphi = Cpptestclassesbridge::CreateIntegerQueue(5);
	FMCP = Cpptestclassesbridge::CreateMCPQueue(5);
}
//---------------------------------------------------------------------------
void __fastcall TfrmMisc::FormDestroy(TObject *Sender)
{
	Cpptestclassesbridge::FreeIntegerQueue(FDelphi);
	FDelphi = NULL;
	Cpptestclassesbridge::FreeMCPQueue(FMCP);
	FMCP = NULL;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMisc::btnMatchesClick(TObject *Sender)
{
	System::Regularexpressions::TMatchCollection matches =
		System::Regularexpressions::TRegEx::Matches(L"demo://weather/dynamic/{city}/{celsius}", L"[^{\\}]+(?=})");

	for (int i = 0; i < matches.Count; ++i)
	{
		System::Regularexpressions::TMatch match = matches.Item[i];
		memoLog->Lines->Add(match.Value);
	}
}
//---------------------------------------------------------------------------
void __fastcall TfrmMisc::btnDelphiQueueClick(TObject *Sender)
{
	memoLog->Lines->Add(Format(L"Enqueing %d", ARRAYOFCONST((FCount))));
	Cpptestclassesbridge::IntegerQueueEnqueue(FDelphi, FCount);
	FCount++;

	memoLog->Lines->Add(Format(L"Queue has %d items", ARRAYOFCONST((Cpptestclassesbridge::IntegerQueueCount(FDelphi)))));
}
//---------------------------------------------------------------------------
void __fastcall TfrmMisc::btnMCPQueueClick(TObject *Sender)
{
	memoLog->Lines->Add(Format(L"Enqueing %d", ARRAYOFCONST((FCount))));

	Mcpconnect::Jrpc::Core::TJRPCNotification *n = new Mcpconnect::Jrpc::Core::TJRPCNotification();
	n->InternalId = FCount;
	n->Method = L"notification/log";
	n->AddNamedParam(L"level", System::Rtti::TValue::_op_Implicit(System::UnicodeString(L"Debug")));
	Cpptestclassesbridge::MCPQueueEnqueue(FMCP, n);
	FCount++;

	memoLog->Lines->Add(Format(L"Queue has %d items", ARRAYOFCONST((Cpptestclassesbridge::MCPQueueCount(FMCP)))));
}
//---------------------------------------------------------------------------
void __fastcall TfrmMisc::btnPrintQueueClick(TObject *Sender)
{
	Mcpconnect::Jrpc::Core::TJRPCNotification *n = Cpptestclassesbridge::MCPQueueDequeue(FMCP);
	if (!n)
	{
		memoLog->Lines->Add(L"Nothing to deque");
		return;
	}

	memoLog->Lines->Add(Format(L"Dequeuing [%d]", ARRAYOFCONST((n->InternalId))));
	delete n;

	memoLog->Lines->Add(Format(L"Queue has %d items", ARRAYOFCONST((Cpptestclassesbridge::MCPQueueCount(FMCP)))));
}
//---------------------------------------------------------------------------
void __fastcall TfrmMisc::Button2Click(TObject *Sender)
{
	System::UnicodeString tpl = edtTemplate->Text;
	memoLog->Lines->Add(tpl);

	System::UnicodeString uri = edtURI->Text;
	memoLog->Lines->Add(uri);

	Mcpconnect::Jrpc::Classes::TRouteMatcher *router = new Mcpconnect::Jrpc::Classes::TRouteMatcher();
	try
	{
		if (router->Match(tpl, uri))
		{
			Mcpconnect::Mcp::Types::TStringMap params = router->Params->ToArray();
			for (int i = 0; i < params.Length; ++i)
			{
				memoLog->Lines->Add(params[i].Key + L": " + params[i].Value);
			}
		}
		else
		{
			memoLog->Lines->Add(L"URI is not a match for the template");
		}
	}
	__finally
	{
		delete router;
	}
}
//---------------------------------------------------------------------------
