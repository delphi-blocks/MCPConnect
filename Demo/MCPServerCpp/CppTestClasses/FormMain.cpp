//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "FormMain.h"
#include "FormSnippets.h"
#include "FormMisc.h"
#include "PersonEntity.h"
#include "CppTestClassesBridge.hpp"

#include <MCPConnect.JRPC.Classes.hpp>
#include <MCPConnect.MCP.Types.hpp>
#include <MCPConnect.MCP.Tools.hpp>
#include <MCPConnect.MCP.Resources.hpp>
#include <MCPConnect.JRPC.Core.hpp>

#include <Neon.Core.Tags.hpp>
#include <Neon.Core.Persistence.hpp>
#include <Neon.Core.Serializers.RTL.hpp>
#include <Neon.Core.Persistence.JSON.hpp>

#include <System.JSON.hpp>

namespace
{
	System::Rtti::TValue IntegerValue(int value)
	{
		return System::Rtti::TValue::_op_Implicit(value);
	}

	System::Rtti::TValue StringValue(const System::UnicodeString &value)
	{
		return System::Rtti::TValue::_op_Implicit(value);
	}

	System::Rtti::TValue DateTimeValue(const System::TDateTime value)
	{
		return System::Rtti::TValue::_op_Implicit(value);
	}

	bool IsToolMethod(const System::UnicodeString &methodName)
	{
		static const wchar_t *ToolMethodNames[] = {
			L"TestParam",
			L"TestFunc",
			L"DoubleValue",
			L"Sub",
			L"GetPersonName",
			L"CreatePerson"
		};

		for (unsigned int i = 0; i < sizeof(ToolMethodNames) / sizeof(ToolMethodNames[0]); ++i)
		{
			if (methodName == ToolMethodNames[i])
				return true;
		}
		return false;
	}
}

//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TfrmMain *frmMain = NULL;
//---------------------------------------------------------------------------
__fastcall TfrmMain::TfrmMain(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::FormCreate(TObject *Sender)
{
	ctx = System::Rtti::TRttiContext::Create();
	FilterTools();
#ifdef _DEBUG
	if (tools.Length != 6)
		throw Exception(Format(L"Expected 6 MCP tool methods in RTTI, found %d",
			ARRAYOFCONST((tools.Length))));
#endif
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::FilterTools()
{
	tools.Length = 0;
	System::Rtti::TRttiType *typ = ctx.GetType(__classid(TfrmMain));
	if (typ)
	{
		DynamicArray<System::Rtti::TRttiMethod*> methods = typ->GetMethods();
		for (int i = 0; i < methods.Length; ++i)
		{
			if (IsToolMethod(methods[i]->Name))
			{
				tools.Length = tools.Length + 1;
				tools[tools.Length - 1] = methods[i];
			}
		}
	}
}
//---------------------------------------------------------------------------
Neon::Core::Persistence::_di_INeonConfiguration __fastcall TfrmMain::GetNeonConfig()
{
	return Neon::Core::Persistence::TNeonConfiguration::Default()
		->RegisterSerializer(__classid(Neon::Core::Serializers::Rtl::TTValueSerializer))
		->RegisterSerializer(__classid(Mcpconnect::Jrpc::Core::TJRequestSerializer));
}
//---------------------------------------------------------------------------
Neon::Core::Persistence::_di_INeonConfiguration __fastcall TfrmMain::GetMCPNeonConfig()
{
	return Mcpconnect::Mcp::Types::MCPNeonConfig()->SetPrettyPrint(true);
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::actRequestPosExecute(TObject *Sender)
{
	Mcpconnect::Jrpc::Core::TJRPCRequest *r = new Mcpconnect::Jrpc::Core::TJRPCRequest();
	try
	{
		r->Id = 1;
		r->Method = L"TestParam";
		r->AddPositionParam(IntegerValue(12));
		r->AddPositionParam(IntegerValue(23));

		mmoLog->Lines->Add(r->ToJson());
	}
	__finally
	{
		delete r;
	}
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::actRequestDesExecute(TObject *Sender)
{
	Mcpconnect::Jrpc::Core::TJRPCRequest *r = Mcpconnect::Jrpc::Core::TJRPCRequest::CreateFromJson(mmoLog->Lines->Text);
	try
	{
		mmoLog->Lines->Add(L"method name: " + r->Method);
		mmoLog->Lines->Add(L"param count: " + IntToStr(r->ParamsCount()));
	}
	__finally
	{
		delete r;
	}
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::actRequestNamedExecute(TObject *Sender)
{
	Mcpconnect::Jrpc::Core::TJRPCRequest *r = new Mcpconnect::Jrpc::Core::TJRPCRequest();
	try
	{
		r->Id = 1;
		r->Method = L"sum";
		r->AddNamedParam(L"first", IntegerValue(12));
		r->AddNamedParam(L"second", StringValue(L"Paolo Rossi"));

		mmoLog->Lines->Add(r->ToJson());
	}
	__finally
	{
		delete r;
	}
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::actResponseExecute(TObject *Sender)
{
	Mcpconnect::Jrpc::Core::TJRPCResponse *res = new Mcpconnect::Jrpc::Core::TJRPCResponse();
	try
	{
		TStringList *list = new TStringList();
		try
		{
			list->Add(L"Paolo");
			list->Add(L"Rossi");
			res->Result = Neon::Core::Persistence::Json::TNeon::ObjectToJSON(list);
		}
		__finally
		{
			delete list;
		}

		mmoLog->Lines->Add(res->ToJson());
	}
	__finally
	{
		delete res;
	}
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::actResponseDesExecute(TObject *Sender)
{
	Mcpconnect::Jrpc::Core::TJRPCResponse *res = Mcpconnect::Jrpc::Core::TJRPCResponse::CreateFromJson(mmoLog->Lines->Text);
	try
	{
		if (res->Result)
		{
			mmoLog->Lines->Add(L"Result (as JSON) is a: " + res->Result->ClassName());
		}
	}
	__finally
	{
		delete res;
	}
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::actRttiCallExecute(TObject *Sender)
{
	// The Delphi sample keeps the experimental invoker implementation disabled.
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::actMessagesExecute(TObject *Sender)
{
	Mcpconnect::Jrpc::Core::TJRPCMessages *msg = new Mcpconnect::Jrpc::Core::TJRPCMessages(true);
	try
	{
		Mcpconnect::Jrpc::Core::TJRPCNotification *nt = new Mcpconnect::Jrpc::Core::TJRPCNotification();
		nt->Method = L"hello-notification";
		nt->AddNamedParam(L"max", IntegerValue(12));
		msg->AddMessage(nt);

		Mcpconnect::Jrpc::Core::TJRPCRequest *rq = new Mcpconnect::Jrpc::Core::TJRPCRequest();
		rq->Id = 11;
		rq->Method = L"get-age";
		rq->AddNamedParam(L"name", StringValue(L"Paolo"));
		msg->AddMessage(rq);

		Mcpconnect::Jrpc::Core::TJRPCResponse *rs = new Mcpconnect::Jrpc::Core::TJRPCResponse();
		rs->Id = 11;
		TJSONObject *obj = new TJSONObject();
		obj->AddPair(L"age", 55);
		rs->Result = obj;
		msg->AddMessage(rs);

		Mcpconnect::Jrpc::Core::TJRPCError *er = new Mcpconnect::Jrpc::Core::TJRPCError();
		Cpptestclassesbridge::SetJRPCErrorDetails(er->Error, 1233, L"Error");
		msg->AddMessage(er);

		mmoLog->Lines->Add(msg->ToJson());
	}
	__finally
	{
		delete msg;
	}
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::actMessagesReadExecute(TObject *Sender)
{
	Mcpconnect::Jrpc::Core::TJRPCMessages *msgs = new Mcpconnect::Jrpc::Core::TJRPCMessages(true);
	try
	{
		msgs->FromJson(mmoLog->Lines->Text);
		mmoLog->Lines->Add(L"Count: " + IntToStr((int)msgs->Count));
		mmoLog->Lines->Add(L"---------------");
		for (int i = 0; i < msgs->List->Count; ++i)
		{
			Mcpconnect::Jrpc::Core::TJRPCMessage *m = msgs->List->Items[i];
			mmoLog->Lines->Add(L"Message: " + m->ClassName());
			if (m->GetType() == Mcpconnect::Jrpc::Core::TJRPCMessageType::Request)
			{
				Mcpconnect::Jrpc::Core::TJRPCRequest *req = static_cast<Mcpconnect::Jrpc::Core::TJRPCRequest*>(m);
				mmoLog->Lines->Add(L"Request Method: " + req->Method);
			}
			else if (m->GetType() == Mcpconnect::Jrpc::Core::TJRPCMessageType::Notification)
			{
				Mcpconnect::Jrpc::Core::TJRPCNotification *notif = static_cast<Mcpconnect::Jrpc::Core::TJRPCNotification*>(m);
				mmoLog->Lines->Add(L"Notification Method: " + notif->Method);
			}
			else if (m->GetType() == Mcpconnect::Jrpc::Core::TJRPCMessageType::Response)
			{
				Mcpconnect::Jrpc::Core::TJRPCResponse *resp = static_cast<Mcpconnect::Jrpc::Core::TJRPCResponse*>(m);
				if (resp->Result)
					mmoLog->Lines->Add(L"Result: " + resp->Result->ToJSON());
			}
			mmoLog->Lines->Add(L"---------------");
		}
	}
	__finally
	{
		delete msgs;
	}
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::actJRPCIDExecute(TObject *Sender)
{
	Mcpconnect::Jrpc::Core::TJRPCError *tpl = new Mcpconnect::Jrpc::Core::TJRPCError();
	try
	{
		Cpptestclassesbridge::SetJRPCErrorDetails(tpl->Error, 32334, L"Error");

		Mcpconnect::Jrpc::Core::TJRPCError *err = static_cast<Mcpconnect::Jrpc::Core::TJRPCError*>(tpl->Clone());
		try
		{
			System::UnicodeString s = Neon::Core::Persistence::Json::TNeon::ObjectToJSONString(err, Mcpconnect::Jrpc::Core::JRPCNeonConfig());
			mmoLog->Lines->Add(s);
		}
		__finally
		{
			delete err;
		}
	}
	__finally
	{
		delete tpl;
	}
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::InitializeResultExecute(TObject *Sender)
{
	Mcpconnect::Mcp::Types::TInitializeResult *res = new Mcpconnect::Mcp::Types::TInitializeResult();
	try
	{
		res->ProtocolVersion = L"1.0";
		Cpptestclassesbridge::SetPromptsListChanged(res, true);
		res->ServerInfo->Name = L"Server MCP";
		res->ServerInfo->Version = L"0.9";

		mmoLog->Lines->Add(res->ToJSON(true));
	}
	__finally
	{
		delete res;
	}
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::actInitializeRequestExecute(TObject *Sender)
{
	Mcpconnect::Mcp::Types::TInitializeParams *pars = new Mcpconnect::Mcp::Types::TInitializeParams();
	try
	{
		pars->ProtocolVersion = L"2025-06-18";
		pars->ClientInfo->Name = L"Delphi MCPLib";
		pars->ClientInfo->Version = L"0.8";
		Cpptestclassesbridge::SetRootsListChanged(pars, true);

		TJSONValue *j = Neon::Core::Persistence::Json::TNeon::ObjectToJSON(pars, GetMCPNeonConfig());
		mmoLog->Lines->Add(j->ToJSON());

		Mcpconnect::Jrpc::Core::TJRPCRequest *req = new Mcpconnect::Jrpc::Core::TJRPCRequest();
		try
		{
			req->Method = L"initialize";
			req->Params = j;
			mmoLog->Lines->Add(req->ToJson());
		}
		__finally
		{
			delete req;
		}
	}
	__finally
	{
		delete pars;
	}
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::actInvokeRequestExecute(TObject *Sender)
{
	// The Delphi sample keeps the experimental invoker implementation disabled.
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::actStructTagsExecute(TObject *Sender)
{
	Neon::Core::Tags::TAttributeTags *tags =
		new Neon::Core::Tags::TAttributeTags(L",", L"=", L'"');
	try
	{
		tags->Parse(L"title=Come stai??,description=sto bene,readOnly,age=12");

		System::DynamicArray<System::Generics::Collections::TPair__2<System::UnicodeString, System::UnicodeString> > pairs =
			tags->TagMap->ToArray();
		for (int i = 0; i < pairs.Length; ++i)
		{
			mmoLog->Lines->Add(L"Name: " + pairs[i].Key);
			mmoLog->Lines->Add(L"Value: " + pairs[i].Value);
			mmoLog->Lines->Add(L"----------------");
		}

		bool b = tags->GetBoolValue(L"readOnly");
		if (b)
			mmoLog->Lines->Add(L"It is True!!");
	}
	__finally
	{
		delete tags;
	}
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::actToolSingleExecute(TObject *Sender)
{
	// Intentionally empty in the Delphi TestClasses sample.
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::actToolListExecute(TObject *Sender)
{
	// Intentionally empty in the Delphi TestClasses sample.
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::actCallToolParamsExecute(TObject *Sender)
{
	Mcpconnect::Mcp::Tools::TCallToolParams *c = new Mcpconnect::Mcp::Tools::TCallToolParams();
	try
	{
		c->Name = L"Somma";
		c->Arguments->AddPair(L"arg1", Neon::Core::Persistence::Json::TNeon::ValueToJSON(IntegerValue(12)));
		c->Arguments->AddPair(L"arg2", Neon::Core::Persistence::Json::TNeon::ValueToJSON(DateTimeValue(Now())));

		System::UnicodeString s = Neon::Core::Persistence::Json::TNeon::ObjectToJSONString(c, GetMCPNeonConfig());
		mmoLog->Lines->Add(s);
		mmoLog->Lines->Add(L"------------------");

		Mcpconnect::Mcp::Tools::TCallToolParams *c2 =
			static_cast<Mcpconnect::Mcp::Tools::TCallToolParams*>(
				Neon::Core::Persistence::Json::TNeon::JSONToObject(
					ctx.GetType(__classid(Mcpconnect::Mcp::Tools::TCallToolParams)), s, GetMCPNeonConfig()));
		if (c2)
		{
			try
			{
				mmoLog->Lines->Add(L"Method: " + c2->Name);
				for (int i = 0; i < c2->Arguments->Count; ++i)
				{
					TJSONPair *arg = c2->Arguments->Pairs[i];
					mmoLog->Lines->Add(L"Argument Name: " + arg->JsonString->Value());
					mmoLog->Lines->Add(L"Argument Value: " + arg->JsonValue->Value());
					mmoLog->Lines->Add(L"--");
				}
			}
			__finally
			{
				delete c2;
			}
		}
	}
	__finally
	{
		delete c;
	}
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::actClearLogExecute(TObject *Sender)
{
	mmoLog->Clear();
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::actOpenMiscExecute(TObject *Sender)
{
	frmMisc->Show();
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::actSnippetsExecute(TObject *Sender)
{
	frmSnippets->Show();
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::actResourceExecute(TObject *Sender)
{
	Mcpconnect::Mcp::Resources::TMCPResource *res = new Mcpconnect::Mcp::Resources::TMCPResource();
	try
	{
		res->Name = L"Clients";
		res->Uri = L"/resources/clients";
		res->MimeType = System::UnicodeString(L"application/json");
		mmoLog->Lines->Add(Neon::Core::Persistence::Json::TNeon::ObjectToJSONString(res, GetMCPNeonConfig()));
	}
	__finally
	{
		delete res;
	}
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::actResourceTemplateExecute(TObject *Sender)
{
	Mcpconnect::Mcp::Resources::TMCPResourceTemplate *tpl = new Mcpconnect::Mcp::Resources::TMCPResourceTemplate();
	try
	{
		tpl->Name = L"Article Template";
		tpl->UriTemplate = System::UnicodeString(L"/templates/article");
		tpl->MimeType = System::UnicodeString(L"application/json");
		mmoLog->Lines->Add(Neon::Core::Persistence::Json::TNeon::ObjectToJSONString(tpl, GetMCPNeonConfig()));
	}
	__finally
	{
		delete tpl;
	}
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::actResourceListExecute(TObject *Sender)
{
	Mcpconnect::Mcp::Resources::TListResourcesResult *lst = new Mcpconnect::Mcp::Resources::TListResourcesResult();
	try
	{
		lst->AddResource(L"clients", L"/resources/clients", L"application/json");
		lst->AddResource(L"orders", L"/resources/orders", L"application/xml");
		mmoLog->Lines->Add(Neon::Core::Persistence::Json::TNeon::ObjectToJSONString(lst, GetMCPNeonConfig()));
	}
	__finally
	{
		delete lst;
	}
}
//---------------------------------------------------------------------------
int __fastcall TfrmMain::TestParam(__int64 AParam1, bool AParam2)
{
	return (int)(AParam1 * 2);
}
//---------------------------------------------------------------------------
System::UnicodeString __fastcall TfrmMain::TestFunc()
{
	return L"Hello World!";
}
//---------------------------------------------------------------------------
int __fastcall TfrmMain::DoubleValue(int AValue)
{
	return AValue * 2;
}
//---------------------------------------------------------------------------
int __fastcall TfrmMain::Sub(int a, int b)
{
	return a - b;
}
//---------------------------------------------------------------------------
System::UnicodeString __fastcall TfrmMain::GetPersonName(TPerson *p)
{
	if (p)
		return p->Name;
	return L"";
}
//---------------------------------------------------------------------------
TPerson* __fastcall TfrmMain::CreatePerson(const System::UnicodeString AName)
{
	return new TPerson(AName);
}
//---------------------------------------------------------------------------
