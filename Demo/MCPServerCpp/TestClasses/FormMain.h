//---------------------------------------------------------------------------
#ifndef FormMainH
#define FormMainH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.CategoryButtons.hpp>
#include <Vcl.ActnList.hpp>
#include <Vcl.ComCtrls.hpp>
#include <Vcl.ToolWin.hpp>
#include <Vcl.ImgList.hpp>
#include <System.ImageList.hpp>
#include <System.Actions.hpp>
#include <System.Rtti.hpp>
#include <Neon.Core.Persistence.hpp>
#include "PersonEntity.h"

//---------------------------------------------------------------------------
// C++Builder emits Delphi RTTI for this class, but it cannot attach Delphi
// custom attributes such as [McpTool] and [McpParam] from C++ source.
// FilterTools therefore mirrors the six attributed Delphi methods explicitly.
// A real MCP server should use MCPConnect's programmatic RegisterTool API, as
// demonstrated by the BigQuery sample's Delphi configuration bridge.
class __declspec(delphiclass) __declspec(delphirtti) TfrmMain : public TForm
{
__published:	// IDE-managed Components
	TMemo *mmoLog;
	TPanel *Panel1;
	TCategoryButtons *CategoryButtons1;
	TActionList *actListMain;
	TAction *actMessages;
	TAction *actJRPCID;
	TAction *actRequestPos;
	TAction *actRequestNamed;
	TAction *actRequestDes;
	TAction *actResponse;
	TAction *actResponseDes;
	TAction *actToolSingle;
	TAction *actToolList;
	TAction *actInitializeRequest;
	TAction *actInitializeResult;
	TAction *actStructTags;
	TAction *actCallToolParams;
	TAction *actRttiCall;
	TAction *actInvokeRequest;
	TToolBar *tlbMain;
	TAction *actClearLog;
	TToolButton *btnClearLog;
	TImageList *ilMain;
	TAction *actResource;
	TAction *actResourceTemplate;
	TAction *actResourceList;
	TAction *actSnippets;
	TToolButton *btnSnippets;
	TAction *actMessagesRead;
	TAction *actOpenMisc;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall actRequestPosExecute(TObject *Sender);
	void __fastcall actRequestDesExecute(TObject *Sender);
	void __fastcall actRequestNamedExecute(TObject *Sender);
	void __fastcall actResponseExecute(TObject *Sender);
	void __fastcall actResponseDesExecute(TObject *Sender);
	void __fastcall actMessagesExecute(TObject *Sender);
	void __fastcall actJRPCIDExecute(TObject *Sender);
	void __fastcall InitializeResultExecute(TObject *Sender);
	void __fastcall actInitializeRequestExecute(TObject *Sender);
	void __fastcall actRttiCallExecute(TObject *Sender);
	void __fastcall actToolSingleExecute(TObject *Sender);
	void __fastcall actInvokeRequestExecute(TObject *Sender);
	void __fastcall actStructTagsExecute(TObject *Sender);
	void __fastcall actToolListExecute(TObject *Sender);
	void __fastcall actCallToolParamsExecute(TObject *Sender);
	void __fastcall actClearLogExecute(TObject *Sender);
	void __fastcall actMessagesReadExecute(TObject *Sender);
	void __fastcall actOpenMiscExecute(TObject *Sender);
	void __fastcall actResourceExecute(TObject *Sender);
	void __fastcall actResourceListExecute(TObject *Sender);
	void __fastcall actResourceTemplateExecute(TObject *Sender);
	void __fastcall actSnippetsExecute(TObject *Sender);
private:	// User declarations
	System::Rtti::TRttiContext ctx;
	DynamicArray<System::Rtti::TRttiMethod*> tools;
	void __fastcall FilterTools();
	Neon::Core::Persistence::_di_INeonConfiguration __fastcall GetNeonConfig();
	Neon::Core::Persistence::_di_INeonConfiguration __fastcall GetMCPNeonConfig();
public:		// User declarations
	__fastcall TfrmMain(TComponent* Owner);

	int __fastcall TestParam(__int64 AParam1, bool AParam2);
	System::UnicodeString __fastcall TestFunc();
	int __fastcall DoubleValue(int AValue);
	int __fastcall Sub(int a, int b);
	System::UnicodeString __fastcall GetPersonName(TPerson *p);
	TPerson* __fastcall CreatePerson(const System::UnicodeString AName);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmMain *frmMain;
//---------------------------------------------------------------------------
#endif
