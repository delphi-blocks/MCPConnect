//---------------------------------------------------------------------------
#ifndef FormMiscH
#define FormMiscH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <MCPConnect.JRPC.Core.hpp>
//---------------------------------------------------------------------------
class TfrmMisc : public TForm
{
__published:	// IDE-managed Components
	TEdit *edtTemplate;
	TEdit *edtURI;
	TButton *Button2;
	TButton *btnMatches;
	TMemo *memoLog;
	TButton *btnDelphiQueue;
	TButton *btnMCPQueue;
	TButton *btnPrintQueue;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall FormDestroy(TObject *Sender);
	void __fastcall btnMatchesClick(TObject *Sender);
	void __fastcall btnDelphiQueueClick(TObject *Sender);
	void __fastcall btnMCPQueueClick(TObject *Sender);
	void __fastcall btnPrintQueueClick(TObject *Sender);
	void __fastcall Button2Click(TObject *Sender);
private:	// User declarations
	int FCount;
	void *FDelphi;
	void *FMCP;
public:		// User declarations
	__fastcall TfrmMisc(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmMisc *frmMisc;
//---------------------------------------------------------------------------
#endif
