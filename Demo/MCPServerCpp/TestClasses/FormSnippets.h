//---------------------------------------------------------------------------
#ifndef FormSnippetsH
#define FormSnippetsH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
//---------------------------------------------------------------------------
class TfrmSnippets : public TForm
{
__published:	// IDE-managed Components
	TMemo *memoSnippets;
private:	// User declarations
public:		// User declarations
	__fastcall TfrmSnippets(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmSnippets *frmSnippets;
//---------------------------------------------------------------------------
#endif
