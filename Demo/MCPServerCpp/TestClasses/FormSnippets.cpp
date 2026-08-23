//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "FormSnippets.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TfrmSnippets *frmSnippets = NULL;
//---------------------------------------------------------------------------
__fastcall TfrmSnippets::TfrmSnippets(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
