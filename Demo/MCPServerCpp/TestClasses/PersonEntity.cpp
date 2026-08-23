//---------------------------------------------------------------------------
#pragma hdrstop
#include "PersonEntity.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)

__fastcall TPerson::TPerson()
	: TObject(), FName(L""), FAge(0), FDeveloper(false)
{
}

__fastcall TPerson::TPerson(const System::UnicodeString AName)
	: TObject(), FName(AName), FAge(0), FDeveloper(false)
{
}
