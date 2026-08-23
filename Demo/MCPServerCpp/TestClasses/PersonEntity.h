//---------------------------------------------------------------------------
#ifndef PersonEntityH
#define PersonEntityH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>

// Delphi-compatible RTTI class for Neon serialization in C++Builder. The
// Delphi sample's NeonProperty/JsonSchema custom attributes cannot be emitted
// by C++ source; this conversion preserves the serializable property RTTI.
class __declspec(delphiclass) __declspec(delphirtti) TPerson : public System::TObject
{
	typedef System::TObject inherited;
private:
	System::UnicodeString FName;
	int FAge;
	bool FDeveloper;

public:
	__fastcall TPerson();
	__fastcall TPerson(const System::UnicodeString AName);
	__fastcall virtual ~TPerson() {}

__published:
	__property System::UnicodeString Name = {read = FName, write = FName};
	__property int Age = {read = FAge, write = FAge};
	__property bool Developer = {read = FDeveloper, write = FDeveloper};
};

#endif
