//---------------------------------------------------------------------------

#ifndef BigQueryToolH
#define BigQueryToolH
//---------------------------------------------------------------------------
#include <System.hpp>
//---------------------------------------------------------------------------
// MCPConnect discovers these methods through Delphi RTTI. The class deliberately
// declares no constructor so the inherited TObject.Create remains available to
// the library's RTTI factory.
class __declspec(delphiclass) __declspec(delphirtti) TBigQueryTool
    : public System::TObject
{
__published:
    System::UnicodeString __fastcall ListDatasets(
        System::UnicodeString AProjectId);
    System::UnicodeString __fastcall DescribeDataset(
        System::UnicodeString AProjectId,
        System::UnicodeString ADatasetId);
};
//---------------------------------------------------------------------------
#endif
