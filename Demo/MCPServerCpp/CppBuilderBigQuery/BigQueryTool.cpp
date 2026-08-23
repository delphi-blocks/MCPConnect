//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "BigQueryTool.h"
#include "BigQueryRest.h"

#include <System.SysUtils.hpp>

#include <memory>
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------

System::UnicodeString __fastcall TBigQueryTool::ListDatasets(
    System::UnicodeString AProjectId)
{
    try
    {
        std::unique_ptr<TBigQueryRest> Rest(new TBigQueryRest());
        return Rest->ListDatasets(AProjectId);
    }
    catch (System::Sysutils::Exception &Error)
    {
        return TBigQueryRest::UnexpectedFailure(L"list_datasets", Error.Message);
    }
    catch (...)
    {
        return TBigQueryRest::UnexpectedFailure(L"list_datasets",
            L"Unknown native exception.");
    }
}
//---------------------------------------------------------------------------

System::UnicodeString __fastcall TBigQueryTool::DescribeDataset(
    System::UnicodeString AProjectId,
    System::UnicodeString ADatasetId)
{
    try
    {
        std::unique_ptr<TBigQueryRest> Rest(new TBigQueryRest());
        return Rest->DescribeDataset(AProjectId, ADatasetId);
    }
    catch (System::Sysutils::Exception &Error)
    {
        return TBigQueryRest::UnexpectedFailure(L"describe_dataset", Error.Message);
    }
    catch (...)
    {
        return TBigQueryRest::UnexpectedFailure(L"describe_dataset",
            L"Unknown native exception.");
    }
}
//---------------------------------------------------------------------------
