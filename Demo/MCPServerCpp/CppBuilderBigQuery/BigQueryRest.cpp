//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "BigQueryRest.h"

#include <System.Classes.hpp>
#include <System.IOUtils.hpp>
#include <System.Net.URLClient.hpp>
#include <System.NetEncoding.hpp>
#include <System.SysUtils.hpp>
#include <Winapi.Windows.hpp>

#include <memory>
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------

const __int64 TBigQueryRest::MaxResponseBytes;

namespace
{
    volatile LONG OutputSequence = 0;

    System::UnicodeString BigQueryBaseUrl()
    {
        // Construct this only when a user starts a BigQuery request. A global
        // UnicodeString would run C++ initialization before _tWinMain, which
        // can stall the Win32 C++/Delphi runtime before the VCL form exists.
        return System::UnicodeString(
            L"https://bigquery.googleapis.com/bigquery/v2");
    }

    System::UnicodeString StageRemediation(const System::UnicodeString &AStage)
    {
        if (AStage == L"validation")
            return L"Provide the required project and dataset identifiers as strings.";
        if (AStage == L"auth")
            return L"Set BQ_ACCESS_TOKEN or regenerate bq_token.txt with gcloud auth print-access-token.";
        if (AStage == L"parse")
            return L"Retry the call. If it persists, inspect whether the BigQuery API response format changed.";
        if (AStage == L"file_io")
            return L"Grant write access to the output folder next to the executable.";
        if (AStage == L"response_too_large")
            return L"Reduce the metadata scope; the sample accepts at most 5 MiB per response.";
        if (AStage == L"timeout")
            return L"Check connectivity to bigquery.googleapis.com and retry; the sample uses fixed request timeouts.";
        return L"Check network access to bigquery.googleapis.com and retry.";
    }

    bool LooksLikeTimeout(const System::UnicodeString &AMessage)
    {
        const System::UnicodeString Lower = System::Sysutils::LowerCase(AMessage);
        return Lower.Pos(L"timeout") > 0 || Lower.Pos(L"timed out") > 0;
    }

    void ValidateAccessTokenValue(const System::UnicodeString &AToken)
    {
        if (AToken.IsEmpty())
            throw System::Sysutils::Exception(
                L"The BigQuery access token is empty.");
        if (AToken.Length() > 16384)
            throw System::Sysutils::Exception(
                L"The BigQuery access token is unreasonably long.");
        for (int Index = 1; Index <= AToken.Length(); ++Index)
            if (AToken[Index] <= 32 || AToken[Index] == 127)
                throw System::Sysutils::Exception(
                    L"The BigQuery access token contains whitespace or a control character.");
    }

    void ValidateOptionalArray(System::Json::TJSONObject *AObject,
        const System::UnicodeString &AName)
    {
        System::Json::TJSONValue *Value = AObject->GetValue(AName);
        if (Value != nullptr &&
            dynamic_cast<System::Json::TJSONArray *>(Value) == nullptr)
            throw System::Sysutils::Exception(
                L"BigQuery returned an invalid " + AName + L" field.");
    }

    bool HasNonEmptyOptionalString(System::Json::TJSONObject *AObject,
        const System::UnicodeString &AName)
    {
        System::Json::TJSONValue *Value = AObject->GetValue(AName);
        if (Value == nullptr)
            return false;
        if (dynamic_cast<System::Json::TJSONString *>(Value) == nullptr)
            throw System::Sysutils::Exception(
                L"BigQuery returned an invalid " + AName + L" field.");
        return !Value->Value().IsEmpty();
    }
}
//---------------------------------------------------------------------------

__fastcall TBigQueryRest::TBigQueryRest()
    : FClient(new System::Net::Httpclientcomponent::TNetHTTPClient(nullptr)),
      FResponseTooLarge(false)
{
    FClient->ConnectionTimeout = 10000;
    FClient->ResponseTimeout = 30000;
    FClient->SynchronizeEvents = false;
    FClient->HandleRedirects = false;
    FClient->Accept = L"application/json";
    FClient->UserAgent = L"MCPConnect-CppBuilder-BigQuery-Sample/1.0";
    FClient->OnReceiveDataEx = ReceiveDataEx;
}
//---------------------------------------------------------------------------

__fastcall TBigQueryRest::~TBigQueryRest()
{
    delete FClient;
}
//---------------------------------------------------------------------------

void __fastcall TBigQueryRest::ReceiveDataEx(System::TObject * const,
    __int64 AContentLength, __int64 AReadCount, void *, unsigned int,
    bool &AAbort)
{
    if ((AContentLength > MaxResponseBytes) || (AReadCount > MaxResponseBytes))
    {
        FResponseTooLarge = true;
        AAbort = true;
    }
}
//---------------------------------------------------------------------------

System::UnicodeString TBigQueryRest::ReadAccessToken() const
{
    System::UnicodeString Token =
        System::Sysutils::Trim(System::Sysutils::GetEnvironmentVariable(
            L"BQ_ACCESS_TOKEN"));
    if (!Token.IsEmpty())
    {
        ValidateAccessTokenValue(Token);
        return Token;
    }

    const System::UnicodeString TokenPath = System::Ioutils::TPath::Combine(
        System::Sysutils::ExtractFilePath(System::ParamStr(0)),
        L"bq_token.txt");

    if (!System::Sysutils::FileExists(TokenPath))
        throw System::Sysutils::Exception(
            L"No access token was found in BQ_ACCESS_TOKEN or " + TokenPath);

    // Let TFile detect a BOM. This accepts UTF-8 as well as the UTF-16LE file
    // produced by the redirection operator in Windows PowerShell 5.1.
    Token = System::Sysutils::Trim(
        System::Ioutils::TFile::ReadAllText(TokenPath));
    ValidateAccessTokenValue(Token);

    return Token;
}
//---------------------------------------------------------------------------

System::UnicodeString TBigQueryRest::ResolveQuotaProject(
    const System::UnicodeString &AProjectId) const
{
    System::UnicodeString Result =
        System::Sysutils::Trim(System::Sysutils::GetEnvironmentVariable(
            L"BQ_QUOTA_PROJECT"));
    if (Result.IsEmpty())
        Result = AProjectId;
    ValidateIdentifier(Result, L"quota project");
    return Result;
}
//---------------------------------------------------------------------------

TBigQueryRest::THttpResult TBigQueryRest::GetJson(
    const System::UnicodeString &AUrl,
    const System::UnicodeString &AToken,
    const System::UnicodeString &AQuotaProject)
{
    System::Net::Urlclient::TNetHeaders Headers;
    Headers.Length = 3;
    Headers[0].Name = L"Accept";
    Headers[0].Value = L"application/json";
    Headers[1].Name = L"Authorization";
    Headers[1].Value = L"Bearer " + AToken;
    Headers[2].Name = L"X-Goog-User-Project";
    Headers[2].Value = AQuotaProject;

    std::unique_ptr<System::Classes::TMemoryStream> Stream(
        new System::Classes::TMemoryStream());
    FResponseTooLarge = false;

    System::Net::Httpclient::_di_IHTTPResponse Response;
    try
    {
        Response = FClient->Get(AUrl, Stream.get(), Headers);
    }
    catch (System::Sysutils::Exception &)
    {
        if (FResponseTooLarge)
            throw System::Sysutils::Exception(
                L"The BigQuery response exceeded the 5 MiB sample limit.");
        throw;
    }

    if (Stream->Size > MaxResponseBytes)
    {
        FResponseTooLarge = true;
        throw System::Sysutils::Exception(
            L"The BigQuery response exceeded the 5 MiB sample limit.");
    }

    System::DynamicArray<System::Byte> Bytes;
    Bytes.Length = static_cast<int>(Stream->Size);
    Stream->Position = 0;
    if (Bytes.Length > 0)
        Stream->ReadBuffer(&Bytes[0], Bytes.Length);

    THttpResult Result;
    Result.StatusCode = Response->StatusCode;
    Result.ByteCount = Stream->Size;
    Result.Body = Bytes.Length == 0
        ? System::UnicodeString()
        : System::Sysutils::TEncoding::UTF8->GetString(Bytes);
    return Result;
}
//---------------------------------------------------------------------------

void TBigQueryRest::ValidateIdentifier(const System::UnicodeString &AValue,
    const System::UnicodeString &AName)
{
    if (AValue.IsEmpty())
        throw System::Sysutils::Exception(AName + L" is required.");
    if (AValue.Length() > 1024)
        throw System::Sysutils::Exception(AName + L" is too long.");

    for (int Index = 1; Index <= AValue.Length(); ++Index)
        if (AValue[Index] < 32)
            throw System::Sysutils::Exception(
                AName + L" contains a control character.");
}
//---------------------------------------------------------------------------

System::UnicodeString TBigQueryRest::EncodePathSegment(
    const System::UnicodeString &AValue)
{
    return System::Netencoding::TNetEncoding::URL->Encode(AValue);
}
//---------------------------------------------------------------------------

System::Json::TJSONObject *TBigQueryRest::ParseObject(
    const System::UnicodeString &AJson)
{
    System::Json::TJSONValue *Value =
        System::Json::TJSONObject::ParseJSONValue(AJson, true, true);
    System::Json::TJSONObject *Object =
        dynamic_cast<System::Json::TJSONObject *>(Value);
    if (Object == nullptr)
    {
        delete Value;
        throw System::Sysutils::Exception(
            L"BigQuery returned JSON whose root is not an object.");
    }
    return Object;
}
//---------------------------------------------------------------------------

System::Json::TJSONObject *TBigQueryRest::ObjectValue(
    System::Json::TJSONObject *AObject, const System::UnicodeString &AName)
{
    return AObject == nullptr ? nullptr :
        dynamic_cast<System::Json::TJSONObject *>(AObject->GetValue(AName));
}
//---------------------------------------------------------------------------

System::Json::TJSONArray *TBigQueryRest::ArrayValue(
    System::Json::TJSONObject *AObject, const System::UnicodeString &AName)
{
    return AObject == nullptr ? nullptr :
        dynamic_cast<System::Json::TJSONArray *>(AObject->GetValue(AName));
}
//---------------------------------------------------------------------------

System::UnicodeString TBigQueryRest::StringValue(
    System::Json::TJSONObject *AObject, const System::UnicodeString &AName)
{
    System::Json::TJSONValue *Value =
        AObject == nullptr ? nullptr : AObject->GetValue(AName);
    return Value == nullptr ? System::UnicodeString() : Value->Value();
}
//---------------------------------------------------------------------------

System::UnicodeString TBigQueryRest::SafeText(
    const System::UnicodeString &AValue)
{
    System::UnicodeString Result;
    const int Limit = AValue.Length() > 800 ? 800 : AValue.Length();
    for (int Index = 1; Index <= Limit; ++Index)
    {
        const wchar_t Character = AValue[Index];
        Result += Character < 32 ? wchar_t(L' ') : Character;
    }
    if (AValue.Length() > Limit)
        Result += L"...";
    return System::Sysutils::Trim(Result);
}
//---------------------------------------------------------------------------

System::UnicodeString TBigQueryRest::SanitizeFileFragment(
    const System::UnicodeString &AValue)
{
    System::UnicodeString Result;
    const int Limit = AValue.Length() > 60 ? 60 : AValue.Length();
    for (int Index = 1; Index <= Limit; ++Index)
    {
        const wchar_t Character = AValue[Index];
        if ((Character >= L'a' && Character <= L'z') ||
            (Character >= L'A' && Character <= L'Z') ||
            (Character >= L'0' && Character <= L'9') ||
            Character == L'_' || Character == L'-' || Character == L'.')
            Result += Character;
        else
            Result += L'_';
    }
    return Result.IsEmpty() ? System::UnicodeString(L"value") : Result;
}
//---------------------------------------------------------------------------

System::UnicodeString TBigQueryRest::TimestampUtc()
{
    SYSTEMTIME Value;
    ::GetSystemTime(&Value);
    return System::Sysutils::Format(
        L"%4.4d%2.2d%2.2d_%2.2d%2.2d%2.2d_%3.3dZ",
        ARRAYOFCONST((Value.wYear, Value.wMonth, Value.wDay,
            Value.wHour, Value.wMinute, Value.wSecond, Value.wMilliseconds)));
}
//---------------------------------------------------------------------------

System::UnicodeString TBigQueryRest::OutputDirectory()
{
    return System::Ioutils::TPath::Combine(
        System::Sysutils::ExtractFilePath(System::ParamStr(0)),
        L"output");
}
//---------------------------------------------------------------------------

System::UnicodeString TBigQueryRest::BuildSuccess(
    const System::UnicodeString &AOperation,
    const System::UnicodeString &AProjectId,
    const System::UnicodeString &ADatasetId,
    bool ATruncated,
    System::Json::TJSONObject *AData)
{
    std::unique_ptr<System::Json::TJSONObject> Root(
        new System::Json::TJSONObject());
    std::unique_ptr<System::Json::TJSONObject> Limits(
        new System::Json::TJSONObject());
    std::unique_ptr<System::Json::TJSONObject> Data(AData);

    const System::UnicodeString Directory = OutputDirectory();
    System::Ioutils::TDirectory::CreateDirectory(Directory);

    System::UnicodeString FileName = AOperation + L"_" +
        SanitizeFileFragment(AProjectId);
    if (!ADatasetId.IsEmpty())
        FileName += L"_" + SanitizeFileFragment(ADatasetId);
    const unsigned int Sequence = static_cast<unsigned int>(
        ::InterlockedIncrement(&OutputSequence));
    FileName += L"_" + TimestampUtc() + L"_p" +
        System::Sysutils::IntToHex(
            static_cast<int>(::GetCurrentProcessId()), 8) + L"_" +
        System::Sysutils::IntToHex(static_cast<int>(Sequence), 8) + L".json";

    const System::UnicodeString OutputFile =
        System::Ioutils::TPath::Combine(Directory, FileName);

    Root->AddPair(L"ok", true);
    Root->AddPair(L"operation", AOperation);
    Root->AddPair(L"project_id", AProjectId);
    if (!ADatasetId.IsEmpty())
        Root->AddPair(L"dataset_id", ADatasetId);
    Root->AddPair(L"output_file", OutputFile);

    Limits->AddPair(L"datasets", 50);
    Limits->AddPair(L"tables", 20);
    Root->AddPair(L"limits", Limits.release());
    Root->AddPair(L"truncated", ATruncated);
    Root->AddPair(L"data", Data.release());

    const System::UnicodeString Json = Root->ToJSON();
    std::unique_ptr<System::Sysutils::TUTF8Encoding> Utf8WithoutBom(
        new System::Sysutils::TUTF8Encoding(false));
    System::Ioutils::TFile::WriteAllText(OutputFile, Json,
        Utf8WithoutBom.get());
    return Json;
}
//---------------------------------------------------------------------------

System::UnicodeString TBigQueryRest::BuildFailure(
    const System::UnicodeString &AOperation,
    const System::UnicodeString &AProjectId,
    const System::UnicodeString &ADatasetId,
    const System::UnicodeString &AKind,
    int AStatus,
    const System::UnicodeString &AReason,
    const System::UnicodeString &AMessage,
    const System::UnicodeString &ARemediation)
{
    std::unique_ptr<System::Json::TJSONObject> Root(
        new System::Json::TJSONObject());
    std::unique_ptr<System::Json::TJSONObject> Error(
        new System::Json::TJSONObject());

    Root->AddPair(L"ok", false);
    Root->AddPair(L"operation", AOperation);
    if (!AProjectId.IsEmpty())
        Root->AddPair(L"project_id", AProjectId);
    if (!ADatasetId.IsEmpty())
        Root->AddPair(L"dataset_id", ADatasetId);

    Error->AddPair(L"kind", AKind);
    Error->AddPair(L"status", AStatus);
    Error->AddPair(L"reason", AReason);
    Error->AddPair(L"message", SafeText(AMessage));
    Error->AddPair(L"remediation", ARemediation);
    Root->AddPair(L"error", Error.release());
    return Root->ToJSON();
}
//---------------------------------------------------------------------------

System::UnicodeString TBigQueryRest::BuildHttpFailure(
    const System::UnicodeString &AOperation,
    const System::UnicodeString &AProjectId,
    const System::UnicodeString &ADatasetId,
    int AStatus,
    const System::UnicodeString &ABody)
{
    System::UnicodeString Message = L"BigQuery returned HTTP " +
        System::Sysutils::IntToStr(AStatus) + L".";
    System::UnicodeString Reason = L"http_error";

    std::unique_ptr<System::Json::TJSONValue> Parsed(
        System::Json::TJSONObject::ParseJSONValue(ABody, true, false));
    System::Json::TJSONObject *Root =
        dynamic_cast<System::Json::TJSONObject *>(Parsed.get());
    System::Json::TJSONObject *Error = ObjectValue(Root, L"error");
    const System::UnicodeString GoogleMessage = StringValue(Error, L"message");
    if (!GoogleMessage.IsEmpty())
        Message = GoogleMessage;

    System::Json::TJSONArray *Errors = ArrayValue(Error, L"errors");
    if (Errors != nullptr && Errors->Count > 0)
    {
        System::Json::TJSONObject *First =
            dynamic_cast<System::Json::TJSONObject *>(Errors->Items[0]);
        const System::UnicodeString GoogleReason =
            StringValue(First, L"reason");
        if (!GoogleReason.IsEmpty())
            Reason = GoogleReason;
    }

    System::UnicodeString Kind = L"http";
    System::UnicodeString Remediation =
        L"Review the safe error message and Google Cloud configuration.";
    if (AStatus == 401)
    {
        Kind = L"auth";
        Remediation = L"Regenerate the token with gcloud auth print-access-token and retry.";
    }
    else if (AStatus == 403)
    {
        Kind = L"permission";
        Remediation = L"Enable the BigQuery API; grant roles/bigquery.metadataViewer; and grant serviceusage.services.use on the quota project.";
    }
    else if (AStatus == 404)
    {
        Kind = L"not_found";
        Remediation = L"Check the project and dataset identifiers and the caller's visibility.";
    }
    else if (AStatus == 429)
    {
        Kind = L"quota";
        Remediation = L"Wait before retrying and review the BigQuery API quota for the quota project.";
    }
    else if (AStatus == 408)
    {
        Kind = L"timeout";
        Remediation = L"Retry the request; the upstream service reported a timeout.";
    }
    else if (AStatus >= 500)
    {
        Kind = L"service";
        Remediation = L"Retry later; Google BigQuery reported a server-side failure.";
    }

    return BuildFailure(AOperation, AProjectId, ADatasetId, Kind, AStatus,
        Reason, Message, Remediation);
}
//---------------------------------------------------------------------------

System::UnicodeString TBigQueryRest::UnexpectedFailure(
    const System::UnicodeString &AOperation,
    const System::UnicodeString &AMessage)
{
    return BuildFailure(AOperation, System::UnicodeString(),
        System::UnicodeString(), L"internal", 0, L"unexpected_exception",
        AMessage, L"Review the sample log and retry after correcting the reported condition.");
}
//---------------------------------------------------------------------------

System::UnicodeString TBigQueryRest::ListDatasets(
    const System::UnicodeString &AProjectId)
{
    const System::UnicodeString Operation = L"list_datasets";
    System::UnicodeString ProjectId = System::Sysutils::Trim(AProjectId);
    System::UnicodeString Stage = L"validation";

    try
    {
        ValidateIdentifier(ProjectId, L"project_id");
        const System::UnicodeString QuotaProject = ResolveQuotaProject(ProjectId);

        Stage = L"auth";
        const System::UnicodeString Token = ReadAccessToken();

        Stage = L"network";
        const System::UnicodeString Url = BigQueryBaseUrl() + L"/projects/" +
            EncodePathSegment(ProjectId) + L"/datasets?maxResults=50";
        const THttpResult Http = GetJson(Url, Token, QuotaProject);
        if (Http.StatusCode < 200 || Http.StatusCode >= 300)
            return BuildHttpFailure(Operation, ProjectId,
                System::UnicodeString(), Http.StatusCode, Http.Body);

        Stage = L"parse";
        std::unique_ptr<System::Json::TJSONObject> Data(ParseObject(Http.Body));
        ValidateOptionalArray(Data.get(), L"datasets");
        const bool Truncated = HasNonEmptyOptionalString(
            Data.get(), L"nextPageToken");

        Stage = L"file_io";
        return BuildSuccess(Operation, ProjectId, System::UnicodeString(),
            Truncated, Data.release());
    }
    catch (System::Sysutils::Exception &Error)
    {
        if (FResponseTooLarge)
            Stage = L"response_too_large";
        else if (Stage == L"network" && LooksLikeTimeout(Error.Message))
            Stage = L"timeout";
        return BuildFailure(Operation, ProjectId, System::UnicodeString(),
            Stage, 0, Stage + L"_failure", Error.Message,
            StageRemediation(Stage));
    }
    catch (...)
    {
        return UnexpectedFailure(Operation, L"Unknown native exception.");
    }
}
//---------------------------------------------------------------------------

System::UnicodeString TBigQueryRest::DescribeDataset(
    const System::UnicodeString &AProjectId,
    const System::UnicodeString &ADatasetId)
{
    const System::UnicodeString Operation = L"describe_dataset";
    System::UnicodeString ProjectId = System::Sysutils::Trim(AProjectId);
    System::UnicodeString DatasetId = System::Sysutils::Trim(ADatasetId);
    System::UnicodeString Stage = L"validation";

    try
    {
        ValidateIdentifier(ProjectId, L"project_id");
        ValidateIdentifier(DatasetId, L"dataset_id");
        const System::UnicodeString QuotaProject = ResolveQuotaProject(ProjectId);

        Stage = L"auth";
        const System::UnicodeString Token = ReadAccessToken();

        const System::UnicodeString ProjectSegment = EncodePathSegment(ProjectId);
        const System::UnicodeString DatasetSegment = EncodePathSegment(DatasetId);
        const System::UnicodeString TablesUrl = BigQueryBaseUrl() + L"/projects/" +
            ProjectSegment + L"/datasets/" + DatasetSegment +
            L"/tables?maxResults=20";

        Stage = L"network";
        const THttpResult TablesHttp = GetJson(TablesUrl, Token, QuotaProject);
        if (TablesHttp.StatusCode < 200 || TablesHttp.StatusCode >= 300)
            return BuildHttpFailure(Operation, ProjectId, DatasetId,
                TablesHttp.StatusCode, TablesHttp.Body);

        Stage = L"parse";
        std::unique_ptr<System::Json::TJSONObject> TableList(
            ParseObject(TablesHttp.Body));
        __int64 AggregateBytes = TablesHttp.ByteCount;
        ValidateOptionalArray(TableList.get(), L"tables");
        const bool Truncated = HasNonEmptyOptionalString(
            TableList.get(), L"nextPageToken");
        System::Json::TJSONArray *Tables = ArrayValue(TableList.get(), L"tables");
        std::unique_ptr<System::Json::TJSONArray> Schemas(
            new System::Json::TJSONArray());

        if (Tables != nullptr)
        {
            const int Count = Tables->Count > 20 ? 20 : Tables->Count;
            for (int Index = 0; Index < Count; ++Index)
            {
                System::Json::TJSONObject *Table =
                    dynamic_cast<System::Json::TJSONObject *>(Tables->Items[Index]);
                System::Json::TJSONObject *Reference =
                    ObjectValue(Table, L"tableReference");
                const System::UnicodeString TableId =
                    StringValue(Reference, L"tableId");
                if (TableId.IsEmpty())
                    throw System::Sysutils::Exception(
                        L"A tables.list item has no tableReference.tableId.");

                Stage = L"network";
                const System::UnicodeString TableUrl = BigQueryBaseUrl() +
                    L"/projects/" + ProjectSegment + L"/datasets/" +
                    DatasetSegment + L"/tables/" +
                    EncodePathSegment(TableId) + L"?view=BASIC";
                const THttpResult TableHttp =
                    GetJson(TableUrl, Token, QuotaProject);
                if (TableHttp.StatusCode < 200 || TableHttp.StatusCode >= 300)
                    return BuildHttpFailure(Operation, ProjectId, DatasetId,
                        TableHttp.StatusCode, TableHttp.Body);

                if (TableHttp.ByteCount > MaxResponseBytes - AggregateBytes)
                {
                    FResponseTooLarge = true;
                    throw System::Sysutils::Exception(
                        L"The combined BigQuery metadata exceeded the 5 MiB sample limit.");
                }
                AggregateBytes += TableHttp.ByteCount;

                Stage = L"parse";
                std::unique_ptr<System::Json::TJSONObject> Schema(
                    ParseObject(TableHttp.Body));
                Schemas->Add(Schema.release());
            }
        }

        std::unique_ptr<System::Json::TJSONObject> Data(
            new System::Json::TJSONObject());
        Data->AddPair(L"table_list", TableList.release());
        Data->AddPair(L"schemas", Schemas.release());

        Stage = L"file_io";
        return BuildSuccess(Operation, ProjectId, DatasetId, Truncated,
            Data.release());
    }
    catch (System::Sysutils::Exception &Error)
    {
        if (FResponseTooLarge)
            Stage = L"response_too_large";
        else if (Stage == L"network" && LooksLikeTimeout(Error.Message))
            Stage = L"timeout";
        return BuildFailure(Operation, ProjectId, DatasetId, Stage, 0,
            Stage + L"_failure", Error.Message, StageRemediation(Stage));
    }
    catch (...)
    {
        return UnexpectedFailure(Operation, L"Unknown native exception.");
    }
}
//---------------------------------------------------------------------------
