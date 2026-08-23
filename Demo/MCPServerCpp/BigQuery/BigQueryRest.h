//---------------------------------------------------------------------------

#ifndef BigQueryRestH
#define BigQueryRestH
//---------------------------------------------------------------------------
#include <System.hpp>
#include <System.JSON.hpp>
#include <System.Net.HttpClient.hpp>
#include <System.Net.HttpClientComponent.hpp>
//---------------------------------------------------------------------------
class TBigQueryRest : public System::TObject
{
private:
    struct THttpResult
    {
        int StatusCode;
        __int64 ByteCount;
        System::UnicodeString Body;
    };

    static const __int64 MaxResponseBytes = 5 * 1024 * 1024;

    System::Net::Httpclientcomponent::TNetHTTPClient *FClient;
    bool FResponseTooLarge;

    void __fastcall ReceiveDataEx(System::TObject * const Sender,
        __int64 AContentLength, __int64 AReadCount, void *AChunk,
        unsigned int AChunkLength, bool &AAbort);

    System::UnicodeString ReadAccessToken() const;
    System::UnicodeString ResolveQuotaProject(
        const System::UnicodeString &AProjectId) const;
    THttpResult GetJson(const System::UnicodeString &AUrl,
        const System::UnicodeString &AToken,
        const System::UnicodeString &AQuotaProject);

    static void ValidateIdentifier(const System::UnicodeString &AValue,
        const System::UnicodeString &AName);
    static System::UnicodeString EncodePathSegment(
        const System::UnicodeString &AValue);
    static System::Json::TJSONObject *ParseObject(
        const System::UnicodeString &AJson);
    static System::Json::TJSONObject *ObjectValue(
        System::Json::TJSONObject *AObject,
        const System::UnicodeString &AName);
    static System::Json::TJSONArray *ArrayValue(
        System::Json::TJSONObject *AObject,
        const System::UnicodeString &AName);
    static System::UnicodeString StringValue(
        System::Json::TJSONObject *AObject,
        const System::UnicodeString &AName);
    static System::UnicodeString SafeText(
        const System::UnicodeString &AValue);
    static System::UnicodeString SanitizeFileFragment(
        const System::UnicodeString &AValue);
    static System::UnicodeString TimestampUtc();

    static System::UnicodeString BuildSuccess(
        const System::UnicodeString &AOperation,
        const System::UnicodeString &AProjectId,
        const System::UnicodeString &ADatasetId,
        bool ATruncated,
        System::Json::TJSONObject *AData);
    static System::UnicodeString BuildFailure(
        const System::UnicodeString &AOperation,
        const System::UnicodeString &AProjectId,
        const System::UnicodeString &ADatasetId,
        const System::UnicodeString &AKind,
        int AStatus,
        const System::UnicodeString &AReason,
        const System::UnicodeString &AMessage,
        const System::UnicodeString &ARemediation);
    static System::UnicodeString BuildHttpFailure(
        const System::UnicodeString &AOperation,
        const System::UnicodeString &AProjectId,
        const System::UnicodeString &ADatasetId,
        int AStatus,
        const System::UnicodeString &ABody);

public:
    __fastcall TBigQueryRest();
    __fastcall ~TBigQueryRest();

    System::UnicodeString ListDatasets(
        const System::UnicodeString &AProjectId);
    System::UnicodeString DescribeDataset(
        const System::UnicodeString &AProjectId,
        const System::UnicodeString &ADatasetId);

    static System::UnicodeString UnexpectedFailure(
        const System::UnicodeString &AOperation,
        const System::UnicodeString &AMessage);
    static System::UnicodeString OutputDirectory();
};
//---------------------------------------------------------------------------
#endif
