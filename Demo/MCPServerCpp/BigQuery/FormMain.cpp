//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "FormMain.h"
#include "BigQueryRest.h"
#include "BigQueryTool.h"
#include "ServerConfigBridge.hpp"

#include <IdSocketHandle.hpp>
#include <System.IOUtils.hpp>
#include <System.SysUtils.hpp>
#include <System.Threading.hpp>
#include <Winapi.ShellAPI.hpp>
#include <Winapi.Windows.hpp>

#include <memory>
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TFormMain *FormMain;
//---------------------------------------------------------------------------

__fastcall TFormMain::TFormMain(TComponent* Owner)
    : TForm(Owner), FServer(nullptr), FConfigured(false), FBusy(false)
{
}
//---------------------------------------------------------------------------

void __fastcall TFormMain::FormCreate(TObject *)
{
    MemoLog->Clear();
    MemoResult->Clear();
    AddLog(L"Application ready. Click Start MCP Server to initialize MCPConnect and open the endpoint.");
    RefreshTokenStatus();
    UpdateUi();
}
//---------------------------------------------------------------------------

void TFormMain::InitializeMcpServer()
{
    if (FConfigured && FServer != nullptr)
        return;

    AddLog(L"Step 1/3: creating the MCPConnect Indy server object.");
    Update();

    try
    {
        FServer = Mcpconnect::Transport::Indy::TJRPCIndyServer::CreateMCPServer(this);
        AddLog(L"Step 1/3 complete: MCPConnect Indy server object created.");

        AddLog(L"Step 2/3: registering the C++Builder BigQuery tools.");
        Update();
        Serverconfigbridge::ConfigureBigQueryServer(FServer->JRPCServer,
            __classid(TBigQueryTool));
        FConfigured = true;
        AddLog(L"Step 2/3 complete: MCPConnect accepted the C++ RTTI tool class.");
    }
    catch (...)
    {
        FConfigured = false;
        if (FServer != nullptr)
        {
            delete FServer;
            FServer = nullptr;
        }
        throw;
    }
}
//---------------------------------------------------------------------------

void __fastcall TFormMain::FormDestroy(TObject *)
{
    if (FServer != nullptr)
    {
        FServer->Active = false;
        if (FConfigured)
        {
            try
            {
                Serverconfigbridge::UnregisterBigQueryTools(
                    FServer->JRPCServer, __classid(TBigQueryTool));
            }
            catch (...)
            {
                // Destruction must not be interrupted by cleanup diagnostics.
            }
        }
    }
}
//---------------------------------------------------------------------------

void __fastcall TFormMain::FormCloseQuery(TObject *, bool &CanClose)
{
    CanClose = !FBusy;
    if (!CanClose)
        ShowMessage(L"Wait for the BigQuery metadata request to finish before closing the sample.");
}
//---------------------------------------------------------------------------

void TFormMain::AddLog(const System::UnicodeString &AMessage)
{
    MemoLog->Lines->Add(System::Sysutils::FormatDateTime(L"hh:nn:ss.zzz",
        System::Sysutils::Now()) + L"  " + AMessage);
}
//---------------------------------------------------------------------------

void TFormMain::RefreshTokenStatus()
{
    const System::UnicodeString EnvironmentToken =
        System::Sysutils::Trim(System::Sysutils::GetEnvironmentVariable(
            L"BQ_ACCESS_TOKEN"));
    const System::UnicodeString TokenFile = System::Ioutils::TPath::Combine(
        System::Sysutils::ExtractFilePath(System::ParamStr(0)),
        L"bq_token.txt");

    if (!EnvironmentToken.IsEmpty())
        LblTokenStatus->Caption = L"Token source: BQ_ACCESS_TOKEN (value hidden)";
    else if (System::Sysutils::FileExists(TokenFile))
        LblTokenStatus->Caption = L"Token source: bq_token.txt (value hidden)";
    else
        LblTokenStatus->Caption = L"Token source: not configured";
}
//---------------------------------------------------------------------------

void TFormMain::UpdateUi()
{
    const bool Active = FServer != nullptr && FServer->Active;
    BtnStart->Enabled = !Active && !FBusy;
    BtnStop->Enabled = Active && !FBusy;
    EdtPort->Enabled = !Active && !FBusy;
    BtnListDatasets->Enabled = !FBusy;
    BtnDescribeDataset->Enabled = !FBusy;
    EdtProjectId->Enabled = !FBusy;
    EdtDatasetId->Enabled = !FBusy;

    if (FServer == nullptr)
        LblServerStatus->Caption = L"MCP status: not initialized; click Start MCP Server";
    else if (!FConfigured)
        LblServerStatus->Caption = L"MCP status: compatibility gate failed; see log";
    else if (Active)
        LblServerStatus->Caption = L"MCP status: listening on http://127.0.0.1:" +
            EdtPort->Text + L"/mcp";
    else
        LblServerStatus->Caption = L"MCP status: stopped";
}
//---------------------------------------------------------------------------

void TFormMain::SetBusy(bool AValue)
{
    FBusy = AValue;
    Screen->Cursor = AValue ? crHourGlass : crDefault;
    UpdateUi();
}
//---------------------------------------------------------------------------

void TFormMain::StartServer()
{
    if (!FConfigured || FServer == nullptr || FServer->Active)
        return;

    const int Port = System::Sysutils::StrToIntDef(
        System::Sysutils::Trim(EdtPort->Text), -1);
    if (Port < 1 || Port > 65535)
        throw System::Sysutils::Exception(L"Port must be between 1 and 65535.");

    FServer->Bindings->Clear();
    Idsockethandle::TIdSocketHandle *Binding = FServer->Bindings->Add();
    Binding->IP = L"127.0.0.1";
    Binding->Port = Port;
    FServer->DefaultPort = Port;

    AddLog(L"Step 3/3: activating the Indy listener on http://127.0.0.1:" +
        System::Sysutils::IntToStr(Port) + L"/mcp.");
    LblServerStatus->Caption = L"MCP status: activating listener...";
    Update();

    // This assignment is the point at which Indy binds the socket and the MCP
    // endpoint starts accepting requests.
    FServer->Active = true;
    AddLog(L"Step 3/3 complete: MCP server is listening on the explicit IPv4 loopback binding.");
    UpdateUi();
}
//---------------------------------------------------------------------------

void TFormMain::StopServer()
{
    if (FServer != nullptr && FServer->Active)
    {
        FServer->Active = false;
        AddLog(L"MCP server stopped.");
    }
    UpdateUi();
}
//---------------------------------------------------------------------------

void __fastcall TFormMain::BtnStartClick(TObject *)
{
    if (FBusy)
        return;

    SetBusy(true);
    LblServerStatus->Caption = L"MCP status: starting by user request...";
    AddLog(L"Start requested by the user. Endpoint authentication is disabled; listener is loopback-only.");
    Update();

    try
    {
        InitializeMcpServer();
        StartServer();
    }
    catch (System::Sysutils::Exception &Error)
    {
        AddLog(L"Server start failed: " + Error.Message);
        MemoResult->Lines->Text = TBigQueryRest::UnexpectedFailure(
            L"start_mcp_server", Error.Message);
        ShowMessage(Error.Message);
    }
    catch (...)
    {
        const System::UnicodeString Message = L"Unknown native exception.";
        AddLog(L"Server start failed: " + Message);
        MemoResult->Lines->Text = TBigQueryRest::UnexpectedFailure(
            L"start_mcp_server", Message);
        ShowMessage(Message);
    }

    SetBusy(false);
}
//---------------------------------------------------------------------------

void __fastcall TFormMain::BtnStopClick(TObject *)
{
    if (FBusy)
        return;

    SetBusy(true);
    try
    {
        StopServer();
    }
    catch (System::Sysutils::Exception &Error)
    {
        AddLog(L"Server stop failed: " + Error.Message);
        ShowMessage(Error.Message);
    }
    SetBusy(false);
}
//---------------------------------------------------------------------------

void TFormMain::RunDirectTest(bool ADescribeDataset)
{
    if (FBusy)
        return;

    const System::UnicodeString ProjectId =
        System::Sysutils::Trim(EdtProjectId->Text);
    const System::UnicodeString DatasetId =
        System::Sysutils::Trim(EdtDatasetId->Text);
    const System::UnicodeString Operation = ADescribeDataset
        ? System::UnicodeString(L"describe_dataset")
        : System::UnicodeString(L"list_datasets");

    RefreshTokenStatus();
    SetBusy(true);
    AddLog(L"Starting direct BigQuery REST test: " + Operation);

    try
    {
        TThread *Worker = TThread::CreateAnonymousThread(
            System::Sysutils::_di_TProc(
            [this, ProjectId, DatasetId, ADescribeDataset, Operation]()
            {
                System::UnicodeString Result;
                try
                {
                    std::unique_ptr<TBigQueryRest> Rest(new TBigQueryRest());
                    Result = ADescribeDataset
                        ? Rest->DescribeDataset(ProjectId, DatasetId)
                        : Rest->ListDatasets(ProjectId);
                }
                catch (System::Sysutils::Exception &Error)
                {
                    Result = TBigQueryRest::UnexpectedFailure(
                        Operation, Error.Message);
                }
                catch (...)
                {
                    Result = TBigQueryRest::UnexpectedFailure(
                        Operation, L"Unknown native exception.");
                }

                TThread::Synchronize(nullptr,
                    System::Classes::_di_TThreadProcedure(
                    [this, Result, Operation]()
                    {
                        MemoResult->Lines->Text = Result;
                        AddLog(L"Direct BigQuery REST test finished: " + Operation);
                        RefreshTokenStatus();
                        SetBusy(false);
                    }));
            }));
        Worker->FreeOnTerminate = true;
        Worker->Start();
    }
    catch (System::Sysutils::Exception &Error)
    {
        SetBusy(false);
        AddLog(L"Could not start worker thread: " + Error.Message);
        MemoResult->Lines->Text = TBigQueryRest::UnexpectedFailure(
            Operation, Error.Message);
    }
}
//---------------------------------------------------------------------------

void __fastcall TFormMain::BtnListDatasetsClick(TObject *)
{
    RunDirectTest(false);
}
//---------------------------------------------------------------------------

void __fastcall TFormMain::BtnDescribeDatasetClick(TObject *)
{
    RunDirectTest(true);
}
//---------------------------------------------------------------------------

void __fastcall TFormMain::BtnOpenOutputClick(TObject *)
{
    try
    {
        const System::UnicodeString Directory = TBigQueryRest::OutputDirectory();
        System::Ioutils::TDirectory::CreateDirectory(Directory);
        HINSTANCE ShellResult = ::ShellExecuteW(Handle, L"open",
            Directory.c_str(), nullptr, nullptr, SW_SHOWNORMAL);
        if (reinterpret_cast<INT_PTR>(ShellResult) <= 32)
            throw System::Sysutils::Exception(
                L"Windows could not open the output folder.");
    }
    catch (System::Sysutils::Exception &Error)
    {
        ShowMessage(Error.Message);
    }
}
//---------------------------------------------------------------------------
