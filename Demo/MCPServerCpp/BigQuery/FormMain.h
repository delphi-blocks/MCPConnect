//---------------------------------------------------------------------------

#ifndef FormMainH
#define FormMainH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.StdCtrls.hpp>

#include <MCPConnect.Transport.Indy.hpp>
//---------------------------------------------------------------------------
class TFormMain : public TForm
{
__published:
    TLabel *LblEndpointCaption;
    TEdit *EdtPort;
    TButton *BtnStart;
    TButton *BtnStop;
    TLabel *LblServerStatus;
    TLabel *LblTokenStatus;
    TLabel *LblProject;
    TEdit *EdtProjectId;
    TLabel *LblDataset;
    TEdit *EdtDatasetId;
    TButton *BtnListDatasets;
    TButton *BtnDescribeDataset;
    TButton *BtnOpenOutput;
    TLabel *LblResult;
    TMemo *MemoResult;
    TLabel *LblLog;
    TMemo *MemoLog;

    void __fastcall FormCreate(TObject *Sender);
    void __fastcall FormDestroy(TObject *Sender);
    void __fastcall FormCloseQuery(TObject *Sender, bool &CanClose);
    void __fastcall BtnStartClick(TObject *Sender);
    void __fastcall BtnStopClick(TObject *Sender);
    void __fastcall BtnListDatasetsClick(TObject *Sender);
    void __fastcall BtnDescribeDatasetClick(TObject *Sender);
    void __fastcall BtnOpenOutputClick(TObject *Sender);

private:
    Mcpconnect::Transport::Indy::TJRPCIndyServer *FServer;
    bool FConfigured;
    bool FBusy;

    void InitializeMcpServer();
    void StartServer();
    void StopServer();
    void RunDirectTest(bool ADescribeDataset);
    void SetBusy(bool AValue);
    void UpdateUi();
    void RefreshTokenStatus();
    void AddLog(const System::UnicodeString &AMessage);

public:
    __fastcall TFormMain(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TFormMain *FormMain;
//---------------------------------------------------------------------------
#endif
