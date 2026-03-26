unit MCP.Form.Snippets;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfrmSnippets = class(TForm)
    memoSnippets: TMemo;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmSnippets: TfrmSnippets;

implementation

{$R *.dfm}

end.
