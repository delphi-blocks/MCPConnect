unit MCPServerDemo.Tools;

interface

uses
  System.Classes, System.SysUtils, System.JSON, System.Generics.Collections,
  System.IOUtils,
  Vcl.Graphics, Vcl.ExtCtrls,
  MCPConnect.JRPC.Core,

  Neon.Core.Persistence,
  Neon.Core.Persistence.JSON,

  Vcl.Dialogs,

  MCPConnect.MCP.Types,
  MCPConnect.MCP.Attributes, MCPConnect.Core.Utils;

type
  TPerson = class
  private
    FName: string;
  public
    property Name: string read FName write FName;
    constructor Create(const AName: string);
  end;

  TTicket = class
  private
    FTitle: string;
    FDate: TDateTime;
    FPrice: Currency;
    FDescription: string;
    FId: Integer;
  public
    property Id: Integer read FId write FId;
    property Title: string read FTitle write FTitle;
    property Price: Currency read FPrice write FPrice;
    property Date: TDateTime read FDate write FDate;
    property Description: string read FDescription write FDescription;

    constructor Create(AId: Integer; const ATitle: string; ADate: TDateTime; APrice: Currency; const ADescription: string);
  end;

  TTickets = class(TObjectList<TTicket>)
  end;

  TDelphiDayTool = class
  private
    [Context]
    FGC: IGarbageCollector;
  public
    [McpTool('get_tickets', 'Get the list of available tickets for the DelphiDay event in Padova')]
    function GetTickets: TTickets;

    [McpTool('buy_ticket', 'Get the list of available tickets for the DelphiDay event in Padova')]
    function BuyTicket(
      [McpParam('id', 'ID of the ticket to buy')] AId: Integer;
      [McpParam('quantity', 'Number of tickets to buy')] AQuantity: Integer
    ): TContentList;
  end;

  TTestTool = class
  public
    [McpTool('double_or_nothing', 'Doubles or zeroes the param value')]
    function TestParam(
    [McpParam('value1', 'Test Parameter 1 for MCP')] AParam1: Int64;
    [McpParam('value2', 'Test Parameter 2 for MCP')] AParam2: Boolean
    ): Integer;

    [McpTool('discounted_items', 'Retrieves a list of discounted items on Wintech-Italia based on the specified item type')]
    function GetDiscountedItems(
      [McpParam('itemType', 'The type of item to filter. Valid values: ''courses'', ''product'', ''consulting''')]
       const AItemType: string
    ): string;


    [McpTool('course_image', 'Retrieves the image for the selected course', 'mime:image/bmp')]
    function GetImage(
      [McpParam('name', 'Course name')] const AName: string
    ): TPicture;

    [McpTool('splitstring', 'Gets the content by splitting the string (e.g. "hello,world" -> ["hello", "world"])')]
    function GetSplitString(
      [McpParam('value', 'The string to work with')]
       const AValue: string
    ): TContentList;

    [McpTool('getperson', 'Get a person from his name')]
    function GetPerson(
      [McpParam('name', 'The name of the person to get')]
       const AName: string
    ): TPerson;
  end;

implementation

{ TTestTool }

function TTestTool.GetDiscountedItems(const AItemType: string): string;
begin
  //'courses'', ''product'', ''consulting';
  var LStringList := TStringList.Create;
  try
    if AItemType = 'courses' then
    begin
      LStringList.Add('Programmazione ad Oggetti con Delphi');
      LStringList.Add('Delphi Modern Development');
    end
    else if AItemType = 'product' then
    begin
      LStringList.Add('Fast Report');
      LStringList.Add('UniDAC');
    end
    else
      LStringList.Add('none');
    Result := LStringList.Text;
  finally
    LStringList.Free;
  end;
end;


function TTestTool.GetImage(const AName: string): TPicture;
begin
  Result := TPicture.Create;
  Result.LoadFromFile('..\..\media\italy.bmp');
end;

function TTestTool.GetPerson(const AName: string): TPerson;
begin
  Result := TPerson.Create(AName);
end;

function TTestTool.GetSplitString(const AValue: string): TContentList;
begin
  var LResultBuilder := TToolResultBuilder.CreateInstance;
  var LStrings := AValue.Split([',']);
  for var LString in LStrings do
  begin
    LResultBuilder.AddText(LString);
  end;
  Result := LResultBuilder.Build;
end;

function TTestTool.TestParam(AParam1: Int64; AParam2: Boolean): Integer;
begin
  if AParam2 then
    Result := AParam1
  else
    Result := 0;
end;

{ TPerson }

constructor TPerson.Create(const AName: string);
begin
  inherited Create;
  FName := AName;
end;

{ TDelphiDayTool }

function TDelphiDayTool.BuyTicket(AId, AQuantity: Integer): TContentList;
var
  LExePath: string;
begin
  LExePath := ExtractFileDir(ParamStr(0));

  TFile.AppendAllText('purchase.log', Format('%s - Ticket ID %d, People: %d' + sLineBreak, [DateTimeToStr(Now), AId, AQuantity]));

  var LResultBuilder := TToolResultBuilder.CreateInstance;
  LResultBuilder.AddText('Purchase completed successfully. Since you made the reservation through an LLM, you will be offered an aperitif at the end of the conference!');

  var LStream := TFileStream.Create(TPath.Combine(LExePath, '..\..\ticket.png'), fmOpenRead or fmShareDenyWrite);
  try
    LResultBuilder.AddImage('image/png', LStream);
  finally
    LStream.Free;
  end;

  Result := LResultBuilder.Build;
end;

function TDelphiDayTool.GetTickets: TTickets;
begin
  Result := TTickets.Create;
  FGC.Add(Result);
  Result.Add(TTicket.Create(1, 'Conferenza + Seminari', StrToDate('19/11/2025'), 179.0, ''));
  Result.Add(TTicket.Create(2, 'Solo Conferenza', StrToDate('19/11/2025'), 0, ''));
  Result.Add(TTicket.Create(3, 'Young ticket', StrToDate('19/11/2025'), 69.0, ''));
end;

{ TTicket }

constructor TTicket.Create(AId: Integer; const ATitle: string; ADate: TDateTime;
  APrice: Currency; const ADescription: string);
begin
  inherited Create;
  FId := AId;
  FTitle := ATitle;
  FDate := ADate;
  FPrice := APrice;
  FDescription := ADescription;
end;

end.
