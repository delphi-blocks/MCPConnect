unit DelphiDay.WebModule;

{
  ==============================================================================
   The DelphiDay site: pages, forms and the MCP endpoint, in one web module
  ==============================================================================

       GET  /                  the home page
       GET  /speakers          the speakers
       GET  /sessions          the program, with filters
       GET  /schedule          the visitor's own schedule
       GET  /tickets           the ticket types
       GET  /checkout          the cart, and the order waiting for confirmation
       GET  /site.css          the stylesheet, served verbatim
       GET  /webmcp.js         the WebMCP client script
       POST /schedule/...      add, remove
       POST /cart/...          add, remove, discount
       POST /checkout/...      prepare, confirm, cancel
       POST /mcp               the MCP endpoint (TJRPCDispatcher)

   The pages are WebStencils templates. Each one asks the shared TMCPWebMCP for
   a view of its own group, so the script tag in the layout offers the browser's
   agent the tools of that page and no others.

   The forms and the tools do the same things through the same service
   (DelphiDay.Service). The visitor is an MCP session: a page creates it and
   hands its id over in a cookie, the server reads the session id from that
   cookie, and so a tool call made from the page finds the same session. The one thing a tool cannot do is confirm a purchase:
   that is a POST from a button on the page.
}

interface

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.NetEncoding,

  Web.HTTPApp,
  Web.Stencils,

  MCPConnect.JRPC.Server,
  MCPConnect.WebMCP,

  DelphiDay.Service;

const
  MCP_PATH = '/mcp';
  CSS_PATH = '/site.css';
  WEBMCP_SCRIPT_PATH = '/webmcp.js';

  /// <summary>
  ///   The images, served from the assets folder next to the executable. A
  ///   wildcard action: an image has no name this unit could know in advance.
  /// </summary>
  ASSETS_PATH = '/assets';

  SERVICE_NAME = 'delphiday-mcp-server';
  SERVICE_VERSION = '1.0.0';

type
  TDelphiDayModule = class(TWebModule)
    procedure WebModuleCreate(Sender: TObject);
  private
    /// <summary>
    ///   The visitor's session, if the request carries a live one; nil
    ///   otherwise. Reading a page never creates a session.
    /// </summary>
    function FindVisitor(Request: TWebRequest): TDelphiDaySession;

    /// <summary>
    ///   The visitor's session, created (with its cookie) when there is none:
    ///   for the forms, which change something.
    /// </summary>
    function NeedVisitor(Request: TWebRequest; Response: TWebResponse): TDelphiDaySession;
    procedure Page(const ATemplate, APage, AGroup: string; Request: TWebRequest;
      Response: TWebResponse);
    procedure Back(Request: TWebRequest; Response: TWebResponse; const AMessage: string;
      AIsError: Boolean);
    procedure RunForm(Request: TWebRequest; Response: TWebResponse;
      const AAction: TFunc<TDelphiDaySession, string>);

    procedure ActionHome(Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure ActionSpeakers(Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure ActionSessions(Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure ActionSchedule(Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure ActionTickets(Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure ActionCheckout(Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure ActionCss(Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure ActionWebMCPScript(Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    /// <summary>
    ///   Serves one image from the assets folder, by file name and nothing else.
    /// </summary>
    procedure ActionAsset(Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);

    procedure ActionScheduleAdd(Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure ActionScheduleRemove(Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure ActionCartAdd(Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure ActionCartRemove(Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure ActionCartDiscount(Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure ActionCheckoutPrepare(Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure ActionCheckoutConfirm(Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure ActionCheckoutCancel(Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);

    procedure ActionNotFound(Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure EngineError(Sender: TObject; const AMessage: string);
  end;

var
  WebModuleClass: TComponentClass = TDelphiDayModule;

  /// <summary>
  ///   The MCP server every request module shares: WebBroker builds one module
  ///   per request, so the protocol engine cannot live in one of them.
  /// </summary>
  ServerMCP: TJRPCServer;

  /// <summary>
  ///   Offers the tools tagged "webmcp" to the agent built into the browser.
  /// </summary>
  WebMCP: TMCPWebMCP;

  /// <summary>
  ///   The folder the templates are read from, and the data file's. The host
  ///   sets them before the socket opens.
  /// </summary>
  TemplatesRoot: string;

function ResolveTemplatesRoot: string;

/// <summary>
///   Loads the catalog and builds the MCP server. Called before the socket
///   opens, so no client pays for the one-time setup.
/// </summary>
procedure ConfigureSite(const ADataFile: string);

implementation

{$R *.dfm}

uses
  Logify,

  MCPConnect.Configuration.MCP,
  MCPConnect.Configuration.Session,
  MCPConnect.Session.Core,
  MCPConnect.Transport.WebBroker,

  DelphiDay.View,
  DelphiDay.Tools;

var
  GEngine: TWebStencilsEngine;
  GResolvedRoot: string;

function ResolveTemplatesRoot: string;
begin
  if GResolvedRoot = '' then
    if TemplatesRoot <> '' then
      GResolvedRoot := IncludeTrailingPathDelimiter(TemplatesRoot)
    else
      GResolvedRoot := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)) + 'templates');
  Result := GResolvedRoot;
end;

procedure ConfigureSite(const ADataFile: string);
begin
  if Assigned(ServerMCP) then
    Exit;

  DelphiDayService := TDelphiDayService.Create(ADataFile);

  ServerMCP := TJRPCServer.Create(nil);
  ServerMCP.Plugin.Configure<IMCPConfig>
    .Server
      .SetName(SERVICE_NAME)
      .SetVersion(SERVICE_VERSION)
    .BackToMCP
    .Tools
      .RegisterClass(TDelphiDayTools)
    .BackToMCP
    .Security
      // The calls come from this site's own pages, so they are same-origin and
      // need no CORS.
      .SetCORS(False)
      // The transport sends the session cookie back on every response, and by
      // default marks it Secure, which a browser will not return over plain
      // http. This demo runs on http://localhost; behind https, leave it on.
      .SetCookieSecure(False)
    .BackToMCP
  .ApplyConfig;

  // The session is the visitor. It exists from their first change: a form
  // creates it and sets the cookie, or - when the agent acts first - the
  // initialize the WebMCP script sends when the server asks for a session. The
  // server reads the session id from the cookie, so the page and the tool calls
  // made from it share one session. Browsing alone creates none: every session
  // has a thread, and a crawler should not cost one per page.
  ServerMCP.Plugin.Configure<ISessionConfig>
    .SetLocation(TSessionIdLocation.Cookie)
    .SetHeaderName(VISITOR_COOKIE)
    .SetTimeout(60)
    // The visitor's schedule and cart live in the session object itself
    .SetSessionClass(TDelphiDaySession)
  .ApplyConfig;

  // The tools carry their own "webmcp" tag, with the group of the page that
  // offers them (see DelphiDay.Tools), so there is nothing to list here.
  WebMCP := TMCPWebMCP.Create(ServerMCP, MCP_PATH, WEBMCP_SCRIPT_PATH);
end;

{ TDelphiDayModule }

procedure TDelphiDayModule.WebModuleCreate(Sender: TObject);

  procedure Add(const AName, APath: string; AMethod: TMethodType; AHandler: THTTPMethodEvent);
  var
    LAction: TWebActionItem;
  begin
    LAction := TWebActionItem(Actions.Add);
    LAction.Name := AName;
    LAction.PathInfo := APath;
    LAction.MethodType := AMethod;
    LAction.OnAction := AHandler;
  end;

var
  LDispatcher: TJRPCDispatcher;
  LDefault: TWebActionItem;
begin
  GEngine.RootDirectory := ResolveTemplatesRoot;
  ConfigureSite(TPath.Combine(ExtractFilePath(ParamStr(0)), 'data' + PathDelim + 'delphiday.json'));

  LDispatcher := TJRPCDispatcher.Create(Self);
  LDispatcher.PathInfo := MCP_PATH;
  LDispatcher.Server := ServerMCP;

  Add('Home', '/', mtGet, ActionHome);
  Add('Speakers', '/speakers', mtGet, ActionSpeakers);
  Add('Sessions', '/sessions', mtGet, ActionSessions);
  Add('Schedule', '/schedule', mtGet, ActionSchedule);
  Add('Tickets', '/tickets', mtGet, ActionTickets);
  Add('Checkout', '/checkout', mtGet, ActionCheckout);
  Add('Css', CSS_PATH, mtGet, ActionCss);
  Add('WebMCPScript', WEBMCP_SCRIPT_PATH, mtGet, ActionWebMCPScript);
  Add('Assets', ASSETS_PATH + '/*', mtGet, ActionAsset);

  Add('ScheduleAdd', '/schedule/add', mtPost, ActionScheduleAdd);
  Add('ScheduleRemove', '/schedule/remove', mtPost, ActionScheduleRemove);
  Add('CartAdd', '/cart/add', mtPost, ActionCartAdd);
  Add('CartRemove', '/cart/remove', mtPost, ActionCartRemove);
  Add('CartDiscount', '/cart/discount', mtPost, ActionCartDiscount);
  Add('CheckoutPrepare', '/checkout/prepare', mtPost, ActionCheckoutPrepare);
  Add('CheckoutConfirm', '/checkout/confirm', mtPost, ActionCheckoutConfirm);
  Add('CheckoutCancel', '/checkout/cancel', mtPost, ActionCheckoutCancel);

  LDefault := TWebActionItem(Actions.Add);
  LDefault.Name := 'NotFound';
  LDefault.Default := True;
  LDefault.OnAction := ActionNotFound;
end;

procedure TDelphiDayModule.EngineError(Sender: TObject; const AMessage: string);
begin
  // WebStencils writes a directive it cannot process into the page and carries
  // on, so the page is the only symptom unless this says so too.
  Logger.Log(Format('WebStencils: %s', [AMessage]), TLogLevel.Warning);
end;

function TDelphiDayModule.FindVisitor(Request: TWebRequest): TDelphiDaySession;
var
  LId: string;
  LSession: TMCPSessionBase;
begin
  Result := nil;
  LId := Request.CookieFields.Values[VISITOR_COOKIE];
  if LId.IsEmpty then
    Exit;

  try
    if (ServerMCP.SessionManager as TMCPSessionManager).TryGetSession(LId, LSession) and
      (LSession is TDelphiDaySession) then
      Result := TDelphiDaySession(LSession);
  except
    on EMCPSessionException do
      Result := nil;  // expired: a new one at the visitor's next change
  end;
end;

function TDelphiDayModule.NeedVisitor(Request: TWebRequest; Response: TWebResponse): TDelphiDaySession;
begin
  Result := FindVisitor(Request);
  if Assigned(Result) then
    Exit;

  Result := (ServerMCP.SessionManager as TMCPSessionManager).CreateSession as TDelphiDaySession;
  // Lax: sent on a link from elsewhere, not on a form posted from elsewhere.
  // No Secure, so that the demo works over plain http on localhost.
  Response.SetCustomHeader('Set-Cookie',
    Format('%s=%s; Path=/; HttpOnly; SameSite=Lax; Max-Age=86400', [VISITOR_COOKIE, Result.SessionId]));
end;

procedure TDelphiDayModule.Page(const ATemplate, APage, AGroup: string; Request: TWebRequest;
  Response: TWebResponse);
var
  LProcessor: TWebStencilsProcessor;
  LModel: TPageModel;
  LVisitor: TDelphiDaySession;
  LBack, LField: string;
  LIndex: Integer;
begin
  LVisitor := FindVisitor(Request);

  LModel := BuildPageModel(APage, LVisitor, Request.QueryFields,
    Request.QueryFields.Values['msg'], Request.QueryFields.Values['err'] = '1');

  // Where a form of this page returns to: the page and its filters, but not
  // the message of the last action.
  LBack := Request.PathInfo;
  LIndex := 0;
  for LField in Request.QueryFields do
    if not (LField.StartsWith('msg=') or LField.StartsWith('err=')) then
    begin
      if LIndex = 0 then
        LBack := LBack + '?'
      else
        LBack := LBack + '&';
      LBack := LBack + LField;
      Inc(LIndex);
    end;
  LModel.Back := LBack;

  LProcessor := TWebStencilsProcessor.Create(Self);
  try
    LProcessor.Engine := GEngine;
    LProcessor.InputFileName := TPath.Combine(ResolveTemplatesRoot, ATemplate);
    LProcessor.WebRequest := Request;
    LProcessor.OnError := EngineError;
    // The processor owns what it is handed, and the module owns the processor
    LProcessor.AddVar('Site', LModel, True);
    LProcessor.AddVar('WebMCP', WebMCP.View(AGroup), True);

    Response.ContentType := 'text/html; charset=utf-8';
    Response.SetCustomHeader('Cache-Control', 'no-store');
    Response.Content := LProcessor.Content;
  finally
    LProcessor.Free;
  end;
end;

procedure TDelphiDayModule.Back(Request: TWebRequest; Response: TWebResponse;
  const AMessage: string; AIsError: Boolean);
var
  LTarget: string;
begin
  // The form says where it came from. Only a path on this site is believed.
  LTarget := Request.ContentFields.Values['back'];
  if (LTarget = '') or not LTarget.StartsWith('/') or LTarget.StartsWith('//') or
    LTarget.Contains(#13) or LTarget.Contains(#10) or LTarget.Contains('\') then
    LTarget := '/';

  if LTarget.Contains('?') then
    LTarget := LTarget + '&'
  else
    LTarget := LTarget + '?';
  LTarget := LTarget + 'msg=' + TNetEncoding.URL.Encode(AMessage);
  if AIsError then
    LTarget := LTarget + '&err=1';

  Response.StatusCode := 303;
  Response.Location := LTarget;
  Response.Content := '';
end;

procedure TDelphiDayModule.RunForm(Request: TWebRequest; Response: TWebResponse;
  const AAction: TFunc<TDelphiDaySession, string>);
var
  LMessage: string;
  LFailed: Boolean;
begin
  LFailed := False;
  try
    LMessage := AAction(NeedVisitor(Request, Response));
  except
    on E: EDelphiDay do
    begin
      LMessage := E.Message;
      LFailed := True;
    end;
  end;
  Back(Request, Response, LMessage, LFailed);
end;

// --- pages ---------------------------------------------------------------------

procedure TDelphiDayModule.ActionHome(Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean);
begin
  Page('home.html', 'home', '', Request, Response);
  Handled := True;
end;

procedure TDelphiDayModule.ActionSpeakers(Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean);
begin
  Page('speakers.html', 'speakers', 'speakers', Request, Response);
  Handled := True;
end;

procedure TDelphiDayModule.ActionSessions(Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean);
begin
  Page('sessions.html', 'sessions', 'sessions', Request, Response);
  Handled := True;
end;

procedure TDelphiDayModule.ActionSchedule(Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean);
begin
  Page('schedule.html', 'schedule', 'schedule', Request, Response);
  Handled := True;
end;

procedure TDelphiDayModule.ActionTickets(Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean);
begin
  Page('tickets.html', 'tickets', 'tickets', Request, Response);
  Handled := True;
end;

procedure TDelphiDayModule.ActionCheckout(Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean);
begin
  Page('checkout.html', 'checkout', 'checkout', Request, Response);
  Handled := True;
end;

procedure TDelphiDayModule.ActionCss(Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean);
begin
  // Served verbatim: a stylesheet must never go through the template parser,
  // which would read its at-rules as directives.
  Response.ContentType := 'text/css; charset=utf-8';
  Response.SetCustomHeader('Cache-Control', 'no-cache');
  Response.Content := TFile.ReadAllText(TPath.Combine(ResolveTemplatesRoot, 'site.css'), TEncoding.UTF8);
  Handled := True;
end;

procedure TDelphiDayModule.ActionWebMCPScript(Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean);
begin
  Response.ContentType := WEBMCP_SCRIPT_CONTENT_TYPE;
  Response.SetCustomHeader('Cache-Control', 'public, max-age=3600');
  Response.Content := TMCPWebMCP.ClientScript;
  Handled := True;
end;

procedure TDelphiDayModule.ActionAsset(Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean);
var
  LName, LFile: string;
begin
  // Only the last segment of the path is looked at, so there is no way out of
  // the folder; and only the image types the site uses are served.
  LName := Request.PathInfo.Substring(Request.PathInfo.LastIndexOf('/') + 1);
  LFile := TPath.Combine(ExtractFilePath(ParamStr(0)) + 'assets', LName);

  if LName.IsEmpty or not SameText(ExtractFileExt(LName), '.png') or
    LName.Contains('') or LName.Contains(':') or not TFile.Exists(LFile) then
  begin
    Response.StatusCode := 404;
    Response.ContentType := 'text/plain';
    Response.Content := 'Not found';
  end
  else
  begin
    Response.ContentType := 'image/png';
    Response.SetCustomHeader('Cache-Control', 'public, max-age=86400');
    Response.ContentStream := TFileStream.Create(LFile, fmOpenRead or fmShareDenyWrite);
  end;
  Handled := True;
end;

// --- forms ---------------------------------------------------------------------

procedure TDelphiDayModule.ActionScheduleAdd(Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean);
begin
  RunForm(Request, Response,
    function (AVisitor: TDelphiDaySession): string
    begin
      Result := DelphiDayService.AddToSchedule(AVisitor,
        StrToIntDef(Request.ContentFields.Values['session_id'], 0));
    end);
  Handled := True;
end;

procedure TDelphiDayModule.ActionScheduleRemove(Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean);
begin
  RunForm(Request, Response,
    function (AVisitor: TDelphiDaySession): string
    begin
      Result := DelphiDayService.RemoveFromSchedule(AVisitor,
        StrToIntDef(Request.ContentFields.Values['session_id'], 0));
    end);
  Handled := True;
end;

procedure TDelphiDayModule.ActionCartAdd(Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean);
begin
  RunForm(Request, Response,
    function (AVisitor: TDelphiDaySession): string
    begin
      Result := DelphiDayService.AddToCart(AVisitor, Request.ContentFields.Values['ticket'],
        StrToIntDef(Request.ContentFields.Values['quantity'], 1));
    end);
  Handled := True;
end;

procedure TDelphiDayModule.ActionCartRemove(Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean);
begin
  RunForm(Request, Response,
    function (AVisitor: TDelphiDaySession): string
    begin
      Result := DelphiDayService.RemoveFromCart(AVisitor, Request.ContentFields.Values['ticket']);
    end);
  Handled := True;
end;

procedure TDelphiDayModule.ActionCartDiscount(Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean);
begin
  RunForm(Request, Response,
    function (AVisitor: TDelphiDaySession): string
    begin
      Result := DelphiDayService.ApplyDiscount(AVisitor, Request.ContentFields.Values['code']);
    end);
  Handled := True;
end;

procedure TDelphiDayModule.ActionCheckoutPrepare(Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean);
begin
  RunForm(Request, Response,
    function (AVisitor: TDelphiDaySession): string
    begin
      DelphiDayService.PlaceOrder(AVisitor, Request.ContentFields.Values['name'],
        Request.ContentFields.Values['email']);
      Result := 'Check the order below, then confirm the purchase.';
    end);
  Handled := True;
end;

procedure TDelphiDayModule.ActionCheckoutConfirm(Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean);
begin
  RunForm(Request, Response,
    function (AVisitor: TDelphiDaySession): string
    begin
      Result := 'Thank you! Order ' + DelphiDayService.ConfirmOrder(AVisitor) +
        ' is confirmed (no real payment was taken: this is a demo).';
    end);
  Handled := True;
end;

procedure TDelphiDayModule.ActionCheckoutCancel(Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean);
begin
  RunForm(Request, Response,
    function (AVisitor: TDelphiDaySession): string
    begin
      Result := DelphiDayService.CancelPending(AVisitor);
    end);
  Handled := True;
end;

procedure TDelphiDayModule.ActionNotFound(Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean);
begin
  Response.StatusCode := 404;
  Response.ContentType := 'text/html; charset=utf-8';
  Response.Content :=
    '<!DOCTYPE html><html><head><meta charset="utf-8"><title>Not found</title></head>' +
    '<body style="font:16px system-ui;padding:2rem"><h1>Not found</h1>' +
    '<p>This page does not exist. <a href="/">Back to Delphi Day</a>.</p></body></html>';
  Handled := True;
end;

initialization
  GEngine := TWebStencilsEngine.Create(nil);
  ServerMCP := nil;
  WebMCP := nil;

finalization
  // Before the server it reads the tools of
  WebMCP.Free;
  ServerMCP.Free;
  GEngine.Free;
  DelphiDayService.Free;

end.
