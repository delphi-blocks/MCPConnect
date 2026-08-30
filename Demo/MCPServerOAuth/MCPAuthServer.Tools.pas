unit MCPAuthServer.Tools;

interface

uses
  System.Classes, System.SysUtils, System.JSON, System.Generics.Collections,
  System.IOUtils, System.Rtti,

  Vcl.Graphics, Vcl.ExtCtrls, Vcl.Dialogs,

  Neon.Core.Persistence,
  Neon.Core.Persistence.JSON,


  MCPConnect.Configuration.MCP,
  MCPConnect.JRPC.Core,
  MCPConnect.JRPC.Classes,
  MCPConnect.MCP.Types.Base,
  MCPConnect.MCP.Types.Tools,
  MCPConnect.MCP.Attributes,
  MCPConnect.Session.Core;

type
  TUser = class
  private
    FName: string;
    FEMail: string;
    FScope: string;
    FEmailVerified: Boolean;
    FSubject: string;
    FFamilyName: string;
    FGivenName: string;
    FPreferredUsername: string;
    FRawToken: TJSONObject;
  public
    property RawToken: TJSONObject read FRawToken;
    property Subject: string read FSubject;
    property Name: string read FName;
    property EMail: string read FEMail;
    property Scope: string read FScope;
    property EmailVerified: Boolean read FEmailVerified;
    property PreferredUsername: string read FPreferredUsername;
    property GivenName: string read FGivenName;
    property FamilyName: string read FFamilyName;

    constructor Create(const AName: string);
    constructor CreateFromToken(AToken: TMCPAccessToken);
    destructor Destroy; override;

  end;

  TTestTool = class
  private
    [Context]
    FToken: TMCPAccessToken;
  public
    [McpTool('get-user', 'Get the corrent user information', 'icon=person.png')]
    function GetUser(): TUser;

  end;

implementation

{ TTestTool }

function TTestTool.GetUser: TUser;
begin
  Result := TUser.CreateFromToken(FToken);
end;

{ TUser }

constructor TUser.Create(const AName: string);
begin
  inherited Create;
  FName := AName;
  FRawToken := nil;
end;

constructor TUser.CreateFromToken(AToken: TMCPAccessToken);
begin
  inherited Create;
  FName := AToken.Name;
  FEMail := AToken.EMail;
  FScope := AToken.Scope;
  FEmailVerified := AToken.EmailVerified;
  FSubject := AToken.Subject;
  FFamilyName := AToken.FamilyName;
  FGivenName := AToken.GivenName;
  FPreferredUsername := AToken.PreferredUsername;

  FRawToken := AToken.Payload.Clone as TJSONObject;

end;

destructor TUser.Destroy;
begin
  FRawToken.Free;
  inherited;
end;

end.
