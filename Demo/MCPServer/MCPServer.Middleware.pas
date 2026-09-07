unit MCPServer.Middleware;

interface

uses
  System.Classes, System.SysUtils, System.JSON, System.Diagnostics,

  Logify,

  JRPC.Core,
  MCPConnect.JRPC.Middleware,

  MCPConnect.MCP.Types.Base,
  MCPConnect.MCP.Types.Tools,
  MCPConnect.MCP.Middleware;

type
  TMessageMiddleWare = class(TMiddleware, IMessageMiddleware)
  public
    procedure Handle(AContext: TMiddlewareContext; const AChain: TMiddlewareChain);
  end;

  TMCPMiddleware = class(TMiddleware, IMCPMiddleware)
  public
    function Handle(AContext: TMiddlewareContext; AParams: TRequestMetaParams; const AChain: TMCPChain): TBaseResult;
  end;

  TDiagnosticMiddleware = class(TMiddleware, IMCPMiddleware)
  public
    function Handle(AContext: TMiddlewareContext; AParams: TRequestMetaParams; const AChain: TMCPChain): TBaseResult;
  end;

  TCallToolMiddleware = class(TMiddleware, ICallToolMiddleware)
  public
    function Handle(AContext: TMiddlewareContext; AParams: TCallToolRequestParams; const AChain: TCallToolChain): TBaseResult;
  end;

  TDiscoverMiddleware = class(TMiddleware, IDiscoverMiddleware)
  public
    function Handle(AContext: TMiddlewareContext; AParams: TRequestMetaParams; const AChain: TDiscoverChain): TDiscoverResult;
  end;

implementation

uses
  MCPConnect.Transport.Base;

{ TMessageMiddleWare }

function JSONArrayToString(AValues: TArray<TJRPCMessage>): string;
begin
  var LArray := TJSONArray.Create;
  try
    for var LValue in AValues do
      LArray.AddElement(LValue.ToJsonObject);
    Result := LArray.ToJSON;
  finally
    LArray.Free;
  end;
end;

procedure TMessageMiddleWare.Handle(AContext: TMiddlewareContext;
  const AChain: TMiddlewareChain);
begin
  Logger.LogDebug('TMessageMiddleWare before: ' + AContext.Message.ToJson());
  try
    AChain.Next(AContext);
  finally
    Logger.LogDebug('TMessageMiddleWare after: ' + JSONArrayToString(AContext.Produced));
  end;
end;

{ TCallToolMiddleware }

function TCallToolMiddleware.Handle(AContext: TMiddlewareContext;
  AParams: TCallToolRequestParams; const AChain: TCallToolChain): TBaseResult;
begin
  Result := AChain.Next(AContext, AParams);
  Result.ResultMeta.ServerInfo.Name := 'Middleware test: call/tool';
end;

{ TDiscoverMiddleware }

function TDiscoverMiddleware.Handle(AContext: TMiddlewareContext;
  AParams: TRequestMetaParams; const AChain: TDiscoverChain): TDiscoverResult;
begin
  Result := AChain.Next(AContext, AParams);
  var LMCPConnectInfo := TJSONObject.Create;
  LMCPConnectInfo.AddPair('implementation', 'MCPConnect');
  LMCPConnectInfo.AddPair('branch', 'feature/2026-07-28');
  Result.ResultMeta.AdditionalData.AddPair('dev.mcpconnect/info', LMCPConnectInfo);
end;

{ TMCPMiddleware }

function TMCPMiddleware.Handle(AContext: TMiddlewareContext;
  AParams: TRequestMetaParams; const AChain: TMCPChain): TBaseResult;
begin
  Logger.LogDebug('>> TMCPMiddleware: ' + AContext.Method);

  Result := AChain.Next(AContext, AParams);
  Result.ResultMeta.ServerInfo.Name := 'MyMCPServer';
  Result.ResultMeta.ServerInfo.Version := '1.0.0';
end;

{ TDiagnosticMiddleware }

function TDiagnosticMiddleware.Handle(AContext: TMiddlewareContext;
  AParams: TRequestMetaParams; const AChain: TMCPChain): TBaseResult;
begin
  var LElapsedMilliseconds: Int64;
  var LStopwatch := TStopwatch.StartNew;
  try
    Result := AChain.Next(AContext, AParams);
  finally
    LElapsedMilliseconds := LStopwatch.ElapsedMilliseconds;
  end;
  var LDiagnostics := TJSONObject.Create;
  LDiagnostics.AddPair('elapsedMilliseconds', LElapsedMilliseconds);
  Result.ResultMeta.AdditionalData.AddPair('dev.mcpconnect/diagnostics', LDiagnostics);

  var LResponse: TMCPTransportResponse;
  if AContext.TryFind<TMCPTransportResponse>(LResponse) then
  begin
    LResponse.SetHeader('x-test', 'sss');
  end;
end;

end.
