unit MCPServer.Middleware;

interface

uses
  System.Classes, System.SysUtils, System.JSON,

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

  TCallToolMiddleware = class(TMiddleware, ICallToolMiddleware)
  public
    function Handle(AContext: TMiddlewareContext; AParams: TCallToolRequestParams; const AChain: TCallToolChain): TBaseResult;
  end;

  TDiscoverMiddleware = class(TMiddleware, IDiscoverMiddleware)
  public
    function Handle(AContext: TMiddlewareContext; AParams: TRequestMetaParams; const AChain: TDiscoverChain): TDiscoverResult;
  end;

implementation

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
  Result.ResultMeta.ServerInfo.Name := 'Middleware test: server/discoveer';
end;

end.
