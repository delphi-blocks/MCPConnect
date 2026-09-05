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
    procedure OnMessage(AContext: TMiddlewareContext; const AChain: TMessageChain);
  end;

  TCallToolMiddleware = class(TMiddleware, ICallToolMiddleware)
  public
    function OnCallTool(AContext: TMiddlewareContext; AParams: TCallToolRequestParams; const AChain: TCallToolChain): TBaseResult;
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

procedure TMessageMiddleWare.OnMessage(AContext: TMiddlewareContext;
  const AChain: TMessageChain);
begin
  Logger.LogDebug('TMessageMiddleWare before: ' + AContext.Message.ToJson());
  try
    AChain.Next(AContext);
  finally
    Logger.LogDebug('TMessageMiddleWare after: ' + JSONArrayToString(AContext.Produced));
  end;
end;

{ TCallToolMiddleware }

function TCallToolMiddleware.OnCallTool(AContext: TMiddlewareContext;
  AParams: TCallToolRequestParams; const AChain: TCallToolChain): TBaseResult;
begin
  Result := AChain.Next(AContext, AParams);
  Result.ResultMeta.ServerInfo.Name := 'Middleware test';
end;

end.
