unit JRPC.Configuration.Neon;

interface

uses
  System.Classes, System.SysUtils,
  Neon.Core.Persistence,

  JRPC.Configuration.Core;

type
  IJRPCNeonConfig = interface(IJRPCConfiguration)
    ['{DAA7F996-C465-4D01-8B70-0C94F85C013A}']
    function SetNeonConfig(ANeonConfig: INeonConfiguration): IJRPCNeonConfig;
  end;

  [Implements(IJRPCNeonConfig)]
  TJRPCNeonConfig = class(TJRPCConfiguration, IJRPCNeonConfig)
  private
    FNeonConfig: INeonConfiguration;
  public
    function SetNeonConfig(ANeonConfig: INeonConfiguration): IJRPCNeonConfig;

    property NeonConfig: INeonConfiguration read FNeonConfig write FNeonConfig;
  end;

implementation

{ TJRPCNeonConfig }

function TJRPCNeonConfig.SetNeonConfig(
  ANeonConfig: INeonConfiguration): IJRPCNeonConfig;
begin
  FNeonConfig := ANeonConfig;
  Result := Self;
end;

initialization
  TJRPCConfigClassRegistry.Instance.RegisterConfigClass(TJRPCNeonConfig);

end.
