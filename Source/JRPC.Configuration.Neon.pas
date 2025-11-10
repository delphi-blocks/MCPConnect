unit JRPC.Configuration.Neon;

interface

uses
  System.Classes, System.SysUtils,

  JRPC.Configuration.Core;

type
  IJRPCNeonConfig = interface(IJRPCConfiguration)
    ['{DAA7F996-C465-4D01-8B70-0C94F85C013A}']
  end;

  [Implements(IJRPCNeonConfig)]
  TJRPCNeonConfig = class(TJRPCConfiguration, IJRPCNeonConfig)
  end;

implementation

initialization
  TJRPCConfigClassRegistry.Instance.RegisterConfigClass(TJRPCNeonConfig);

end.
