# Dependencies

1. [Delphi-JRPC Library](https://github.com/delphi-blocks/Delphi-JRPC) — in `JRPC`, the
   JSON-RPC 2.0 layer MCPConnect builds on.
2. [Neon Library](https://github.com/paolo-rossi/delphi-neon) — in `Neon`.
3. [Logify Library](https://github.com/delphi-blocks/Logify) — in `Logify`.
4. [JOSE Library](https://github.com/paolo-rossi/delphi-jose-jwt) — in `JWT`, required
   by `MCPConnect.Security.Token.JOSE`. Remove the `DELPHI_JOSE_JWT` define from
   `Source/MCPConnect.inc` to build without it.

# Install

In the Libs folder (the target folder name matters — it is what the project search
paths expect):

```bash
git clone https://github.com/delphi-blocks/Delphi-JRPC JRPC
git clone https://github.com/paolo-rossi/delphi-neon Neon
git clone https://github.com/delphi-blocks/Logify Logify
git clone https://github.com/paolo-rossi/delphi-jose-jwt JWT
```
