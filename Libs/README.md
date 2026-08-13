# Dependencies

1. [Neon Library](https://github.com/paolo-rossi/delphi-neon)
2. [Logify Library](https://github.com/delphi-blocks/Logify)
3. [JOSE Library](https://github.com/paolo-rossi/delphi-jose-jwt) — in `JOSE`, required
   by `MCPConnect.Security.Token.JOSE`. Remove the `DELPHI_JOSE_JWT` define from
   `Source/MCPConnect.inc` to build without it.
   
# Install

In the Libs folder:

```bash
git clone https://github.com/paolo-rossi/delphi-neon Neon
git clone https://github.com/delphi-blocks/Logify Logify
git clone https://github.com/paolo-rossi/delphi-jose-jwt JWT
```
