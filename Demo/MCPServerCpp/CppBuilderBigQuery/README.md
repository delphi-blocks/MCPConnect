# C++Builder BigQuery MCP Server Sample

This sample demonstrates how to build and host an **MCP (Model Context Protocol)** server inside a **C++Builder (VCL)** application using **MCPConnect**, exposing Google Cloud BigQuery metadata tools.

---

## Features

- **`bq_list_datasets`**: Lists the datasets available in a specified Google Cloud project.
- **`bq_describe_dataset`**: Lists tables within a dataset and retrieves their column schemas.
- **Direct REST Testing**: Built-in VCL UI to test direct REST queries without going through MCP.
- **MCP Indy HTTP Server**: Hosts an HTTP endpoint (`http://127.0.0.1:8080/mcp`) for MCP client integration.
- **C++ Native Core**: Implements BigQuery REST communication and tool handling in modern C++, using a lightweight Delphi bridge (`ServerConfigBridge.pas`) for MCPConnect configuration and RTTI registration.

---

## Prerequisites

1. **RAD Studio / C++Builder 13+**
   - **Win64 Modern (`Win64x`)** is recommended and verified.
   - **Win32** is also supported for compilation.
2. **MCPConnect and Dependencies**
   - Build the prerequisite runtime packages in order:
     1. `Neon` (`Libs/Neon/Packages/11AndLater/Neon.dproj`)
     2. `Logify` (`Libs/Logify/Packages/Logify.dproj`)
     3. `JOSE` (`Libs/JWT/Packages/11AndLater/JOSE.dproj`)
     4. `MCPConnect` (`Packages/11AndLater/MCPConnect.dproj`)
3. **Google Cloud Account**
   - A Google Cloud project with the **BigQuery API** enabled.
   - IAM permissions: `roles/bigquery.metadataViewer` and `serviceusage.services.use`.

---

## Getting the Google Cloud Access Token

BigQuery operations require a valid OAuth2 Bearer token:

### Option A: Generate a Token File (Recommended)
1. Install and initialize the [Google Cloud CLI (`gcloud`)](https://cloud.google.com/sdk/docs/install).
2. Authenticate your account:
   ```bash
   gcloud auth login
   ```
3. Generate the token file:
   ```bash
   gcloud auth print-access-token > bq_token.txt
   ```
4. Place the generated `bq_token.txt` file in the **same directory as the compiled executable** (e.g., `Win64x\Debug\bq_token.txt` or `Win32\Debug\bq_token.txt`).

> **Note**: Do not commit `bq_token.txt` to version control. Tokens are short-lived (usually 1 hour).

### Option B: Set Environment Variable
Alternatively, export the access token via environment variable before launching the application:

- **PowerShell**:
  ```powershell
  $env:BQ_ACCESS_TOKEN = (gcloud auth print-access-token)
  ```
- **Command Prompt**:
  ```cmd
  for /f "tokens=*" %i in ('gcloud auth print-access-token') do set BQ_ACCESS_TOKEN=%i
  ```

### Optional Quota Project
If your billing/quota project differs from the queried `project_id`, set the `BQ_QUOTA_PROJECT` environment variable:
```bash
$env:BQ_QUOTA_PROJECT = "my-billing-project"
```

---

## Building the Project

1. Open `BQMcpSample.cbproj` in RAD Studio / C++Builder.
2. In the **Project Manager**, select the target platform: **Windows 64-bit (Modern)** (`Win64x`) or **Windows 32-bit**.
3. Select the **Debug** or **Release** configuration.
4. Click **Project > Build BQMcpSample** (Build will compile `ServerConfigBridge.pas` to generate the `.hpp` bridge header and then build the C++ sources).

---

## Running and Testing

1. Launch `BQMcpSample.exe`.
2. **Direct REST Test**:
   - Enter your **Project ID** in the UI.
   - For describe, enter a **Dataset ID**.
   - Click **Test List Datasets** or **Test Describe Dataset** to verify direct REST communication with BigQuery.
3. **Start the MCP Server**:
   - Click **Start MCP Server**. The server will bind to `http://127.0.0.1:8080/mcp`.
4. **Test via MCP Client / cURL**:
   - Run the provided PowerShell test script in `scripts/`:
     ```powershell
     cd scripts
     .\run-curl-tests.ps1 -ProjectId "your-project-id" -DatasetId "your_dataset_id"
     ```
   - The script sends `initialize`, `notifications/initialized`, `tools/list`, and `tools/call` for both BigQuery tools.

---

## Architecture

- `BQMcpSample.cpp`: Main application entry point and package linkage.
- `BigQueryRest.h` / `BigQueryRest.cpp`: Native C++ REST client using `TNetHTTPClient` to communicate with Google BigQuery v2 REST APIs.
- `BigQueryTool.h` / `BigQueryTool.cpp`: C++ tool class registered with MCPConnect RTTI.
- `ServerConfigBridge.pas`: Delphi unit bridging generic MCP configuration and tool registration to C++Builder.
- `FormMain.h` / `FormMain.cpp`: VCL user interface for server control and direct testing.
