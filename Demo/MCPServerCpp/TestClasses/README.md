# C++Builder MCPConnect Test Classes Sample

This project is a **C++Builder (VCL)** demo porting the Delphi `TestClasses` demo to C++Builder. It provides a comprehensive test suite for verifying **MCPConnect**, **JSON-RPC 2.0**, and **Neon** persistence operations within C++Builder applications.

---

## Features

- **JSON-RPC 2.0 Testing**:
  - Request serialization and deserialization with positional and named parameters.
  - Response generation and result/error parsing.
  - JSON-RPC ID variations (string, integer, null).
- **Delphi RTTI & Tool Dispatching**:
  - Dynamic discovery and invocation of methods via Delphi RTTI (`System::Rtti::TRttiContext`).
  - Parameter binding and return value conversion for C++ classes.
- **Neon Persistence in C++**:
  - Custom entity serialization (`PersonEntity`) using Neon persistence configurations.
- **Delphi-C++ Generic Interop**:
  - Uses `CppTestClassesBridge.pas` to interface with Delphi generic collections (`TThreadedQueue<T>`) and MCP message queues from C++Builder without generic syntax limitations.
- **Interactive VCL Test Suite**:
  - Category buttons for triggering individual test cases.
  - Output memo showing generated JSON-RPC payloads, deserialized data, and execution results.

---

## Prerequisites

1. **RAD Studio / C++Builder 13+**
   - **Win64 Modern (`Win64x`)** is recommended.
   - **Win32** is also supported.
2. **MCPConnect and Dependencies**
   - Ensure the required runtime packages are built:
     1. `Neon` (`Libs/Neon/Packages/11AndLater/Neon.dproj`)
     2. `Logify` (`Libs/Logify/Packages/Logify.dproj`)
     3. `JOSE` (`Libs/JWT/Packages/11AndLater/JOSE.dproj`)
     4. `MCPConnect` (`Packages/11AndLater/MCPConnect.dproj`)

---

## Building the Project

1. Open `CppTestClasses.cbproj` in RAD Studio / C++Builder.
2. Select your target platform: **Windows 64-bit (Modern)** (`Win64x`) or **Windows 32-bit**.
3. Select configuration (**Debug** or **Release**).
4. Click **Project > Build CppTestClasses** (The build will first compile `CppTestClassesBridge.pas` to generate the bridge header, then compile all C++ units).

---

## Running the Demo

1. Launch `CppTestClasses.exe`.
2. Click on the categories in the left-hand navigation panel to run tests:
   - **JSON-RPC**: Request (Positional / Named), Request Deserialization, Response, Response Deserialization.
   - **MCP Tools**: Single Tool, Tool List, Call Tool Params, RTTI Call.
   - **Initialization**: Initialize Request, Initialize Result.
   - **Messages & Queues**: Message creation, reading, and queue processing.
   - **Snippets / Misc**: View additional serialization samples and snippet tests.
3. Review the serialized JSON and test results logged in the main text area.

---

## Architecture

- `CppTestClasses.cpp`: Application entry point and package linkage.
- `CppTestClassesBridge.pas`: Delphi bridge providing helper functions for generic queues and record helpers.
- `FormMain.h` / `FormMain.cpp`: Main form hosting the test categories, RTTI invocations, and log display.
- `FormMisc.h` / `FormMisc.cpp`: Auxiliary test form for message queue and ID tests.
- `FormSnippets.h` / `FormSnippets.cpp`: Form for viewing sample code snippets and JSON outputs.
- `PersonEntity.h` / `PersonEntity.cpp`: Entity class used to test Neon persistence with custom classes.
