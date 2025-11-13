<p align="center">
  <img src="logo-white.png" alt="MCPConnect" width="300" height="300" />
</p>

# 🔌 Delphi MCP Connect Library (MCPConnect)

**A powerful, attribute-driven framework for building Model Context Protocol (MCP) Servers in Delphi.**

-----


## ✨ What is MCP?

The Model Context Protocol (MCP) is an open standard for connecting large language models (LLMs) to external tools and data.

It enables AI models to go beyond their training data by accessing new information, performing actions, and interacting with tools and databases. 

## ⚡ Highlights

**Delphi MCP Connect (MCPConnect)** is a lightweight yet robust framework designed to drastically simplify the creation of **Model Context Protocol (MCP) Servers** using Embarcadero Delphi. By leveraging the power of **Attributes**, the framework allows developers to re-use existing business logic and standard Delphi classes, turning them into protocol-aware server components with minimal boilerplate code.
MCPConnect handles the serialization, routing, and context management required for the server-side implementation of the MCP protocol.

- 🛡️**Type safety** - Define your tool arguments as native delphi class or records, have mcp-connect handle the rest. Automatic schema generation, deserialization, error handling etc.
- 🚛 **Transports** - Use the built-in transport: HTTP for stateless communication (with stdio already in development).
- ⚡ **Low boilerplate** - mcp-connect generates all the MCP endpoints for you apart from your tools, prompts and resources.


-----

## 🚀 Features

  * **Attribute-Driven Development:** Simply register classes to automatically discover tools, resources, and prompts using the **`[McpTool], [McpResource], [McpPrompt]`** attributes to expose specific methods.
  * **Standard Code Re-use:** Easily expose existing business logic classes without heavy modification or complex inheritance hierarchies.
  * **Automatic Routing:** The framework automatically scans and registers methods decorated with the appropriate attributes, handling all request routing.

-----

## 🛠️ Installation

### Requirements

  * Delphi **10 or newer** (support for Attributes is essential).
  * Neon as Serialization Engine (https://github.com/paolo-rossi/delphi-neon)

### Getting Started

1.  **Clone Neon the Repository:**
    ```bash
    git clone https://github.com/paolo-rossi/delphi-neon
    ```
3.  **Clone MCPConnect the Repository:**
    ```bash
    git clone https://github.com/delphi-blocks/MCPConnect.git
    ```
3.  **Add to Project Path:** Add the `Source` directory of the cloned repositories to your Delphi Project's search path.
4.  **Integrate:** Reference the core units, such as `MCPConnect.JRPC.Core` and `MCPConnect.MCP.Attributes`, in your server project.

-----

## 💡 Usage Example

Creating an MCP-enabled service is as simple as adding the required attributes to a standard Delphi class and methods.

### 1. Create a New MCP Server Application

To get started with your MCP server, you'll need to set up a WebBroker application and configure the JSON-RPC components.

#### Step 1: Create a WebBroker Application

1. In Delphi, create a new **WebBroker Application** project (File → New → Other → Web → Web Server Application).
2. Choose your preferred web server type (standalone, ISAPI, Apache, etc.). For development, a standalone application is recommended.

> **Note:** While you can use Indy components directly, WebBroker provides a simpler and more straightforward approach for HTTP-based MCP servers.

#### Step 2: Configure the Server Components

In your WebModule's `OnCreate` event or constructor, create and configure the `TJRPCServer` and `TJRPCDispatcher` components:

```delphi
uses
  MCPConnect.JRPC.Server, 
  MCPConnect.MCP.Server.Api, // This register the standard MCP API
  MCPConnect.Transport.WebBroker, 
  MCPConnect.Configuration.MCP,
  
  Demo.HelpDeskService; // Unit with your MCP classes

// Create the JSON-RPC Server
FJRPCServer := TJRPCServer.Create(Self);
FJRPCServer
  .Plugin.Configure<IMCPConfig>
    .SetServerName('delphi-mcp-server')
    .SetServerVersion('2.0.0')
    .SetToolClass(THelpDeskService)  // Register your tool class here
    .ApplyConfig;

// Create and configure the Dispatcher
FJRPCDispatcher := TJRPCDispatcher.Create(Self); // Self should be the TWebModule
FJRPCDispatcher.PathInfo := '/mcp';  // Set the endpoint path
FJRPCDispatcher.Server := FJRPCServer;  // Connect to the server
```

#### Step 3: Understand the Automatic Integration

The `TJRPCDispatcher` integrates seamlessly with WebBroker through Delphi's standard component ownership mechanism:

1. **Automatic Registration**: When you create the dispatcher with `TWebModule` as its owner (via the constructor parameter), it automatically registers itself with the WebBroker framework.

2. **Request Routing**: For each incoming HTTP request, WebBroker checks all registered dispatchers to determine which one should handle it based on the `PathInfo` property.

3. **No Manual Wiring Needed**: Since the dispatcher was created with `Self` (the WebModule) as owner in Step 2, the connection is already established.

```delphi
// This line (from Step 2) does all the wiring:
FJRPCDispatcher := TJRPCDispatcher.Create(Self); // Self = TWebModule
// ↑ The owner parameter registers the dispatcher automatically
```

**That's it!** Your MCP server is now ready to accept JSON-RPC requests at the configured endpoint.

**Example**: If your server runs on port 8080, requests sent to `http://localhost:8080/mcp` will be automatically routed to your registered MCP tools.

### 2. Define Your Service (Model)

Register the class (the **Model**) and use the **`[McpTool]`** attribute for the methods (the **Tools** or actions).

```delphi
unit Demo.HelpDeskService;

interface

uses
  System.SysUtils, 
  MCPConnect.MCP.Attributes;

type
  THelpDeskService = class
  public
    // This method is published as an MCP tool
    [McpTool('doclist', 'List all the available documents')]
    function ListDocument(
      [McpParam('category', 'Document Category')] const ACategory: string
    ): TObjectList<TPDFDocument>;
    
    // This method is NOT exposed because it lacks the [McpTool] attribute
    procedure InternalStuff;
  end;
```

-----

## 🤝 Contributing

We welcome contributions\! If you have suggestions, bug reports, or want to contribute code, please:

1.  Fork the repository.
2.  Create a new branch (`git checkout -b feature/AmazingFeature`).
3.  Commit your changes (`git commit -m 'Add some AmazingFeature'`).
4.  Push to the branch (`git push origin feature/AmazingFeature`).
5.  Open a **Pull Request**.

-----

## 📄 License

Distributed under the **MIT License**. See `LICENSE` for more information.
