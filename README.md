
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
  * Neon as Serialization Engine (submodule)

### Getting Started

1.  **Clone the Repository:**
    ```bash
    git clone https://github.com/delphi-blocks/MCPConnect.git
    ```
2.  **Add to Project Path:** Add the `Source` directory of the cloned repository to your Delphi Project's search path.
3.  **Integrate:** Reference the core units, such as `MCP.Core` and `MCP.Attributes`, in your server project.

-----

## 💡 Usage Example

Creating an MCP-enabled service is as simple as adding the required attributes to a standard Delphi class and methods.

### 1\. Define Your Service (Model)

Register the class (the **Model**) and use the **`[McpTool]`** attribute for the methods (the **Tools** or actions).

```delphi
unit MCPServer;

interface

uses
  MCP.Attributes, System.SysUtils;

type
  THelpDeskService = class
  public
    // This method is published as an MCP tools
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

-----

## 📞 Contact

Your Name/Organization Name - **[@YourTwitterHandle](https://www.google.com/search?q=https://twitter.com/YourTwitterHandle)** (or other contact info)

Project Link: **[https://github.com/delphi-blocks/MCPConnect](https://github.com/delphi-blocks/MCPConnect)**

