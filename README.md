<p align="center">
  <img src="logo-white.png" alt="MCPConnect" width="400" height="400" />
</p>

# 🔌 MCPConnect: A Delphi MCP Server Library

**A powerful, attribute-driven framework for building Model Context Protocol (MCP) Servers in Delphi.**

-----

## ✨ What is MCP?

The Model Context Protocol (MCP) is an open standard for connecting large language models (LLMs) to external tools and data.

It enables AI models to go beyond their training data by accessing new information, performing actions, and interacting with tools and databases.

With MCP servers you can:
* Provide functionality through `Tools` (used to execute code or otherwise produce a side effect)
* Expose data through `Resources` (used to load information into the LLM’s context)
* Define interaction through `Prompts` (reusable templates for LLM interactions)

## ⚡ Highlights

**Delphi MCP Connect (MCPConnect)** is a lightweight yet robust framework designed to drastically simplify the creation of **Model Context Protocol (MCP) Servers** using Embarcadero Delphi. By leveraging the power of **Attributes**, the framework allows developers to re-use existing business logic and standard Delphi classes, turning them into protocol-aware server components with minimal boilerplate code.
MCPConnect handles the serialization, routing, and context management required for the server-side implementation of the MCP protocol.

- 🛡️**Type safety** - Define your tool arguments as native delphi class or records, have mcp-connect handle the rest.
- 🚛 **Transports** - Built-in HTTP (WebBroker, Indy) and STDIO transports for both stateless and persistent connections.
- 🗂️ **Session Management** - Built-in stateful session support across requests with automatic cleanup and custom session data.
- ⚡ **Low boilerplate** - mcp-connect generates all the MCP endpoints for you apart from your tools, prompts and resources.


-----

## 🚀 Key Features

  * **Attribute-Driven Development:** Simply register classes to automatically discover tools, resources, and prompts using the **`[McpTool], [McpResource], [McpPrompt]`** attributes to expose specific methods.
  * **Standard Code Re-use:** Easily expose existing business logic classes without heavy modification or complex inheritance hierarchies.
  * **Automatic Routing:** The framework automatically scans and registers methods decorated with the appropriate attributes, handling all request routing.
  * **Easy-to-use** classes for tools, prompts, and resources
  * **Session Management:** Thread-safe session support with configurable timeout, automatic cleanup, and support for both generic (TJSONObject) and custom typed session data. Sessions are automatically injected via `[Context]` attribute.
  * **API-Key** authentication for http transport (more to be implemented)
  * **JSON-RPC** MCPConnect contains a JSON-RPC library (`JRPC`) a comprehensive, high-performance **JSON-RPC 2.0** library built specifically for Delphi.
 *  **Automatic JSON Schema generation** - Using the powerful Neon TSchemaGenaerator, MCPConnect support any Delphi type as parameter or result. 
  

## 📡What is JSON-RPC?

JSON-RPC is a stateless, light-weight remote procedure call (RPC) protocol. Primarily this specification defines several data structures and the rules around their processing. It is transport agnostic in that the concepts can be used within the same process, over sockets, over http, or in many various message passing environments. It uses JSON (RFC 4627) as data format and it is designed to be simple!

### JRPC for Delphi
Inside MCPConnect you can find a complete implementation of the JSON-RPC v2.0 protocol that can be used independently of MCPConnect for all types of Delphi projects. This library empowers you to focus purely on your application logic, allowing you to define your remote APIs using simple Delphi class methods and attributes. Whether you are creating a client to consume external RPC services or exposing your own high-performance server methods, **JRPC** makes complex distributed computing simple, declarative, and fast.

The main features of JRPC are:

* **Automatic Marshaling**: Seamless conversion of Delphi objects into JSON-RPC requests and responses.
* **Broad Delphi types support**: Using Neon, JRPC supports virtually every Delphi type as Request parameters or result
* **Protocol Compliance**: Full adherence to the JSON-RPC 2.0 specification.

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
  MCPConnect.MCP.Server.Api, // This registers the standard MCP API
  MCPConnect.Transport.WebBroker,
  MCPConnect.Configuration.MCP,
  MCPConnect.Content.Writers.RTL,
  MCPConnect.Content.Writers.VCL,

  Demo.HelpDeskService; // Unit with your MCP classes

// Create the JSON-RPC Server
FJRPCServer := TJRPCServer.Create(Self);

FJRPCServer
  .Plugin.Configure<IMCPConfig>

    .Server
      .SetName('delphi-mcp-server')
      .SetVersion('2.0.0')
      .SetCapabilities([Tools, Resources])  // Declare which capabilities to expose
      .RegisterWriter(TMCPImageWriter)       // Register content writers for complex types
      .RegisterWriter(TMCPStreamWriter)
    .BackToMCP

    .Resources
      .SetBasePath(GetCurrentDir + '\data')  // Base path for static file resources
      .RegisterClass(TWeatherResource)        // Classes with [McpResource] methods
      .RegisterClass(TMyApp)                  // Classes with [McpApp] methods
      .RegisterFile('docs\readme.md', 'Documentation')  // Static file resources
    .BackToMCP

    .Tools
      .RegisterClass(THelpDeskService)  // Classes with [McpTool] methods
    .BackToMCP;

// Create and configure the Dispatcher
FJRPCDispatcher := TJRPCDispatcher.Create(Self); // Self should be the TWebModule
FJRPCDispatcher.PathInfo := '/mcp';  // Set the endpoint path
FJRPCDispatcher.Server := FJRPCServer;  // Connect to the server
```

The configuration is split into three sections:

- **`.Server`** — server identity, declared capabilities, and content writers
- **`.Resources`** — resource classes and static files
- **`.Tools`** — tool classes

Each section ends with `.BackToMCP` to return to the root config builder.

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
    ): TContentList;
    
    // This method is NOT exposed because it lacks the [McpTool] attribute
    procedure InternalStuff;
  end;
```

-----------------------------

### 3. Working with Sessions

MCPConnect provides built-in session management for maintaining stateful interactions across multiple requests. Sessions are thread-safe, automatically managed, and can store both generic JSON data or custom typed properties.

#### Configuring Session Support

Add session configuration to your server setup:

```delphi
uses
  MCPConnect.Configuration.Session,
  MCPConnect.Configuration.MCP,
  MCPConnect.Session.Core;

FJRPCServer
  .Plugin.Configure<ISessionConfig>
    .SetLocation(TSessionIdLocation.Header)  // or Cookie
    .SetHeaderName('Mcp-Session-Id')         // Default for MCP
    .SetTimeout(30)                           // Minutes
    .SetSessionClass(TSessionData)            // Or your custom class
  .ApplyConfig

  .Plugin.Configure<IMCPConfig>
    .Server
      .SetName('delphi-mcp-server')
      .SetVersion('2.0.0')
      .SetCapabilities([Tools])
    .BackToMCP
    .Tools
      .RegisterClass(TShoppingCartTool)  // Your session-aware tool
    .BackToMCP;
```

**Session Behavior by Transport:**
- **HTTP (WebBroker/Indy)**: Session ID passed via header or cookie. Server returns `Mcp-Session-Id` header on first request.
- **STDIO**: Implicit session per connection - no session ID needed.

#### Using Sessions in Your Tools

Sessions are automatically injected into your tool classes using the `[Context]` attribute:

**Option 1: Generic JSON Storage (TSessionData)**

```delphi
type
  TShoppingCartTool = class
  private
    [Context]
    FSession: TSessionData;  // Automatically injected
  public
    [McpTool('cart_add', 'Add item to shopping cart')]
    function AddToCart(
      [McpParam('item_id')] const AItemId: string;
      [McpParam('quantity')] AQuantity: Integer
    ): string;
  end;

implementation

function TShoppingCartTool.AddToCart(const AItemId: string;
  AQuantity: Integer): string;
var
  LCart: TJSONObject;
begin
  // Get or create cart in session
  if not FSession.Data.TryGetValue<TJSONObject>('cart', LCart) then
  begin
    LCart := TJSONObject.Create;
    FSession.Data.AddPair('cart', LCart);
  end;

  // Add item
  LCart.AddPair(AItemId, TJSONNumber.Create(AQuantity));
  Result := Format('Added %d x %s to cart', [AQuantity, AItemId]);
end;
```

**Option 2: Custom Typed Session**

For better type safety, create a custom session class:

```delphi
type
  TCartItem = class
  private
    FItemId: string;
    FQuantity: Integer;
  public
    property ItemId: string read FItemId write FItemId;
    property Quantity: Integer read FQuantity write FQuantity;
  end;

  TShoppingSession = class(TSessionBase)
  private
    FCart: TObjectDictionary<string, TCartItem>;
  public
    property Cart: TObjectDictionary<string, TCartItem> read FCart;

    constructor Create;
    destructor Destroy; override;
  end;

constructor TShoppingSession.Create;
begin
  inherited;  // No parameters needed
  FCart := TObjectDictionary<string, TCartItem>.Create([doOwnsValues]);
end;

// In your tool class:
type
  TShoppingCartTool = class
  private
    [Context]
    FSession: TShoppingSession;  // Typed session!
  public
    [McpTool('cart_add', 'Add item to cart')]
    function AddToCart(const AItemId: string; AQuantity: Integer): string;
  end;

function TShoppingCartTool.AddToCart(const AItemId: string;
  AQuantity: Integer): string;
var
  LItem: TCartItem;
begin
  // Type-safe access
  if FSession.Cart.TryGetValue(AItemId, LItem) then
    LItem.Quantity := LItem.Quantity + AQuantity
  else
  begin
    LItem := TCartItem.Create;
    LItem.ItemId := AItemId;
    LItem.Quantity := AQuantity;
    FSession.Cart.Add(AItemId, LItem);
  end;

  Result := Format('Added %d x %s', [AQuantity, AItemId]);
end;
```

Don't forget to register your custom session class in the configuration:

```delphi
.SetSessionClass(TShoppingSession)  // Use your custom class
```

-----------------------------

### 4. Resources

Resources let you expose data that an LLM can read. MCPConnect supports three kinds:

- **Class-based resources** — methods decorated with `[McpResource]` that return dynamic content
- **Class-based MCP Apps** — methods decorated with `[McpApp]` that return a UI (e.g. HTML) rendered by the client
- **Static file resources** — files served directly from disk

#### Defining a Resource Class

```delphi
type
  TWeatherResource = class
  public
    [McpResource('weather-resource', 'text://weather', 'text/csv', 'Current weather data')]
    function GetWeatherInfo: string;
  end;
```

The `[McpResource]` attribute takes `(name, uri, mimeType, description)`.

#### Defining an MCP App

MCP Apps are UI resources served via a `ui://` URI scheme. The client (if it supports it) renders the returned content as an interactive widget.

```delphi
type
  TMyApp = class
  public
    [McpApp('ui://my-app/index.html', 'ui://my-app/index.html', 'An interactive UI panel')]
    function GetUI: string;
  end;

function TMyApp.GetUI: string;
begin
  Result := TFile.ReadAllText(TPath.Combine(TPath.GetAppPath, 'data', 'my-app.html'));
end;
```

The `[McpApp]` attribute takes `(name, uri, description)`.

#### Linking a Tool to an App

A tool can declare that it has an associated MCP App using the `app=` annotation in `[McpTool]`:

```delphi
[McpTool('get_tickets', 'List available tickets', 'app=ui://my-app/index.html')]
function GetTickets: TTickets;
```

This tells the client that the tool result can be rendered inside the specified app UI.

#### Registering Resources and Static Files

Resources (both class-based and app-based) and static files are all registered in the `.Resources` section:

```delphi
.Resources
  .SetBasePath(GetCurrentDir + '\data')
  .RegisterClass(TWeatherResource)   // [McpResource] class
  .RegisterClass(TMyApp)             // [McpApp] class
  .RegisterFile('readme.md', 'Documentation')         // static text file
  .RegisterFile('docs\guide.pdf', 'User Guide')       // static binary file
.BackToMCP
```

Remember to declare `Resources` in `.SetCapabilities`:

```delphi
.Server
  .SetCapabilities([Tools, Resources])
.BackToMCP
```

-----------------------------

### 5. Organizing Tools with Scopes

When building larger MCP servers with multiple tool classes, you can assign a **scope** (namespace prefix) to a class using the `[McpScope]` attribute. This avoids name conflicts and produces a cleaner, more organized API.

#### Why Use Scopes?

- **Avoid conflicts**: Multiple tool classes can have methods with the same name
- **Clear organization**: Group related tools logically
- **Better API structure**: Tools are exposed as `scope_toolname` (e.g., `test_double_or_nothing`)

#### Defining a Scoped Tool Class

```delphi
[McpScope('auth')]
TAuthService = class
public
  [McpTool('login', 'Authenticate user')]
  function Login([McpParam('username')] AUser: string): Boolean;

  [McpTool('logout', 'Logout user')]
  function Logout: Boolean;
end;
```

Exposed tool names will be `auth_login` and `auth_logout`.

#### Multiple Scoped Classes

```delphi
// Classes
[McpScope('auth')]    TAuthService    = class ... end;
[McpScope('tickets')] TTicketService  = class ... end;
[McpScope('users')]   TUserService    = class ... end;

// Registration (no namespace parameter needed)
FJRPCServer
  .Plugin.Configure<IMCPConfig>
    .Tools
      .RegisterClass(TAuthService)     // auth_login, auth_logout
      .RegisterClass(TTicketService)   // tickets_list, tickets_create
      .RegisterClass(TUserService)     // users_get, users_update
    .BackToMCP;
```

**Important**: Tool names must match the MCP pattern `^[a-zA-Z0-9_-]{1,64}$` (only alphanumeric, underscore, hyphen).

#### Tool Annotations

`[McpTool]` accepts an optional third string parameter for key-value annotations:

```delphi
[McpTool('my_tool', 'Description', 'app=ui://my-app/index.html,category=demo')]
function MyTool: string;
```

Supported built-in annotations:

| Key | Example | Meaning |
|-----|---------|---------|
| `app` | `app=ui://my-app/index.html` | Links tool to an MCP App UI |
| `disabled` | `disabled` | Hides the tool from the tools list |

-----------------------------

### 6. Connecting LLM Clients to Your MCP Server

Once your MCP server is running, you need to configure your LLM client to connect to it. Below are configuration examples for popular clients.

#### Prerequisites

Before configuring any client, ensure:
1. Your MCP server is running and accessible (e.g., `http://localhost:8080/mcp`)
2. You know the authentication token if your server requires one
3. The endpoint path matches your `TJRPCDispatcher.PathInfo` setting

#### LM Studio Configuration

LM Studio supports HTTP-based MCP servers natively. Add the following configuration to your LM Studio settings:

**Configuration file location:**
- Windows: `%USERPROFILE%\.lmstudio\mcp.json`
- macOS/Linux: `~/.lmstudio/mcp.json`

**Configuration:**
```json
{
  "mcpServers": {
    "delphi-mcp-server": {
      "url": "http://localhost:8080/mcp",
      "headers": {
        "Authorization": "Bearer my-secret-token"
      }
    }
  }
}
```

**Configuration Parameters:**
- `delphi-mcp-server`: A unique identifier for your server (can be any name)
- `url`: The full URL to your MCP server endpoint
- `headers`: Optional HTTP headers (e.g., for authentication)

After saving the configuration, restart LM Studio to load the new MCP server.

#### Claude Desktop Configuration

Claude Desktop currently requires an intermediate tool called `mcp-remote` to connect to HTTP-based MCP servers, as it doesn't support HTTP transport natively yet.

##### Step 1: Test the Connection (Recommended)

Before configuring Claude Desktop, verify that `mcp-remote` can connect to your server:

```bash
npx mcp-remote http://localhost:8080/mcp --header "Authorization: Bearer my-secret-token"
```

If the connection is successful, you should see your server's capabilities listed.

##### Step 2: Configure Claude Desktop

**Configuration file location:**
- Windows: `%APPDATA%\Claude\claude_desktop_config.json`
- macOS: `~/Library/Application Support/Claude/claude_desktop_config.json`

**Configuration:**
```json
{
  "mcpServers": {
    "my-demo-server": {
      "command": "C:\\Program Files\\nodejs\\npx",
      "args": [
        "-y",
        "mcp-remote",
        "http://localhost:8080/mcp",
        "--header",
        "Authorization: Bearer my-secret-token"
      ]
    }
  }
}
```

**Configuration Parameters:**
- `my-demo-server`: A unique identifier for your server
- `command`: Path to the Node.js `npx` executable
  - Windows: `C:\\Program Files\\nodejs\\npx` (note the double backslashes)
  - macOS/Linux: `/usr/local/bin/npx` or `npx` (if in PATH)
- `args`: Arguments passed to `npx`:
  - `-y`: Auto-confirm package installation
  - `mcp-remote`: The bridge tool for HTTP transport
  - URL to your MCP server
  - `--header`: Optional authentication header

##### Step 3: Restart Claude Desktop

After saving the configuration, restart Claude Desktop to load the MCP server connection.

-----

## Testing

MCPConnect servers can be tested using different approaches, depending on your testing needs and preferences.

### Low-Level Testing with HTTP Clients

For low-level protocol testing, you can use standard HTTP clients like **Bruno** or **Postman** to send JSON-RPC requests directly to your MCP server endpoint.

**Example test files for Bruno** are already available in the `demo/api` directory of this repository. These files demonstrate how to structure JSON-RPC calls for testing various MCP operations.

### Testing with MCPJam Inspector

For a more specialized testing experience, you can use **MCPJam Inspector**, a tool specifically designed for testing and debugging MCP servers. MCPJam provides a web-based interface that makes it easy to explore your server's capabilities and test its tools interactively.

#### Quick Start with MCPJam

To launch MCPJam Inspector, simply run:

```bash
npx @mcpjam/inspector@latest
```

This command will:
1. Download the latest version of MCPJam Inspector
2. Start the local server
3. Open a web interface where you can add and test your MCP server

From the web interface, you can add your Delphi MCP server by providing its endpoint URL (e.g., `http://localhost:8080/mcp`) and start testing your tools immediately.

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
