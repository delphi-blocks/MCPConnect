<#
.SYNOPSIS
    A minimal command-line MCP client for Demo/MCPServerMetrics.

.DESCRIPTION
    This is the scratchpad used while building the metrics showcase, kept as a
    repeatable demo. It speaks the raw 2026-07-28 HTTP contract - the per-request
    _meta, the Mcp-Method/Mcp-Name headers, and the client capabilities - so it
    needs no MCP client library and doubles as a reference for what a request
    looks like on the wire.

    It exercises, in order:
      * server/discover
      * tools/list
      * tools/call: metrics_snapshot, metrics_simulate_workload,
        metrics_instrument, metrics_slowest, metrics_exporters,
        metrics_cardinality, metrics_separate_provider
      * resources/read of resource://metrics/live
      * prompts/get of analyse-metrics
      * the multi-round-trip (MRTR) delete_task flow: ask, then answer

    Start Demo/MCPServerMetrics first; see the README next to this file.

.PARAMETER Url
    The MCP endpoint. Default: http://localhost:8080/

.PARAMETER ProtocolVersion
    The version stated in _meta. Default: 2026-07-28

.PARAMETER Truncate
    How many characters of each text answer to print. Default: 1200

.PARAMETER SkipMrtr
    Skip the multi-round-trip demonstration (it writes to the todo list).

.EXAMPLE
    pwsh ./MCPClient.ps1

.EXAMPLE
    pwsh ./MCPClient.ps1 -Url http://localhost:9090/ -Truncate 4000
#>
[CmdletBinding()]
param(
    [string]$Url = 'http://localhost:8080/',
    [string]$ProtocolVersion = '2026-07-28',
    [int]$Truncate = 1200,
    [switch]$SkipMrtr
)

$ErrorActionPreference = 'Stop'

# System.Net.Http is the one dependency; keep it explicit so the script also
# works in a fresh Windows PowerShell 5.1 session.
Add-Type -AssemblyName System.Net.Http -ErrorAction SilentlyContinue

# Every POST carries the request metadata 2026-07-28 requires: the protocol
# version and the capabilities this client declares. Declaring elicitation is
# what lets an MRTR tool answer input_required instead of refusing with
# -32021 MissingRequiredClientCapability (-32021 + HTTP 400).
$meta = [ordered]@{
    'io.modelcontextprotocol/protocolVersion'    = $ProtocolVersion
    'io.modelcontextprotocol/clientCapabilities' = [ordered]@{
        elicitation = [ordered]@{}
    }
}

$script:Http = [System.Net.Http.HttpClient]::new()

function Send-Mcp {
    <#
      One JSON-RPC request. Returns the HTTP status, the raw body and the parsed
      body, so the caller can print either a result or a refused request.
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)][string]$Method,
        [System.Collections.IDictionary]$Params,
        [string]$Name
    )

    if ($null -eq $Params) { $Params = @{} }
    $Params['_meta'] = $meta

    $json = ([ordered]@{
        jsonrpc = '2.0'
        id      = [int](Get-Random -Minimum 1 -Maximum 100000)
        method  = $Method
        params  = $Params
    } | ConvertTo-Json -Depth 32 -Compress)

    $request = [System.Net.Http.HttpRequestMessage]::new([System.Net.Http.HttpMethod]::Post, $Url)
    $request.Content = [System.Net.Http.StringContent]::new($json, [System.Text.Encoding]::UTF8, 'application/json')
    $request.Headers.Add('MCP-Protocol-Version', $ProtocolVersion)
    $request.Headers.Add('Mcp-Method', $Method)
    $request.Headers.Add('Accept', 'application/json')
    if ($Name) { $request.Headers.Add('Mcp-Name', $Name) }

    $response = $script:Http.SendAsync($request).GetAwaiter().GetResult()
    $text = $response.Content.ReadAsStringAsync().GetAwaiter().GetResult()

    $parsed = $null
    if (-not [string]::IsNullOrWhiteSpace($text)) {
        try { $parsed = $text | ConvertFrom-Json } catch { $parsed = $null }
    }

    [pscustomobject]@{ Status = [int]$response.StatusCode; Body = $text; Json = $parsed }
}

function Write-Section([string]$Title) {
    Write-Host ''
    $rule = '=' * [Math]::Max(0, 72 - $Title.Length)
    Write-Host "== $Title $rule" -ForegroundColor Cyan
}

function Show-Text([string]$Text) {
    if ([string]::IsNullOrEmpty($Text)) {
        Write-Host '(empty)' -ForegroundColor DarkGray
        return
    }
    if ($Text.Length -gt $Truncate) {
        Write-Host ($Text.Substring(0, $Truncate) + ' ... [truncated]')
    }
    else {
        Write-Host $Text
    }
}

function Tool-Text($Response) {
    if ($null -eq $Response.Json) { return $null }
    if ($Response.Json.PSObject.Properties['error']) { return $null }
    return $Response.Json.result.content[0].text
}

function Invoke-Tool([string]$Name, [System.Collections.IDictionary]$Arguments) {
    if ($null -eq $Arguments) { $Arguments = @{} }
    Send-Mcp -Method 'tools/call' -Name $Name -Params ([ordered]@{
        name      = $Name
        arguments = $Arguments
    })
}

# --- server/discover ---------------------------------------------------------
Write-Section 'server/discover'
$discover = Send-Mcp -Method 'server/discover'
if ($null -eq $discover.Json) {
    Write-Host ('The server did not answer at ' + $Url + '. Is it running?') -ForegroundColor Red
    Write-Host $discover.Body -ForegroundColor DarkGray
    exit 1
}
$info = $discover.Json.result._meta.'io.modelcontextprotocol/serverInfo'
Write-Host ('server    : ' + $info.name + ' ' + $info.version)
Write-Host ('versions  : ' + ($discover.Json.result.supportedVersions -join ', '))
Write-Host ('cacheScope: ' + $discover.Json.result.cacheScope + '  ttlMs: ' + $discover.Json.result.ttlMs)

# --- tools/list --------------------------------------------------------------
Write-Section 'tools/list'
$tools = Send-Mcp -Method 'tools/list'
Write-Host (@($tools.Json.result.tools | ForEach-Object { $_.name }) -join ', ')

# --- the metrics_* tools -----------------------------------------------------
Write-Section 'tools/call metrics_snapshot'
Show-Text (Tool-Text (Invoke-Tool 'metrics_snapshot'))

Write-Section 'tools/call metrics_simulate_workload (25 operations)'
$sim = Invoke-Tool 'metrics_simulate_workload' ([ordered]@{ operations = 25; regions = 'eu,us,apac' })
Show-Text (Tool-Text $sim)

Write-Section 'tools/call metrics_instrument (demo.workload / orders.created)'
Show-Text (Tool-Text (Invoke-Tool 'metrics_instrument' ([ordered]@{ meter = 'demo.workload'; name = 'orders.created' })))

Write-Section 'tools/call metrics_slowest (top 5)'
Show-Text (Tool-Text (Invoke-Tool 'metrics_slowest' ([ordered]@{ top_n = 5 })))

Write-Section 'tools/call metrics_exporters'
Show-Text (Tool-Text (Invoke-Tool 'metrics_exporters'))

Write-Section 'tools/call metrics_cardinality'
Show-Text (Tool-Text (Invoke-Tool 'metrics_cardinality'))

Write-Section 'tools/call metrics_separate_provider'
Show-Text (Tool-Text (Invoke-Tool 'metrics_separate_provider'))

# --- the live resource -------------------------------------------------------
Write-Section 'resources/read resource://metrics/live'
$live = Send-Mcp -Method 'resources/read' -Name 'resource://metrics/live' -Params ([ordered]@{
    uri = 'resource://metrics/live'
})
if ($live.Json.result.contents) {
    Show-Text $live.Json.result.contents[0].text
}
else {
    Write-Host $live.Body -ForegroundColor DarkGray
}

# --- the prompt --------------------------------------------------------------
Write-Section 'prompts/get analyse-metrics'
$prompt = Send-Mcp -Method 'prompts/get' -Name 'analyse-metrics' -Params ([ordered]@{
    name      = 'analyse-metrics'
    arguments = [ordered]@{ focus = 'latency' }
})
if ($prompt.Json.result.messages) {
    Show-Text $prompt.Json.result.messages[0].content.text
}
else {
    Write-Host $prompt.Body -ForegroundColor DarkGray
}

# --- MRTR: tools/call that answers input_required, then the retry -------------
if (-not $SkipMrtr) {
    Write-Section 'MRTR: delete_task asks, then the retry answers'

    $add = Invoke-Tool 'add_task' ([ordered]@{ title = 'Review the client demo' })
    $addText = Tool-Text $add
    Write-Host $addText

    $taskId = 1
    if ($addText -match '#(\d+)') { $taskId = [int]$Matches[1] }

    # First call: no answer yet, so the server answers input_required with the
    # questions it needs filled and the state it wants back.
    $ask = Invoke-Tool 'delete_task' ([ordered]@{ task_id = $taskId })
    Write-Host ('resultType   : ' + $ask.Json.result.resultType)
    Write-Host ('requestState : ' + $ask.Json.result.requestState)
    if ($ask.Json.result.inputRequests) {
        $keys = @($ask.Json.result.inputRequests.PSObject.Properties.Name)
        Write-Host ('keys         : ' + ($keys -join ', '))
    }

    # Retry: the same call, with requestState and the answer under the key the
    # server chose. Answering anything but 'accept' would take a different path.
    $inputResponses = [ordered]@{}
    $inputResponses['delete'] = [ordered]@{
        action  = 'accept'
        content = [ordered]@{ confirm = $true }
    }

    $done = Send-Mcp -Method 'tools/call' -Name 'delete_task' -Params ([ordered]@{
        name           = 'delete_task'
        arguments      = [ordered]@{ task_id = $taskId }
        requestState   = $ask.Json.result.requestState
        inputResponses = $inputResponses
    })
    Write-Host (Tool-Text $done)
}

$script:Http.Dispose()
Write-Host ''
Write-Host 'Done.' -ForegroundColor Green
