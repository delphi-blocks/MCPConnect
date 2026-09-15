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
      * the multi-round-trip (MRTR) flows, one per kind of input request:
        delete_task (elicitation), summarize_tasks (sampling), import_tasks
        (roots, then an elicitation, three rounds) and draft_day_plan (roots and
        sampling in a single interim result)

    The sampling answers are canned: a real client would call its model where
    this script fabricates a message. What the demo is about is the envelope.

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
# version and the capabilities this client declares. The capabilities are what
# let an MRTR tool answer input_required instead of refusing with -32021
# MissingRequiredClientCapability (HTTP 400): the server checks what an interim
# result asks for against this list before the result goes out.
#
# One entry per kind of request this script can answer - elicitation for
# delete_task and import_tasks, sampling for summarize_tasks, roots for
# import_tasks, and both for draft_day_plan. Declaring "sampling" bare says
# nothing about its sub-capabilities: a request with includeContext or tools
# would need "context" / "tools" declared inside it, and none of these send either.
$meta = [ordered]@{
    'io.modelcontextprotocol/protocolVersion'    = $ProtocolVersion
    'io.modelcontextprotocol/clientCapabilities' = [ordered]@{
        elicitation = [ordered]@{}
        sampling    = [ordered]@{}
        roots       = [ordered]@{}
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

function Invoke-ToolRetry {
    <#
      The second half of an MRTR call: the same tool with the same arguments,
      plus the continuation token the server handed back and the answers to the
      requests it made. Only the current round's answers travel - what has to
      survive between rounds is in the requestState, which is the server's.
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)][string]$Name,
        [System.Collections.IDictionary]$Arguments,
        [string]$RequestState,
        [Parameter(Mandatory)][System.Collections.IDictionary]$InputResponses
    )

    if ($null -eq $Arguments) { $Arguments = [ordered]@{} }

    $params = [ordered]@{
        name      = $Name
        arguments = $Arguments
    }

    # A tool that needs nothing remembered issues no state, and a member that is
    # not there is not the same as one that is there and null
    if (-not [string]::IsNullOrEmpty($RequestState)) {
        $params['requestState'] = $RequestState
    }
    $params['inputResponses'] = $InputResponses

    Send-Mcp -Method 'tools/call' -Name $Name -Params $params
}

function Show-Ask($Response) {
    <#
      What an input_required result says: the continuation token, and the
      request filed under each key. The method is what tells the client what to
      do with it - render a form, call a model, or list its roots. Returns the
      result, or $null when the call failed instead of asking.
    #>
    if ($null -eq $Response.Json) {
        Write-Host $Response.Body -ForegroundColor DarkGray
        return $null
    }

    if ($Response.Json.PSObject.Properties['error']) {
        Write-Host ('error ' + $Response.Json.error.code + ': ' + $Response.Json.error.message) -ForegroundColor Red
        return $null
    }

    $result = $Response.Json.result
    Write-Host ('resultType   : ' + $result.resultType)

    if ($result.requestState) {
        $state = [string]$result.requestState
        if ($state.Length -gt 56) { $state = $state.Substring(0, 56) + '...' }
        Write-Host ('requestState : ' + $state)
    }

    if ($result.inputRequests) {
        foreach ($request in $result.inputRequests.PSObject.Properties) {
            Write-Host ('  key "' + $request.Name + '" -> ' + $request.Value.method)
        }
    }

    return $result
}

function New-SamplingReply([string]$Text) {
    <#
      The answer to a sampling/createMessage request. A real client would call
      its model here; the script fabricates the message, because what the demo
      is about is the envelope: one message, plus which model produced it and
      why it stopped.
    #>
    [ordered]@{
        role       = 'assistant'
        content    = [ordered]@{ type = 'text'; text = $Text }
        model      = 'demo-canned-model'
        stopReason = 'endTurn'
    }
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

# --- MRTR: the other two request kinds ---------------------------------------
# delete_task above asks the *user* something. The three calls below ask the
# *client*: for its model (sampling/createMessage), for the folders it is
# willing to expose (roots/list), and for both at once.
if (-not $SkipMrtr) {

    # The workspace this client is willing to expose. A real client would name
    # the folders its user has opened; the script makes one so import_tasks has
    # something to find.
    $workspace = Join-Path ([IO.Path]::GetTempPath()) 'mcp-import-demo'
    $null = New-Item -ItemType Directory -Path $workspace -Force
    @(
        '# Imported list',
        '- Write the release notes',
        '- [ ] Review the OAuth demo',
        '* Update the README'
    ) | Set-Content -Path (Join-Path $workspace 'todo-import.md') -Encoding UTF8

    $rootUri = 'file:///' + [uri]::EscapeUriString(($workspace -replace '\\', '/'))

    # --- sampling ------------------------------------------------------------
    Write-Section 'MRTR: summarize_tasks asks the client to sample a model'

    # Something to summarize: the tool answers a value rather than an input
    # request when the list is empty, since a round trip costs the client a
    # model call
    Write-Host (Tool-Text (Invoke-Tool 'add_task' ([ordered]@{
        title       = 'Draft the release notes'
        description = 'Added so the sampling demo has something to summarize'
    })))

    $summaryArgs = [ordered]@{ style = 'two short bullet points' }
    $ask = Invoke-Tool 'summarize_tasks' $summaryArgs
    $askResult = Show-Ask $ask

    if ($null -ne $askResult -and $askResult.inputRequests.summary) {
        # What the server asked the model for. Everything here is advisory
        # except maxTokens: the client picks the model and may ignore the lot.
        $sampling = $askResult.inputRequests.summary.params
        Write-Host ('systemPrompt : ' + $sampling.systemPrompt)
        Write-Host ('maxTokens    : ' + $sampling.maxTokens + '   temperature: ' + $sampling.temperature)
        Write-Host ('preferences  : hints=' + ((@($sampling.modelPreferences.hints) | ForEach-Object { $_.name }) -join ',') +
                    '  speed=' + $sampling.modelPreferences.speedPriority +
                    '  cost=' + $sampling.modelPreferences.costPriority +
                    '  intelligence=' + $sampling.modelPreferences.intelligencePriority)
        Write-Host ('includeContext: ' + $sampling.includeContext)

        $done = Invoke-ToolRetry -Name 'summarize_tasks' -Arguments $summaryArgs `
            -RequestState $askResult.requestState -InputResponses ([ordered]@{
                summary = New-SamplingReply '- One task is waiting on a review. - The rest are new.'
            })
        Show-Text (Tool-Text $done)
    }

    # --- roots, then a form --------------------------------------------------
    Write-Section 'MRTR: import_tasks - roots, then a form, then the import'
    Write-Host ('workspace    : ' + $workspace)

    # Round one: the server has nowhere to read from, so it asks
    $ask = Invoke-Tool 'import_tasks'
    $askResult = Show-Ask $ask

    if ($null -ne $askResult) {
        # Round two: the roots. Only this round's answer travels - what the
        # server needs to remember is in the requestState it just handed back,
        # signed, because it names the folder the next round will read.
        $pick = Invoke-ToolRetry -Name 'import_tasks' -RequestState $askResult.requestState `
            -InputResponses ([ordered]@{
                where = [ordered]@{
                    roots = @( [ordered]@{ uri = $rootUri; name = 'Demo workspace' } )
                }
            })
        $pickResult = Show-Ask $pick

        if ($null -ne $pickResult -and $pickResult.inputRequests.file) {
            # The form the server built from what it found there: the question
            # names the candidates, because the options were only known once
            # the roots were in.
            Write-Host ('question     : ' + $pickResult.inputRequests.file.params.message)

            # Round three: the file name, as an ordinary elicitation answer
            $done = Invoke-ToolRetry -Name 'import_tasks' -RequestState $pickResult.requestState `
                -InputResponses ([ordered]@{
                    file = [ordered]@{
                        action  = 'accept'
                        content = [ordered]@{ fileName = 'todo-import.md' }
                    }
                })
            Write-Host (Tool-Text $done)
        }
    }

    # --- both in one round trip ----------------------------------------------
    Write-Section 'MRTR: draft_day_plan asks for roots and sampling in one result'

    $ask = Invoke-Tool 'draft_day_plan'
    $askResult = Show-Ask $ask

    if ($null -ne $askResult) {
        # Two requests, two answers, one retry: the keys are what pairs them up.
        # No requestState here - this tool needs nothing remembered between the
        # rounds, so it issued none.
        $done = Invoke-ToolRetry -Name 'draft_day_plan' -InputResponses ([ordered]@{
            where = [ordered]@{
                roots = @( [ordered]@{ uri = $rootUri; name = 'Demo workspace' } )
            }
            draft = New-SamplingReply 'Start with the review, then the release notes, then the README.'
        })
        Show-Text (Tool-Text $done)
    }
}


$script:Http.Dispose()
Write-Host ''
Write-Host 'Done.' -ForegroundColor Green
