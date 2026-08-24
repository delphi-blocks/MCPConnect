param(
    [Parameter(Mandatory = $true)]
    [string]$ProjectId,

    [Parameter(Mandatory = $true)]
    [string]$DatasetId,

    [string]$Endpoint = "http://127.0.0.1:8080/mcp"
)

$ErrorActionPreference = "Stop"
$scriptRoot = Split-Path -Parent $MyInvocation.MyCommand.Path

function Invoke-McpRequest {
    param(
        [string]$FileName,
        [bool]$IncludeProtocolHeader = $true
    )

    $body = Get-Content -Raw (Join-Path $scriptRoot $FileName)
    # Replace complete JSON string literals so quotes, slashes, and control
    # characters in PowerShell input remain valid JSON rather than becoming
    # raw request text.
    $body = $body.Replace(
        '"__PROJECT_ID__"',
        ($ProjectId | ConvertTo-Json -Compress)
    )
    $body = $body.Replace(
        '"__DATASET_ID__"',
        ($DatasetId | ConvertTo-Json -Compress)
    )

    $arguments = @(
        "-sS",
        "-X", "POST",
        $Endpoint,
        "-H", "Content-Type: application/json",
        "-H", "Accept: application/json, text/event-stream"
    )
    if ($IncludeProtocolHeader) {
        $arguments += @("-H", "MCP-Protocol-Version: 2025-11-25")
    }
    $arguments += @("--data-raw", $body)

    Write-Host ""
    Write-Host "=== $FileName ==="
    & curl.exe @arguments
    if ($LASTEXITCODE -ne 0) {
        throw "curl.exe failed for $FileName with exit code $LASTEXITCODE"
    }
    Write-Host
}

Invoke-McpRequest -FileName "initialize.json" -IncludeProtocolHeader $false
Invoke-McpRequest -FileName "notifications-initialized.json"
Invoke-McpRequest -FileName "tools-list.json"
Invoke-McpRequest -FileName "call-list-datasets.json"
Invoke-McpRequest -FileName "call-describe-dataset.json"
