# PowerShell syntax highlighting test
function Greet {
    param(
        [string]$Name = "World"
    )
    Write-Host "Hello, $Name!"
}

$config = @{
    Version = "1.0"
    Enabled = $true
    Count   = 42
}

Greet -Name "World"

Get-ChildItem -Path . -Filter "*.txt" | ForEach-Object {
    Write-Host "File: $($_.Name)"
}
