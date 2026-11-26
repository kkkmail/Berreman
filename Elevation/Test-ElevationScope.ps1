<#
.SYNOPSIS
    Verifies elevation scope selection without running analyzers or changing Git.
#>
#Requires -Version 5.1
[CmdletBinding()]
param([Parameter(Mandatory = $true)][string] $OutputDirectory)

Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'
$repoRoot = [IO.Path]::GetFullPath((Join-Path $PSScriptRoot '..'))
$outputRoot = [IO.Path]::GetFullPath($OutputDirectory)
$normalized = $outputRoot.Replace('\', '/')
if (-not $normalized.StartsWith($repoRoot.Replace('\', '/') + '/specs/', [StringComparison]::OrdinalIgnoreCase) -or
    $normalized -notmatch '/\.artifacts/') {
    throw 'OutputDirectory must be inside a spec artifacts folder.'
}
$fixture = Join-Path $outputRoot ('fixture-' + [Guid]::NewGuid().ToString('N'))
$utf8 = New-Object Text.UTF8Encoding($false)
function Write-Fixture([string] $Relative, [string] $Text) {
    $path = Join-Path $fixture $Relative
    [void][IO.Directory]::CreateDirectory((Split-Path -Parent $path))
    [IO.File]::WriteAllText($path, $Text.Replace("`r`n", "`n"), $utf8)
}
Write-Fixture 'Berreman/Berreman.slnx' @'
<Solution>
  <Project Path="Core/Core.fsproj" />
  <Project Path="App/App.fsproj" />
  <Project Path="Independent/Independent.fsproj" />
  <Project Path="UnitTests/UnitTests.fsproj" />
  <Project Path="../MathNetNumerics/Vendor.fsproj" />
</Solution>
'@
Write-Fixture 'Berreman/Core/Core.fsproj' '<Project><ItemGroup><Compile Include="Core.fs" /></ItemGroup></Project>'
Write-Fixture 'Berreman/App/App.fsproj' '<Project><ItemGroup><Compile Include="App.fs" /><ProjectReference Include="../Core/Core.fsproj" /></ItemGroup></Project>'
Write-Fixture 'Berreman/Independent/Independent.fsproj' '<Project><ItemGroup><Compile Include="Independent.fs" /></ItemGroup></Project>'
Write-Fixture 'Elevation/elevation-known-findings.json' '{"version":1,"findings":[]}'
$source = Join-Path $PSScriptRoot 'Run-ElevationGate.ps1'
Copy-Item -LiteralPath $source -Destination (Join-Path $fixture 'Elevation/Run-ElevationGate.ps1')
Write-Fixture 'invoke.ps1' @'
param([string] $CasePath)
Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'
$global:scopeCase = Get-Content -LiteralPath $CasePath -Raw | ConvertFrom-Json
[Environment]::SetEnvironmentVariable('ASG_ARC_BASE_COMMIT', [string]$scopeCase.arcBase, 'Process')
function global:git {
    if ($args -contains 'status') {
        $global:LASTEXITCODE = [int]$global:scopeCase.statusExit
        return @($global:scopeCase.dirty)
    }
    if ($args -contains 'diff') {
        if ([string]$args[-1] -ne ([string]$global:scopeCase.expectedBase + '...HEAD')) {
            throw 'The driver compared against a different origin.'
        }
        $global:LASTEXITCODE = [int]$global:scopeCase.diffExit
        return @($global:scopeCase.committed)
    }
    throw 'Unexpected Git invocation.'
}
$gateArgs = @{ ExcludeRules = @('ELEV004') }
if ($scopeCase.scopeOnly) { $gateArgs['ScopeOnly'] = $true }
foreach ($property in $scopeCase.parameters.PSObject.Properties) { $gateArgs[$property.Name] = $property.Value }
& (Join-Path $PSScriptRoot 'Elevation/Run-ElevationGate.ps1') @gateArgs
exit $LASTEXITCODE
'@

$core = 'Berreman/Core/Core.fsproj'
$app = 'Berreman/App/App.fsproj'
$independent = 'Berreman/Independent/Independent.fsproj'
$all = @($app, $core, $independent)
$origin = 'a' * 40
$cases = @(
    @{ name = 'standalone-clean'; expected = $all },
    @{ name = 'standalone-reports'; dirty = @(' M handoff.md'); expected = $all },
    @{ name = 'standalone-tests'; dirty = @(' M Berreman/UnitTests/Test.fs'); expected = $all },
    @{ name = 'standalone-mixed'; dirty = @(' M Berreman/Independent/Independent.fs'); committed = @('Berreman/Core/Core.fs'); expected = $all },
    @{ name = 'arc-committed-with-reports'; arcBase = $origin; dirty = @(' M handoff.md'); committed = @('Berreman/Core/Core.fs'); expected = @($app, $core) },
    @{ name = 'arc-mixed-untracked'; arcBase = $origin; dirty = @('?? Berreman/Independent/Independent.fs'); committed = @('Berreman/Core/Core.fs'); expected = $all },
    @{ name = 'explicit-base-union'; parameters = @{ Base = $origin }; dirty = @(' M Berreman/Independent/Independent.fs'); committed = @('Berreman/Core/Core.fs'); expected = $all },
    @{ name = 'explicit-base-no-dependents'; parameters = @{ Base = $origin; NoDependents = $true }; committed = @('Berreman/Core/Core.fs'); expected = @($core) },
    @{ name = 'explicit-project'; parameters = @{ Project = @('Core') }; expected = @($app, $core) },
    @{ name = 'explicit-project-no-dependents'; parameters = @{ Project = @('Core'); NoDependents = $true }; expected = @($core) },
    @{ name = 'explicit-all'; parameters = @{ All = $true }; expected = $all },
    @{ name = 'global-trigger'; parameters = @{ Base = $origin }; committed = @('Directory.Build.targets'); expected = $all },
    @{ name = 'unreadable-git'; parameters = @{ Base = $origin }; statusExit = 128; expected = $all },
    @{ name = 'invalid-base'; parameters = @{ Base = $origin }; diffExit = 128; exit = 3 },
    @{ name = 'invalid-project'; parameters = @{ Project = @('Missing') }; exit = 3 },
    @{ name = 'invalid-arc-origin'; arcBase = 'HEAD'; exit = 3 },
    @{ name = 'conflicting-explicit-base'; arcBase = $origin; parameters = @{ Base = ('b' * 40) }; exit = 3 },
    @{ name = 'empty-inspection-is-not-verdict'; parameters = @{ Base = $origin }; dirty = @(' M handoff.md'); expected = @() }
)
$results = @()
foreach ($case in $cases) {
    $inputCase = @{
        name = $case.name; parameters = @{}; dirty = @(); committed = @();
        statusExit = 0; diffExit = 0; arcBase = ''; expectedBase = $origin; scopeOnly = $true
    }
    foreach ($key in @('parameters', 'dirty', 'committed', 'statusExit', 'diffExit', 'arcBase')) {
        if ($case.ContainsKey($key)) { $inputCase[$key] = $case[$key] }
    }
    $casePath = Join-Path $fixture ($case.name + '.json')
    [IO.File]::WriteAllText($casePath, ($inputCase | ConvertTo-Json -Depth 5), $utf8)
    $output = @(& powershell -NoProfile -ExecutionPolicy Bypass -File (Join-Path $fixture 'invoke.ps1') -CasePath $casePath 2>&1)
    $code = $LASTEXITCODE
    $text = ($output | ForEach-Object { [string]$_ }) -join "`n"
    [IO.File]::WriteAllText((Join-Path $outputRoot ($case.name + '.log')), $text + "`n", $utf8)
    $expectedExit = 0
    if ($case.ContainsKey('exit')) { $expectedExit = $case.exit }
    if ($code -ne $expectedExit) { throw "$($case.name): exit $code, expected $expectedExit`n$text" }
    if ($text -match '(?m)^ELEV-GATE: OK\b') { throw "$($case.name): scope inspection emitted a success verdict" }
    if ($code -eq 0) {
        $actual = @([regex]::Matches($text, '(?m)^ELEV-SCOPE: (.+)$') | ForEach-Object { $_.Groups[1].Value.Trim() } | Sort-Object)
        $expected = @($case.expected | Sort-Object)
        if (($actual -join "`n") -ne ($expected -join "`n")) { throw "$($case.name): unexpected scope`n$text" }
        if ($text -notmatch 'SCOPE-ONLY') { throw "$($case.name): missing inspection marker" }
    }
    $results += @{ name = $case.name; passed = $true; exit = $code }
}
$summary = @{ passed = $true; cases = $results; driverSha256 = (Get-FileHash -LiteralPath $source -Algorithm SHA256).Hash.ToLowerInvariant() }
[IO.File]::WriteAllText((Join-Path $outputRoot 'results.json'), ($summary | ConvertTo-Json -Depth 5).Replace("`r`n", "`n") + "`n", $utf8)
Write-Host "SCOPE-REGRESSION: $($results.Count) passed; no analyzer or repository Git mutation."
