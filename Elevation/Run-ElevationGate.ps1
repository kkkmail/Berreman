<#
.SYNOPSIS
    Runs the Softellect elevation analyzers (the ELEV rules) over the Berreman projects a change touches, and
    reports the verdict in the shape the arc-runner parses.

.DESCRIPTION
    The rules are STL's Softellect.Analyzers, consumed as a package (spec 0048 .manual/001): its version is the
    Softellect.Analyzers row of the root Directory.Build.targets, the nupkg comes from a local feed nuget.config
    names (or the NuGet global packages folder), and the fsharp-analyzers CLI is installed at the
    FSharp.Analyzers.SDK version that package's nuspec depends on - the CLI skips an analyzer built against
    another SDK version and then reports nothing. Both are cached under Elevation/bin/, which git ignores. What
    the rules apply to is the root .elevation-analyzers.json.

    The output contract is that of STL's Analyzers/scripts/Run-ElevationGate.ps1, so the gate descriptors read
    the same lines: one "FAILED: <rule> <file>:<line>:<column> - <message>" per finding, then exactly one
    "ELEV-GATE: OK ..." or "ELEV-GATE: FAILED ..." line. Exit 0 only on OK, 1 on findings, 3 when the run itself
    failed.

    SCOPE, in order of precedence:

      -Project <name|path> ...   exactly these in-scope projects
      -All                       every in-scope project
      -Base <commit>             committed changes since this base plus the working tree and dependents
      (arc default)              the same union from ASG_ARC_BASE_COMMIT, the immutable arc origin
      (standalone default)       every in-scope project when no comparison base is available

    An unreadable git, or a change to what the rules read (.elevation-analyzers.json, a solution,
    the root Directory.Build.props or .targets) analyzes every in-scope project rather than report a green the
    run did not earn.

    KNOWN FINDINGS. Elevation/elevation-known-findings.json (STL's format) records the findings the tree had when
    the gates were set up, each with the ledger it waits on (spec 0048 .manual/002). A recorded finding counts in
    the ELEV-KNOWN summary and does not fail the run; any other finding does. An entry found fewer times than
    recorded is stale and fails, so a fix removes its entry in the same change and the list only shrinks. An entry
    is judged whenever a project compiling its file was analyzed (every entry under -All). -NoKnownFindings fails
    on every finding.

    In scope: every F# project of Berreman/Berreman.slnx except the vendored MathNetNumerics and the test
    projects, which the analyzers exempt from every rule. The analyzers read each project's references as
    built, so the build gate runs first.

.EXAMPLE
    # what the elevation gate runs
    Elevation/Run-ElevationGate.ps1 -ExcludeRules ELEV004

.EXAMPLE
    # every in-scope project, nothing suppressed
    Elevation/Run-ElevationGate.ps1 -All
#>

#Requires -Version 5.1
[CmdletBinding()]
param(
    # In-scope projects to analyze, by .fsproj stem, folder name or repo-relative path. Overrides change detection.
    [string[]] $Project = @(),

    # Analyze every in-scope project.
    [switch] $All,

    # Also treat everything changed in "<Base>...HEAD" as changed.
    [string] $Base = '',

    # Do not expand the scope to the projects that reference the changed ones.
    [switch] $NoDependents,

    # Rule ids this run does not enforce. Suppressed findings are still counted, so an exclusion is never invisible.
    [string[]] $ExcludeRules = @(),

    # Fail on the findings elevation-known-findings.json records as well.
    [switch] $NoKnownFindings,

    # Concurrent analyzer processes. Each peaks at 2-7 GB (STL spec 0200 step 089).
    [int] $Throttle = 4,

    # Where the SARIF reports land. The default is Elevation/bin/reports: run output stays in the repository's
    # git-ignored folders, never in %TEMP% (CLAUDE.md).
    [string] $OutDir = '',

    # Print at most this many FAILED lines; the summary always carries the true total.
    [int] $MaxReported = 50,

    # Print the scope this run WOULD analyze and stop; never a verdict.
    [switch] $ScopeOnly
)

Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'

$repoRoot = (Resolve-Path (Join-Path $PSScriptRoot '..')).Path
$binRoot = Join-Path $PSScriptRoot 'bin'
$solution = Join-Path $repoRoot 'Berreman\Berreman.slnx'

function Write-GateFailure([string] $Reason) {
    Write-Host "FAILED: $Reason"
}

function Exit-GateError([string] $Reason, [string] $Marker) {
    Write-GateFailure $Reason
    Write-Host "ELEV-GATE: FAILED - $Marker"
    exit 3
}

trap {
    Write-GateFailure "elevation gate crashed: $($_.Exception.Message)"
    Write-Host 'ELEV-GATE: FAILED - the gate driver did not complete'
    exit 3
}

# Printable ASCII, whitespace collapsed: the key STL's known-findings list compares messages by.
function ConvertTo-KnownFindingText([string] $Text) {
    (($Text -replace '[^\x20-\x7E]', '') -replace '\s+', ' ').Trim()
}

# Read before any analysis, so a malformed list fails in seconds, not after the run.
$knownPath = Join-Path $PSScriptRoot 'elevation-known-findings.json'
$knownEntries = @()
if (-not $NoKnownFindings -and (Test-Path -LiteralPath $knownPath)) {
    $knownDoc = Get-Content -LiteralPath $knownPath -Raw -Encoding UTF8 | ConvertFrom-Json
    if ($knownDoc.version -ne 1) { Exit-GateError "$knownPath is not version 1" 'known-findings list unreadable' }
    foreach ($e in @($knownDoc.findings)) {
        $names = @($e.PSObject.Properties.Name)
        foreach ($field in 'rule', 'file', 'message', 'ruling') {
            if (($names -notcontains $field) -or [string]::IsNullOrWhiteSpace([string] $e.$field)) {
                Exit-GateError "an entry of $knownPath has no $field - every entry names the ruling it waits on" 'known-findings list unreadable'
            }
        }
        if (($names -notcontains 'count') -or [int] $e.count -lt 1) {
            Exit-GateError "the $($e.rule) entry for $($e.file) in $knownPath needs a count of at least 1" 'known-findings list unreadable'
        }
        $knownEntries += [pscustomobject]@{
            Rule = [string] $e.rule; File = [string] $e.file; Message = (ConvertTo-KnownFindingText ([string] $e.message))
            Times = [int] $e.count; Ruling = [string] $e.ruling
        }
    }
}

function ConvertTo-RepoRelative([string] $Path) {
    $full = [System.IO.Path]::GetFullPath($Path)
    if ($full.StartsWith($repoRoot, [System.StringComparison]::OrdinalIgnoreCase)) {
        return $full.Substring($repoRoot.Length).TrimStart('\', '/').Replace('\', '/')
    }
    return $full.Replace('\', '/')
}

# --- the in-scope projects, what each compiles, and what each references ------------------------------------
$solutionDir = Split-Path -Parent $solution
$inScope = @()
foreach ($m in [regex]::Matches((Get-Content -LiteralPath $solution -Raw), 'Project Path="([^"]+\.fsproj)"')) {
    $rel = ConvertTo-RepoRelative (Join-Path $solutionDir $m.Groups[1].Value)
    if ($rel -like 'MathNetNumerics/*' -or $rel -match 'Tests\.fsproj$') { continue }
    $inScope += $rel
}
$inScope = @($inScope | Sort-Object -Unique)
if ($inScope.Count -eq 0) {
    Exit-GateError "no in-scope project was read from $(ConvertTo-RepoRelative $solution)" 'empty project set'
}

$compiledBy = @{}   # repo-relative source path (lower case) -> the in-scope projects that compile it
$references = @{}   # project -> the in-scope projects it references
foreach ($rel in $inScope) {
    $projectPath = Join-Path $repoRoot $rel
    $projectDir = Split-Path -Parent $projectPath
    $text = Get-Content -LiteralPath $projectPath -Raw
    foreach ($m in [regex]::Matches($text, '<Compile\s+(?:Include|Update)="([^"]+)"')) {
        $key = (ConvertTo-RepoRelative (Join-Path $projectDir $m.Groups[1].Value)).ToLowerInvariant()
        if (-not $compiledBy.ContainsKey($key)) { $compiledBy[$key] = @() }
        $compiledBy[$key] += $rel
    }
    $refs = @()
    foreach ($m in [regex]::Matches($text, '<ProjectReference\s+Include="([^"]+)"')) {
        $referenced = ConvertTo-RepoRelative (Join-Path $projectDir $m.Groups[1].Value)
        if ($inScope -contains $referenced) { $refs += $referenced }
    }
    $references[$rel] = $refs
}

function Get-Dependents([string[]] $Seed) {
    $set = New-Object 'System.Collections.Generic.HashSet[string]' ([System.StringComparer]::OrdinalIgnoreCase)
    foreach ($s in $Seed) { [void] $set.Add($s) }
    do {
        $grew = $false
        foreach ($p in $inScope) {
            if ($set.Contains($p)) { continue }
            foreach ($r in @($references[$p])) {
                if ($set.Contains($r)) { [void] $set.Add($p); $grew = $true; break }
            }
        }
    } while ($grew)
    return @($set)
}

# --- the change set ------------------------------------------------------------------------------------------
$git = @('--no-optional-locks', '-c', "safe.directory=$($repoRoot.Replace('\', '/'))", '-C', $repoRoot)

# Spec 0050 manual 013: committing an escalation must not narrow the arc's scope.
$arcBase = [Environment]::GetEnvironmentVariable('ASG_ARC_BASE_COMMIT')
if (-not [string]::IsNullOrWhiteSpace($arcBase)) {
    if ($arcBase -notmatch '\A(?:[0-9a-f]{40}|[0-9a-f]{64})\z') {
        Exit-GateError 'ASG_ARC_BASE_COMMIT is not a full commit hash' 'invalid arc base'
    }
    if ($Base -ne '' -and $Base -ne $arcBase) {
        Exit-GateError '-Base conflicts with the immutable ASG arc origin' 'conflicting arc base'
    }
    $Base = $arcBase
    Write-Host "ELEV-GATE: arc base = $Base"
}

function Get-ChangedPaths {
    $porcelain = & git @git status --porcelain -uall 2>$null
    if ($LASTEXITCODE -ne 0) { return $null }
    $paths = @()
    foreach ($line in @($porcelain)) {
        if ([string]::IsNullOrWhiteSpace($line)) { continue }
        $p = $line.Substring(3).Trim().Trim('"')
        if ($p -match ' -> ') { $p = ($p -split ' -> ')[1] }
        $paths += $p
    }
    if ($Base -ne '') {
        $diff = & git @git diff --name-only "$Base...HEAD" 2>$null
        if ($LASTEXITCODE -ne 0) {
            Exit-GateError "git diff against base '$Base' failed - cannot determine the change set" 'change detection failed'
        }
        $paths += @($diff | Where-Object { -not [string]::IsNullOrWhiteSpace($_) })
    }
    return , @($paths | Sort-Object -Unique)
}

# A change to what the rules read invalidates every earlier verdict; a change to this driver or a gate does not.
function Test-GlobalTrigger([string] $Path) {
    $p = $Path.Replace('\', '/')
    return ($p -eq '.elevation-analyzers.json') -or ($p -like '*.slnx') -or
           ($p -eq 'Directory.Build.props') -or ($p -eq 'Directory.Build.targets')
}

$scopeReason = ''
$targets = @()

if ($Project.Count -gt 0) {
    foreach ($wanted in $Project) {
        $w = $wanted.Replace('\', '/')
        $hit = @($inScope | Where-Object {
            $_ -eq $w -or [System.IO.Path]::GetFileNameWithoutExtension($_) -eq $wanted -or
            (Split-Path -Leaf (Split-Path -Parent $_)) -eq $wanted
        })
        if ($hit.Count -eq 0) { Exit-GateError "-Project '$wanted' matches no in-scope project" 'unknown project' }
        $targets += $hit
    }
    $scopeReason = 'explicit -Project'
}
elseif ($All) {
    $targets = $inScope
    $scopeReason = 'explicit -All'
}
elseif ($Base -eq '') {
    $targets = $inScope
    $scopeReason = 'no comparison base (all)'
}
else {
    $changed = Get-ChangedPaths
    if ($null -eq $changed) {
        Write-Host 'ELEV-GATE: NOTE - git is unavailable here; analyzing every in-scope project'
        $targets = $inScope
        $scopeReason = 'git unavailable (fail-safe: all)'
    }
    elseif (@($changed).Count -eq 0) {
        Write-Host 'ELEV-GATE: NOTE - no changed paths found; analyzing every in-scope project'
        $targets = $inScope
        $scopeReason = 'no changed paths (fail-safe: all)'
    }
    else {
        $global = @($changed | Where-Object { Test-GlobalTrigger $_ })
        if ($global.Count -gt 0) {
            Write-Host "ELEV-GATE: NOTE - $($global[0]) changed (what the rules read); analyzing every in-scope project"
            $targets = $inScope
            $scopeReason = 'analyzer configuration changed (all)'
        }
        else {
            foreach ($file in @($changed | Where-Object { $_ -match '\.(fs|fsi)$' })) {
                $key = $file.Replace('\', '/').ToLowerInvariant()
                if ($compiledBy.ContainsKey($key)) { $targets += $compiledBy[$key] }
                else { Write-Host "ELEV-GATE: NOTE - no in-scope project compiles '$file'; not analyzed" }
            }
            foreach ($file in @($changed | Where-Object { $_ -match '\.fsproj$' })) {
                $p = $file.Replace('\', '/')
                if ($inScope -contains $p) { $targets += $p }
            }
            $scopeReason = "changed files ($(@($changed).Count))"
        }
    }
}

$targets = @($targets | Sort-Object -Unique)
if ($targets.Count -gt 0 -and -not $NoDependents -and $targets.Count -lt $inScope.Count) {
    $expanded = @(Get-Dependents $targets | Sort-Object -Unique)
    if ($expanded.Count -gt $targets.Count) {
        Write-Host "ELEV-GATE: NOTE - expanded $($targets.Count) changed project(s) to $($expanded.Count) with their dependents (cross-project duplicate-name rules)"
        $targets = $expanded
    }
}

$excludedLabel = 'none'
if ($ExcludeRules.Count -gt 0) { $excludedLabel = $ExcludeRules -join ', ' }

Write-Host "ELEV-GATE: scope = $($targets.Count) of $($inScope.Count) project(s) [$scopeReason] [excluded rules: $excludedLabel]"
foreach ($t in $targets) { Write-Host "ELEV-SCOPE: $t" }
if ($ScopeOnly) {
    Write-Host 'ELEV-GATE: SCOPE-ONLY - nothing was analyzed, this run is not a verdict'
    exit 0
}
if ($targets.Count -eq 0) {
    Write-Host 'ELEV-GATE: OK - no in-scope project was touched'
    exit 0
}

# --- the analyzers and the CLI that runs them ----------------------------------------------------------------
$centralList = Get-Content -LiteralPath (Join-Path $repoRoot 'Directory.Build.targets') -Raw
$pin = [regex]::Match($centralList, '<PackageVersion\s+Include="Softellect\.Analyzers"\s+Version="(?<v>[^"]+)"')
if (-not $pin.Success) {
    Exit-GateError 'the root Directory.Build.targets declares no Softellect.Analyzers version' 'analyzers unresolved'
}
$analyzersVersion = $pin.Groups['v'].Value
$analyzersPath = Join-Path $binRoot "analyzers\$analyzersVersion"
$analyzerDll = Join-Path $analyzersPath 'Softellect.Analyzers.dll'
$nuspecPath = Join-Path $analyzersPath 'Softellect.Analyzers.nuspec'

if (-not ((Test-Path -LiteralPath $analyzerDll) -and (Test-Path -LiteralPath $nuspecPath))) {
    New-Item -ItemType Directory -Force -Path $analyzersPath | Out-Null
    $config = [xml] (Get-Content -LiteralPath (Join-Path $repoRoot 'nuget.config') -Raw)
    $nupkg = $null
    foreach ($source in @($config.configuration.packageSources.add)) {
        $value = [string] $source.value
        if ($value -match '^https?:') { continue }
        $candidate = Join-Path (Join-Path $repoRoot $value) "Softellect.Analyzers.$analyzersVersion.nupkg"
        if (Test-Path -LiteralPath $candidate) { $nupkg = $candidate; break }
    }
    $packagesRoot = $env:NUGET_PACKAGES
    if (-not $packagesRoot) { $packagesRoot = Join-Path $env:USERPROFILE '.nuget\packages' }
    $restored = Join-Path $packagesRoot "softellect.analyzers\$analyzersVersion"
    if ($nupkg) {
        Add-Type -AssemblyName System.IO.Compression.FileSystem
        $zip = [System.IO.Compression.ZipFile]::OpenRead($nupkg)
        try {
            foreach ($entry in $zip.Entries) {
                if ($entry.FullName -eq 'lib/net10.0/Softellect.Analyzers.dll') {
                    [System.IO.Compression.ZipFileExtensions]::ExtractToFile($entry, $analyzerDll, $true)
                }
                elseif ($entry.FullName -eq 'Softellect.Analyzers.nuspec') {
                    [System.IO.Compression.ZipFileExtensions]::ExtractToFile($entry, $nuspecPath, $true)
                }
            }
        }
        finally { $zip.Dispose() }
    }
    elseif (Test-Path -LiteralPath (Join-Path $restored 'lib\net10.0\Softellect.Analyzers.dll')) {
        Copy-Item -LiteralPath (Join-Path $restored 'lib\net10.0\Softellect.Analyzers.dll') -Destination $analyzerDll -Force
        Copy-Item -LiteralPath (Join-Path $restored 'softellect.analyzers.nuspec') -Destination $nuspecPath -Force
    }
    if (-not ((Test-Path -LiteralPath $analyzerDll) -and (Test-Path -LiteralPath $nuspecPath))) {
        Exit-GateError "Softellect.Analyzers $analyzersVersion is in no local feed nuget.config names and not in $packagesRoot - build STL's slow ring at that version, or move the pin" 'analyzers missing'
    }
}

$sdk = [regex]::Match((Get-Content -LiteralPath $nuspecPath -Raw), '<dependency\s+id="FSharp\.Analyzers\.SDK"\s+version="\[?(?<v>[0-9][^",\]\)]*)')
if (-not $sdk.Success) {
    Exit-GateError "the Softellect.Analyzers $analyzersVersion nuspec names no FSharp.Analyzers.SDK dependency" 'analyzer tool unresolved'
}
$toolVersion = $sdk.Groups['v'].Value
$toolDir = Join-Path $binRoot "tool\$toolVersion"
$toolPath = Join-Path $toolDir 'fsharp-analyzers.exe'
if (-not (Test-Path -LiteralPath $toolPath)) {
    $installOutput = & dotnet tool install fsharp-analyzers --version $toolVersion --tool-path $toolDir --configfile (Join-Path $repoRoot 'nuget.config') 2>&1
    if ($LASTEXITCODE -ne 0 -or -not (Test-Path -LiteralPath $toolPath)) {
        Exit-GateError "dotnet tool install fsharp-analyzers $toolVersion exited $LASTEXITCODE - $($installOutput -join ' ')" 'analyzer tool missing'
    }
}
Write-Host "ELEV-GATE: Softellect.Analyzers $analyzersVersion (Directory.Build.targets), fsharp-analyzers $toolVersion (its FSharp.Analyzers.SDK)"

# --- run -----------------------------------------------------------------------------------------------------
if ($OutDir -eq '') { $OutDir = Join-Path $binRoot 'reports' }
if (Test-Path -LiteralPath $OutDir) { Remove-Item -LiteralPath $OutDir -Recurse -Force }
New-Item -ItemType Directory -Force -Path $OutDir | Out-Null
$env:DOTNET_CLI_UI_LANGUAGE = 'en'
# STL spec 0233 .manual/008: the CLI fans every file out on an unbounded Async.Parallel; 8 cores bound its memory.
if (-not $env:DOTNET_PROCESSOR_COUNT) { $env:DOTNET_PROCESSOR_COUNT = '8' }

function Start-Analyzer([string] $Rel, [string] $Key) {
    # Started from the repository root, so every SARIF uri is repo-relative.
    Start-Process -FilePath $toolPath -WorkingDirectory $repoRoot -NoNewWindow -PassThru -ArgumentList @(
            '--project', (Join-Path $repoRoot $Rel),
            '--analyzers-path', $analyzersPath,
            '-c', 'Release', '--property', 'Platform=x64',
            '--report', (Join-Path $OutDir "$Key.sarif"), '--verbosity', 'n'
        ) -RedirectStandardOutput (Join-Path $OutDir "$Key.log") -RedirectStandardError (Join-Path $OutDir "$Key.err.log")
}

function Read-Report([string] $Key) {
    $sarif = Join-Path $OutDir "$Key.sarif"
    if (-not (Test-Path -LiteralPath $sarif)) { return $null }
    try {
        $run = (Get-Content -LiteralPath $sarif -Raw -Encoding UTF8 | ConvertFrom-Json).runs[0]
        if ($run.PSObject.Properties['results']) { return , @($run.results) }
        return , @()
    }
    catch { return $null }
}

function Test-Enforced($Finding) {
    # An ELEVnnnT finding is a third-party namesake: a warning our name stands against, never enforced.
    ($ExcludeRules -notcontains $Finding.ruleId) -and ($Finding.ruleId -notmatch '^ELEV\d+T$')
}

$queue = [System.Collections.Generic.Queue[object]]::new()
foreach ($rel in $targets) {
    $key = ($rel -replace '[\\/]', '_') -replace '\.fsproj$', ''
    $queue.Enqueue([pscustomobject]@{ Rel = $rel; Key = $key })
}

$running = @()
$outcomes = @()
$clock = [System.Diagnostics.Stopwatch]::StartNew()
while ($queue.Count -gt 0 -or $running.Count -gt 0) {
    while ($running.Count -lt $Throttle -and $queue.Count -gt 0) {
        $job = $queue.Dequeue()
        $running += [pscustomobject]@{ Job = $job; Proc = (Start-Analyzer $job.Rel $job.Key); Clock = [System.Diagnostics.Stopwatch]::StartNew() }
    }
    Start-Sleep -Milliseconds 500
    $still = @()
    foreach ($entry in $running) {
        if (-not $entry.Proc.HasExited) { $still += $entry; continue }
        # WaitForExit() is what guarantees a readable exit code and flushed streams.
        $entry.Proc.WaitForExit()
        $exitCode = $null
        try { $exitCode = [int] $entry.Proc.ExitCode } catch { $exitCode = $null }
        $results = Read-Report $entry.Job.Key
        $retried = $false
        if ($null -eq $results) {
            # A report lost under memory pressure is a property of the run, not the code (STL spec 0200 step 089):
            # one serial retry tells the two apart, and a project with no report from either still fails.
            Write-Host "ELEV-GATE: NOTE - $($entry.Job.Rel) delivered no report; re-running it alone"
            $retry = Start-Analyzer $entry.Job.Rel $entry.Job.Key
            $retry.WaitForExit()
            try { $exitCode = [int] $retry.ExitCode } catch { $exitCode = $null }
            $results = Read-Report $entry.Job.Key
            $retried = $true
        }
        $mine = @()
        if ($null -ne $results) { $mine = @($results | Where-Object { Test-Enforced $_ }) }
        $outcome = [pscustomobject]@{
            Rel        = $entry.Job.Rel
            Exit       = $exitCode
            Parsed     = ($null -ne $results)
            Mine       = $mine
            Suppressed = $(if ($null -ne $results) { @($results).Count - $mine.Count } else { 0 })
            Seconds    = [int] $entry.Clock.Elapsed.TotalSeconds
        }
        $outcomes += $outcome
        if ($outcome.Parsed) {
            $note = ''
            if ($outcome.Suppressed -gt 0) { $note = " (suppressed: $($outcome.Suppressed))" }
            if ($retried) { $note += ' (serial retry)' }
            Write-Host ("ELEV-PROJECT: {0}  findings={1}{2}  {3}s" -f $outcome.Rel, $mine.Count, $note, $outcome.Seconds)
        }
    }
    $running = $still
}
$clock.Stop()

# --- verdict -------------------------------------------------------------------------------------------------
function Get-FindingLine($Finding) {
    $uri = ''
    $line = 0
    $column = 0
    if ($Finding.PSObject.Properties['locations'] -and @($Finding.locations).Count -gt 0) {
        $location = $Finding.locations[0].physicalLocation
        $uri = [string] $location.artifactLocation.uri
        $line = [int] $location.region.startLine
        $column = [int] $location.region.startColumn
    }
    [pscustomobject]@{ Rule = [string] $Finding.ruleId; Uri = $uri; Line = $line; Column = $column; Text = [string] $Finding.message.text }
}

$toolFailures = 0
$suppressedTotal = 0
$enforced = @()
foreach ($o in ($outcomes | Sort-Object Rel)) {
    if (-not $o.Parsed) {
        Write-GateFailure "$($o.Rel) - the analyzer produced no readable SARIF report (exit $($o.Exit)); see $OutDir"
        $toolFailures++
        continue
    }
    # 0 = clean, -2 = the CLI's own "diagnostics were reported at error level"; anything else is a tool fault.
    if ($null -ne $o.Exit -and $o.Exit -ne 0 -and $o.Exit -ne -2) {
        Write-GateFailure "$($o.Rel) - fsharp-analyzers exited $($o.Exit); see $OutDir"
        $toolFailures++
        continue
    }
    $suppressedTotal += $o.Suppressed
    $enforced += @($o.Mine | ForEach-Object { Get-FindingLine $_ })
}

# A source-linked file is analyzed once per project compiling it; report each finding once.
$enforced = @($enforced | Sort-Object Uri, Line, Column, Rule, Text -Unique)

# --- known findings: New fails, Known is recorded, Stale (found fewer times than recorded) fails ---------------
$recorded = @{}
foreach ($e in $knownEntries) { $recorded["$($e.Rule)|$($e.File)|$($e.Message)"] = $e }
$used = @{}
$known = @()
$new = @()
foreach ($f in $enforced) {
    $key = "$($f.Rule)|$($f.Uri)|$(ConvertTo-KnownFindingText $f.Text)"
    $taken = 0
    if ($used.ContainsKey($key)) { $taken = $used[$key] }
    if ($recorded.ContainsKey($key) -and $taken -lt $recorded[$key].Times) { $used[$key] = $taken + 1; $known += $f }
    else { $new += $f }
}
$analyzed = New-Object 'System.Collections.Generic.HashSet[string]' ([System.StringComparer]::OrdinalIgnoreCase)
foreach ($o in @($outcomes | Where-Object { $_.Parsed })) { [void] $analyzed.Add($o.Rel) }
$stale = @()
foreach ($e in $knownEntries) {
    if ($ExcludeRules -contains $e.Rule) { continue }
    $owners = @()
    if ($compiledBy.ContainsKey($e.File.ToLowerInvariant())) { $owners = @($compiledBy[$e.File.ToLowerInvariant()]) }
    $judged = $All -or (@($owners | Where-Object { $analyzed.Contains($_) }).Count -gt 0)
    if (-not $judged) { continue }
    $key = "$($e.Rule)|$($e.File)|$($e.Message)"
    $found = 0
    if ($used.ContainsKey($key)) { $found = $used[$key] }
    if ($found -lt $e.Times) { $stale += [pscustomobject]@{ Entry = $e; Found = $found } }
}

if ($known.Count -gt 0) {
    $knownByRule = @($known | Group-Object Rule | Sort-Object Name | ForEach-Object { "$($_.Name)=$($_.Count)" })
    $rulings = @($knownEntries | ForEach-Object { $_.Ruling } | Sort-Object -Unique)
    Write-Host "ELEV-KNOWN: $($known.Count) recorded finding(s) still present, not failing ($($knownByRule -join ' ')) - $($rulings -join '; ')"
}
$printed = 0
foreach ($f in $new) {
    if ($printed -ge $MaxReported) { break }
    Write-GateFailure ("{0} {1}:{2}:{3} - {4}" -f $f.Rule, $f.Uri, $f.Line, $f.Column, $f.Text)
    $printed++
}
if ($new.Count -gt $printed) {
    Write-Host "ELEV-GATE: NOTE - $($new.Count - $printed) further finding(s) not printed; the full reports are in $OutDir"
}
$printed = 0
foreach ($st in $stale) {
    if ($printed -ge $MaxReported) { break }
    Write-GateFailure ("{0} {1} - recorded {2} time(s), found {3}: remove the fixed finding from Elevation/elevation-known-findings.json - {4}" -f $st.Entry.Rule, $st.Entry.File, $st.Entry.Times, $st.Found, $st.Entry.Message)
    $printed++
}
$byRule = @($new | Group-Object Rule | Sort-Object Name | ForEach-Object { "$($_.Name)=$($_.Count)" })
if ($byRule.Count -gt 0) { Write-Host "ELEV-RULES: $($byRule -join ' ')" }

$suffix = "in $($targets.Count) project(s), $([int] $clock.Elapsed.TotalSeconds)s [excluded rules: $excludedLabel; suppressed findings: $suppressedTotal; known findings: $($known.Count)]"
if ($toolFailures -gt 0) {
    Write-Host "ELEV-GATE: FAILED - $toolFailures project(s) did not analyze, $($new.Count) new finding(s) $suffix"
    exit 3
}
if ($new.Count -gt 0 -or $stale.Count -gt 0) {
    Write-Host "ELEV-GATE: FAILED - $($new.Count) new finding(s), $($stale.Count) stale known finding(s) $suffix"
    exit 1
}
Write-Host "ELEV-GATE: OK - 0 new finding(s) $suffix"
exit 0
