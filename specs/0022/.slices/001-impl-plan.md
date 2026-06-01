# 001 — impl-plan: Solution scaffolding & assembly layout (Part A)

## Goal

Add the five `OpticalConstructor.*` F# projects (Domain, Storage,
Optimization, Ui, App) plus the gated `OpticalConstructor.Tests` host to
`Berreman/Berreman.slnx`, wire their project/package references, and land
placeholder-only modules so the solution builds green at Release/x64.
No domain types, no orchestration, no UI surfaces — scaffold only.

## Approach

1. Create `Berreman/OpticalConstructor/` with one subfolder per project.
2. Author each `.fsproj` modelled on the existing repo convention
   (`net10.0`, `Platforms x64`, `Debug|x64`/`Release|x64` groups with
   `--warnaserror+:25 --platform:x64`, `PlatformTarget x64`).
3. Reference graph (following the **numbered R-1 requirements** as the
   normative source; see Gotchas for the Files-in-scope discrepancy):
   - Domain → `Berreman.fsproj`, `Analytics.fsproj` (no UI/storage pkgs).
   - Storage → Domain + `Softellect.Sys` (Berreman/Analytics arrive
     transitively through Domain).
   - Optimization → Domain only + `alglib.net`.
   - Ui → Domain, Storage, Optimization + public MIT Avalonia / FuncUI /
     ScottPlot / OpenTK / WebView2 / Plotly.NET NuGet — **never** the
     `Avalonia.FuncUI.Clone`.
   - App → Ui (minimal `[<EntryPoint>]`).
   - Tests → Domain, Storage, Optimization, Ui + xUnit; one trivial Fact.
4. Register all six projects in `Berreman.slnx` with `<Platform Project="x64" />`.
5. Build `Berreman.slnx -c Release`; iterate on package versions until green.
6. Run `unit-tests` (BerremanTests untouched) and `constructor-unit-tests`.

## Pinned package versions (resolved against nuget.org this round)

- Avalonia / Avalonia.Desktop / Avalonia.Themes.Fluent `11.3.4`
  (ScottPlot.Avalonia 5.1.58 → Avalonia 11.3.4; FuncUI 1.6.0 → 11.3.0;
  pin the higher to avoid a downgrade).
- Avalonia.FuncUI `1.6.0`, ScottPlot.Avalonia `5.1.58`, OpenTK `4.9.4`,
  Microsoft.Web.WebView2 `1.0.3967.48`, Plotly.NET `5.1.0`.
- alglib.net `3.19.0` (netstandard2.0/2.1 → net10.0 compatible).
- Softellect.Sys `10.0.101.41` (matches BerremanRunner's Softellect ref).
- FSharp.SystemTextJson `1.4.36`, FSharp.Data `8.1.14`,
  JsonSchema.Net `9.2.1`, ClosedXML `0.105.0`.

## Files to modify

- New: six `.fsproj` + placeholder `.fs` under `Berreman/OpticalConstructor/`.
- Edit: `Berreman/Berreman.slnx` (register six projects, x64).

## Risks

- **Package-resolution drift** is the load-bearing risk (per spec). Mitigate
  by adding packages incrementally and keeping placeholder modules empty so a
  failed reference is isolated. Avalonia 12 exists on nuget but FuncUI/ScottPlot
  track 11.3.x — pin 11.3.4, do **not** pull Avalonia 12.
- The `build` gate's `stdout_match` forbids the lowercase token `error` in
  build output; MSBuild's `Error(s)` summary is capitalised, so a clean build
  passes. Watch for any package warning text containing lowercase `error`.

## Out of scope (deferred to later slices)

Domain types, storage logic, optimization impl, UI pages, the FuncUI clone
reference + linking-mechanism decision (AC-A8 stays NOT-YET-RUN), the
`.gitignore` sidecar edit (slice 012), and any edit to `.gates/**`.
