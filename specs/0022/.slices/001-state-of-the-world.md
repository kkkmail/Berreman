# 001 — State of the world: Solution scaffolding & assembly layout (Part A)

## Where we are

Slice 001 is the load-bearing scaffold for the whole 0022 arc. It adds the five
`OpticalConstructor.*` F# projects (Domain, Storage, Optimization, Ui, App) plus
the gated `OpticalConstructor.Tests` host to `Berreman.slnx`, wires their
project/package references, and lands placeholder-only modules so the solution
builds green at Release/x64. No domain types, orchestration, or UI surfaces land
here — later slices (002 Domain foundation, 003 Storage core, then the feature
parts) fill the projects.

## What's working

- Add five `OpticalConstructor.*` projects + the gated `OpticalConstructor.Tests`
  host to `Berreman.slnx`, each pinned to the x64 solution platform.
- Wire the build-order reference graph (engine → orchestration → UI → tooling)
  with placeholder-only F# modules; no engine module forked or shadowed.
- Consume the public MIT `Avalonia.FuncUI` NuGet for the UI project, leaving the
  audit-gated FuncUI clone unreferenced and AC-A8 NOT-YET-RUN.
- Add `Berreman/Directory.Build.props` defaulting the platform to x64 so the
  `dotnet test` unit-tests gate resolves the same output tree the slnx build emits.
- Build green at Release/x64; all three roster gates pass locally.

## Tests

- `build` — PASS (exit 0, "Build succeeded.", 0 errors; no lowercase `error`).
- `unit-tests` — PASS (exit 0, 70 passed / 5 skipped / 75 total; BerremanTests
  untouched). Baseline `berreman_unit_tests = 70`.
- `constructor-unit-tests` — PASS (exit 0, 1 passed). The trivial scaffold
  `[<Fact>]` makes the `exit_code: 0` gate green from this first round.

None deferred. Logs under `.artifacts/001-*.log`.

## Architecture

- Reference graph follows the **numbered R-1 requirements** as normative where
  they conflict with the descriptive "Files in scope" bullets: Domain →
  Berreman + Analytics; Storage → Domain + Softellect.Sys; Optimization → Domain
  only (+ alglib.net); Ui → Domain/Storage/Optimization (+ MIT Avalonia stack);
  App → Ui; Tests → Domain/Storage/Optimization/Ui. Engine projects still reach
  Storage/Optimization transitively through Domain.
- **Buildable FuncUI interpretation** (§A.6/§A.9): the UI project consumes the
  public, MIT-licensed `Avalonia.FuncUI` 1.6.0 + Avalonia 11.3.4 NuGet — a
  distinct artefact from the audit-gated clone at `C:\GitHub\Avalonia.FuncUI.Clone\`.
  The clone stays unreferenced and the linking-mechanism choice stays unresolved
  until the §A.9 audit passes. Every UI-touching slice (this one, 004, 005, 007,
  011, 012, 014–016) relies on this.
- `Berreman/Directory.Build.props` normalises the default build/test platform to
  x64 (the slnx's only platform), making default-platform invocations land in the
  same `bin\x64\...` tree as the solution build. This is the file the `build`
  gate's `applies_to` already anticipates.

## Deferred

- All real content: Domain types/units spine (slice 002), Storage JSON/schema/
  `.binz` IO (slice 003), the ALGLIB-backed optimization interface (Part H), UI
  pages/MVU model+update+view (later UI slices).
- The FuncUI-clone audit (AC-A8) — recorded NOT-YET-RUN; the clone reference and
  linking-mechanism decision are intentionally not taken here.
- `.gitignore` edit for `*.binz`/`*.autosave` sidecars — owned by slice 012.
- Trimming the WebView2-induced WPF/WindowsBase warning — left to a later UI slice
  if it matters.

## Gotchas

- `dotnet test --no-build -c Release` defaults `Platform` to `AnyCPU`
  (`bin\Release\...`) while the slnx build emits x64 (`bin\x64\Release\...`);
  without the new `Directory.Build.props` the unit-tests gate fails to find the
  test assembly for ANY slice. The props file fixes this; do not remove it.
- Spec has an internal R-1-vs-Files-in-scope contradiction on Storage/
  Optimization references — R-1 (normative) was followed; engine assemblies
  arrive transitively through Domain.
- Avalonia 12 is on nuget but FuncUI/ScottPlot track 11.3.x — keep Avalonia
  pinned at 11.3.4; bumping to 12 reintroduces a spec-0110-class build break.
- `System.Text.Json` is intentionally not pinned explicitly (framework +
  FSharp.SystemTextJson provide it); an explicit pin risks an NU1605 downgrade
  on net10.

## Changelog

- 2026-05-31 (slice 001): Scaffold the five `OpticalConstructor.*` projects +
  the gated `OpticalConstructor.Tests` host; register them in `Berreman.slnx`
  (x64); add `Berreman/Directory.Build.props` (default platform x64); land
  placeholder-only modules; build green at Release/x64 with all three gates
  passing. FuncUI clone left unreferenced (AC-A8 NOT-YET-RUN).

```yaml
gates:
  berreman_unit_tests:   70
  constructor_unit_tests: 1
```
