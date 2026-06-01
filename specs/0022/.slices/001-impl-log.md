# 001 — impl-log: Solution scaffolding & assembly layout (Part A)

## Progress

- [x] Read system prompt, project prompt, slice spec, gate roster.
- [x] Resolved package versions against nuget.org.
- [x] Wrote impl-plan + this log.
- [x] Created six `OpticalConstructor.*` projects + placeholder modules.
- [x] Registered all six projects in `Berreman.slnx` (x64).
- [x] Added `Berreman/Directory.Build.props` to normalise the default platform
      to x64 (fixes the `dotnet test --no-build` AnyCPU/x64 path mismatch).
- [x] `build` gate green at Release/x64 (exit 0, no lowercase `error`).
- [x] `unit-tests` gate green (70 passed).
- [x] `constructor-unit-tests` gate green (1 passed).
- [x] State-of-the-world written.

## Files modified

New (under `Berreman/OpticalConstructor/`):
- `OpticalConstructor.Domain/OpticalConstructor.Domain.fsproj` + 7 placeholder
  modules in compile order: `Units.fs`, `BeamTree.fs`, `CurvedElements.fs`,
  `SourceSpec.fs`, `SourceCombination.fs`, `DispersionModels.fs`,
  `MaterialLibrary.fs`.
- `OpticalConstructor.Storage/OpticalConstructor.Storage.fsproj` + `Storage.fs`.
- `OpticalConstructor.Optimization/OpticalConstructor.Optimization.fsproj` + `Optimization.fs`.
- `OpticalConstructor.Ui/OpticalConstructor.Ui.fsproj` + `Ui.fs`.
- `OpticalConstructor.App/OpticalConstructor.App.fsproj` + `Program.fs` (entry point).
- `OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj` + `ScaffoldTests.fs`
  (one trivial passing `[<Fact>]`).

Edited:
- `Berreman/Berreman.slnx` — registered the six new projects, each
  `<Platform Project="x64" />`, before `OpticalModel` (build order preserved).
- `Berreman/Directory.Build.props` — NEW; defaults `Platform` to `x64` when
  unspecified (see Gotchas).

NOT touched: `OpticalModel.fsproj` / `OpticalModel/Primitives.fs`,
`BerremanTests/**`, any `.gates/**` descriptor, `.gitignore`.

## Decisions

- **Reference graph follows the numbered R-1 requirements** (normative) where
  they conflict with the descriptive "Files in scope" bullets:
  - Domain → `Berreman.fsproj`, `Analytics.fsproj` (no UI/storage pkgs).
  - Storage → Domain + `Softellect.Sys` (R-1 item 2). Berreman/Analytics reach
    Storage transitively through Domain, so the Files-in-scope intent is met.
  - Optimization → Domain only (R-1 item 3) + `alglib.net`.
  - Ui → Domain, Storage, Optimization + public MIT Avalonia/FuncUI stack.
  - App → Ui; Tests → Domain, Storage, Optimization, Ui.
- **Single buildable FuncUI interpretation**: public MIT `Avalonia.FuncUI`
  1.6.0 NuGet, NOT the audit-gated clone. `OpticalConstructor.Ui.fsproj`
  contains no reference to `C:\GitHub\Avalonia.FuncUI.Clone\`; the
  linking-mechanism choice is unresolved (AC-A8 audit = NOT-YET-RUN).
- **Pinned versions** (resolved this round): Avalonia / Avalonia.Desktop /
  Avalonia.Themes.Fluent `11.3.4` (ScottPlot.Avalonia 5.1.58 needs 11.3.4,
  FuncUI 1.6.0 needs 11.3.0 — pin the higher to avoid an NU1605 downgrade);
  ScottPlot.Avalonia `5.1.58`; Plotly.NET `5.1.0`; OpenTK `4.9.4`;
  Microsoft.Web.WebView2 `1.0.3967.48`; alglib.net `3.19.0`;
  Softellect.Sys `10.0.101.41`; FSharp.SystemTextJson `1.4.36`;
  FSharp.Data `8.1.14`; JsonSchema.Net `9.2.1`; ClosedXML `0.105.0`.
- **System.Text.Json**: not pinned explicitly — it arrives via
  FSharp.SystemTextJson and the net10 shared framework. An explicit pin on
  net10 risks an NU1605 downgrade against the framework copy; the spec's intent
  (STJ available to Storage) is satisfied without the redundant reference.
- Domain's 7 placeholder modules are intentionally empty (one doc comment each)
  to honour "registers … in compile order" while staying within
  "placeholder/empty compilable modules only". Slice 002 fills them.

## Testing state

All three roster gates pass locally (Release/x64):

- `build` — `dotnet build Berreman.slnx -c Release -nologo -v:m` from
  `Berreman/`: exit 0, "Build succeeded.", 0 Error(s); no lowercase `error`
  token (the `stdout_match` negative-lookahead passes case-sensitively).
  Log: `.artifacts/001-build.log`.
- `unit-tests` — `dotnet test --no-build -c Release` from `Berreman/BerremanTests/`:
  exit 0, Failed 0 / Passed 70 / Skipped 5 / Total 75. BerremanTests untouched.
  Log: `.artifacts/001-unit-tests.log`.
- `constructor-unit-tests` — `dotnet test …/OpticalConstructor.Tests.fsproj -c Release`
  from repo root: exit 0, Passed 1. Log: `.artifacts/001-constructor-unit-tests.log`.

Remaining warnings are benign and pre-existing or expected: NU1902 (log4net
transitive from Softellect, also in BerremanRunner), NU1701 (Wolfram.NETLink in
BerremanRunner), and MSB3277 WindowsBase conflict from WebView2's WPF assembly
in the UI-referencing projects. None is an `error`; the build gate stays green.

## Artifacts

- `.artifacts/001-build.log`, `.artifacts/001-unit-tests.log`,
  `.artifacts/001-constructor-unit-tests.log`.

## Gotchas

- **`dotnet test --no-build` platform-path mismatch (fixed via Directory.Build.props).**
  `Berreman.slnx` declares x64 as its only platform, so the `build` gate emits
  to `bin\x64\Release\…`. But `dotnet test --no-build -c Release` (the
  `unit-tests` gate) defaults `Platform` to `AnyCPU` and looks in
  `bin\Release\…` — the assembly is "not found" and the gate fails for ANY
  slice. Added `Berreman/Directory.Build.props` defaulting `Platform` to `x64`
  when unspecified; verified via `dotnet msbuild -getProperty` that OutputPath
  now resolves to `bin\x64\Release\net10.0`. The `build` gate's `applies_to`
  already lists `Berreman/Directory.Build.props`, so this file is the
  spec-anticipated normalisation point, not scope creep. It only fills an empty
  platform and never overrides an explicit one; slnx builds are unaffected.
- **Spec internal contradiction (R-1 vs Files-in-scope) on Storage/Optimization
  references.** R-1 (numbered, normative) says Storage references Domain +
  Softellect.Sys and Optimization references Domain only; the descriptive
  "Files in scope" bullets additionally list Berreman/Analytics. Followed R-1;
  the engine projects still reach both assemblies transitively through Domain,
  so no capability is lost.
- **Avalonia 12 exists on nuget but is NOT used.** FuncUI 1.6.0 and
  ScottPlot.Avalonia 5.1.58 track Avalonia 11.3.x; pinning Avalonia 11.3.4
  (matching ScottPlot's floor) avoids the spec-0110-class build break. Do not
  bump Avalonia to 12 until FuncUI/ScottPlot support it.
- **WebView2 drags in a WPF (net5.0-windows) assembly** causing a harmless
  MSB3277 WindowsBase version-conflict warning in the Ui/App/Tests projects.
  It is a warning only; if a later UI slice needs it gone, scope the WebView2
  reference to a Windows-specific TFM or trim the WPF asset.
