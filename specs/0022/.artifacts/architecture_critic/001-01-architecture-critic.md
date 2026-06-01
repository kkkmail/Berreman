# Architecture critique -- 001.slice-md cycle 1

## Summary

Clean, faithful scaffold. Six projects land under one
`OpticalConstructor/` folder, the reference graph follows the normative R-1
build order, every new `.fsproj` mirrors the existing Berreman project template
almost line-for-line, the FuncUI clone stays unreferenced (AC-A8 honoured), and
only compilable placeholders ship. I found no layering or separation-of-concerns
issue and no SoW/diff contradiction; the only notes are minor scope/consistency
observations, chiefly the repo-wide `Directory.Build.props` introduced by a slice
whose planned-files list does not mention it.

## Consistency

This is the strongest part of the slice. Each new project reproduces the
established Berreman fsproj idiom rather than inventing its own:

- `AssemblyName` follows the `Softellect.Berreman.OpticalConstructor.*` pattern,
  matching the existing `Softellect.Berreman.Core` / `.Analytics` /
  `.OpticalModel` naming (`Berreman/Berreman.fsproj:6`,
  `Analytics/Analytics.fsproj:6`).
- The `Debug|x64` / `Release|x64` PropertyGroups, the
  `--warnaserror+:25 --platform:x64` `OtherFlags`, the
  `NU5100;NU5110;NU5111;NU1903` `NoWarn`, and the `FSharp.Core` `Update` pin all
  copy the engine's template verbatim (`Berreman/Berreman.fsproj:21-32,53`).
- The test host uses the same `xunit.v3` 3.2.2 + `Microsoft.NET.Test.Sdk` 18.6.0
  + `coverlet.collector` stack as `BerremanTests/BerremanTests.fsproj:41-51`,
  so the `constructor-unit-tests` gate runs on the same harness the project
  already trusts.

The four-space indentation, ordered `<Compile>` item groups, and explanatory
comments are all in keeping with house style.

## Spec fit

- The reference graph matches the **normative R-1** wording, not the looser
  "Files in scope" bullets, and the SoW Gotchas record that decision: Domain ->
  Berreman + Analytics; Storage -> Domain + `Softellect.Sys`
  (`OpticalConstructor.Storage.fsproj:36,40`); Optimization -> Domain only
  (R-1 item 3, vs the bullet's "+ Berreman/Analytics"); Ui -> Domain/Storage/
  Optimization; App -> Ui; Tests -> all four. Engine assemblies reach the leaf
  projects transitively through Domain. This is the right reading of the spec's
  own internal R-1-vs-bullets contradiction and is documented -- good.
- `Softellect.Sys` is pinned at 10.0.101.41, consistent with the
  `Softellect.DistributedProcessing.SolverRunner` 10.0.101.41 the runner already
  uses (`BerremanRunner/BerremanRunner.fsproj:29`). No version drift.
- **Minor under-delivery vs a descriptive bullet:** the Storage "Files in scope"
  bullet lists `System.Text.Json` among the package refs, but the implementation
  omits an explicit `System.Text.Json` pin
  (`OpticalConstructor.Storage.fsproj:31-36` has FSharp.SystemTextJson,
  FSharp.Data, JsonSchema.Net, ClosedXML, Softellect.Sys -- no STJ). The SoW
  Gotchas justify this: on `net10.0` the framework + FSharp.SystemTextJson supply
  STJ, and an explicit 9.x pin would risk an NU1605 downgrade. That is a sound
  call (NU1605 is not in the `NoWarn` list and `--warnaserror+:25` would not catch
  it, so a bad pin could silently downgrade). I agree with the omission; flagging
  only because it diverges from the literal bullet and the next slice's JSON IO
  work depends on STJ resolving from the framework as assumed.
- AC-A8 satisfied: `OpticalConstructor.Ui.fsproj:33-43` consumes the public MIT
  Avalonia 11.3.4 + Avalonia.FuncUI 1.6.0 stack with no reference to the clone,
  and the linking mechanism stays unresolved. The clone-vs-public distinction is
  called out in a comment at the reference site -- exactly where a future editor
  needs it.

## Evolvability

- **`Directory.Build.props` has repo-wide reach and is not in the planned-files
  list.** Placed at `Berreman/Directory.Build.props`, it defaults
  `Platform=x64` for *every* project under `Berreman/`, including pre-existing
  ones and `OpticalModel.fsproj`, which the slice says MUST be left untouched.
  The effect is benign -- the prop is guarded by `Condition="'$(Platform)' == ''"`
  so it only fills an empty platform and never overrides an explicit one, and
  every existing project already declares `<Platforms>x64</Platforms>` -- and the
  rationale (so the no-platform `dotnet test` unit-tests gate resolves the
  `bin\x64\Release` tree the slnx build emits) is real and documented in the SoW
  Gotchas. Keep it. The note is that a load-bearing *scaffold* slice now owns a
  repo-wide MSBuild default; the rationale currently lives only in the SoW, so the
  in-file comment it carries (lines 3-14) is the right safeguard against a future
  slice deleting it and silently re-breaking the test gate.
- x64 is now asserted in four places per project (slnx `<Platform Project>`,
  `<Platforms>`, conditioned `<PlatformTarget>`, and the new props default). This
  is redundant, but it is the *same* redundancy the existing projects already
  carry, so harmonising it is a whole-repo cleanup, not this slice's job.

## Risks

- **Package-surface drift (acknowledged).** The Ui project pulls a large net10
  package set -- Avalonia 11.3.4, Avalonia.FuncUI 1.6.0, ScottPlot.Avalonia
  5.1.58, Plotly.NET 5.1.0, OpenTK 4.9.4, Microsoft.Web.WebView2 -- and Storage
  adds JsonSchema.Net, ClosedXML, etc. The SoW Risks and Gotchas already flag the
  Avalonia-11-vs-12 pin discipline and the WebView2-induced WPF/WindowsBase
  warning. Nothing actionable now; the incremental-add mitigation in the SoW is
  the right posture, and keeping these behind placeholder-only modules isolates
  any future resolution failure.

## Bottom line

Architecturally this is a clean, well-disciplined scaffold: correct folder shape,
R-1 build order, faithful reuse of the existing project template, honoured
clone-audit gate, and placeholder-only content as the slice requires. The
deviations from the descriptive bullets (Optimization references, the
`System.Text.Json` omission) are the defensible R-1-normative / net10-correct
choices and are documented. The one thing I'd want the judge to weigh is the
repo-wide `Directory.Build.props` arriving via a scaffold slice without a
planned-files entry -- benign and justified, but broad. I'd ship this; if a
re-spawn happens for other reasons, adding the props file to the slice's
files-modified record would close the only loose thread. The verdict is the
judge's.
