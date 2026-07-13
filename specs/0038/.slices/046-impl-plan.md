# Impl-plan — 046 (IMPLEMENT: final warning-clean sweep, spec-md §0.6 / §N.0)

## Goal

A Release build of `Berreman.slnx` completes with **zero our-code FS or MSBuild
warnings**; only the exempt third-party NuGet advisories (`NU1701`,
`NU1901`–`NU1904`) remain. In particular `MSB3277` must not have been
reintroduced by the Part-B relocation or the three new projects
(`OpticalConstructor.Controls`, `OpticalConstructor.Database`,
`OpticalConstructor.Seeding`, plus `OpticalConstructor.TestWindows.App`).

## Warning inventory (clean-ish Release build, captured `046-build-before.log`)

10 warnings total:

| Code | Count | Site | Verdict |
|---|---|---|---|
| `NU1701` | 2 | `BerremanRunner.fsproj` (Wolfram.NETLink) | **exempt** (§0.6) |
| `SYSLIB0051` | 2 | `MathNetNumerics/Numerics/Exceptions.cs` | **out of scope** — vendored third-party MathNet, not our code, C# (not FS/MSBuild) |
| `FS0044` | 1 | `OpticalConstructor.Controls/ChartWindow.fs:78` | **fix** |
| `FS3873` | 1 | `Berreman/Berreman/Dispersion.fs:205` | **fix** |
| `FS1125` | 4 | `OpticalConstructor.Tests/SeriesDataTests.fs:29,49,57,59` | **fix** |

**No `MSB3277`, no other MSBuild warning, no `error` line** — the relocation
did not regress the assembly-version resolution (the 0035 WebView2 fix holds).

## Fixes (all at source — no `NoWarn`, no suppression attribute)

1. **FS0044** — `a.Label.FontSize <- …` uses ScottPlot's deprecated `AxisBase.Label`.
   Change to `a.LabelStyle.FontSize <- …`, the accessor the deprecation message
   endorses and the exact parallel of the adjacent non-deprecated
   `a.TickLabelStyle.FontSize` (line 79). Behaviour-identical.
2. **FS3873** — `{ 1 .. abs n } |> Seq.fold …` is the deprecated bare-range
   sequence form. Change to `seq { 1 .. abs n } |> Seq.fold …`. Behaviour-identical.
3. **FS1125** — `Range.create n lo hi` cannot infer `Range<'T>` from the static
   member access. Change the four call sites to `Range<_>.create n lo hi`, the
   convention already used everywhere else (`StandardLightVariables.fs`, every
   `Analytics/Examples/*.fsx`). Behaviour-identical.

## Files to modify

- `Berreman/OpticalConstructor/OpticalConstructor.Controls/ChartWindow.fs`
- `Berreman/Berreman/Dispersion.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/SeriesDataTests.fs`

## Risks / interpretation calls

- The step `touches` list names five UI/App/Domain/Storage projects, but the
  acceptance is a **whole-solution** zero-warnings gate. FS3873 (Berreman core)
  and FS1125 (OpticalConstructor.Tests) live outside the touches list yet are
  our-code FS warnings the §N.0 final sweep must clear — so the sweep reaches
  them. All three edits are one-token, behaviour-preserving.
- `SYSLIB0051` is left in the vendored MathNet: it is third-party source
  (CLAUDE.md: "reference the projects, not the NuGet"; do not modify), the
  warning is a C# obsolete-API notice (not an FS or MSBuild warning), and it
  predates spec 0038. Modifying MathNet's serialization constructors or adding
  a `NoWarn` are both worse than leaving a pre-existing third-party notice.

## Verification

Full clean `-t:Rebuild` Release build of `Berreman.slnx`, then confirm the FS0044 /
FS3873 / FS1125 lines are gone and only `NU1701` (×2) + `SYSLIB0051` (×2) remain,
with 0 errors. (Gate execution itself belongs to the arc-runner; this build is a
diagnostic to drive the fix, per the IMPLEMENT family's construction obligation.)
