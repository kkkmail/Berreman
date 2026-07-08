# 018 — IMPLEMENT — impl-plan

## Scope

Step 018 is the final **warning-clean sweep** for spec 0035, `touches:
[OpticalConstructor.Ui, OpticalConstructor.App]`. Two obligations:

1. Resolve any `FS####` warning originating in the files this spec edits, across
   the touched projects (Ui, App).
2. If the build emits **`MSB3277`** (conflicting assembly versions) from the
   WebView2 reference chain the Ui project carries, resolve it **at the reference
   level** (constrain or drop the offending reference) — never a suppression.

## Findings from the pre-change build (`.artifacts/018-build-before.log`)

- **MSB3277 — present, in scope.** Four projects (`App`, `Tests`, `Ui.Tests`,
  `Ui`) emit `Found conflicts between different versions of "WindowsBase"`:
  `WindowsBase 4.0.0.0` (SDK ref pack, chosen) vs `WindowsBase 5.0.0.0`
  (WPF), the latter pulled by `Microsoft.Web.WebView2.Wpf.dll`.
- **Root cause.** `Microsoft.Web.WebView2` is declared **only** in
  `Ui.fsproj:176`; App/Tests/Ui.Tests inherit it via `buildTransitive`. On
  `net10.0` the package's `lib` compile asset is `net462`-only — it contributes
  **no compile-time reference** (as `ChartHosts.fs:78-86` already documents). Its
  build `.targets` (`Common.targets:128-135`) inject three desktop control
  `<Reference>`s for any net5.0+ TFM, one of which
  (`Microsoft.Web.WebView2.Wpf.dll`) depends on `WindowsBase 5.0.0.0` → the
  MSB3277. No `.fs` references any WebView2 type (every mention is a
  comment/placeholder string; `ChartHosts.webView2Host` returns the §U1.8
  placeholder).
- **FS#### warnings — all pre-existing on master, all OUT of the touched
  projects.** The three FS warnings the full-solution build emits —
  `Dispersion.fs:205 FS3873` (Berreman core), `ChartWindow.fs:78 FS0044`
  (Controls), `SeriesDataTests.fs FS1125 ×4` (Tests) — were each verified present
  at the identical code on the merge-base with `master`
  (`6d39794`). None was originated by spec 0035's edits, and none is in
  `OpticalConstructor.Ui` / `OpticalConstructor.App`. They are therefore out of
  this slice's scope (see Gotchas in the impl-log).

## Approach

- **Drop** the `Microsoft.Web.WebView2` `PackageReference` from `Ui.fsproj`
  (reference-level fix, not a suppression). Because the package contributes no
  compile reference and no `.fs` touches a WebView2 type, dropping it changes no
  compiled behaviour; it only stops the WPF/`WindowsBase 5.0.0.0` reference
  injection that caused MSB3277 in all four projects. Replace the reference line
  with a comment recording *why* and how to re-add (asset-scoped) when the
  deferred Windows-desktop `NativeControlHost` WebView bridge lands.
- **Update the `ChartHosts.fs` doc-comment** (`:78-86`) so it no longer reads as
  if the package is still referenced — it now records the drop and the deferred
  re-add. (In-scope file: `Ui`.)

## Files to modify

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/OpticalConstructor.Ui.fsproj`
  — remove the WebView2 `PackageReference`; add explanatory comment.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/ChartHosts.fs` — refresh the
  WebView2 host doc-comment to reflect the dropped reference.

## Risks

- **Behavioural regression:** none expected — no compiled code references a
  WebView2 type; the placeholder path is unchanged, so the Ui.Tests panel tests
  that assert the "Plotly chart (WebView2) — renderer unavailable" placeholder
  text stay green.
- **Scope tension:** the step's `acceptance` reads "no FS#### warning from our
  code" absolutely, but the `how_to` scopes the sweep to the touched projects and
  "files this spec edits." The three residual FS warnings pre-exist on master and
  sit outside Ui/App, so fixing them would exceed the slice's stated scope (a hard
  base-protocol prohibition). Resolved by fixing only the in-scope MSB3277 and
  documenting the pre-existing warnings with evidence.

## Verification

Rebuild `Berreman.slnx -c Release` and confirm MSB3277 no longer appears in the
App/Ui/Tests/Ui.Tests builds and that no new `FS####`/`MSB` warning from our code
was introduced. (Gate execution itself belongs to the arc-runner engine —
Invariant 6.)
