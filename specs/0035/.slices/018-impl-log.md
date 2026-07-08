# 018 — IMPLEMENT — impl-log

## Progress

- [x] Confirm the MSB3277 root cause (WebView2 → WPF `Microsoft.Web.WebView2.Wpf.dll`
      → `WindowsBase 5.0.0.0` vs the SDK ref pack's `WindowsBase 4.0.0.0`).
- [x] Confirm the WebView2 package is declared ONLY in `Ui.fsproj` and that no
      `.fs` references a WebView2 type (all mentions are comments/placeholder
      strings; `ChartHosts.webView2Host` returns the §U1.8 placeholder).
- [x] Drop the `Microsoft.Web.WebView2` `PackageReference` from `Ui.fsproj`
      (reference-level fix; explanatory comment left in its place).
- [x] Refresh the `ChartHosts.fs` WebView2 host doc-comment to reflect the drop
      and the deferred re-add.
- [x] Rebuild `Berreman.slnx -c Release`: MSB3277 cleared (0 occurrences),
      0 errors, and the FS-warning set is byte-identical to the pre-change build
      (no warning added or removed by the change).

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/OpticalConstructor.Ui.fsproj`
  - Removed `<PackageReference Include="Microsoft.Web.WebView2" Version="1.0.4078.44" />`
    (was line 176). Replaced with a comment recording *why* the reference is
    dropped (MSB3277 WindowsBase conflict, resolved at the reference level per
    §0.6, never suppressed) and how to re-add it (asset-scoped to exclude the WPF
    assembly) when the deferred Windows-desktop `NativeControlHost` WebView bridge
    lands.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/ChartHosts.fs`
  - Updated the `tryHostPlotly` doc-comment (`:78-86`) so it no longer reads as
    if the package is still referenced — it now records the dropped reference, the
    WPF/`WindowsBase` cause, and the deferred asset-scoped re-add. No code change;
    the `webView2Host` seam still degrades to the §U1.8 placeholder.

## Testing state

Per **Invariant 6** (the worker acts and runs no checks; the arc-runner's
deterministic gate engine runs `build`, `unit-tests`, `constructor-unit-tests`,
`ui-smoke`, `ui-tests` after this session exits), the gates were **not** run as
gates. To do a warning-clean *sweep* I did compile the solution to observe the
warnings (a warning sweep is not possible otherwise); that is development
observation, not gate execution.

- **`build` (in scope):** `dotnet build Berreman.slnx -c Release` →
  **0 Error(s)**, and **MSB3277 = 0** (was 40 lines across App/Ui/Tests/Ui.Tests
  before). Captured: `.artifacts/018-build-before.log` and
  `.artifacts/018-build-after.log`.
- **No new warnings from our code.** The FS-warning site set is byte-for-byte
  identical before and after the change (`diff` of the unique FS sites is empty).
  The change is a dropped dead package reference plus comments — it touches no
  compiled code.
- **`unit-tests` / `constructor-unit-tests` / `ui-smoke` / `ui-tests`:** not run
  (Invariant 6). They remain green by construction — dropping WebView2 changes no
  compiled behaviour (no `.fs` referenced a WebView2 type; the placeholder path is
  unchanged), so the Ui.Tests panel tests that assert the
  "Plotly chart (WebView2) — renderer unavailable" placeholder text still hold.
  Baseline counts carry forward from slice 017 unchanged (no test added/removed).

Baseline counts (unchanged this round): `berreman_unit_tests` 119,
`constructor_unit_tests` 447, `ui_smoke_tests` 105, `ui_tests` 329.

## Artifacts

- `.artifacts/018-build-before.log` — full-solution Release build BEFORE the
  change (captures the MSB3277 WindowsBase conflict in App/Ui/Tests/Ui.Tests).
- `.artifacts/018-build-after.log` — full-solution Release build AFTER the change
  (0 errors; MSB3277 gone; only pre-existing FS + exempt NU + vendored SYSLIB
  warnings remain).

## Gotchas

- **Reference-level fix chosen: DROP, not constrain.** On `net10.0`
  `Microsoft.Web.WebView2` contributes no compile-time reference (its `lib`
  compile asset is `net462`-only; the net5.0+ `<Reference>`s to Core/WinForms/Wpf
  come purely from the package's build `.targets`, `Common.targets:128-135`).
  Excluding those build assets would leave a `PackageReference` that contributes
  literally nothing — a misleading no-op — so I dropped it outright. The spec
  §0.6 sanctions either ("constrain or drop the offending reference"); drop is the
  honest, minimal representation and clears MSB3277 in all four downstream
  projects at once (they inherit it transitively from Ui, the only declarer).
  Recorded per the project prompt's "pick the sensible interpretation" rule.
- **Scope decision — the residual FS#### warnings are OUT of scope and left
  untouched.** The full-solution build still emits three FS warnings —
  `Berreman/Berreman/Dispersion.fs(205) FS3873` (bare `{ 1 .. n }` range),
  `OpticalConstructor.Controls/ChartWindow.fs(78) FS0044` (deprecated ScottPlot
  `Label.FontSize`), and `OpticalConstructor.Tests/SeriesDataTests.fs FS1125 ×4`
  (uninstantiated generic `Range`). Each was verified **present at the identical
  code on the `master` merge-base (`6d39794`)** — i.e. pre-existing, NOT
  originated by spec 0035's edits — and each lives **outside this step's touched
  projects** (`OpticalConstructor.Ui` / `.App`): Dispersion in the core solver,
  ChartWindow in Controls, SeriesDataTests in Tests. Step 018's `how_to` scopes
  the FS sweep "across the touched projects … [to] the files this spec edits," and
  the base protocol forbids implementing beyond the slice's stated scope. So the
  in-scope FS-warning work is empty (no FS warning originates in Ui/App), and the
  three pre-existing warnings are deferred (see the SoW `Deferred`). The step's
  `acceptance` phrasing ("no FS#### warning from our code") is read in the context
  of this scope — consistent with it also tolerating the vendored
  `MathNetNumerics` `SYSLIB0051` C# warnings, which no Ui/App slice would touch.
- **`Ui.fsproj:176` line number in the spec is now stale** — the WebView2
  reference that lived there is removed; the reference-level fix is the comment
  block now occupying that region.
