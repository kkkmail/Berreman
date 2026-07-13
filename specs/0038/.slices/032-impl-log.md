# Impl-log — step 032 (IMPLEMENT: two-pane Material editor + tabbed multi-curve preview)

## Progress

### Attempt 03 (retry — cover the sole uncovered slice feature: the per-series show/hide toggle)

The two prior attempts delivered the entire slice and built green; the retry hint identifies ONE
feature that shipped without a direct test — the **per-series show/hide toggle** on the preview
tabs — and directs a single focused test against BOTH halves of that feature (the `update` handler
and the `applyHidden` lowering), with an explicit sanction to expose `applyHidden` minimally rather
than re-derive its logic. This round is exactly that; the two-pane layout and the chart builders
(correct and well-covered) are untouched.

- [done] `MaterialEditorView.fs` — dropped `private` on `applyHidden` (the lowering that maps each
  `hiddenSeries` key → `ChartStyle.setSeriesVisible i false`) so the test asserts against the REAL
  mapping, not a copy. Its doc comment now records the exposure and why. NO behaviour change — the
  only call site (`chartTab`) is unaffected.
- [done] `MaterialEditorWindowTests.fs` — one focused `[<Fact>]`
  ``spec 0038 (032): the per-series show/hide toggle round-trips through update and lowers onto the exact curve``
  (a PURE contract test — no window, so it runs under the `ui-tests` gate, not `ui-smoke`), plus a
  tiny private helper `seriesVisibleAt` that reads back `ChartStyle.seriesStyleOf(i).visible`. The
  test:
  1. **Handler** — dispatches `ToggleSeriesVisibility (UiIds.seriesToggle "nk" "n₁")` through
     `update`, asserts the key lands in `m.hiddenSeries`, dispatches it again, asserts it is removed.
  2. **Lowering (right curve)** — builds `nkDispersionChart` over the isotropic default, seeds
     `nkDispersionStyle`, calls `applyHidden (Set.singleton key) "nk" chart style`, and asserts
     series index 0 (n₁) is flipped invisible while indices 1..5 stay visible.
  3. **Lowering (wrong index cannot pass)** — hides `k₂` (the 5th series → index 4) and asserts
     exactly index 4 flips; the index is read from the series NAME, never hard-coded.
  4. **Lowering (wrong tabCode cannot pass)** — applies the SAME n₁ key under tabCode `"gyration"`
     and asserts NOTHING hides, proving the tabCode namespaces the toggle key.

### Attempt 02 (retry — the round-1 code failed the `build` gate)

The round-1 code was correct in shape but did NOT compile, so the `build` gate short-circuited
(and the downstream test gates never ran). Two compile errors, both fixed surgically that round:

- [fix] `NkDispersionChart.fs` — `FS0039: The type 'nm' is not defined` at the two `<nm>`
  unit-of-measure literals. Added `open Berreman.Constants`.
- [fix] `NkDispersionChartTests.fs` — `FS0001` `defaultState` open-collision between
  `MaterialComplexityEditor` and `ChartStyle`. Qualified it as `MaterialComplexityEditor.defaultState`.

### Round-1 construction (unchanged, still in the tree)

- [done] `NkDispersionChart.fs` — replaced the single 2-series eps11 builder with the per-axis
  `nkDispersionChart` (n₁/n₂/n₃ left, k₁/k₂/k₃ right, via `getEps`); added `gyrationChart` /
  `gyrationStyle` (Im[ρᵢⱼ] via `rhoWithDisp`), `muChart` / `muStyle` (Re[μᵢᵢ] + gyration magnitude via
  `muWithDisp`), and the view-only aspect classifiers `hasGyration` / `hasMagnetic`.
- [done] `MaterialEditorView.fs` — new UiIds (splitter, previewTabs, nkTab/gyrationTab/muTab, gyration/μ
  chart hosts, `seriesToggle`); `hiddenSeries : Set<string>` model field + `ToggleSeriesVisibility`
  message + update handler; the tabbed `previewPane` (n/k always, Gyration when active, μ when
  magnetic, each with per-series show/hide toggles); the `twoPane` Grid + vertical GridSplitter layout;
  rewrote `view`; rewrote `gainWarningOf` to scan all k curves.
- [done] `MaterialEditorWindow.fs` — window sized 1400×1000 so the narrower left ladder pane fits.
- [done] Tests — per-axis builder tests + biaxial / gyration / μ; updated `EmbeddedChartTests`; added
  four headless proofs to `MaterialEditorWindowTests` (splitter + full-height chart; Gyration / μ tabs
  track the toggles; one frame per tab for isotropic / biaxial / active / magnetic).

## Files modified

### This round (attempt 03)

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/MaterialEditorView.fs` — `applyHidden` made
  non-private (one-word change + doc note).
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/MaterialEditorWindowTests.fs` — one
  focused pure `[<Fact>]` + the `seriesVisibleAt` helper.

### Earlier rounds (attempts 01–02, still in the tree)

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/NkDispersionChart.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/MaterialEditorView.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/MaterialEditorWindow.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/NkDispersionChartTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/EmbeddedChartTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/MaterialEditorWindowTests.fs`

## Testing state

Per Invariant 6 the worker runs no gates; the arc-runner gate engine is the sole authority. As
advisory diagnostic verification (and heeding the attempt-02 lesson that only a real `dotnet build`
proves a module compiles), the worker built the solution and ran the two touched-project test sets:

- **`build`** — `dotnet build Berreman.slnx -c Release` → **Build succeeded, 0 errors** (only the two
  exempt third-party `NU1701` Wolfram.NETLink advisories remain).
- **`ui-tests`** (`Category!=ui-smoke`) — **432/432 passed** (was 431; the one new pure test is the +1).
- **`ui-smoke`** (`Category=ui-smoke`) — **164/164 passed** (unchanged — this round adds no headless test).

The new test alone: `--filter "DisplayName~per-series"` → **1/1 passed**. The solver `unit-tests`
(119) and `constructor-unit-tests` (636) projects are untouched by this slice and were not re-run.

Expected roster (engine-run): `build`, `unit-tests`, `constructor-unit-tests`, `ui-smoke`, `ui-tests`.
This round adds 1 fact under `ui-tests` (431 → 432); `ui-smoke` (164), solver `unit-tests` (119), and
`constructor-unit-tests` (636) are unchanged.

## Artifacts

- `.artifacts/dsl-typecheck.fsx` — the attempt-01 `dotnet fsi` type-check of the novel FuncUI DSL
  patterns (kept for the record). This round produced no new persistent artifacts.

## Gotchas

- **`ChartStyle` is a TOP-LEVEL module in `OpticalConstructor.Controls`, a sibling of the
  `ExperimentChart` module — NOT nested inside it.** So the qualified path is
  `OpticalConstructor.Controls.ChartStyle.X`, NOT `…ExperimentChart.ChartStyle.X`: the latter binds
  `ExperimentChart` to the like-named TYPE and fails with `FS0039 The type 'ChartStyle' is not
  defined`. (`ChartFont` is the same shape.) The test references `ChartStyle` fully qualified.
- **Do NOT `open OpticalConstructor.Controls.ChartStyle` in this test file.** It exports
  `defaultState : ExperimentChart -> ChartStyleState`, which would shadow the
  `MaterialComplexityEditor.defaultState` this file uses unqualified in ~a dozen places (`applied`,
  `derived defaultState`, the init assertions). This is the same open-collision that broke attempt 02
  in `NkDispersionChartTests.fs` — there it was fixed by qualifying the domain `defaultState`; here it
  is avoided by fully qualifying `ChartStyle` at the one use site and never opening it.
- **The n/k series names are subscript-digit unicode** (`n₁ n₂ n₃ k₁ k₂ k₃`, U+2081…U+2083). The
  toggle key must match the builder's exact glyphs; the lowering assert (index 0 invisible) fails
  LOUDLY on a mismatch rather than masking one, so a mis-typed name cannot silently pass.
- **`applyHidden` is intentionally non-private now** (spec 0038 032 retry) — an explicit,
  hint-sanctioned exposure so the lowering is proved against the real mapping, not a re-derivation.
- The n/k builder emits SIX per-axis series; `nSeriesIndex`/`kSeriesIndex` are gone. Consumers select
  the k curves by the `"k"` name prefix (`gainWarningOf`, `EmbeddedChartTests`).
- Tab presence in edit mode tracks the LADDER TOGGLES (`m.editor.activity` / `.magnetic`); a view-only
  preset (no ladder) is classified from its assembled ρ/μ tensors (`hasGyration` / `hasMagnetic`).
- The task file's system-prompt path is stale (`.../implement_worker.system-md` at the AI-Strategy repo
  root); the real file is under `.../src/ai_strategy_generator/multistep/`. Same note as steps 031/032.
