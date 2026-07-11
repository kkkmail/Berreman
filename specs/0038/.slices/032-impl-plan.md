# Impl-plan — step 032 (IMPLEMENT: two-pane Material editor + tabbed multi-curve preview)

## Attempt 03 (retry — the sole slice feature left uncovered by a test)

The two prior attempts delivered the whole slice (two-pane split, tabbed n/k + Gyration + μ
preview, per-series toggles, all builders, and the layout/tab/render tests). The retry hint
identifies ONE uncovered feature: the **per-series show/hide toggle**. Everything else — the
two-pane layout and the chart builders — is correct and well-covered; this round does NOT touch it.

### What this round adds

1. **Expose `applyHidden` minimally.** `MaterialEditorView.applyHidden` (the lowering that maps
   `hiddenSeries` keys → `ChartStyle.setSeriesVisible i false`) is currently `private`. The retry
   hint sanctions exposing it "minimally … rather than re-deriving its logic," so the test asserts
   against the REAL lowering, not a re-implementation. Change `let private applyHidden` →
   `let applyHidden` and note the exposure in its doc comment. No behaviour change.

2. **One focused test** in `OpticalConstructor.Ui.Tests/MaterialEditorWindowTests.fs`:
   - **Handler:** run `ToggleSeriesVisibility (UiIds.seriesToggle "nk" "n₁")` through `update`,
     assert the key lands in `m.hiddenSeries`; dispatch again, assert it is removed.
   - **Lowering:** build the n/k chart (`nkDispersionChart` over the isotropic default),
     seed its style (`nkDispersionStyle`), call `applyHidden (Set.singleton key) "nk" chart style`,
     and assert series index 0 (n₁) flips invisible while 1..5 stay visible. Then hide `k₂` and
     assert index 4 flips (the index is read from the series NAME, not hard-coded — a wrong index
     cannot pass silently). Then apply the same n₁ key under the WRONG tabCode ("gyration") and
     assert NOTHING hides (a wrong `tabCode` cannot pass silently either).

   A tiny private helper `seriesVisibleAt i style` reads back
   `ChartStyle.seriesStyleOf(i).visible`. The test is PURE (no window), so it runs under the
   `ui-tests` gate (Category!=ui-smoke), not `ui-smoke`.

## Files to modify

- `OpticalConstructor.Ui/MaterialEditorView.fs` — drop `private` on `applyHidden` (+ doc note).
- `OpticalConstructor.Ui.Tests/MaterialEditorWindowTests.fs` — the `seriesVisibleAt` helper and
  the one focused `[<Fact>]`.

## Risks

- **Unicode series names.** The n/k series are named `n₁…n₃` / `k₁…k₃` (subscript digits, U+2081…);
  the test key must match the builder's exact glyphs, else the lowering assert (index 0 invisible)
  fails loudly rather than silently — the guard is that a mismatch is caught, not masked.
- **Open-collision.** Do NOT `open OpticalConstructor.Controls.ChartStyle` — its `defaultState`
  would shadow `MaterialComplexityEditor.defaultState` the file already uses (the attempt-02 trap).
  Reference `ChartStyle` fully-qualified in the one helper instead.
- Per Invariant 6 the worker runs no gates; a `dotnet build` + Ui.Tests run is advisory diagnostic
  (the attempt-02 lesson: only a real compile proves the module builds), not a gate green-light.
