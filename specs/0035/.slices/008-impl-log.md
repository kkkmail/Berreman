# Impl log — slice 008 (IMPLEMENT — full-surface workbench bays, reordered LAST)

## Progress

- [x] Read system prompt (implement_worker + shared base), project prompt, slice spec, step-007 SoW.
- [x] Mapped the touched surface: `Ribbon.fs` (Controls), `TableAndElementRotationView.fs`
      (TestWindows), and the Ui.Tests that pin bay order / drive the workbench bays.
- [x] Ribbon.fs — add `BayContentMode` DU + `mode` field; `view` renders the keyed pane only for
      an `InRibbonPane` active bay.
- [x] TableAndElementRotationView.fs — reorder `BayNames.all` + `mainBays` (Materials/Library LAST),
      tag each bay's `mode`, thread pre-built `bays` through `mainControlBar`, gate the table canvas
      + gestures on the active bay's mode in `mainView`.
- [x] RibbonPaneTests.fs — synthetic bays get `mode = InRibbonPane`.
- [x] ExperimentControlsTests.fs / LayerBandsControlsTests.fs — order assertions follow the reorder.
- [x] MainWorkbenchTests.fs — NEW headless ui-smoke proof for the full-surface (no-canvas) behaviour.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Controls/Ribbon.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.TestWindows/TableAndElementRotationView.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/RibbonPaneTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/ExperimentControlsTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/LayerBandsControlsTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/MainWorkbenchTests.fs`

## Retry (attempt 02) — ui-smoke fix

The attempt-01 code failed the **ui-smoke** gate: 6 tests red (4 pre-existing `MainWorkbenchTests`
Library cases, 2 pre-existing `WireUiCompositionTests`). Reproduced locally with the exact gate
command → `Failed: 6, Passed: 85`. The Materials full-surface bay rendered, but the Library
(samples) bay never appeared (`SampleSearchBox` absent), and the 820-px real-app composition
sweep could not even see the Materials verbs.

**Diagnosis.** Wrote a throwaway diagnostic (`ZZZDiagWorkbench.fs`, since removed) that mounted
the real Main loop and dumped the visual tree + tab bounds. Findings: clicking the **Library**
tab changed nothing (the tree stayed on the previous bay), while clicking **Materials** worked;
and the tab bounds showed the strip spanning x=8→1156 in a 980-px window — `RibbonTab_Library`
sat at x0=**1032**, fully off the right edge (its centre at x≈1094 is outside the window), and
`RibbonTab_Materials` centre (x≈952) was only *just* inside. So the click landed on empty space
off-window and never dispatched `SelectBay`.

**Root cause (NOT the below-strip).** The ribbon tab strip was a non-wrapping horizontal
`StackPanel`. With nine bays it overflows a narrow window, and the reorder (Materials / Library
LAST) put the two bays the tests must click at the overflowing right end — Library fully
off-screen (fails everywhere), Materials off-screen only in the 820-px `MainConstructorWindow`
(fails the `WireUiComposition` sweep). My first hypothesis — a mixed keyed/unkeyed `DockPanel`
sibling diff bug — was wrong: FuncUI diffs children by index (`Differ.diffContentMultiple` →
`MapIndexed`) and replaces on `KeyDidChange`, so the attempt-01 keyed slot was already correct.
The speculative `mainView` isolation was reverted to the attempt-01 form.

**Fix (Ribbon.fs).** Wrap the tab strip: the tabs container is now a `WrapPanel` (was a
horizontal `StackPanel`), so tabs flow onto a second row instead of overflowing off-screen — every
bay tab stays reachable. Added a 4-px bottom margin on each tab so a wrapped second row does not
touch the first. Verified with the diagnostic: Library now sits at x0=164 (row 2) and both a
direct table→Library switch and a Materials→Library switch render `SampleSearchBox`. No test
files changed for the fix.

## Testing state

The arc-runner's deterministic gate engine is the sole gate authority (Invariant 6); it re-runs
the gates after this worker exits. Because this is a retry addressing a specific `ui-smoke`
failure, the gates were reproduced and re-run LOCALLY to diagnose the failure and confirm the fix
(advisory only). Local results after the fix:

- `build` (`dotnet build Berreman.slnx -c Release`): **succeeded, 0 Error(s)**. My changed files
  (`Ribbon.fs`, `TableAndElementRotationView.fs`, test files) emit no warnings; the only warnings
  are pre-existing FS3873/FS1125 in untouched files (`Dispersion.fs`, `SeriesDataTests.fs`) and the
  documented pre-existing MSB3277 WindowsBase conflict from `OpticalConstructor.Ui.fsproj`.
- `ui-smoke` (`--filter Category=ui-smoke`): **91 passed, 0 failed** (was 85/6 before the fix).
- `ui-tests` (`--filter Category!=ui-smoke`): **324 passed, 0 failed**.
- `constructor-unit-tests`: **440 passed, 0 failed**.
- `unit-tests` (`berreman_unit_tests`): not re-run — the core solver / its tests are untouched by
  this UI-only slice and the full-solution build compiled them clean; baseline 119 stands.

No `count_at_least` baseline regresses: `ui_smoke_tests` 90 → 91 (the added acceptance test), and
`ui_tests` / `constructor_unit_tests` / `berreman_unit_tests` are unchanged.

## Artifacts

None retained. A throwaway diagnostic test (`ZZZDiagWorkbench.fs`) and its capture were used to
localize the off-screen-tab root cause, then removed; the working tree carries only the product
edits.

## Gotchas

See the state-of-the-world `Gotchas` section.
