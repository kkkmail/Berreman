# Step 0226 — impl log

## Progress

- [x] UiIds: five kit-copy ids in the kit-bar group, each with a one-line doc
- [x] ConstructorUiIdOfDomain: `kitCopyExperimentToggle`, beside `attachDataFile`
- [x] KitWindowView: the verb after Show in Explorer, and the copy panel under the send progress
- [x] KitWindowTests: four facts (three ui-tests, one ui-smoke)
- [x] ScrollExtentTests: the tallest bar adds a running send and an open copy source of forty experiments
- [x] Development runs: build, constructor-unit-tests, ui-tests (selected classes), ui-smoke, invariants, elevation

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Controls/UiIds.fs`: added `kitCopyExperimentsButton`,
  `kitCopySourceText`, `kitCopyExperimentList`, `kitCopyChosenButton` and `kitCopyCancelButton`.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/ConstructorUiIdOfDomain.fs`: added
  `kitCopyExperimentToggle (experimentId : ExperimentId)`, which gives `KitCopyExperimentToggle_<n>`.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/KitWindowView.fs`:
  - `copyExperiments` in `kitMenuRow`. It is offered for `ControlOffered` with `kitCopyOpt = None`, and its press
    runs `model.kitShell.kitPicker.pickThen (OpenCopySource >> dispatch)`.
  - `kitCopyRows` holds:
    - the wrapping source text;
    - a `ScrollViewer` with vertical `Auto` and `MaxHeight` 160, over a `StackPanel` of `CheckBox`es;
    - a horizontal row with Copy (positive, only with a non-empty choice) and Cancel (negative).
  - `kitCopyRows` is inserted after `kitSendRows` in the bar.
  - Added `open Avalonia.Controls.Primitives` (for `ScrollBarVisibility`) and `open OpticalConstructor.Ui.KitOpening`
    (for `KitFolderPicker.pickThen`).
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/KitWindowTests.fs`:
  - `pressAtCentre` is widened from `Border` to `Control`, so it also presses a `CheckBox`. The existing calls are
    unchanged.
  - New helpers: `closedKitHolding`, `sourceExperiment`, `copyPanelIds` and `copyToggleOf`.
  - New facts:
    - `the copy UiIds are the stable intent-named ids`;
    - `an open source lists its experiments with a toggle each, and Copy and Cancel share a row, Cancel to the right`;
    - `a kit opened read-only offers no Copy experiments`;
    - `pressing Copy experiments, a toggle and Copy brings the experiment into the kit` (trait `ui-smoke`).
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/ScrollExtentTests.fs`:
  - `tallestKitBar` adds `kitSend` started and progressed (2 / 5).
  - It also adds `kitCopyOpt`: forty experiments copied from the collected one, E1 and E2 chosen. The source is an
    in-memory scratch kit opened read-only, with a long name.
  - Added `open OpticalConstructor.Ui.KitArchiveJob` for `KitArchiveJobHandle` and the `jobProgress` extension.

## Decisions

- **The toggle's content is a wrapping `TextBlock`** whose text is `E<n> — <description>`, not a bare string. A long
  varied-experiment description then wraps instead of being clipped (AGENTS.md: nothing is clipped out of reach). The
  layout fact reads the text through `Content`.
- **The toggle dispatches on `CheckBox.onClick`**, as `pivotCheckBox` and `SampleEditorView.emissionCheck` do.
  `onClick` fires only on the user's press, so the render's `IsChecked` never feeds back. It uses
  `SubPatchOptions.OnChangeOf (box id.value)`: the message depends only on the experiment id.
- **The copy panel is one keyed `StackPanel`** (keyed by `kitCopySourceText`), as `kitChangesRows` is.
- **The layout fact opens its source through step 225's messages** (`OpenCopySource`, then
  `ToggleCopiedExperiment E2`), not through a record update. It closes the source at the end through
  `closeCopySource`.
- **The read-only fact's kit holds no experiment.** The fact checks only the verb and the absence of the panel.
- **The scroll test's copy source is a scratch kit, not the window's own kit.** It is a separate in-memory context
  (`KitOpening.scratchKit`), renamed and marked read-only, as a real source would be.

## Testing state

Development runs over the final source (Invariant 6: the gate engine gives the verdict).

- **build:** `dotnet build Berreman.slnx -c Release -nologo -v:m` exit 0, 0 errors.
  - It reported 2 warnings, both `SYSLIB0051` in vendored `MathNetNumerics/Numerics/Exceptions.cs` (C#). They appear
    whenever MathNet recompiles and are already in this arc's build logs from step 0001.
  - No F# or MSBuild warning comes from our code, and the log has no lowercase `error`.
  - Log: `.artifacts/0226-dev-build.log`.
- **ui-tests** over `KitWindowTests` and `ScrollExtentTests` (non-smoke, non-kit-acceptance): **23 passed**,
  0 failed.
  - `KitWindowTests`: 8 → 11.
  - `ScrollExtentTests`: 12 (unchanged count; its kit-window fact carries the new expectation).
  - Log: `.artifacts/0226-dev-ui-tests.log`.
- **constructor-unit-tests** (all): **1336 passed**, 0 failed, 0 skipped (unchanged).
  Log: `.artifacts/0226-dev-constructor-unit-tests.log`.
- **ui-smoke** (all): **238 passed**, 0 failed, 0 skipped (237 + 1).
  - The new `pressing Copy experiments, a toggle and Copy brings the experiment into the kit` is `Passed` in the TRX.
  - The run took 200 s against the 600 s `attempt_timeout_seconds`.
  - Log: `.artifacts/0226-dev-ui-smoke.log`.
- **elevation**, run with `ASG_ARC_BASE_COMMIT=f395597…`:
  - `ELEV-GATE: OK - 0 new finding(s) in 12 project(s), 194s`, with ELEV004 excluded, 1315 suppressed findings and
    0 known findings.
  - Log: `.artifacts/0226-dev-elevation.log`.
- **The fourteen `invariant-s22-*` greps:** each finds nothing.
- **Line endings:** `git diff --numstat` equals `--ignore-cr-at-eol`, and all five files are `i/lf w/lf`.
- **Mutants** (`.artifacts/0226-mutants/`), each rebuilt with `dotnet build OpticalConstructor.Ui.Tests.fsproj`:
  - **Round 1** made the verb ignore `kitWrites` and dropped `MaxHeight`. It failed:
    - `a kit opened read-only offers no Copy experiments`;
    - the layout fact;
    - `the kit window: the kit bar and the page stay reachable` (214 unreachable at 800 × 600).
  - **Round 2** put Cancel before Copy and made a toggle that dispatches nothing. It failed:
    - the layout fact (`Cancel must sit to the right of Copy, past a gap`);
    - the ui-smoke fact (`KitCopyChosenButton must be in the window`).
  - The source was restored byte for byte (`cmp`). The solution was rebuilt (0 warnings, 0 errors; log
    `.artifacts/0226-dev-build-final.log`).
  - Re-runs: the selected ui-tests passed **23** and `KitWindowTests`' ui-smoke facts passed **5**
    (`.artifacts/0226-dev-ui-tests-final.log`, `.artifacts/0226-dev-ui-smoke-kitwindow-final.log`).
- **Baselines:** `constructor_unit_tests` 1336, `ui_smoke_tests` 238, `ui_tests` 23. The ui-tests selection now
  spans both classes; it was 20 before this step, so the count rose by 3.

## Artifacts

- `.artifacts/0226-dev-build.log`, `.artifacts/0226-dev-build-final.log`
- `.artifacts/0226-dev-ui-tests.log`, `.artifacts/0226-dev-ui-tests-final.log`
- `.artifacts/0226-dev-constructor-unit-tests.log`
- `.artifacts/0226-dev-ui-smoke.log`, `.artifacts/0226-dev-ui-smoke-kitwindow-final.log`
- `.artifacts/0226-dev-elevation.log`
- `.artifacts/0226-mutants/round{1,2}-{build,tests}.log`

## Gotchas

- `MaterialsWindowView.actionButton` subscribes its press with `OnChangeOf id`, so its closure is the first one
  rendered. Copy and Cancel send constant messages, so this is harmless. A future action whose message depends on
  the model must not use `actionButton` as is.
- A press on a verb that removes it (Copy experiments…, Copy) is followed by a release at the same point over
  whatever moved there. The boxes act on press, and a release alone clicks nothing, so the smoke fact is unaffected.
- The real app's shell (`OpticalConstructor.App/Program.fs`) still uses `KitWindowShell.inert`'s pickers. Step 0233's
  brief wires `kitPicker = KitFolderPicker.ofWindow window context.locations.kitsFolder`. The App is outside this
  step's authorized edits, so it was left as is.
- The first full build recompiled vendored MathNet and printed two `SYSLIB0051` C# warnings
  (`MathNetNumerics/Numerics/Exceptions.cs`). They are pre-existing and appear in the arc's build logs since step
  0001. They do not come from our F# code, and the final incremental build printed 0 warnings.
