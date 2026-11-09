# Step 0226 — state of the world

## Where we are

Spec 0050, section S6 ("Send and receive kits; copy experiments between kits"), step 0226 (IMPLEMENT). Step 225 put
Copy experiments from another kit… behind semantic messages. This step adds the Kit-menu verb and the copy panel that
send them, so the copy can now be driven from the window. Steps 227–229 extend the copy with item transfer and
explicit mapping before S6's closing composition.

## What's working

- Added the Kit menu's `Copy experiments from another kit…` after Show in Explorer. It is offered only while the kit
  takes writes and no source is open, and it opens the kit folder dialog (`kitPicker.pickThen`).
- Added the copy panel under the send progress:
  - the source kit's name;
  - a list of the source's experiments with a toggle each, bounded at 160 px and scrolling inside its own height;
  - Copy (shown only with a choice) and Cancel on one row, Cancel to the right.
- Added five fixed copy UiIds and `kitCopyExperimentToggle <id>` (`KitCopyExperimentToggle_<n>`), each an
  AutomationId.
- Added four `KitWindowTests` facts, one of them ui-smoke (pick, toggle, Copy, then `E2.experiment-json` lands in the
  kit).
- The kit-window scroll law now also holds with a running send and an open source of forty experiments.

## Tests

Development runs over the final source, recorded in `0226-impl-log.md`. The gate engine gives the verdict
(Invariant 6).

- **build:** exit 0, 0 errors.
  - It reported two `SYSLIB0051` warnings from vendored MathNet C# (`MathNetNumerics/Numerics/Exceptions.cs`). They
    appear whenever MathNet recompiles and are in this arc's build logs since step 0001; they are not our code.
  - The log has no lowercase `error`.
- **constructor-unit-tests** (all discovered): **1336** passed, 0 failed, 0 skipped (unchanged).
- **ui-smoke** (all discovered): **238** passed, 0 failed, 0 skipped.
  - That is 237 + 1, the new `pressing Copy experiments, a toggle and Copy brings the experiment into the kit`,
    which is `Passed` in the TRX.
  - The run took 200 s against the 600 s `attempt_timeout_seconds`; no deadline was exceeded.
- **ui-tests** over `KitWindowTests` and `ScrollExtentTests`: **23** passed, 0 failed, 0 skipped.
  - `KitWindowTests` went from 8 to 11, the three new identities.
  - `ScrollExtentTests` has 12, including the changed `the kit window: the kit bar and the page stay reachable`.
  - Each new or changed identity is `Passed` in the TRX.
- **elevation** (from the arc origin `f395597`, ELEV004 excluded):
  - `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, 194 s against its 600 s estimate;
  - 1315 suppressed findings (excluded-rule ELEV004 hits; 1312 at step 225) and 0 known findings.
- **The fourteen `invariant-s22-*` gates:** each grep finds nothing.
- **The facts bite.** Four mutants of `KitWindowView.fs`, in two rebuilt rounds, were each caught by the fact meant
  to guard them:
  - the verb offered whatever the access: caught by `a kit opened read-only offers no Copy experiments`;
  - the list's `MaxHeight` dropped: caught by the layout fact and by the scroll law (214 corners unreachable at
    800 × 600);
  - Cancel placed before Copy: caught by the layout fact (`Cancel must sit to the right of Copy, past a gap`);
  - a toggle that dispatches nothing: caught by the ui-smoke fact (`KitCopyChosenButton must be in the window`).
- **After the mutants:** the source was restored byte for byte and the solution rebuilt (0 warnings, 0 errors).
  The selected ui-tests (23) and `KitWindowTests`' five ui-smoke facts passed again.
- Logs are in `.artifacts/0226-dev-*.log` and `.artifacts/0226-mutants/`.

```yaml
gates:
  constructor_unit_tests: 1336
  ui_smoke_tests: 238
  ui_tests: 23
```

## Architecture

- **The panel is a pure projection of `kitCopyOpt`.** It sends only step 225's messages: `OpenCopySource` through
  the picker, `ToggleCopiedExperiment`, `CopyChosenExperiments` and `CloseCopySource`. A test and the panel therefore
  drive the same update.
- **The verb follows `model.kitWrites`**, the single access decision (step 185). The update's `guardKitWrite`
  still refuses a read-only kit on its own, so the view is not the only guard.
- **The list is bounded.** The bar sits outside the page's `ScrollViewer` (step 188), so the list scrolls inside a
  `MaxHeight` of 160 and a long source can never push a bar control or the page out of reach. `ScrollExtentTests`
  checks this at 800 × 600 and 1000 × 540.
- **Each toggle is a `CheckBox`.**
  - Its content is a wrapping `TextBlock`, `E<n> — <description>`.
  - It dispatches on `onClick` only, the existing check-box precedent, so the render never feeds back.

## Deferred

Nothing in this step's scope is deferred. Item transfer and explicit mapping are steps 227–229, as this step's brief
and step 225 name them.

## Gotchas

- `KitWindowTests.pressAtCentre` now takes any `Control`, not only a `Border`, so a fact can press a `CheckBox`. A
  `CheckBox` toggles on release, which `pressAtCentre` sends after the press, unless the press closed the window.
- `MaterialsWindowView.actionButton` keeps its first press closure (`OnChangeOf id`). This is safe for Copy and Cancel,
  whose messages are constant, but not for a model-dependent action.
- The app's `Program.fs` still builds its shell as `KitWindowShell.inert with …`, so the real window's picker answers
  `None`, as its zip picker does. Step 0233 wires
  `kitPicker = KitFolderPicker.ofWindow window context.locations.kitsFolder` there.
  - `OpticalConstructor.App` is outside this step's authorized edits.
  - The verb and the panel are proven headless through a shell whose picker answers a folder.
- The build may print two `SYSLIB0051` warnings from vendored MathNet C# whenever MathNet recompiles. They are
  third-party and pre-existing; an incremental rebuild prints none.

## Changelog

- 2026-11-09 — Step 0226: added the Copy experiments from another kit… verb and the copy panel (source name, a
  bounded toggle list, Copy / Cancel), the five copy UiIds and `kitCopyExperimentToggle`, four `KitWindowTests` facts
  (ui-tests 20 → 23 over the selected classes, ui-smoke 237 → 238), and the tallest kit-bar scroll fixture with a
  send and a forty-experiment source.
