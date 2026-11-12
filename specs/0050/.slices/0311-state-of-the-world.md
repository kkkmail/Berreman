# Step 0311 — state of the world

## Where we are

Spec 0050 section S9.2 covers import profiles and the column-mapping wizard (001 §K). Step 308 gave the wizard its
pure MVU, step 309 its view, and step 310 the kit window's messages (`OpenImportWizard`, `SaveImportProfile`,
`AttachDataFile`, `ImportWizardApplied.messages`). This step gives the wizard its own window, and adds the shell member
that opens it over the kit window's dispatch. Next, step 312 composes `.withImportWizard window dispatch` into the
App's `ConstructorScene.mount` and proves the whole path end to end through a real folder kit.

## What's working

- Add `ImportWizardWindow`: the column-mapping wizard in its own window (title `Import data file`, id
  `importWizardWindow`), sized 1000 × 700 and fitted to its screen with `WindowFit.keepOnScreen` (spec 0050 S9.2).
- Add `ImportWizardOpening.KitWindowShell.withImportWizard owner dispatch`: the shell's `openImportWizard` shows the
  wizard owned by the kit window, and Attach dispatches `applied.messages` in order.
- Closing the wizard any way other than Attach applies nothing.
- `ScrollExtentTests` gains `the import wizard leaves everything reachable`, over a twelve-column, forty-row lab file,
  for both the plain window and a column mapping with a scan column.
- `ImportWizardTests` gains the ui-smoke acceptance fact: pressing a profile, then Attach, in the shown window records
  exactly `[ AttachDataFile (ExperimentId 1, file, Phone ROI CSV) ]` and closes the window.

## Tests

These were development runs of each gate's own command over the final tree. The arc-runner's gate engine runs the gates
themselves.

| Gate | Result | Time | Deadline |
|---|---|---|---|
| build | exit 0, `0 Error(s)`; the full build's only warnings are the two pre-existing vendored MathNet SYSLIB0051 (operator note at 0185) | 79 s full, 17 s incremental | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`; known findings 0; ELEV004 excluded (suppressed 1428) | 204 s | 600 s |
| constructor-unit-tests | Passed!, 1525 of 1525, 0 failed, 0 skipped | 70 s | 180 s |
| ui-smoke | Passed!, 252 of 252, 0 failed, 0 skipped | 219 s | 600 s |
| ui-tests (selected: `ImportWizardTests`, `ScrollExtentTests`) | Passed!, 25 of 25, 0 failed, 0 skipped | 13 s | 360 s |
| invariant-s30-\* (16) | each exit 0, `INVARIANT HOLDS` | under 1 s each | 300 s |

- **ui-smoke: 252**, which is 251 + 1, as the slice says. The TRX shows `acceptance: the import wizard window maps a
  picked file and its Attach reaches the kit window as messages` as `Passed`.
- **ui-tests: 25** over the two selected classes, which is 24 + 1. The TRX shows `the import wizard leaves everything
  reachable` as `Passed` (0.64 s).
- **constructor-unit-tests: 1525**, unchanged from step 310, as expected for a step that adds no domain fact.
- **The first ui-smoke run failed 251 of 252.** The new fact's mouse-up landed on the wizard after Attach's press had
  closed it. The fix gives the fact's `pressControl` the guard `ImportDialogTests` already has. After a rebuild,
  ui-smoke and ui-tests passed.
- **Elevation and constructor-unit-tests ran before that test-only fix**, which neither reads, so they were not re-run.
- The suppressed count rose by 2 over step 310. These are ELEV004, from the window's two
  `ConstructorUiId.importWizardWindow.value` reads at the Avalonia property line, the same as every other window.
- **The new law is not vacuous.** With the preview's `ScrollViewer` temporarily removed, it failed with
  `ImportWizard at 800×600: 311 unreachable`. The view was then restored byte for byte (`git diff --exit-code`).
- **Red by construction.** `ImportWizardWindow` and `withImportWizard` are new, so both facts fail to compile against
  step 310's tree.
- No run went over its effective deadline.
- Every touched file is LF only (0 CR bytes), and `git diff --numstat` equals the `--ignore-cr-at-eol` count.

```yaml
gates:
  constructor_unit_tests: 1525
  ui_smoke_tests: 252
  ui_tests: 25
```

## Architecture

- **The window is a thin host, in `ImportDialogWindow`'s shape.** It builds the `ImportWizardContext` from its request,
  the caller's `onApplied` and its own `Close`, and runs the pure MVU of steps 308 and 309 unchanged. Only
  `ApplyImportWizard` reaches `onApplied`, so closing the window applies nothing.
- **The shell extension is the composition seam.** `withImportWizard` lives in `module ImportWizardOpening`, after the
  window, because `KitWindowShell` (in `TableAndElementRotationView.fs`) compiles before any window type. The kit
  window's update still only hands a request to `openImportWizard`. The wizard's Attach comes back through
  `ImportWizardApplied.messages` as the window's own messages, so a new profile is saved before the file is mapped.
- The module is not `AutoOpen`: step 312 opens it in `Program.fs`.
- No stored format, schema, version or package changed.

## Deferred

Nothing. Every requirement of the slice landed this round. Step 312, the App's composition and the end-to-end folder-kit
acceptance, is a later slice, not a deferral.

## Gotchas

- **In the running app, Attach data file… still opens nothing until step 312.** Step 310's state-of-the-world said step
  311 sets the App's shell. This slice's effects cover only the Ui and Ui.Tests projects, and step 312 composes
  `.withImportWizard` into `ConstructorScene.mount`.
- **Attach acts on pointer-press** (`MaterialsWindowView.actionButton`). A test that presses a closing button must
  not release on the closed window. `ImportWizardTests.pressControl` now releases only while the window is visible.
- **The scroll-extent fixture's lab file is read in memory**, under a test-local name, like step 308's class
  fixture. Nothing touches the disk. The preview shows the first 20 of the 40 rows (`previewWith`), which is still
  taller than both small screens.
- **The acceptance fact finds the wizard through `owner.OwnedWindows`**, which proves `Show owner`. It asserts that
  the list is empty again after Attach.
- **ui-smoke's per-test durations include queueing** on the one headless session.
- **The build's two warnings are not this step's.** They are vendored MathNet SYSLIB0051.
- `specs/0050/.manifest.state.json` was already dirty from the supervisor when the round began. This round did not
  touch it.

## Changelog

- 2026-11-11 — Step 0311: the column-mapping wizard gets its own window (S9.2).
  - `ImportWizardWindow` (keepOnScreen, 1000 × 700, id `importWizardWindow`).
  - `ImportWizardOpening.KitWindowShell.withImportWizard`.
  - `ScrollExtentTests`: `the import wizard leaves everything reachable`.
  - `ImportWizardTests`: the ui-smoke window acceptance fact; the private `pressControl` now releases only on a
    window that is still open.
