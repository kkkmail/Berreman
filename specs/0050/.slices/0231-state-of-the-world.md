# Step 0231 — state of the world

## Where we are

Spec 0050, section S6 ("Send and receive kits; copy experiments between kits"), step 0231 (IMPLEMENT), obligation
`S6-ZIP-VIEW`.

- Steps 197–200 built the archive seam, the job host, the job state and the shared progress row.
- Step 230 made Open kit from zip… a set of semantic messages of the start screen.
- This step puts the button and its progress row on the screen.
- Step 232 gives the start window the real zip dialog (it still composes `KitZipPicker.none`).

## What's working

- Added the four Open kit from zip UiIds to the start-screen group: `OpenKitZipButton`, `OpenKitZipProgressBar`,
  `OpenKitZipProgressText` and `OpenKitZipCancelButton`.
- Added Open kit from zip… after Open kit… on the start screen's first row, hidden while an extraction runs.
  - It asks for the zip, then the folder, then sends `OpenKitZip`.
  - A cancel of either dialog sends nothing.
- A running extraction shows the shared progress row directly under that row: bar, `n / total` count and Cancel
  (`CancelOpenKitZip`).
- Added three `KitStartWindowTests` facts:
  - the ids;
  - the running row against the idle screen;
  - a ui-smoke press-through over recording pickers and a deferred job host.

## Tests

Development runs over the final source, recorded in `0231-impl-log.md`. The gate engine gives the verdict
(Invariant 6).

- **build:** exit 0, 0 errors. The final build has 0 warnings.
- **constructor-unit-tests** (all discovered): **1353** passed, 0 failed, 0 skipped (unchanged).
- **ui-tests** over `KitStartWindowTests`: **8** passed, 0 failed, 0 skipped. That is 6 + the two new facts, each
  `Passed` in the TRX.
- **ui-smoke** (all discovered): **240** passed, 0 failed, 0 skipped. That is 239 + the new fact, `Passed` in the TRX.
  - It took 213 s, run alongside elevation, against the 600 s `attempt_timeout_seconds`; no deadline was exceeded.
- **elevation** (ELEV004 excluded, all 12 projects): `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, 201 s.
  - 0 known findings, 1317 suppressed findings.
- **The fourteen `invariant-s22-*` gates:** each holds.
- **The facts bite.** Four mutants of `KitStartView.fs` were each caught:
  - the running guard dropped;
  - the dialogs swapped;
  - the progress row never drawn;
  - Cancel sending nothing.

  The source was restored byte for byte and rebuilt.
- Logs are in `.artifacts/0231-dev-*.log` and `.artifacts/0231-mutants/`.

```yaml
gates:
  constructor_unit_tests: 1353
  ui_smoke_tests: 240
  ui_tests: 8
```

## Architecture

- **The view only projects step 230's model.**
  - The button's press composes the two context dialogs and dispatches the same `OpenKitZip` a test sends.
  - The progress row is step 200's `kitArchiveProgressView` over `m.zipJob`, with the start screen's own ids, as the
    kit bar's Send kit row is.
  - No new message, model field or seam was added.
- **One mount body in the tests.** `withKitStartProgram update model0 check` holds the mount. `withKitStartScreen` is
  that over `updateKitStart`, so the smoke fact can record every message the screen sends without copying the mount.

## Deferred

Nothing in this step's scope is deferred. The real zip dialog in `KitStartWindow` is step 232's.

## Gotchas

- **The pickers' doubles answer inside the press.** `startPick` uses `Async.StartImmediate`, so `OpenKitZip` is sent
  before `pressAt` returns.
- **Build the handle and the progress inline.** `KitWindowTests` has private `recordingHandle` / `zippedFiles`, and
  sharing them would edit files outside this step's effects. The facts build both inline as record literals, as
  `ScrollExtentTests` does.
- **Annotate `KitZipOpening` record expressions.** `KitUnzipJob` shares the `zip` / `target` labels (step 230).
- **The zip button leaves the tree while a job runs** and comes back as a fresh keyed box. No stale press
  subscription survives.

## Changelog

- 2026-11-09 — Step 0231:
  - added Open kit from zip… and its progress row to the start screen, with four new `ConstructorUiId`s;
  - the test mount now takes its update;
  - added three `KitStartWindowTests` facts; ui-tests 6 → 8, ui-smoke 239 → 240.
