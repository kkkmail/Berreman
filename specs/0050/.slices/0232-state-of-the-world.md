# Step 0232 — state of the world

## Where we are

Spec 0050, section S6 ("Send and receive kits; copy experiments between kits"), step 0232 (WIRE_UI), obligation
`S6-ZIP-WIRE`.

- Steps 197–200 built the archive seam, the job host, the job state, the zip dialogs and the shared progress row.
- Step 230 made Open kit from zip… a set of the start screen's messages.
- Step 231 put its button and progress row on the screen.
- This step gives the start window the real zip dialog. Open kit from zip… now works end to end in the app.

## What's working

- `KitStartWindow` takes `zipPickerFor : TopLevel -> KitZipPicker` after `pickerFor`, and composes `zipPickerFor this`
  in place of `KitZipPicker.none`.
- The launcher builds Main's and Inverse's start screens with `KitZipPicker.ofWindow` over the exports folder, where
  Send kit… writes.
- Every other `KitStartWindow` construction (`ScrollExtentTests`, `KitStartWindowTests`, `WireUiFinalCompositionTests`)
  passes a zip picker that answers nothing. Their assertions are unchanged.
- Added the ui-smoke fact `acceptance: Open kit from zip extracts a sent kit and opens it in the constructor of the
  screen's mode`. It covers a sent zip, the real worker extraction and the real start window. The kit opens
  read-write in Main, every sent file is received, the screen closes and the history holds the received folder.

## Tests

No gate verdict is claimed here (Invariant 6). After exit, the arc-runner runs the frozen roster: build, elevation,
constructor-unit-tests and ui-smoke (all discovered), ui-tests over `KitStartWindowTests`, `LauncherTests` and
`ScrollExtentTests`, and the fourteen s22 invariants.

The worker's development runs over the final tree are in the impl-log's `Testing state`, with logs under
`specs/0050/.artifacts/0232-*`:

- **build:** 0 errors and 0 warnings on the final build. The first full build printed only the vendored MathNet's two
  `SYSLIB0051`.
- **constructor-unit-tests:** 1353 passed, unchanged against checkpoint 230.
- **ui-smoke:** 241 passed. That is 240 + 1, the declared fact.
  - It took 199 s against the 600 s `attempt_timeout_seconds`; no deadline was exceeded.
- **ui-tests** over the three selected classes: 28 passed (8 + 8 + 12).
- **elevation** from the arc origin: `ELEV-GATE: OK`, 0 new findings in 12 of 12 projects.
- **The fourteen s22 invariant greps:** none finds anything.
- **Mutant check.** With the window composing `KitZipPicker.none` again, the new fact fails. The source was restored
  byte for byte and rebuilt before the runs above.

No test is removed or renamed, and no assertion is removed or weakened.

```yaml
gates:
  constructor_unit_tests: 1353
  ui_smoke_tests: 241
  ui_tests: 28
```

## Architecture

- **The window owns the dialogs' owner; the composition root owns their start folders.** `KitStartWindow` receives
  both dialogs as `TopLevel -> …` factories and applies them to itself. `LauncherWindow` decides where each starts:
  kits for the folder dialog (step 81, through `pickerFor`), and exports for the zip dialog, through one local
  `zipPickerFor`.
- **No new seam, message or model field.** The launcher's public signature is unchanged. The start folder comes from
  the `DataLocations` it already holds.
- **The acceptance fact runs the production host.** It uses the window's own `KitArchiveJobHost.onUiThread`. The
  extraction runs on a thread-pool worker and posts to the headless UI thread. The fact drains the dispatcher until
  the kit is shown, bounded at ten seconds.

## Deferred

Nothing in this step's scope is deferred. Open kit from zip… is wired end to end.

## Gotchas

- **`writeExperiment` is an extension** in `OpticalConstructor.Storage.KitContentFiles`; open it to write through
  `kit.proxy`.
- **A kit zip holds folder entries** (`"<path>/"`). Check a received tree entry by entry, a folder entry as a folder.
- **Step 230's sent-zip fixture is private** to `OpticalConstructor.Tests`. `KitStartWindowTests` keeps a shorter local
  helper rather than editing `KitStartTests.fs`, which is outside this step's effects. This is the same pattern as its
  copied history fixture.
- **A broken wiring costs ten seconds of smoke time**, not a hang: the wait is bounded and asserted after.
- **`grep -c $'\r'` in this Git Bash counts lines containing `r`.** Count CR bytes with Python, or use
  `git ls-files --eol`.

## Changelog

- 2026-11-09 — Step 0232:
  - wired the real Open kit from zip… dialog (`KitZipPicker.ofWindow` over the exports folder) into the launcher's
    start screens through the new `KitStartWindow` `zipPickerFor` argument;
  - added the end-to-end acceptance fact; ui-smoke 240 → 241.
