# Where we are

Step 0440 proves AT-2's exchange path as AT-1 does (spec 0050 S12.2; 001 §Q 'Send', §H).

- Steps 0432–0439 build AT-2's kit, commit it and prove its read path.
- This step sends the built kit through `SendKit` and reopens it through `OpenKitZip`. The reopened kit equals the
  built one file by file, and its study's start residual equals the built one entry for entry.
- AT-1's step-430 round trip now goes through the same function.

# What's working

- Add the fact `the absorbing-biaxial-noisy kit travels through Send kit and Open kit from zip unchanged, its
  study's residual at the start point included`.
- Extract step 430's inline round trip into `sendAndReopen (context) (model) (run)`. AT-1's send fact now calls it
  and keeps every assertion.
- Share the start-residual check as `startResidualOf`, and the run-folder path as `runFolderOf`, which both builds
  now use.
- AT-2's reopened kit holds every kit file, `library/**` and `data/**` (the five observation files included), byte
  for byte. Its `AbsorbingTriclinic` residual at the start point equals the built one's.

# Tests

These are development runs, evidence only; the supervisor's gates decide. Each used the gate's own command and
selection.

- **build:** exit 0 with 0 errors, 87 s against 600 s. The only warnings are the exempt NU1902 and the vendored
  MathNet's four pre-existing SYSLIB0051.
- **elevation** (ELEV004 excluded, all 13 projects): `ELEV-GATE: OK - 0 new finding(s)`, 315 s against 600 s. The
  code diff is test-only.
- **kit-acceptance** over `OpticalConstructor.Ui.Tests.KitAcceptanceTests`: `Passed!`, 1517 s against 3600 s.
  - Total 15: 14 succeeded, 1 skipped, 0 failed. The count rises from 13 to 14.
  - The skip is the explicit five-seed ensemble (TRX `NotExecuted`), as at 0437–0439.
  - The new fact took 6.4 s. AT-1's refactored send fact took 11.8 s.
- **ui-tests** (`StudyViewTests`, `DataStepTests`): `Passed!`, 18 of 18, 35 s against 360 s.
- **ui-smoke** (all discovered): `Passed!`, 264 of 264, 263 s against 600 s.
- **The twenty-one s42 invariants:** each decoded `git grep` finds nothing.
- **Nothing written into `data/`:** `git status --short data/` is empty after the class run.
- No development run exceeded its gate's effective deadline.

```yaml
gates:
  kit_acceptance_tests: 14
  ui_smoke_tests: 264
  ui_tests: 18
```

# Architecture

- **Code:** only `OpticalConstructor.Ui.Tests/KitAcceptanceTests.fs` changed. No production code changed, and
  R0001–R0005 are preserved.
- **The round trip:**
  `sendAndReopen (context : AppContext) (model : TableAndElementRotationModel) (run : FolderName) : KitFolder * TableAndElementRotationModel`.
  - It works on its own copy of the model. The copy's `kitShell` holds `KitArchiveProxy.create ()` and
    `KitArchiveJobHost.immediate`, which records `SendKitProgressed` and `SendKitEnded`.
  - It sends `SendKit` of `kitFile.name.zipFileName` in `runFolderOf run`, outside the kit.
  - It asserts what step 430 asserted: the job, the file count, the progress, the idle job, the status, the zip on
    disk and no `.oc/` entry.
  - A start screen over `context` then sends `OpenKitZip` into a fresh `received/<kit file stem>`, and its recorded
    messages fold through `updateKitStart`.
  - It answers the folder and `TestAppScope.inverseModelOver context opened`.
- **Where the start screen's settings come from:** its clock, kit id and author are the window's own
  (`model.kit.clock`, `kitFile.id`, `kitFile.author`). For AT-1 these are exactly the values the fact used to name
  literally.
- **Closing:** a failure after the open closes the reopened kit and rethrows. On success the caller closes it
  through the returned window's `kit`.
- **The facts:** each one compares the built and reopened folders with `assertSameKitFiles`, and their residuals with
  `startResidualOf`, which requires `Ok` and a non-empty vector.
- **AT-2's fact:**
  - It runs under the fixed run `AT-2-send`, over `absorbingBuild`, which it never closes or writes.
  - It also runs `assertNoisyObservationFiles` on the reopened folder, so the five observation files are named
    explicitly rather than only counted by the comparison.

# Deferred

Nothing.

# Gotchas

- **`KitArchiveProxy` does not create the zip's parent folder.** `sendAndReopen` creates the run folder first. AT-1's
  run folder already exists (the build's); AT-2's `AT-2-send` does not.
- **The shared AT-2 build stays intact in any fact order.**
  - `SendKit` only reads the kit folder; the zip streams to `<zip>.tmp` beside the zip.
  - `OpenKitZip` hands the kit to `showKit` directly, with no import dialog.
  - `resolveSnapshots` decides without importing, so the build's context stores are unchanged.
- **AT-2's send run has a fixed name.** A leftover `AT-2-send` is removed first, and the folder is removed in
  `finally`.
- **Where the time goes:** AT-2's lazy build (its fit through the shell) is charged to whichever AT-2 fact xUnit runs
  first. In this run that was the grid fact, at 14 min 11 s. R0005's 1800 s bound on the shell fit is unchanged.

# Changelog

- 2026-11-16, step 0440:
  - added AT-2's Send kit / Open kit from zip fact;
  - extracted `sendAndReopen`, which AT-1's send fact now calls, plus `startResidualOf` and `runFolderOf`.
