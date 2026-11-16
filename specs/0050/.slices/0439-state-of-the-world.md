# Where we are

Step 0439 proves AT-2's read path on the committed kit (spec 0050 S12.2; 001 §Q 'Open', AT-2; .spec-md §3.12).

- Steps 0432–0438 build the absorbing-biaxial-noisy kit through the app's messages and commit it as
  `data/absorbing-biaxial-noisy/`.
- This step opens that committed kit read-only against an empty library. From there it reproduces the observation
  files, the snapshot resolution, the data identity, the model identity and the fit, and it writes nothing.
- AT-1's read-only fact now goes through the same open and the same unchanged check.

# What's working

- Add the fact `the committed absorbing-biaxial-noisy kit opens read-only against an empty library, writes nothing,
  and reproduces AT-2's data, model and fit`.
- Extract `openCommittedReadOnly` and `assertCommittedUnchanged`. AT-1's read-only fact now calls them and keeps its
  assertions.
- Every AT-2 item resolves its model, preset, sample and material only through the kit's snapshots. The committed
  seed files equal the regenerated ones byte for byte.
- `StudyFitParam.runFit` over the read-only window recovers the absorbing crystal within the noisy suite's bands, and
  the committed folder stays byte-identical.

# Tests

These are development runs, evidence only; the supervisor's gates decide. Each used the gate's own command and
selection.

- **build:** exit 0 with 0 errors, 86 s against 600 s. The only warnings are the exempt NU1902 and the vendored
  MathNet's four pre-existing SYSLIB0051.
- **elevation** (ELEV004 excluded, all 13 projects): `ELEV-GATE: OK - 0 new finding(s)`, 344 s against 600 s. The
  code diff is test-only.
- **kit-acceptance** over `OpticalConstructor.Ui.Tests.KitAcceptanceTests`: `Passed!`, 1408 s against 3600 s.
  - Total 14: 13 succeeded, 1 skipped, 0 failed. The count rises from 12 to 13.
  - The skip is the explicit five-seed ensemble (TRX `NotExecuted`), as at 0437 and 0438.
  - The new fact took 549 s; AT-1's read-only fact, refactored, took 2.6 s.
- **Earlier filtered run:** the two read-only facts alone passed 2 of 2 in 626 s.
- **Nothing written into `data/`:** a sha256 manifest of both committed kits (175 files) is identical before and
  after the class run.
- **ui-tests** (`StudyViewTests`, `DataStepTests`): `Passed!`, 18 of 18, 35 s against 360 s.
- **ui-smoke** (all discovered): `Passed!`, 264 of 264, 299 s against 600 s.
- **The twenty-one s42 invariants:** each decoded `git grep` finds nothing.
- No development run exceeded its gate's effective deadline.

```yaml
gates:
  kit_acceptance_tests: 13
  ui_smoke_tests: 264
  ui_tests: 18
```

# Architecture

- **Code:** only `OpticalConstructor.Ui.Tests/KitAcceptanceTests.fs` changed. No production code changed, and
  R0001–R0005 are preserved.
- **The session:** `openCommittedReadOnly (kit : CommittedKit) : CommittedKitSession`.
  - It records the folder: relative files and directories, `.oc/` included, and bytes.
  - It opens through `TestAppScope.emptyLibraryAppContext` and the existing `kitStartOver`, sending `OpenKitReadOnly`.
    The start screen's clock is fixed at `refitTimeOf kit`.
  - It answers the context, the opened kit, the `inverseModelOver` window, the clock and the recording.
- **The checks:**
  - `assertCommittedUnchanged (kit) (recorded)` is the one unchanged-folder check.
  - `closeCommittedReadOnly` closes the kit, then runs that check in a `finally`.
  - `assertEmptyLibrary (context)` covers the item-model, live-library, material, sample and source stores.
- **The refit:** `readOnlyStudyFit (session)` is the one `StudyFitParam` composition both read-only refits use. It
  takes the window's forward context, the session clock, the SVD and the seed-1 deviates `StudyFitEnvironment.create`
  composes, and `model.kitFitRuns`.
- **Helpers retargeted:** `committedFolder` takes the kit. `assertAbsorbingStudy` and
  `assertNoisyObservationAttachments` take the window model instead of the build. `relativeDirectories` sits beside
  `relativeFiles`.
- **The new fact reads only through the window's read-only `model.kit.proxy`.** It reuses the existing AT-2 helpers
  (files, setups, experiments, configurations, study, fit run, attachments, data, residual, recovery).
  `assertAbsorbingSnapshotResolution` is the only new assertion helper.

# Deferred

Nothing.

# Gotchas

- **The session record holds the opened kit and the clock as well.** The slice names only the context, the window
  and the recording. The opened kit keeps step 429's `opened.*` assertions verbatim and lets the kit close; the clock
  stamps the refit with the clock the open used.
- **Some of step 429's assertions now run for both kits.**
  - Its generic open assertions moved into `openCommittedReadOnly`.
  - Its empty-library check now also lists materials, samples and sources.
  - Its start screen now uses `kitStartOver` with the kit's own id; `OpenKitReadOnly` never mints one.
- **Never create a `KitProxy` over a committed folder in a read-only fact.** Read through `session.model.kit.proxy`,
  the read-only view.
- **Where the time goes.**
  - The class runs two AT-2 fits: the lazy build's fit through the shell (charged to the first AT-2 fact, about
    790 s) and this fact's read-only refit (about 550–620 s).
  - That makes about 23.5 min against kit-acceptance's 3600 s.
  - R0005's 1800 s bound on the shell fit is unchanged.

# Changelog

- 2026-11-16, step 0439:
  - added AT-2's read-only fact over the committed kit;
  - extracted `openCommittedReadOnly`, `assertCommittedUnchanged`, `assertEmptyLibrary`, `readOnlyStudyFit` and
    `closeCommittedReadOnly`, which AT-1's read-only fact now calls;
  - retargeted two AT-2 helpers to the window model.
