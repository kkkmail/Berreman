# Step 0288 — impl log

## Progress

- [x] Domain: Experiments.fs (Experiment v2, draft axes/blocks, commit/edit)
- [x] Domain: OutOfBandDiagnostic.requestedWavelengthsOf
- [x] Domain: ExperimentDataLoad.loadAndValidate
- [x] Storage: experiment.schema.json
- [x] Ui: chartForExperiment
- [x] Fixtures re-pointed (listed files plus those the build found)
- [x] New / changed tests
- [x] Development runs (all green on the final tree)

## Files modified

### Product

- `OpticalConstructor.Domain/Experiments.fs`:
  - `VariedElement` and `Experiment.variedLabel` are deleted.
  - `Experiment` becomes `{ id; setup; scan; role; sessionOpt; notes; dataFileOpt }`, and `description` is
    re-derived from the scan.
  - `ExperimentDraft` gains `outerAxes`, `closedBlocks`, `axisOpt` and `scanOpt`.
  - `canCommit` now matches `axisOpt`.
  - New: `addScanAxis` and `addScanBlock`. `commit` and `edit` are rebuilt on the scan.
- `OpticalConstructor.Domain/OutOfBandDiagnostic.fs`: `requestedWavelengthsOf` spans the scan's wavelength axes.
- `OpticalConstructor.Domain/ExperimentDataLoad.fs`: `loadAndValidate` handles a scan of one block of one axis, a
  scan of one block of no axis, and refuses anything else.
- `OpticalConstructor.Storage/schema/experiment.schema.json`: `scan`, `role`, `sessionOpt` and `notes` replace
  `varied`, `measurement`, `range` and `sourceWaveLengthOpt`. The version stays 1.
- `OpticalConstructor.Ui/TableAndElementRotationView.fs`: `chartForExperiment` charts a scan of one block of one axis
  only.

### Tests, from the slice's list

ExperimentProxyTests, ExperimentCollectionRoundTripTests, KitContentFileJsonTests, KitLibraryTests,
ExperimentCopyTests, OutOfBandDiagnosticTests, SetupTests, SetupModelTests, KitWindowModelTests,
ExperimentDataProxyTests, InverseConstructorTests and KitWindowTests.

### Tests the closing build found

These were re-pointed by the same rule (slice §6):

- `OpticalConstructor.Tests/KitOpeningTests.fs`
- `OpticalConstructor.Tests/KitStartTests.fs`
- `OpticalConstructor.Ui.Tests/KitStartWindowTests.fs`
- `OpticalConstructor.Ui.Tests/LauncherTests.fs`
- `OpticalConstructor.Ui.Tests/ElementMenuTests.fs`

SolverHandoffWindowTests and ScrollExtentTests needed no change: they only copy-update `dataFileOpt` / `id` or
build no experiment.

## Decisions

- **`addScanAxis` and `addScanBlock` are `ExperimentCollection` members** (`c.addScanAxis`, `c.addScanBlock`).
  - The first elevation run failed with two ELEV013 findings on the free functions, `Experiments.fs:545` and `:564`:
    one parameter, of our type.
  - AGENTS.md says the analyzers win (spec 0048 `.manual/002` R6). The arc's precedent is steps 178, 225 and 234,
    and `newDraft` already lives on the type.
  - The contract symbols' name and behaviour are kept; only the call form is `c.addScanAxis`.
- **Doc comments are trimmed to the slice's one to three lines**, each citing spec 0050 S9.1.
- **`commit` with no edited axis commits nothing**, even while editing (`scanOpt = None` → collection unchanged).
  Editing a dark line therefore gives a draft with no axis, and committing it leaves the experiment as it was. This
  keeps the slice's "editing then committing gives the experiment back" for a dark line too. The bay already
  withholds Add when `canCommit` is false.
- **`addScanBlock` reuses `addScanAxis`**. It moves the edited axis into the outer axes, then closes them on the
  capture, so the four-field clear lives once.
- **`requestedWavelengthsOf` reuses `requestedWavelengthsFor`** over a spanning range: the lowest `min` to the
  highest `max`, keeping the first axis's points. The nm-to-`WaveLength` crossing stays in one place.
- **`loadAndValidate` resolves the detector inside the accepted branches only**, so a multi-variable scan reads
  nothing: not the library, and not the data file.
- **`edit` of a scan with no blocks** (possible only from a hand-written file) opens an empty draft that edits it.

## Testing state

Every run below is on the final tree.

| Gate | Result | Time | Deadline |
|---|---|---|---|
| build | exit 0, 0 warnings, 0 errors | 27 s (incremental) | 600 s |
| constructor-unit-tests | Passed!, 1483 of 1483 | 75 s | 180 s |
| ui-smoke | Passed!, 249 of 249 | 206 s | 600 s |
| ui-tests (10 selected classes) | Passed!, 132 of 132 | 134 s | 360 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)` | 195 s | 600 s |

- constructor-unit-tests: 1480 + 3. Each new and changed fact is `Passed` in the TRX.
- ui-tests: all ten classes ran.
- invariant-s29-\* (16): every decoded `git grep` finds nothing.
- Extra check, not a gate: LauncherTests and KitStartWindowTests (non-smoke) pass, 16 of 16.
- Red evidence (mutation check):
  - `edit` taking the first axis as edited, together with `loadAndValidate` matching `axis :: _`, turned exactly
    the two new facts red (40 run, 2 failed).
  - The files were restored (SHA-256 OK) and rebuilt; every DLL copy postdates the restore.
- No run went over its effective deadline.

## Artifacts

- `specs/0050/.artifacts/0288-build-dev-1.log`: the first build after the Domain change. Test fixtures were red
  on the old shape.
- `specs/0050/.artifacts/0288-build-dev-3.log` and `0288-build-dev-4.log`: green intermediate builds.
- `specs/0050/.artifacts/0288-build-final.log`: the final build. 0 warnings, 0 errors.
- `specs/0050/.artifacts/0288-elevation-dev-1.log`: the first elevation run, with two ELEV013 findings, since fixed.
- `specs/0050/.artifacts/0288-elevation-final.log`: elevation OK.
- `specs/0050/.artifacts/0288-mutation-run.log` and `0288-mutation-hashes.txt`: the red mutation run and the
  restore hashes.
- Suite logs:
  - `0288-constructor-unit-tests-dev-1.log` and `-final.log`
  - `0288-ui-smoke-dev-1.log` and `-final.log`
  - `0288-ui-tests-final.log`
  - `0288-ui-tests-extra-classes.log`

## Gotchas

- **`Experiments.ExperimentNotes.none` does not compile.** With the module qualification, `ExperimentNotes`
  resolves to the union case, not the type (FS0039 `'none' is not defined`). The fully qualified namespace path
  fails the same way. Five test files that qualified `Experiments.` everywhere therefore now
  `open OpticalConstructor.Domain.Experiments`, right after `open OpticalConstructor.Domain`, so every later open
  keeps precedence:
  - KitStartTests and KitWindowModelTests;
  - SetupTests;
  - KitStartWindowTests and KitWindowTests.
- **The Bash tool breaks a quoted heredoc that holds an apostrophe.** Use the Edit tool for multi-line test edits.
