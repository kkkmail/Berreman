# Step 0383 — state of the world

## Where we are

This is spec 0050 section S11.3: engines, staged and joint fits, and append-only fit runs.

- **Step 0382** gave a study its fit start count.
- **This step adds the fit run's domain shape.** A fit run is the append-only record of one study fit,
  `<study>@<stamp>.fitrun-json` (001 §A, §E, §I). It holds the runs and data files it read, every parameter with its
  state, the fitted vector (each entry's start and fitted value), every multi-start member and every linear object
  solve.
- **Step 0384 writes the file and step 0385 the stage.** S11.4 adds the results.

## What's working

- Add `FitRuns.fs` (Domain, after `StudyForward.fs`): `FitStage`, `CalibrationUse` and `StudyEngineKind`, each with an
  int wire tag, `tryOfWire`, `text` and `all`
- Add the fit run's records: `FitRunMember` (with `FitMemberOutcome`), `FitRunEntry`, `FitRunParameter`,
  `ObjectMatrixFit` and `FitRun` (schema version 1), with `FitRunNotes`, `FitChiSquared` and `FitMemberFailure`
- Name a fit run `<study stem>@<UTC yyyyMMdd'T'HHmmss'Z'>` (`StudyName.fitRunNameAt`); `FitRunName.fileStem` gives its
  `.fitrun-json` stem
- Render a nonlinear solver failure as a member's failure (`NonlinearSolverError.failureText`)
- Add `FitRunTests` with three facts (constructor suite 1653 → 1656)

## Tests

```yaml
gates:
  constructor_unit_tests: 1656
```

| Gate | Result | Time (deadline) |
|---|---|---|
| build (`dotnet build Berreman.slnx -c Release -nologo -v:m`, from `Berreman/`) | exit 0, `0 Error(s)`. The only warnings are the exempt NU1902 advisories and vendored MathNet's `SYSLIB0051`, which steps 0360–0382 also show | 79 s (600 s) |
| constructor-unit-tests (the gate's own command, whole suite) | `Test run summary: Passed!`, 1656 succeeded (1653 at checkpoint 382, +3), 0 failed. In the TRX, all three new facts are `Passed` | 78 s (180 s) |
| elevation (`ASG_ARC_BASE_COMMIT` = arc base `f395597c`, ELEV004 excluded) | `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`, exit 0. Suppressed findings are 1540 (1539 at step 0382); the one new one is the excluded ELEV004, `FitRuns.fs:188`'s `at.value` where the stamp is formatted | 217 s (600 s) |
| the twenty s38 invariants (each decoded `git grep`) | all twenty find nothing | seconds each (300 s) |

These are development runs; the supervisor's gate engine is the authority. Every run is over the final source.

The facts this step creates, all in `OpticalConstructor.Tests.FitRunTests`:

- `a fit run is named by its study's file stem and the clock's instant`;
- `a fit stage, a calibration use and an engine round-trip their wire tags and render their text`;
- `a solver failure renders the reason a multi-start member failed`.

## Architecture

- **`FitRuns.fs` is the last Domain file.** A fit run names studies, items, materials, owners and attachments, so it
  compiles after `StudyForward.fs`.
- **Three closed sets with int tags.** `FitStage`, `CalibrationUse` and `StudyEngineKind` follow `FitMode`'s shape:
  `value` (1-based), `tryOfWire` (`None` for any other tag), `text` and `all`.
- **Outcome and owner in the serializer's case form.** `FitMemberOutcome` carries no int tag, as `SessionGainModel`
  does not (step 356). `FitRunEntry.owner` is an `UnknownOwner`, stored the same way as `StudyParameterRef`.
- **`ObjectMatrixFit`** keeps `ReconstructedMatrix`'s readouts: the items in light order, the sixteen elements m00 …
  m33 row-major as `ParameterValue`s, the rank and the RMSE.
- **The name.** `fitRunNameAt` formats the instant in UTC with the invariant culture, after the study's `fileStem`.
  The name's own `fileStem` re-applies the shared name rule, and `FitRunKitFile.fileNameOf` gives the
  `.fitrun-json` file name.

## Deferred

Nothing.

## Gotchas

- **`UnknownOwner`'s doc still says "Derived, never stored".** A fit run's entries now hold an owner, which step 0384's
  file stores in case form. `Studies.fs` is outside this step's authorized edits, so the doc is left as it is.
- **Record labels are shared.**
  - `FitRunEntry` and `FitRunParameter` both have `parameter`.
  - `FitRunMember` and `FitRunEntry` both have `start`.
  - `ObjectMatrixFit` shares `rank` and `rmse` with `ReconstructedMatrix`.
  
  A later record expression in a file that opens `FitRuns` may need a type annotation.
- **The stamp goes through `ToUniversalTime()`.** The clock supplies UTC (`DateTime.UtcNow`), which the call leaves
  unchanged. A `Local`-kind instant is converted, so the `Z` is always true.

## Changelog

- 2026-11-13 — Step 0383: `FitRuns.fs`: the fit stage, calibration-use and engine tags; the fit run's member, entry,
  parameter and object-solve records and `FitRun` (schema 1); `StudyName.fitRunNameAt`, `FitRunName.fileStem` and
  `NonlinearSolverError.failureText`. Three new `FitRunTests` facts; constructor suite 1656.
