# Step 0402 — implementation plan

## Approach

Four additions to `StudyEngine.fs`, placed after `calibrationProblemsWith` (step 401) and before `StudyFitRequest`, each
doc one to three lines citing spec 0050 S11.4:

1. `StagedStudy.engineProblemsWith` dispatches on the solution's engine and the stage, exactly as `runFit` chose the
   engine: `nonlinearProblemsWith` for a nonlinear solution, `calibrationProblemsWith` for a linear one under
   `CalibrationStage`, `objectProblemsWith` for any other linear one.
2. `EngineProblems.sigmasFor layout` builds one `FitEntrySigma` per layout entry, in order. Its σ comes from the first
   problem whose `entrySigmas` name that entry (parameter and owner), else `None`. Each problem's `entrySigmas` is
   computed once.
3. `SuspectCoordinate` (`[<ReferenceEquality>]`, since it holds a `FittedProblem`) and `EngineProblems.suspectsWith
   thresholds layout`. This walks each problem, each `EntryCoordinate` k and each of its entries:
   - `UndeterminedSigmaReason` when `sigmaAt k` is `None`;
   - `WideSigmaReason` of σ / (upper − lower) of the entry's layout bounds when that exceeds the sigma fraction;
   - `CorrelatedEntryReason` of the first entry of each other `EntryCoordinate` l with |ρₖₗ| above the threshold, when
     `quality` holds a correlation matrix.

   An entry with any reason is suspect. Each problem's quality is computed once and each coordinate's σ once.
4. `StagedStudy.realizabilityWith context layout fitted`, over the staged items:
   - It takes the items `Experiment.itemsUsedBy` the fitted runs, in that order, keeping the black boxes
     (`LibraryModelChoice` of `OpticalElementModel (BlackBoxMuellerModel _)`).
   - Each is read at the first fitted run that uses it (per-run `itemsUsedBy`): `layout.assignmentFor`, then
     `valuesOf`.
   - Its matrix is `BoundOpticalForm (form, values)`, evaluated by `muellerAt (ElementOrientation.transmittingAt
     Angle.zero)` at the run's operating wavelength. That wavelength is `boundWith`'s `operatingWaveLength`, step 367's
     rule as the forward applies it.
   - Its `ObjectRealizability` holds `cloudeEigenvalues`. Errors map to `StudyForwardFitErr` (setup, forward and the
     shared `evaluatedForFit`).

## Tests (StudyEngineTests, a new S11.4 section before the fit-run section)

- `an entry is suspect when ...`: step 397's two least-squares problems, rebuilt test-locally (independent [1 0; 0 1;
  1 1] and proportional), over test-local entries, with layouts of bounds [−2, 2] and [−0.5, 0.5].
- `every entry takes the sigma ...`: step 400's noisy QZ fixture. The layout's row-major entries take the σ of
  vec-order coordinate 4c + r. Two extra layout entries no problem holds (the Phone's gain, and QZ's m00 under another
  owner) take `None`.
- `every black-box object ...`: step 390's session fixture, parametrised by QZ's truth (`sessionFixture` gains a truth
  argument and its three callers pass `qzTruth`). The retarder truth is realizable; diag(1, 1.5, 0, 0) is not.
- `each engine's results ...`: step 389's calibration, step 390's object and step 387's nonlinear solutions through
  `engineProblemsWith`.

## Risks

- Realizability tolerance: the fitted QZ must be within round-off of the retarder for `RealizableMueller`; this is
  checked in the development run.
- Record label inference: `SuspectCoordinate` adds the labels `entry` and `reasons`, which `ParameterProfile` and
  `FitEntrySigma` also use, so every new construction site is annotated.
- Runtime: StudyEngineTests is the suite's critical path (≈123 s of 138 s). The new facts add one nonlinear fit (≈6 s)
  plus cheap linear fixtures.
