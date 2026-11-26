# Step 0401 — impl plan

## Approach

The calibration stage gets its `EngineProblems` (spec 0050 S11.4), read off stage one's three AIR fringe fits.

1. **`AirRunRows` and `AirFamilyRuns`** (`StudyEngine.fs`). `AirRunRows` is one AIR run's kept capture rows.
   `AirFamilyRuns` holds the Dark rows and each family's runs. Its `stageOneRows` member gives each family's rows run
   after run as a `StageOneAirRows`.
2. **`StagedStudy.airRunsWith context prefits`.** `calibrateLinear`'s selection moves here. The Dark runs' rows come
   in prefit order. Each family's runs come in study order, each with its prefit's kept capture rows
   (`ExperimentPrefit.captureRows`).
   - `calibrateLinear` becomes `calibrateFromAirRows solver` of `(airRunsWith …).stageOneRows`, then the same
     constants mapping as before.
   - One private helper, `calibrationFamiliesIn`, gives the `(run, CalibrationRunFamily)` pairs. Both members read it,
     so the family selection is not duplicated.
3. **`StagedStudy.calibrationProblemsWith context prefits solver`.** It reads `airRunsWith`, then stage one (c).
   - Each family's trace is `reduceAirTraces c.darkMean` of its rows.
   - The coordinates of family F are its runs' k-th stage-1 constants. Each is an `EntryCoordinate` of every layout
     entry taking that constant, or `FringeVisibilityCoordinate` for the LP-LP visibility.
     - Bounds are the intersection of the entries' unknown bounds (unbounded when there is none).
     - Starts are `constantIn c`, exactly the value `calibrateLinear` writes. The LP-LP visibility starts at
       `freeCosineFit solver` of its trace.
   - The residuals:
     - (a) `fringeResidualAt (Angle s₀) (FringeVisibility s₁)`;
     - (b) `fringeResidualAt (Angle s₀) (FringeVisibility (cos s₁))`;
     - (c) `{ thetaRel = s₀ + 45°; retardance = s₁ }.lpFrameResidualOn trace c.lpZero`.
   - `runResiduals`: each AIR run's own trace under its family's residual at the problem's start, families in order
     LP-LP, LP-CPL, CPL-LP. `productChecks` is empty.

## Placement

F# needs a member declared before its caller. `AirRunRows`, `AirFamilyRuns` and `airRunsWith` must therefore precede
`calibrateLinear`, which reads them, so they go right after `ExperimentPrefit.captureRows`. `calibrationProblemsWith`
follows `objectProblemsWith`, as the slice says. This is a recorded deviation, the same as step 400's.

## Tests (`StudyEngineTests`, a new S11.4 calibration section after the object-stage problems)

- `the calibration fit's problems are the three AIR fringes in the instruments' own constants`
- `each instrument constant of a calibration fit gets its sigma from its fringe`
- `each AIR run of a calibration fit has its own trace's residual`

`jittered` is generalised to take its amount, so the AIR rows can be moved by 2 · (−1)ⁱ without a second copy of its
body.

## Risks

- New record fields `run` and `rows` may capture an unannotated record-label inference later in the file (the build
  will show it). Construction sites are annotated.
- Stage one reports CPL2's zero in [0°, 180°), so it is 178° against bounds of ±0.5 rad. That coordinate's start lies
  outside its bounds. The slice prescribes both, and nothing in this step searches.
- ELEV: elevate at origin (a `let`, never a wrap at the call), no free one-parameter function of our types.
