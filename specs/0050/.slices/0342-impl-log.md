# Step 0342 — impl log

Obligation S101-COVARIANCE-MOVE. This step moves a fit's own account of its uncertainty from
`BerremanTests/InverseFitHarness.fs` into `OpticalConstructor.Optimization`. No operator note and no
step-specific operator ruling were in flight. The standing ruling `specs/0050/.manual/013` (elevation from the arc
origin) applies.

## Progress

- [x] **`MuellerFitData.report`.** `fitReportOf` became `member this.report : FitQuality.FitReport` on
  `MuellerFitData` in `MuellerFitting.fs` (ELEV013). Its body is unchanged: `FitQuality.reportFrom` over the residual
  closure, the solution as a `DesignVector`, and `solution.finalResiduals`.
- [x] **`MuellerFitError`** gained `UnknownAxisErr of ItemModels.ParameterName`, documented as "no fitted parameter
  has that name".
- [x] **Created `FitUncertainty.fs`** (`module FitUncertainty` in `namespace OpticalConstructor.Optimization`).
  - It is compiled directly after `RecoveryEnsembles.fs` and before `Synthesis.fs`, with one `<Compile>` line and
    no comment.
  - The module doc is two lines and cites spec 0050 S10.1 and 001 §M.
  - It declares:
    - `ParameterWeights` (`float[]`, `[<Softellect.Sys.NumericCarrier(...)>]` with the reason the brief gives) and
      `PredictedVariance` (`double`), each with a `value` member;
    - `predictedRelativeErrors`, with its signature and body as they were;
    - `combinationVariance : FitReport -> ParameterWeights -> PredictedVariance`;
    - `differenceWeights : ParameterScaling<'Parameters> -> ParameterName -> ParameterName -> Result<ParameterWeights, MuellerFitError>`.
- [x] **Deleted the four declarations from `InverseFitHarness.fs`:** `fitReportOf`, `predictedRelativeErrors`,
  `combinationVariance` and `differenceWeights` (52 lines).
- [x] **Updated the callers.** Only the changes the step names were made:
  - `fitReportOf first.fit` → `first.fit.report` in `MuellerInverseTests` and `BiaxialInverseTests`.
  - In `MuellerInverseTests`' ensemble fact:
    - `pairVariance` is now
      `combinationVariance quality (differenceWeights first.fit.scaling (ParameterName "n_e") (ParameterName "n_o") |> expectOk)`.
    - It is read as `pairVariance.value` in `pairPrediction` and in the assertion message.
  - Both files open `OpticalConstructor.Optimization.FitUncertainty`, because the compiler needs it for
    `predictedRelativeErrors` (and, in `MuellerInverseTests`, for `combinationVariance` / `differenceWeights`).
- [x] Ran build, elevation, the constructor suite, `MuellerInverseTests` and the 19 invariant greps as development
  runs (see Testing state).

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Optimization/OpticalConstructor.Optimization.fsproj`: one
  `<Compile>` line.
- `Berreman/OpticalConstructor/OpticalConstructor.Optimization/FitUncertainty.fs`: new.
- `Berreman/OpticalConstructor/OpticalConstructor.Optimization/MuellerFitting.fs`: added `UnknownAxisErr` and
  `MuellerFitData.report`.
- `Berreman/BerremanTests/InverseFitHarness.fs`: removed the four moved declarations.
- `Berreman/BerremanTests/MuellerInverseTests.fs`
- `Berreman/BerremanTests/BiaxialInverseTests.fs`

## Decisions

- **The numbers are bit-identical.**
  - `report` runs exactly what `fitReportOf` ran, on the same arguments.
  - `predictedRelativeErrors` keeps its body character for character.
  - `combinationVariance` forms the same list `[ w.[i] * C.[i].[j] * w.[j] ]` in the same `i`, `j` order and sums it
    with `List.sum`. Only the result is wrapped, as `PredictedVariance`.
  - `differenceWeights` produces the same vector as before:
    - It is built by `Array.init` instead of `Array.zeroCreate` plus two writes.
    - The elements are `if i = m then -1.0 elif i = p then 1.0 else 0.0`, so `minus` wins at a coinciding axis
      (−1.0, as before), and every other element is the same positive `0.0`.
- **`differenceWeights` resolves both indices first, then matches on the pair.**
  - Each index is `List.tryFindIndex ((=) name)` over the axis names. `ParameterName` is a single-case union over
    `string` with structural equality, so this is the same comparison as the old `a.name.value = name`.
  - An unknown `plus` is reported first, then an unknown `minus`. Each is `Error (UnknownAxisErr <that name>)`.
  - No mutation and no local `Result<int, _>` helper.
  - The two-`int option` match tuple has the same shape as `throughputResidualFor`'s `match logThroughput …,
    logThroughput … with`, which is ELEV-clean.
- **`report` reuses the existing seam shape.** It re-wraps the solution as a `DesignVector` in the same way that
  `MuellerFitting.residualJacobianAt` already does (an ELEV-clean shape). No new conversion member was invented.
- **Doc comments are cut to their contracts** (AGENTS.md "Comments: short, precise, useful"):
  - `predictedRelativeErrors`: two lines (what it converts and why it is comparable with an ensemble).
  - `combinationVariance`: two lines. The "offered, not trusted" hazard is kept, because the code cannot show it.
  - `differenceWeights`: one line, which records the coinciding-axis rule and the typed error.
  - `ParameterWeights`: one line.
  - `PredictedVariance`: the one line the brief gives.
  - `report`: one line, which names the scaled space its numbers live in.
- **Test-file prose was left alone.** The long comments in the two ensemble facts still describe what the facts
  measure, and the step names no comment edit.
- **No test was added, removed or renamed.** `differenceWeights`' unknown-axis branch is not exercised by a new
  test, because the step authorizes none.

## Testing state

Development runs of each gate's command over the final tree. The arc-runner's gate engine runs the gates themselves.

- **build:** exit 0 in 79 s, with 0 errors. The only warnings are the 2 vendored MathNet SYSLIB0051.
  (`.artifacts/0342-dev-build.log`)
- **elevation** (no base, so 12 of 12 projects): `ELEV-GATE: OK - 0 new finding(s)`, known findings 0, in 206 s.
  (`.artifacts/0342-dev-elevation.log`)
  - Suppressed findings went from 1497 to 1500. The +3 are all ELEV004 (excluded by the gate command, R7), at
    these `.value` reads in `FitUncertainty.fs`:
    - line 34: `DesignVector`, the moved `predictedRelativeErrors` body;
    - line 39: `ParameterWeights`;
    - line 42: `ParameterMatrix`.
- **constructor-unit-tests** (whole): Passed!, 1553 of 1553, 0 failed, 0 skipped, in 69 s. That is the same count
  as step 341. (`.artifacts/0342-dev-constructor-unit-tests.log`)
- **unit-tests** (`BerremanTests.MuellerInverseTests`): Passed!, 20 of 20, 0 failed, 0 skipped, in 705 s. That is
  the same count as step 341. (`.artifacts/0342-dev-unit-tests.log`; TRX copied to
  `.artifacts/0342-dev-unit-tests.trx.log`)
  - The change-expectation fact (`an ensemble of noisy experiments recovers every constant, and its scatter IS
    what one fit's own covariance predicts`) has TRX outcome `Passed`, in 9 min 48 s. Its bands, including the
    pair-covariance ratio in [0.5, 2.0], still pass unchanged.
  - No run exceeded its deadline (7200 s).
- **19 × invariant-s33-\***: each decoded `git grep` finds nothing, both over tracked files and with `--untracked`.
  (`.artifacts/0342-dev-invariants.log`)
- **Acceptance greps:**
  - `InverseFitHarness.fs` declares none of the moved names.
  - Each of these is declared exactly once in the tree: `ParameterWeights`, `PredictedVariance`,
    `predictedRelativeErrors`, `combinationVariance`, `differenceWeights`, `member this.report` and
    `UnknownAxisErr`.
  - `FitUncertainty.fs` sits directly after `RecoveryEnsembles.fs` in the fsproj.
- **LF:** every edited file and the new file have 0 CR bytes.

## Artifacts

- `specs/0050/.artifacts/0342-dev-build.log`
- `specs/0050/.artifacts/0342-dev-elevation.log`
- `specs/0050/.artifacts/0342-dev-constructor-unit-tests.log`
- `specs/0050/.artifacts/0342-dev-unit-tests.log`
- `specs/0050/.artifacts/0342-dev-unit-tests.trx.log`
- `specs/0050/.artifacts/0342-dev-invariants.log`

## Gotchas

- **`MuellerFitData.report` is a property that recomputes.** Each read takes a finite-difference Jacobian of the
  residual. Both ensemble facts read it once, into `quality`. A future caller that needs it more than once should
  bind it the same way.
- **`BiaxialInverseTests` (about 997 s) is not selected, so it was not run.** It compiles, and its two edits (the
  open, and `first.fit.report`) are the same substitutions that the selected class exercises.
