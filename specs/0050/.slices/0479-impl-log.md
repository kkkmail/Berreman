# Step 0479 — impl log

## Progress

- Read the task, the system and project prompts, the slice and 0478's state of the world. Read rulings R0001–R0005: at
  this step each is preserve-only (dispatch, analyzer law, element ids, single evaluation and concurrent residual,
  single `rhoT` read and server GC). This step touches none of those code paths.
- Checked the analyzers before writing:
  - ELEV006 pairs only `to<Target>` members, so `ParameterValue.angleUncertainty` beside `AngleUncertainty.parameterValue`
    is not a bijection under the rule.
  - ELEV001 flags only `Ctor x.value` re-wraps.
- `BenchNoise.fs` (after `NoiseParam`):
  - Added `type ParameterValue with` `angleUncertainty`, `elementUncertainty` and `throughputUncertainty`.
  - Added `type ParameterValues with member this.benchNoise : Result<NoiseParam, ItemEvaluationError>`.
- `PlannerInformation.fs` (after step 478's declarations):
  - Added the private literal `unsolvedPenalty` (1.0e3, `residualFor`'s penalty).
  - Added `type NoiseParam with member this.noiseVarianceOver`.
  - Added the module function `candidateRowsOver`.
- `PlannerInformationTests.fs`:
  - Added the opens these facts need.
  - Added the test-local `incidenceForward` / `indexForward` and the two `configurationOf` configurations.
  - Added the two census facts.
- Built the solution, then ran the class, the whole constructor suite, elevation, kit-acceptance and unit-tests (see
  Testing state).

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Optimization/BenchNoise.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Optimization/PlannerInformation.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/PlannerInformationTests.fs`

## Decisions

- **`benchNoise` reuses `ParameterValues.checkedFor`.** The axes are the budget's own, `NoiseParam.noiseless.parameterAxes`
  (`checkedFor` reads only their names and kinds). That check covers:
  - a missing value (`MissingParameterValueErr`);
  - a non-finite value;
  - an element or throughput outside [0, 1] (`ParameterOutOfRangeErr`).

  An angle's kind is unbounded, so a negative rotation is refused afterwards as `ParameterOutOfRangeErr` in the same
  `<name> = <value> lies outside [0, ∞)` form. The three values then go through the three new `ParameterValue`
  members. Unrelated entries are never read.
- **The misaligned readings reuse `observeWith`.** It runs over a turned proxy `{ muellerOf = fun c -> forward.muellerOf
  (c.misalignedBy Δi Δa) }`, so each reading is filed against the nominal configuration, as `noisyObserveWith` files it.
  The model side is `throughputResidualFor forward readings (scalingAround [] nominal nominal) (ScaledVector [||])`.
- **The penalty is a private `[<Literal>]`.** `MuellerFitting.residualFor`'s `1.0e3` is inline there and not exported,
  and `MuellerFitting.fs` is outside this step's edits. The literal's doc names its source (step 336).
- **No configurations gives `NoiseVariance 0.0`.** With no configurations there is no entry, so the mean would be 0/0.
  It answers 0.0 instead of NaN, which `PlannerInformation.over` refuses as noiseless all the same.
- **`candidateRowsOver` takes `observeWith` once for its own Jacobian.** `noiseVarianceOver` takes it again (m forward
  solves, small beside the (k + 1)·m of the Jacobian). Both orders give the same first error.

## Testing state

These are development runs on the final sources; no source changed after the build these runs used.

- **build**: `dotnet build Berreman.slnx -c Release -nologo -v:m` exited 0 in 86 s (deadline 600 s).
  - No `error` appears in the log.
  - The only warnings are the exempt `NU1902`s and the vendored MathNet's pre-existing 4 `SYSLIB0051`s.
- **elevation**: `Run-ElevationGate.ps1 -ExcludeRules ELEV004` (no base, so all 13 projects) answered
  `ELEV-GATE: OK - 0 new finding(s) in 13 project(s), 238s` (deadline 600 s). Known findings 0, suppressed 1770,
  unchanged from 0478.
- **constructor-unit-tests**: the whole suite, as the gate's `all_discovered` runs it, answered `Passed!` with 1875 of
  1875 in 115 s (deadline 180 s).
  - The count is 1873 + 2.
  - Both census identities are `Passed` in the TRX.
  - The class alone: 9 of 9.
- **kit-acceptance**: selected `OpticalConstructor.Ui.Tests.KitAcceptanceTests` under `Category=kit-acceptance`.
  `Passed!`, 14 succeeded and 1 skipped (total 15) in 1460 s (deadline 3600 s).
  - The skipped fact is AT-2's Explicit ensemble.
  - AT-2's grid-generator fact (with the fit) took 809 s, inside R0005's 1800 s.
- **unit-tests**: selected `BerremanTests.MuellerInverseTests` and `BerremanTests.BiaxialInverseTests`, with the gate's
  command. `Passed!`, 31 of 31 in 1374 s (deadline 7200 s).
- **invariant-s46-\*** (21): each decoded `git grep` finds nothing.
- **Line endings**: the three edited sources and this step's markdown hold no CR byte, and `git diff --numstat` equals
  `--ignore-cr-at-eol`.

## Artifacts

- `specs/0050/.artifacts/0479-scratch/fd-check.fsx`: the finite-difference precision check behind the tolerance deviation.
- `specs/0050/.artifacts/0479-build-dev-01.log`
- `specs/0050/.artifacts/0479-planner-tests-dev-01.log`
- `specs/0050/.artifacts/0479-constructor-unit-tests-dev-01.log`
- `specs/0050/.artifacts/0479-elevation-dev-01.log`
- `specs/0050/.artifacts/0479-kit-acceptance-dev-01.log`
- `specs/0050/.artifacts/0479-unit-tests-dev-01.log`

## Gotchas

- **Tolerance deviation (candidate rows).** The slice asks for the rows' non-zero entries "each within 1e-9 relative".
  At an index near 1.5 this is not reachable with the offset frame the slice mandates:
  - The frame steps 1.49e-6 scaled units (`√ε · 100`). At the 1e-3 scale that is 1.49e-9 of an index whose ulp is
    2.2e-16, so `ofScaled`'s `centre + v · scale` resolves the move to 7.45e-8 relative at worst.
  - `fd-check.fsx` measures 5.96e-8 for both n_o 1.544 and n_e 1.553. The n_o column at the zero component is exact.

  The fact therefore holds those two entries to 1e-7 relative. Everything else stays as the slice specifies:
  - every other entry within 1e-12 of zero;
  - the noise exactly `noiseVarianceOver`'s, and within 1e-9 of its closed form;
  - all of the variance fact's comparisons at 1e-9.

  The angle sensitivity there resolves to ~2e-11, because the 10° and 20° incidences have ulps of 2.8e-17 and 5.6e-17.
- **The rows fact centres the scaling at each nominal.** This holds for the n_o 0.0 case as well. A scaling centred at
  1.544 with nominal 0.0 would start the scaled vector at −1544, and `−1544 + h` loses a further 1.5e-7 of the step.
- **The variance fact's extended assertions are inside its own identity.** No test identity was added beyond the census
  pair. The fact covers:
  - the budget read through `benchNoise` with all three axes nonzero and an unrelated negative `gain`;
  - a missing throughput axis refused with the counting forward's count unchanged;
  - negative, NaN, infinite and above-one values refused as `ParameterOutOfRangeErr`.
