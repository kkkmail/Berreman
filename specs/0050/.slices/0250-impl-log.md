# Step 0250 — impl log (IMPLEMENT: cut plates through the spec 0044 Berreman forward)

## Progress

- [x] `ItemEvaluation.fs`:
  - `CutPlateSolveErr`;
  - `ElementOrientation.sampleAzimuth` / `observable`;
  - `CutPlate` / `ofForward`;
  - `BoundCutPlate` and its `muellerAt` arm.
- [x] `ItemEvaluationTests.fs`: two new facts and the widened render fact.
- [x] Red measured by a temporary mutation, then reverted byte for byte (see Testing state).
- [x] Development runs over the final tree: build, constructor tests (all discovered), elevation and the invariant
  greps.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/ItemEvaluation.fs`
  - `ItemEvaluationError.CutPlateSolveErr of reason : string`. Its `describe` renders
    `the cut plate's Berreman solve failed: <reason>`.
  - On `ElementOrientation` (step 247), two intrinsic members:
    - `sampleAzimuth : MuellerInverse.SampleAzimuth` is `MuellerInverse.SampleAzimuth this.rotation`;
    - `observable : MuellerInverse.Observable` matches the branch, with no wildcard: `TransmittedMueller` on
      `BranchTransmitted` and `ReflectedMueller` on `BranchReflected`.
  - `[<ReferenceEquality>] type CutPlate = { cut : MuellerInverse.SampleCut; thickness : Thickness; predict :
    MuellerInverse.MeasurementConfiguration -> Result<MuellerMatrix, MuellerInverse.ForwardModelError> }`.
    - `static member ofForward (forward : ForwardModelProxy<'Parameters>) (parameters : 'Parameters) (cut) (thickness)
      : CutPlate` holds `fun configuration -> forward.muellerOf configuration parameters`.
    - Nothing of `createBerremanForward` / `orientForCut` is copied.
  - `BoundOpticalElement.BoundCutPlate of CutPlate`.
  - In `muellerAt`, `BoundCutPlate plate` on `BranchTransmitted | BranchReflected` (an or-pattern, no wildcard):
    - it builds `{ cut; thickness; incidenceAngle = orientation.incidence; azimuth = orientation.sampleAzimuth;
      observable = orientation.observable; waveLength = w }`;
    - it calls `plate.predict`;
    - it maps `ForwardSolveFailedErr (_, reason)` to `CutPlateSolveErr reason`.
  - Docs: `ItemEvaluationError`, `BoundOpticalElement` and `muellerAt` are widened to name the cut plate. `muellerAt`
    keeps its `001 §I, §N` citation.
  - The module does not open `MuellerInverse`; every reference is qualified, as the module already qualifies
    `Propagation` and `MuellerReconstruction`.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/ItemEvaluationTests.fs`
  - New opens: `Berreman.MaterialProperties` (`OpticalProperties`, `RefractionIndex`, `RhoValue`), `Berreman.Solvers`
    (`SolverParameters`) and `OpticalConstructor.Domain.MuellerInverse`.
  - New private fixtures:
    - `uniaxialCrystal : UniaxialParameters`: n_o 1.544, n_e 1.553, g11 = g33 = 0;
    - `uniaxialForward`, which is `createBerremanForward` of a test-local builder, `OpticalProperties.fromRefractionIndex
      (n_o, n_o, n_e)`, at `SolverParameters.defaultValue`.
  - New facts:
    - `a cut plate is predicted through the Berreman forward at the element's spin, incidence, branch and wavelength`
    - `a cut plate whose forward fails answers CutPlateSolveErr`
  - `every evaluation error renders its condition and its payload` gains a `CutPlateSolveErr "degenerate"` row, so
    the distinct count goes from 6 to 7. No existing row or assertion is removed.

## Decisions

- **The test file opens `MuellerInverse`.** The fully qualified `MuellerInverse.SampleAzimuth.degree` does not
  compile (measured: `FS0039: The field, constructor or member 'degree' is not defined`). The qualified long
  identifier resolves `SampleAzimuth` to the union case, not the type.
  - The brief names `SampleAzimuth.degree 30.0<deg>`. The other suites over the same module (`MuellerInverseTests`,
    `BiaxialInverseTests`, `LangasiteInverseTests`) open `MuellerInverse` and call it unqualified, so this file does
    the same.
  - `MuellerInverse` is opened before `ItemEvaluation`, so `ItemEvaluation`'s names stay the latest in scope.
    `CutPlate.cut` / `thickness` and `MeasurementConfiguration.cut` / `thickness` are both resolved by the type
    annotation at each construction.
- **`CutPlate` is `[<ReferenceEquality>]`, as the brief prescribes.** Its `predict` is a function, and step 249 found
  that a function-bearing payload silently costs `BoundOpticalElement` its value equality. The step-248 binding fact
  (`assertBound … BoundOpticalForm`) still passes.
- **The forward's `configuration` payload is dropped from `CutPlateSolveErr`.** The brief declares
  `CutPlateSolveErr of reason : string` "carrying its reason", and the configuration is the evaluation's own input,
  which the caller already holds.
- **Assertions beyond the brief's minimum, inside the declared facts** (no new identity):
  - Fact 1:
    - the members themselves are checked: `sampleAzimuth` equals `SampleAzimuth.degree 30.0<deg>`, and `observable`
      is the right case on both branches;
    - a second wavelength (632.8 nm) is checked exactly against the forward, which proves the evaluation's λ reaches
      the configuration;
    - for the 60° turn, the largest element change must exceed 1e-3.
  - Fact 2: both branches answer `CutPlateSolveErr "degenerate"`.
- **Choosing 1 mm, Δn 0.009 and 600 nm.** These come from the brief. At normal incidence they give exactly 15 waves
  of retardance, so a normal-incidence plate would look like the identity at every azimuth. The brief's 20°
  incidence breaks that, and the 60°-vs-30° assertion confirms a real change.

## Testing state

These are development runs over the final tree. Invariant 6: the arc-runner's gate engine gives the verdict, not
these runs.

- **build:** `dotnet build Berreman.slnx -c Release -nologo -v:m` exit 0, `0 Warning(s)`, `0 Error(s)`
  (`.artifacts/0250-build.log`, and `0250-build-final.log` after the mutation revert).
  - The first build of the round failed on the `SampleAzimuth.degree` resolution (Decisions).
  - That first build also recompiled the vendored MathNet C#, which raised its two pre-existing `SYSLIB0051`
    warnings, as step 249 recorded. These are not from this step's code; every later solution build reports 0.
- **constructor-unit-tests,** the gate's own command (all discovered): `Passed!`, total 1397, succeeded 1397,
  failed 0, skipped 0, in 1 m 07 s against an estimate of 180 s (`.artifacts/0250-constructor-unit-tests.log`, TRX
  `.artifacts/0250-constructor-unit-tests.trx`).
  - 1397 is checkpoint 249's 1395 plus the two new facts.
  - The three declared identities are each `Passed` in the TRX.
- **elevation** with `ASG_ARC_BASE_COMMIT=f395597c…`, exit 0:
  `ELEV-GATE: OK - 0 new finding(s) in 12 project(s), 200s`. Known findings 0, suppressed 1350 (the excluded
  ELEV004, unchanged from step 249), and the Domain project has 0 findings (`.artifacts/0250-elevation-final.log`).
  - A first run (`0250-elevation.log`, also OK, 199 s) overlapped a doc-comment edit to `muellerAt`, so the run was
    repeated on the final tree.
  - The solution file counts as changed since the arc origin, so the driver widens to all 12 projects.
- **The fourteen s25 invariants,** by their gate commands: all 14 `INVARIANT HOLDS`. The same greps with
  `--untracked` also find nothing (`.artifacts/0250-invariants.log`).
- **Red, measured by mutation.** The new symbols did not exist before this step, so the facts could not compile
  against the prior tree. To show they test behaviour, `ItemEvaluation.fs` was mutated temporarily:
  - `sampleAzimuth` always answered azimuth 0;
  - the error mapping dropped the forward's reason.

  `--filter-class OpticalConstructor.Tests.ItemEvaluationTests` was then `Failed!`, 2 of 19
  (`.artifacts/0250-red-mutation.log`):
  - `a cut plate is predicted …` failed on `Expected: SampleAzimuth (Angle 0.5235987756)`, `Actual: SampleAzimuth
    (Angle 0.0)`;
  - `a cut plate whose forward fails answers CutPlateSolveErr` failed on `Values differ`.

  The file was restored from a copy (`cmp` identical), and the build and the full suite above were re-run on it.

- **Final tree.** The `muellerAt` doc and one test comment were edited after the first full run. The solution was
  rebuilt (`0250-build-final.log`: 0 warnings, 0 errors), and the gate's test command re-ran: 1397 / 1397
  `Passed!`, 1 m 07 s, with the TRX re-captured. Elevation also re-ran (above).

No test was removed or renamed, and no assertion was weakened.

## Artifacts

- `specs/0050/.artifacts/0250-build.log`, `0250-build-final.log`
- `specs/0050/.artifacts/0250-item-evaluation-tests.log`
- `specs/0050/.artifacts/0250-constructor-unit-tests.log`, `0250-constructor-unit-tests.trx`
- `specs/0050/.artifacts/0250-elevation.log`, `0250-elevation-final.log`
- `specs/0050/.artifacts/0250-invariants.log`
- `specs/0050/.artifacts/0250-red-mutation.log`, `0250-red-mutation-build.log`

## Gotchas

- **`MuellerInverse.SampleAzimuth.degree` does not compile when qualified.** The union case shadows the type in a
  module-qualified long identifier. Open `MuellerInverse` (as the inverse suites do), or wrap an `Angle` with the
  case.
- **Two `CutPlate`s are never `=`, even over one forward, parameters, cut and thickness.** Compare cut plates by what
  they predict.
- **1 mm of n_o 1.544 / n_e 1.553 at 600 nm is 15 whole waves at normal incidence.** A fact that turns the azimuth at
  normal incidence would see almost no change. Keep an oblique incidence when asserting azimuth dependence.
- **`specs/0050/.manifest.state.json` was already modified when the round began.** It is the supervisor's file, and
  this step did not touch it.
