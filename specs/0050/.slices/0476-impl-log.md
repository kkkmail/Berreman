# Step 0476 — impl log

## Progress

- Read the arc-runner and IMPLEMENT system prompts, the project prompt, AGENTS.md, the slice and the five operator
  rulings. R0004 (`.manual/035`) binds this step by name: "at 0476 the offset frame's move keeps one Jacobian and one
  report per problem". R0001–R0003 and R0005 are preserved untouched: no dispatch, analyzer, identity, `rhoT` or
  garbage-collector code is edited.
- (1) `FitQuality.fs`: the private `covarianceMatrix` became the public `covarianceOf`, body unchanged. Its doc keeps
  the `MathNetNumericsMath` `RealMatrix` seam line and cites S14.2. `reportFrom` calls it.
- (2) `FittedProblems.fs`: the private `FittedProblem.offsetFrame` tuple member is gone. Its rule now lives before
  `FittedProblem` in `type ScaledVector with`:
  - `offsetResidualOf` is the closure y ↦ residual (this + (y − 100)), the same `solved + (moved - 100.0)` per
    coordinate;
  - `offsetOrigin` is the vector of hundreds;
  - `offsetJacobianOf` is `FitQuality.residualJacobian (this.offsetResidualOf residual) this.offsetOrigin`, with
    step 397's line on why the frame is offset.
  - The 100 is a private `[<Literal>] frameOffset : float`. A plain private `let` would be ELEV003P (memory note).
  - `jacobian` and `report` keep their `FittedProblemEvaluations` `ConditionalWeakTable` caches (R0004: one of each
    per problem). Inside the cache callbacks they call `problem.search.start.offsetJacobianOf problem.residual` and
    `FitQuality.reportFrom (start.offsetResidualOf problem.residual) start.offsetOrigin (problem.residual start)`.
    The same doubles flow.
- (3) `BenchNoise.fs`: `type MeasurementConfiguration with member misalignedBy (incidence : Angle) (azimuth : Angle)`
  is declared directly before `disturb`. It holds the record update `disturb` had: the incidence through
  `IncidenceAngle + Angle`, the azimuth through `SampleAzimuth`. Its doc keeps the one line on why the operator, not
  `IncidenceAngle.create`, carries a signed excursion. `disturb` is now `c.misalignedBy (wobble ()) (wobble ())`.
  F# evaluates curried arguments left to right, so the incidence is still drawn first, as the old record-field order
  drew it.
- (4) `StudyEngine.fs`: `type ParameterInterval with member searchUnit : ParameterValue`, declared directly after
  `studySearchBox`, holds (upper − lower) / (2 · `studySearchBox.halfWidth`). `scaledProblemWith` reads
  `unknown.bounds.searchUnit.value`.
- Tests:
  - `FitResultsTests` gains `the offset frame differentiates a residual about any point, zero included, and
    covarianceOf scales the inverse normal matrix`. It checks A about [0; 0] and [5; −3] within 1e-6, and 0.5 · (AᵀA)⁻¹
    = 0.5 · [5 −2; −2 10] / 46 within 1e-12. It adds `open OpticalConstructor.Optimization`, as
    `OptimizationInterfaceTests.fs` does, to reach `FitQuality`.
  - `StudyEngineTests` gains `an interval's search unit is its span over twice the search box's half-width`, placed
    after the centred-search fact in the S11.3 scaled-search block.
- No consumer needed a new `open`: every caller reaches the moved rules through the module it already opened.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Optimization/FitQuality.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Optimization/FittedProblems.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Optimization/BenchNoise.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Optimization/StudyEngine.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/FitResultsTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/StudyEngineTests.fs`

All six are inside the authorized effects. Line endings are LF (0 CR bytes in each), with no `--ignore-cr-at-eol`
difference.

## Testing state

Development runs only; the arc-runner's gate engine is the authority (Invariant 6).

- **build:** exit 0 in 83 s (deadline 600 s).
  - The only non-NuGet warnings are the vendored-MathNet `SYSLIB0051` ones, which were already there.
  - No FS or MSB warning.
- **constructor-unit-tests** (whole suite, as the gate's `all_discovered` selection runs it): `Passed!`, 1866 of 1866
  in 113 s (deadline 180 s).
  - Both new identities are `Passed` in the TRX.
  - The count rises by 2 over checkpoint 471's 1864.
- **21 × invariant-s46-\*:** every decoded `git grep` finds nothing.
- **elevation** (`Run-ElevationGate.ps1 -ExcludeRules ELEV004`, no base, so all 13 projects):
  `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)` in 283 s (deadline 600 s).
  - The suppressed count is 1767, the same as step 0475, and the known findings are 0.
- **unit-tests, selected `BerremanTests.MuellerInverseTests`** (the gate's own command and filter): `Passed!`, 20 of 20
  in 408 s (deadline 7200 s).
  - The count is unchanged at 20.
  - The run overlapped elevation and kit-acceptance.
- **kit-acceptance, selected `OpticalConstructor.Ui.Tests.KitAcceptanceTests`** (the gate's own command, trait and
  filter): `Passed!`, 14 succeeded with 1 skipped (total 15) in 1453 s (deadline 3600 s).
  - The skipped fact is AT-2's Explicit five-seed ensemble; the gate's skip policy is `allow`.
  - The count is unchanged at 14.
  - Both `... equals the committed kit file by file` facts passed, so the fit, noise and study numbers are
    bit-identical.
  - AT-2's grid-generator fact, which includes the fit through the shell, took 13 m 34 s (814 s), inside R0005's
    1800 s bound.
  - The committed kit's read-only reopen fact took 9 m 23 s.
  - `git status` shows nothing under `data/`.

Every run finished inside its effective deadline.

## Artifacts

- `specs/0050/.artifacts/0476-dev-build.log`
- `specs/0050/.artifacts/0476-dev-constructor-unit-tests.log`
- `specs/0050/.artifacts/0476-dev-elevation.log`
- `specs/0050/.artifacts/0476-dev-unit-tests.log`
- `specs/0050/.artifacts/0476-dev-kit-acceptance.log`

## Gotchas

- The slice cites `InverseFitHarness.fs:disturb`, but `disturb` lives in `OpticalConstructor.Optimization/BenchNoise.fs`
  (step 339 moved it there). The move was made in `BenchNoise.fs`, the file the effects authorize.
- `MuellerInverse.fs` (Domain) keeps its own `(JᵀJ)⁻¹` in the `ResidualJacobian` diagnostics. That is a lower-assembly
  rule outside this step's `touches`, and the slice names only `covarianceMatrix`, so it was left unchanged.
- The `jacobian` / `report` caches key on the problem's identity. The moved members are pure, so the cache, not the
  members, is what keeps R0004's single evaluation.
