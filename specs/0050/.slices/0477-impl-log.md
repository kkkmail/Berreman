# Step 0477 — impl log

## Progress

- Read the arc-runner and IMPLEMENT system prompts, the project prompt, AGENTS.md and the slice.
- Read the five operator rulings R0001–R0005 (`.manual/030`, `031`, `033`, `035`, `036`). None names this step. Each
  binds only "preserve":
  - the dispatch rule;
  - the analyzer rule;
  - the element-identity provider;
  - the single Jacobian and report per problem;
  - the concurrent residual;
  - the single `rhoT` read;
  - the server collector.

  This step edits none of that code.
- (1) Domain: `PlannerError` gained `NoiselessPlannerCandidateErr` and `PlannerShapeErr` with their `describe` arms.
  `NoiseVariance` and `CandidateRows` follow `PlannerInputs`' step-472 members.
- (2) `PlannerInformation.fs` was written in the slice's order (a)–(g) and appended last to the Optimization
  compile list.
- (3) `PlannerInformationTests.fs` holds the four facts and is appended last to the test project's compile list.
- (4) Consequential: the exhaustive `PlannerError` or-patterns in `PlannerInputsTests.fs` (`unresolvedReasonOf`) and
  `PlannerReportTests.fs` gained the two new cases (FS0025 is an error). This follows steps 470 and 471.
- Build 1 failed with FS0583/FS0010. The or-patterns had been wrapped across lines inside `Error (…)`, which does
  not parse. Each was put back on one line, as the original was. Each diff is now one changed line that adds the two
  cases.
- Build 2 is green.
- `PlannerInformationTests` alone: 4 of 4. The whole constructor suite: 1870 of 1870.
- Elevation, unit-tests (two selected classes) and kit-acceptance were started from the gates' own commands.
- Elevation 1 failed with three new findings, all ELEV010 (NoPrimitiveBesideElevated):
  - `SingularSpectrum.ofRows (rows : float[][]) (coordinateCount : int) : SingularSpectrum`;
  - `SingularDirection.text (coordinates : ParameterName list) : string`;
  - the private helper `directionsBetween (first : int) (last : int)` (ELEV010P).

  ELEV010 checks members, and its doc states no `[<ElevationBoundary>]` exempts it. The slice's literal signatures
  cannot pass, and AGENTS.md rules that the analyzers win over any document. The deviations, each the analyzer's own
  remedy ("the primitives must be elevated too"):
  - `ofRows (jacobian : ResidualJacobian) (coordinates : ParameterName list)`. The rows are the information's own
    `ResidualJacobian`, and the coordinate count is that list's length. The MathNet boundary mark stays, because
    the member still calls MathNet's SVD (AGENTS.md: such a call is marked on its own declaration).
  - `text` returns a new single-case `CombinationText of string` with `.value`. The rendering rule is unchanged.
  - The private helper became the parameterless private property `directions`. `confounded` is
    `List.skip rank` of it, and `weakestOpt` is `List.tryItem (rank − 1)`.
- The two long runs were stopped and restarted on the final build, because they had been started on the pre-fix
  binaries and locked their output DLLs during the rebuild (MSB3021).
- Build 3 is green. The DLL copies in the four bin folders are byte-identical (md5).
- The whole constructor suite, rerun: 1870 of 1870.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/MeasurementPlanner.fs`: two `PlannerError` cases with their
  `describe` arms, `NoiseVariance` and `CandidateRows` (+22 / −2).
- `Berreman/OpticalConstructor/OpticalConstructor.Optimization/PlannerInformation.fs` (new).
- `Berreman/OpticalConstructor/OpticalConstructor.Optimization/OpticalConstructor.Optimization.fsproj`: one compile
  line, last.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/PlannerInformationTests.fs` (new).
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj`: one compile line, last.
- Consequential, outside `effects.edits` but inside the `touches` project `OpticalConstructor.Tests`:
  - `PlannerInputsTests.fs`: one line, with two cases added to an or-pattern;
  - `PlannerReportTests.fs`: one line, likewise.

Line endings are LF: 0 CR bytes in all seven files. `git diff --numstat` is the same with and without
`--ignore-cr-at-eol`.

## Testing state

Development runs only, all on the final build; the arc-runner's gate engine is the authority (Invariant 6).

- **build:** exit 0. The final incremental build finished in 4 s, after a full compile in 76 s.
  - The only warnings are the exempt `NU1902` advisories.
  - There is no FS or MSB warning.
- **elevation** (`Run-ElevationGate.ps1 -ExcludeRules ELEV004`, no base, so all 13 projects):
  `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)` in 230 s (deadline 600 s).
  - The known findings are 0, and the suppressed count is 1770.
  - The first run (254 s) failed on the three ELEV010 findings described above.
- **constructor-unit-tests** (whole suite, the gate's `all_discovered`): `Passed!`, 1870 of 1870 in 120 s
  (deadline 180 s).
  - The count rises by 4 over checkpoint 1866.
  - All four new identities are `Passed` in the TRX, named exactly as the census names them.
- **unit-tests**, selected `BerremanTests.MuellerInverseTests` and `BerremanTests.BiaxialInverseTests` (the gate's own
  command and filter): `Passed!`, 31 of 31 (20 + 11) in 1025 s (deadline 7200 s).
- **kit-acceptance**, selected `OpticalConstructor.Ui.Tests.KitAcceptanceTests`: `Passed!`, 14 succeeded with 1
  skipped (total 15) in 1420 s (deadline 3600 s).
  - The skipped fact is AT-2's Explicit five-seed ensemble; the gate's skip policy is `allow`.
  - Both `... equals the committed kit file by file` facts passed.
  - AT-2's grid-generator fact, which includes the fit through the shell, took 793 s, inside R0005's 1800 s bound.
  - `git status` shows nothing under `data/`.
- **21 × invariant-s46-\*:** every decoded `git grep` finds nothing.

Every run finished inside its effective deadline.

## Artifacts

- `specs/0050/.artifacts/0477-dev-build.log`
- `specs/0050/.artifacts/0477-dev-elevation.log`
- `specs/0050/.artifacts/0477-dev-constructor-unit-tests.log`
- `specs/0050/.artifacts/0477-dev-unit-tests.log`
- `specs/0050/.artifacts/0477-dev-kit-acceptance.log`

## Gotchas

- **ELEV010 is not silenced by a boundary mark, and it checks members.** The slice's `ofRows (float[][]) (int)` and
  `text … : string` signatures fail it, though `ofRows` carries the boundary the slice asked for. The deviations
  above are the analyzer's own remedy.
  - `CombinationText` is a public type the contracts do not list. It is the minimum elevation of `text`'s result.
  - `directions` is private.
- **`ReducedChiSquared this.noise.value` would be ELEV001.** The receiver is `this.noise`, not `this`. A
  `NoiseVariance.toReducedChiSquared` member beside `ReducedChiSquared.noiseVariance` would bridge a bijection.
  `scaledSigmasOpt` therefore destructures its own noise (`let (NoiseVariance noise) = this.noise`) and passes
  `FitQuality.ReducedChiSquared noise`, as `FitQuality.covarianceOf` destructures its arguments.
- **Two consequential test edits.** `PlannerInputsTests.fs` and `PlannerReportTests.fs` lie outside `effects.edits`.
  - Each matched `PlannerError` exhaustively, and FS0025 is an error under `--warnaserror+:25`.
  - Each or-pattern gained the two cases on its existing line. No assertion or identity changed. Steps 470 and 471 set
    this precedent.
  - A multi-line or-pattern inside `Error (…)` does not parse (FS0583).
- **Two totalizations not in the slice.** Neither changes any slice-specified case.
  - `boundsOver` answers `None` when the scaling's axes are not one per coordinate, rather than throwing in `List.zip`.
  - `conditionOpt` answers `None` with no coordinate, so MathNet is never handed a column-less matrix. `ofRows` takes
    its identity branch for no coordinate for the same reason.
- **Do not start long test runs before elevation is clean.** A rebuild while they run fails on locked DLLs (MSB3021).
