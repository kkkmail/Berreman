# Step 0381 — impl log

## Progress

- [x] Production: `ScaledSearch`, `ScaledSearch.solveWith`, `SearchBox.centredSearch`, and `fitWithResidual` rewired
  (MuellerFitting.fs) (2026-11-13)
- [x] Tests: `StudyEngineTests` with its two facts, appended to the compile list (2026-11-13)
- [x] First elevation run: one ELEV010 on `centredSearch (dimension : int)`. Fixed by elevating the parameter (see
  Decisions) (2026-11-13)
- [x] Development runs: build, constructor-unit-tests, elevation, the twenty s38 invariants, and unit-tests over
  `AbsorbingBiaxialInverseTests` and `FeasibleBiaxialInverseTests` (2026-11-13)

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Optimization/MuellerFitting.fs`
  - opens `OpticalConstructor.Optimization.MuellerInverseSolver`, for the step-380 extension
    `IterationWatch.levenbergMarquardtSolver`;
  - after `SearchBox`: `ScaledSearch = { start; lowerBounds; upperBounds }`, all `ScaledVector`, with the intrinsic
    member `solveWith (watch : OptimizationInterface.IterationWatch) (residual : ScaledVector -> ResidualVector) :
    Result<NonlinearSolution, NonlinearSolverError>`. It builds the one `NonlinearRequest` (`IterationCount 400`,
    `StepTolerance 1.0e-12`) and solves it through `watch.levenbergMarquardtSolver`;
  - `type SearchBox with member this.centredSearch (scaling : ParameterScaling<'Parameters>) : ScaledSearch`: the zero
    start and ∓`halfWidth` bounds, in `scaling.dimension` coordinates;
  - `fitWithResidual` is now `match (box.centredSearch scaling).solveWith OptimizationInterface.UnwatchedIterations
    residual with`. Its `Ok` / `Error` arms are unchanged: the same `MuellerFitData`, and `NonlinearFitErr e`.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/StudyEngineTests.fs` (new): `module StudyEngineTests` in
  `namespace OpticalConstructor.Tests`, with the two facts the slice names.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj`: `StudyEngineTests.fs` is
  appended last in the compile list, with no comment.

## Decisions

- **`centredSearch` takes the `ParameterScaling`, not an `int` dimension (deviation from the slice's how_to).**
  - **What happened.** The slice spelled `member this.centredSearch (dimension : int) : ScaledSearch`. The first
    elevation run failed on it: `FAILED: ELEV010 …MuellerFitting.fs:157:21 - centredSearch mixes bare primitive(s) int
    with elevated types in one signature`.
  - **Why the member exemption does not apply.** ELEV010 checks members too, except the elevation-pattern members
    (`Softellect/Analyzers/NoPrimitiveBesideElevated.fs`). `int` is not `SearchBox`'s own primitive, which is
    `double`.
  - **Authority.** AGENTS.md: "Where any document, STL's included, disagrees with the analyzers, the analyzers win"
    (operator, spec 0048 `.manual/002` R6). The finding cannot be listed or silenced.
  - **Why `ParameterScaling` and not a new count type.** No elevated dimension type exists. A new one would need
    `ParameterScaling.dimension` (Domain, outside this step's touches) to return it. Otherwise `fitWithResidual` would
    re-wrap the `int` at the call site (ELEV001's "elevate at origin"), and the slice adds no other type or member.
    The scaling is the value `fitWithResidual` already holds, and `centredSearch` reads its `dimension`.
  - **What is unchanged.** The member's name, receiver, return type and behaviour, the contract symbol
    `MuellerFitting.SearchBox.centredSearch`, and both test identities. No later slice (0382–0397) calls
    `centredSearch`; they build `ScaledSearch` directly.
- **The centred-search fact builds its three coordinates from the Domain's own axes.** It uses
  `scalingAround (List.take 3 UniaxialParameters.axes) p p`, with `p = CrystalMaterialFixtures.uniaxialParameters`,
  the suite's shared crystal fixture. The slice's literal `(SearchBox 50.0).centredSearch 3` reads
  `(SearchBox 50.0).centredSearch threeAxes`. The asserted start [0; 0; 0], lower [−50; −50; −50] and upper
  [50; 50; 50] are as the slice wrote them.
- **`solveWith` is an intrinsic member written inside the record**, not a separate `type ScaledSearch with` block. It
  is the same member, `ScaledSearch.solveWith`. `centredSearch` is an intrinsic augmentation (`type SearchBox with`)
  because `ScaledSearch` is declared after `SearchBox`, as the slice orders them.
- **`OptimizationInterface` stays qualified** (`OptimizationInterface.IterationWatch`, `.UnwatchedIterations`), as the
  file already writes `OptimizationInterface.DesignVector`. Opening it would put `OptimizationRequest`'s labels
  (`residual`, `initial`, …) beside `NonlinearRequest`'s. The request is also type-annotated (`let request :
  NonlinearRequest`), because `ScaledSearch` now shares `lowerBounds` / `upperBounds` with it.
- **The numbers are unchanged by construction.** The request is field-for-field the old one: zero start, ∓half-width
  bounds, 400 iterations, 1e-12. It goes through the same proxy, since step 380 made `createAlglibLevenbergMarquardt ()`
  equal `UnwatchedIterations.levenbergMarquardtSolver`. `createAlglibLevenbergMarquardt` stays, because
  `LangasiteInverseTests` and `MuellerInverseTests` call it.
- **The scaled-search fact also asserts the start and the box, not only the two answers.**
  - The answers 2 and 5 alone would not catch a `solveWith` that ignored `start`.
  - The fact therefore records every point the residual is evaluated at. It asserts that the first is the given start,
    and that every point lies within the search's bounds. ALGLIB's `minlmsetbc` documents that the bounded LM evaluates
    only inside the box or on it.
  - The slice's assertions (2 and 5, each within 1e-9, through `UnwatchedIterations`) are all present.
- **Tolerances** use xUnit's `Assert.Equal(double, double, tolerance)`, the constructor suite's idiom (step 0380's
  log). `BerremanTests/MatrixComparison.fs` is not referenced by `OpticalConstructor.Tests`.

## Testing state

Development runs only; the arc-runner's gate engine is the gate authority. The captures are under
`specs/0050/.artifacts/0381-dev-*.log`.

- **build** (`dotnet build Berreman.slnx -c Release -nologo -v:m`, from `Berreman/`): exit 0, `0 Error(s)`. The only
  warnings are the exempt NU190x advisories and vendored MathNet's `SYSLIB0051`. 78 s full, 44 s after the ELEV010 fix,
  and 29 s for the final incremental build (deadline 600 s).
- **constructor-unit-tests** (the gate's command, whole suite, final source): `Passed!`, 1652 succeeded, 0 failed,
  0 skipped (1650 → 1652). Both `StudyEngineTests` facts are `Passed` in the TRX. 75 s (deadline 180 s).
- **unit-tests** (`AbsorbingBiaxialInverseTests` + `FeasibleBiaxialInverseTests`, the gate's command): `Passed!`,
  12 of 12, 0 failed. Every one is `Passed` in the TRX. 299 s (deadline 7200 s). The first run, before the ELEV010 fix,
  was also 12 of 12 (289 s).
- **elevation** (`ASG_ARC_BASE_COMMIT=f395597c…`, ELEV004 excluded):
  - first run: `FAILED - 1 new finding(s)`, the ELEV010 above;
  - after the fix: `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`, 238 s;
  - again over the final source: OK, 232 s (deadline 600 s);
  - suppressed findings are 1539 (`OpticalConstructor.Optimization` 34), both as at step 0380.
- **The twenty s38 invariants**: each exits 0 with `INVARIANT HOLDS` (`0381-dev-invariants.log`).
- **Bit-identity probe**: five four-coordinate fits through the new `fitWithResidual` and HEAD `ea089620`'s
  transcribed request agree bit for bit in solution, final residuals and iterations (`0381-bit-identity.log`).

`commit_ready: true`. Every requirement of the slice landed, the `centredSearch` parameter excepted, which is
elevated per the analyzers-win rule above.

## Artifacts

- `specs/0050/.artifacts/0381-dev-build.log`
- `specs/0050/.artifacts/0381-dev-constructor-unit-tests.log`
- `specs/0050/.artifacts/0381-dev-unit-tests.log`
- `specs/0050/.artifacts/0381-dev-elevation.log`
- `specs/0050/.artifacts/0381-dev-invariants.log`
- `specs/0050/.artifacts/0381-bit-identity-probe.txt` (the `dotnet fsi` probe, renamed from `.fsx`) and
  `0381-bit-identity.log`

## Gotchas

- **ELEV010 reaches members.** A member speaks only its own type's primitive. `(dimension : int)` on a
  `double`-carrying `SearchBox`, beside a `ScaledSearch` return, is a finding.
- **The Write tool's output was already LF.** The new files were checked by counting `\r` bytes (`tr -cd '\r' | wc -c`
  → 0). `grep -c $'\r'` misreports in this shell.
