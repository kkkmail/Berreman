# Step 0249 — impl log (IMPLEMENT: stack samples go through Berreman)

## Progress

- [x] `ItemEvaluation.fs`:
  - `ParameterValue.thickness`, `Sample.withThicknessValues` and `SampleMaterialErr`;
  - `BoundStackSample` / `ofStackSample`;
  - the stack-sample arm of `muellerAt`.
- [x] `ItemEvaluationTests.fs`: three new facts and the widened render fact.
- [x] A measured failure, `a form is bound only with a value for each of its parameters, in range` going red, was
  found and repaired (Decisions, deviation 2).
- [x] Red measured by a temporary mutation, then reverted byte for byte (see Testing state).
- [x] Development runs over the final tree: build, constructor tests (all discovered), elevation and the invariant
  greps.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/ItemEvaluation.fs`
  - New opens: `Berreman.Media` (`Thickness`), `Berreman.Constants` (`meter`) and
    `OpticalConstructor.Domain.MaterialLibrary` (`MaterialError`, `MaterialProxy`).
    `MaterialLibrary` is opened before `Library`, as `Propagation.fs` orders them.
  - `ItemEvaluationError.SampleMaterialErr of error : MaterialError`. Its `describe` renders
    `the sample's materials do not resolve: <reason>`, the reason read from whichever `MaterialError` case it is.
  - `let private sequenced` is the Result-list traversal. It moved from a local inside `muellerAt` (step 248) to
    module level, after `ItemEvaluationError`. `muellerAt` and `withThicknessValues` share one body.
  - `ParameterValue.thickness : Thickness`, which reads the number as metres: `Thickness (metres * 1.0<meter>)`.
  - `Sample.withThicknessValues (values : ParameterValues) : Result<Sample, ItemEvaluationError>`:
    - first, `values.checkedFor this.parameterAxes`;
    - then each finite layer is read under `filmThickness k`, `repeatedLayerThickness k j` (the cell is stored once)
      or `substrateThickness`;
    - a layer whose `thickness.parameterValueOpt` is `None` is kept;
    - every other field is kept, by record copy.
  - `[<ReferenceEquality>] type ResolvedStackSample = { resolved : Propagation.ResolvedSample }` (deviation 2).
  - `BoundOpticalElement.BoundStackSample of ResolvedStackSample`.
  - `static member BoundOpticalElement.ofStackSample (materials : MaterialProxy) (values : ParameterValues) (sample :
    Sample)` is `withThicknessValues`, then `Propagation.resolveSampleMaterials`. The error is mapped by
    `Result.mapError SampleMaterialErr`.
  - In `muellerAt`, `BoundStackSample stack` on `BranchTransmitted | BranchReflected` (an or-pattern, no wildcard)
    answers `Ok (Propagation.sampleMuellerSpun orientation.branch stack.resolved w orientation.incidence rotation)`.
  - Docs: `ItemEvaluationError`, `BoundOpticalElement` and `muellerAt` are widened to name the stack sample.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/ItemEvaluationTests.fs`
  - New opens: `Berreman.Media`, `OpticalConstructor.Domain.MaterialLibrary` and `OpticalConstructor.Domain.MaterialStore`.
  - New private helpers: `stackSampleMuellerAt`, `resolvedAsTheTable` and `assertSample`.
  - New facts:
    - `a stack sample at its own thicknesses is solved as the table solves it`
    - `a stack sample's thickness parameters change the layers they name`
    - `a sample whose material does not resolve answers SampleMaterialErr`
  - `every evaluation error renders its condition and its payload` gains two `SampleMaterialErr` rows, so the
    distinct count goes from 4 to 6. No existing row or assertion is removed.

## Decisions

- **Deviation 1: the empty store is `MaterialProxy.createInMemory (fun _ -> [])`, not `createInMemoryEmpty`.**
  - The brief names `MaterialStore.fs:MaterialProxy.createInMemoryEmpty`, which does not exist.
  - The store's empty in-memory constructor is `createInMemory`, documented "Empty material store".
  - `MaterialStore.fs` is outside this step's authorized edits, so nothing was added there. The fact's substance
    (an empty in-memory store, `SampleMaterialErr` of an `UnknownMaterialIdErr`) is as declared.
- **Deviation 2: `BoundStackSample` holds a `[<ReferenceEquality>]` wrapper, `ResolvedStackSample`, not a bare
  `Propagation.ResolvedSample`.**
  - With the bare payload, the step-248 fact `a form is bound only with a value for each of its parameters, in
    range` went red (measured):
    `Assert.Equal() Failure: Values differ` at `assertBound (Ok (BoundOpticalForm …)) (ofOpticalForm …)`.
  - The cause: `ResolvedSample` holds `EpsWithDisp` / `MuWithDisp` / `RhoWithDisp`, whose dispersive cases are
    functions.
    - F# then drops the generated structural `Equals` from every union holding it, so `BoundOpticalElement` fell back
      to reference equality.
    - A `.fsx` probe confirmed it: a union with a function-bearing case answers `false` for two equal values of its
      other case.
  - `[<ReferenceEquality>]` on the wrapper gives it equality (each value is equal only to itself), so the union keeps
    its generated value equality.
    - This is the same pattern step 250's brief prescribes for its function-bearing `CutPlate`
      (`[<ReferenceEquality>] CutPlate = { …; predict : … }`, then `BoundCutPlate of CutPlate`).
    - Step 251 needs it as well: "the `PresetIds.polarizerLp` polarizer binds to its `BoundPolarizerPreset`".
  - Every declared contract symbol exists as declared: `BoundOpticalElement.BoundStackSample`, `ofStackSample`,
    `Sample.withThicknessValues`, `ParameterValue.thickness` and `ItemEvaluationError.SampleMaterialErr`.
  - No later slice's text pattern-matches `BoundStackSample` (grep of 0250–0386), so no brief depends on the bare
    payload.
  - Rejected alternatives:
    - `[<CustomEquality>]` with hand-written `Equals` / `GetHashCode` on the union is more code, and a hazard for
      every later case.
    - Editing the step-248 assertion lies outside this step's test effects.
    - Making `ResolvedSample` equatable would mean editing `Propagation.fs` / `Dispersion.fs`, which are not
      authorized.
- **`withThicknessValues` decides "finite" by `layer.thickness.parameterValueOpt`,** the very rule `parameterAxes`
  publishes by. A layer is read exactly when it has an axis, and the finite/infinite decision is not copied.
- **The substrate is read after the films,** in `parameterAxes` order. `checkedFor` has already guaranteed every
  value, so `valueOf` can fail only for a hand-built `ParameterValues` it never sees.
- **The material error's reason is extracted by an or-pattern over all six `MaterialError` cases.**
  - `MaterialError` has no reason member.
  - `MaterialLibrary.fs` is not authorized for edits.
  - The match has no wildcard, so a seventh case is a compile error here.
- **Assertions beyond the brief's minimum, inside the declared facts** (no new identity):
  - Fact 1: `withThicknessValues` at the catalogue sample's own defaults is `Ok sample`, unchanged.
  - Fact 2:
    - the whole `Sample` is compared, so material, orientation, the `lower`, the name and the emission are all
      proven kept;
    - each layer of the test-local structure has its own material (glass175, glass152, glass200, glass150);
    - m00 at 300 nm must differ from m00 at the 200 nm default by more than 1e-6, which proves the bound thickness
      reached the solve.
  - Render fact: a second `SampleMaterialErr` row (`MaterialStoreUnavailableErr`) shows that any case's reason is
    the payload.

## Testing state

These are development runs over the final tree. Invariant 6: the arc-runner's gate engine gives the verdict, not
these runs.

- **build:** `dotnet build Berreman.slnx -c Release -nologo -v:m` exit 0, `0 Warning(s)`, `0 Error(s)`
  (`.artifacts/0249-build.log`, and `0249-build-final.log` after the mutation revert).
  - A Domain-only build run during development recompiled the vendored MathNet C#, which raised its two
    pre-existing `SYSLIB0051` warnings.
  - The solution builds above report 0.
- **constructor-unit-tests,** the gate's own command (all discovered): `Passed!`, total 1395, succeeded 1395,
  failed 0, skipped 0, in 1 m 06 s against an estimate of 180 s (`.artifacts/0249-constructor-unit-tests.log`, TRX
  `.artifacts/0249-constructor-unit-tests.trx`).
  - 1395 is checkpoint 248's 1392 plus the three new facts.
  - The four declared identities are each `Passed` in the TRX.
- **elevation** with `ASG_ARC_BASE_COMMIT=f395597c…`, exit 0:
  `ELEV-GATE: OK - 0 new finding(s) in 12 project(s), 196s`. Known findings 0, suppressed 1350 (the excluded
  ELEV004), and the Domain project has 0 findings (`.artifacts/0249-elevation.log`).
- **The fourteen s25 invariant greps,** with `--untracked`: all 14 find nothing (`.artifacts/0249-invariants.log`).
- **Red, measured by mutation.** The new symbols did not exist before this step, so the facts could not compile
  against the prior tree. To show they test behaviour, `ItemEvaluation.fs` was mutated temporarily:
  - the stack arm always solved `BranchTransmitted`;
  - `withThicknessValues` kept the substrate unread.

  `--filter-class OpticalConstructor.Tests.ItemEvaluationTests` was then `Failed!`, 2 of 17
  (`.artifacts/0249-red-mutation.log`): `a stack sample at its own thicknesses is solved as the table solves it` and
  `a stack sample's thickness parameters change the layers they name`.

  The file was restored from a copy (`cmp` identical), and the build and the full suite above were re-run on it.

No test was removed or renamed, and no assertion was weakened.

## Artifacts

- `specs/0050/.artifacts/0249-build.log`, `0249-build-final.log`
- `specs/0050/.artifacts/0249-constructor-unit-tests.log`, `0249-constructor-unit-tests.trx`
- `specs/0050/.artifacts/0249-elevation.log`
- `specs/0050/.artifacts/0249-invariants.log`
- `specs/0050/.artifacts/0249-red-mutation.log`

## Gotchas

- **A union case holding anything with a function inside silently costs the whole union its value equality.** F#
  falls back to reference `Equals`, and it is no compile error unless `=` is used. Wrap such a payload in a
  `[<ReferenceEquality>]` record, as `ResolvedStackSample` and step 250's `CutPlate` do.
- **Two `ofStackSample` results are never equal,** even over the same sample, because each resolves afresh.
  Compare stack samples by what they evaluate to, as step 251's brief does.
- **`ParameterValue.thickness` is exact.** `Thickness (metres * 1.0<meter>)` round-trips `parameterValueOpt`
  bit for bit, so a sample at its own defaults is `=` to itself. Fact 1 compares its matrices exactly for that
  reason.
- **The render text uses the material error's raw reason,** e.g. `unknown material version '<guid>' v1`.
- **`specs/0050/.manifest.state.json` was already modified when the round began.** It is the supervisor's file, and
  this step did not touch it.
