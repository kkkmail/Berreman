# Step 0341 — impl log

Obligation S101-ENSEMBLE-MOVE. This step moves the ensemble statistics from `BerremanTests/InverseFitHarness.fs`
into `OpticalConstructor.Optimization.RecoveryEnsembles`. No operator note and no operator ruling were in flight
for this step.

## Progress

- [x] Created `RecoveryEnsembles.fs` and added it to the fsproj directly after `BenchNoise.fs`.
  - One `<Compile>` line, with no comment.
  - The module is `module RecoveryEnsembles` in `namespace OpticalConstructor.Optimization`. Its doc runs two lines
    and cites spec 0050 S10.1 and specs/0044/.manual/011-noisy-measurement-tests.md.
  - It declares `RecoveredQuantity<'Parameters>`, `RecoveryStatistics<'Parameters>`, `NoisyExperiment<'Parameters>`,
    `EnsembleSize`, `statisticsFor`, `standardErrorOfMean` and `rowFor`.
- [x] `MuellerFitError` gained `UnknownQuantityErr of ItemModels.ParameterName`.
- [x] Deleted the three types and the three functions from `InverseFitHarness.fs` (117 lines).
- [x] Updated the test callers. Only the changes the step names were made:
  - **`BiaxialSample.recoveredQuantities`, `MuellerInverseTests.recoveredQuantities` and
    `AbsorbingBiaxialNoisyTests.reportedQuantities`** now build each quantity as
    `RecoveredQuantity.derived (ParameterName "<name>") (fun p -> QuantityValue (<the same expression>))`.
  - **`rowFor table <name>`** became `(rowFor table (ParameterName <name>) |> expectOk)`.
  - **`rowFor table name.value`** became `(rowFor table name |> expectOk)`.
  - **Absolute scatters** now read `.absoluteScatter.value` wherever they are compared as numbers.
  - **`standardErrorOfMean`** now takes `EnsembleSize (List.length <seeds>)`.
  - **Opens.** Every consumer opens `OpticalConstructor.Optimization.RecoveryEnsembles`. `BiaxialSample` and
    `AbsorbingBiaxialNoisyTests` also open `OpticalConstructor.Domain.ItemModels`, which they need for
    `ParameterName`, and `BiaxialSample` also opens `OpticalConstructor.Optimization.MuellerFitting` for
    `QuantityValue`. The compiler asked for each of these.
- [x] Ran build, elevation, the constructor suite, `MuellerInverseTests` and the 19 invariant greps as development
  runs (see Testing state).

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Optimization/OpticalConstructor.Optimization.fsproj`: one
  `<Compile>` line.
- `Berreman/OpticalConstructor/OpticalConstructor.Optimization/RecoveryEnsembles.fs`: new.
- `Berreman/OpticalConstructor/OpticalConstructor.Optimization/MuellerFitting.fs`: added `UnknownQuantityErr`.
- `Berreman/BerremanTests/InverseFitHarness.fs`: removed the moved declarations.
- `Berreman/BerremanTests/BiaxialSample.fs`
- `Berreman/BerremanTests/MuellerInverseTests.fs`
- `Berreman/BerremanTests/BiaxialInverseTests.fs`
- `Berreman/BerremanTests/AbsorbingBiaxialNoisyTests.fs`

## Decisions

- **`statisticsFor` performs the same operations on the same doubles, in the same order.**
  - `trueValue` and each recovered value are now `QuantityValue`s wrapping exactly the double the old `valueIn`
    returned.
  - The mean is `List.average` over the unwrapped values, born as a `QuantityValue`.
  - The variance is the same `(v - mean) ** 2.0` sum divided by `count - 1`.
  - `absoluteScatter` is `sqrt variance`, born as a `QuantityValue`.
  - `scatter` is `absoluteScatter.value / abs trueValue.value`, the same double as the old
    `sqrt variance / abs trueValue`.
  - `bias` and `worst` call `RelativeError.between` exactly as before.
- **`rowFor` compares `ParameterName`s structurally** (`row.quantity.name = name`). `ParameterName` is a single-case
  union over `string` with default equality, so this match is the same as the old `.value = name` string compare.
- **The doc comments are cut to their contracts.**
  - The `quantity` field's doc ("Which quantity this row is about.") only repeated the identifier, so it was deleted
    (AGENTS.md: delete, do not shorten).
  - The hazard lines kept are:
    - `absoluteScatter`: compare an index with a difference of indices only in absolute units;
    - `rowFor`: names are the identity because the record holds a function;
    - `statisticsFor`: the scatter is Bessel-corrected.

## Testing state

Each gate's own command was run as a development run over the final tree. Logs are under
`specs/0050/.artifacts/0341-dev-*.log`.

- **build:** exit 0 in 80 s.
  - The first build compiled every changed project. Its two warnings are SYSLIB0051 in vendored
    `MathNetNumerics/Numerics/Exceptions.cs`, the same pair steps 0337–0340 recorded.
  - The final incremental build reports 0 warnings and 0 errors. Our code has no warnings.
- **elevation (no base, 12 of 12 projects):** `ELEV-GATE: OK - 0 new finding(s)`, known findings 0, in 213 s. The
  Optimization project has `findings=0`.
- **constructor-unit-tests (whole):** Passed!, 1553 of 1553, 0 failed, 0 skipped, in 72 s. The count is unchanged
  from step 340.
- **unit-tests (`BerremanTests.MuellerInverseTests`):** Passed!, 20 of 20, 0 failed, 0 skipped, in 694 s against
  the 7200 s deadline.
  - The change-expectation fact `an ensemble of noisy experiments recovers every constant, and its scatter IS what
    one fit's own covariance predicts` passed in 9 min 37 s (TRX outcome `Passed`). Its bands are unchanged.
  - The count, 20, is the same as step 339's run of the class.
- **Not run:** `BiaxialInverseTests` and `AbsorbingBiaxialNoisyTests`. Each takes about 1000 s and neither is
  selected for this step. Both compile.
- **Effects:** no test was added, removed or renamed.
- **19 × invariant-s33-\*:** every decoded `git grep` found nothing (`0341-dev-invariants.log`).

## Artifacts

- `specs/0050/.artifacts/0341-dev-build.log`
- `specs/0050/.artifacts/0341-dev-elevation.log`
- `specs/0050/.artifacts/0341-dev-constructor-unit-tests.log`
- `specs/0050/.artifacts/0341-dev-unit-tests.log`
- `specs/0050/.artifacts/0341-dev-invariants.log`

## Gotchas

- **The step text writes `standardErrorOfMean`'s result as `... |> RelativeError`.** The union's case constructor is
  `RelativeErr` (`MuellerFitting.fs`), so the code pipes into `RelativeErr`. The double is the same.
- **`rowFor` still has other declarations in the tree, and none is a duplicate of the moved function:**
  - `OpticalConstructor.Tests/CatalogueImportInMemoryTests.fs:1221` and
    `OpticalConstructor.Ui.Tests/LibraryControlsTests.fs:311` each declare a local `let rowFor` inside a test body.
  - Neither is a module-level declaration, both are test projects outside ELEV's scope, and both predate this
    step.
  - The moved name is declared once, in `RecoveryEnsembles.fs`.
