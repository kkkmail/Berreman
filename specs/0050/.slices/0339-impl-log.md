# Step 0339 — impl log

## Progress

- [x] Created `BenchNoise.fs` and added it to `OpticalConstructor.Optimization.fsproj`, directly after
  `MuellerFitting.fs`, as one `<Compile>` line with no comment.
- [x] Added `MuellerFitError.MuellerNormalizationErr of InversionError`.
- [x] Deleted the moved declarations and the measurement-error banner from the harness.
- [x] Updated the callers in `MuellerInverseTests`, `BiaxialInverseTests` and `AbsorbingBiaxialNoisyTests`.
- [x] Added the constructor fact `the promoted bench budgets equal the detector catalogue defaults`.
- [x] Ran build, elevation (fixed one ELEV010, then re-ran), constructor-unit-tests, unit-tests
  (`MuellerInverseTests`) and the invariants, all green.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Optimization/BenchNoise.fs` (new): `module BenchNoise` in
  `namespace OpticalConstructor.Optimization`. It declares, in order:
  - `AngleUncertainty`, `ElementUncertainty`, `ThroughputUncertainty` and `NoiseParam` (`decentLab`, `noiseless`),
    moved with their bodies and constants unchanged. Each budget adds `parameterValue`, and `NoiseParam` adds
    `parameterAxes`, which calls `ItemParameterAxis.detectorNoiseAxes`.
  - `NoiseSeed`.
  - `UniformDeviate` (new).
  - `UniformDeviateProvider`, with `nextDeviate : unit -> UniformDeviate`.
  - `disturb`.
  - `misread`, which now answers `Result<MuellerMatrix, MuellerFitError>`.
  - `noisyObserveWith`, which now answers `Result<MuellerObservation list, MuellerFitError>`.
- `OpticalConstructor.Optimization.fsproj`: added `<Compile Include="BenchNoise.fs" />` after `MuellerFitting.fs`.
- `MuellerFitting.fs`: `MuellerFitError` gains `MuellerNormalizationErr of InversionError`.
- `BerremanTests/InverseFitHarness.fs`:
  - Deleted the banner, `NoiseSeed`, the four data types, `UniformDeviateProvider`, `disturb`, `misread` and
    `noisyObserveWith`.
  - Opens `OpticalConstructor.Optimization.BenchNoise`.
  - `misreadAbsolute` reads `(draws.nextDeviate ()).value` at both draws.
  - `noisyObserveAbsoluteWith` now reaches the product `disturb` through the open, and `ensembleSeeds` the product
    `NoiseSeed`.
- `BerremanTests/MuellerInverseTests.fs`:
  - Opens `BenchNoise`.
  - `firstTen` reads `.value`; the direct `misread` pipes into `expectOk`; `noisyObserve` pipes into `expectOk`.
  - Three comments now name `BenchNoise` and specs/0044/.manual/011-noisy-measurement-tests.md: the class doc, the
    banner above `blindStart` and the banner above the noisy facts.
- `BerremanTests/BiaxialInverseTests.fs`:
  - Opens `BenchNoise`.
  - `recoverFromNoisy` pipes `noisyObserveWith` into `expectOk`.
  - The noisy-section banner and the ensemble fact's comment now name `BenchNoise` and 011.
- `BerremanTests/AbsorbingBiaxialNoisyTests.fs`: opens `BenchNoise`. This is the only change; the compiler needs it
  for `NoiseParam` and `NoiseSeed`.
- `OpticalConstructor.Tests/ItemModelsTests.fs`: opens `BenchNoise` and adds the new fact.

## Decisions

- **No `.value` round trip in the conversions.** `parameterValue` destructures the value by pattern (for example,
  `let (AngleUncertainty (Angle radians)) = this in ParameterValue radians`), as Domain's `ParameterValue.angle`
  does. This avoids an ELEV005 `(T x.value)` round trip.
- **Return types are annotated on the moved members.** They are `Angle`, `double` and `int`, per AGENTS.md's
  explicit public signatures. The bodies are unchanged.
- **`noisyObserveWith` is a `Result` fold.** It is the same shape as `MuellerFitting.observeWith`. The configurations
  are visited in order, and each one draws twice in `disturb` and then fifteen times in `misread`, so the seeded
  stream is bit-identical. Once an `Error` is held, later configurations are not evaluated and take no draws.
- **Shortened docs.** The long harness docs are cut to one to three lines each. The narrative and the sources live in
  specs/0044/.manual/011 and 019, which the module doc cites. Two hazard lines are kept:
  - `disturb`: `IncidenceAngle.create` folds a negative tilt.
  - `fromSeed`: the seeded `System.Random` keeps its legacy algorithm.
- **The new fact's tolerances.**
  - The rotation's 0.2° is checked through `degrees` within 1e-12. That is a radians-to-degrees conversion.
  - The element and throughput budgets are compared exactly. Those literals are stored as they are.
  - The shipped axes are compared to `NoiseParam.decentLab.parameterAxes` through the file's `assertAxesWithin`:
    names and kinds by `=`, defaults within 1e-15, because catalogue numbers are read back from text.
  - The fact asserts that there are three shipped detector models (intensity, Mueller polarimeter, ellipsometer), so
    the loop cannot pass vacuously.
  - It checks the declaring assembly of `AngleUncertainty`, `ElementUncertainty`, `ThroughputUncertainty` and
    `NoiseParam` through `typeof<_>.Assembly.GetName().Name`.

## Testing state

Logs are under `specs/0050/.artifacts/0339-dev-*.log`. Each log holds the final run.

- **build:** exit 0, 0 warnings and 0 errors, in 47 s.
  - The first build took 87 s. It recompiled the vendored MathNet and showed its two known SYSLIB0051 warnings,
    which step 0337 also recorded.
- **elevation, first run: FAILED** with one finding:
  `ELEV010 BenchNoise.fs:28:23 - degree mixes bare primitive(s) float with elevated types`. Members are not exempt
  from ELEV010.
  - Fix: `static member degree (d : float<deg>) : AngleUncertainty = Angle.degree d |> AngleUncertainty`, and the two
    callers in `NoiseParam` pass `0.2<deg>` and `0.0<deg>`.
  - This is a deviation from the step's "bodies unchanged", recorded here; the analyzers win (AGENTS.md; spec 0048
    `.manual/002` R6).
  - The value is bit-identical: `Angle.degree d` computes `float d * radiansPerDegree`, the same as
    `Angle.degree (d * 1.0<deg>)`.
- **elevation, re-run:** `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, in 233 s.
  - Optimization has 0 findings and 11 suppressed. The suppressed ones are ELEV004, which the gate command excludes.
- **constructor-unit-tests (whole):** `Passed!`, 1553 of 1553, in 87 s. That is 1552 plus the new fact, whose TRX
  outcome is `Passed`.
- **unit-tests (`BerremanTests.MuellerInverseTests`):** `Passed!`, 20 of 20, in 11 min 50 s, against a 7200 s
  deadline. Elevation ran concurrently for the first four minutes.
  - Both change-expectation facts are `Passed` in the TRX. The ensemble fact took 9 min 47 s.
- **invariants:** all 19 `git grep`s found nothing on the final tree.
- **LF:** `git diff --numstat` equals `--ignore-cr-at-eol`, and `BenchNoise.fs` has no CR.
- **Single declarations:** every moved name has exactly one declaration, in `BenchNoise.fs`. `InverseFitHarness.fs`
  declares none of them and has no measurement-error banner.

## Artifacts

- `specs/0050/.artifacts/0339-dev-build.log`
- `specs/0050/.artifacts/0339-dev-constructor-unit-tests.log`
- `specs/0050/.artifacts/0339-dev-invariants.log`
- `specs/0050/.artifacts/0339-dev-elevation.log`
- `specs/0050/.artifacts/0339-dev-unit-tests.log`

## Gotchas

- **The step text's line numbers are stale.** It cites the harness banner at `InverseFitHarness.fs:59-110`. At this
  step's start the banner was at lines 14–65 and `NoiseSeed` at 69. The deletion followed the content, not the
  numbers.
- **SYSLIB0051 in the vendored MathNet.** This build recompiled `MathNetNumerics/Numerics`, which this step does not
  touch, and so surfaced two SYSLIB0051 warnings from `Exceptions.cs`. Step 0338's incremental build skipped that
  project, so it showed 0 warnings. The warnings are pre-existing, vendored and outside this step's effects. The
  build gate's `stdout_match` is satisfied, because the log contains no `error`.
- **Deviation: `AngleUncertainty.degree` takes `float<deg>`.**
  - Why: the step says the four types move with their bodies unchanged, but the step's `degree (d : double)` beside
    our type is ELEV010, and the tree must stay ELEV-clean.
  - Effect: the computed angle is bit-identical. Step 340 must call it with a `float<deg>` literal.
