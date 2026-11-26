# Step 0480 — impl-log

## Progress

- Read the task, both system prompts, the project prompt, operator rulings R0001–R0005 and the slice.
  - The rulings bind steps 0427, 0428 and 0436 and ask later steps to preserve their behavior.
  - This step touches none of their files: `StudyEngine.fs`, `ItemEvaluation.fs`, `StudyForward.fs`,
    `TableAndElementRotationView.fs`, `FittedProblems.fs`, `BerremanMatrix.fs`, `Fields.fs` and the two `.fsproj`
    files are all unchanged.
- Mapped the moved bindings in the two classes (before the step):
  - `MuellerInverseTests.fs`:
    - lines 61–75: `solverParameters`, `forward`;
    - lines 85–111: `scalingAround`;
    - lines 172–219: `blindStart`, `wideBox`, `noisyObserve`, `recoverFromNoisy`.
  - `AbsorbingBiaxialNoisyTests.fs` lines 67–75: `forward`, `recoverFromNoisy`.
- `Berreman/BerremanTestSupport/QuartzSample.fs`. Appended after `fullConfigurations`:
  - `solverParameters`;
  - `forward`, annotated `ForwardModelProxy<UniaxialParameters>`;
  - `quartzScalingAround`, the class's `scalingAround` renamed;
  - `blindStart`;
  - `wideBox`, annotated `SearchBox`;
  - `noisyQuartzObservations`, answering `Result<MuellerObservation list, MuellerFitError>`;
  - `recoverQuartzFromNoisy`, answering `Result<NoisyExperiment<UniaxialParameters>, MuellerFitError>`:
    `noisyQuartzObservations … fullConfigurations |> Result.bind (fitObservations forward wideBox … (quartzScalingAround
    blindStart)) |> Result.map (fun fit -> { seed; fit })`.
  - Opens added where the compiler needs them: `Berreman.Solvers` (`SolverParameters`, `NumberOfReflections`),
    `OpticalConstructor.Optimization.BenchNoise` (`NoiseParam`, `NoiseSeed`, `noisyObserveWith`) and
    `OpticalConstructor.Optimization.RecoveryEnsembles` (`NoisyExperiment`).
  - Each doc was cut to one to three lines, cites spec 0050 S14.2, and keeps the reason. `solverParameters` keeps
    spec R4's physics reason in one sentence.
- `Berreman/BerremanTestSupport/AbsorbingBiaxialSample.fs`. Appended after `absorbingBandBreaches`:
  - `absorbingForward : ForwardModelProxy<AbsorbingTriclinicParameters> = createBerremanForward buildAbsorbing
    solverParameters`. `solverParameters` is `BiaxialSample.solverParameters` through the module's existing
    `open BerremanTestSupport.BiaxialSample`, the same binding the class resolved.
  - `recoverAbsorbingFromNoisy`, answering `Result<NoisyExperiment<AbsorbingTriclinicParameters>, MuellerFitError>`:
    `noisyObserveAbsoluteWith absorbingForward absorbing noise seed fullConfigurations |> Result.bind
    (fitAbsorbingObservations absorbingForward searchBox … (absorbingScalingAround perturbedStart)) |> Result.map
    (fun fit -> { seed; fit })`.
  - No open added.
- `MuellerInverseTests.fs`:
  - Deleted the seven bindings and their docs: 93 lines, saved to `0480-removed-lines.txt`.
  - `QuartzSample` is already opened last, so `forward` and `solverParameters` resolve to the moved values.
  - `scalingAround` → `quartzScalingAround` in the `fitObservations` wrapper and two facts (lines 693 and 772).
  - `noisyObserve …` → `noisyQuartzObservations … |> expectOk` (4 uses, one fact).
  - `recoverFromNoisy` → `fun seed -> recoverQuartzFromNoisy NoiseParam.decentLab seed |> expectOk` (the ensemble
    fact).
  - The `fitObservations` wrapper's doc said "Every fit in this file, noiseless or noisy, goes through it". The move
    made that false, so it now reads "goes through it or through `recoverQuartzFromNoisy`, which runs the same fit".
  - `fitObservations`, `runFit`, `observe`, `perturbedStart`, `nearBox` and `recoveredQuantities` stay, as the slice
    requires.
- `AbsorbingBiaxialNoisyTests.fs`:
  - deleted `forward` and `recoverFromNoisy`;
  - the ensemble fact maps `noisySeeds` through `fun seed -> recoverAbsorbingFromNoisy NoiseParam.decentLab seed |>
    expectOk`.
- **Numbers are bit-identical by construction:**
  - `fitWithResidual` (`MuellerFitting.fs:191`) builds `{ recovered = ofScaled scaling solution.solution; solution;
    scaling; residual }`. The class's `recoverFromNoisy` rebuilt exactly that record from the fields its wrapper
    unpacked, so passing the record through is the same value.
  - The absorbing runner already passed the record through.
  - The solver parameters, scale, start, box, seed, noise and configurations are byte-for-byte the class's.
  - `createBerremanForward` holds no state: it builds a new `OpticalSystemSolver` per call, so one module-level proxy
    in place of one per class instance changes nothing.
- Build: green in 84 s.
- Elevation: OK in 231 s.
- 21 invariants: nothing found.

## Files modified

- `Berreman/BerremanTestSupport/QuartzSample.fs` (+59 / −2: the seven names, three opens, and the module doc's one clause)
- `Berreman/BerremanTestSupport/AbsorbingBiaxialSample.fs` (+17 / −1: the two names and the module doc's one clause)
- `Berreman/BerremanTests/MuellerInverseTests.fs` (+12 / −103)
- `Berreman/BerremanTests/AbsorbingBiaxialNoisyTests.fs` (+1 / −11)

All four are the step's authorized edits. No project file changed, because both support files were already compiled.
`specs/0050/.manifest.state.json` was already modified when the session started; that is the supervisor's file and
was not touched.

## Testing state

These are development runs; the arc-runner's gate engine is the authority. No run exceeded its effective deadline.

| Gate | Outcome | Measured | Deadline |
|---|---|---|---|
| build | exit 0, 0 errors. The only warnings are the exempt `NU1902`s and the vendored MathNet's pre-existing `SYSLIB0051`s. No FS warning. | 84 s | 600 s |
| build (after the two module-doc edits) | exit 0, 0 errors, only `NU1902`. `BerremanTestSupport.dll` was rebuilt after the edit; its reference assembly was unchanged, so the dependents were up-to-date checks. | 8 s | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`: no comparison base, so all 13; ELEV004 excluded; known 0; suppressed 1770, unchanged from 0479 | 231 s | 600 s |
| unit-tests (selected `AbsorbingBiaxialNoisyTests`, `MuellerInverseTests`) | `Passed!` 21 of 21 (failed 0, skipped 0) | 1018 s | 7200 s |
| kit-acceptance (selected `KitAcceptanceTests`) | `Passed!` 14 succeeded, 1 skipped (AT-2's Explicit five-seed ensemble; the skip policy is `allow`) | 1418 s | 3600 s |
| 21 × invariant-s46-* | each decoded `git grep` finds nothing (21 × `hits=0`); positive control `QuartzSample` finds 8 | < 5 s | 300 s each |

- **Unit-test identities.** The TRX names exactly the step's 21 `change_expectation` identities: 20 in
  `MuellerInverseTests` and 1 in `AbsorbingBiaxialNoisyTests`. All passed, so the counts are unchanged.
  - `MuellerInverseTests` stays at 20, as at 0473.
  - The noisy absorbing ensemble ran 16 m 56 s; the quartz ensemble 7 m 51 s.
- **Kit-acceptance.** AT-2's grid-generator fact, which includes the fit through the shell, took 793 s, inside R0005's
  1800 s bound. The count of 14 matches 0479's.
- **Concurrency.** unit-tests and kit-acceptance ran at the same time, as at 0477–0479.
- **Run order.** The two module-doc edits landed while the test runs were in flight, over DLLs built before them. They
  change comments only. The rebuild's unchanged reference assembly confirms the compiled surface did not move.

## Artifacts

All under `specs/0050/.artifacts/`:

- `0480-build.log`
- `0480-elevation.log`
- `0480-invariants.log`
- `0480-dev-unit-tests.log`
- `0480-removed-lines.txt`: the 93 lines deleted from `MuellerInverseTests.fs`

## Gotchas

- **Module docs.** After the moves, both support modules' head docs gained one clause naming the runner they now hold,
  citing S14.2. These are comment-only edits inside the authorized files; the build after them is recorded under
  Testing state.
- **Shadowing in `MuellerInverseTests`.** The moved names resolve through `open BerremanTestSupport.QuartzSample`, the
  file's last open, so they win over every earlier open exactly as the class lets did.
  - `scalingAround` alone could not stay: with the class binding gone it would resolve to
    `MuellerFitting.scalingAround` (axes → scale → centre). That is why the slice renames it `quartzScalingAround`, and
    every use was renamed.
- **Namesakes left alone.** These are other classes' bindings for their own samples. None is in this step's edits, and
  none opens `QuartzSample`:
  - `BiaxialSample.solverParameters` (equal value);
  - `LangasiteInverseTests`' `solverParameters` and `forward` (the same uniaxial expression, over its own class);
  - `BiaxialInverseTests`' `blindStart` / `wideBox` (triclinic);
  - `AbsorbingBiaxialInverseTests`' `forward` (the same expression as `absorbingForward`);
  - `KitAcceptanceTests`' private `absorbingForward`, which the slice explicitly leaves untouched (step 434's helper).
  `BerremanTestSupport` is outside `.elevation-analyzers.json`'s `inScopePatterns`, so the duplicate-name rules do not
  see these. That is the position step 0473 recorded; the suppressed count stays 1770.
- **"Answering Result".** The two runners and `noisyQuartzObservations` answer `Result<_, MuellerFitError>`. The
  callers pipe through `InverseFitHarness.expectOk`. `BerremanTestSupport` references no test framework (step 344).
  `solverParameters`, `forward`, `quartzScalingAround`, `blindStart`, `wideBox` and `absorbingForward` are total values
  or functions, as they were in the classes.
- **Record inference.** `{ seed = seed; fit = fit }` resolves to `RecoveryEnsembles.NoisyExperiment`, the last opened
  record with those labels in both support files. The build confirms it.
