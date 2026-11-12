# Step 0336 — state of the world

## Where we are

Spec 0050 section S10.1 moves the bench noise model and the fit harness into Optimization. Step 335 created the
product home, `OpticalConstructor.Optimization/MuellerFitting.fs`, with `configurationOf` and `observeWith`. This
step moves the next four names from 001 §Q AT-2 into it, after `observeWith`:

- the two residual closures, `residualFor` and `throughputResidualFor`;
- the Jacobian adapter, `residualJacobianAt`;
- the generic `scalingAround`.

Step 337 moves the fit runners (`fitWithResidual`, `fitObservations`, `fitAbsorbingObservations`), which already call
these product closures.

## What's working

- Moved `residualFor`, `throughputResidualFor`, `residualJacobianAt` and `scalingAround` into
  `OpticalConstructor.Optimization.MuellerFitting`. Signatures and bodies are unchanged.
- Cut each moved doc to its contract in three lines or fewer. The penalty, log-throughput and ALGLIB-step hazards are
  kept.
- Deleted the four from `InverseFitHarness`. Its fit runners now call the product closures.
- Pointed the shadowing comment on `MuellerInverseTests.scalingAround` at `MuellerFitting.scalingAround`.
- Numbers and counts are unchanged: the Feasible and Absorbing inverse classes ran 12 of 12, and the constructor suite
  1552 of 1552.

## Tests

These are development runs of each gate's own command from `0336.gates`, over the final tree. The arc-runner's gate
engine runs the gates themselves.

| Gate | Result | Time | Deadline |
|---|---|---|---|
| build | exit 0, 0 errors, 0 warnings | 45 s | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`; known findings 0; ELEV004 excluded | 227 s | 600 s |
| constructor-unit-tests (all) | Passed!, 1552 of 1552, 0 failed, 0 skipped | 81 s | 180 s |
| unit-tests (`AbsorbingBiaxialInverseTests`, `FeasibleBiaxialInverseTests`) | Passed!, 12 of 12, 0 failed, 0 skipped | 301 s | 7200 s |
| invariant-s33-\* (19) | each `INVARIANT HOLDS`, exit 0 | under 1 s each | 300 s |

- **build.**
  - The first build failed with FS0039 at `m.element 0 0` in the moved `throughputResidualFor`. The
    `OpticalConstructor.Domain` open repaired it, and the rebuild recompiled `OpticalConstructor.Optimization`,
    `BerremanTests` and `OpticalConstructor.Tests`.
  - The rebuild reported 0 warnings. MathNet was not recompiled, so its two known `SYSLIB0051` warnings did not
    appear.
- **Counts.**
  - constructor-unit-tests counts 1552, the same as step 0335.
  - The TRX splits the selected classes into 5 `FeasibleBiaxialInverseTests` and 7 `AbsorbingBiaxialInverseTests`.
    These equal the `[<Fact>]` counts at HEAD and in the working tree.
  - The five tests named in the step's effects all ran and passed.
  - No test was added, removed or renamed, and no expected value or tolerance changed.
- **Elevation** had no comparison base, so it covered all 12 projects.
  - `OpticalConstructor.Optimization`, which now holds the moved bodies, reports 0 findings and 0 suppressed.
  - The tree-wide suppressed total (ELEV004) is 1475, the same as step 0335.
- **One declaration each.**
  - `grep -rnE "^\s*let (residualFor|throughputResidualFor|residualJacobianAt|scalingAround)\b"` over `Berreman/`
    finds the four in `MuellerFitting.fs` and none in `InverseFitHarness.fs`.
  - Two `scalingAround` hits remain elsewhere, both one-argument specialisations the step names as staying:
    `BiaxialSample.scalingAround` and `MuellerInverseTests.scalingAround`.
- **LF.** No edited or new file has a CR byte, and `git diff --numstat` equals `--ignore-cr-at-eol`.
- No run exceeded its effective deadline. Elevation, unit-tests and constructor-unit-tests ran concurrently.

```yaml
gates:
  berreman_unit_tests: 12
  constructor_unit_tests: 1552
```

## Architecture

- **`MuellerFitting` now owns the residual layer.** It holds the observations-to-residual closures the optimizer
  drives, their Jacobian and the generic scaling builder: what the study engine (S11) and the Planner will call. The
  test harness keeps only the fit runner (until step 337), the noise model and test assertions.
- **`MuellerFitting` opens `OpticalConstructor.Domain`.** `MuellerMatrix.element` is an extension in the
  `[<AutoOpen>]` module `OpticalConstructor.Domain.PropagationReadouts`, not a member of the engine type. The open
  comes before `OpticalConstructor.Domain.MuellerInverse`, so `MuellerInverse` still wins any shared name.
  `InverseFit.fs` in the same project already opens it.
- **Shadowing is preserved by open order.** The test files open `MuellerFitting` before `InverseFitHarness`,
  `BiaxialSample` and `AbsorbingBiaxialSample`, and the order did not change. So the one-argument specialisations
  still shadow the generic `scalingAround` exactly where they shadowed the harness copy.
- **Bit-identical by construction.** The bodies are verbatim, and the test call sites are unchanged.

## Deferred

Nothing. Every requirement of the slice landed. The slice assigns these to later steps:

- step 337 moves `fitWithResidual`, `fitObservations` and `fitAbsorbingObservations`.

## Gotchas

- **`MuellerMatrix.element` needs `open OpticalConstructor.Domain`.** Code moved out of a file that opens it fails
  with FS0039 in its new home until the open is added.
- **`git grep` and the `git diff` stats see only tracked files.** Use `grep -rn` to count declarations. This step
  adds no new source file, so the tracked diff covers every edit.
- **`grep -c $'\r'` under this Bash tool counts every line**, because the pattern reaches grep empty. Count CR bytes
  with Python.
- `.manifest.state.json` was already modified when the round started. It is the supervisor's file and was not touched.

## Changelog

- 2026-11-12 — Step 0336: moved `residualFor`, `throughputResidualFor`, `residualJacobianAt` and `scalingAround` from
  `InverseFitHarness` into `OpticalConstructor.Optimization.MuellerFitting`.
  - Signatures and bodies are unchanged; the docs were cut to the contract.
  - `MuellerFitting` gained `open OpticalConstructor.Domain` for `MuellerMatrix.element`.
  - Numbers and counts are unchanged.
