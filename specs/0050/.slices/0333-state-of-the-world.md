# Step 0333 — state of the world

## Where we are

Spec 0050 section S10.1 moves the bench noise model and the fit harness into Optimization. Its first steps move the
crystal tensor construction out of `BerremanTests` and onto the product parameter records (`.spec-md` §3.16).

- Step 332 moved the uniaxial builder onto `UniaxialParameters.opticalProperties`.
- This step moves the triclinic tensor construction onto `TriclinicParameters.opticalProperties`.
  `BiaxialSample.buildTriclinic` stays, with its signature, as the test-side caller of that member; it moves to
  `BerremanTestSupport` in step 345 (001 §Q).
- Step 334 does the same for `AbsorbingTriclinicParameters`.

## What's working

- Added `TriclinicParameters.opticalProperties`, the engine's class 1 crystal built from the record's nine constants
  (spec 0050 §3.16).
- `BiaxialSample.buildTriclinic` now calls the member; its old tensor body is deleted, leaving one implementation.
- Dropped the now-dead `open OpticalProperties.Active` from `BiaxialSample.fs`.
- BerremanTests' numbers and counts are unchanged.

## Tests

These are development runs of each gate's own command from `0333.gates`, over the final tree. The arc-runner's gate
engine runs the gates themselves.

| Gate | Result | Time | Deadline |
|---|---|---|---|
| build | exit 0, 0 errors | 76 s | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`; known findings 0; ELEV004 excluded | 210 s | 600 s |
| constructor-unit-tests (all) | Passed!, 1552 of 1552, 0 failed, 0 skipped | 71 s | 180 s |
| unit-tests (`BerremanTests.FeasibleBiaxialInverseTests`) | Passed!, 5 of 5, 0 failed, 0 skipped | 174 s | 7200 s |
| invariant-s33-\* (19) | each `INVARIANT HOLDS`, exit 0 | under 1 s each | 300 s |

- **build.** Only the two known `SYSLIB0051` warnings from the vendored MathNet `Exceptions.cs`. No file this step
  touched produced a warning.
- **Counts.**
  - constructor-unit-tests counts 1552, equal to checkpoint 331.
  - `FeasibleBiaxialInverseTests` declares 5 facts at HEAD and in the working tree, and all 5 ran. No test was added,
    removed or changed; no expected value or tolerance moved.
- **unit-tests time.** 174 s against the slice's ~104 s estimate, because it ran concurrently with the constructor
  tests and the elevation run. Well inside the 7200 s deadline.
- **Elevation** had no comparison base, so it covered all 12 projects. 1451 suppressed findings (ELEV004), the same as
  step 332. This step's diff adds no `.value`.
- **One implementation.** The only `Rho.type_1_Crystal` call left under `Berreman/` outside `Active.fs` is the member.
- **LF.** Neither edited file has a CR, and `git diff --numstat` equals `--ignore-cr-at-eol`.
- No run exceeded its effective deadline.

```yaml
gates:
  berreman_unit_tests: 5
  constructor_unit_tests: 1552
```

## Architecture

- **The tensor construction is a member on the record it builds from** (`.spec-md` §3.16), as step 332 did for the
  uniaxial record. `MuellerInverse` stays generic in `'Parameters`: `createBerremanForward` still takes a
  `'Parameters -> OpticalProperties` builder.
- **The test-side builder is a caller, not a copy.** `BiaxialSample.buildTriclinic` keeps its concrete
  `TriclinicParameters -> OpticalProperties` signature, so `BiaxialInverseTests` and `FeasibleBiaxialInverseTests` keep
  calling it unchanged. Its body is `p.opticalProperties`.
- **Bit-identical by construction.** The member evaluates the deleted body's record expression with the same arguments
  in the same order: `Eps.fromRefractionIndex (n1, n2, n3)`, `Mu.vacuum`, `Rho.type_1_Crystal g11 g22 g33 g23 g13 g12`.
- **No new project reference.** `MuellerInverse.fs` already opens `OpticalProperties.Active` (step 332), reached through
  Domain's `Analytics` reference.
- **ELEV.** The member is on its own type, so the primitive rules do not apply. It has no `.value`, no call-site wrap,
  and no new public module-level name.

## Deferred

Nothing. Every requirement of the slice landed. The slice assigns these to later steps:

- `AbsorbingTriclinicParameters.opticalProperties`, in step 334;
- the move of `BiaxialSample` (with `buildTriclinic`) to `BerremanTestSupport`, in step 345.

## Gotchas

- **`buildTriclinic` is deliberately kept.** The step retires only the tensor arithmetic, not the builder. Do not inline
  it into its callers: step 345 moves it with the rest of the sample to `BerremanTestSupport`.
- **`OpticalProperties` is both a namespace and the engine record type.** In `MuellerInverse.fs` the member's return
  type resolves to the record from `Berreman.MaterialProperties`.
- `.manifest.state.json` was already modified when the round started. It is the supervisor's file and was not touched.

## Changelog

- 2026-11-12 — Step 0333: added `TriclinicParameters.opticalProperties`, the one triclinic gyrotropic crystal builder.
  - `BiaxialSample.buildTriclinic` now calls it; the old tensor body and its dead open are deleted.
  - Numbers and counts are unchanged.
