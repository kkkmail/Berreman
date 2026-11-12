# Step 0334 — state of the world

## Where we are

Spec 0050 section S10.1 moves the bench noise model and the fit harness into Optimization. Its first steps move the
crystal tensor construction out of `BerremanTests` and onto the product parameter records (`.spec-md` §3.16).

- Step 332 moved the uniaxial builder onto `UniaxialParameters.opticalProperties`.
- Step 333 moved the triclinic tensor construction onto `TriclinicParameters.opticalProperties`.
- This step moves the absorbing triclinic tensor construction onto `AbsorbingTriclinicParameters.opticalProperties`.
  `AbsorbingBiaxialSample.buildAbsorbing` stays, with its signature, as the test-side caller of that member; it moves
  to `BerremanTestSupport` in step 346 (001 §Q).

## What's working

- Added `AbsorbingTriclinicParameters.opticalProperties`, the engine's absorbing triclinic gyrotropic crystal built
  from the record's fifteen constants (spec 0050 §3.16).
- `AbsorbingBiaxialSample.buildAbsorbing` now calls the member; its old tensor body is deleted, leaving one
  implementation.
- Dropped the now-dead `open System.Numerics` from `AbsorbingBiaxialSample.fs`.
- BerremanTests' numbers and counts are unchanged.

## Tests

These are development runs of each gate's own command from `0334.gates`, over the final tree. The arc-runner's gate
engine runs the gates themselves.

| Gate | Result | Time | Deadline |
|---|---|---|---|
| build | exit 0, 0 errors | 76 s | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`; known findings 0; ELEV004 excluded | 223 s | 600 s |
| constructor-unit-tests (all) | Passed!, 1552 of 1552, 0 failed, 0 skipped | 76 s | 180 s |
| unit-tests (`BerremanTests.AbsorbingBiaxialInverseTests`) | Passed!, 7 of 7, 0 failed, 0 skipped | 189 s | 7200 s |
| invariant-s33-\* (19) | each `INVARIANT HOLDS`, exit 0 | under 1 s each | 300 s |

- **build.** Only the two known `SYSLIB0051` warnings from the vendored MathNet `Exceptions.cs`. No file this step
  touched produced a warning.
- **Counts.**
  - constructor-unit-tests counts 1552, equal to checkpoint 332.
  - `AbsorbingBiaxialInverseTests` declares 7 facts at HEAD and in the working tree, and all 7 ran. No test was added,
    removed or changed; no expected value or tolerance moved. The four facts the step names
    (`the absorbing sample attenuates…`, `normalized data is markedly blind…`, `the absorbing measurement set recovers
    all FIFTEEN…`, `all fifteen parameters are observable…`) pass through the member via `buildAbsorbing`.
- **unit-tests time.** 189 s against the slice's ~177 s estimate, because it ran concurrently with the constructor
  tests and the elevation run. Well inside the 7200 s deadline.
- **Elevation** had no comparison base, so it covered all 12 projects. 1475 suppressed findings (ELEV004), 24 more than
  step 333: the member's `.value` reads, which moved into Domain with the body from the out-of-scope test project.
  ELEV004 is measured, not gated (spec 0048 `.manual/002` R7).
- **One implementation.** `git grep "let n1Sq"` under `Berreman/` finds only the member.
- **LF.** Neither edited file has a CR, and `git diff --numstat` equals `--ignore-cr-at-eol`.
- No run exceeded its effective deadline.

```yaml
gates:
  berreman_unit_tests: 7
  constructor_unit_tests: 1552
```

## Architecture

- **The tensor construction is a member on the record it builds from** (`.spec-md` §3.16), as steps 332 and 333 did
  for the uniaxial and triclinic records. `MuellerInverse` stays generic in `'Parameters`: `createBerremanForward`
  still takes a `'Parameters -> OpticalProperties` builder.
- **The test-side builder is a caller, not a copy.** `AbsorbingBiaxialSample.buildAbsorbing` keeps its concrete
  `AbsorbingTriclinicParameters -> OpticalProperties` signature, so `AbsorbingBiaxialInverseTests` and
  `AbsorbingBiaxialNoisyTests` keep calling it unchanged. Its body is `p.opticalProperties`.
- **Bit-identical by construction.** The member evaluates the deleted body with the same arguments in the same order:
  `Complex(nᵢ·nᵢ, ε″ᵢᵢ)` on the diagonal and `Complex(0, ε″ᵢⱼ)` off it through `Eps.create`, `Mu.vacuum`, and
  `Complex(0, gᵢⱼ)` through `Rho.create`.
- **No new project reference or open.** `MuellerInverse.fs` already opens `System.Numerics` and
  `Berreman.MaterialProperties`.
- **ELEV.** The member is on its own type, so the primitive rules do not apply; its local `e` is a closure inside the
  member, not a module-level name. No call-site wrap, and no new public module-level name.

## Deferred

Nothing. Every requirement of the slice landed. The slice assigns this to a later step:

- the move of `AbsorbingBiaxialSample` (with `buildAbsorbing`) to `BerremanTestSupport`, in step 346.

## Gotchas

- **`buildAbsorbing` is deliberately kept.** The step retires only the tensor arithmetic, not the builder. Do not inline
  it into its callers: step 346 moves it with the rest of the sample to `BerremanTestSupport`.
- **`OpticalProperties` is both a namespace and the engine record type.** In `MuellerInverse.fs` the member's return
  type resolves to the record from `Berreman.MaterialProperties`.
- **ELEV004's suppressed count rose by 24.** That is the moved body's `.value` reads arriving in an analyzed project,
  not a new finding; the gate excludes ELEV004.
- `.manifest.state.json` was already modified when the round started. It is the supervisor's file and was not touched.

## Changelog

- 2026-11-12 — Step 0334: added `AbsorbingTriclinicParameters.opticalProperties`, the one absorbing triclinic
  gyrotropic crystal builder.
  - `AbsorbingBiaxialSample.buildAbsorbing` now calls it; the old tensor body and its dead open are deleted.
  - Numbers and counts are unchanged.
