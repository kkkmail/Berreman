# Step 0332 — state of the world

## Where we are

Spec 0050 section S10.1 moves the bench noise model and the fit harness into Optimization. Its first three steps move
the crystal builders out of `BerremanTests` and onto their product parameter records (`.spec-md` §3.16).

- Step 331 retired the archive-keyed Mueller data seam.
- This step moves the uniaxial builder. `UniaxialParameters.opticalProperties` is now the one uniaxial gyrotropic
  builder. Its three test copies are gone: `buildUniaxialGyrotropic`, `buildLangasite` and `BiaxialInverseTests`'
  fact-local `buildUniaxial`.
- Steps 333 and 334 do the same for `TriclinicParameters` and `AbsorbingTriclinicParameters`.

## What's working

- Added `UniaxialParameters.opticalProperties`, the engine's class 3/4/6 crystal built from the record's four constants
  (spec 0050 §3.16).
- Retired `buildUniaxialGyrotropic`, `buildLangasite` and `BiaxialInverseTests`' local `buildUniaxial`. The three
  uniaxial forward models and the `orientForCut` fact read the member.
- Rewrote `MuellerInverse`'s module doc: each parameter record carries its crystal class's builder.
- Dropped the now-dead `open OpticalProperties.Active` from the three test files.
- BerremanTests' numbers and counts are unchanged.

## Tests

These are development runs of each gate's own command from `0332.gates`, over the final tree. The arc-runner's gate
engine runs the gates themselves.

| Gate | Result | Time | Deadline |
|---|---|---|---|
| build | exit 0, 0 errors | 79 s | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`; known findings 0; ELEV004 excluded | 213 s | 600 s |
| constructor-unit-tests (all) | Passed!, 1552 of 1552, 0 failed, 0 skipped | 75 s | 180 s |
| unit-tests (`BerremanTests.LangasiteInverseTests`) | Passed!, 6 of 6, 0 failed, 0 skipped | 8 s | 7200 s |
| invariant-s33-\* (19) | each `INVARIANT HOLDS`, exit 0 | under 1 s each | 300 s |

- **build.** Only the two known `SYSLIB0051` warnings from the vendored MathNet `Exceptions.cs`. No file this step
  touched produced a warning.
- **Counts.**
  - constructor-unit-tests counts 1552, equal to checkpoint 330.
  - `LangasiteInverseTests` declares 6 facts at HEAD and in the working tree, and all 6 ran. No test was added or
    removed.
- **The two changed facts outside the selected class** were also run, by `--filter-method`. Both passed (2 of 2) at
  their unchanged tolerances:
  - `MuellerInverseTests.orientForCut carries the optic axis from the surface normal into the surface plane`;
  - `BiaxialInverseTests.the triclinic parameter set is a strict generalization of the uniaxial one`.
- **Elevation** had no comparison base, so it covered all 12 projects. It reports 1451 suppressed findings (ELEV004,
  which the command excludes and R7 measures without gating). Step 330 recorded 1453; step 331 retired the archive seam
  in between. This step's diff adds and removes no `.value`.
- **Leftovers.** No `.fs` under `Berreman/` names `buildUniaxialGyrotropic`, `buildLangasite` or `buildUniaxial`.
- **LF.** Every edited source file has no CR, and `git diff --numstat` equals `--ignore-cr-at-eol`.
- No run exceeded its effective deadline.

```yaml
gates:
  berreman_unit_tests: 6
  constructor_unit_tests: 1552
```

## Architecture

- **The builder is a member on the record it builds from** (`.spec-md` §3.16). `MuellerInverse` stays generic in
  `'Parameters`: `createBerremanForward` still takes a `'Parameters -> OpticalProperties` builder. Each caller passes the
  record's own member, `fun (p : UniaxialParameters) -> p.opticalProperties`.
- **Domain reaches the class 3/4/6 builder without a new project reference.** `MuellerInverse.fs` opens
  `OpticalProperties.Active`, which Domain already reaches through `Analytics`, as `MaterialLibrary.fs` does.
- **Bit-identical by construction.** The member evaluates the retired builders' expression, with the same arguments
  in the same order: `type_3_4_6_Crystal (EpsValue.fromRefractionIndex n_o) (EpsValue.fromRefractionIndex n_e) g11 g33`.
- **ELEV.** The member is on its own type, so the primitive rules do not apply. It has no `.value`, no call-site wrap,
  and no new public module-level name.

## Deferred

Nothing. Every requirement of the slice landed. The slice assigns these to later steps:

- `TriclinicParameters.opticalProperties` (retiring `BiaxialSample.buildTriclinic`), in step 333;
- `AbsorbingTriclinicParameters.opticalProperties`, in step 334.

## Gotchas

- **The module doc already says "each parameter record carries its crystal class's builder".** That is the wording the
  step prescribes and the section's end state. Until steps 333 and 334 land, only `UniaxialParameters` carries one.
- **`OpticalProperties` is both a namespace and the engine record type.** After `open OpticalProperties.Active`,
  `OpticalProperties.type_3_4_6_Crystal` resolves to the static extension on the record.
- **The lambdas need their `(p : UniaxialParameters)` annotation.** The member is looked up on `p` before
  `createBerremanForward` fixes `'Parameters`.
- `.manifest.state.json` was already modified when the round started. It is the supervisor's file and was not touched.

## Changelog

- 2026-11-12 — Step 0332: added `UniaxialParameters.opticalProperties`, the one uniaxial gyrotropic crystal builder.
  - Retired `buildUniaxialGyrotropic`, `buildLangasite` and `BiaxialInverseTests`' local `buildUniaxial`.
  - The three uniaxial forward models read the member. Numbers and counts are unchanged.
