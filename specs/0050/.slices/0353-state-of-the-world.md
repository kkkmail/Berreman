# Step 0353 — state of the world

## Where we are

This step opens spec 0050 S11.1: material physics forms.
- A material version will be able to hold one of the three crystal-parameter records as its physics (.spec-md §3.16).
  For that, the records must compile before `MaterialLibrary.fs`, where `MaterialEntry` lives.
- This step moves `UniaxialParameters`, `TriclinicParameters` and `AbsorbingTriclinicParameters`, with the
  `opticalProperties` builders steps 332–334 put on them, out of `MuellerInverse.fs` into the new `CrystalPhysics`
  module. That module compiles directly after `DispersionModels.fs`.
- Their `axes` stay in `MuellerInverse.fs` as static type extensions, beside `ParameterAxis`.
- Step 0354 is next.

## What's working

- Added `OpticalConstructor.Domain/CrystalPhysics.fs`. It declares the three crystal-parameter records and their engine
  builders once, ahead of the material library (spec 0050 S11.1).
- Moved the records and builders out of `MuellerInverse.fs` verbatim. Their `axes` stay there as
  `type … with static member axes` extensions, with the same docs and bodies.
- Re-pointed the inverse-fit tests, the two `BerremanTestSupport` samples and `ItemEvaluationTests` with one
  `open OpticalConstructor.Domain.CrystalPhysics` line each.
- Added the constructor fact `each crystal record's builder composes its permittivity from its own fields`.

## Tests

These are development runs of each gate's own command from `0353.gates`, over the final tree. The arc-runner's gate
engine runs the gates themselves.

| Gate | Result | Time | Deadline |
|---|---|---|---|
| build | exit 0; the word `error` appears 0 times; warnings only NU1902 (exempt) and the vendored MathNet SYSLIB0051 | 76 s | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`; known findings 0; suppressed 1509 (ELEV004 excluded) | 210 s | 600 s |
| constructor-unit-tests | Passed!, 1559 of 1559 (1558 + 1 new, Passed in the TRX) | 70 s | 180 s |
| unit-tests (`AbsorbingBiaxialInverseTests`, `FeasibleBiaxialInverseTests`, `LangasiteInverseTests`) | Passed!, 18 of 18, 0 failed (7 + 5 + 6 in the TRX, the facts the three files declare; count unchanged) | 272 s | 7200 s |
| invariant-s36-\* (20) | each grep found nothing, both as written and with `--untracked` | under 1 s each | 300 s |

- No run went past its effective deadline.
- BerremanTests' numbers are bit-identical by construction:
  - the builders and the `axes` are the same expressions, moved line for line;
  - no BerremanTests fact was added or removed.
- The new constructor fact raises the count by exactly 1, as the acceptance requires.

```yaml
gates:
  constructor_unit_tests: 1559
  berreman_unit_tests: 18
```

## Architecture

- **`CrystalPhysics` sits between `DispersionModels` and `MaterialLibrary`** in the Domain compile order. It depends
  only on the engine (`Berreman.MaterialProperties`, `OpticalProperties.Active`) and `System.Numerics`, so the
  material library can name the records in the next steps.
- **The inverse-problem surface stays in `MuellerInverse`.** `axes` needs `ParameterAxis` (declared there) and
  `ParameterName` (`ItemModels.fs`), both compiled after `MaterialLibrary.fs`. So each record's `axes` is an
  optional static extension declared directly after `ParameterAxis`. Every `UniaxialParameters.axes` call site reads
  as before wherever `MuellerInverse` is open.
- **Departure from §3.16, as the brief records it:** §3.16 put the builders "into `MuellerInverse.fs` as members of
  their parameter records". The records now live in `CrystalPhysics`, and each builder still has exactly one
  implementation.
- **Contract changes:**
  - Retired: `MuellerInverse.UniaxialParameters`, `.TriclinicParameters` and `.AbsorbingTriclinicParameters`.
  - Created: the same three records in `CrystalPhysics`.
  - No project reference changed.
  - No alias, re-export or second declaration exists (ELEV014).

## Deferred

- Nothing. Every part of the brief, (1) to (4), is in place.

## Gotchas

- **The edit list missed `OpticalConstructor.Tests/ItemEvaluationTests.fs`.** It names `UniaxialParameters`, and it
  got the same one-line open under the brief's rule (3), as steps 0259, 0291 and 0313 did for their unlisted
  fixtures.
- **`git grep` skips untracked files.** Until `CrystalPhysics.fs` is committed, the acceptance grep
  `type (UniaxialParameters|…) =` finds nothing. With `--untracked` it finds only `CrystalPhysics.fs`.
- **`CrystalPhysics.fs` opens `System.Numerics`, `Berreman.MaterialProperties` and `OpticalProperties.Active`.** The
  brief's list named `Fields`. The moved bodies read nothing from `Fields`; `RefractionIndex` lives in
  `MaterialProperties`. They do read `Complex`.
- **Record labels are shared:**
  - `g11` / `g33` with the engine's gyration records in `Berreman.Dispersion`;
  - `index1..3` with `MaterialComplexityEditor`'s state record.

  A file that opens `CrystalPhysics` after one of those modules must annotate a bare record literal.
- **`Berreman/BerremanTests/BiaxialSample.fs` and `AbsorbingBiaxialSample.fs` are census paths only.** Steps 345–346
  moved those files to `BerremanTestSupport/`.

## Changelog

- 2026-11-13 — Step 0353 (spec 0050 S11.1): moved `UniaxialParameters`, `TriclinicParameters` and
  `AbsorbingTriclinicParameters`, with their `opticalProperties` builders, from `MuellerInverse.fs` into the new
  `CrystalPhysics.fs`, compiled after `DispersionModels.fs`. Their `axes` stay in `MuellerInverse.fs` as static
  extensions. Consumers open `CrystalPhysics`. One constructor fact was added (1559). Development runs: build,
  elevation, constructor-unit-tests, unit-tests (three selected classes) and the 20 s36 invariants.
