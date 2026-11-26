# Step 0351 — state of the world

## Where we are

This step is part of spec 0050 S10.3: the staged Mueller pipeline and its reference values are promoted.
- Step 0349 moved Stage 1 into `OpticalConstructor.Optimization.StagedMuellerPath`.
- Step 0350 made `buildDesign` hand each kept row's reading to its projection.
- This step moves Stages 2 and 3 into the same module, as `reconstructStaged`. The hand-composed Stage 3 now exists
  once, in product code. The section 7.2 fact calls it and keeps only assertions.
- Step 0352 is next. It re-points the cascade check, which still reads `cascadeProduct` in the fact.

## What's working

- Promoted the staged Mueller path's Stages 2 and 3: `reconstructStaged` answers `StagedMuellerData` (stage-1
  constants, AIR gain rules, day-1 CPL-CPL interpolation, M_QZ / M_LR / M_{QZ+LR} with rank and residual) or a typed
  `StagedMuellerError`.
- Added `ObjectReconstructionErr (kind, SolverError)`. An empty AIR block now answers `EmptyCaptureSetErr` naming its
  label, where the fact used to divide 0 by 0 or throw.
- Re-pointed the section 7.2 fact to `reconstructStaged`. Its body is now assertions only, plus one new assertion
  holding all nine AIR gains within `dataMatrixTol` of `MuellerReference`.
- Added the nine reference AIR gains (`gain_summary`) to `MuellerReference`, and noted their observed band on
  `dataMatrixTol`.
- Added two `StagedMuellerPathTests` facts over synthetic captures: a missing AIR block, and a failed QZ solve.

## Tests

These are development runs of each gate's own command from `0351.gates`, over the final tree. The arc-runner's gate
engine runs the gates themselves.

| Gate | Result | Time | Deadline |
|---|---|---|---|
| build | exit 0, no `error`; warnings only NU1902 (exempt) and the vendored MathNet SYSLIB0051 | 77 s (full), 7 s (final incremental) | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`; known findings 0; suppressed 1506 (unchanged) | 217 s | 600 s |
| constructor-unit-tests | Passed!, 1557 of 1557 (1555 + 2 new, both Passed in the TRX) | 69 s | 180 s |
| unit-tests (`BerremanTests.MuellerReconstructionTests`) | Passed!, 25 of 25 (count unchanged) | 3 s | 7200 s |
| invariant-s35-\* (20) | each grep found nothing, both as written and with `--untracked` | under 1 s each | 300 s |

- No run went past its effective deadline.
- `MuellerReconstructionTests`' numbers are bit-identical by construction: every gain, projection and solve runs the
  fact's own float operations, in the same order, on the same rows. Its matrix and cascade bands pass unchanged.
- The two new constructor facts raise the count by exactly 2, as the acceptance requires.

```yaml
gates:
  constructor_unit_tests: 1557
  berreman_unit_tests: 25
```

## Architecture

- **`reconstructStaged` is a pure function** of a `MuellerSolverProxy`, the caller's `ExcludedPoint`s and the seven
  files' capture rows.
  - The excluded points stay the caller's data (spec 0042 §0).
  - The nine AIR labels are literals of the path until S11.1 turns them into session data.
- **The private helpers carry the moved bodies.**
  - `AirBlockRows` is a non-empty block, so its `meanTime` is total.
  - `airBlock` answers a block or `EmptyCaptureSetErr`.
  - `fitAirGain`, one builder per gain rule, `airGainRules`, and `prepareRow`.
  - Every helper has two or more parameters and no bare primitive (ELEV003P, ELEV010P, ELEV013).
- **The `Result.bind` chain is split by gain rule**, five levels deep instead of nine. The tree has no `result { }`
  builder to reuse.
- **Contract changes:**
  - `StagedMuellerData` and `reconstructStaged` are created.
  - `StagedMuellerError` and `BerremanTestSupport.MuellerReference` change shape.
  - No project reference changed.

## Deferred

- Nothing. Every item of the brief is in place. The cascade check moving off `cascadeProduct` in the fact is step
  0352's, as the brief says ("until step 352").

## Gotchas

- **The labels are written inline as `MuellerExperimentLabel "…"` literals** in `airGainRules`, where each rule
  uses them. ELEV001 fires only on a `.value` re-wrap, and the doc records that the labels are the reference's own.
- **The synthetic darkness row is the existing `darknessRow` (869.5).**
  - Against AIR rows of 2 + cos 2α, the dark-subtracted signals and gains are negative but finite.
  - Every stage-1 solve has rank 2, so stage 1 solves as the brief requires.
- **The design is concatenated with `Array.concat`.** It gives the same rows, in the same order, as the fact's
  `List.collect` / `List.toArray`, and it does no arithmetic.
- **ELEV004's suppressed count stayed at 1506**, even with the new `label.value` in `airBlock`'s error text.
  ELEV004 is excluded from the gate (spec 0048 `.manual/002` R7), and no enforced rule found anything.

## Changelog

- 2026-11-12 — Step 0351: moved Stages 2 and 3 of the section 7.2 pipeline into `StagedMuellerPath.reconstructStaged`
  (spec 0050 S10.3), with `StagedMuellerData` and `ObjectReconstructionErr`. The 7.2 fact calls it and keeps only
  assertions, plus a nine-gain assertion against the new `MuellerReference` AIR gains. Two typed-error facts were
  added. Development runs: build, elevation, constructor-unit-tests (1557), unit-tests (`MuellerReconstructionTests`,
  25) and all 20 s35 invariants are green.
