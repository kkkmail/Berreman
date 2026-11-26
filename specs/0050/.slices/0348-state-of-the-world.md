# Step 0348 — state of the world

## Where we are

This step is part of spec 0050 S10.3 (001 §Q AT-1, §S10): the staged Mueller pipeline and its reference values are
promoted.
- Steps 0344–0347 created `BerremanTestSupport` and made both test suites reference it.
- This step gives the reference pipeline's transcribed expectations their one home there:
  `BerremanTestSupport.MuellerReference`.
- `MuellerReconstructionTests` now reads the module, and AT-1 will open the same module.
- This is a MOVE. Every expected literal of the Stage-1 and section 7.2 facts, the two bands and the excluded point
  now exist once, in the module.

## What's working

- Added `BerremanTestSupport.MuellerReference` with the two bands, the six Stage-1 constants, the three section 7.2
  matrices, the three cascade metrics, the two largest-delta positions and the excluded point, each born elevated.
- Moved the expectations out of `MuellerReconstructionTests`: the Stage-1, `buildDesign` and section 7.2 facts read
  `MuellerReference.<name>`, and the class-level bands and their comment block are deleted.
- Enumerated the cascade's largest-delta positions as `(MuellerRowIndex, MuellerColumnIndex)` against the module's set.
- Kept `MuellerReconstructionTests` bit-identical and green: 25 of 25, the same count and the same doubles.

## Tests

These are development runs of each gate's own command from `0348.gates`, over the final tree. The arc-runner's gate
engine runs the gates themselves.

| Gate | Result | Time | Deadline |
|---|---|---|---|
| build | exit 0, 0 errors; 6 warnings, all NU1902 (exempt advisory) and the vendored MathNet SYSLIB0051 | 76 s full, 14 s final incremental | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`; known findings 0; suppressed 1502 (unchanged); ELEV004 excluded | 221 s (final tree; 224 s on the first run) | 600 s |
| unit-tests (`BerremanTests.MuellerReconstructionTests`) | Passed!, 25 of 25, 0 failed, 0 skipped | 3 s | 7200 s |
| invariant-s35-\* (20) | each grep found nothing, both as written and with `--untracked` | under 1 s each | 300 s |

- No run went past its effective deadline.
- The unit-test count matches the class's last measured selection (25, step 0331). No fact was added or removed.
- The TRX shows all three changed facts `Passed`: Stage-1, `buildDesign` and section 7.2.
- Bit-identity: the five degree literals come back exactly through `Angle.degree` / `Retardance.degree` and
  `.degrees` (`.artifacts/0348-roundtrip-check.log`). The matrix and metric literals are the same text.
- All touched files are LF-only: `git diff --numstat` equals `--ignore-cr-at-eol`.

```yaml
gates:
  berreman_unit_tests: 25
```

## Architecture

- `MuellerReference` sits in `BerremanTestSupport` after `AbsorbingBiaxialSample.fs`. It depends only on Core
  (`Berreman.Constants`, `.Geometry`, `.Fields`) and on `OpticalConstructor.Domain.MuellerReconstruction`, both
  already referenced. No project reference changed.
- Each value has a concrete annotation and is born elevated:
  - `DarkMean`, `Angle`, `Retardance`, `MuellerMatrix` and `MatrixDeviation`;
  - `Set<MuellerRowIndex * MuellerColumnIndex>` for the positions;
  - `ExcludedPoint list` for the excluded point.
  The two bands stay `float`, like `MatrixComparison.allowedDiff`.
- The test reads the expected values through the IO-seam accessors (`.value`, `.degrees`) only where it compares
  against raw computed doubles.

## Deferred

- Nothing. Every item of the brief is in place.

## Gotchas

- **The bands stay `float`.** `stage1Tol` bounds both a dark-count deviation and degree deviations, so no single
  elevated type fits it. The brief moves both bands "with their values", and the facts compare them against raw
  deviations.
- **Two literals of the same values remain in the test class, outside the three changed facts:**
  - The Stage-3 signal-reduction fact (always-run) uses `DarkMean 869.666` as a synthetic input. It is not one of
    this step's authorized test effects, so it is unchanged.
  - The `buildDesign` fact's `MuellerRawRow` fixture keeps an `LP-(LR#90)-CPL-2` / capture 17 / 140° row. That row
    is the input the excluded point must drop, not a copy of the `ExcludedPoint`.
- **The spec 0044 note on the largest-delta positions moved with the value.** It is now two lines on
  `MuellerReference.cascadeLargestDeltaPositions` and is no longer in the fact, so the decision is documented once.
  The fact keeps its spec 0042 paragraph.
- **One extra comment changed.** The `buildDesign` fact said the point was "supplied here", which is no longer
  true. It now names `MuellerReference.excludedPoints`.
- **`BerremanTestSupport` is listed in elevation's scope but has no findings.** Its assembly name is not
  `Softellect.Berreman.*`, so `.elevation-analyzers.json`'s `inScopePatterns` leaves it out (as step 0344
  records). It reports `findings=0` and nothing suppressed.
- **The module's docs avoid the path `Berreman/Data/MuellerMatrix/`.** The brief cites it for
  `EXCLUDED_POINTS`, but the s35 invariant `t9-no-data-mueller-matrix` greps `Data/MuellerMatrix` under
  `Berreman/`. The doc cites `matrix_glue.py:22` instead, which is in `data/optics-mueller/data/reference/`.

## Changelog

- 2026-11-12 — Step 0348: moved the reference pipeline's transcribed expectations, bands and excluded point into
  `BerremanTestSupport.MuellerReference` (001 §Q AT-1, S10.3). Build, elevation, unit-tests
  (`MuellerReconstructionTests`, 25) and all 20 s35 invariants are green.
