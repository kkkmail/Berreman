# Step 0395 — state of the world

## Where we are

Spec 0050, section S11.4: fit results (uncertainty, correlation, realizability, product check, profile likelihood).

- Step 394 added the Jacobian's rank and a system's residuals.
- This step adds the two thresholds that decide which fitted parameters are suspect, and so are profiled:
  - a correlation with another parameter;
  - a sigma as a fraction of the bounds' span.
- They are study data, stored in the study file's fit settings beside the mode and the start count (step 382). They
  are not an application setting (.spec-md §8).

## What's working

- Added `FitCorrelation` and `SigmaFraction`, and `SuspectThresholds { correlation; sigmaFraction }`.
  - Its `initial` is 0.95 / 0.1.
  - Its `tryCreate` keeps a correlation in (0, 1] and a finite fraction above 0. Anything else, NaN included, is
    `InvalidSuspectThresholdErr`, naming the value.
- `StudyFitSettings` gained `suspect` as its last field. `Study.withSuspectThresholds` sets it in place, beside
  `withFitStarts`.
- The study schema requires `settings.suspect`, an object of exactly `correlation` (above 0, at most 1) and
  `sigmaFraction` (above 0). Both are written as plain numbers, with no converter.
- Added the `StudyTests` fact for the thresholds, and widened the study-file round-trip, schema-refusal and
  error-render facts.

## Tests

Run locally on 2026-11-14. Every run finished within its effective deadline.

- `build`: exit 0, 0 errors. The first build took 82 s and the final incremental build 4 s (deadline 600 s).
  - The warnings are the exempt NU1902 advisories, plus the vendored MathNet `SYSLIB0051` pair, which appears only
    when MathNet recompiles.
- `constructor-unit-tests`, whole suite: 1699/1699, 118 s (deadline 180 s).
  - The count is 1698 → 1699, +1, as the acceptance requires.
  - The TRX shows each of these `Passed`: the new `StudyTests` fact, both widened `KitContentFileJsonTests` facts and
    the widened `StudySessionTests` render fact.
- `elevation`: `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`, 219 s (deadline 600 s), with 1579 suppressed and 0
  known.
- The twenty `invariant-s39-*` greps all find nothing.
- Mutation checks:
  - Widening the domain's correlation bound fails only the new fact.
  - Widening the schema's `maximum` fails only the schema-refusal fact.
  - Both were restored byte for byte before the final build and test run.

```yaml
gates:
  constructor_unit_tests: 1699
```

## Architecture

- **A threshold and a measurement share one type.** `FitCorrelation` is a correlation coefficient between two
  fitted parameters, so the results can read it both as a measured correlation and as its threshold.
  - Its own constructor is unchecked, because a measured correlation may be negative.
  - Only `SuspectThresholds.tryCreate` limits a threshold to (0, 1].
- **The schema and the domain state the same bounds.** A stored file is refused at the schema (`SchemaValidationErr`)
  before it binds, and a value built in code is refused by `tryCreate`.
- **No up-converter** (.spec-md §4 'Stored formats'). A version-1 study file without `suspect` is schema-invalid, and
  the tree holds no committed `*.study-json`.

## Deferred

Nothing. Every requirement of the step landed this round.

## Gotchas

- **`KitLibraryTests.fs` was edited, though it is outside `effects.edits`.**
  - The shared fixture `StudyFileFixtures.qzLrStudyOver` lives there, not in `KitStudyFileTests.fs`, and it spells
    out a `StudyFitSettings` record.
  - How-to (3) requires `suspect = SuspectThresholds.initial` in every such record expression, and the record does not
    compile without it.
  - The edit is that one line. `KitStudyFileTests.fs` needed no change.
- **`withSuspectThresholds` is an intrinsic member of `Study`.** Step 382 read the same `type Study with member ...`
  wording the same way for `withFitStarts`.
- **The record label `correlation`** also names a field of `FitQuality`'s record in Optimization. That record is
  declared locally after its opens and keeps precedence.

## Changelog

- 2026-11-14 — Step 0395: suspect thresholds in a study's fit settings (S11.4).
  - Added `FitCorrelation`, `SigmaFraction` and `SuspectThresholds` (0.95 / 0.1, `tryCreate`).
  - Added `StudyFitSettings.suspect`, `Study.withSuspectThresholds` and `InvalidSuspectThresholdErr`.
  - The schema requires `settings.suspect`.
  - Added one `StudyTests` fact and widened three facts.
