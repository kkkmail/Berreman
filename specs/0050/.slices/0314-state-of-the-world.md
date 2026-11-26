# Step 0314 — state of the world

## Where we are

Spec 0050 section S9.3 covers one file serving many experiments, exclusions, corrections, the Data step and the
experiment matrix (001 §K). Step 313 gave a mapped attachment its point exclusions, each with a reason. This step makes
them count:

- an excluded point stays in the data but leaves what is validated;
- the experiment's data badge says how many points were kept and how many excluded.

`MappedPoints.keptBy` is the one rule from an attachment's exclusions to the points a validation reads, and the rule a
study's residual reads in S11. The steps that follow add the messages that set and clear exclusions.

## What's working

- Add `MappedPoints.keptBy`: the points in order without the excluded ones; the case and a series' kind kept; an
  exclusion naming no point changes nothing (S9.3).
- `loadAndValidate` validates only the kept points. An excluded point outside the scan's range no longer refuses the
  data, and it is never counted as validated.
- Add `ExcludedPointsBadge` (kept and excluded counts, in `KeptPointsInfo`). `ofCheck` answers it in place of
  `ValidatedDataBadge` when a validated attachment has exclusions. Its text is
  `excluded points (<k> excluded, <n> validated)`.
- The data-file row's caption for it is `Loaded — <n> point(s) kept, <k> excluded`. Every other caption and every
  other `ofCheck` arm is unchanged.

## Tests

These were development runs of each gate's own command over the final tree. The arc-runner's gate engine runs the
gates themselves.

| Gate | Result | Time | Deadline |
|---|---|---|---|
| build | exit 0, 0 warnings, 0 errors | 57 s, then 23 s incremental | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`; known findings 0; ELEV004 excluded (suppressed 1428, unchanged) | 202 s | 600 s |
| constructor-unit-tests | Passed!, 1529 of 1529, 0 failed | 70 s | 180 s |
| ui-smoke | Passed!, 253 of 253 | 233 s | 600 s |
| ui-tests (selected: `InverseConstructorTests`, `SolverHandoffWindowTests`) | Passed!, 23 of 23 | 27 s | 360 s |
| invariant-s31-\* (16) | each `INVARIANT HOLDS` | under 1 s each | 300 s |

- **constructor-unit-tests: 1529**, which is 1527 + 2, as the slice says. The TRX shows each named fact as `Passed`:
  - new: `kept points leave out the excluded ones and keep the rest in order`;
  - new: `loading data validates the kept points, so an excluded point outside the range does not refuse the data`;
  - changed: `the data badge follows the attachment, its file's hash and its validation`. It now also pins the new
    badge, its text, its caption and `DataChangedBadge` ahead of it.
- **ui-tests: 23** over the two selected classes. No Ui test was touched, so their count is unchanged.
- **ui-smoke: 253**, equal to the baseline.
- **Red first.** The test project failed to build with 10 errors, each naming the missing `keptBy` or
  `ExcludedPointsBadge`.
- **The shape deviation is measured, not assumed.** The slice's literal tuple case failed the analyzer with ELEV011.
  The file was then restored byte-identical.
- No run exceeded its effective deadline.
- Every touched file is LF only (0 CR bytes), and `git diff --numstat` equals the `--ignore-cr-at-eol` count.

```yaml
gates:
  constructor_unit_tests: 1529
  ui_smoke_tests: 253
  ui_tests: 23
```

## Architecture

- **The pipeline is read, then keep, then validate.** `readMapped` applies the import profile and the row selection.
  `keptBy` applies the exclusions, and `validatedFor` checks what is left. Exclusions therefore number the selected
  points, as `MappedPointNumber` (step 313) defines them.
- **`keptBy` never touches the file.** Raw data stays immutable: `readMapped` still answers every point, and only the
  validated view drops the excluded ones.
- **The badge counts sit in a record**, `KeptPointsInfo = { kept; excluded }`, declared just before
  `ExperimentDataBadge`. Two same-typed fields on one union case are an ELEV011 finding; a record is the analyzer's
  remedy. The field names and types are the slice's.
- The callers (`TableAndElementRotationView.fs`'s `checkExperimentData` / `dataBadgeOf`, and
  `SolverHandoffView.validateExperiment`) are unchanged. They pass the mapped attachment to `ofCheck`, or count the
  validated points, so they now see only the kept points.

## Deferred

- Nothing. Every requirement of the slice landed in this round.

## Gotchas

- **Deviation from the slice's literal case shape.** The slice prescribes `ExcludedPointsBadge of kept :
  MeasuredPointCount * excluded : MeasuredPointCount`.
  - The pinned `Softellect.Analyzers 10.1.402.136` reports that as ELEV011 (repeated tuple element type), measured in
    `.artifacts/0314-dev-mutation-literal-case.log`. The analyzers win (AGENTS.md).
  - The case is `ExcludedPointsBadge of counts : KeptPointsInfo`, and the tests build it as
    `ExcludedPointsBadge { kept = MeasuredPointCount 2; excluded = MeasuredPointCount 1 }`.
- **`excluded` is the number of exclusions** (the slice's rule), not the number of points they remove. An exclusion
  naming a point past the end still counts in the badge, though it removes nothing.
- **A copy-and-update keeps a series' kind.** `IntensitySeries` and `EllipsometricSeries` share the `points` label, so
  `keptBy` writes `{ series with points = … }`. A bare `{ points = … }` would resolve by label order.
- Step 300's fixtures in `ImportProfileTests` are now private module values, shared by the count fact and the
  kept-points fact.

## Changelog

- 2026-11-11 — Step 0314: an excluded point leaves what is validated, and the badge counts kept and excluded points
  (S9.3).
  - Domain: `MappedPoints.keptBy`; `loadAndValidate` validates the kept points; `KeptPointsInfo` and
    `ExperimentDataBadge.ExcludedPointsBadge` with its text and `ofCheck` arm.
  - Ui: the `Loaded — <n> point(s) kept, <k> excluded` caption.
  - Tests: two new facts and one extended fact. Step 300's fixtures were lifted to module values.
