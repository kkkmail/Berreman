# Step 0382 — state of the world

## Where we are

This is spec 0050 section S11.3: engines, staged and joint fits, and append-only fit runs.

- **Steps 0380–0381** gave the LM solve a watch and moved the one bounded request into `ScaledSearch.solveWith`.
- **This step adds the fit's start count to a study's fit settings.** The nonlinear engine is multi-start (001 §L),
  and the count is stored in the study file beside the mode. It defaults to one start, the parameters' own, so a
  study fits as the inverse suites do.
- **Step 0387's nonlinear engine reads `settings.starts`** to decide how many members it fits. Step 0383 begins the
  fit-run file shapes.

## What's working

- Add `Studies.FitStartCount`: at least one start. `tryCreate` refuses a count below 1 with the new
  `StudyError.InvalidFitStartCountErr`, and `single` is 1
- `StudyFitSettings` gains `starts` (initially `FitStartCount.single`); `Study.withFitStarts` sets it in place, beside
  `withFitMode`
- `study.schema.json` version 1 requires `settings.starts`, an integer of at least 1. The serializer writes it as its
  number
- Add the `StudyTests` fact for the start count; widen the study-file round-trip, schema-refusal and error-render facts
  (constructor suite 1652 → 1653)

## Tests

```yaml
gates:
  constructor_unit_tests: 1653
```

| Gate | Result | Time (deadline) |
|---|---|---|
| build (`dotnet build Berreman.slnx -c Release -nologo -v:m`, from `Berreman/`) | exit 0, `0 Error(s)`. The only warnings are the exempt NU1902 advisories and vendored MathNet's `SYSLIB0051`, which steps 0360–0377 also show | 78 s (600 s) |
| constructor-unit-tests (the gate's own command, whole suite) | `Test run summary: Passed!`, 1653 succeeded (1652 at checkpoint 381, +1), 0 failed, 0 skipped. In the TRX, the new fact and the three widened facts are `Passed` | 73 s (180 s) |
| elevation (`ASG_ARC_BASE_COMMIT` = arc base `f395597c`, ELEV004 excluded) | `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`, exit 0. Suppressed findings are 1539, as at step 0381 | 215 s (600 s) |
| the twenty s38 invariants (each decoded `git grep`) | all twenty find nothing | seconds each (300 s) |

These are development runs; the supervisor's gate engine is the authority. Every run is over the final source, and
every duration is within its deadline.

The facts this step creates or widens:

- **created:** `StudyTests.a fit start count refuses fewer than one start, and a study's starts are set in place`;
- **widened:**
  - `KitContentFileJsonTests.a study file round-trips through its canonical text`;
  - `KitContentFileJsonTests.a schema-invalid or malformed study file is refused before it binds`;
  - `StudySessionTests.every study error renders its condition and its payload`.

## Architecture

- **`FitStartCount`** (`Studies.fs`, just before `StudyFitSettings`) is a single-case union over `int`, with
  `.value`, `tryCreate` and `single`. It follows `ResidualWeight`'s shape, and its refusal names the count
  (rendered invariantly).
- **`StudyFitSettings.starts`** is the record's last field.
  - `Study.withFitStarts` is an intrinsic member beside `withFitMode`.
  - `InvalidFitStartCountErr` is the last `StudyError` case.
- **Storage adds no converter.** FSharp.SystemTextJson unwraps single-case unions, so `starts` is written and read as
  a plain JSON number. The schema keeps `settings` closed (`additionalProperties: false`) and requires all five
  fields.

## Deferred

Nothing.

## Gotchas

- **The authorized edit list is not where the shared study fixture lives.**
  - The list names `KitStudyFileTests.fs`, but the shared QZ-LR fixture (`StudyFileFixtures`) lives in
    `KitLibraryTests.fs`, which compiles first.
  - Its `StudyFitSettings` record expression gained `starts = FitStartCount.single`, the one line how_to (3) requires;
    without it the test project does not compile. `KitStudyFileTests.fs` spells out no `StudyFitSettings` and needed
    no edit.
  - The deviation is recorded in the impl-log.
- **The round-trip fact uses three starts; the shared fixture keeps one.** The fact builds its study as
  `qzLrStudy.withFitStarts` of 3. The fixture stays at one start, as how_to (3) says, so the facts that share it are
  unchanged.
- **A study file without `settings.starts` is refused by its schema.** There is no up-converter (.spec-md §4). No
  committed `.study-json` exists yet; the S12 reference kits are written after this step.

## Changelog

- 2026-11-13 — Step 0382: `FitStartCount` and `StudyFitSettings.starts` (default one start), `Study.withFitStarts`,
  `InvalidFitStartCountErr`, and the study schema's required `settings.starts` (≥ 1). One new fact and three widened
  facts; constructor suite 1653.
