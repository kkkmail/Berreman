# Step 0382 — impl log

## Progress

- [x] Domain: `FitStartCount`, `StudyFitSettings.starts`, `Study.withFitStarts`, `InvalidFitStartCountErr` (2026-11-13)
- [x] Storage: `study.schema.json` `settings.starts` (2026-11-13)
- [x] Tests: record expressions, new `StudyTests` fact, widened JSON and error-render facts (2026-11-13)
- [x] Development runs: build, constructor-unit-tests, elevation, the twenty s38 invariants (2026-11-13)

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/Studies.fs`
  - `StudyError` gains `InvalidFitStartCountErr of reason : string`, last. It renders as `the fit start count is not
    valid: <reason>`.
  - Before `StudyFitSettings`: `FitStartCount of int`, with:
    - `.value`;
    - `tryCreate (count : int) : Result<FitStartCount, StudyError>`, which refuses a count below 1 with
      `InvalidFitStartCountErr "<count> is fewer than one start"`, the count rendered invariantly;
    - `single` (1).
  - `StudyFitSettings` gains `starts : FitStartCount` as its last field; `initial` holds `FitStartCount.single`.
  - `Study.withFitStarts (starts : FitStartCount) : Study` sets the field in place. It is an intrinsic member, right
    after `withFitMode`.
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/schema/study.schema.json`: under version 1, `settings`
  requires `starts`, `{ "type": "integer", "minimum": 1 }`. `additionalProperties: false` is kept, so `settings`
  holds exactly `mode`, `experimentWeights`, `detectorWeights`, `disabled` and `starts`. No converter is added:
  FSharp.SystemTextJson's `WithUnionUnwrapSingleCaseUnions` writes the count as its number.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/StudyTests.fs`:
  - the settings fact's `expectedSettings` gains `starts = FitStartCount.single`;
  - new fact `a fit start count refuses fewer than one start, and a study's starts are set in place`;
  - the module doc names the start count (S11.3).
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitContentFileJsonTests.fs`:
  - `a study file round-trips through its canonical text` round-trips `qzLrStudy.withFitStarts` of 3 starts. It reads
    the 3 starts back and checks that `study.settings.starts` is written as the number 3;
  - `a schema-invalid or malformed study file is refused before it binds` also refuses `settings.starts` 0 and a
    `settings` without `starts`, each with `SchemaValidationErr`.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/StudySessionTests.fs`: `every study error renders its
  condition and its payload` renders `InvalidFitStartCountErr "0 is fewer than one start"` as `rendered.[19]`.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitLibraryTests.fs`: the `StudyFileFixtures` QZ-LR study's
  `StudyFitSettings` record expression gains `starts = FitStartCount.single`. Nothing else changes. See Decisions.

## Decisions

- **`KitLibraryTests.fs` is edited, though the effects' edit list does not name it.**
  - **Why.** The shared `StudyFileFixtures` module (QZ-LR) lives in that file ("because this file compiles first
    among those facts"). Its `settings` value spells out a `StudyFitSettings` record. how_to (3) requires exactly
    this edit: "Every record expression that spells out a `StudyFitSettings` in the tests gains `starts =
    FitStartCount.single`, and nothing else changes". Without it the test project does not compile.
  - **What changed.** One line. No test identity is added, renamed or changed. The fixture keeps one start, as how_to
    (3) asks, so every fact that reads it is unchanged.
  - **What the list names instead.** It names `KitStudyFileTests.fs`, which only consumes the fixture and spells out
    no `StudyFitSettings`, so that file needed no change.
- **The round-trip's three starts come from `withFitStarts` in the fact, not from the shared fixture.** how_to (3)
  keeps the fixture at `FitStartCount.single`, and `qzLrStudy` also seeds the schema-refusal edits. The fact builds
  its study as `qzLrStudy.withFitStarts threeStarts`, with `threeStarts` from `FitStartCount.tryCreate 3`, and
  asserts it reads back.
- **`InvalidFitStartCountErr` is appended last in `StudyError`.** The error-render fact indexes its rendered list in
  case order, so appending leaves indices 0–18 as they are and adds `[19]`.
- **`withFitStarts` is an intrinsic member beside `withFitMode`, not a `type Study with` block.** how_to writes
  `type Study with member this.withFitStarts …` and also places it "beside `withFitMode` (step 365)", which is
  intrinsic to `Study`. `FitStartCount` is declared before `Study`, so the intrinsic form compiles. The contract
  symbol `Studies.Study.withFitStarts` is the same either way.
- **`tryCreate (count : int)` is ELEV-clean.** It is the type's own primitive on an elevation-pattern member, as
  `ResidualWeight.tryCreate (value : float)` already is. The elevation run confirmed 0 findings.

## Testing state

Development runs on 2026-11-13. These are the worker's checks; the arc-runner's gate engine is the authority.

| Run | Outcome | Time (deadline) |
|---|---|---|
| build (`dotnet build Berreman.slnx -c Release -nologo -v:m`) | exit 0, 0 errors | 78 s (600 s) |
| constructor-unit-tests (whole) | `Passed!`, succeeded 1653 of 1653, 0 skipped | 73 s (180 s) |
| elevation (`ASG_ARC_BASE_COMMIT` = `.arc-base-json` origin, `-ExcludeRules ELEV004`) | `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)` | 215 s (600 s) |
| twenty s38 invariants (each decoded `git grep`) | all twenty find nothing | < 5 s (300 s each) |

- **constructor-unit-tests count.** 1652 (step 0381's checkpoint) → 1653: the new `StudyTests` fact, +1 as the
  acceptance requires.
- **The named facts in the TRX.** All four passed:
  - `StudyTests.a fit start count refuses fewer than one start, and a study's starts are set in place`;
  - `KitContentFileJsonTests.a study file round-trips through its canonical text`;
  - `KitContentFileJsonTests.a schema-invalid or malformed study file is refused before it binds`;
  - `StudySessionTests.every study error renders its condition and its payload`.
- **Build warnings.**
  - 12 × NU1902 (exempt).
  - 4 × SYSLIB0051 from the vendored `MathNetNumerics/Numerics/Exceptions.cs`. That file was last changed in 2022;
    the warning appears whenever the project recompiles, as in steps 0360–0377's build logs. It is not from this
    step's code.
- No run exceeded its deadline.

## Artifacts

- `specs/0050/.artifacts/0382-dev-build.log`
- `specs/0050/.artifacts/0382-dev-constructor-unit-tests.log`
- `specs/0050/.artifacts/0382-dev-elevation.log`
- `specs/0050/.artifacts/0382-dev-invariants.log`

## Gotchas

- **The authorized edit list is not where the shared study fixture lives.** It names `KitStudyFileTests.fs`, but
  `StudyFileFixtures` is in `KitLibraryTests.fs`. A later step that changes the `StudyFitSettings` or `Study` shape
  will hit the same thing.
- **A study file without `settings.starts` is now refused by its schema (`SchemaValidationErr`).** There is no
  up-converter, per .spec-md §4. No committed `.study-json` exists yet (S12 writes the reference kits after this step).
- **The elevation run analyzes all 13 projects.** `Berreman.slnx` changed since the arc origin, so the gate widens
  to every project. It took about 215 s.
