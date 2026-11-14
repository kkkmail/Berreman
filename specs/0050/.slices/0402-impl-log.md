# Step 0402 — implementation log

## Progress

- [x] Read the task, both system prompts, the project prompt, the slice, ruling 013 (arc-origin elevation, binding from
  0160 on) and step 401's state of the world.
- [x] `StudyEngine.fs`: `engineProblemsWith`, `sigmasFor`, `SuspectCoordinate` / `suspectsWith` and
  `realizabilityWith`, in that order, after `calibrationProblemsWith` and before `StudyFitRequest`.
- [x] `StudyEngineTests.fs`: a new S11.4 section ("sigmas, realizability and suspects") holding the four facts.
  `sessionFixture` now takes QZ's truth, and its four callers pass `qzTruth`.
- [x] Development build: exit 0, 47 s, only the exempt `NU1902` advisory.
- [x] The four new facts: 4/4 passed (11 s).
- [x] Mutation check: each of four planted defects turned exactly its own fact red (see Testing state).
- [x] Pristine `StudyEngine.fs` restored (`cmp` identical) and the solution rebuilt from it before the final runs.
- [x] Whole constructor suite, selected unit-tests classes, elevation, the twenty s39 invariants: all green.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Optimization/StudyEngine.fs` (+166 lines, nothing removed).
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/StudyEngineTests.fs`: +260 / −9. The 9 changed lines are the
  `sessionFixture` doc, signature and truth line, and its four call sites.

## Decisions

- **`engineProblemsWith`** matches on `(solution.engine, this.stage)` with the same three arms as `runFit`'s engine
  dispatch: nonlinear, linear under `CalibrationStage`, linear under `ObjectStage | JointStage`.
- **`sigmasFor`** computes every problem's `entrySigmas` once. Each layout entry's `FitEntryRef` (its parameter and
  owner) is looked up with `List.tryPick`, so the first problem naming it wins. An entry no problem names, or one held
  under another owner, gets `None`.
- **`suspectsWith`** walks each problem in order and each `EntryCoordinate` k in order.
  - `quality` is read once per problem, `sigmaAt k` once per coordinate.
  - Each entry's reasons come in the slice's order: `UndeterminedSigmaReason`, then `WideSigmaReason` (σ over its
    layout entry's `unknown.unknown.bounds` span), then one `CorrelatedEntryReason` per other entry coordinate l (named
    by its first entry, with ρₖₗ), in coordinate order.
  - An entry whose layout holds no matching entry has no bounds, so it gets no `WideSigmaReason`.
  - Visibility and element coordinates are neither suspects nor partners. An `EntryCoordinate []` is no partner.
- **`realizabilityWith`**:
  - The black boxes come from `Experiment.itemsUsedBy` over the fitted runs, so they keep the staged items' order.
  - "The first fitted run that uses it" is the first fitted run whose own `itemsUsedBy [ run ]` holds the item.
  - Its values: `layout.assignmentFor this.study this.items run fitted`, then `valuesOf`.
  - The operating wavelength is the run's `boundWith … None` `operatingWaveLength`: step 367's rule as the forward
    applies it, called rather than re-composed. With `None` the detector reads its own dark offset, which never touches
    the wavelength.
  - The matrix is `BoundOpticalForm (form, values)` built directly, as the slice says. This skips `ofOpticalForm`'s
    range check, which never refuses a Mueller element (`MuellerElementParameter` is unbounded). It is evaluated at
    `ElementOrientation.transmittingAt Angle.zero` (rotation zero, normal incidence, transmitted).
  - Errors: a setup error is `StudyForwardFitErr (ForwardSetupErr _)`, as `runFit` maps `itemsUsedBy`'s. A forward
    error is `StudyForwardFitErr`, and an evaluation error goes through the shared `evaluatedForFit`.
- **Test fixtures.** Step 397's two least-squares problems are private to `FitResultsTests`, which this step may not
  edit. They are rebuilt test-locally (`independentRows`, `proportionalRows`, `solvedOver`) over the test-local entries
  x0, x1 (full rank) and y0, y1 (rank-deficient), each set its own, so each suspect's problem is unambiguous.
- **`sessionFixture` takes its truth** (`sessionFixture qzTruth` at all four former call sites), so the realizability
  fact can generate QZ's rows from diag(1, 1.5, 0, 0). Every existing assertion is unchanged.
- **Unknown entries in the sigma fact.** The fact adds two entries no problem holds: the Phone's gain, and QZ's m00
  under `SessionOwner day1`. The second shows that matching includes the owner.
- **Coordinate order in the sigma fact.** The fact first asserts that the σ of vec coordinates 1 (m10) and 4 (m01)
  differ. That makes its row-major-versus-vec check discriminating.

## Testing state

These are development runs on 2026-11-14, not gate verdicts; the supervisor re-runs every gate. Each finished within its
effective deadline.

| Gate | Development outcome | Duration / deadline |
|---|---|---|
| build | `dotnet build Berreman.slnx -c Release`: exit 0, 0 errors, only the exempt `NU1902` advisory | 47 s (full), 10 s (rebuild after the mutation check) / 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)` (no base, so all 13; known findings 0) | 219 s / 600 s |
| constructor-unit-tests | whole suite, `Test run summary: Passed!`, 1720/1720 (1716 + 4) | 137 s / 180 s |
| unit-tests (selected `MuellerReconstructionTests`, `MuellerInverseTests`) | `Passed!`, 45/45 (25 + 20) | 673 s / 7200 s |
| invariant-s39-* (20) | each gate's own command: `INVARIANT HOLDS ... found nothing`, exit 0 | < 1 s each / 300 s |

Elevation ran at the same time as the unit-tests run, so its 219 s is under load (step 401 measured 216 s alone).

New facts in the whole-suite TRX:

| Fact | Duration |
|---|---|
| `an entry is suspect when its sigma is undetermined, wide against its bounds, or correlated past the threshold` | 0.01 s |
| `every entry takes the sigma of the coordinate that holds it` | 2.41 s |
| `every black-box object of a fit is checked for realizability at its fitted values` | 0.17 s |
| `each engine's results are read off the solves it made` | 6.04 s (one nonlinear fit) |

StudyEngineTests now sums to 134 s over 33 facts, the suite's critical path.

### Mutation check

Each mutant was built and run over the four new facts. Logs are in `.artifacts/0402-mutation/`.

| Mutant | Defect planted | Result |
|---|---|---|
| `wide` | `suspectsWith` never gives `WideSigmaReason` | only the suspect fact failed (3 passed) |
| `sigmaorder` | `sigmasFor` takes the k-th entry σ of the first problem (coordinate order) instead of the entry it names | only the sigma fact failed |
| `start` | `realizabilityWith` reads the layout's start vector instead of the fitted one | only the realizability fact failed |
| `dispatch` | `engineProblemsWith` sends every linear solution to `objectProblemsWith` | only the engine fact failed |

The file was restored (`cmp` identical) and the solution rebuilt before the final runs above.

Line endings: a byte count shows both edited sources and the three slice outputs are LF only (no CR byte). `git diff
--numstat` equals `--ignore-cr-at-eol` (166/0 and 260/9).

## Artifacts

- `specs/0050/.artifacts/0402-dev-build.log`, `0402-dev-build-final.log`: the development solution builds.
- `specs/0050/.artifacts/0402-dev-constructor-unit-tests.log`, `0402-dev-unit-tests.log`, `0402-dev-elevation.log`:
  the development test and analyzer runs.
- `specs/0050/.artifacts/0402-mutation/`: the mutation script, the pristine `StudyEngine.fs`, and each mutant's build
  and test logs.

## Gotchas

- `SuspectCoordinate` adds the record labels `problem`, `coordinate`, `entry` and `reasons`. `entry` and `reasons` are
  also `ParameterProfile`'s and `FitEntrySigma`'s, so every new record construction and lambda over them is annotated.
- The last fact's nonlinear stage is named `plateStaged`, so it does not shadow the module's quarter-wave `plate` item.
