# Step 0313 — state of the world

## Where we are

Spec 0050 section S9.3 covers one file serving many experiments, exclusions, corrections, the Data step and the
experiment matrix (001 §K). S9.2 finished at step 312 with the import wizard composed into the App. This step gives
the experiment file its two audit records:

- a mapped attachment's **point exclusions**, each with its reason;
- an experiment's **setup corrections**, each with its mandatory note.

Lab files stay read-only; both records live in the experiment file. Steps 315 and 316 add the messages that set them.

## What's working

- Add `MappedPointNumber`, `ExclusionReason` and `PointExclusion`. `MappedAttachment` now keeps its exclusions in
  point order, one per point, through `withExclusion`, `withoutExclusion` and `tryFindExclusion` (S9.3).
- Add `CorrectionNote` and `SetupCorrection`. `Experiment.corrections` is oldest first; `commit` starts a new
  experiment with none, and an edit keeps them (S9.3).
- The experiment schema requires `exclusions` (point ≥ 1, non-blank reason) and `corrections` (`previous`,
  `corrected` and a non-blank `note`). The setup reference is one local `$defs` entry.
- A kit copy keeps each experiment's exclusions and corrections as written. A new data attachment starts with no
  exclusion.
- Every fixture building either record sets the new field to `[]`.

## Tests

These were development runs of each gate's own command over the final tree. The arc-runner's gate engine runs the
gates themselves.

| Gate | Result | Time | Deadline |
|---|---|---|---|
| build | exit 0, 0 warnings, 0 errors (incremental); the full build's only warnings are the 2 pre-existing vendored MathNet SYSLIB0051 | 78 s full, 22 s incremental | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`; known findings 0; ELEV004 excluded (suppressed 1428, unchanged) | 200 s | 600 s |
| constructor-unit-tests | Passed!, 1527 of 1527, 0 failed, 0 skipped | 70 s | 180 s |
| ui-smoke | Passed!, 253 of 253 | 217 s | 600 s |
| ui-tests (5 selected classes) | Passed!, 97 of 97 | 41 s | 360 s |
| invariant-s31-\* (16) | each `INVARIANT HOLDS` | under 1 s each | 300 s |

- **constructor-unit-tests: 1527**, which is 1525 + 2, as the slice says. The TRX shows all five named facts as
  `Passed`:
  - new: `a point exclusion needs a point from 1 and a reason, and an attachment keeps one exclusion per point in
    point order`;
  - new: `a correction note refuses a blank text, and a committed experiment starts with no correction and keeps its
    corrections through an edit`;
  - changed: `an experiment file round-trips through its canonical text`;
  - changed: `a schema-invalid or malformed experiment file is refused before it binds`;
  - changed: `a copy keeps each copied experiment's import profile and row selection`.
- **ui-tests: 97** over `ExperimentControlsTests`, `InverseConstructorTests`, `KitWindowTests`, `ScrollExtentTests`
  and `SolverHandoffWindowTests`. No fact was added to or removed from them, so their count is unchanged.
- **ui-smoke: 253**, equal to the baseline.
- **The schema refusals are not vacuous.** With the build-copied schema loosened, the refusal fact failed. This
  happened both with every new constraint dropped and with only the point, reason and note checks dropped. The copy was
  restored byte-identical to the source schema, and the class passed again (14 of 14).
- **Red by construction.** The two new facts name types and members that did not exist before this step.
- No run exceeded its effective deadline.
- Every touched file is LF only (0 CR bytes), and `git diff --numstat` equals the `--ignore-cr-at-eol` count.

```yaml
gates:
  constructor_unit_tests: 1527
  ui_smoke_tests: 253
  ui_tests: 97
```

## Architecture

- **An exclusion belongs to the mapped attachment, not the experiment.** It numbers that attachment's points, which
  are the rows its selection keeps, in file order. Attaching a file anew builds a new attachment with no exclusion.
- **Point order sorts on the elevated `MappedPointNumber`** through structural comparison. The members never read
  `.value`.
- **A correction is an audit record.** It holds the previous and corrected setup references and a non-blank note.
  The experiment's own `setup` remains the current one. `commit`'s edit arm copies with `{ e with … }`, so no edit
  drops the trail.
- **No closed set was added.** None of the new types has an int tag.
- **The schema version stays 1** (.spec-md §7): no kit of the old shape exists outside test output.
- **The setup-reference shape is stated once** in the schema's local `$defs/setupReference`, which `setup`,
  `previous` and `corrected` reference, as `kit.schema.json` does for its column numbers.
- **Validation lives at the schema boundary.** The serializer binds whatever validates, so the schema is what refuses
  a point below 1, a blank reason, a blank note or a missing list before binding. In code, the `tryCreate`s refuse the
  same values.

## Deferred

Nothing. Every requirement of the slice landed this round. The messages that set exclusions and corrections are steps
315 and 316, which are later slices and not deferrals.

## Gotchas

- **Four unlisted fixtures were edited.** `KitOpeningTests`, `KitStartTests`, `KitStartWindowTests` and
  `LauncherTests` each build an `Experiment` literal, so they need `corrections = []` to compile.
  - The slice's rule (6) covers every such fixture, but its file list misses these four.
  - Steps 261, 288 and 291 repaired the same four files the same way.
- **`chartForRun` gets one compile repair.** It builds a throwaway `Experiment` for its description and now sets
  `corrections = []`. Its behaviour is unchanged.
- **A copy keeps corrections as written.** It copies only the setup an experiment currently references.
  - A correction's other setup is not copied.
  - A renamed clashing setup is not renamed inside the corrections.
  - This is what the slice prescribes.
- **FSharp.SystemTextJson names the wrong field for a missing list.** Without the schema's `corrections` requirement,
  a file without `corrections` fails binding as `Missing field … sessionOpt`. The schema requirement is what gives
  the typed `SchemaValidationErr`.
- The `e1Corrected` fixture's `setup` is its correction's `corrected` reference, so the fixture is coherent.
- `specs/0050/.manifest.state.json` was already dirty from the supervisor when the round began. This round did not
  touch it.

## Changelog

- 2026-11-11 — Step 0313: the experiment file records point exclusions and setup corrections (S9.3).
  - Domain: `MappedPointNumber`, `ExclusionReason`, `PointExclusion`, `MappedAttachment.exclusions` and its three
    members; `CorrectionNote`, `SetupCorrection` and `Experiment.corrections`; `commit` appends with none.
  - Storage: the experiment schema requires both lists; one `$defs/setupReference`; the copy's doc cites S9.3.
  - Ui: `AttachDataFile` starts with no exclusion.
  - Tests: two new facts and three extended ones; every fixture literal of either record sets the new field.
