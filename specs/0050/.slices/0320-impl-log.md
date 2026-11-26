# Step 0320 — impl log

## Progress

- [x] Read the task, both system prompts, the project prompt, the slice, and step 319's code and SoW. The task carries no
  operator note and no ruling for this step.
- [x] `MatchByTemplate` in `TableAndElementRotationView.fs`: the message, the private `SplitWrite` DU, `splitWritesOver`
  (the pure plan), `withSplitWriteHeld` (the model after each write) and the handler.
- [x] Three `DataSplitModelTests` facts; the read-only fact in `KitWindowModelTests` also sends `MatchByTemplate`.
- [x] Mutation checks (`.artifacts/0320-dev-mutation.log`).
- [x] Development runs: build, elevation, constructor-unit-tests, ui-smoke, ui-tests and the sixteen s31 invariants.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/TableAndElementRotationView.fs`
  - `TableAndElementRotationMsg.MatchByTemplate`, with a one-line doc citing spec 0050 S9.3 and 001 §P.
  - `type private SplitWrite = SplitSetupWrite | SplitExperimentWrite`, with `setupOpt` and `experimentOpt` members.
  - `splitWritesOver`: plans the kit writes group by group, in order, over the one attachment.
  - `withSplitWriteHeld`: the model once a write is recorded.
  - The `MatchByTemplate` arm: `guardKitWrite`, then no split, then the refusal, then the attach, then `writtenInOrder`,
    then the status.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/DataSplitModelTests.fs`
  - Opens `ExperimentDataLoad`, and the module doc now names `MatchByTemplate`.
  - Helpers `copiedAttachment`, `keyedBy` and `addedFiles`.
  - Three new facts, named exactly as the slice's effects name them.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitWindowModelTests.fs`
  - `a kit opened read-only refuses every write message and writes nothing` also sends `MatchByTemplate`, under the
    same assertions: collection, setups, kit and `dataSplitOpt` unchanged, the read-only status, and the folder's bytes
    unchanged.

## Decisions

- **Guard first.** The guard runs before the no-split check, so a read-only kit answers with its read-only status even
  with no split held. The read-only fact sends `MatchByTemplate` over a model with no split, so that status proves the
  guard runs first; the dropped-guard mutation fails it.
- **"A draft that cannot record"** means `not canCommit` or `draftCapture = None`. These are exactly `RecordExperiment`'s
  two refusals (step 278). The capture is read only when `canCommit` holds, and the refusal applies only while
  `creating` is not empty. With nothing ticked, a blank draft still attaches the matched groups ("or tick no offer").
- **The writes are planned purely, then written by the existing `writtenInOrder`** (`GenerateSetupGrid`'s, step 276),
  which stops at the first failure.
  - The setup and experiment writes interleave in group order: an offer's setup, then its experiment.
  - A matched experiment and a created one each take the keyed attachment before they are written, so each experiment
    file is written once.
  - A new experiment comes from `chooseMeasurement capture` then `commitOverSetups [ target ]`, as `RecordExperiment`
    builds one, so it is the draft's scan over `offer.setup.reference`, named `offer.experiment`.
- **The draft is kept** because `withSplitWriteHeld` puts each experiment into the model's own collection: in its id's
  place, or appended with `nextId` past it. The planning collection's draft never reaches the model.
- **Setups join `kitSetups` through `withSetupRecorded`** (ordinal name order), with `activeSetupOpt` put back, as
  `GenerateSetupGrid`'s `setupHeld` does. "The kit lacks it" means `kitSetups` and the earlier planned setups do not hold
  it by equality, so two offers of one new setup write it once.
- **Each badge** comes from `checkExperimentData` over the model as written so far. An offer's setup is written before
  its experiment, so the badge validates against that setup.
- **The status counts come from the writes.**
  - `a` is the experiment writes.
  - `c` is `appendedOver` the model's collection, and `m = a - c`.
  - `s` is the setup writes.
  - `u` is the groups minus `a`, which is the unticked offers plus the unmatched groups.
- **On a failed write the split stays held.** The slice sets `dataSplitOpt = None` only on success, and every other
  refusal (Not split, Not attached, a read-only kit) keeps the held split. The failure answers the model as written so
  far, with `Not saved to the kit — <describe>`.

## Testing state

Development runs of each gate's own command over the final tree (the gate engine runs the gates themselves):

| Gate | Result | Time | Deadline |
|---|---|---|---|
| build | exit 0, 0 warnings, 0 errors; no lowercase `error` in stdout | 38 s | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`; known findings 0 | 206 s | 600 s |
| constructor-unit-tests | Passed!, 1548 of 1548 (1545 + 3) | 73 s | 180 s |
| ui-smoke | Passed!, 253 of 253 | 221 s | 600 s |
| ui-tests (`InverseConstructorTests`, `KitWindowTests`) | Passed!, 30 of 30, unchanged | 28 s | 360 s |
| invariant-s31-\* (16) | each `INVARIANT HOLDS` | under 1 s each | 300 s |

**Mutations** (`.artifacts/0320-dev-mutation.log`), each over a rebuilt tree, with the view file restored byte-identical:

- the held setup never written: the ticked-offer fact fails;
- no refusal without a draft scan: the no-draft-scan fact fails;
- `guardKitWrite` dropped: the read-only fact fails;
- the planning collection's draft not restored: survived. The restore was dead code, because the model's draft never
  comes from the planning collection, so it was deleted. The ticked-offer fact still asserts the draft unchanged.

## Artifacts

- `.artifacts/0320-dev-build.log`
- `.artifacts/0320-dev-elevation.log`
- `.artifacts/0320-dev-constructor-unit-tests.log`
- `.artifacts/0320-dev-ui-smoke.log`
- `.artifacts/0320-dev-ui-tests.log`
- `.artifacts/0320-dev-invariants.log`
- `.artifacts/0320-dev-mutation.log`
- `.artifacts/0320-dev-trx/` (`constructor-unit-tests.trx`, `ui-tests.trx`, `ui-smoke.trx`)

## Gotchas

- **Ordinal file order puts upper case first**: `LP1-…` sorts before `data\…`. The `addedFiles` expectation follows
  that order.
- **Running `ui-tests` without the gate's two `--filter-not-trait` options counts 43**, not 30. Always use the gate's
  exact command.
- **The first build of this round recompiled the vendored MathNet** and printed two `SYSLIB0051` warnings from
  `MathNetNumerics/Numerics/Exceptions.cs`. That code is untouched by this step. The gate-form rebuild, and every build
  after it, printed 0 warnings.
