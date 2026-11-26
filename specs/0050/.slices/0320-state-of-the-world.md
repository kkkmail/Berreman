# Step 0320 — state of the world

## Where we are

This step is in spec 0050 section S9.3: one file serving many experiments (001 §K).

- Step 317 split mapped points into keyed groups.
- Step 318 matched each group through the kit's naming template.
- Step 319 gave the window `SplitByColumn`, `ToggleSplitOffer` and `CancelDataSplit`, which identify the groups and
  write nothing.
- This step adds `MatchByTemplate` (001 §P), the confirm half of .spec-md §10.3. It attaches the file once, gives each
  matched experiment its key's rows, and creates each ticked offer's setup and experiment.
- The panel that shows the split and sends these messages is step 324's.

## What's working

- Add `MatchByTemplate`, which applies the held split behind step 185's read-only guard.
  - It attaches the picked file once, copied into `data/`.
  - Each matched experiment gets `KeyedDataRows key` through the chosen profile.
  - Each ticked offer records its setup when the kit lacks it, then a new experiment of the draft's scan with its rows.
- Writes happen in group order through `writtenInOrder`, as in `GenerateSetupGrid`, and the first failure stops the run.
  - Each experiment file is written once, with its badge from the one validation.
  - The table, the active setup, the draft and the review are left unchanged.
- A ticked offer without a draft scan is refused with `Not split — …`. A missing split changes nothing. A failed attach
  answers `Not attached — …`.
- Tests: three new `DataSplitModelTests` facts (1545 → 1548). The read-only `KitWindowModelTests` fact now also refuses
  `MatchByTemplate`.

## Tests

These were development runs of each gate's own command over the final tree. The arc-runner's gate engine runs the
gates themselves.

| Gate | Result | Time | Deadline |
|---|---|---|---|
| build | exit 0, 0 warnings, 0 errors; no lowercase `error` in stdout | 38 s | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`; known findings 0; ELEV004 excluded (suppressed 1434) | 206 s | 600 s |
| constructor-unit-tests | Passed!, 1548 of 1548, 0 failed, 0 skipped | 73 s | 180 s |
| ui-smoke | Passed!, 253 of 253 | 221 s | 600 s |
| ui-tests (`InverseConstructorTests`, `KitWindowTests`) | Passed!, 30 of 30 | 28 s | 360 s |
| invariant-s31-\* (16) | each `INVARIANT HOLDS` (`.artifacts/0320-dev-invariants.log`) | under 1 s each | 300 s |

- **constructor-unit-tests: 1548**, which is 1545 + 3, as the slice requires.
  - The TRX (`.artifacts/0320-dev-trx/constructor-unit-tests.trx`) shows these as `Passed`:
    - `applying a split attaches the file once and gives each matched experiment the rows of its key, and an unticked offer creates nothing`;
    - `a ticked offer records its setup when the kit lacks it and a new experiment of the draft's scan with its rows`;
    - `a split with a ticked offer and no draft scan, or with no split, writes nothing`;
    - the changed `a kit opened read-only refuses every write message and writes nothing`.
- **ui-smoke: 253**, equal to its checkpoint. **ui-tests: 30** over the two selected classes, unchanged, since no UI
  test changed.
- **Mutations** (`.artifacts/0320-dev-mutation.log`):
  - never writing the offer's setup fails the ticked-offer fact;
  - dropping the no-draft refusal fails the no-draft-scan fact;
  - dropping `guardKitWrite` fails the read-only fact.

  A fourth mutation survived because it hit dead code (a draft restore in the planner), so that code was deleted. The
  view was restored byte-identical after each mutation, and the final tree was rebuilt and re-tested.
- No run exceeded its effective deadline. Every touched file is LF only, and `git diff --numstat` equals the
  `--ignore-cr-at-eol` count.

```yaml
gates:
  constructor_unit_tests: 1548
  ui_smoke_tests: 253
```

## Architecture

- **Plan purely, then write through the existing seam.**
  - `splitWritesOver` folds the groups in order into a private `SplitWrite` list (`SplitSetupWrite | SplitExperimentWrite`)
    over the one attachment.
  - `writtenInOrder` (step 276) records each write through `KitLibraryContext.recordSetup` / `recordExperiment`.
  - `withSplitWriteHeld` gives the model as it stands after each write.
  - No new message, model field or option was added beyond `MatchByTemplate`.
- **A new experiment is built as `RecordExperiment` builds one** (step 278): `chooseMeasurement capture`, then
  `commitOverSetups [ { setup = offer.setup.reference; name = offer.experiment } ]`. It takes
  `Experiments.attachDataFile` of the keyed attachment before it is written.
  - `withSplitWriteHeld` puts the experiment into the model's own collection, appended with `nextId` past it.
  - So the draft, including its `editingId`, is never touched.
- **A new setup joins `kitSetups` through `withSetupRecorded`**, in name order, with `activeSetupOpt` put back, as
  `GenerateSetupGrid` does. "The kit lacks it" is equality against `kitSetups` plus the setups planned earlier in this
  run.
- **The status counts come from the writes.** `created` is `appendedOver` the model's collection, `matched` is the
  experiment writes minus `created`, and the groups left unattached are the groups minus the experiment writes.

## Deferred

Nothing. Every requirement of the slice landed. The split panel is step 324's by the spec's plan.

## Gotchas

- **A failed write keeps the split held.** The slice clears `dataSplitOpt` only on success, and every other refusal
  keeps the split, so a partly written run answers the model as written with `Not saved to the kit — …`. Re-splitting
  matches anything already created by name.
- **Ordinal path order puts upper case first.** In `addedFiles`, `LP1-…` sorts before `data\…`.
- **`ui-tests` without the gate's two `--filter-not-trait` options counts 43, not 30.** Use the gate's exact command.
- **The first build of the round recompiled the vendored MathNet** and printed two `SYSLIB0051` warnings from
  `MathNetNumerics/Numerics/Exceptions.cs`, which this step did not touch. Every later build, the gate-form build
  included, printed 0 warnings.
- `DataSplitModelTests` still copies `SetupModelTests`' private fixture with its own GUIDs (step 319's note). Keep the
  two in step if step 272's table changes.

## Changelog

- 2026-11-12 — Step 0320: added `MatchByTemplate`. It applies the held split: one attach, matched experiments keyed to
  their rows, and ticked offers' setups and new experiments of the draft's scan, all written in group order, with the
  first failure stopping the run. Three `DataSplitModelTests` facts were added (1545 → 1548), and the read-only fact was
  extended.
