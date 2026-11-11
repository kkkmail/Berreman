# Step 0276 — state of the world

## Where we are

Spec 0050 section S8.3 covers the naming template and the grid generator. Steps 268–273 added the template, the
setup names it writes, and the kit bar's template field. Step 274 added the pure grid generator (`SetupGrids`), and
step 275 added the experiments half (`Experiments.commitOverSetups`). This step adds the host. The semantic message
`GenerateSetupGrid` records every ticked combination as a setup of the kit, named by the kit's template, and records
an experiment of the draft over each, all at once (001 §J, §P). It also adds the naming-template gotcha to AGENTS.md.
The grid's controls (`SetupGridGenerateButton`) come in S8.4.

## What's working

- Add `GenerateSetupGrid`, which records every ticked setup the kit lacks, named by the kit's template
- It commits a draft that can be committed over every grid setup and records each experiment as `ExpCommit` does
- An impossible grid is refused (`Not generated — …`) and a read-only kit is refused, both writing nothing
- The first failed write stops the run and keeps what was written; the table, active setup and draft's choices stay as they were
- Add the AGENTS.md gotcha on naming-template placeholders, `setupNameAmong`, `NamingTemplate.standard` and `SetupGrids.fs`
- Add four `SetupModelTests` facts over step 272's table

## Tests

Every gate ran locally on the final tree, using the snapshot's own commands. None went over its effective deadline.

- build: pass, 73 s (deadline 600 s). No warning came from our code; the only warnings are the vendored MathNet
  `SYSLIB0051` ones that were already there.
- constructor-unit-tests: pass, 1457 of 1457, in 71 s (deadline 180 s). The baseline was 1453 at checkpoint 274, so
  the count rose by 4. All four new facts show `Passed` in the TRX:
  - `GenerateSetupGrid records a setup per ticked combination, named by the kit's template, and an experiment of the
    draft over each`
  - `a grid over setups the kit holds writes no setup again, and a grid it cannot make writes nothing`
  - `a grid over a draft that cannot be committed records its setups and no experiment`
  - `a kit opened read-only generates nothing`
- ui-smoke: pass, 245 of 245, in 221 s (deadline 600 s). It ran at the same time as elevation, which explains the time.
- ui-tests, selected (`InverseConstructorTests`, `SetupSaveLoadTests`): pass, 18 of 18, in 27 s (deadline 360 s).
- elevation, from the arc origin `f395597…`: `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, 206 s (deadline
  600 s).
- invariant-s27-* (all fifteen): pass.

```yaml
gates:
  constructor_unit_tests: 1457
  ui_smoke_tests: 245
  ui_tests: 18
```

## Architecture

- The arm sits behind `guardKitWrite`. It computes everything first, without writing:
  - the grid's setups (`grid.generate` over the table, the kit's items, `tryItemModelChoice`, the kit's template and
    the held setups);
  - the ones the kit lacks, compared by equality;
  - the committed collection: `chooseMeasurement` of `draftCapture`, then `commitOverSetups` over the setups'
    references in order;
  - the experiments that collection appends.
- One local fold, `writtenInOrder`, then records the new setups (`recordSetup`) and after them the new experiments
  (`recordExperiment`). It carries `Result<model, model * KitLibraryError>`, so the first failure stops the run with
  the model as written so far.
- Each setup joins `kitSetups` through the existing `withSetupRecorded`, which keeps ordinal name order. The active
  setup is put back afterwards. Each experiment extends the collection as `{ committed with experiments = … }`, so
  after a full run the collection is exactly what `commitOverSetups` answered.
- No other message, field or option was added.

## Deferred

Nothing. Every requirement of the slice landed this round.

## Gotchas

- `withSetupRecorded` makes the recorded setup active. The grid arm restores `activeSetupOpt` after each setup,
  because the slice keeps the active setup unchanged.
- Two ticked combinations with equal slots answer the same setup twice. It is written once and counted once, but
  `commitOverSetups` still makes one experiment per combination.
- The capture is the table's `draftCapture`, as `ExpCommit` reads it. The grid only re-picks items and angles, so
  every generated setup has the table's detectors.
- After a run the draft edits nothing, as `commitOverSetups` leaves it, so a later commit appends. A draft that could
  not be committed leaves the collection exactly as it was.
- AGENTS.md says "an empty second or later slot … or an empty note". `writeName` drops an optional placeholder only
  when the reading lacks its value.
- No fact makes a write fail part-way. The census names none, and adding one would need a new test identity.

## Changelog

- 2026-11-10: Step 0276 adds `TableAndElementRotationMsg.GenerateSetupGrid`, which records the grid's setups and an
  experiment of the draft over each, refusing an impossible grid or a read-only kit. It also adds the AGENTS.md
  naming-template gotcha. constructor-unit-tests went from 1453 to 1457.
