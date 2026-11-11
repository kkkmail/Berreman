# Step 0275 — state of the world

## Where we are

Spec 0050 section S8.3 covers the naming template and the grid generator. Step 0274 added the setups half of the
grid (`SetupGrids`: options per slot, their product, the impossible combinations unticked, one named setup per ticked
combination). This step adds the experiments half, `Experiments.commitOverSetups`: the draft is committed once over
each setup reference in order. Step 276 will add the `GenerateSetupGrid` message and the host that records both
halves. S8.4 adds the controls.

## What's working

- Add `Experiments.commitOverSetups`, which commits the draft once over each setup reference in order
- Each new experiment takes the next id and the draft's varied slot, capture, range and wavelength, with no data file
- The answer's draft edits nothing, so a later commit appends; an incomplete draft leaves the collection unchanged
- The body folds `commit`, so there is no second experiment builder
- Add the `ExperimentProxyTests` fact `committing the draft over setups appends one experiment over each, and an incomplete draft appends none`

## Tests

Every gate was run locally on the final tree, and none went over its effective deadline.

- build: pass, 74 s (deadline 600 s). No warning came from our code; the only warnings are the vendored MathNet
  `SYSLIB0051` ones that were already there.
- constructor-unit-tests: pass, 1453 of 1453 succeeded, in 68 s (deadline 180 s). The baseline was 1452 at checkpoint
  273, so the count rose by 1 with `committing the draft over setups appends one experiment over each, and an
  incomplete draft appends none`, which shows `Passed` in the TRX.
- elevation (arc origin `f395597…`): `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, 200 s (deadline 600 s).
- invariant-s27-* (all fifteen): pass.

```yaml
gates:
  constructor_unit_tests: 1453
```

## Architecture

- `commitOverSetups` sits directly after `commit` in `Experiments.fs`. When `canCommit` holds, it folds `commit` over
  the references, and each step starts from `{ draft with editingId = None }`. That keeps `commit` on its append
  branch for every reference. Each appended experiment gets the next id, and `nextId` moves past them all.
- The function is pure and writes nothing. The host that turns a grid into setups and experiments, and records them,
  comes in step 276.

## Deferred

Nothing. Every requirement of the slice landed this round.

## Gotchas

- `commit` leaves the draft editing the experiment it just appended. Folding `commit` without first resetting
  `editingId` would update one experiment over and over instead of appending one per setup.
- In the test, E1 holds a data attachment. That makes "no data file" on the appended experiments a real check
  against update-in-place, which would keep E1's file.
- A committable draft over no references answers the collection with its draft editing nothing and no new experiment.
  The step does not name this case; it follows from "the answer's draft is the draft editing nothing".

## Changelog

- 2026-11-10: Step 0275 adds `Experiments.commitOverSetups`, the experiments half of the grid: the draft is committed,
  editing nothing, over each setup in order. constructor-unit-tests went from 1452 to 1453.
