# Step 0406 — state of the world

## Where we are

Spec 0050 S11.5 covers publishing and the study view. Step 0405 gave a promoted library entry a fit provenance.
This step adds Publish calibration's pure half: a stage-one fit run's item-owned values become the kit's items'
next revisions (001 §I, §L, §P).

`Study.calibratedItemsWith` builds these revisions:

- each value becomes a `CalibratedParameter` of its fitted value, its σ (`FitRunResults.sigmas`, S11.4) and the run;
- a link's follower takes its leader's state, so stage 2's `stageWith` finds it calibrated.

Session- and experiment-owned values stay in the run. Nothing is written yet: recording the items is step 413's
job, in the window.

## What's working

- Added `StudyPublishing.PublishError`: six two-word `*Err` cases, each rendered by `describe` with its condition
  and payload.
- Added `Study.calibratedItemsWith`, which publishes a run's item-owned fitted values as the items' next revisions
  through `Item.publishRevision`. Each value carries its σ and fit run, and a linked follower takes its leader's
  calibration.
- Refused: a foreign run, an unknown item, an undetermined σ, a run with no item-owned value, and a run whose
  calibration every item already holds.
- Added `StudyPublishingTests` (four facts). Constructor tests rise from 1726 to 1730.

## Tests

Development runs on 2026-11-14. These are not gate verdicts; the supervisor re-runs every gate. Every run finished
within its effective deadline.

| Gate | Development outcome | Duration / deadline |
|---|---|---|
| build | exit 0, 0 errors, only the exempt `NU1902` advisories (plus vendored MathNet's recorded `SYSLIB0051` on recompile) | 81 s first, 38 s final / 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)` (no base: all 13), known findings 0 | 226 s / 600 s |
| constructor-unit-tests | 1730/1730 passed, whole suite (+4) | 153 s / 180 s |
| invariant-s40-* (20) | each ran its own command: `INVARIANT HOLDS ... found nothing` | < 1 s each / 300 s |

The four new facts passed:

- `publishing a calibration writes each item-owned fitted value with its sigma and fit run into the item's next revision, and a linked follower takes its leader's`
- `a session-owned value is no item calibration, and a run that calibrates nothing or only what is published is refused`
- `a value without a sigma, a run of another study or an item the kit lacks publishes nothing`
- `every publish error renders its condition and its payload`

A mutation check ran four mutants in one build:

- followers dropped;
- held states kept;
- the foreign check disabled;
- one text changed.

All four facts went red. The source was restored byte-identical (sha256-checked) and rebuilt.

```yaml
gates:
  constructor_unit_tests: 1730
```

## Architecture

- **A pure update over the kit's items.**
  - The member reads only the study (its name and links), the fit run (its study, name, entries and σs) and the
    items it is given.
  - It writes nothing and reads no clock.
  - It answers only the items that change, in `items` order.
- **One revision write.** Every revision goes through `Item.publishRevision` (step 235), so `Parameter.validate`
  and the "published twice" rule stay its own. An `ItemError` surfaces unchanged inside `PublishItemErr`.
- **The checks run in the brief's order** (impl-log D1): foreign, unknown item, σ, no calibration, nothing new,
  item refusal.
  - The follower states are taken from the run's calibrations BEFORE held states are dropped (D3). So a follower
    added after its leader was published is still published, while a repeat publish writes nothing.
- **Reuse.**
  - The ordered first-error collector is `ItemEvaluation.sequenced` (ruling 016).
  - The `FitEntryRef` key matches a fitted value to its σ, as S11.4 stores it.
  - No other type or member was added.

## Deferred

Nothing. The brief is complete. Recording the published items is step 413's own brief, not deferred work.

## Gotchas

- A link whose follower's item is not among `items` is skipped silently (brief). Only an item-owned ENTRY's
  missing item is refused.
- A σ the run does not record at all is as undetermined as `sigmaOpt = None` (D2).
- The constructor suite runs 153 s of its 180 s deadline. Its shape is unchanged: the critical path is
  `StudyEngineTests`.

## Changelog

- 2026-11-14 — Step 0406: added `StudyPublishing` (`PublishError`, `Study.calibratedItemsWith`).
  - A fit run's item-owned values become the items' next revisions as `CalibratedParameter`s, and linked followers
    take their leader's.
  - Four facts added; constructor tests 1726 → 1730.
