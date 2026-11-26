# Step 0318 — state of the world

## Where we are

This step is in spec 0050 section S9.3, which covers one file serving many experiments, exclusions, corrections, the
Data step and the experiment matrix (001 §K).

- Step 317 split mapped points into keyed groups (`splitGroups`).
- This step matches each group to an experiment through the kit's naming template (001 §K "match each group by the
  naming template, list the unmatched groups and offer to create setups for them"; 001 §P `MatchByTemplate`). It is pure
  Domain and writes nothing.
- Step 319 adds the `SplitByColumn` message. Step 320's window applies a split.

## What's working

- Add `DataRowKey.nameText`, a key as a name the naming template reads. Nothing converts back (ELEV006).
- Add `DataSplitError`. Its cases are `SplitKeyTextErr`, `SplitKeyNameErr`, `SplitKeySetupErr` and `SplitKeyTwiceErr`,
  and each `describe` renders its condition and payload.
- Add `SplitSetupOffer`, `SplitGroupMatch` (`ExperimentForGroup` / `SetupOfferForGroup` / `GroupWithoutMatch`),
  `SplitGroup`, `DataSplitParam` and `DataSplit` with `matched`, `offered` and `unmatched`.
- Add `DataSplit.ofGroups`, the one matching rule, group by group in order:
  - the kit's experiment the key names, as written or as the template writes it back;
  - else the setup (held or new) and the experiment the template reads in the key, over the table's slots and angles;
  - else the typed reason.

  Two groups never match one experiment.
- Tests: four new `DataSplitTests` facts over a test-local copy of step 271's bench.

## Tests

These were development runs of each gate's own command over the final tree. The arc-runner's gate engine runs the
gates themselves.

| Gate | Result | Time | Deadline |
|---|---|---|---|
| build | exit 0, 0 warnings, 0 errors; no lowercase `error` in stdout | 45 s | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`; known findings 0; ELEV004 excluded (suppressed 1434) | 206 s | 600 s |
| constructor-unit-tests | Passed!, 1542 of 1542, 0 failed, 0 skipped | 69 s | 180 s |
| invariant-s31-\* (16) | each decoded `git grep` finds nothing | under 1 s each | 300 s |

- **constructor-unit-tests: 1542**, which is 1538 + 4, as the slice requires.
  - The TRX (`.artifacts/0318-dev-trx/constructor-unit-tests.trx`) shows the four new facts as `Passed`:
    - `a group matches the experiment its key names, as written or as the template writes it back`;
    - `an unmatched group the template reads is offered its setup and experiment over the table's slots`;
    - `an unmatched group the template cannot place is listed with its reason, and two groups never share an experiment`;
    - `every split error renders its condition and its payload`.
  - Step 317's two `DataSplitTests` facts pass unchanged.
- **Mutations** (`.artifacts/0318-dev-mutation.log`, `.artifacts/0318-dev-mutation-dial.log`):
  - disabling the twice-rule fails the refusal fact;
  - dropping the offered names from `taken` fails the offer fact;
  - using a dial reading as R1 fails the offer fact on R1 30°.

  The file was restored byte-identical (`cmp`), and the final tree was rebuilt and re-tested.
- Elevation's suppressed count is 1434. These are ELEV004 `.value` reads, which the gate command excludes (spec 0048
  `.manual/002` R7). The known-findings list is untouched and empty.
- No run exceeded its effective deadline.
- Both touched files are LF only, and `git diff --numstat` equals the `--ignore-cr-at-eol` count.

```yaml
gates:
  constructor_unit_tests: 1542
```

## Architecture

- **One fold, whose state is the groups already made.** Three things come from the earlier groups:
  - the twice-rule (`SplitKeyTwiceErr` naming both keys and the experiment);
  - the setups offered earlier, which follow `param.setups` in `generate`'s `held`, so keys describing one setup are
    offered the same one;
  - the experiment names offered earlier, which join the kit's in `experimentNameAmong`'s `taken`.

  No second accumulator can drift from the groups.
- **The offer reuses the grid generator, not a copy of it.**
  - Each slot whose role a `SlotPlaceholder` of the template names becomes a one-choice axis: the read item or nothing,
    at R1. R1 is the read angle for a fixed object, `sign.physicalAngleOf item.dialZeroAngle angle` for a dial-mounted
    one, and otherwise the slot's own.
  - `SetupGrid.generate` makes, matches and names the setup.
  - The grid is built with `unticked = []` rather than through `SetupGrid.create`, so the table's refusal reaches the
    group as `SplitKeySetupErr` instead of an empty grid.
- **The kit's name identity is the experiment's file stem, ignoring case (step 286).** The first experiment of
  `param.experiments` that equals the key's name, or the template's re-written name, wins. So `#45.0` matches `#45`.
- `DataSplitParam` is `[<ReferenceEquality>]` because it holds `choiceOf`. The window passes its `tryItemModelChoice`
  (step 263). A split is never stored, so no union in it carries a wire tag.

## Deferred

Nothing. Applying a split (attaching each group, creating the offered setups and experiments) is step 320's, by the
spec's plan. The `SplitByColumn` message is step 319's.

## Gotchas

- `SetupGrid.create` unticks impossible combinations, so a refusal would vanish into `generate`'s `Ok []`. The offer
  builds the grid literally so the refusal surfaces. Keep it that way if this code is ever refactored.
- The record labels `key`, `points`, `setup`, `items` and `table` are shared with `KeyedGroup`, `Experiment`,
  `ExperimentTarget` and `GridSlotDraft`. Annotate every new construction of these records.
- "A role the reading names and the table has no slot of" is checked over the reading's slot parts. In every template
  an object's angle travels in the same optional group as its part, so the missing slot is named either way.
- A recursive `grep` over a test project without a file filter walks `bin/` / `obj/` and stalls. Use the Grep tool or
  `--include=*.fs`.

## Changelog

- 2026-11-11 — Step 0318: `DataSplit.ofGroups` matches each split group to the kit's experiment its key names, as
  written or as the template writes it back. An unmatched group the template reads is offered its setup and experiment
  over the table's slots, and every other group is listed with its typed `DataSplitError`. Four `DataSplitTests` facts
  were added (1538 → 1542).
