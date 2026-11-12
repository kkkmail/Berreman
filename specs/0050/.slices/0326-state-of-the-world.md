# Step 0326 — state of the world

## Where we are

This step is in spec 0050 section S9.3: one file serving many experiments, exclusions, and the Data step (001 §K, §O).

- Step 316 gave the model `SetCorrectionNote`, `CorrectSetup`, `dataDraft.correctionNote` and the experiment's
  `corrections`, with no surface showing them.
- Steps 323–325 built the Data step, its split panel and its review panel.
- This step closes the review panel with the setup correction. The reviewed experiment shows its setup and an audit note.
  Each other setup of the kit is offered as a re-point that sends `CorrectSetup` with the typed note, and the audit
  trail lists earlier corrections.
- Step 327 adds the step's further panel, the experiment matrix.

## What's working

- Added a `Setup:` block to the Data step's review panel. It shows the experiment's setup and an `Audit note:` field,
  which commits on Enter and on blur.
- Added a `Re-point to <setup>` verb for each other setup of the kit. It sends `CorrectSetup` with the note field's live
  text and is disabled while the kit takes no writes.
- Added the audit trail: one line per correction, oldest first, reading `<previous> → <corrected>: <note>`.
- Added the ids `dataCorrectionNoteField`, `dataCorrectionTrail` and `dataCorrectSetupChoice <name>`.
- The review's two fields now share one builder, and the Apply split and re-point verbs share one availability box.
- Tests: +2 ui-tests facts in `DataStepTests` (6 → 8) and the ids fact extended.

## Tests

These are development runs of each gate's own command over the final tree (`.artifacts/0326-dev-gates-summary.log`). The
arc-runner's gate engine runs the gates themselves.

| Gate | Result | Time | Deadline |
|---|---|---|---|
| build | exit 0, 0 warnings, 0 errors; no lowercase `error` in stdout | 35 s | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`; known findings 0; ELEV004 excluded | 206 s | 600 s |
| constructor-unit-tests | Passed!, 1552 of 1552, 0 failed, 0 skipped | 74 s | 180 s |
| ui-smoke | Passed!, 256 of 256, 0 failed, 0 skipped | 222 s | 600 s |
| ui-tests (`DataStepTests`) | Passed!, 8 of 8, 0 failed, 0 skipped | 16 s | 360 s |
| invariant-s31-\* (16) | each `INVARIANT HOLDS` (`.artifacts/0326-dev-invariants.log`) | under 1 s each | 300 s |

- **The slice's test effects pass under their exact ids** (`.artifacts/0326-dev-trx/ui-tests.trx`):
  - created: `the correction panel offers every other setup of the kit and lists the audit trail`;
  - created: `pressing a setup sends CorrectSetup with the typed audit note`;
  - changed: `the Data step UiIds are the stable intent-named ids`, which now also pins `DataCorrectionNoteField`,
    `DataCorrectionTrail` and `DataCorrectSetup_Bench B`.
- **Counts.**
  - The selected class `DataStepTests` rose by 2, from 6 to 8.
  - Step 325's ui-tests figure of 49 also counted `ExperimentControlsTests` (43), which that step selected and this one
    does not.
  - ui-smoke is unchanged at 256, and constructor-unit-tests at 1552.
- **Mutations** (`.artifacts/0326-dev-mutation.log`, driver `.artifacts/0326-mutation-tools/mutate.py`). All 13 were
  caught, and each source was restored byte-identical. They covered:
  - Re-point sending the draft's note, or the reason field's text, or the experiment's own setup;
  - the own setup offered; the choices in reverse order; the choices offered over a read-only kit;
  - the note field committing as the exclusion reason;
  - the setup line without its revision; an empty trail shown; the trail newest first; a trail line without its note;
  - the refactored reason field and Apply split box, each still caught by step 324's or 325's facts.
- The final build and gate runs came after the mutations.
- No run exceeded its effective deadline.
- Every touched file is LF only: 0 CR bytes counted in Python, and `git diff --numstat` equals `--ignore-cr-at-eol`.

```yaml
gates:
  constructor_unit_tests: 1552
  ui_smoke_tests: 256
  ui_tests: 8
```

## Architecture

- **A pure projection of the correction.** The block reads:
  - `experiment.setup`, `experiment.corrections`;
  - `model.kitSetups`, `model.dataDraft.correctionNote`, `model.kitWrites`.

  It sends only the existing `SetCorrectionNote` and `CorrectSetup`, and the update is untouched. `CorrectSetup`
  already refuses a blank note, an unknown setup, the experiment's own setup and a read-only kit.
- **Re-point commits the note field's live text first** (spec 0050 S4.3, as step 325's Exclude). A verb box does not
  blur the field. When the field was committed, the note sent is exactly `model.dataDraft.correctionNote`.
- **`typedText` finds a field by its id.** Step 325's `typedReason` hard-coded the reason field; the review now holds two
  text fields.
- **The re-point verb is `verbButtonOn` inside `availabilityBox`.** `availableWorkbenchButton` re-patches its press only
  when its id changes, and the id carries only the setup's name. `verbButtonOn` re-patches on every render, so a setup
  re-recorded at a new revision re-points to that revision.
- **One availability box serves the whole WrapPanel and Apply split.** It was step 324's inline wrapper.
- **One builder serves both review fields** (`reviewField`): the label docked left and the text box committing through
  a message constructor. Its handlers re-patch on every render, as `ItemsPanelView` and `SetupGridView` do.

## Deferred

Nothing. Every requirement of the slice landed. The experiment matrix belongs to step 327, by the spec's plan.

## Gotchas

- **Re-point may send two messages.** It sends `SetCorrectionNote`, then `CorrectSetup`. A check of its messages should
  pick out `CorrectSetup`, not expect a single message.
- **The note field stays enabled over a read-only kit.** Only the re-point verbs are withheld. `SetCorrectionNote` writes
  nothing, as with the reason field.
- **A choice's id carries the setup's name only** (`DataCorrectSetup_<name>`), so it can contain spaces
  (`DataCorrectSetup_Bench B`). The kit holds one current revision per setup name, so the ids stay unique.
- **The block shows even with no other setup.** `Setup:`, the note field and an empty WrapPanel appear for an
  experiment over the kit's only setup. That case is step 325's smoke fact with `Dark`.
- **The vendored MathNet's SYSLIB0051 warnings** show up only when `MathNetNumerics/Numerics` recompiles, as in this
  round's first build and step 325's Ui build. They are pre-existing, outside our F# code, and contain no lowercase
  `error`. The final incremental build reported 0 warnings.
- `.manifest.state.json` was already modified when the round started. It is the supervisor's file and was not touched.

## Changelog

- 2026-11-12 — Step 0326: the Data step's setup correction.
  - The review panel closes with `Setup:`, an `Audit note:` field, `Re-point to <setup>` verbs sending `CorrectSetup`,
    and the oldest-first audit trail.
  - Three new UiIds.
  - Tests: +2 ui-tests facts in `DataStepTests` (8); the ids fact extended.
