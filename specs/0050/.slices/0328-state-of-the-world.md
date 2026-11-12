# Step 0328 — state of the world

## Where we are

This step closes spec 0050 section S9.3: one file serving many experiments, exclusions, corrections, the Data step and
the experiment matrix (001 §K, §O).

- Steps 320–327 built the pieces:
  - the split and its writes;
  - the Data step and its split panel;
  - the review with its exclusions and setup correction;
  - the experiment matrix.
- This step proves them end to end through the kit window over a real folder kit.
- It also records in AGENTS.md where Attach data file… lives and that the Data step never edits a lab file.
- No product code changed.

## What's working

- Added the `ui-smoke` acceptance fact for the Data step to `KitWindowTests`.
  - A real folder kit's Data step splits a Phone ROI file across its experiments: one is matched by name, and one is
    created with its new setup.
  - The fact reviews an experiment from the matrix, excludes a point and re-points its setup with an audit note.
  - The experiment files hold each change, and the lab file in `data/` keeps its bytes throughout.
- The fact mounts `kitWindowView` as `ConstructorScene.mount` does, through `Program.mkProgram`. The init command
  captures the dispatch, so the fact dispatches the split, the exclusion reason and the correction note itself.
- Reused instead of copied:
  - step 312's lab-file writer now takes the file's bytes;
  - step 284's item and slot lookups are the module's `kitItemIdOf` and `tableSlotIdOf`.
- AGENTS.md:
  - the Main ribbon gotcha says Attach data file… lives in the Inverse constructor's Data step;
  - a new gotcha, "The Data step never edits a lab file", covers exclusions, `keptBy`, corrections, the split's matching
    and `MatchByTemplate`.

## Tests

These are development runs of each gate's own command from `0328.gates`, over the final tree
(`.artifacts/0328-dev-gates-summary.log`). The arc-runner's gate engine runs the gates themselves.

| Gate | Result | Time | Deadline |
|---|---|---|---|
| build | exit 0, 0 warnings, 0 errors; no lowercase `error` in stdout | 16 s (first build of the round 78 s) | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`; known findings 0; ELEV004 excluded | 209 s | 600 s |
| constructor-unit-tests | Passed!, 1552 of 1552, 0 failed, 0 skipped | 77 s | 180 s |
| ui-smoke | Passed!, 258 of 258, 0 failed, 0 skipped | 233 s | 600 s |
| ui-tests (`DataStepTests`, `KitWindowTests`, `ScrollExtentTests`) | Passed!, 41 of 41, 0 failed, 0 skipped | 34 s | 360 s |
| invariant-s31-\* (16) | each `INVARIANT HOLDS`, exit 0 | under 1 s each | 300 s |

- **The slice's test effect passes under its exact id** (`.artifacts/0328-dev-trx/ui-smoke.trx`): created (ui-smoke)
  `OpticalConstructor.Ui.Tests.KitWindowTests.acceptance: the Data step splits a lab file across the kit's experiments,
  excludes a point and corrects a setup, all kept in the kit`. It ran in 19 s within the suite.
- **Counts.**
  - ui-smoke rose by 1, from 257 to 258.
  - The ui-tests selection is 41: DataStepTests 9, KitWindowTests 16 and ScrollExtentTests 16. Each class's count is
    unchanged; KitWindowTests' non-smoke facts are 16 before and after. The selection now includes KitWindowTests, so it
    counts 41 against the earlier 25.
  - constructor-unit-tests is unchanged at 1552.
- **Mutations** (`.artifacts/0328-dev-mutation.log`, driver `.artifacts/0328-mutation-tools/mutate.py`). All 5 were
  caught, each source was restored byte-identical, and the solution was rebuilt before the final runs:
  - the offer toggle sending nothing;
  - Exclude sending a blank reason;
  - the matrix entry reviewing nothing;
  - the correction dropping its audit trail;
  - the split mapping every row instead of the group's key.
- **Build warnings.** The round's first build recompiled the vendored MathNet and printed its two known, pre-existing
  `SYSLIB0051` warnings, as in steps 320–326. The final build printed none.
- **Elevation's suppressed count** (ELEV004, excluded in the command, measured not gated per R7) is unchanged at 1453.
- No run exceeded its effective deadline. Elevation ran concurrently with constructor-unit-tests and ui-smoke.
- Every touched file is LF only: `git diff --numstat` equals `--ignore-cr-at-eol`.

```yaml
gates:
  constructor_unit_tests: 1552
  ui_smoke_tests: 258
  ui_tests: 41
```

## Architecture

- **A test-only step.** The proof drives the production surfaces unchanged:
  - `kitWindowView`;
  - `updateTableAndElementRotation`, through its `SplitByColumn`, `ToggleSplitOffer`, `MatchByTemplate`,
    `ReviewExperimentData`, `SetExclusionReason`, `ExcludePoint`, `SetCorrectionNote` and `CorrectSetup` arms;
  - the kit's real `ExperimentDataProxy` over a real folder kit.
- **The fact's mount mirrors `ConstructorScene.mount`.** It runs `Program.mkProgram` with `Cmd.ofEffect` capturing the
  dispatch, under the inert shell.
  - It dispatches only what the split wizard's Split, and the review's two fields, would send.
  - Every press goes at a control's centre through `pressBox`, which first finds the control by its id and asserts it
    visible.
- **The kit is read back through `KitProxy.create folder`** after each write, as step 312's fact reads it. The fact
  therefore checks the files, not only the model.

## Deferred

Nothing. Every requirement of the slice landed.

## Gotchas

- **The Inverse constructor seeds a sample slot** (`src`, `sample`, `det`).
  - Adding a second sample makes it Object 2. The template then names the seeded one, so turn and fill the seeded
    sample.
  - The fact selects it by `selection = ElementSelected (SceneElementIndex i)`, as `ExperimentControlsTests` does:
    nothing but a pointer click selects a slot.
- **`AddElement` spreads middle elements along the beam** (`-0.3 + 0.2 × count` m from the source). Around the seeded
  sample, the first polarizer added lands before it and the second after it. The fact pins this with a light-order
  assertion.
- **How the file's groups match.**
  - `LP1-(QZ#45)-LPd-2` reads through the standard template with the note `2`. It is offered as its own experiment over
    the held setup, and the fact leaves it unticked.
  - `Darkness` matches no way of reading the template, so it stays unmatched.
- **The AGENTS.md Main ribbon edit** replaced only the clause naming the Experiments bay's "Data files" section. The same
  sentence's chart clause is kept as "the rest of the bullet" (impl-log Decisions).
- `.manifest.state.json` was already modified when the round started. It is the supervisor's file and was not touched.

## Changelog

- 2026-11-12 — Step 0328: the end-to-end proof of the Data step.
  - A `ui-smoke` acceptance fact in `KitWindowTests` splits a lab file across a folder kit's experiments, matched and
    created, then excludes a point and corrects a setup. Each change is kept in the experiment files, and the lab file
    is untouched.
  - AGENTS.md says where Attach data file… lives and carries the gotcha "The Data step never edits a lab file".
