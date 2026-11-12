# Step 0321 — state of the world

## Where we are

This step is in spec 0050 section S9.3: one file serving many experiments (001 §K, §O).

- Steps 317–320 gave the kit window a split: `SplitByColumn` identifies the groups, and `MatchByTemplate` applies them.
  Until this step, nothing opened a split from a picked file.
- This step lets the import wizard of steps 308–311 serve the split too. Its preview, column roles and profile picker
  are the data dialog's (001 §O; .spec-md §6 "Reuse before invention": one wizard).
- The wizard now carries a target: attach to one experiment, or split across the kit's experiments. The new
  `OpenSplitWizard` opens it for a split.
- The Data step's `Split a file by key…` button, which sends `OpenSplitWizard`, is step 323's. The split panel is
  step 324's.

## What's working

- Added `ImportWizardTarget` (`AttachWizardTarget of ExperimentId | SplitWizardTarget`).
  - The wizard's request and applied records carry `target` in place of `experimentId`.
  - Apply hands the target on unchanged.
- The wizard says which target it serves, in its heading and its button:
  - attach: `Import <leaf>` and `Attach`;
  - split: `Split <leaf> by its key column` and `Split`.
- `ImportWizardApplied.messages` answers `AttachDataFile` or `SplitByColumn` by target. Each comes after
  `SaveImportProfile` when there is a new mapping, so a new profile is in the kit before the file is mapped or split.
- Added `OpenSplitWizard of FileName`, behind the read-only guard. It opens the shell's one wizard with Phone ROI CSV
  chosen and the kit's profiles, and answers the model unchanged.
- Tests:
  - two new facts: `KitWindowModelTests` 1548 → 1549; ui-tests over the selected classes 41 → 42;
  - five facts changed to the target: the two Attach facts, the opening fact, the wizard-Attach fact and the read-only
    fact;
  - `ScrollExtentTests`' wizard request targets `E1`.

## Tests

These are development runs of each gate's own command over the final tree. The arc-runner's gate engine runs the
gates themselves.

| Gate | Result | Time | Deadline |
|---|---|---|---|
| build | exit 0, 0 warnings, 0 errors; no lowercase `error` in stdout | 75 s first, 17 s final | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`; known findings 0; ELEV004 excluded (suppressed 1435) | 220 s | 600 s |
| constructor-unit-tests | Passed!, 1549 of 1549, 0 failed, 0 skipped | 72 s, 70 s final | 180 s |
| ui-smoke | Passed!, 253 of 253 | 215 s, 220 s final | 600 s |
| ui-tests (`ImportWizardTests`, `KitWindowTests`, `ScrollExtentTests`) | Passed!, 42 of 42 | 29 s, 27 s final | 360 s |
| invariant-s31-\* (16) | each `INVARIANT HOLDS` (`.artifacts/0321-dev-invariants.log`) | under 1 s each | 300 s |

- **constructor-unit-tests: 1549**, which is 1548 + 1, as the slice requires.
  - The TRX (`.artifacts/0321-dev-trx/constructor-unit-tests.trx`) shows these as `Passed`:
    - the new `opening the split wizard hands it the picked file with Phone ROI CSV chosen, and its Apply splits by the chosen profile`;
    - the changed `opening the import wizard hands it the picked file, the kit's profiles and the detector's default`;
    - the changed `the wizard's Attach saves its new mapping first and attaches the file by it`;
    - the changed `a kit opened read-only refuses every write message and writes nothing`.
- **ui-tests: 42** over the three selected classes, run with the gate's two `--filter-not-trait` options:
  `ImportWizardTests` 11, `KitWindowTests` 16 and `ScrollExtentTests` 15.
  - `ImportWizardTests` had 10 non-smoke facts at HEAD, so the selection rose by 1, as the slice requires.
  - The TRX (`.artifacts/0321-dev-trx/ui-tests.trx`) shows these as `Passed`:
    - the new `a split wizard says it splits and hands the kit window the file and the profile to split by`;
    - the changed `Attach with a picked profile hands it to the kit window once and closes`;
    - the changed `Attach with a new mapping hands the kit window the profile to save and attaches by it`.
- **ui-smoke: 253**, equal to its checkpoint. No ui-smoke fact changed.
- **Mutations** (`.artifacts/0321-dev-mutation.log`). Each ran over a rebuilt tree, with the source restored
  byte-identical, and each was caught:
  - M1, `OpenSplitWizard` without `guardKitWrite`: the read-only fact fails.
  - M2, the button reads `Split` for either target: the new UI fact fails, at its attach check.
  - M3, the split ignores the chosen profile: the new model fact fails.
  - M4, the split heading reads as an import: the new UI fact fails.

  The final tree was then rebuilt, with 0 warnings and 0 errors, and re-tested: constructor-unit-tests 1549, ui-tests
  42 and ui-smoke 253 (220 s) all passed.
- No run exceeded its effective deadline.
- Every touched file is LF only (0 CR bytes), and `git diff --numstat` equals the `--ignore-cr-at-eol` count.

```yaml
gates:
  constructor_unit_tests: 1549
  ui_smoke_tests: 253
```

## Architecture

- **One wizard, two targets.**
  - `ImportWizardTarget` is a closed two-case DU. It is never stored, so it has no wire tag, as `DataSplit` has none.
  - The wizard's model and update are untouched, except that `applied` hands on `request.target`.
  - The view reads the target in exactly two places, the heading and the caption of `importWizardAttachButton`. The
    control keeps its id, so automation finds the apply button under either target.
- **The target decides the kit window's message, in one place** (`ImportWizardApplied.messages`).
  `ImportWizardWindow` and `withImportWizard` (step 311) are unchanged: the shell opens the same window for either
  target, and its apply dispatches `messages` in order.
- **`OpenSplitWizard` mirrors `OpenImportWizard`.**
  - It sits behind `guardKitWrite`, opens the shell's wizard, and answers the model unchanged.
  - It needs no experiment, so it has no lookup and no detector default. It opens on `ImportProfileName.phoneRoiCsv`,
    the built-in that carries a key column (step 298).

## Deferred

Nothing. Every requirement of the slice landed. The Data step's button (step 323) and the split panel (step 324) are
later steps by the spec's plan.

## Gotchas

- **The split heading's second line.** The slice names the heading's first line only.
  - The attach heading keeps `Nothing is attached until you press Attach.`, which an existing view fact pins.
  - The split heading says `Nothing is split until you press Split.`, because a split wizard has no Attach button.
  - The new UI fact pins both lines.
- **Escapes in Git Bash heredocs.** A `\\n` inside a heredoc fed to Python reached the script as `\n`, a real newline,
  even with a quoted `'EOF'`. Write mutation scripts with the Write tool, or build the escape with `chr(92)`.
- **Mutation runs leave mutated binaries.** Restoring the source is not enough: rebuild before any final test run.
- **The first build of the round recompiled the vendored MathNet** and printed two `SYSLIB0051` warnings from
  `MathNetNumerics/Numerics/Exceptions.cs`, which this step did not touch. Every later build printed 0 warnings, as in
  step 320.
- `.manifest.state.json` was already modified when the round started. It is the supervisor's file, and it was not
  touched here.

## Changelog

- 2026-11-12 — Step 0321: the import wizard gained `ImportWizardTarget` (attach to an experiment, or split across the
  kit's experiments).
  - Its heading and button say which target it serves, and its apply becomes `AttachDataFile` or `SplitByColumn` after
    any new profile is saved.
  - Added `OpenSplitWizard`, behind the read-only guard, which opens the wizard with Phone ROI CSV chosen.
  - Tests: one new `KitWindowModelTests` fact (1548 → 1549) and one new `ImportWizardTests` fact (41 → 42 selected),
    with five facts and the scroll-extent request moved to the target.
