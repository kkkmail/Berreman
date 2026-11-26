# Step 0286 — state of the world

## Where we are

Spec 0050 section S9.1 covers Experiments v2: scans, roles, sessions and attachments. Step 285 laid the run's role,
session and notes. This step adds the name an experiment is stored under, and the kit naming template's member that
writes that name from the experiment's setup slots (001 §A, §J; the S9.1 brief). Nothing stores a name yet: step 290
adds `Experiment.name`, writes `<name>.experiment-json` and deletes `ExperimentId.fileStem`.

## What's working

- Add `ExperimentName`. Its `tryCreate` keeps the text as given and refuses a blank text with
  `InvalidExperimentNameErr`. Its `fileStem` follows the shared name rule. Its `nextFreeAmong` takes the next free
  suffix from `FileStem.firstFreeText`.
- `ExperimentError` moves up beside the S9.1 types and gains `InvalidExperimentNameErr` and a `describe` that renders
  each case.
- Add `ExperimentId.defaultName`, `E<n>`: the fallback name, and the stem `ExperimentId.fileStem` now derives from.
- Add `NameText.toExperimentName`, which is one-way (ELEV006).
- Add `NamingTemplate.experimentNameAmong`. It writes the template's name for a setup's slots and note, made free among
  the kit's experiments, or answers the name it is given.
- Three new constructor-unit-tests facts cover all of the above.

## Tests

These are development runs on the final tree, using the snapshot's own commands. The arc-runner's gate engine runs the
gates themselves. No run went over its effective deadline.

- **build:** exit 0, 0 errors, in 74 s against a 600 s deadline. The only warnings were the two vendored MathNet
  `SYSLIB0051`, which are not ours.
- **constructor-unit-tests:** Passed!, 1477 of 1477, in 69 s against a 180 s deadline. That is 1474 at the last
  checkpoint plus the three new facts. The TRX shows each new fact `Passed`:
  - `an experiment name keeps its text, refuses a blank one, takes the next free suffix and defaults to its id`
  - `every experiment error renders its condition and its payload`
  - `an experiment is named by the kit's template from its setup's slots, made free among the kit's experiments, or
    takes the name it is given`
- **elevation:** `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, known findings 0, in 206 s against a 600 s
  deadline.
- **invariant-s29-\*** (16): every decoded grep finds nothing.
- **Mutation check:**
  - Letting `ExperimentName.tryCreate` keep a white-space text turned both name facts red, each naming the bug.
  - The file was restored byte for byte (SHA-256 checked), rebuilt and re-run green: 1477 of 1477.

```yaml
gates:
  constructor_unit_tests: 1477
```

## Architecture

- **Order in `Experiments.fs`.** The types now run: `ExperimentId` → step 285's `ExperimentRole` / `SessionName` /
  `ExperimentNotes` → `ExperimentError` → `ExperimentName` → `type ExperimentId with` (`defaultName`, `fileStem`).
- **Why `defaultName` is an extension.** `ExperimentName` is declared after `ExperimentId`, so `defaultName` is an
  intrinsic extension in the same module. AGENTS.md: towards a later type, a type extension declared after it.
- **`ExperimentId.fileStem` is `this.defaultName.fileStem`.** The `E<n>` text exists once, and an experiment the
  template cannot name keeps its file name by construction. The member and its value are unchanged.
- **`experimentNameAmong` reuses the setup naming path.** It calls the same members `setupNameOf` and
  `setupNameAmong` do: `nameReadingOf` → `writeName` → `toExperimentName` → `nextFreeAmong`. There is no second naming
  rule, and any error answers `unnamed`.
- **`NamingTemplates.fs` opens `OpticalConstructor.Domain.Experiments`.** `Experiments.fs` compiles before it.

## Deferred

Nothing. Every requirement of the slice landed this round. Storing the name is step 290's brief, not this step's: it
covers `Experiment.name`, the `<name>.experiment-json` files and `experimentTargets`.

## Gotchas

- No code matched over `ExperimentError` before this step, so "a match over the error anywhere gains the case" had no
  site to change.
- `ExperimentName.nextFreeAmong` passes the taken names' texts to `FileStem.firstFreeText`, as `SetupName` and
  `ItemLabel` do. `firstFreeText` compares their stems itself, and ELEV010 refuses a `string` beside a `FileStem`.
- In Git Bash, `grep -c $'\r'` reports every line, so it is not a CR count. Count CR bytes with Python instead. Every
  touched file is LF.

## Changelog

- 2026-11-11: Step 0286 adds S9.1's `ExperimentName` and `ExperimentId.defaultName`, gives `ExperimentError` a
  blank-name case and `describe`, and adds `NameText.toExperimentName` and `NamingTemplate.experimentNameAmong`.
  constructor-unit-tests went from 1474 to 1477.
