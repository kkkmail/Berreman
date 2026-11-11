# Step 0294 — state of the world

## Where we are

Spec 0050 section S9.1 covers Experiments v2: scans, roles, sessions and attachments.
- Step 288 gave the experiment its role, session and notes.
- Step 292 added the scan messages, and step 293 the scan chart.

This step makes a recorded experiment's role, session and notes editable through three semantic messages
(001 §P `SetExperimentRole`; 001 §E notes live in the experiment's file; 001 §K role and session). Each message writes
the experiment's kit file. The bay controls that send them are step 296's.

## What's working

- Added `Experiment.withRole`, `withSessionOpt` and `withNotes`, and `ExperimentCollection.withStoredExperiment`. The
  last replaces the experiment of that id, does nothing when none matches, and leaves the draft alone.
- Added `SetExperimentRole`, `SetExperimentSession` and `SetExperimentNotes` to the Main scene. Each one:
  - goes through step 185's read-only guard first;
  - records the changed experiment through `KitLibraryContext.recordExperiment`;
  - then stores it in the collection.
- A blank session text clears the session (`SessionName.tryCreate`). An id the collection lacks changes nothing and
  writes nothing. A failed write keeps the model and reports `Not saved to the kit — <reason>`.
- Two new `ExperimentModelTests` facts, plus the three messages added to the read-only `KitWindowModelTests` fact.

## Tests

These are development runs with the gates' own commands and this step's selectors. The arc-runner's gate engine runs
the gates themselves. No run went over its effective deadline.

| Gate | Result | Time | Deadline |
|---|---|---|---|
| build | exit 0, 0 warnings, 0 errors | 16 s (incremental; 37 s after the first edit) | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, known findings 0, ELEV004 excluded | 201 s | 600 s |
| constructor-unit-tests | Passed!, 1494 of 1494, 0 failed, 0 skipped | 69 s | 180 s |
| ui-smoke | Passed!, 249 of 249, 0 failed, 0 skipped | 207 s | 600 s |
| ui-tests (4 selected classes, the gate's exact command) | Passed!, 77 of 77, 0 failed, 0 skipped | 107 s | 360 s |
| invariant-s29-\* (16) | every decoded `git grep` finds nothing | < 1 s each | 300 s |

- **constructor-unit-tests: 1494**, which is 1492 + the two new facts.
  - `ExperimentModelTests` has 4 facts.
  - The changed `a kit opened read-only refuses every write message and writes nothing` passes.
- **ui-tests: 77** over this step's four classes: ExperimentControls 39, FsxAcceptance 14, KitWindow 16 and
  WorkbenchInfoAndCapture 8. None of these classes changed.
  - Step 0293 selected six different classes for 95.
  - The three classes both steps select still count 39, 14 and 8.
- **ui-smoke: 249**, unchanged.
- **Mutation check (red):** the product file was restored byte for byte (SHA-256 checked) and rebuilt.
  - With the guard dropped, the read-only fact failed: its status read the write failure, not the read-only refusal.
  - When a blank session was kept, the blank-session fact failed (`Some (SessionName "  ")`).
  - Skipping the kit write failed the role/session/notes fact, because the experiment file's bytes did not change.
  - The blank-session fact now also checks that the file held `Day 1` before the clear.
- **Elevation's first run failed** with one ELEV001: `ExperimentNotes text.value` re-wraps a `ControlCaption` at the
  call site. It was fixed at the source (see Architecture), and the rerun is clean.

```yaml
gates:
  constructor_unit_tests: 1494
  ui_smoke_tests: 249
  ui_tests: 77
```

## Architecture

- **One helper for the three arms.** `recordExperimentEdit` in `TableAndElementRotationView.fs` has the same shape as
  `recordKitItemEdit`.
  - Order: guard, then lookup, then change, then `recordExperiment`, then `withStoredExperiment`.
  - On success `kitStatus` clears, as on every other write arm.
  - An unknown id in a read-only kit reports the read-only status, because the guard runs first.
- **The notes conversion is a member on the source type.** The slice text prescribes `ExperimentNotes text.value`, which
  fails ELEV001, and AGENTS.md says the analyzers win. The arm calls `text.toExperimentNotes`, a private extension of
  `ControlCaption` in the view (the precedent is `exportText`). It is in the Ui project because Domain does not reference
  Controls.
- **The session conversion keeps `SessionName.tryCreate`**, so the blank rule is reused and not copied. It is reached
  through `type private ExperimentSessionName = Experiments.SessionName`, for the reason under Gotchas.

## Deferred

Nothing. Every requirement of the step landed in this round. The bay controls that send these messages are step 296's,
by the spec.

## Gotchas

- **`Experiments.SessionName.tryCreate` does not compile in the view (FS0039).**
  - A module-qualified name that is both a single-case union and its type resolves to the union case in an expression,
    and the case has no `tryCreate`.
  - Unqualified (`SessionName.tryCreate`, under `open Experiments`) the type wins.
  - The view does not open `Experiments`, because that would shadow dozens of names and record labels. It uses the
    private abbreviation instead.
- **"E1" in the slice is the experiment of id 1.** Its file name comes from the kit's naming template, so the facts find
  the file through the stored name, not a literal `E1.experiment-json`, as step 292's fact does.
- **The ui-tests count changed** because the selection changed (77 over four classes, against 95 over six at step
  0293). The three shared classes still count 39, 14 and 8.
- `specs/0050/.manifest.state.json` was already dirty from the supervisor when the round began. This round did not
  touch it.

## Changelog

- 2026-11-11 — Step 0294: a recorded experiment's role, session and notes are set by message and written to its file.
  - Added `Experiment.withRole` / `withSessionOpt` / `withNotes` and `ExperimentCollection.withStoredExperiment`.
  - Added `SetExperimentRole` / `SetExperimentSession` / `SetExperimentNotes`, each guarded for a read-only kit and
    written through `recordExperiment`.
  - Two new `ExperimentModelTests` facts; the read-only `KitWindowModelTests` fact sends the three messages.
