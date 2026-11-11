# Step 0292 — state of the world

## Where we are

Spec 0050 section S9.1 covers Experiments v2: scans, roles, sessions and attachments.
- Step 288 gave the domain draft a multi-variable scan: `addScanAxis`, `addScanBlock`, `axisOpt` and `scanOpt`, with a
  commit that records `scanOpt`.
- Steps 289 to 291 added the dial-reading variable, experiment names and mapped attachments.

This step makes building a scan a pair of semantic messages of the Main scene (001 §K "one or more variables"; 001 §P).
The bay's Add variable and Add block buttons that send them are step 296's.

## What's working

- Added `ExpAddScanAxis` (Add variable): the edited axis joins the draft's current block, and the editor takes the next.
- Added `ExpAddScanBlock` (Add block): the current block closes on the capture the detectors allow, and a new block
  starts.
- Neither message writes the kit or passes the read-only guard. Without an edited axis, or with no capture, the model
  is unchanged.
- `ExpCommit`, `RecordExperiment` and `GenerateSetupGrid` commit the draft's whole scan as one experiment, unchanged.
- Added `ExperimentModelTests` over a read-write kit: a two-block scan commits as one experiment, and the no-axis case
  changes nothing and writes nothing.

## Tests

These are development runs with the gates' own commands and this step's selectors, run one after another. The
arc-runner's gate engine runs the gates themselves. No run went over its effective deadline.

| Gate | Result | Time | Deadline |
|---|---|---|---|
| build | exit 0, 0 warnings, 0 errors | 16 s; 22 s for the final rebuild (incremental) | 600 s |
| constructor-unit-tests | Passed!, 1492 of 1492, 0 failed, 0 skipped | 69 s; 71 s on the final tree | 180 s |
| ui-smoke | Passed!, 249 of 249, 0 failed, 0 skipped | 210 s | 600 s |
| ui-tests (3 selected classes, the gate's exact command) | Passed!, 58 of 58, 0 failed, 0 skipped | 91 s | 360 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, known findings 0, ELEV004 excluded | 195 s | 600 s |
| invariant-s29-\* (16) | every decoded `git grep` finds nothing | < 1 s each | 300 s |

- **constructor-unit-tests: 1492** is step 0291's 1490 plus the two new facts, as the acceptance requires. The TRX
  shows `Passed` for both:
  - `Add variable and Add block build the draft's scan, and a commit records it as one experiment`;
  - `Add variable without a chosen element and variable changes nothing and writes nothing`.
- **ui-tests: 58** over ExperimentControls 36, FsxAcceptance 14 and WorkbenchInfoAndCapture 8. Each class has the count
  step 0291 measured, so the count is unchanged.
- **ui-smoke: 249**, unchanged.
- **Mutation check (red):**
  - Dropping `ExpAddScanBlock`'s capture coercion failed the first fact: it expected T and got Both.
  - A model change in the no-axis branch failed the second fact.
  - A no-op `ExpAddScanAxis` failed the first fact.
  - The product file was restored byte for byte (SHA-256 checked) and rebuilt.

```yaml
gates:
  constructor_unit_tests: 1492
  ui_smoke_tests: 249
  ui_tests: 58
```

## Architecture

- **The messages are thin.** Each is one call into step 288's members on `ExperimentCollection` (`addScanAxis`,
  `addScanBlock`). They are members, not free functions, because of ELEV013. No domain code changed.
- **A closed block is read on the detectors' capture.**
  - `ExpAddScanBlock` first gives the draft `model.draftCapture` through `Experiments.chooseMeasurement`, exactly as
    `ExpCommit` gives the last block its capture.
  - So every block of a committed scan is read on a capture its edited element's detectors allow.
- **`ExpAddScanBlock` matches the edited axis as well as the capture.**
  - A draft whose element varies nothing has a capture but no axis.
  - Coercing its capture would change the model while Add block adds nothing, so that case is a no-op too.
- **The scan stays in the model until a commit.** The two messages are not behind `guardKitWrite`, so a kit opened
  read-only can build a scan; only the commit is refused.

## Deferred

Nothing. Every requirement of the step landed in this round. The bay's buttons that send these messages are step 296's,
by the slice.

## Gotchas

- **The first fact asks for Both before `ExpAddScanBlock`.** The slice's sequence does not list this
  `ExpChooseMeasurement`.
  - On this table only the end detector captures, through the sample's T, so `ExpChooseElement` already defaults to T.
  - Without the extra request, the coercion the slice requires of `ExpAddScanBlock` would be untested.
- **`Experiments.ElementLabel.blank`, written qualified, resolves to the union case, not the type, and does not
  compile.** The fact compares with `Experiments.ExperimentDraft.empty.elementLabel` instead.
- **`readExperiments` is a Storage extension on `KitProxy`.** A test must
  `open OpticalConstructor.Storage.KitContentFiles`.
- **`git grep` cannot see the new untracked test file.** The s29 invariants were also grepped over it directly, and
  found nothing.
- **A non-incremental build recompiles the vendored MathNet.** That prints its existing `SYSLIB0051` warnings; earlier
  steps' full-build logs show the same. Incremental builds report 0 warnings.

## Changelog

- 2026-11-11 — Step 0292: the Main scene gains `ExpAddScanAxis` / `ExpAddScanBlock`.
  - They build the draft's multi-variable scan and write nothing. Each closed block is read on the detectors' capture.
  - Commits record the whole scan as one experiment.
  - New `ExperimentModelTests` (2 facts).
  - Counts: constructor tests 1492 (+2), smoke 249, selected ui-tests 58 (per-class counts unchanged).
