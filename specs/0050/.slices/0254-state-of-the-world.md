# Step 0254 — state of the world

## Where we are

Spec 0050, section S8.1 ("The run law walks every object; the source is real"). Step 0252 made the run law
evaluate every object on a light path, and step 0253 added the pure Domain rule `BeamLayout.detectorCaptures`.
This step points the Domain's `detectedBranches` and the host's `captureDetectors` and `availableCaptures` at that
rule, and deletes `measuredElementOf` ("the first bound sample"). Each detector now captures the branch by which
its light leaves the last object on its path.

## What's working

- Re-pointed `BeamLayout.detectedBranches` at `detectorCaptures`: it takes `objects` and answers their distinct
  branches in order.
- Deleted `measuredElementOf`; the host names its objects once, in `objectIds` (the elements bound to a sample).
- Re-pointed `captureDetectors` and `availableCaptures` at the per-object rule, and dropped the host-side
  no-sample T filter.
- A detector on a second sample's reflected ray now offers R, and its chart reads plate 1's T then plate 2's R.
- AGENTS.md's run-law gotcha now says how a detector's capture is decided.

## Tests

No gate verdict is claimed here (Invariant 6). After exit, the arc-runner runs the frozen roster: build,
elevation, constructor-unit-tests (whole), ui-smoke (whole), ui-tests over the six selected classes, and the
fourteen s25 invariants.

The worker's development runs over the final tree used each gate's own command. Logs are under
`specs/0050/.artifacts/0254-*`.

- **build:** exit 0, 0 warnings, 0 errors.
- **constructor-unit-tests:** `Passed!`, 1403 succeeded, 0 failed, 0 skipped, 68 s (deadline 180 s). The count
  is unchanged: the two `BeamLayoutTests` facts now pass `[ Library.ElementId "s" ]` as the objects, with their
  assertions unchanged.
- **ui-smoke:** `Passed!`, 245 succeeded, 0 failed, 0 skipped, 202 s (deadline 600 s).
- **ui-tests (selected):** `Passed!`, 86 succeeded, 0 failed, 0 skipped, 106 s (deadline 360 s). That is 85 plus
  the new fact
  `ElementMenuTests.a detector on the second sample's reflected ray offers R, decided by the last object on its path`.
- **elevation:** `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, 195 s (deadline 600 s), known findings 0.
- **The fourteen s25 invariants,** by their greps, all find nothing. So does
  `git grep -n -F "measuredElementOf" -- Berreman/`.
- **Red, measured first:** before the fix, the new fact failed on the offer
  (`Expected: [CaptureTransmitted, CaptureReflected, CaptureBoth]`, `Actual: [CaptureTransmitted]`). Its setup
  assertions passed.
- **Extra check (not a gate):** the whole non-smoke UI suite passed 675 tests (674 + 1), 0 failed, 0 skipped.
- No development run exceeded its deadline.

```yaml
gates:
  constructor_unit_tests: 1403
  ui_smoke_tests: 245
  ui_tests: 86
```

## Architecture

- **One rule decides every capture.** `detectedBranches` is now `detectorCaptures` reduced to its distinct
  branches. So what each detector captures (`captureDetectors`, read by `chartForRun` and `captureLabel`) and what
  an experiment may offer (`availableCaptures`) cannot disagree.
- **The host says what an object is in one place:** `objectIds`, the elements bound to a library sample in scene
  order. `detectorIds` beside it lists the scene's `Detector` elements, so both callers share one list.
- The no-sample pass-through rule (only light the varied element transmits) is the Domain rule's (step 253). The
  host keeps no filter of its own.

## Deferred

Nothing in this step's scope is deferred.

## Gotchas

- **Deviation: `member internal`, not `member private`.** The slice names `member private model.objectIds`. But
  `availableCaptures` is a module-level `let`, and F# refuses a private member of an intrinsic augmentation
  outside its type (FS0491, measured). `internal` is this file's established accessibility for helper members
  that module functions read (`withViewRotationsReset`, `wavelengthRange`). `detectorIds` follows it.
- **Behaviour change where a captured path holds no bound sample.** Before, nothing was captured there, because
  the path missed the first bound sample. Now T is captured when the varied element transmits to it (step 253's
  no-object rule). Every capture whose path holds the one bound sample is unchanged.
- `detectedBranches` moved below `detectorCaptures` in `BeamLayout.fs`, because F# declaration order requires it.
- A first build after MathNet's obj is stale prints its two pre-existing `SYSLIB0051` warnings (vendored C#). The
  next build is clean.

## Changelog

- 2026-11-10 — Step 0254: the host captures per object (spec 0050 S8.1). `detectedBranches`, `captureDetectors` and
  `availableCaptures` read `BeamLayout.detectorCaptures`, and `measuredElementOf` is deleted. One `ElementMenuTests`
  fact was added (ui-tests 85 → 86; constructor 1403 and smoke 245 unchanged).
