# Step 0253 — state of the world

## Where we are

Spec 0050, section S8.1 ("The run law walks every object; the source is real"). Step 0252 made the run law
evaluate every object on a light path. This step adds the ONE pure Domain rule that decides which branch each
detector captures when any number of objects sit in series. Step 0254 re-points `detectedBranches` and the host
(`captureDetectors`, `availableCaptures`) at it and deletes `measuredElementOf`.

## What's working

- Added `BeamLayout.DetectorCapture`: one detector an experiment captures, and the branch it captures.
- Added `BeamLayout.detectorCaptures`: each lit detector that sees the varied element captures the branch its
  light leaves the last object on its path by, in scene order.
- A detector whose path holds no object captures only light the varied element transmits (the no-sample rule).
- Two new `BeamLayoutTests` facts cover two samples in series and the no-object pass-through.

## Tests

No gate verdict is claimed here (Invariant 6). After exit, the arc-runner runs the frozen roster: build,
elevation, constructor-unit-tests over everything discovered, and the fourteen s25 invariants.

The worker's development runs over the final tree, logs under `specs/0050/.artifacts/0253-*`:

- **build:** exit 0, `0 Warning(s)`, `0 Error(s)`. The round's first build recompiled the vendored MathNet C# and
  printed its two pre-existing `SYSLIB0051` warnings; the final build printed none.
- **constructor-unit-tests,** the gate's command: `Passed!`, 1403 succeeded, 0 failed, 0 skipped, in 68 s
  (estimate 180 s). That is checkpoint 252's 1401 plus the two declared facts:
  - `each detector captures the branch its light leaves the last object by`
  - `a detector whose path holds no object captures only light the varied element transmits`
- **elevation,** the gate's command over all 12 projects: `ELEV-GATE: OK - 0 new finding(s)`, 203 s (estimate
  600 s). Known findings: 0.
- **The fourteen s25 invariants,** by their greps: all find nothing.
- **Red, measured by mutation:** "first object" in place of "last object", and no T filter without an object,
  turned exactly the two new facts red (`BeamLayoutTests` 23 total, 2 failed). Restored and rebuilt before the
  final runs.
- No development run came near a deadline. No existing assertion is touched.

```yaml
gates:
  constructor_unit_tests: 1403
  ui_smoke_tests: 245
  ui_tests: 85
```

## Architecture

- **One rule, in Domain:** capture is decided per object over the laid-out beam tree (`PlacedElement.path`), so
  it is provable headless and the host only supplies its objects (step 254: the bound samples, in scene order).
- **The last object wins:** a detector receives the light that left the last object on its path, each object
  solved for its own branch (step 252); earlier objects only shape what reaches it.
- **No object means a pass-through:** with nothing to reflect, only light the varied element transmits is
  captured. This is the rule `availableCaptures` applied as a host-side filter, now in Domain.
- `detectedBranches` is unchanged in this step; no other type or function is added.

## Deferred

Nothing in this step's scope is deferred. Re-pointing `detectedBranches`, `captureDetectors` and
`availableCaptures`, and deleting `measuredElementOf`, is step 0254's declared scope.

## Gotchas

- "With one object on every path this is today's rule" holds only when every captured path holds that object. A
  detector that sees the varied element but not the object falls under the no-object rule (T when the varied
  element transmits to it). No caller uses `detectorCaptures` yet, so no behaviour changes in this step.
- The record's `detector` / `branch` labels shadow earlier labels for un-annotated field lookups in files that
  open `BeamLayout`; every existing site is typed or piped.
- A first build after MathNet's obj is stale prints its two pre-existing `SYSLIB0051` warnings; a second is clean.

## Changelog

- 2026-11-10 — Step 0253: added `BeamLayout.DetectorCapture` and `BeamLayout.detectorCaptures` (the per-object
  capture rule, spec 0050 S8.1) with two `BeamLayoutTests` facts; constructor tests 1401 → 1403.
