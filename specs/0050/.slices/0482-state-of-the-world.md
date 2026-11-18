# Step 0482 — state of the world

## Where we are

Spec 0050, section S14.2 (the Planner's numerical layer, the plan, the cache and the Plan view). Steps 0470–0472
gave the planner its inputs (lab profile, plates, prior, targets) and its structural report; 0477–0479 gave a
candidate's information (`CandidateRows`, Fisher information, bench noise). This step 0482 lands the kit's candidate
experiments themselves (obligation S142-KIT-CANDIDATES): which plate, cut, branch and line a Mueller polarimeter
could read, swept over the spec 0044 designs' incidence × azimuth grid, and whether the kit already holds each one.

## What's working

- Add `PlanCandidate` (source, plate, cut, observable) with the planner's constant incidence (0–60°) and azimuth
  (0–67.5°) ranges, its capture and its caption (`QZ-Z (z-cut plate), transmitted, 632.8 nm`).
- Add `PlanCoverage` and `PlanCandidate.coverageIn`: the first experiment whose one block sweeps the plate's R2 then R1
  over those ranges on its branch (or both) in Object 1, at the source's own line, whatever its role.
- Add `PlannerInputs.polarimeterOpt`, `unplacedPlates` and `candidates` (source × placed plate × emitted branch, none
  without a polarimeter).
- Share the emission → branches mapping between `looks` and `candidates` through one module-private member.
- Add the two `PlannerInputsTests` facts and the tilted QZ-T plate; constructor-unit-tests 1875 → 1877.

## Tests

- `build`: green (63 s; only the exempt NU1902 advisories). `.artifacts/0482-build-02.log`.
- `constructor-unit-tests` (whole suite, the gate's own command): Passed!, succeeded 1877, failed 0, skipped 0, in
  118 s against a 180 s deadline (run alongside the elevation analyzers). The count rises by exactly 2 over
  checkpoint 478's 1875: `a kit's candidates are each placed plate on each branch it emits at each source's line, read
  by its polarimeter` and `a candidate is in the kit once an experiment over its plate sweeps its angles on its
  branch`, both passing. `.artifacts/0482-constructor-unit-tests.log`.
- `elevation` (standalone, no base, so all 13 projects; ELEV004 excluded): `ELEV-GATE: OK - 0 new finding(s) in 13
  project(s)`, known findings 0, 234 s against a 600 s deadline. `.artifacts/0482-elevation.log`.
- The 21 `invariant-s46-*` greps: each finds nothing (`.artifacts/0482-invariant-commands.txt` lists them).
- No gate exceeded its effective deadline.

```yaml
gates:
  constructor_unit_tests: 1877
```

## Architecture

- `PlanCandidate` and `PlanCoverage` live in `MeasurementPlanner.fs` after step 477's `CandidateRows`; the three new
  `PlannerInputs` members sit in a `type PlannerInputs with` block in the same file and module, so they compile as
  intrinsic members (the contract symbols `MeasurementPlanner.PlannerInputs.polarimeterOpt` / `unplacedPlates` /
  `candidates`).
- The ranges are static members (constants of the planner, .spec-md §8: no configuration knob), built from A1's
  azimuths and A2's incidences with normal incidence.
- A plate's cut is recovered from its `normalAxisOpt` through `SampleCut.normalAxisOpt` (step 466) over the three
  principal cuts; a plate with no principal axis on its normal has no candidate and is listed by `unplacedPlates`
  (departing from step 467's note that layer 2 measures such a plate, as the slice records).
- Only a full Mueller polarimeter reads a candidate; an intensity detector, even in the HAUP class, yields none.
- `coverageIn` is pure over the kit's setups and experiments; nothing is cached or stored.

## Deferred

Nothing. Every requirement of the slice landed in this step.

## Gotchas

- `SweptValue` / `CurvePointCount` are declared in `Propagation.fs`; a `VariableRange` literal needs
  `open OpticalConstructor.Domain.Propagation`.
- `coverageIn` does not compare the setup's source slot with the candidate's source, exactly as the slice enumerates
  its conditions: in a two-source kit, one matching experiment holds both lines' candidates of that plate and branch.
- Labels are blanked before the axes are compared, so a scan's captured element label never decides coverage, while
  the element id, variable and full range (min, max and point count) do.

## Changelog

- 2026-11-18 — Step 0482: kit candidates (`PlanCandidate`, `PlanCoverage`, `PlannerInputs.polarimeterOpt` /
  `unplacedPlates` / `candidates`) and their two `PlannerInputsTests` facts.
