# Step 0482 — impl log

## Progress

- Read the task, both system prompts, the project prompt, rulings R0001–R0005 (none touches this step's files; their
  preserved behaviors live outside `MeasurementPlanner.fs` / `PlannerInputsTests.fs` and are untouched), the slice and
  its gates snapshot.
- Implemented, in `MeasurementPlanner.fs` right after step 477's `CandidateRows`:
  - `PlanCoverage = CandidateInKit of ExperimentName | CandidateNotInKit` (derived, no wire tag);
  - `PlanCandidate = { source; plate; cut; observable }` with `static member incidenceRange` (0…60°, 4 points),
    `static member azimuthRange` (0…67.5°, 4 points), `member capture`, `member caption` and
    `member coverageIn setups experiments`;
  - a `type PlannerInputs with` block (same file and module, so intrinsic members) with `polarimeterOpt`,
    `unplacedPlates` and `candidates`.
- Added `open` of `Propagation` (`SweptValue`, `CurvePointCount`), `Experiments` and `Setups` to `MeasurementPlanner.fs`,
  placed before `Studies` so every name the file already resolved still resolves the same way.
- Added the QZ-T plate (EulerRotation ZmYpXp, theta 30°, `EmitBoth`) and the two facts to `PlannerInputsTests`.
- Build 1 failed (FS0039 `SweptValue` / `CurvePointCount`: they live in `Propagation`); added the open; build 2 green.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/MeasurementPlanner.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/PlannerInputsTests.fs`

## Decisions

- **Reuse over copy.** `candidates` needs the same emission → branches mapping `PlannerInputs.looks` already had inline.
  Rather than duplicate the match, it moved into one private extension member `Emission.plannerObservables` (private to
  the `MeasurementPlanner` module, as the file's `Angle.plannerDegreesText` already is) and both `looks` and
  `candidates` read it. `looks`' output is unchanged (its fact still passes). The slice's "No cache, store or other
  member is added" is read as "no other planner API": a module-private helper adds no public surface and no contract.
- **Coverage compares exactly what the slice enumerates**: the setup resolves (`slotsIn`), its Object 1 slot
  (`ObjectSlotRole (SlotPosition 1)` of `SetupSlot.inLightOrder`) holds the plate's item, the scan is exactly one block
  whose capture is `this.capture` or `CaptureBoth`, whose axes, labels blanked, equal `[ VaryR2 over incidenceRange;
  VaryR1 over azimuthRange ]` on that slot, and `sourceWaveLengthOpt` is `None`. Role is not compared.
- **The cut of a plate** is the first of `[ ZCut; XCut; YCut ]` whose `normalAxisOpt` equals the plate's; a plate with
  `None` matches no principal cut (each has `Some` axis), so it yields no candidate and appears in `unplacedPlates`.
- **Caption wavelength** goes through `Units.wavelengthToUnit Units.Nanometer`, the sole unit-conversion seam, formatted
  `F1` under `CultureInfo.InvariantCulture`.
- **Test strengthening within the two authorized identities** (no identity added or renamed): a second planner source
  (532 nm) checks source-outer order and the caption's line; coverage also checks that a `PlannedRun` holds the
  candidate, that the first holding experiment is named, that QZ-X's transmitted candidate is held by its own bench,
  that an unresolved setup or a set `sourceWaveLengthOpt` holds nothing, and that a range differing only in its point
  count holds nothing.

## Testing state

- `dotnet build Berreman.slnx -c Release` (from `Berreman/`): green, 63 s, 0 errors; the only warnings are NU1902
  (exempt third-party advisories). Log: `.artifacts/0482-build-02.log` (the failing first build: `0482-build-01.log`).
- `OpticalConstructor.Tests.PlannerInputsTests` filtered: 11 passed, 0 failed (9 existing + 2 new).
  Log: `.artifacts/0482-planner-inputs-tests.log`.
- `constructor-unit-tests` whole suite (the gate's command): Passed!, succeeded 1877, failed 0, skipped 0, 118 s
  (deadline 180 s; run concurrently with the elevation analyzers). Prior checkpoint 478 / SoW 479: 1875, so +2 exactly.
  Log: `.artifacts/0482-constructor-unit-tests.log`.
- The 21 `invariant-s46-*` must-find-nothing greps, decoded from `.slices/0482.gates`
  (`.artifacts/0482-invariant-commands.txt`): every one finds nothing.
- Elevation (`Elevation/Run-ElevationGate.ps1 -ExcludeRules ELEV004`, no base, so all 13 projects):
  `ELEV-GATE: OK - 0 new finding(s) in 13 project(s), 234s`, known findings 0; Domain 0 findings, Ui 0 findings.
  Deadline 600 s. Log: `.artifacts/0482-elevation.log`.
- No run exceeded its effective gate deadline.

## Artifacts

- `specs/0050/.artifacts/0482-build-01.log`, `0482-build-02.log`
- `specs/0050/.artifacts/0482-planner-inputs-tests.log`
- `specs/0050/.artifacts/0482-constructor-unit-tests.log`
- `specs/0050/.artifacts/0482-invariant-commands.txt`
- `specs/0050/.artifacts/0482-elevation.log`

## Gotchas

- `SweptValue` and `CurvePointCount` are declared in `Propagation.fs`, not `Experiments.fs`; a file building a
  `VariableRange` literal must open `OpticalConstructor.Domain.Propagation`.
- `coverageIn` does not compare the setup's source slot with the candidate's source: the slice enumerates the
  conditions (plate in Object 1, one block, branch, R2-then-R1 ranges, no set line) and leaves the source out. With two
  sources, one matching experiment therefore holds the candidates of both lines. Recorded, not changed, because the
  slice and its acceptance name the comparison exactly.
- `{ plateOf qzT … ThirdCrystalAxis with normalAxisOpt = None }` in the fact is only the existing helper reused for
  the tilted plate's expected record; the axis argument is overwritten.
