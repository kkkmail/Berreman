# Step 0367 — state of the world

## Where we are

Spec 0050, section S11.2 (the forward model of an experiment and the study residual).

- The forward model has to evaluate a RECORDED setup in Domain. Until now, three rules it needs lived only inside
  the table's run in the UI.
- This step moves them into Domain, following 001 §N "One item-model evaluator", .spec-md §4 "A reuse is a move or
  a call" and AGENTS.md "Reuse, never copy". The table now calls them, and nothing it draws, lays out or runs
  changes.

## What's working

- Moved the drawn incidence into Domain as `BeamLayout.PlacedElement.incidenceOn`. Added
  `BeamTree.BeamBranch.propagationBranch`, the branch mapping `chartForRun` used to write inline.
- Moved the bound-source rule into Domain as `Setups.SetupSlot.boundSourceIn slots library`.
- Moved the operating-wavelength rule into Domain as `ExperimentRun.SourcePreset.operatingWaveLengthOf sourceOpt
  requested`.
- The table's `chartForRun`, `boundSource` and `operatingWaveLength` call the Domain members. The private UI
  `incidenceOn` is deleted, so no copy is left in the UI.
- Added three constructor-unit facts (`BeamLayoutTests`, `SetupTests`, `ExperimentRunTests`) over the moved rules.

## Tests

These are development runs of each gate's own command from `0367.gates`, over the final tree:

- **build:** exit 0, `0 Error(s)`, 57 s (deadline 600 s).
  - The only warnings are the exempt NU1902.
  - The vendored MathNet SYSLIB0051 pair appeared on the non-incremental build only.
- **elevation:** `ELEV-GATE: OK - 0 new finding(s) in 13 project(s), 227s` (deadline 600 s).
  - It ran from the arc origin through `ASG_ARC_BASE_COMMIT`.
  - 1520 ELEV004 findings suppressed, 0 known findings.
- **constructor-unit-tests:** `Passed!`, 1611 of 1611 (1608 + 3), 71 s (deadline 180 s).
  - Passed: `the drawn incidence is the angle between an element's drawn face normal and its arriving beam`.
  - Passed: `the bound source is the first light-source slot bound to a source preset`.
  - Passed: `the run's wavelength is a tunable source's clamped request, a fixed line's own, or 600 nm without a source`.
- **ui-smoke:** `Passed!`, 258 of 258, 227 s (allowance 600 s).
- **ui-tests** (the seven selected classes): `Passed!`, 101 of 101, 105 s (deadline 360 s).
  - No UI test file changed, so the count is unchanged.
- **invariant-s37-\* (20):** each decoded `--find-nothing` grep finds nothing over the final tree.

No run went past its deadline.

```yaml
gates:
  constructor_unit_tests: 1611
  ui_smoke_tests: 258
  ui_tests: 101
```

## Architecture

- **`PlacedElement.incidenceOn` is an intrinsic member.** It is declared in `BeamLayout`, the module that declares
  `PlacedElement`.
  - It calls `RayModel.faceNormal d placement` for the drawn face normal. That function's body is exactly the
    `r1Axis (RayModel.orientedToBeam d placement)` the UI wrote, so the value is identical and no formula is
    re-transcribed.
  - The rest of the body is moved verbatim: the cap at `Propagation.r2SweepMaxDegrees`, and 0° for the root.
- **`SetupSlot.boundSourceIn` is an intrinsic static member**, in the `SetupSlot` block right after `inLightOrder`.
- **`BeamBranch.propagationBranch` and `SourcePreset.operatingWaveLengthOf` are optional extensions.** They are
  declared in `BeamLayout` and `ExperimentRun`, so the table's view now opens those two modules. Names are unique
  across our assemblies (ELEV014/015/016), so the opens shadow nothing of ours.
- **`operatingWaveLengthOf` clamps through the one units seam.** It uses `Units.wavelengthToUnit Nanometer` and
  `Units.toWaveLength Nanometer`, which perform the same float operations as the UI's private `nanometers`
  projection. Every run's wavelength is therefore bit for bit what it was.

## Deferred

Nothing. Every requirement of the step landed in this round.

## Gotchas

- With `Berreman.Fields` not open, `Berreman.Fields.IncidenceAngle.create` resolves `IncidenceAngle` to the union
  case (FS0039). Open the module instead.
- An optional extension member is invisible until its declaring module is opened. Writing the qualified type name
  is not enough.
- `WaveLength` is `Mkm | Nm`, so it must never be clamped or compared structurally. Project it to a unit first.
- The UI keeps its private `WaveLength.nanometers` for its 10 other uses (sweep clamping, wavelength range, bay
  settings).

## Changelog

- 2026-11-13: step 0367. Moved the drawn incidence, the beam-branch mapping, the bound source and the operating
  wavelength from the table's run into Domain (S11.2). The table calls them. Added three constructor-unit facts.
