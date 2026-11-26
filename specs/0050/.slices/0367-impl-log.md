# Step 0367 — impl-log

## Progress

- [x] Read the protocol, the project prompt, AGENTS.md, the slice and its `.gates` snapshot. There is no operator note
  and no operator ruling for this step.
- [x] Domain `BeamLayout.fs`, after `layout`:
  - `type PlacedElement with member placed.incidenceOn (placement : ElementPlacement) : IncidenceAngle`;
  - `type BeamTree.BeamBranch with member branch.propagationBranch : Propagation.Branch`;
  - the file now opens `Berreman.Fields` and `Berreman.Geometry`, before `Placement`, as `RayModel.fs` does. A fully
    qualified `Berreman.Fields.IncidenceAngle.create` resolved to the union case, not the type (FS0039).
- [x] Domain `Setups.fs`: `static member boundSourceIn (slots) (library) : SourcePreset option`, in the `SetupSlot`
  block right after `inLightOrder`.
- [x] Domain `ExperimentRun.fs`, after `read`: `type SourcePreset with static member operatingWaveLengthOf`. The
  file now opens `Berreman.Constants` for the `nm` measure.
- [x] UI `TableAndElementRotationView.fs`:
  - the private `PlacedElement.incidenceOn` extension is deleted;
  - `chartForRun` calls the Domain member and `step.branch.propagationBranch`;
  - `boundSource` is `SetupSlot.boundSourceIn model.elements model.library`;
  - `operatingWaveLength` is `Library.SourcePreset.operatingWaveLengthOf model.boundSource requested`;
  - each doc says in one line that the rule is Domain's;
  - added `open OpticalConstructor.Domain.BeamLayout` and `open OpticalConstructor.Domain.ExperimentRun`, because
    F# only shows an optional extension member when its module is open.
- [x] Tests: the three facts the step names.
- [x] Development runs of build, constructor-unit-tests, ui-smoke, the selected ui-tests, elevation and the 20 s37
  invariants.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/BeamLayout.fs` (+24)
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/Setups.fs` (+12)
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/ExperimentRun.fs` (+16)
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/TableAndElementRotationView.fs` (+8 / −39)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/BeamLayoutTests.fs` (+12)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/SetupTests.fs` (+17)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/ExperimentRunTests.fs` (+17)

Every file is LF: `git diff --numstat` matches `--ignore-cr-at-eol`.

## Decisions

- **Face normal by call, not by re-transcription.** The UI computed the face normal as
  `r1Axis (RayModel.orientedToBeam d placement)`. That is exactly the body of `RayModel.faceNormal incoming p`
  (`RayModel.fs:144-145`), so the Domain member calls `RayModel.faceNormal d placement`. The value is the same; the
  rest of the body (the clamp to 1, the `r2SweepMaxDegrees` cap, 0° for the root) is moved verbatim. This follows
  AGENTS.md "Reuse, never copy".
- **The nanometre clamp goes through the units seam.**
  - The UI clamped through its private `WaveLength.nanometers` (`w.value / nmToMeter / oneNanometer`), and
    `WaveLength.nm (x * 1.0<nm>)` rebuilt the wavelength.
  - Domain uses `Units.wavelengthToUnit Units.Nanometer` (`(m / nmToMeter) / 1.0<nm>`) and
    `Units.toWaveLength Units.Nanometer` (`WaveLength.Nm (x * 1.0<nm>)`). These are the same floating-point
    operations in the same order, so every run's wavelength is bit for bit what it was.
  - `Units` calls itself "the SOLE conversion seam", so no third copy of the nm conversion is written.
  - The clamp takes `max` / `min` over `UnitMagnitude`, so there is no `.value` and no re-wrap. Structural comparison
    of a one-float union picks the same operand as the float comparison.
- **The private UI `WaveLength.nanometers` stays.** It still has 10 other uses in the UI, and the slice allows no
  other UI line to change.
- **`boundSourceIn` is in the existing `type SetupSlot with` block right after `inLightOrder`**, so it is an
  intrinsic static member. `incidenceOn` is also intrinsic, because `PlacedElement` is declared in the same module.
  `propagationBranch` and `operatingWaveLengthOf` are optional extensions, hence the two UI opens. Our names are
  unique (ELEV014/015/016), so those opens shadow nothing of ours. Every other unqualified name the opens could
  catch (`read`, `layout`) is a local binding in the UI.
- **Test extras.**
  - `SetupTests` also asserts that a polarizer slot bound to the HeNe preset binds no source (the light-source filter).
  - `ExperimentRunTests` also checks the fixed line and the no-source cases with each kind of request.
  - Tolerances use xUnit's `Assert.Equal(double, double, tolerance)` (as `SetupTests.assertAngle` does) or the file's
    own `close` (BeamLayoutTests, 1e-9).

## Testing state

These are development runs of each gate's own command from `0367.gates`, over the final tree. Logs are in
`specs/0050/.artifacts/`.

- **build:** exit 0, `0 Error(s)`, 57 s (deadline 600 s).
  - The only warnings are the exempt NU1902.
  - The vendored MathNet SYSLIB0051 pair appeared on the first, non-incremental build only.
  - Log: `0367-build-local.log`.
  - The first attempt failed with FS0039 on the qualified `IncidenceAngle.create`. It was fixed with the opens above.
- **constructor-unit-tests:** `Passed!`, 1611 of 1611, 71 s (deadline 180 s).
  - That is 1608 (checkpoint 365) + 3.
  - Passed: `the drawn incidence is the angle between an element's drawn face normal and its arriving beam`.
  - Passed: `the bound source is the first light-source slot bound to a source preset`.
  - Passed: `the run's wavelength is a tunable source's clamped request, a fixed line's own, or 600 nm without a source`.
  - Log: `0367-constructor-unit-tests-local.log`.
- **ui-smoke:** `Passed!`, 258 of 258 (baseline 258), 227 s (allowance 600 s). Log: `0367-ui-smoke-local.log`.
- **ui-tests** (the 7 selected classes): `Passed!`, 101 of 101, 105 s (deadline 360 s).
  - No Ui.Tests file changed, so the count is unchanged.
  - Log: `0367-ui-tests-local.log`.
- **elevation:** `ELEV-GATE: OK - 0 new finding(s) in 13 project(s), 227s` (deadline 600 s).
  - It ran with `ASG_ARC_BASE_COMMIT` set to the `.arc-base-json` origin. Because the slnx changed in the arc, it
    covered all 13 projects.
  - 1520 ELEV004 findings suppressed, 0 known findings.
  - Log: `0367-elevation-local.log`.
- **invariant-s37-\* (20):** each decoded `--find-nothing` grep finds nothing.

No run went past its deadline.

TDD note: the three facts name members that did not exist before this step, so before the move they are red by
compile error (the symbol is undefined). The Domain members were written first and the facts then, in the same round.

## Artifacts

- `specs/0050/.artifacts/0367-build-local.log`
- `specs/0050/.artifacts/0367-constructor-unit-tests-local.log`
- `specs/0050/.artifacts/0367-ui-smoke-local.log`
- `specs/0050/.artifacts/0367-ui-tests-local.log`
- `specs/0050/.artifacts/0367-elevation-local.log`

## Gotchas

- `Berreman.Fields.IncidenceAngle.create`, fully qualified, resolves `IncidenceAngle` to the union CASE and fails
  with FS0039. Open `Berreman.Fields` and use `IncidenceAngle.create`.
- An optional extension member (here `BeamBranch.propagationBranch` and `SourcePreset.operatingWaveLengthOf`) is
  invisible until its module is opened. A qualified module path does not bring it into scope.
- `WaveLength` is a two-case union (`Mkm | Nm`), so it must never be compared structurally. Clamp through a unit
  projection (`Units.wavelengthToUnit`), as `operatingWaveLengthOf` does.
