# Step 0367 — implementation plan

Move three run rules (plus the beam-branch mapping) from the table's run in
`OpticalConstructor.Ui/TableAndElementRotationView.fs` into Domain, so the forward model of a
recorded setup (spec 0050 S11.2) can call them; the table then calls the Domain members.

## Moves

1. `OpticalConstructor.Domain/BeamLayout.fs`, after `layout`:
   - `type PlacedElement with member this.incidenceOn (placement : ElementPlacement) : IncidenceAngle` —
     the private UI extension's body; the drawn face normal is `RayModel.faceNormal d placement`, which
     IS `r1Axis (RayModel.orientedToBeam d placement)` (`RayModel.fs:144`), so the same number is
     computed by a call rather than a re-transcribed expression.
   - `type BeamTree.BeamBranch with member this.propagationBranch : Propagation.Branch` — the inline
     match `chartForRun` writes today.
2. `OpticalConstructor.Domain/Setups.fs`, beside `SetupSlot.inLightOrder`:
   `static member boundSourceIn (slots : SetupSlot list) (library : LibraryProxy) : SourcePreset option`.
3. `OpticalConstructor.Domain/ExperimentRun.fs`, after `read`:
   `type SourcePreset with static member operatingWaveLengthOf (sourceOpt) (requested) : WaveLength`.
   The UI clamps in nanometres through its private `WaveLength.nanometers`; Domain reaches the same
   arithmetic through the one conversion seam, `Units.wavelengthToUnit Nanometer` /
   `Units.toWaveLength Nanometer` (identical operations: `w.value / nmToMeter / 1 nm` and
   `WaveLength.Nm (x * 1 nm)`), so every run's wavelength is bit-for-bit what it was.
4. UI: delete the private `incidenceOn`; `chartForRun` calls the Domain member and
   `step.branch.propagationBranch`; `model.boundSource` = `SetupSlot.boundSourceIn model.elements model.library`;
   `operatingWaveLength model requested` = `SourcePreset.operatingWaveLengthOf model.boundSource requested`.

## Tests (3 new facts)

- `BeamLayoutTests` — the drawn incidence (30°, 0°, root 0°, 89.5° capped at 89°) and the branch mapping.
- `SetupTests` — the bound source over `BuiltinCatalogue.library ()`.
- `ExperimentRunTests` — the operating wavelength (clamped tunable, fixed line, 600 nm default).

## Risks

- ELEV: new members are on our types (member, not free functions); no rewrap at call sites.
- Floating-point: untilted incidence must be exactly 0 within 1e-9 (the face normal is (1,0,0) exactly on the
  main beam).
- The private UI `WaveLength.nanometers` stays: other UI lines use it and the slice forbids other UI changes.
