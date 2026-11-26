# Step 0375 — impl plan

## Approach

A Mueller observation carries its own configuration (spec 0044's `MuellerObservation`), so its prediction is the cut
plate of the experiment's one opened crystal, solved by `MuellerInverse.createBerremanForward` at that configuration
(001 §K, §N; .spec-md §3.16). Everything is in `StudyForward.fs`, after step 374's `readingAt`, plus its tests.

1. `type MeasurementConfiguration with member this.elementOrientation : ElementOrientation`: the azimuth's angle as the
   rotation, the incidence as is, `BranchTransmitted` / `BranchReflected` for `TransmittedMueller` / `ReflectedMueller`.
   `ElementOrientation.sampleAzimuth` / `observable` (step 250) then give the configuration's own back, so
   `BoundCutPlate.muellerAt` rebuilds exactly `o.configuration`.
2. `BoundExperiment` gains `observedCrystalOpt : CrystalParameters option` and `solverParameters : SolverParameters`.
   `Experiment.boundWith` takes the `Study` as its second parameter. `solverParameters` is `context.solverParameters`.
   `observedCrystalOpt` is `assignment.crystalOf` of the one copy `sample.openedMaterialsIn study` answers. That holds
   when the drawn slots' `ObjectSlotRole` slots (`SetupSlot.inLightOrder`) hold exactly one item, the kit holds that item,
   and `context.choiceOf` is a `StackSampleChoice (_, _, sample)` with exactly one opened material. Otherwise `None`.
3. `type BoundExperiment with member this.predictedReadingOf (row : MeasuredRow)`:
   - a `SettingsRow` reads `readingAt settings waveLengthOpt`;
   - an `ObservationRow` under `PolarimeterDetectorReadout` with `Some crystal` reads `RecordedMueller` of
     `BoundCutPlate (CutPlate.ofForward (createBerremanForward (fun (c : CrystalParameters) -> c.opticalProperties)
     this.solverParameters) crystal cut thickness)`, evaluated by `muellerAt o.configuration.elementOrientation
     o.configuration.waveLength`. Its `ItemEvaluationError` answers `ForwardSlotErr (SlotEvaluationErr e)`;
   - an `ObservationRow` with `None` answers `ObservationPlateErr` naming the experiment;
   - an `ObservationRow` under any other readout answers `ReadingMismatchErr` naming the readout.
4. `StudyForwardError` gains `ObservationPlateErr of reason : string` and `ReadingMismatchErr of reason : string`, with
   their renders.

Tests (`StudyForwardTests.fs`):

- Step 374's five `boundWith` calls pass `layoutStudy`, the study their context's kit already holds.
- The bench drafts gain step 372's `qzDraft` (the test-local 1 mm plate of the probe uniaxial version that `layoutStudy`
  opens). The new facts run over a material store that also holds the probe crystal, with an assignment whose crystal is
  n_o 1.544, n_e 1.553 and zero gyration.
- Two new facts: `an observation is predicted as a cut plate of its object's opened crystal at its own configuration`
  and `an observation without one opened plate, or under a detector that is no polarimeter, is refused`.
- The error-render fact is widened by the two cases.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/StudyForward.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/StudyForwardTests.fs`

## Risks

- ELEV: two new DU case names (`ObservationPlateErr`, `ReadingMismatchErr`) must be unique. The repo grep finds neither,
  and the elevation gate decides. The readout is rendered inside `predictedReadingOf` by a local match, so no new public
  member is added beyond the authorized contracts.
- `BoundExperiment` equality must still compile (the refusal facts compare `Result<BoundExperiment, _>`).
  `SolverParameters` and `CrystalParameters` are plain records and unions.
- The stack sample of the crystal must resolve while binding, so the new facts' store holds the probe crystal entry.
