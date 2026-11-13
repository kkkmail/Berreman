# Step 0374 — impl plan

## Approach

The forward model of one experiment at one scan point, over S8.1's evaluator, as the how-to lays it out.

1. `ExperimentRun.fs`, after `read`: `type RunStep with static member productOf`, the steps' matrices folded in light
   order from `Propagation.identityMueller` (`step.mueller * product`).
2. `StudyForward.fs`, after `assignmentFor`:
   - `ParameterValue.darkMean` (deconstructs the value, as `ParameterValue.angle` / `thickness` do, so no `.value`
     re-wrap);
   - `DetectorReadout` (`IntensityDetectorReadout of gain * dark`, `PolarimeterDetectorReadout`,
     `EllipsometerDetectorReadout`);
   - `BoundPathStep` and `BoundExperiment` records, exactly the fields the how-to names;
   - `Experiment.boundWith context assignment darkOpt`: slots from `slotsIn` (→ `ForwardSetupErr`), drawn with the kit's
     items, laid out by `BeamLayout.layout`; the `SlotRunParam` over the assignment (`materialProxyOver`, `valuesOf`);
     emission by `SetupSlot.runEmissionOf`; the operating wavelength by `SourcePreset.operatingWaveLengthOf` over
     `boundSourceIn`; the first `DetectorSlotRole` slot holding an item of a detector form (else `StudyDetectorErr`
     naming the setup reference) and its readout; the detector's placed `path`, each step's slot bound by
     `runElementWith` and kept when `Some`, with its branch, drawn incidence and its item's dial and dial zero (from the
     assignment's values);
   - `BoundExperiment.readingAt settings waveLengthOpt`: the wavelength, each step's rotation and incidence by the
     how-to's precedence, `muellerAt` → `runStepOf`, then S₀ · gain + dark / Ψ, Δ / `RunStep.productOf`;
   - `StudyForwardError` gains `ForwardSetupErr`, `ForwardSlotErr`, `StudyDetectorErr` and their renders.
3. Tests: `ExperimentRunTests` gains the product fact; `StudyForwardTests` gains the four forward facts and the widened
   render fact, over a HeNe source item, an ideal-LP preset item (prep), a real-LP item (analyzer), an optional glass
   plate item and a detector item of each form.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/ExperimentRun.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/StudyForward.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/ExperimentRunTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/StudyForwardTests.fs`

## Risks

- ELEV: new DU case and type names must be unique (checked: none exists); no `Ctor x.value` re-wrap.
- `read` returns the generic `Reading`; the intensity and ellipsometer arms match it, the other arm being the honest
  mapping of the other reading.
- The built-in glass plate (n = 1.52, non-dispersive) may not depend on λ, so the retune assertion also uses a
  wavelength-dependent element to stay discriminating.
- `FsxAcceptanceTests` (ui-tests) is untouched by this step; it must stay green (no UI code changes).
