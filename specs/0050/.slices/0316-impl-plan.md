# Step 0316 — impl plan

## Approach

S9.3's setup correction: re-point an experiment's setup after the fact, with a mandatory audit note, through a
semantic message. The lab file is never read or written, and the experiment keeps its name and its file.

1. **Domain** (`Experiments.fs`), beside `withRole`: `Experiment.correctedTo setup note`. It answers the experiment
   over `setup`, with `{ previous = this.setup; corrected = setup; note }` appended to `corrections`. Every other field
   is kept. Pinning follows from `Setup.pinFor`, which reads `experiment.setup`, so nothing else changes there.
2. **Ui** (`TableAndElementRotationView.fs`):
   - `SetCorrectionNote of ControlCaption` sets `dataDraft.correctionNote` and writes nothing.
   - `CorrectSetup of ExperimentId * SetupReference * ControlCaption` sits behind `guardKitWrite`. Its refusals, in
     the slice's order:
     1. an absent experiment changes nothing;
     2. a blank note;
     3. a reference no kit setup answers at its revision;
     4. the experiment's own reference.
   - The kit check reuses the one lookup, `Experiment.slotsIn` (find by name, then `slotsAt`), over the corrected
     experiment. It is not a second copy.
   - On success the arm reuses `withExperimentDataRecorded` (step 315). That records through
     `KitLibraryContext.recordExperiment`, stores with `withStoredExperiment`, re-badges through
     `checkExperimentData`, re-reads the review when it is this experiment's, and sets the told status. Its doc is
     widened to say "its data or its setup changed".
3. **Tests:**
   - `ExperimentProxyTests`: one new fact over a test-local `CPL-(LR#45)-CPL` experiment, corrected and corrected back.
   - `ExperimentModelTests`: a correction fixture built on step 315's `withDataStepKit`. It selects the polarizer,
     turns it to R1 30° and records it with `CreateSetup` as `Bench B`. Two new facts run over it.
   - `KitWindowModelTests`: the read-only fact also sends `CorrectSetup`.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/Experiments.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/TableAndElementRotationView.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/ExperimentProxyTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/ExperimentModelTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitWindowModelTests.fs`

## Risks

- Selecting the polarizer: there is no select-by-id message. The test sets `selection` the way
  `PlacedSampleEmissionTests` does.
- ELEV: `correctedTo` is a member, so it is exempt from the free-function rules. `CorrectSetup`'s three-element payload
  has three distinct types (ELEV011). New strings appear only inside interpolations.
- The fixture holds E2 as well, from `withDataStepKit`. It is over `Setup 1` revision 1 and is untouched by the
  correction.
