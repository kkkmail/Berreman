# Step 0375 — state of the world

## Where we are

This is spec 0050 section S11.2, the forward model of an experiment and the study residual. Step 374 bound an
experiment at a parameter vector and predicted its detector item's reading at a scan point's settings. This step
predicts the other kind of measured row, the Mueller observation, which carries its own configuration (001 §K). The
observation is the cut plate of the experiment's one opened crystal, solved through the spec 0044 Berreman forward
(`MuellerInverse.createBerremanForward`) at that configuration (001 §N; .spec-md §3.16). `predictedReadingOf` now
predicts every `MeasuredRow`. The later S11.2 steps compare those predictions with the measured rows.

## What's working

- Add `BoundExperiment.predictedReadingOf`: a settings row reads as `readingAt` does, and a Mueller observation as the cut plate of the observed crystal through `createBerremanForward` at its own configuration
- Add `MeasurementConfiguration.elementOrientation`, whose `sampleAzimuth` and `observable` give the configuration back
- `Experiment.boundWith` takes the study and binds `observedCrystalOpt` (the lone object item's sample made of one opened material, at its assigned crystal) and `solverParameters`
- Add `ObservationPlateErr` and `ReadingMismatchErr` to `StudyForwardError`
- Add two observation facts and widen the error-render fact (constructor suite 1634 → 1636)

## Tests

```yaml
gates:
  constructor_unit_tests: 1636
```

- build (`dotnet build Berreman.slnx -c Release -nologo -v:m`): exit 0, `0 Error(s)`, no lowercase `error` in the log.
  The 12 warning lines are all NU1902 advisories, which are exempt. The Tests project built alone with
  `0 Warning(s)`. 27 s (deadline 600 s).
- constructor-unit-tests (the gate's own command, whole suite): `Test run summary: Passed!`, 1636 succeeded (1634 at
  checkpoint 374, +2), 0 failed, 0 skipped. 72 s (deadline 180 s). In the TRX, both new facts and the widened
  `every forward error renders its condition and its payload` are `Passed` under exactly the authorized ids.
- elevation (from arc base `f395597c`): `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`, exit 0; Domain `findings=0`. 224 s
  (deadline 600 s).
- The twenty s37 invariants: each `section_invariants --find-nothing` exits 0 (`INVARIANT HOLDS`), a few seconds each
  (deadline 300 s).
- Nothing is deferred. Every duration is within its deadline.

## Architecture

- `boundWith` works out the observed crystal once per bound experiment, over the drawn slots:
  - it takes the items held by the `ObjectSlotRole` slots (`SetupSlot.inLightOrder`);
  - with exactly one item, which the kit holds and whose `choiceOf` is a `StackSampleChoice (_, _, sample)`, it takes
    the one copy `sample.openedMaterialsIn study` answers, as `assignment.crystalOf` assigns it;
  - anything else is `None`.
- `solverParameters` is `context.solverParameters`. The forward does not default it, as `createBerremanForward` asks.
- `predictedReadingOf` builds the forward and `BoundCutPlate (CutPlate.ofForward …)` for each observation, then
  evaluates it with `muellerAt` at `configuration.elementOrientation` and the configuration's wavelength. `muellerAt`
  rebuilds the configuration from that orientation (step 250), so the prediction is exactly
  `createBerremanForward(...).muellerOf o.configuration crystal`.
- The checks run in this order. Any readout other than `PolarimeterDetectorReadout` answers `ReadingMismatchErr`
  first, naming the readout and the experiment. Then a polarimeter without an observed crystal answers
  `ObservationPlateErr` (reason `<setup reference>, which <experiment> references`, the shape `StudyDetectorErr` uses).
  A failed solve answers `ForwardSlotErr (SlotEvaluationErr (CutPlateSolveErr …))`.
- No cache or option was added.

## Deferred

Nothing.

## Gotchas

- The how-to names the stack choice's model `sampleModel`. Nothing reads it, so the match binds `_`, as
  `parameterLayoutWith`'s `madeOf` does.
- "Made of exactly one opened material" is read as `openedMaterialsIn study` answering one copy. A sample whose other
  layers are of materials the study did not open is still observed as that crystal's bare plate.
- A slot holding a sample is bound while the experiment binds. So the observation facts' material store also holds
  the probe crystal entry (`crystalBenchContext`). Without it, `boundWith` refuses with `SampleMaterialErr` before any
  observation is read.
- `qzDraft` (step 372's test-local 1 mm plate of the probe uniaxial version that `layoutStudy` opens) joined
  `benchDrafts`, so `placing` and the bench's `choiceOf` know it. No existing fact's values or results changed.
- `RealMatrix4x4` wraps a MathNet matrix, so the facts compare Mueller matrices by their sixteen elements (`elementsOf`),
  exactly, and not by `Result` equality.

## Changelog

- 2026-11-13 — Step 0375: Mueller observations are predicted as the cut plate of the experiment's one opened crystal
  (`MeasurementConfiguration.elementOrientation`, `BoundExperiment.predictedReadingOf`, `observedCrystalOpt` /
  `solverParameters`, `boundWith` over the study, `ObservationPlateErr` / `ReadingMismatchErr`). Two new facts and the
  widened error-render fact (constructor suite 1634 → 1636).
