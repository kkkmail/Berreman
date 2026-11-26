# Step 0374 — state of the world

## Where we are

Spec 0050 section S11.2 (the forward model of an experiment and the study residual). Step 371 placed each measured
point on its scan, step 372 laid out the study's parameter vector, and step 373 turned one vector into the numbers one
experiment is evaluated with. This step is the forward model itself (001 §N): an experiment's setup slots, a scan
point's settings and the study's numbers give its detector item's predicted reading through S8.1's evaluator. The
study residual that compares those predictions with the measured rows comes in later S11.2 steps.

## What's working

- Add `RunStep.productOf`, a path's Mueller matrix as its steps' product in light order
- Add `Experiment.boundWith`: the setup revision drawn, its slots run over the assignment, read by the first detector slot holding a detector item
- Add `BoundExperiment.readingAt`: S₀ · gain + dark, Ψ and Δ, or the path's Mueller matrix at a scan point's settings and wavelength
- Add `ParameterValue.darkMean`, `DetectorReadout`, `BoundPathStep`, `BoundExperiment` and three `StudyForwardError` cases
- Add five forward-model facts and widen the error-render fact (constructor suite 1629 → 1634)

## Tests

```yaml
gates:
  constructor_unit_tests: 1634
  ui_tests: 14
```

- build: exit 0, `0 Error(s)`; the 12 warning lines are all NU1902 advisories (exempt); the Domain built alone after its
  last edit with `0 Warning(s)`. 46 s (deadline 600 s).
- constructor-unit-tests: Passed!, 1634 succeeded (1629 at checkpoint 373, +5), 0 failed, 0 skipped, 74 s (deadline
  180 s). The five new facts and the widened `every forward error renders its condition and its payload` are `Passed`
  in the TRX under exactly the authorized ids.
- ui-tests (selected `OpticalConstructor.Ui.Tests.FsxAcceptanceTests`, the gate's own command): Passed!, 14 of 14,
  0 failed, 0 skipped, 76 s (deadline 360 s). Its count is unchanged.
- elevation from arc base `f395597c`: `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`, exit 0; Domain `findings=0`.
  239 s (deadline 600 s).
- The twenty s37 invariants: each exits 0.
- Nothing deferred. Every duration is within its deadline.

## Architecture

- `boundWith` does the work that is the same at every scan point once: it resolves the setup revision, draws its slots
  (`drawnWith`), lays them out (`BeamLayout.layout`), builds the `SlotRunParam` over the assignment (`materialProxyOver`,
  `valuesOf`), reads the emission (`runEmissionOf`) and the operating wavelength (`operatingWaveLengthOf` over
  `boundSourceIn` and the scan's `sourceWaveLengthOpt`), picks the detector and its readout, and binds each run element
  on the detector's placed path once (`runElementWith`). A stack sample's materials are therefore resolved once per
  bound experiment, as the table's run does.
- `readingAt` is the per-point half. The wavelength is `waveLengthOpt`, else a `VaryWaveLength` setting, else the
  operating one. A step's rotation is its slot's `VaryR1` setting, else a `VaryDialReading` setting through its dial,
  else the slot's recorded `DialTurn` through its dial, else the drawn R1. Its incidence is a `VaryR2` setting, else the
  drawn incidence. Each step is evaluated by `muellerAt` and made a run step by `runStepOf`. Among settings for one
  variable, the innermost (last) one wins, as the table's `chartForRun` reads it.
- A dial's zero is the assignment's `dialZero` value, not the item's own, so a fitted dial zero turns the slot.
- Errors keep their source: `ForwardSetupErr` wraps the `SetupError`, `ForwardSlotErr` the `SlotRunError` (item
  resolution, or an `ItemEvaluationError` as `SlotEvaluationErr`), and `StudyDetectorErr` names the setup reference and
  the experiment.
- No cache or option was added.

## Deferred

Nothing.

## Gotchas

- The intensity and the Ψ, Δ readings come from `ExperimentRun.detectorStokes`'s `.intensity` / `.psiDelta`, the two
  projections `ExperimentRun.read` returns. Calling `read` would need an unreachable arm per kind, and the ellipsometer's
  would have to re-wrap `MeasuredIntensity s0.value` (an ELEV001 `Ctor other.value`). The tests compute S₀ and Ψ, Δ
  through `ExperimentRun.read`, so they pin the two to the same numbers.
- A detector slot whose item the kit lacks, or whose model is not a detector form, is not a detector item, so the
  setup answers `StudyDetectorErr`, not a slot error. A slot holding an item on the path is resolved first by
  `runElementWith`, so a missing analyzer item answers `ForwardSlotErr (SlotItemErr …)`.
- The built-in 1 mm glass plate (n = 1.52, non-dispersive, thick plate) does not depend on λ. So the retune assertions
  also run over the built-in 200 nm free-standing glass film, whose interference does, and the fact checks that the film
  reads differently at 450, 500 and 632.8 nm.
- `BoundExperiment` keeps the drawn slot in each `BoundPathStep`. A dial-mounted slot's drawn R1 uses the item's own
  zero, but `readingAt` reads its recorded `DialTurn` through the assignment's zero, which takes precedence over R1.

## Changelog

- 2026-11-13 — Step 0374: the forward model of an experiment (`RunStep.productOf`; `ParameterValue.darkMean`,
  `DetectorReadout`, `BoundPathStep`, `BoundExperiment`, `Experiment.boundWith`, `BoundExperiment.readingAt`; three
  `StudyForwardError` cases), five new facts and the widened error-render fact (constructor suite 1629 → 1634).
