# Step 0252 — impl plan

Operator extension, 2026-11-10 (manual 016): repair the copied-experiment fixture by binding and storing its
three assigned items, then expose/reuse `ItemEvaluation.sequenced` and delete the host's duplicate collector.
Preserve every assertion, gate and compiled slice. Both repairs are complete; all 19 gates and the whole
non-smoke UI suite passed. Wait for commits, then normal profile-1 resume must publish checkpoint 251.

Spec 0050 S8.1, the run law. `ExperimentRun` stops special-casing the first polarizer and folds every step
of the light path through its Mueller matrix. It starts from a real source emission whose default parameter
set (unpolarized, unit scale, referenced after the first polarizer) reproduces today's numbers exactly.

## Approach

1. **`OpticalConstructor.Domain/ExperimentRun.fs`**
   - `RunStep` becomes `ElementStep of mueller` | `ObjectStep of mueller`, with `member this.mueller`. It is
     declared before `SourceEmission`, so `emittedInto` can be an intrinsic member.
   - `SourceEmission.emittedInto steps` is `this.stokes` times r. r = 1 / S₀(first element's matrix ·
     unpolarized) when the path starts with an `ElementStep` and that S₀ is above zero; otherwise r = 1.
   - `detectorStokes emission steps` folds `step.mueller * sv` from `emission.emittedInto steps`.
   - `read kind emission steps` reads the result.
   - The module doc is rewritten in 1–3 lines.
2. **`Propagation.fs`**
   - Delete `behaviorInputStokes` and its doc.
   - `DetectorIntensity`'s doc names the source's intensity reference.
   - The stale "ComputedIdeal keeps synthesizing through `inputStokes`" section comment is fixed.
3. **`ElementId.fs`**: `PolarizerBehavior`'s doc names `Propagation.analyzerMueller`.
4. **`TableAndElementRotationView.fs`**
   - Delete `runInputStokes`.
   - `StepBuilder`'s `SweptStep` answers `Result<RunStep, ItemEvaluationError>`.
   - `builderFor` builds through the evaluator:
     - a polarizer is `BoundPolarizerPreset` → `ElementStep`;
     - a sample is `BoundOpticalElement.ofStackSample model.materials (defaults of its axes)`, bound once per
       path step → `ObjectStep`.
   - Every chart reads with the default `SourceEmission`.
   - A `SampleMaterialErr` keeps the old failure chart. Any other error shows "The run cannot be evaluated"
     with its `describe`.
   - The docs of `chartForRun` and of the run section header are rewritten.
   - Opens `ItemModels` and `ItemEvaluation`, which the extension members need.
5. **`AGENTS.md`**: the "An experiment runs on the light's real path" clause is rewritten as the slice
   dictates.
6. **Tests**
   - `ExperimentRunTests`:
     - new `lp` / `clear` helpers and a default emission;
     - one rename and two kept facts;
     - four new facts.
   - `PolarizerBehaviorTests`: delete the two `behaviorInputStokes` facts, and the helper that only they used.
   - `ExperimentControlsTests`: the new two-sample series fact.

## Risks

- **Exactness.** "Every component equal" relies on an ideal matrix passing exactly ½ (so r = 2), and on the
  products with zeros being exact. Checked by the new parity fact.
- **Opening `ItemModels` / `ItemEvaluation` in the UI file** could shadow names. ELEV014/015 keep our names
  unique, so this should be safe; the build will show it.
- **The UI test depends on two added samples landing on the main beam in front of the detector.** The test
  asserts that on the path before comparing values.
