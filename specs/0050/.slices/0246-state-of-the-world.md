# Step 0246 — state of the world

## Where we are

Spec 0050, section S8.1 ("The run law walks every object; the source is real"), step 246 (IMPLEMENT).

- Steps 244–245 gave the light source its four parameter axes and the evaluator's `ParameterValues` with
  `checkedFor`.
- This step makes the source real: a `SourceEmission` (azimuth, ellipticity angle, degree of polarization, intensity
  scale) whose `stokes` is the source's polarization state times its intensity scale.
- It also adds the evaluator's source arm, `ItemModelChoice.emissionWith`.
- The run law itself is unchanged here; step 252 makes `detectorStokes` start from the emission, and S8.2's source
  slot (step 266) feeds a source item's own values through `emissionWith`.

## What's working

- Added `ExperimentRun.SourceEmission`, whose `stokes` is I · [1, p·cos 2χ·cos 2ψ, p·cos 2χ·sin 2ψ, p·sin 2χ] (+S₃ = left, as `inputStokes`)
- Added `SourceEmission.ofValues`, reading the four light-source values once `checkedFor` accepts them, the angles through `ParameterValue.angle`
- Added `ItemModelChoice.emissionWith`: a source preset choice emits, every other choice answers `NotALightSourceErr` naming its model's kind
- Added `ItemEvaluationError.NotALightSourceErr`, rendered `the model is not a light source: <reason>`
- Added three `ItemEvaluationTests` facts and widened the error-rendering fact; constructor tests rise 1382 → 1385

## Tests

No gate verdict is claimed here (Invariant 6). After exit, the arc-runner runs the frozen roster:

- build and elevation;
- constructor-unit-tests over everything discovered;
- ui-tests over `OpticalConstructor.Ui.Tests.FsxAcceptanceTests`;
- the fourteen s25 invariants.

The worker's development runs over the final tree, with logs under `specs/0050/.artifacts/0246-*`:

- **build:** exit 0, `0 Warning(s)`, `0 Error(s)`.
- **constructor-unit-tests:** `Passed!`, 1385 succeeded, 0 failed, 0 skipped, in 66 s (estimate 180 s).
  - That is checkpoint 244's 1382 plus the three new facts:
    - `the default source is unpolarized light at unit scale`
    - `a source emits its polarization state times its intensity scale`
    - `only a light-source model emits, and its values must be in range`
  - The widened `every evaluation error renders its condition and its payload` also passes.
  - Each is `Passed` in the TRX under its declared identity.
- **ui-tests (FsxAcceptanceTests selected):** `Passed!`, 14 succeeded, 0 failed, 0 skipped, in 71 s (estimate 360 s).
  - The constructor/export parity with the Analytics examples is preserved, and the class's count is unchanged.
- **elevation** from the arc origin `f395597`: `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, in 196 s.
  Known findings: 0.
- **The fourteen s25 invariant greps,** untracked files included: none finds anything.
- **Red before green** was not captured separately: the facts and members were written in one pass. See the impl-log.

No run came near a deadline. No test is removed or renamed, and no assertion is removed or weakened.

```yaml
gates:
  constructor_unit_tests: 1385
  ui_tests: 14
```

## Architecture

- **Where things live:**
  - `SourceEmission` is plain data in `ExperimentRun`, beside the run steps it will feed (step 252).
  - Reading it from `ParameterValues` is an extension in `ItemEvaluation`, which compiles after `ExperimentRun` and
    owns `ParameterValues` and `ItemEvaluationError`.
- **`emissionWith` is the source arm of the one evaluator.**
  - It matches on step 216's choice payloads.
  - It never resolves or manufactures a model; the caller hands it the resolved `ItemModelChoice`.
  - The source's wavelength stays the bound preset's.
- **The `NotALightSourceErr` reason names the model** (name and GUID) and its form's kind.
  - The form match has no wildcard, so a new `ItemModelForm` or `OpticalForm` case fails to compile until it is named.

## Deferred

Nothing. Every requirement of the step landed in this round. The run law's switch to the emission is step 252's,
and the source slot's own values are step 266's; both are scheduled steps, not deferrals from this one.

## Gotchas

- **`ExperimentRun.fs` did not open `ItemModels`** before this step (the slice assumed it did); it does now.
  - The new record labels (`azimuth`, `ellipticity`, `degreeOfPolarization`, `intensityScale`) win record inference
    in any file that opens `ExperimentRun` after another module with those labels.
  - `Berreman.Fields`' `IncidentLightInfo` has an `ellipticity` label, so annotate the record you mean.
- **`ItemEvaluation.fs` needs `open OpticalConstructor.Domain.Library`** for `SourcePreset.lightSourceAxes`.
- **Circular emission is not exact:** at χ = ±45°, S₁ is about 6e-17 (cos π/2), so compare within a tolerance.
  - The default emission is exactly [1, 0, 0, 0].
- **Picking the LP polarizer preset:** two private `polarizerPresetOf` helpers already exist in the tests. This step
  could not add a shared one to `BuiltinCatalogue.fs`, which is outside its authorized edits, so
  `ItemEvaluationTests` picks the preset inline.
- **The vendored MathNet `Exceptions.cs` emits two `SYSLIB0051` warnings** whenever it recompiles from scratch.
  - They pre-date this step and do not affect the build gate's pass rule.

## Changelog

- 2026-11-10 — Step 0246:
  - added `SourceEmission` with `stokes`, `SourceEmission.ofValues`, `ParameterValue.angle`,
    `ItemModelChoice.emissionWith` and `NotALightSourceErr`;
  - added three `ItemEvaluationTests` facts and widened the error-rendering fact (constructor tests 1382 → 1385).
