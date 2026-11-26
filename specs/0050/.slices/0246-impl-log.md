# Step 0246 — impl log (IMPLEMENT: the source is real)

## Progress

- [x] `ExperimentRun.fs`: opens `ItemModels`; `SourceEmission` + `stokes` before `RunStep`; run law untouched.
- [x] `ItemEvaluation.fs`: `NotALightSourceErr`, `ParameterValue.angle`, `SourceEmission.ofValues`, `ItemModelChoice.emissionWith`.
- [x] `ItemEvaluationTests.fs`: three new facts, widened error-rendering fact.
- [x] Development runs: build, constructor tests (all discovered), FsxAcceptanceTests, elevation, invariant greps.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/ExperimentRun.fs`
  - `open OpticalConstructor.Domain.ItemModels`.
  - `SourceEmission = { azimuth : Angle; ellipticity : Angle; degreeOfPolarization : ParameterValue; intensityScale : ParameterValue }`,
    declared before `RunStep`.
  - `member this.stokes : StokesVector`: I · [1, p·cos 2χ·cos 2ψ, p·cos 2χ·sin 2ψ, p·sin 2χ] through `StokesVector.create`.
  - `detectorStokes` / `read` unchanged (step 252 moves the run law onto the emission).
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/ItemEvaluation.fs`
  - opens `Berreman.Geometry`, `OpticalConstructor.Domain.Library` (for `SourcePreset`) and `ExperimentRun`;
  - `ItemEvaluationError.NotALightSourceErr of reason : string`, rendered `the model is not a light source: <reason>`;
  - `type ParameterValue with member this.angle : Angle` (the number as radians);
  - `type SourceEmission with static member ofValues`: `checkedFor SourcePreset.lightSourceAxes`, then the four values
    through `ParameterName.azimuth` / `ellipticity` / `degreeOfPolarization` / `intensityScale`, the angles via `angle`;
  - `type ItemModelChoice with member this.emissionWith`: `ofValues` for `SourcePresetChoice`, otherwise
    `NotALightSourceErr "'<model name>' (<model id>) is <kind>"`, the kind read from the model's `ItemModelForm`.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/ItemEvaluationTests.fs`
  - new: `the default source is unpolarized light at unit scale`;
  - new: `a source emits its polarization state times its intensity scale`;
  - new: `only a light-source model emits, and its values must be in range`;
  - widened: `every evaluation error renders its condition and its payload` covers `NotALightSourceErr` (distinct count 2 → 3).

## Decisions

- **The kind named by `NotALightSourceErr`** is the model's form (`model.entry.model`): ideal polarizer, stack
  sample, real linear polarizer, retarder plate, black-box Mueller element, compound element, empty slot, intensity
  detector, Mueller polarimeter or ellipsometer.
  - A source form outside a `SourcePresetChoice` (a `LibraryModelChoice` of it, or a mis-bound preset choice) is
    "a light-source model bound to no source preset".
  - The match has no wildcard, so a later `ItemModelForm` or `OpticalForm` case is a compile error here (as step
    251 asks of its own evaluator arm).
  - The reason starts with the model's name and GUID, as `ItemDraft.addTo`'s `modelText` and the add form's caption do.
- **`ParameterValue.angle` destructures** (`let (ParameterValue radians) = this`) instead of `Angle this.value`, so
  no wrap-of-`.value` pattern (ELEV005) appears.
- **`ofValues` reads after `checkedFor`.** `checkedFor` already guarantees each axis holds a value in range, but the
  four `valueOf` reads still bind through `Result`, so no partial match or `failwith` is needed.
- **Choice fixtures** come from step 210's shipped item models
  (`BuiltinCatalogue.itemModel ItemModelIds.sourcePresetItemModelId / idealPolarizerItemModelId / stackSampleItemModelId / intensityDetectorModelId`)
  and from step 216's payloads over Built-in entries:
  - `BuiltinCatalogue.source PresetIds.heNe633`;
  - the `PresetIds.polarizerLp` polarizer picked from `BuiltinCatalogue.presets ()`;
  - `BuiltinCatalogue.sample SampleIds.glassPlate1mmId` at `SampleVersionId.firstOf`.

  No model is manufactured, and no preset or sample is used as a model.
- **Beyond the slice's minimum, the refusal test also covers** a `StackSampleChoice`, a `LibraryModelChoice` (the
  intensity detector) and the polarizer over empty values.
  - It stays inside the declared fact `only a light-source model emits, and its values must be in range`.
  - No new test identity is added.

## Testing state

These are development runs over the final tree. Invariant 6: the arc-runner's gate engine gives the verdict, not these runs.

- **build:** `dotnet build Berreman.slnx -c Release -nologo -v:m` exit 0, `0 Warning(s)`, `0 Error(s)`
  (`.artifacts/0246-build.log`).
  - The first attempt failed with `FS0039 'SourcePreset' is not defined` in `ItemEvaluation.fs`; it was fixed by
    `open OpticalConstructor.Domain.Library`.
- **constructor-unit-tests (all discovered):** `Passed!`, total 1385, succeeded 1385, failed 0, skipped 0, 1 m 06 s
  (`.artifacts/0246-constructor-unit-tests.log`, TRX at `.artifacts/0246-constructor-unit-tests.trx`).
  - 1385 is checkpoint 244's 1382 plus the three new facts.
  - All seven `ItemEvaluationTests` facts are `Passed` in the TRX under their declared identities.
- **ui-tests (selected `OpticalConstructor.Ui.Tests.FsxAcceptanceTests`):** `Passed!`, total 14, succeeded 14,
  failed 0, skipped 0, 1 m 11 s (`.artifacts/0246-ui-tests.log`, TRX at `.artifacts/0246-ui-tests.trx`).
  - The class was not edited, so its count is unchanged.
- **elevation** with `ASG_ARC_BASE_COMMIT=f395597c…`: `ELEV-GATE: OK - 0 new finding(s) in 12 project(s), 196s`;
  known findings 0, suppressed 1350 (the excluded ELEV004) (`.artifacts/0246-elevation.log`).
- **The fourteen s25 invariant greps,** with `--untracked`: all 14 find nothing (`.artifacts/0246-invariants.log`).
- **Red before green: not measured.** The production members and the facts were written in one pass.
  - The new facts name symbols that did not exist before this step (`SourceEmission`, `ofValues`, `emissionWith`,
    `NotALightSourceErr`).
  - Against the prior tree they cannot compile (FS0039), but that red build was not captured.

No test was removed or renamed, and no assertion was weakened.

## Artifacts

- `specs/0050/.artifacts/0246-build.log`
- `specs/0050/.artifacts/0246-constructor-unit-tests.log`, `0246-constructor-unit-tests.trx`
- `specs/0050/.artifacts/0246-ui-tests.log`, `0246-ui-tests.trx`
- `specs/0050/.artifacts/0246-elevation.log`
- `specs/0050/.artifacts/0246-invariants.log`

## Gotchas

- **`ExperimentRun.fs` did not open `ItemModels`,** although the slice says it does; the open was added.
  - Only `ExperimentRunTests` and `ItemEvaluation` open `ExperimentRun`, and neither constructs a record whose labels
    the new `azimuth` / `ellipticity` / … labels could capture.
- **`polarizerPresetOf` exists twice already,** as private helpers in `ExperimentProxyTests` and
  `KitContentFileJsonTests`.
  - A shared `BuiltinCatalogue.polarizer` would be the reuse, but `BuiltinCatalogue.fs` is outside this step's
    authorized edits.
  - So the test picks the polarizer inline from `BuiltinCatalogue.presets ()`, the way `ItemsPanelModelTests` lists
    its Built-in polarizers.
- **The vendored MathNet C# raised two `SYSLIB0051` warnings** on the first (failed) build, when it recompiled.
  - They are in `MathNetNumerics/Numerics/Exceptions.cs` and pre-date this step; the clean incremental rebuild shows
    `0 Warning(s)`.
  - The build gate's pass rule (exit 0, no `error` in the output) is not affected.
- **Rounding at the circular cases:** at χ = ±45°, cos 2χ is about 6e-17, not 0. S₁ is therefore about 6e-17,
  inside the 1e-15 tolerance.
  - The default emission, by contrast, is exactly [1, 0, 0, 0], because p = 0 zeroes every polarized component.
