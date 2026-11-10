# Step 0246 — impl plan (IMPLEMENT: the source is real)

## Approach

1. `OpticalConstructor.Domain/ExperimentRun.fs` opens `ItemModels` and gains, before `RunStep`, the record
   `SourceEmission = { azimuth : Angle; ellipticity : Angle; degreeOfPolarization : ParameterValue;
   intensityScale : ParameterValue }` with `member this.stokes : StokesVector` =
   I · [1, p·cos 2χ·cos 2ψ, p·cos 2χ·sin 2ψ, p·sin 2χ], written through `StokesVector.create` as
   `Propagation.inputStokes` writes its vectors. The run law (`detectorStokes`, `read`) is untouched (step 252).
2. `OpticalConstructor.Domain/ItemEvaluation.fs` opens `Berreman.Geometry` and `ExperimentRun` and gains:
   - `ItemEvaluationError.NotALightSourceErr of reason : string`, rendered `the model is not a light source: <reason>`;
   - `type ParameterValue with member this.angle : Angle` (the number as radians, destructured, no `.value` re-wrap);
   - `type SourceEmission with static member ofValues` — `checkedFor SourcePreset.lightSourceAxes`, then the four
     values through the `ParameterName` members, the two angles through `angle`;
   - `type ItemModelChoice with member this.emissionWith` — `ofValues` for `SourcePresetChoice`, otherwise
     `NotALightSourceErr` naming the model and its form's kind (no wildcard arm over `ItemModelForm`).
3. `OpticalConstructor.Tests/ItemEvaluationTests.fs`: three new facts and the widened error-rendering fact.
   Choices are built from step 210's shipped item models (`BuiltinCatalogue.itemModel ItemModelIds.*`) and the
   Built-in presets/samples (`BuiltinCatalogue.source`, `BuiltinCatalogue.presets`, `BuiltinCatalogue.sample`).

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/ExperimentRun.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/ItemEvaluation.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/ItemEvaluationTests.fs`

## Risks

- Record-label inference: the new labels (`azimuth`, `ellipticity`, …) could capture another record's construction
  in a file that opens `ExperimentRun`; only `ExperimentRunTests` and `ItemEvaluation` open it, and neither builds
  such a record.
- ELEV: no `.value` re-wrap (`ParameterValue.angle` destructures), no new free function, unique names
  (`SourceEmission`, `NotALightSourceErr`, `angle`/`ofValues`/`emissionWith` extension members checked absent).
- 1e-15 tolerances: the emission formula multiplies in the same order `inputStokes` does, so the linear and
  circular cases agree to within a few ulps.
- The polarizer fixture is picked inline from `BuiltinCatalogue.presets ()` — `BuiltinCatalogue.fs` is outside this
  step's authorized edits, so no shared `polarizer` helper is added there.
