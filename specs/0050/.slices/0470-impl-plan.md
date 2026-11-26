# Step 0470 — impl plan

## Approach

The lab profile is the kit's items read through their models (spec 0050 S14.1; 001 §M). Everything lands in
`MeasurementPlanner.fs` after step 469's declarations, plus one new test class.

1. `PlannerError` (step 469) gains `UnresolvedPlannerItemErr of reason : string`, rendered
   `the item's model is not in the library: <reason>`.
2. New derived (never stored, no wire tag) shapes, each doc citing S14.1: `PlannerSource`, `OpticRole`
   (`PolarizerOptic | CompensatorOptic | CompoundOptic`), `PlannerOptic`, `DetectorRecording`
   (`RecordsIntensity | RecordsMuellerMatrix | RecordsPsiDelta`), `PlannerDetector`, `LabProfile`.
3. `LabProfile.ofItems items choiceOf`: each item, in order, to a one-item profile (or the empty profile), the list
   collected by `sequenced` (the shared ordered Result collector, `ItemEvaluation`) and concatenated, so the first
   refusal answers and nothing is answered in part. `None` from `choiceOf` → `UnresolvedPlannerItemErr <label>`.
   Detectors: `ParameterValues.ofParameters item.parameters` `checkedFor` the form's noise axes; a failure →
   `UnresolvedPlannerItemErr "<label>: <the evaluation error's describe>"` (it names the axis). A compound is a
   `CompoundOptic` when any leaf form at any depth is a `RealPolarizerModel` (`List.tryPick` over the flattened
   leaves, so no `bool`), else `CompensatorOptic`. A bare binding form under `LibraryModelChoice` refuses as
   unresolved (as `boundWith` refuses it).
4. `reachOf` / `reaches` exactly as the brief.

`MeasurementPlanner.fs` opens `OpticalConstructor.Domain.Kit` and `OpticalConstructor.Domain.ItemEvaluation`; the
library's `PolarizerBehavior` cases are written `Library.ComputedIdeal` / `Library.ConstantMueller`. Nothing from
Optimization.

## Tests

`OpticalConstructor.Tests/PlannerInputsTests.fs` (`module PlannerInputsTests`), appended last to the compile list: nine
`ItemDraft`s over the Built-in catalogue (`BuiltinCatalogue.itemModel`, `.source`, `.preset`, `.sample`), a test-local
`choiceOf` from item id to draft model, and the three facts of the step's effects.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/MeasurementPlanner.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/PlannerInputsTests.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj`

## Risks

- Opening `Kit` / `ItemEvaluation` may shadow a name the file already uses; record construction is annotated so the
  shared field names (`item`, `label`) cannot pick the wrong record.
- ELEV: every case name two words, no `bool`, no primitive on a public surface; the names were checked unique.
