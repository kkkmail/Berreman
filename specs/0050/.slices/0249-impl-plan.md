# Step 0249 — impl plan (IMPLEMENT: stack samples go through Berreman)

## Approach

A stack sample joins the item-model evaluator as one more `BoundOpticalElement` case. It is solved exactly as the
table solves it (`Propagation.resolveSampleMaterials` once, then `Propagation.sampleMuellerSpun` per evaluation).

1. `ParameterValue.thickness : Thickness` reads the number as metres. It is the inverse of step 207's
   `Thickness.parameterValueOpt`.
2. `Sample.withThicknessValues (values) : Result<Sample, ItemEvaluationError>`:
   - first, `values.checkedFor this.parameterAxes`;
   - then each finite layer's thickness is read under the name `parameterAxes` publishes for it:
     `film<k>.thickness`, `film<k>.layer<j>.thickness` (the cell is stored once and shared by every period), or
     `substrate.thickness`;
   - a layer whose `parameterValueOpt` is `None` (an `Infinity`) is kept;
   - every other field is kept.
3. `ItemEvaluationError.SampleMaterialErr of error : MaterialError` renders "the sample's materials do not resolve:
   <reason>".
4. `BoundOpticalElement.BoundStackSample of Propagation.ResolvedSample`.
   - `static member ofStackSample materials values sample` is `withThicknessValues`, then `resolveSampleMaterials`.
     Its `MaterialError` is mapped to `SampleMaterialErr`.
   - `muellerAt` on `BoundStackSample` is `sampleMuellerSpun orientation.branch resolved w orientation.incidence
     orientation.rotation` on both branches, written as an or-pattern, not a wildcard.
5. Step 248's local `sequenced` (Result list → list Result) inside `muellerAt` moves to one private module-level
   helper declared after `ItemEvaluationError`. Both `muellerAt` and `withThicknessValues` call it, so no body is
   duplicated.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/ItemEvaluation.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/ItemEvaluationTests.fs`:
  - three new facts;
  - the widened render fact.

## Risks

- **`MaterialProxy.createInMemoryEmpty`, which the brief names, does not exist.**
  - `MaterialStore.fs` is outside this step's authorized edits.
  - The empty in-memory store is `MaterialProxy.createInMemory (fun _ -> [])`, documented "Empty material store".
  - The test uses it; the deviation is recorded.
- **ELEV checks.**
  - The new names are unique (git grep finds none).
  - ELEV006 matches only `to<Pascal>` names, so `thickness` / `parameterValueOpt` is no bijection under the rule.
  - The new private helper's single parameter is an FSharp.Core `list`, not a type of ours (ELEV013).
- **Opening `Berreman.Media` / `MaterialLibrary` in the Domain file may shadow names.** Order the opens as
  `Propagation.fs` does and build.
