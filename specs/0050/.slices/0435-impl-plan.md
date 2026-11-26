# Step 0435 — implementation plan

AT-2's study, plus its two identity assertions (spec 0050 S12.2; 001 §Q AT-2). The only authorized edit is
`Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/KitAcceptanceTests.fs`. Rulings R0001–R0003 bind: no table
slot is added, so the element-id provider still mints only the reflected detector's id, and no fit runs.

## Approach

1. **Build.** `buildAbsorbingKit` goes on after `attachNoisyObservations` with `buildAbsorbingStudy`:
   - `StudyCommand (CreateStudy (StudyName "AbsorbingTriclinic", <five experiments in kit order>))`;
   - `StudyCommand (OpenMaterial (…, MaterialVersionId.firstOf absorbingMaterial.id))`;
   - one `SetOpenedMaterialParameter` per generated axis of the opened copy's crystal. Each frees its component from
     `perturbedStart` (`s`), within ± `searchBox.halfWidth` × the harness's scale on the record axis at the same
     position (`h`).

   `kitStatus` is checked after every command. No scope, link, session or fit is set.
2. **Shared composition.** Step 430's inline `residualAtStart` becomes two private functions:
   - `studyDataOf model study`: `prefitWith [] |> dataWith`;
   - `studyResidualAtStart model study`: `residualWith` over `parameterLayoutWith`'s layout at its `startVector`.

   A private `studyIn` finds the window's study. The Send fact calls `studyResidualAtStart` and keeps its assertions.
3. **Harness side.**
   - `absorbingForward`: the forward step 434's `noisyObservationsOf` uses, lifted out so the residual check uses the
     same one.
   - `byPlate`: observations grouped plate by plate in kit order.
   - `perturbedStartUnknownsOf crystal`: the one body the build and the fact share.
   - `assertSameObservation`: split out of `assertSameObservations`, with its messages unchanged, so rows and keyed
     observations compare through one body.
4. **The fact.** It runs four checks:
   - the stored study over a second `KitProxy`;
   - the generated axes against `AbsorbingTriclinicParameters.axes`;
   - data identity, exact, against `byPlate (noisyObservationsOf attachedSeed)`;
   - the start residual: 704 entries, each within 1e-12 · max(1, |h_k|) of `throughputResidualFor` at the zero scaled
     vector. Every mismatch is listed with its position and both values.

## Risks

- `recordStudy` snapshots the study's material reference. It is already snapshotted as the plate's closure, so step
  432's exact library assertion should still hold. A development run will confirm it.
- The 1e-12 residual identity depends on the study's cut-plate path agreeing with `createBerremanForward`. Step 379
  proved that agreement for a uniaxial crystal, and the run will measure it for this one.
- The build is shared through `absorbingBuild` (Lazy), so the earlier AT-2 facts see the study. None of them reads
  studies, but all of them are re-run.
