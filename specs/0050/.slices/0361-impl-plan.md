# Step 0361 — impl plan

OpenMaterial's pure half (spec 0050 S11.1; 001 §I, §N, §P). A study opens a crystal-parameter material version into a
working copy whose components are the parameters generated from the crystal record's own axes.

## Approach

All production code goes in `OpticalConstructor.Domain/Studies.fs`, which compiles after `MuellerInverse.fs`. It opens
`MuellerInverse`, for `ParameterAxis`, and `Library`, for `Sample`.

1. **`ParameterAxis<'Parameters>` extension.** Adds `parameterValueOf`, the axis's `read` as a `ParameterValue`, and
   `writeValue`, which is `write` of a `ParameterValue`.
2. **`CrystalParameters` extension, `parameterAxes`.** The crystal record's axes (`UniaxialParameters.axes`,
   `TriclinicParameters.axes` or `AbsorbingTriclinicParameters.axes`), each lifted onto the crystal by one private
   helper per case.
   - Each case helper hands the shared private lifter three things: its own record, the case's record finder and the
     case constructor.
   - The lifter maps each field case (`RefractionIndexField` / `RhoValueField` / `EpsValueField`) through one inner
     generic access lifter.
   - A crystal of another case reads as the own record and is written back unchanged.
3. **Intrinsic `OpenedMaterial` members:**
   - `openFrom`: crystal → working copy with `KnownParameter` per generated axis; complexity → `MaterialWithoutParametersErr`;
   - `withParameterState`: `UnknownStudyParameterErr` or `InvalidStudyParameterErr` (via `Parameter.validate`), else the
     component's state replaced in place;
   - `currentCrystal`: each parameter's `currentValue` written through its generated axis.

   Steps 1 and 2 are declared just before `OpenedMaterial`, because its intrinsic members use them.
4. **Intrinsic `Study` members**, after step 360's `withExperiments`:
   - `openMaterial`: refuses a version the study holds with `MaterialAlreadyOpenErr`, else appends the working copy;
   - `withMaterialParameterState`: `UnknownStudyMaterialErr`, else the copy's `withParameterState` in place.
5. **`Sample` extension, `openedMaterialsIn`.** Declared after `Study`. It answers the study's copies whose version
   `structure.referencedMaterialVersions` holds, in study order.
6. **`StudyError` gains five cases**, each with the rendering the slice specifies.

## Tests

- **New `StudyMaterialTests.fs`.** It holds the five facts the slice names and is appended last in the test project's
  compile list.
  - The crystal records are reused from `CrystalMaterialFixtures` in `MaterialProxyTests.fs`, which compiles earlier.
    Its absorbing record already holds fifteen distinct literal values, so re-typing them would be a copy.
  - The `MaterialEntry` values and the `MaterialVersionId` are test-local.
- **`StudySessionTests`' study-error fact** widens to the five new cases.

## Risks

- An inner generic function inside a generic private static member may not generalize (FS0064); if not, restructure.
- ELEV naming rules (unique DU case names, extension members): every new case name was grepped and none exists yet.
- The compile order forces steps 1 and 2 before `OpenedMaterial` rather than after `withExperiments`; this is recorded
  as a placement decision.
