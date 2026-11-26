# Step 0460 — impl plan

A study opens a single isotropic tabulated segment the way it opens a crystal. The working copy's physics becomes
`OpenedMaterialPhysics`, either `OpenedCrystalPhysics` or `OpenedTablePhysics`. Its components are `row<i>.n` and
`row<i>.k`. Freeing a component of an uncertain table takes the source σ as its prior.

## Approach

1. **`ItemModels.fs`.** `ParameterName.tableRowIndex` and `tableRowExtinction`, 1-based, beside step 244's position
   members. They take `ParameterNameIndex`, not `int`. ELEV010 refuses an `int` beside `ParameterName` in a member,
   which is why step 244 introduced that type. This is a recorded deviation from the brief's `(row : int)`.
2. **`Studies.fs`.** `OpenedMaterialPhysics` goes directly before `OpenedMaterial`, with these members:
   - `componentNames`;
   - `componentValue`;
   - `withComponent`;
   - `componentSigmaOpt`;
   - `withoutSourceUncertainty`;
   - `crystalOpt`;
   - `materialPhysics`.

   A table's components come from one private member that lists each row's n and k: name, value, written table and
   source σ. The three lookups then share one walk.

   `OpenedMaterial` changes as follows:
   - its `crystal` field becomes `physics`;
   - `openFrom` accepts one isotropic tabulated segment with no magnetic or active part;
   - `withParameterState` stores the source σ prior when no prior is given;
   - `currentCrystal` becomes `currentPhysics`.
3. **`StudyForward.fs`.** In `ExperimentAssignment`:
   - `crystals` becomes `physics`;
   - `crystalOf` becomes `physicsOf`;
   - `materialProxyOver` answers `materialPhysics`.

   `assignmentFor` writes through `withComponent` and reads leaders through `componentValue`. `observedCrystalIn` reads
   `(physicsOf m).crystalOpt`. The residual is not touched, so it stays concurrent (R0004).
4. **`StudyPublishing.fs`.** `promotedMaterialWith` writes `currentPhysics.withoutSourceUncertainty.materialPhysics`.
5. **Tests.** Every `OpenedMaterial` record and `crystal` read moves to `physics`, and every `ExperimentAssignment` record
   moves to `physics`, with no assertion changed. Five new facts land: three in `StudyMaterialTests`, one in
   `StudyForwardTests` and one in `StudyPublishingTests`. Their shared test-local tabulated material lives in a fixtures
   module at the top of `StudyMaterialTests.fs`, the first of the three files to compile. `StudyFileFixtures` in
   `KitLibraryTests.fs` does the same.
6. **The AT-2 committed study file.** Regenerate it from a build: run the file-by-file fact, then copy the built
   `AbsorbingTriclinic.study-json` into `data/absorbing-biaxial-noisy/`.

## Files

Domain:
- `ItemModels.fs`
- `Studies.fs`
- `StudyForward.fs`
- `StudyPublishing.fs`

Tests:
- `StudyMaterialTests.fs`
- `StudyForwardTests.fs`
- `StudyPublishingTests.fs`
- `KitAcceptanceTests.fs`

Data: `data/absorbing-biaxial-noisy/AbsorbingTriclinic.study-json`.

Compile-only repairs outside the effects list:
- `KitLibraryTests.fs`, which holds `StudyFileFixtures`, the shared `OpenedMaterial` record expression;
- `StudyEngineTests.fs`, which has two `ExperimentAssignment` record expressions.

Both are required by brief (7)'s "every `OpenedMaterial` record expression …" and by the field rename. They are
recorded as deviations.

## Risks

- **ELEV.** No `int` may appear beside our types in a signature. New DU case names must be unique (`OpenedCrystalPhysics`,
  `OpenedTablePhysics`). Record labels must not shadow earlier ones in `Studies.fs`.
- **Crystal numbers.** `currentPhysics` regenerates the axes after each write, as `writtenComponent` already did in
  `assignmentFor`. The lifted axes only consult `own` for a crystal of another case, so every number stays the same.
- **AT-2 bytes.** The committed file must equal the built one, so it is copied from a build rather than hand-edited.
