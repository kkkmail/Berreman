# Step 0015 — IMPLEMENT — impl plan

## Approach

Move every built-in identity literal into two well-known id modules in Domain, and delete the Ui's
`SeedEntryIds` in the same step (their shared names may not coexist, ELEV015). The id text is
unchanged here (step 20 mints the preset GUIDs), so every fact keeps its result.

1. `OpticalConstructor.Domain/ElementId.fs`, module `Library`, directly after `newSampleId`:
   - `module SampleIds`: eleven `SampleId` values. The Guid literals move out of the
     `SeedSamples` records, which then read `id = SampleIds.<name>Id`.
   - `module PresetIds`: 33 `ElementValueId` values, one per seeded source, detector and
     polarizer preset, each `ElementValueId "<its current id>"`.
   - Both are plain modules shaped like `MaterialLibrary.MaterialIds`: no
     `[<RequireQualifiedAccess>]`, no `[<Literal>]`, every member a typed value.
   - `SeedSources` (`source` / `tunable` calls), `seedEntries` (the five detector and polarizer
     records) and `seedTrees` (the five detector and polarizer leaves) read the members.
   - Docs: `PresetIds`, `SampleIds`, `SampleId`, `SeedSamples`, and `sampleLeaf` (whose doc
     also says where the literal lives).
2. `OpticalConstructor.Domain/ExperimentDataLoad.fs`: `detectorKindOf`'s doc names
   `PresetIds.detectorIntensity` / `PresetIds.detectorEllipsometer`.
3. `OpticalConstructor.Ui/TableAndElementRotationView.fs`: delete `module SeedEntryIds` and its
   doc. `defaultSeedEntry` and `paletteButtons` read `Library.PresetIds.*`, and the
   `defaultSeedEntry` doc names `PresetIds.polarizerCpRight`.
4. The four test files read `PresetIds`, and fixtures hold `ElementValueId`:
   - `MainSceneMsgTests`: the pre-binding theory's rows carry the kind code only. A private
     `expectedSeedOf` maps each code to its member. The other binds read the members.
   - `TableAndElementRotationTests:532`: the members.
   - `ExperimentControlsTests`: `bind` takes `ElementValueId`. The sample ids hold
     `entryId` itself. The source, detector and polarizer binds and the comments read the
     members.
   - `FsxAcceptanceTests`: `TableRun.sourceEntry : ElementValueId`, plus `tableRun` and
     `userSource`. `runOnTable` binds directly. `source600`, `xenonMonochromator` and
     `euvBeamline` come from `PresetIds`. The sample-entry helpers return the `entryId` too,
     so that no fixture re-wraps `.value` (the slice's item (4) rule).

## Files

The seven files in the slice's authorized effects, and no others.

## Risks

- **Name collisions (ELEV015).** The 44 new names were grepped against product code. Only
  `SeedEntryIds`' five names collide, and that module is deleted in this step.
- **A member cannot ride `InlineData`.** The theory is reshaped to rows of the kind code, with a
  mapping helper. Its name and its four rows are kept.
- **Opening `Library`** in a test file that does not open it today could shadow names. The two
  files without that open (`TableAndElementRotationTests`, `ExperimentControlsTests`) write
  `Library.PresetIds.*`, as `ExperimentControlsTests` already writes `Library.SeedSamples`.
- **LF line endings** must hold.
