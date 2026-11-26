# Step 0359 — impl plan (study file, spec 0050 S11.1)

A study persists as `<name>.study-json`, canonical JSON validated against its own schema, and recording it snapshots
and pins the material versions it opens. Everything rides the existing stack: `nodeOf` + `canonicalFileText`,
`parseVersionedText`, `SchemaValidation.validateWith`, `writeRenderedKindFile` / `readEveryKindFile`, and
`KitLibraryContext.snapshotReferences`. No second parser or writer is added.

## Files

- `OpticalConstructor.Domain/Studies.fs`: `StudyFile` (`schemaVersion`, `study`, `currentSchemaVersion` = 1);
  `type Study with member studyFile` and `member libraryReferences` (each opened material's exact version, in study
  order).
- `OpticalConstructor.Storage/schema/study.schema.json`: draft 2020-12, `$id` `https://berreman/schema/study.json`,
  copied from `item.schema.json`'s shape: version checked unconditionally, version-1 body under `if`/`then`.
- `OpticalConstructor.Storage/OpticalConstructor.Storage.fsproj`: a `Content` item copied `PreserveNewest`, beside the
  item schema. No comment.
- `OpticalConstructor.Storage/SchemaValidation.fs`: `validateStudyFile`, schema behind a `lazy` that loads the common
  schema first (`tryLoadLibrarySchema`), as `validateItemFile` does.
- `OpticalConstructor.Storage/CanonicalJson.fs`: two `ClosedSetTagConverter`s, for `ParameterScope` and `FitMode`,
  registered after `ExperimentRole`'s. The `options` doc lists them.
- `OpticalConstructor.Storage/KitContentFiles.fs`: `StudyFile.fileText`, `KitFileText.parseStudyFile`,
  `KitProxy.writeStudy` / `readStudies`, and `KitLibraryContext.recordStudy`.
- Tests:
  - `KitContentFileJsonTests`: four new facts.
  - `CanonicalJsonTests`: two widened facts.
  - New `KitStudyFileTests`: three facts, appended last in the compile list.
  - `KitLibraryTests`: two new facts.

## Test study

The QZ-LR fixture is shared, not copied three times. It lives in a `StudyFileFixtures` module at the top of
`KitLibraryTests.fs`, the first of the three consumers to compile; `CrystalMaterialFixtures` and
`KitContentRoundTripFixture` set the precedent. `studyOver (material : OpenedMaterial)` builds QZ-LR (E1, E2, one
scope, one link, a gain setting per `SessionGainModel` case, both weights, E2 disabled, two lines of notes). It runs
over either:

- the probe material, a test-local version with `AbsorbingTriclinicCrystal CrystalMaterialFixtures.absorbingTriclinicParameters`
  and its one parameter `n3` unknown under a prior;
- in `KitLibraryTests`, the user material saved through `materials.saveMaterial`.

## Risks

- Opening `Studies` in Storage may shadow record-label inference (`name`, `experiments`, `version`). Check with the
  compiler and qualify a literal if needed.
- ELEV: every new public signature takes and returns elevated types only. The schema-loading `lazy` stays private.
- The saved user material must be visible to the import proxy's `tryHeldEnvelope` for `snapshotReferences` to
  resolve it. `freshStores` shares one backing, as `the session resolves a divergent version` relies on.
