# Step 0359 — state of the world

## Where we are

Spec 0050, section S11.1 (studies). Step 0358 gave the study its Domain shape and CreateStudy's pure half. This step
persists a study as its own kit file, `<name>.study-json`, in canonical JSON validated against its own schema. Recording
a study snapshots and pins the material versions it opens before the file is written. What follows: filling an opened
material's working copy is step 361, and fit runs are S11.3.

## What's working

- Added `StudyFile` (schema version 1), `Study.studyFile` and `Study.libraryReferences`: each opened material's exact
  version, in study order.
- Added `schema/study.schema.json` and `SchemaValidation.validateStudyFile`. The version is checked unconditionally and
  only version 1's body is validated, as for item files.
- `ParameterScope` and `FitMode` are stored as their int tags through `ClosedSetTagConverter`. An unknown tag is
  `MalformedJsonErr`.
- Added `StudyFile.fileText` and `KitFileText.parseStudyFile`, plus `KitProxy.writeStudy` / `readStudies`. Each study is
  stored under its name, and a bad file refuses the whole read, naming the file.
- Added `KitLibraryContext.recordStudy`. It snapshots and pins the opened material versions, then writes the study's
  file. A read-only kit records nothing.
- Added `KitStudyFileTests` and nine new facts in all. `CanonicalJsonTests`' two closed-set facts are widened.

## Tests

These are development runs of each gate's own command from `0359.gates`, over the final tree:

- **build:**
  - exit 0, 0 errors, no `error` text in stdout;
  - the only warnings are NU1902, which is exempt;
  - 44 s for the first full build and 22 s incremental, against a 600 s deadline.
- **constructor-unit-tests:**
  - `Test run summary: Passed!`, 1588 of 1588, 0 skipped. That is 1579 + 9, the count rising by 9 as the acceptance
    requires.
  - The TRX shows each of the eleven authorized ids `Passed` under exactly its id:
    - the four new `KitContentFileJsonTests` facts;
    - the three `KitStudyFileTests` facts;
    - the two new `KitLibraryTests` facts;
    - the two widened `CanonicalJsonTests` facts.
  - 72 s against a 180 s deadline.
- **elevation:**
  - `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`, known findings 0, suppressed 1517 (ELEV004 only, which is
    excluded);
  - 218 s against a 600 s deadline;
  - with no `ASG_ARC_BASE_COMMIT` in this shell it analysed all 13 projects, wider than the gate's arc-origin scope.
- **invariant-s36-\* (20):** every snapshot command reports `INVARIANT HOLDS`. `git grep` does not see the two new
  untracked files, so they were grepped for every pattern as well: nothing found. Each runs in under 1 s against 300 s.

No run went past its effective deadline.

```yaml
gates:
  constructor_unit_tests: 1588
```

## Architecture

- **The storage stack is reused, not copied.** The study file rides the stack every kit content file shares: `nodeOf` +
  `canonicalFileText`, `parseVersionedText`, `validateWith`, `writeRenderedKindFile` and `readEveryKindFile`. No second
  parser or writer was added.
- **Study file only.** `writeStudy` and `readStudies` mirror the setup pair: the file sits under its name's stem, which
  must match ignoring case, and studies are read in ordinal name order. No delete, rename or cache was added.
- **Recording.** `recordStudy` is `snapshotReferences` followed by the write, the same shape as `recordSetup` and
  `recordItem`. Under `ReadOnlyKitAccess` the snapshot step refuses first, so nothing is pinned or written.
- **The schema validates structure only.** It checks the root and `study`/`settings` keys, a non-blank name, the mode's
  1..2 range and non-empty experiment names. The inner shapes (materials, scopes, links, gain models) are bound by the
  serializer, which answers `MalformedJsonErr`. `SessionGainModel`, `StudyParameterRef` and `CrystalParameters` keep the
  serializer's case form.
- **Tests:** the QZ-LR study fixture (`StudyFileFixtures`) lives at the top of `KitLibraryTests.fs`, the first of its
  three consumers in compile order. This follows the `CrystalMaterialFixtures` / `KitContentRoundTripFixture` precedent.
  `KitContentFileJsonTests`' three per-kind edit helpers now share one `editedRendering` rather than a fourth copy.

## Deferred

Nothing. Every requirement of the slice landed this round.

## Gotchas

- **Which store set the `recordStudy` fact uses.** The slice speaks of "the set's `materials.saveMaterial`". In the
  in-memory store set (`CatalogueImportProxy.createInMemory` / `BuiltinCatalogue.freshStores`), a material saved
  through `materials.saveMaterial` is not seen by the import proxy's `tryHeldEnvelope`, which reads its own entry map.
  `snapshotReferences` would therefore answer `UnheldEntryErr`.
- **Resolution.** The fact runs over the built-in library database (`BuiltinCatalogue.freshLibraryDatabase`), whose
  `MaterialProxy.createInDatabase` and `CatalogueImportProxy.createInDatabase` share one database. This follows the
  existing `recordSetup` fact's precedent. The read-only fact keeps the in-memory set, since nothing resolves there.
- **The kit pins two versions, not one.** Recording pins the material's version AND its category's: the material
  references its category at latest, and the closure snapshots and pins both. The fact asserts both, in visit order.
- **Escapes in shell-written F#.** The shell unescapes `\\n` / `\r` in heredoc'd Python, which wrote a real CR and
  newlines into F# string literals. Edit F# literals with escapes through the Edit tool.
- **FS3369.** An indexer chain in argument position (`node["a"]["b"] <- …` inside a list of lambdas) raises FS3369. Use
  `node.["a"].["b"]`.

## Changelog

- 2026-11-13: step 0359 added:
  - the study file (`StudyFile`, `study.schema.json`, `validateStudyFile`);
  - the `ParameterScope` / `FitMode` tag converters;
  - `writeStudy` / `readStudies` and `recordStudy`;
  - `KitStudyFileTests`, nine facts in all.

  constructor-unit-tests went from 1579 to 1588.
