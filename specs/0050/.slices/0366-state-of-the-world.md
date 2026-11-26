# Step 0366 — state of the world

## Where we are

Spec 0050, section S11.1 (studies).

- Steps 0358–0365 gave the study its shape, its file and its pure updates:
  - CreateStudy's and AddExperimentsToStudy's halves;
  - OpenMaterial's working copy;
  - SetParameterScope and LinkParameters;
  - its settings.
- This step joins the five user verbs into one semantic message applied to what the logic receives. It follows 001
  §P ("Every user step is a semantic message … The same messages back the buttons, so tests drive by meaning") and
  the S11.1 brief ("The messages are pure updates, with tests over in-memory kits").
- The study view that sends them is S11.5's.

## What's working

- Added `StudyContext`: the kit's studies, experiments, setups and items, and the library's material resolution.
  Its `itemsUsedBy` gives the items a study's experiments use.
- Added `StudyMsg` (`CreateStudy`, `AddExperimentsToStudy`, `OpenMaterial`, `SetParameterScope`, `LinkParameters`).
  `applyTo` answers the created or changed study through the pure updates, or a typed refusal that changes nothing.
- `StudyError` gains `UnknownStudyErr` and `MaterialUnavailableErr`.
- Added `StudyMessageTests`, four facts over an in-memory kit that record each answered study:
  - the study file;
  - OpenMaterial's snapshot and working copy;
  - the stored scope and link;
  - a refusal leaving every path and byte unchanged.
- The study error render fact covers the two new cases.

## Tests

These are development runs of each gate's own command from `0366.gates`, over the final tree:

- **build:** exit 0, `0 Error(s)`, no lowercase `error` (deadline 600 s).
  - The first build took 80 s, the incremental ones 23 s.
  - The only warnings are the exempt NU1902 and the vendored MathNet SYSLIB0051 pair.
- **elevation:** `ELEV-GATE: OK - 0 new finding(s) in 13 project(s), 233s` (deadline 600 s).
  - 1520 ELEV004 findings suppressed, 0 known findings.
  - The run covered all 13 projects, because the standalone run has no comparison base.
- **constructor-unit-tests:** `Passed!`, 1608 of 1608 (1604 + 4) (deadline 180 s).
  - It took 71 s, then 78 s over the final tree.
  - Passed: `CreateStudy and AddExperimentsToStudy write the study file over the kit's experiments`.
  - Passed: `OpenMaterial snapshots the material version into the kit and stores its working copy in the study file`.
  - Passed: `SetParameterScope and LinkParameters change the stored study`.
  - Passed: `a refused study message records nothing`.
  - Passed: the widened `every study error renders its condition and its payload`.
- **invariant-s36-\* (20):** each decoded `--find-nothing` grep finds nothing over the final tree. The untracked new
  test file was also grepped directly.

No run went past its deadline.

```yaml
gates:
  constructor_unit_tests: 1608
```

## Architecture

- **`StudyContext` follows the `*Context` convention.**
  - It is the data the window last read, plus one function: the library's resolution of a material version.
  - The composer maps its store's `Ok None` or failure to `MaterialUnavailableErr`, so `applyTo` stays pure and
    testable with in-memory stubs.
  - It is `[<ReferenceEquality>]` because it holds a function.
- **`applyTo` only composes.**
  - `CreateStudy` is `Study.create`.
  - Every other case finds its study by exact name (`UnknownStudyErr`, naming it and the count held). It then
    answers `withExperiments`, `openMaterial` of the resolved entry, `withParameterScope` or `withLink`, the last two
    over `itemsUsedBy`.
  - The study is found before the material is resolved.
  - The caller records the answered study through `KitLibraryContext.recordStudy` and replaces it in its list.
- **`StudyContext.itemsUsedBy` reuses `Experiment.itemsUsedBy` (step 0261) per experiment of the study, as
  `experiments` holds them.**
  - It answers the kit's items, in kit order, that the resolving experiments use.
  - An experiment whose revision or held item the kit lacks adds none. That keeps the brief's `Item list` signature
    without hiding the items of the rest.
  - `Studies.fs` now opens `Setups`, where that extension is declared.

## Deferred

Nothing. Every requirement of the slice landed this round.

## Gotchas

- **The fixture's library is database-backed, not `freshStores ()`** (deviation, recorded in the impl-log).
  - In the in-memory store set, `materials.saveMaterial` writes the material store alone, and the import seam never
    sees the version.
  - Measured with that fixture: `recordStudy` refuses the opened version with `UnheldEntryErr`, so no snapshot can
    land.
  - The facts use step 0359's pair instead: one `freshLibraryDatabase ()` under `CatalogueImportProxy.createInDatabase`
    and `MaterialProxy.createInDatabase`. The material is still saved through `materials.saveMaterial`.
- **The study lookup is by exact `StudyName`.** A case variant of a held name is refused by `CreateStudy` (file-stem
  rule), and is unknown to the other messages.
- **`OpenMaterial` of a version the library lacks** answers `MaterialUnavailableErr` from the composer's
  `resolveMaterial`. `UnknownStudyMaterialErr` remains the refusal for a version the study has not opened.
- **The test items are step 0362's drafts** with their models' known values. The scope and the link need no
  unknown.
- **`specs/0050/.manifest.state.json` was already modified** when the round started. It is the supervisor's, and this
  round did not touch it.

## Changelog

- 2026-11-13: step 0366 added the study messages:
  - `StudyContext` with `itemsUsedBy`;
  - `StudyMsg` with `applyTo` over the five verbs;
  - `UnknownStudyErr` and `MaterialUnavailableErr`;
  - the four `StudyMessageTests` facts over an in-memory kit;
  - two new rows in `StudySessionTests`' error-render fact.
