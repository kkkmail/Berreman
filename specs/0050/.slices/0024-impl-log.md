# Step 0024 — IMPLEMENT — impl log

## Progress

- [x] Read the protocol (arc-runner, implement worker), the project prompt, the slice, 001 §A/§B
      and .spec-md §3–§5. The Operator note section is empty.
- [x] Grepped every new type name, case name and extension-member name across Berreman and STL
      (ELEV014/015/016/019). None exists in either tree. STL's only `fileStem` is a record field of
      a VNC log-target record, not an extension member on `EntryName`.
- [x] `LibraryEntryFiles.fs` written; one `<Compile>` line added directly after `ElementId.fs`, with
      no comment.
- [x] `LibraryEntryFilesTests.fs` written; one `<Compile>` line added after `CanonicalJsonTests.fs`.
- [x] Local observations (build, constructor suite, ELEV script, s04 greps) recorded below.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/LibraryEntryFiles.fs` (new):
  `module LibraryEntryFiles` in `namespace OpticalConstructor.Domain`. It opens
  `Softellect.Sys.Primitives`, `Placement`, `MaterialLibrary` and `Library`, and declares:
  - `FileSchemaVersion`;
  - `LibraryEntryKind` (`value` 1..6, `tryOfWire`, `extension`, `currentSchemaVersion`, `all`);
  - `EntrySchema`;
  - `LibraryEntryId`, plus the extensions `MaterialId` / `SampleId` / `CategoryId.toLibraryEntryId`
    and `ElementValueId.tryToLibraryEntryId`;
  - `CatalogueId`, `CatalogueName`, `KitId`;
  - `MaterialSourceName`, `MaterialSourceRecordId`, `MaterialSourceVersion`, `EntryLicence`,
    `EntryCitation`, `SourcePullInfo`;
  - `EntryProvenance` (`protection`, `categoryOrigin`);
  - `ContentHash`;
  - `FileStem` (`ofText`, `withExtension`), plus the extension `EntryName.fileStem`.
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/OpticalConstructor.Domain.fsproj`: +1 line
  (`<Compile Include="LibraryEntryFiles.fs" />` after `ElementId.fs`).
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/LibraryEntryFilesTests.fs` (new):
  `module LibraryEntryFilesTests` with the four facts the slice names, word for word.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj`: +1 line
  (`<Compile Include="LibraryEntryFilesTests.fs" />` after `CanonicalJsonTests.fs`).

## Decisions

- **`FileSchemaVersion` is declared before `LibraryEntryKind`.** The slice lists the kind first,
  but `currentSchemaVersion` returns a `FileSchemaVersion`, and F# needs the type declared first.
  The choice was between reordering and an `and` chain; reordering is the plainer of the two.
- **`currentSchemaVersion` lists all six kinds in one match arm** instead of returning a constant.
  When one kind's schema moves, only its own arm changes, and a seventh kind (S7.1's item model)
  will not compile until it states a version.
- **`EntryProvenance.protection` / `categoryOrigin` name every case** (no wildcard), so a new
  provenance case has to decide its protection when it is added.
- **`tryToLibraryEntryId` parses with `Guid.TryParseExact(…, "D")`**, the canonical hyphenated form
  that every seeded and minted preset id has used since step 20. Plain `Guid.TryParse` would also
  accept the `N`, `B` and `P` spellings, so two different id texts could map to one entry id. The
  slice's "answers `None` for any other text" reads most naturally as the strict form.
- **`FileStem.withExtension` builds `FileName $"{stem}{extension}"`.** It does not call STL's
  `FileName.addExtension`, because that member's no-dot branch evaluates `failwith this.value`
  before it concatenates, so it throws. `withExtension` stays total; every `LibraryEntryKind`
  extension starts with `.`.
- **The character rule is matched inside `ofText`**, not kept as a private `Set<char>` value. This
  keeps a char-typed value out of a module-level signature (ELEV003P).
- **No reverse conversion** (`LibraryEntryId.toMaterialId` and the like) exists, per the slice
  (ELEV006): step 25's decoders build a kind's id at the file seam.
- **The tests compare GUIDs through `.value`** (`MaterialIds.silicon.value` against
  `….toLibraryEntryId.value`), never `LibraryEntryId MaterialIds.silicon.value`. AGENTS.md holds
  fixtures to the same rule against call-site re-wraps. The non-GUID fixture is
  `ElementValueId "not-a-guid"`, which no `invariant-s04-no-seeded-string-preset-ids` pattern matches.

## Testing state

The arc-runner's gate engine is the gate authority. These are local observations only, made so the
round ends on code that compiles:

- `build` command (`dotnet build Berreman.slnx -c Release -nologo -v:m`): exit 0, `0 Error(s)`.
  There are two warnings, both `SYSLIB0051` in the vendored `MathNetNumerics/Numerics` (C#); step
  0023's build shows the same two. No warning comes from our code (`.artifacts/0024-build.log`).
- Constructor suite, whole: `Test run summary: Passed!`, total 746, succeeded 746, failed 0,
  skipped 0. That is 742 before this step plus the 4 new facts
  (`.artifacts/0024-constructor-unit-tests-whole.log`). `--list-tests` with the class filter
  discovers exactly the four slice ids under `OpticalConstructor.Tests.LibraryEntryFilesTests`.
- ELEV script (`Elevation/Run-ElevationGate.ps1 -ExcludeRules ELEV004`):
  `ELEV-GATE: OK - 0 new finding(s) in 8 project(s), 154s`, known findings 0. Domain was expanded to
  its 8 dependents for the duplicate-name rules. The test file is out of analyzer scope, as the
  script notes (`.artifacts/0024-elevation-local.log`).
- The six s04 invariants each find nothing (exit 1):
  - `ELEV0` in the known-findings list;
  - the VSTest packages;
  - `kit.kit-json`;
  - `EpsAxisEvaluated`;
  - `complexity = (None|Some)`;
  - the seeded string preset ids.

  The content greps were re-run with `--untracked` so that they also cover the two new,
  not-yet-tracked files.
- Line endings: both new files are LF / UTF-8. `git diff --numstat` matches
  `--ignore-cr-at-eol` for every file this step changed.

Every requirement of the slice, (1) to (7) and tests (a) to (d), is delivered in this round.

## Artifacts

- `.artifacts/0024-build.log`: the solution build.
- `.artifacts/0024-constructor-unit-tests-whole.log`: the whole constructor suite.
- `.artifacts/0024-elevation-local.log`: the ELEV script over Domain and its dependents.

## Gotchas

- `git grep` (the invariant gates' tool) does not search untracked files. A new file that breaks
  an invariant passes the local grep until the file is committed. Grep with `--untracked` before
  then.
- `KitId` is born here, not in S4.1's `Kit.fs`. The `Kit` module MUST reuse
  `LibraryEntryFiles.KitId`; a second `KitId` is an ELEV014 failure.
- STL's `FileName.addExtension` throws for an extension without a leading `.` (`failwith` is
  applied before the concatenation). Do not route `FileStem.withExtension` through it.
- The working tree also shows `specs/0050/.manifest.state.json` as modified. That is the
  supervisor's file; this worker did not touch it.
