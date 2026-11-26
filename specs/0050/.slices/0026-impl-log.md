# Step 0026 — IMPLEMENT — impl log

## Progress

- Read the task, the system prompts, the project prompt, the slice, its gates, 001 §A and
  .spec-md §3, §5, and the later slices that consume the manifest (31, 32, 33, 92, 98), so the
  shapes fit them (`CatalogueManifest.extension` for step 31's `UnsupportedSchemaVersionErr`,
  `CatalogueManifest.fileName` for step 33's folder read, `tryCreate`'s refusal for step 32's
  `InvalidCatalogueEntriesErr`).
- Only `LibraryEntryFilesTests` opens `LibraryEntryFiles`. No name collision for
  `CatalogueManifest`, `CatalogueManifestRow`, `DuplicateEntryFileNameErr` or
  `DuplicateEntryVersionErr` in Berreman or STL (ELEV014 / ELEV016).
- Domain, in `LibraryEntryFiles.fs`:
  1. `LibraryEntryError` gains `DuplicateEntryFileNameErr` and `DuplicateEntryVersionErr`, both
     `of reason : string`. Its doc now covers the catalogue refusal.
  2. `CatalogueManifestRow` is declared just before `LibraryEntryEnvelope`, so the envelope's
     `manifestRow` is an intrinsic member: a cross-type reuse towards an earlier type.
  3. `LibraryEntryContent.describe` (name, id, version) is the one render both refusals use.
  4. `CatalogueManifest` is declared after the envelope, with these members:
     - `currentSchemaVersion` (1);
     - `extension` (`.catalogue-json`, the only place the text is written);
     - `fileName` (`(FileStem.ofText "").withExtension extension`);
     - `tryCreate`.
- `tryCreate` checks every pair for a repeated id + version first, then every pair of one kind for
  file names equal under `StringComparison.OrdinalIgnoreCase`. The rows are `manifestRow`s sorted by
  kind tag, then by `String.CompareOrdinal` on the file name.
- Tests: three facts appended to `LibraryEntryFilesTests`, under the exact slice ids.
- ELEV round 1 found 2 × ELEV010P on private static helpers of `CatalogueManifest`, because their
  signatures mixed our types with a bare primitive:
  - `describeEntry : LibraryEntryEnvelope -> string`;
  - `compareRows : CatalogueManifestRow -> CatalogueManifestRow -> int`.

  The fix moved the label to `LibraryEntryContent.describe`, a property of the type it renders, and
  inlined the comparison as the sort's lambda. Round 2 is clean.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/LibraryEntryFiles.fs` (+96 / −2): two
  error cases, `LibraryEntryContent.describe`, `CatalogueManifestRow`,
  `LibraryEntryEnvelope.manifestRow`, `CatalogueManifest` with its four static members.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/LibraryEntryFilesTests.fs` (+91): three
  hash literals, a test catalogue id and name, three envelopes (material, sample, source), the
  `renamed` helper and the three facts.

## Testing state

The arc-runner's gate engine is the gate authority. These are local observations only, made so the
round ends on code that compiles and passes:

- `build` command (`dotnet build Berreman.slnx -c Release -nologo -v:m`, from `Berreman/`): exit 0,
  `0 Warning(s)`, `0 Error(s)` on the final rebuild (`.artifacts/0026-build.log`).
- The constructor suite, whole (`dotnet test --no-build -c Release -p:Platform=x64`):
  `Test run summary: Passed!`, total 754, succeeded 754, failed 0, skipped 0. That is 751 from step 25
  plus the three new facts (`.artifacts/0026-constructor-unit-tests-whole.log`). Running
  `--list-tests --filter-class OpticalConstructor.Tests.LibraryEntryFilesTests` discovers the three
  slice ids exactly as written.
- The ELEV script (`Elevation/Run-ElevationGate.ps1 -ExcludeRules ELEV004`) reports
  `ELEV-GATE: OK - 0 new finding(s) in 8 project(s), 155s` with 0 known findings. Domain was expanded
  to its dependents (`.artifacts/0026-elevation-local.log`).
- The six s04 invariants, decoded from the gate snapshot and run with `git grep`: each finds nothing
  (exit 1).
- Line endings: both files are LF with zero CR bytes, and `git diff --numstat` equals
  `--ignore-cr-at-eol` (96/2 and 91/0).

Every requirement of the slice is delivered in this round: (1) the row and `manifestRow`, (2) the
manifest with `currentSchemaVersion`, `extension`, `fileName` and `tryCreate`, both refusals in
`LibraryEntryError`, the sort rule, no stored manifest hash, and tests (a), (b) and (c).

## Artifacts

- `.artifacts/0026-build.log`
- `.artifacts/0026-constructor-unit-tests-whole.log`
- `.artifacts/0026-elevation-local.log`

## Gotchas

- The first build of the round recompiled the vendored MathNet and showed its two
  `Exceptions.cs` SYSLIB0051 warnings. Earlier rounds recorded the same pair on the start tree. They
  sit outside this step's authorized edits, and the final incremental rebuild shows 0 warnings.
- Interpretation: when two envelopes are one id + version, that refusal wins over the file-name
  refusal. A duplicated entry usually also repeats its file name, and "one entry version twice" is
  the more specific diagnosis. The slice does not order the two checks.
- A catalogue may hold one id at two versions, as long as their file names differ. The slice
  refuses only a repeated id + version, and no test fixes this either way.
- ELEV010P also fires on a PRIVATE static member when its signature names one of our types beside
  a bare primitive. Only a member whose primitive belongs to its own type is exempt. Put a render
  on the rendered type (`describe`) and keep comparators as lambdas.
