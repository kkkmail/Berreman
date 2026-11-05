# Step 0025 — IMPLEMENT — impl log

## Progress

- [x] Read the protocol (arc-runner, implement worker), the project prompt, the slice, 001 §A/§B and
      .spec-md §3–§6. The Operator note section is empty.
- [x] Grepped every new type, case and extension-member name across Berreman and STL
      (ELEV014/016/019). None exists. The only `toCategoryName` in the tree sits on `ControlCaption`,
      not on `EntryName`, so the decoders' `CategoryName` re-wrap is not half of a bijection (ELEV006).
- [x] `LibraryEntryFiles.fs`: payload, content, envelope, error, `CategoryName.toEntryName`, builders
      and decoders, all after step 24's types.
- [x] `LibraryEntryFilesTests.fs`: facts (a)–(e), named word for word as in the slice.
- [x] Local observations recorded below: build, constructor suite, ELEV script, s04 greps, line
      endings.
- [x] Attempt 02 (retry hint): fact (c) gains an assertion that an upper-case GUID preset id is
      refused, covering the tightened `tryToLibraryEntryId`. Nothing else changed. Rebuilt, re-ran
      the constructor suite and the six s04 greps.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/LibraryEntryFiles.fs` (+233 / −5):
  - `open Berreman.Fields`, for `WaveLength`.
  - The module doc now covers the content model.
  - `ElementValueId.tryToLibraryEntryId` accepts only the canonical lower-case `D` text (see
    Decisions).
  - `SamplePayloadData`, `SourcePayloadData`, `PolarizerPayloadData`.
  - `LibraryEntryPayload` (six cases) with `kind` and `currentSchema`.
  - `LibraryEntryError` (`EntryKindMismatchErr`, `EntryIdNotGuidErr`, `ProvenanceMismatchErr`,
    `MissingEntryFieldErr`, each `of reason : string`).
  - The extension `CategoryName.toEntryName`.
  - `LibraryEntryContent`, with:
    - `fileName`;
    - the builders `ofMaterialEntry`, `ofSample`, `ofSourcePreset`, `ofDetectorPreset`,
      `ofPolarizerPreset` and `ofMaterialCategory`;
    - the decoders `toMaterialEntry`, `toSample`, `toSourcePreset`, `toDetectorPreset`,
      `toPolarizerPreset` and `toMaterialCategory`, each marked
      `[<Softellect.Sys.ElevationBoundary(nameof Softellect.Sys.StorageBoundary, …)>]` with the
      slice's reason;
    - the private helpers `ofPayload`, `tryPresetEntryId`, `kindMismatchErr` and `presetId`.
      `presetId` carries the same mark.
  - `LibraryEntryEnvelope = { content; contentHash }`.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/LibraryEntryFilesTests.fs` (+132 / −2):
  - adds `open Berreman.Geometry` (for `Angle`) and widens the module doc;
  - adds the fixtures and five facts;
  - attempt 02: one more assertion in fact (c), see below.
- No `.fsproj` change. Both files were already compiled, by step 24.

## Decisions

- **The schema comes from `LibraryEntryPayload.currentSchema`**
  (`{ kind = this.kind; version = this.kind.currentSchemaVersion }`). The slice's
  `schema = { kind = payload.kind; version = kind.currentSchemaVersion }` is written once instead
  of once per builder, and the private `ofPayload` is the single place a content record is
  assembled. `ofMaterialEntry` and `ofSample` add their category or description with
  `{ … with … }`.
- **`tryToLibraryEntryId` is tightened to the canonical text** (`string g = this.value` after
  `Guid.TryParseExact(…, "D")`). `TryParseExact "D"` also accepts upper-case hex. Without the
  tightening, a preset with an upper-case GUID id would be filed and then decoded back lower-cased,
  so builder ∘ decoder would not be the identity that the slice requires ("A builder followed by its
  decoder answers the value it was given"). Every seeded id (`PresetIds`) and every minted one
  (`newSourceId`, `string (Guid.NewGuid ())`) is already canonical. Step 24's doc comment already
  promised "canonical", and its facts still pass.
- **Preset checks run in the slice's order.** The id is checked first (`EntryIdNotGuidErr`), then
  the protection against `provenance.protection` (`ProvenanceMismatchErr`). Both are in one private
  `tryPresetEntryId`, which the three preset builders share. It takes the id, name and protection
  separately, because the three preset records have no common type.
- **Error reasons name the entry** by name and id. They render no DU through interpolation: kinds
  render through `extension.value`, and the rest is prose. This follows ELEV002's intent, and the
  analyzer reports nothing.
- **Decoders match on the payload, not on `schema.kind`.** A material decoder given a material
  payload with `categoryOpt = None` answers `MissingEntryFieldErr`. Any other kind's payload
  answers `EntryKindMismatchErr` through a wildcard arm, so a seventh payload kind (S7.1) is refused
  by every existing decoder without an edit.
- **A preset's decoded id is `ElementValueId (string this.id.value)`**, in the private `presetId`,
  which carries the same boundary mark. Three decoders share it, so the GUID-to-string conversion is
  written once at the seam. No public reverse conversion is added (ELEV006).
- **The property-level mark is load-bearing (measured).** With the mark removed from
  `toMaterialCategory`, a Domain-only ELEV run reported exactly two ELEV001 findings:
  `CategoryId LibraryEntryId.value` and `CategoryName EntryName.value`. With the mark restored, the
  findings are gone. The file was put back byte for byte from a copy; `grep -c ElevationBoundary`
  answers 7 (six decoders plus `presetId`). Afterwards the solution was rebuilt and every
  observation below re-run.
- **Test fixtures.**
  - The intensity detector has no named seed value; it is picked from `seedEntries` by
    `PresetIds.detectorIntensity`.
  - The tunable source is `SeedSources.tunables`' Ti:sapphire (`PresetIds.tiSapphireTunable`).
  - The file-name fact uses `SeedSources.lasers`' `Ti:sapphire laser 800 nm`
    (`PresetIds.tiSapphire800`).
  - The user polarizer is a test-local two-component `ConstantMueller` (`CustomMueller`,
    `UserManaged`, under `UserProvenance`). It is filed at `VersionNumber.first.next`, which also
    shows that the version is carried.
  - The non-GUID preset id is `"my-own-polarizer"`. No `invariant-s04-no-seeded-string-preset-ids`
    pattern matches it.

## Attempt 02 — the added assertion

The retry hint asked to keep the tightened `ElementValueId.tryToLibraryEntryId` and cover its new
refusal. In `a preset or category filed under a provenance that disagrees is refused`, after the
`"my-own-polarizer"` case:

```fsharp
let upperCaseGuidPolarizer : PolarizerPreset = { userCompoundPolarizer with id = ElementValueId "5C0E9A7B-3D2F-4E61-8A4B-9F1C2D3E4A5B" }

match LibraryEntryContent.ofPolarizerPreset VersionNumber.first UserProvenance upperCaseGuidPolarizer with
| Error (EntryIdNotGuidErr _) -> ()
| other -> Assert.Fail($"a preset id in upper-case GUID text: expected EntryIdNotGuidErr, got %A{other}")
```

- The id is test-local. None of the six s04 patterns matches it (no `src-` / `det-` / `pol-` text).
- The assertion covers the tightening, not just the parse. An `fsi` probe showed
  `Guid.TryParseExact("5C0E9A7B-…", "D")` answers `true`, with canonical text
  `5c0e9a7b-3d2f-4e61-8a4b-9f1c2d3e4a5b` (not equal to the input). So only the
  `when string g = this.value` guard refuses this id; without it the builder would answer `Ok`.
- The domain file is unchanged in attempt 02. The suite's count is unchanged (751): this is an
  assertion inside an existing fact, not a new fact.

## Testing state

The arc-runner's gate engine is the gate authority. These are local observations only, made so the
round ends on code that compiles and passes:

- `build` command (`dotnet build Berreman.slnx -c Release -nologo -v:m`, from `Berreman/`): exit 0,
  `0 Warning(s)`, `0 Error(s)` (`.artifacts/0025-build.log`, attempt 02's rebuild after the added
  assertion; `OpticalConstructor.Tests.dll` was re-emitted).
- The constructor suite, whole (`dotnet test --no-build -c Release -p:Platform=x64`), attempt 02:
  `Test run summary: Passed!` with total 751, succeeded 751, failed 0, skipped 0. That is 746 from
  step 24 plus the five new facts (`.artifacts/0025-constructor-unit-tests-whole.log`). The
  touched fact alone (`--filter-method` on its full id) also passes, 1/1.
  `--list-tests --filter-class OpticalConstructor.Tests.LibraryEntryFilesTests` discovers nine facts,
  among them the five slice ids exactly as written.
- The ELEV script (`Elevation/Run-ElevationGate.ps1 -ExcludeRules ELEV004`):
  `ELEV-GATE: OK - 0 new finding(s) in 8 project(s), 161s`, with 0 known findings. Domain was
  expanded to its 8 dependents. The test file is out of analyzer scope, as the script notes
  (`.artifacts/0025-elevation-local.log`).
- The six s04 invariants, decoded from the gate snapshot and run with `git grep`, each find nothing
  (exit 1), re-run in attempt 02. Both touched files are already tracked, so `git grep` sees them.
- The ELEV run above is from attempt 01. Attempt 02 touches only the test file, which is outside
  the analyzers' scope, so it was not re-run.
- Line endings: both files are LF / UTF-8 with zero CR bytes. `git diff --numstat` equals
  `--ignore-cr-at-eol`: 233/5 and 132/2.

Every requirement of the slice is delivered in this round: (1) the payload, (2) the content,
envelope and `fileName`, (3) the error union, (4) the builders with `CategoryName.toEntryName`,
(5) the marked decoders, and tests (a) to (e).

## Artifacts

- `.artifacts/0025-build.log`: the solution build.
- `.artifacts/0025-constructor-unit-tests-whole.log`: the whole constructor suite.
- `.artifacts/0025-elevation-local.log`: the ELEV script over Domain and its dependents.

## Gotchas

- **Record-label inference.** `LibraryEntryContent` adds the labels `id`, `name`, `version` and
  `schema` in a module that also opens `Library` and `MaterialLibrary`. Any `{ id = …; name = … }`
  written after it, in a file that opens `LibraryEntryFiles`, infers the LAST record with those
  labels. Every domain record built here, decoders and tests included, is type-annotated
  (`let sample : Sample = { … }`). Keep doing that in the files that open this module (steps 26–30).
- **The decoders' boundary marks are load-bearing.** Removing one brings back ELEV001 on the id and
  name re-wraps. Do not "tidy" them away.
- **Run the ELEV script from outside `Elevation/bin/reports`.** A shell whose working directory is
  that folder locks it, and the driver then crashes with "being used by another process".
- `specs/0050/.manifest.state.json` shows as modified in the working tree. It is the supervisor's
  file, and this worker did not touch it.
