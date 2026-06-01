# 003 — impl-log: Storage core

## Progress

- [x] Errors.fs — `StorageError` DU (6 required cases)
- [x] SchemaValidation.fs — validate-on-load via JsonSchema.Net (schema cached once)
- [x] ProjectJson.fs — serialize/deserialize + shared options + `Matrix<Complex>` converter
- [x] ProjectFile.fs — saveProject/openProject (`.ocproj`)
- [x] Sidecar.fs — writeSidecar/readSidecar (`.binz`)
- [x] schema/optical-constructor-project.schema.json (envelope + storage-owned `$defs`)
- [x] fsproj wiring (compile order + content item + Numerics ref) + .gitignore `*.binz`
- [x] tests: ProjectJsonRoundtripTests, RoundTripTests, SidecarTests (9 new)
- [x] gates green

## Files modified

New:
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/Errors.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/SchemaValidation.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/ProjectJson.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/ProjectFile.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/Sidecar.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/schema/optical-constructor-project.schema.json`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/ProjectJsonRoundtripTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/RoundTripTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/SidecarTests.fs`

Edited:
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/OpticalConstructor.Storage.fsproj`
  (compile order, schema content item; retry-02: dropped the direct Numerics.csproj
  reference — Math.NET flows transitively via Domain → Berreman → Numerics)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj`
  (register the three new test files)
- `.gitignore` (`*.binz`)

## Decisions / issues encountered

- **`schemaVersion` is an envelope concern, not an aggregate field.** The §A.7
  aggregate has no `schemaVersion`; AC-I3 needs one. `serializeProject` injects
  `"schemaVersion": "1.0"` into the serialized `JsonObject`; the schema pins it
  with `const`; `deserializeProject` validates (so a mismatch → `SchemaValidationError`)
  then binds, with STJ ignoring the extra property.
- **Compile order deviates from the splitter's hint.** R-2 requires
  `deserializeProject` (in `ProjectJson`) to call schema validation, so
  `SchemaValidation.fs` MUST precede `ProjectJson.fs`. Final order:
  `Errors → SchemaValidation → ProjectJson → ProjectFile → Sidecar`.
- **`Matrix<Complex>` JSON converter.** `OpticalProperties` bottoms out at the
  Math.NET `Matrix<Complex>`, which STJ cannot round-trip natively. A custom
  `JsonConverter<Matrix<Complex>>` serializes it as rows of `[re, im]` pairs and
  rebuilds a dense matrix; Math.NET's element-wise value-equality then makes the
  whole-project `=` round-trip (AC-I1) hold.
- **Schema must be loaded ONCE.** `JsonSchema.FromText` registers the document by
  `$id` in a process-global registry; reloading the same `$id` throws
  ("Overwriting registered schemas"). Caching behind `lazy` fixed the first
  red test run. `EvaluationResults.Errors` AND `.Details` can both be `null`;
  the message-walker guards both (fixed a second red run).
- **Sidecar test payload** is a `(float*float)[]` sweep table (value-comparable),
  not the Math.NET-heavy `calculate` output, so round-trip identity is exact.
  `writeSidecar` is never called with an `OpticalConstructorProject` (forbidden,
  §I.4 / binding rule 4).

## Retry-02 (2026-05-31)

Narrow recovery round per the operator/retry hint — schema, serializer, and tests untouched:
- **`.gitignore` LF restore.** The previous round rewrote the whole file CRLF, so the diff was
  667 changed lines. Rebuilt from `git show HEAD:.gitignore` (LF, no BOM) and re-appended ONLY the
  `*.binz` block; `git diff` is now exactly +5 insertions. `*.autosave` deliberately NOT added (slice 013).
- **Dropped the direct `Numerics.csproj` ProjectReference** on `Storage.fsproj`. Verified Math.NET
  flows transitively (Storage → OpticalConstructor.Domain → Berreman → Numerics.csproj); rebuilt at
  Release/x64 — `Build succeeded.`, 0 errors. All three gates re-run green.

## Testing state

All gates pass locally (logs under `C:\GitHub\Berreman\specs\0022\.artifacts\`):
- `build` — exit 0, "Build succeeded.", 0 Error(s), no lowercase `error`. (`003-build.log`)
- `unit-tests` — exit 0, Passed 70 / Skipped 5 / Total 75; baseline `berreman_unit_tests = 70` held; `BerremanTests` untouched. (`003-unit-tests.log`)
- `constructor-unit-tests` — exit 0, Passed 27 / Total 27 (up from slice-002 baseline 18; +9 new). (`003-constructor-unit-tests.log`)

## Artifacts

- `003-build.log`, `003-unit-tests.log`, `003-constructor-unit-tests.log`
