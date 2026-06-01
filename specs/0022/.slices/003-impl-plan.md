# 003 — impl-plan: Storage core (canonical JSON, schema validate-on-load, `.binz` sidecars)

## Approach

Fill the slice-001 placeholder `OpticalConstructor.Storage` project with five modules
that realize Part I §I.1–§I.4 over the slice-002 `OpticalConstructorProject` aggregate:

1. **`Errors.fs`** — `StorageError` DU (sole error channel; `Result`, never throw).
2. **`SchemaValidation.fs`** — load the build-copied schema once, `Evaluate` a parsed
   `JsonNode` via JsonSchema.Net, return `Result<unit, StorageError>`.
3. **`ProjectJson.fs`** — shared `JsonSerializerOptions` (FSharp.SystemTextJson +
   a custom `Matrix<Complex>` converter), `serializeProject`/`deserializeProject`.
   `deserializeProject` validates against the schema *before* binding.
4. **`ProjectFile.fs`** — `saveProject`/`openProject` over `.ocproj`, wrapping all IO
   so exceptions become `FileIoError`.
5. **`Sidecar.fs`** — `writeSidecar`/`readSidecar` reusing
   `Softellect.Sys.Core.serialize`/`deserialize` with `BinaryZippedFormat` (`.binz`).

Plus the published schema `schema/optical-constructor-project.schema.json`
(envelope + storage-owned `$defs`; physics `$defs` left permissive for Parts B–G),
the fsproj wiring (compile order + content item + Numerics reference), the
`.gitignore` `*.binz` entry, and three test files.

## Compile order (deviation noted)

The splitter's planned order put `ProjectJson` before `SchemaValidation`, but R-2
requires `deserializeProject` (in `ProjectJson`) to *call* schema validation, so
`SchemaValidation.fs` MUST compile first. Final order:
`Errors → SchemaValidation → ProjectJson → ProjectFile → Sidecar`.

## Key decisions / risks

- **Envelope vs aggregate.** The aggregate has no `schemaVersion`; AC-I3 needs one.
  `serializeProject` serializes the aggregate to a `JsonObject` then injects
  `"schemaVersion": "1.0"`; the schema pins it with `const`. `deserializeProject`
  validates then binds (STJ ignores the extra `schemaVersion` property).
- **Math.NET round-trip.** `OpticalProperties` bottoms out at `Matrix<Complex>`,
  which STJ cannot round-trip natively. A custom `JsonConverter<Matrix<Complex>>`
  serializes it as rows of `[re, im]` pairs and rebuilds a dense matrix; Math.NET's
  element-wise value-equality then makes whole-project `=` hold.
- **Union encoding.** `JsonFSharpOptions.Default()` (single-case unwrap + option
  unwrap) `+ WithUnionUnwrapFieldlessTags()` so `UnitOfMeasure`/`BeamBranch`/the
  fieldless `ConstructorElement` cases serialize as plain strings.
- **Sidecar payload** for the test is a simple sweep table (`(float*float)[]`), not
  the Math.NET-heavy `calculate` output, to keep value-equality clean. `writeSidecar`
  of a project is never called (forbidden by §I.4).
