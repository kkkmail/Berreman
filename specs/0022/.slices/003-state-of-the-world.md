# 003 — State of the world: Storage core (canonical JSON, schema validate-on-load, `.binz` sidecars)

## Where we are

Slice 003 pulls the storage/JSON seam in EARLY (before the feature parts) so every
later part fills its `$defs` and serializes against one already-validated envelope.
It fills the slice-001 placeholder `OpticalConstructor.Storage` project with the four
[Core] storage directives of Part I (§I.1–§I.4) over the slice-002
`OpticalConstructorProject` aggregate: canonical `System.Text.Json` +
FSharp.SystemTextJson round-trip, the published JSON Schema with validate-on-load via
JsonSchema.Net, `.ocproj` file read/write through the single project-open seam, and
`.binz` sidecars for derived/bulk artefacts only. `StorageError` is the sole error
channel — every IO function returns `Result<_, StorageError>` and never throws.

## What's working

- Add the `StorageError` DU as the sole, net-new storage error channel (`Errors.fs`).
- Add canonical JSON serialize/deserialize over `OpticalConstructorProject` via
  System.Text.Json + FSharp.SystemTextJson, writing SI fields verbatim with no unit
  conversion; a custom `Matrix<Complex>` converter round-trips the engine tensors.
- Author the published schema (`$id` https://berreman/schema/optical-constructor-project.json)
  with the envelope, the storage-owned `$defs` (materialEntry/sourceSpec/unitOfMeasure),
  and permissive physics-`$def` anchors for Parts B–G; wire validate-on-load before binding.
- Add `.ocproj` `saveProject`/`openProject` (the single project-open seam) with all
  filesystem access wrapped to `FileIoError`.
- Add `.binz` `writeSidecar`/`readSidecar` reusing `Softellect.Sys.Core.serialize`
  `BinaryZippedFormat` for derived artefacts only; ignore `*.binz` in `.gitignore`.
- Add 9 unit tests (round-trip identity, schema rejection, version-mismatch rejection,
  default-unit + SI preservation, sidecar identity, missing-sidecar) — all green.

## Tests

- `build` — PASS (exit 0, "Build succeeded.", 0 Error(s); no lowercase `error`). Log `.artifacts/003-build.log`.
- `unit-tests` — PASS (exit 0, Passed 70 / Skipped 5 / Total 75; `BerremanTests` untouched). Baseline `berreman_unit_tests = 70` held. Log `.artifacts/003-unit-tests.log`.
- `constructor-unit-tests` — PASS (exit 0, Passed 27 / Total 27; up from slice-002 baseline of 18). Log `.artifacts/003-constructor-unit-tests.log`.

Coverage: AC-A2 (validate-on-load admits valid; ProjectJsonRoundtripTests + RoundTripTests
AC-I2); AC-A5/AC-D6 (SI magnitudes verbatim, defaultUnit label preserved); AC-I1
(`saveProject`→`openProject` whole-project `=` identity); AC-I2 (schema violation →
`SchemaValidationError`, binds nothing); AC-I3 (`schemaVersion` mismatch → `SchemaValidationError`,
no migration); AC-A6/AC-I4 (`.binz` sweep-table write/read identity, `.binz` extension);
AC-I5 (missing sidecar → `SidecarMissing`). None deferred.

## Architecture

- **`schemaVersion` lives in the envelope, not the aggregate.** The §A.7 aggregate has
  no version field; `serializeProject` injects `"schemaVersion": "1.0"` into the
  serialized object and the schema pins it with `const`. A mismatch fails validation
  (AC-I3) — no migration path exists (binding rule 6 / §I.2 out-of-scope).
- **Reflection-based serializer, extensible by schema.** FSharp.SystemTextJson with
  single-case-union unwrap + option unwrap + fieldless-tag unwrap round-trips records,
  DUs, and options idiomatically (`UnitOfMeasure`/`BeamBranch`/nullary `ConstructorElement`
  cases as plain strings). Later slices extend the aggregate and the schema, NOT the serializer.
- **Custom `Matrix<Complex>` converter.** The only Math.NET-backed leaf reachable from
  the aggregate is `OpticalProperties`' eps/mu/rho (`ComplexMatrix3x3` → `Matrix<Complex>`).
  System.Text.Json cannot round-trip it natively, so a `JsonConverter<Matrix<Complex>>`
  serializes rows of `[re, im]` pairs; Math.NET value-equality then makes whole-project `=` hold.
- **Compile order forced by the validate-on-load dependency:**
  `Errors → SchemaValidation → ProjectJson → ProjectFile → Sidecar` (see Gotchas).
- **Schema loaded once.** `JsonSchema.FromText` registers globally by `$id`; the loader
  caches behind `lazy` so validate-on-load is idempotent across calls.
- **No direct `Numerics.csproj` reference on `Storage.fsproj`.** Math.NET (`Matrix<Complex>`,
  used by the JSON converter) flows in transitively: Storage → OpticalConstructor.Domain →
  Berreman → `Numerics.csproj`. The direct reference was dropped (build stays green), so no
  new C# project reference is invented here (binding rule 1).

## Deferred

- Physics `$defs` (`opticalSystem`, `layer`, `opticalProperties`, `beamNode`, `beamBranch`,
  `constructorElement`) are permissive anchors here; Parts B–G fill them (no re-shaping).
- `materialEntry` dispersion sub-object → slice 004; `sourceSpec` → slice 007;
  `chartSettings` → slice 012.
- Recent files / autosave / undo-redo / CSV-Excel export / report / material import-export
  and the `*.autosave` `.gitignore` entry → slice 013 (§I.5–§I.8).
- Deeper beam-tree `$def` round-trip (AC-B9) → slice 005.

## Gotchas

- **`JsonSchema.FromText` registers the document by `$id` in a process-global registry.**
  Loading the same `$id` twice throws "Overwriting registered schemas is not permitted".
  The schema is therefore loaded ONCE behind `lazy`; do not call `FromText` per-validate.
- **`EvaluationResults.Errors` and `.Details` can both be `null`** (not empty) on a
  JsonSchema.Net result node; the message-walker guards both before iterating.
- **Compile order deviates from the splitter's planned `ProjectJson`-before-`SchemaValidation`.**
  R-2 requires `deserializeProject` (in `ProjectJson`) to call validate-on-load, so
  `SchemaValidation.fs` MUST compile first. Chosen per `arc-runner.system-md` (spec
  requirements over the planned fsproj hint).
- **`writeSidecar` is never called with an `OpticalConstructorProject`** — the canonical
  project MUST NOT be FsPickler-serialized (binding rule 4 / §I.4). Sidecars are derived
  data only (sweep tables, field maps, fit histories).
- **The legacy placeholder `Storage.fs` is retained** as a namespace anchor and compiled
  last; it carries no code.

## Changelog

- 2026-05-31 (slice 003): Land the storage core — `StorageError` channel (`Errors.fs`),
  canonical System.Text.Json serialize/deserialize with a `Matrix<Complex>` converter
  (`ProjectJson.fs`), the published schema + JsonSchema.Net validate-on-load
  (`SchemaValidation.fs`, `schema/optical-constructor-project.schema.json`), `.ocproj`
  read/write (`ProjectFile.fs`), and `.binz` sidecars reusing Softellect `BinaryZippedFormat`
  (`Sidecar.fs`). Add `*.binz` to `.gitignore`. Add 9 unit tests; build green at Release/x64;
  all three gates pass (constructor-unit-tests 27, berreman_unit_tests 70 held).
- 2026-05-31 (slice 003, retry-02): Restore `.gitignore` to original LF line endings so the
  diff is the intended +5-line `*.binz` block only (was a 667-line CRLF churn). Drop the direct
  `Numerics.csproj` ProjectReference from `Storage.fsproj` — Math.NET resolves transitively
  through OpticalConstructor.Domain → Berreman; build + all three gates stay green.

```yaml
gates:
  berreman_unit_tests:    70
  constructor_unit_tests: 27
```
