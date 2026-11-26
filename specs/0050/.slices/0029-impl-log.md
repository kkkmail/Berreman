# Step 0029 — IMPLEMENT — impl-log

## Progress

- [x] `SchemaValidation.validateWith` holds the evaluation body. `validate` calls it over the project
  lazy, so `ProjectJson` is unchanged.
- [x] `GroupsLibrary.validateGroupsLibrary` calls `validateWith` over its groups lazy. Its copy of the
  body is deleted.
- [x] `UserEnvironment.validateUserEnvironment` calls `validateWith` over `envSchema`. Its copy of the
  body, which collected top-level errors only, is deleted.
- [x] Local runs: build, constructor-unit-tests (whole), ui-smoke, the three selected ui-tests classes,
  elevation, and the six s04 invariants.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Storage/SchemaValidation.fs`
  - Added `validateWith (schema : Lazy<Result<JsonSchema, StorageError>>) (element : JsonElement) : Result<unit, StorageError>`.
    It holds the old `validate` body: `EvaluationOptions(OutputFormat = OutputFormat.List)`,
    `collectMessages` and the `schema validation failed` fallback.
  - `validate` is now `validateWith schema element`.
  - `collectMessages` is now `private`, because `validateWith` is its only caller. Its doc no longer
    says "public so the groups store reuses this", which would have gone stale.
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/GroupsLibrary.fs`
  - `validateGroupsLibrary` is now `validateWith schema element`. It keeps its name, its private
    `schema` lazy and its `optical-constructor-groups.schema.json` (so its `$id`).
  - Its doc and the module doc say it reuses `validateWith`. Before, they pointed at the public
    `collectMessages` and at `SchemaValidation.fs:61`.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/UserEnvironment.fs`
  - `validateUserEnvironment` is now `validateWith envSchema element`. It keeps its name, its
    `envSchema` lazy, `schemaFileName` and its `$id`.
  - Its doc says it reuses `validateWith`, and that nested instance locations are now kept.

No test file changed: the slice authorizes none, and the round-trip facts must pass unchanged.

## Decisions

- **One fallback text.** The groups fallback (`groups schema validation failed`) and the environment
  fallback (`environment schema validation failed`) are now the shared `schema validation failed`. The
  slice names that text as part of the one body. `GroupsRoundTripTests` asserts that the
  groups-specific text is absent, and `EnvironmentRoundTripTests` only that messages are present, so
  both hold unchanged.
- **The parameter is named `schema`, as the slice writes the signature.** The loaded value inside is
  named `loaded`, so the old `Ok schema` shadowing of the lazy is gone.
- **`collectMessages` became private.** Its last outside caller (groups) now goes through
  `validateWith`. The ELEV gate reports no ELEV003P for it.

## Testing state

Local observations only. The arc-runner's gate engine is the gate authority.

- `build` (`dotnet build Berreman.slnx -c Release -nologo -v:m`, from `Berreman/`): exit 0.
  - First build: `2 Warning(s)`, both `SYSLIB0051` in the vendored `MathNetNumerics/Numerics/Exceptions.cs`
    (see Gotchas).
  - Rebuild after a comment-only rewrap: `0 Warning(s)`, `0 Error(s)`.
- `constructor-unit-tests`, whole suite: `Passed!`, 761 total / 761 succeeded / 0 failed / 0 skipped.
  - The TRX shows `RoundTripTests` (3), `GroupsRoundTripTests` (9) and `EnvironmentRoundTripTests`
    (33), all passed, unchanged.
- `ui-smoke`: `Passed!`, 208/208.
- `ui-tests`, selected (`AppShellSeamTests`, `DataFilePickerFolderTests`, `LocalizationTests`):
  `Passed!`, 26/26.
- `elevation`: `ELEV-GATE: OK - 0 new finding(s) in 5 project(s)` (Storage and Ui plus their
  dependents; known findings 0).
- Six s04 invariants: each reports `INVARIANT HOLDS ... found nothing`.
- EOL: `git diff --numstat` equals `--ignore-cr-at-eol` on the three files, so there is no CRLF churn.

## Artifacts

- `specs/0050/.artifacts/0029-build.log` and `0029-build-final.log`
- `specs/0050/.artifacts/0029-constructor-unit-tests-whole.log`
- `specs/0050/.artifacts/0029-ui-smoke.log`
- `specs/0050/.artifacts/0029-ui-tests-selected.log`
- `specs/0050/.artifacts/0029-elevation-local.log`

## Gotchas

- **`Report.validateEntryElement` (`OpticalConstructor.Storage/Report.fs:79-86`) still evaluates on its
  own.** It is a schema evaluation of the same shape, but it reports a fixed message
  (`document violated the materialEntry $def`), not `collectMessages`. Its wrapper schema has no
  `$id`. The slice names exactly two copies to delete (groups and environment) and does not authorize
  edits to `Report.fs`, so it is unchanged. A later step that touches it can route it through
  `validateWith`, though that would change its message.
- **`SYSLIB0051` from the vendored MathNet appears only when MathNet recompiles.** The same two warnings
  are in the logs of steps 20, 23 and 24. Incremental builds (21, 22, 25–28, and this step's final
  build) show none. They come from `MathNetNumerics/Numerics/Exceptions.cs`, outside this step's
  authorized effects. The build gate's `stdout_match` (`(?!.*error)`) does not match them.
