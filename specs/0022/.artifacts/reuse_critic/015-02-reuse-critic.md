# Reuse critique -- 015.slice-md cycle 1

## Coverage

- Helper roots walked: `C:\GitHub\Berreman` (focused on `Berreman/OpticalConstructor/` — Storage, Domain, Ui, Tests).
- Files inspected: ~18/200 (diff additions + the slice-003 storage core, slice-013 recent-files, Units, ChartSettings).
- Extensions: `.fs`, `.json` (`.md` inputs read for context).
- Diff read: `git diff HEAD` + the four new `.fs`/`.json` worker files (untracked) and the two `.fsproj` edits.
- Cap: not tripped; the walk was scoped to the reuse-relevant storage/UI surface the slice declares it builds on.

## Findings

### F1: `UserEnvironment` re-implements the slice-003 schema-validation machinery (and diverges from it)

- **Worker added:** `UserEnvironment.envSchema` (lazy schema load, `UserEnvironment.fs:217-223`), `schemaFullPath` (`:211-212`), and `validate` (`:228-241`).
- **Existing helper:** `SchemaValidation.schema` (`OpticalConstructor.Storage/SchemaValidation.fs:29-36`), `schemaFullPath` (`:20-21`), `validate` (`:61-73`), and crucially `collectMessages` (`:42-56`).
- **Why it matters:** The two are near-identical in shape — lazy single-load by `$id`, `SchemaMissing`/`FileIoError` on absence/read failure, then `Evaluate` with `OutputFormat.List` and a flatten-to-`SchemaValidationError` step. This is not harmless restatement: the worker's inline message collector (`UserEnvironment.fs:236-240`) reads only the top-level `results.Errors`, whereas `SchemaValidation.collectMessages` *recurses into `results.Details`* (`SchemaValidation.fs:50-54`). So a nested env-schema violation will surface with thinner (or empty → the "environment schema validation failed" fallback) diagnostics than the project path produces for the equivalent failure. That is exactly the divergent-duplicate maintenance risk the rubric targets: one of the two copies is already behaviourally behind the other.
- **Suggested action:** Extract a shared, schema-file-parameterised helper in `OpticalConstructor.Storage` (e.g. `SchemaValidation.validateAgainst (relativePath) element`, reusing the existing `collectMessages`) and have `UserEnvironment.validate` call it. `UserEnvironment.fs` already `open`s `OpticalConstructor.Storage` (`:36`), so the dependency direction is fine; only the hard-coding of `SchemaValidation` to `optical-constructor-project.schema.json` (`SchemaValidation.fs:17-18`) blocks direct reuse today. (Note: the SoW at `015-state-of-the-world.md:66-73` consciously "mirrors `SchemaValidation.fs`" because the env schema lives in the Ui project — a defensible boundary call, but mirroring `collectMessages` *incompletely* is the part that should not stand.)

### F2: per-user application-data path duplicated from `RecentFiles`

- **Worker added:** `UserEnvironment.settingsPath` (`UserEnvironment.fs:279-284`).
- **Existing helper:** `RecentFiles.prefsPath` (`OpticalConstructor.Storage/RecentFiles.fs:28-33`).
- **Why it matters:** Both compute the identical product app-data directory — `Path.Combine(GetFolderPath ApplicationData, "Softellect", "Berreman", "OpticalConstructor")` — and only differ in the trailing filename (`environment.json` vs `recent.json`). The worker's own doc-comment concedes it "Mirrors the slice-013 recent-files path convention." The four-segment directory literal now lives in two modules; a relocation of the product's app-data root (or a rename of the `Softellect/Berreman/OpticalConstructor` triple) must be made in lock-step in both or the two per-user files silently split across folders.
- **Suggested action:** Extract the directory derivation into a single shared helper (e.g. `appDataDir ()` in Storage) and have both `prefsPath` and `settingsPath` `Path.Combine` only their filename onto it. `RecentFiles.prefsPath` is currently `private`, so the fix is to promote the shared *directory* piece, not the whole path function.

### F3: envelope-injecting serialize/deserialize wrappers duplicated from `ProjectJson`

- **Worker added:** `UserEnvironment.serialize` (`UserEnvironment.fs:246-252`), `deserialize` (`:257-270`), and the `schemaVersion` literal `"1.0"` (`:203-204`).
- **Existing helper:** `ProjectJson.serializeProject` (`OpticalConstructor.Storage/ProjectJson.fs:85-91`), `deserializeProject` (`:98-111`), and `ProjectJson.schemaVersion` (`:21-22`).
- **Why it matters:** `serialize` is a line-for-line restatement of `serializeProject`: `JsonSerializer.SerializeToNode(_, ProjectJson.options)` → `.AsObject()` → `envelope.["schemaVersion"] <- JsonValue.Create(schemaVersion)` → `ToJsonString(ProjectJson.options)`, with the same `JsonParseError e.Message` catch. `deserialize` likewise mirrors `deserializeProject`'s parse → `validate` → `Deserialize<_>` skeleton with identical error handling. The slice's load-bearing reuse directive — reuse the *shared `ProjectJson.options`* — **is** honoured (good, and it is what the SoW emphasises). What is reinvented is the envelope *wrapper* around those options, plus a second `schemaVersion = "1.0"` constant that can now drift from `ProjectJson.schemaVersion` independently.
- **Suggested action:** Extract a generic envelope codec in Storage (e.g. `serializeWithEnvelope options version value` / `deserializeValidated validate options json`) reused by both the project and the environment paths, so the version-key injection and the validate-then-bind sequence exist once. Leave-as-is is also defensible if the judge weighs the cross-module-generic cost against three short wrappers — but the duplicated literal at minimum should reference a single source.

## Bottom line

The diff gets the spec's *named* reuse obligation right — it serialises through the shared `ProjectJson.options` rather than standing up a second JSON stack — but it reinvents three lower-level pieces of the slice-003/013 storage core (schema validation, the app-data path, and the envelope serialize/deserialize wrappers) that already exist a project away. F1 is the one I'd weight: it is not merely duplicated but already *divergent* (the inline message collector drops the nested-`Details` recursion the original has), which is a latent behaviour gap rather than cosmetic repetition. F2/F3 are ordinary extract-a-shared-helper candidates and partly acknowledged in the SoW; none are correctness blockers and the judge may reasonably route them as follow-up cleanups rather than a re-spawn.
