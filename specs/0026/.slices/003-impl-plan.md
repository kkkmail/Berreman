# 003 — impl-plan: Localization (EN/RU), Part I

## Approach

Add a centralized, runtime-read, human-editable EN/RU string resource and the
loader/completeness machinery around it, sequenced before the big UI slices so
later ribbon/menu/catalogue text resolves through it from the start.

1. **`Localization.fs` (new, Ui project).** A pure module: a `Language` fieldless
   DU (`English` | `Russian`), a parsed `Resource` (`key -> langCode -> value`), and
   the functions `parse` / `loadFromFile` / `lookup` / `check` / `startupCheck`,
   plus a `Symbols` set of language-neutral scientific glyphs (Ψ, Δ, °, R1/R2/R3,
   unit symbols) kept OUT of the translatable resource. `lookup` falls back to the
   English entry, then to the key itself; `check` reports the keys missing for the
   active language and renders a copyable message. The resource is read at run time
   from `AppContext.BaseDirectory/strings.json`, mirroring the env-schema /
   `SchemaValidation.fs:17` build-copied-content-item + base-directory read.

2. **`strings.json` (new, Ui project).** A flat `{ key: { en, ru } }` catalogue
   seeded with realistic app/menu/settings keys, shipped complete in EN+RU. Added
   as a `<Content CopyToOutputDirectory=PreserveNewest>` item in
   `OpticalConstructor.Ui.fsproj` so it flows beside the assembly of every
   consuming project (App, Ui.Tests, Tests) — exactly how the env schema ships.

3. **`UserEnvironment.fs` (edit).** Add `language : Language` to
   `EnvironmentSettings`, defaulting to `English` in `defaults` (the sole full-record
   construction, so no other call site breaks). The shared `ProjectJson.options`
   already serialize a fieldless DU as a bare string, so the round-trip is free.

4. **`optical-constructor-environment.schema.json` (edit).** Add `language` to
   `required` + `properties` and a `language` `$def` (enum `English`/`Russian`), so
   validate-on-load admits the new field.

5. **`Program.fs` (edit).** Compute a startup localization report
   (`Localization.startupCheck settings.language`); when the shipped resource is
   missing/malformed or incomplete for the active language, open a small copyable
   error `Window` (read-only multiline `TextBox` + Copy button) ALONGSIDE the main
   window. The app still runs with the English fallback (never crash, never silent,
   AC-I3). The shipped resource is complete, so this never fires in the smoke host.

6. **Tests.** `LocalizationTests.fs` (new, Ui.Tests, gate `ui-tests`): AC-I1 runtime
   load + editability, AC-I3 missing-string fallback + copyable error + shipped-
   resource completeness, AC-I4 language-neutral symbols. Extend
   `EnvironmentRoundTripTests.fs` (gate `constructor-unit-tests`) with the AC-I2
   `language` persist/restore round-trip (the testing plan names this harness).

## Files to modify

New: `Localization.fs`, `strings.json`, `LocalizationTests.fs`.
Edit: `UserEnvironment.fs`, `optical-constructor-environment.schema.json`,
`Program.fs`, `OpticalConstructor.Ui.fsproj`, `OpticalConstructor.Ui.Tests.fsproj`,
`EnvironmentRoundTripTests.fs`.

## Risks

- The build-copy + base-directory read must mirror the env-schema content item or
  the runtime read fails in the published layout. Mitigation: add the Content item to
  the Ui fsproj exactly as the env schema is, and a test that loads from
  `resourcePath ()` (the base directory) proves the copy landed.
- The copyable error surface must not crash the headless smoke host. Mitigation: the
  shipped resource is complete (a dedicated test asserts it), so `startupCheck`
  returns `None` and the error window is never constructed under smoke; the Copy
  handler guards a null clipboard.
- Adding `language` as a required schema field invalidates any pre-existing
  `environment.json` lacking it. By design (`0.6` no-migration): `load` is total and
  falls back to defaults (English). Recorded in the impl-log Gotchas.
