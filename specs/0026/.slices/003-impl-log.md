# 003 — impl-log: Localization (EN/RU), Part I

## Progress

- [x] `Localization.fs` — `Language`, `Resource`, parse/load/lookup/check/startupCheck, `Symbols`
- [x] `strings.json` — EN/RU catalogue, shipped complete
- [x] `OpticalConstructor.Ui.fsproj` — register `Localization.fs` (before `UserEnvironment.fs`) + `strings.json` content item
- [x] `UserEnvironment.fs` — add `language` field + default `English`
- [x] `optical-constructor-environment.schema.json` — add `language` (required + property + $def)
- [x] `Program.fs` — startup completeness check + copyable error window
- [x] `LocalizationTests.fs` — AC-I1 / AC-I3 / AC-I4 (8 tests) (+ register in Ui.Tests fsproj)
- [x] `EnvironmentRoundTripTests.fs` — AC-I2 language round-trip (2 tests)
- [x] all touched files normalized to LF (host tooling emits CRLF; repo is LF)
- [x] gates green (build, unit-tests, constructor-unit-tests, ui-smoke, ui-tests, structural)

## Files modified

New:
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/Localization.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/strings.json`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/LocalizationTests.fs`

Edited:
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/UserEnvironment.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/optical-constructor-environment.schema.json`
- `Berreman/OpticalConstructor/OpticalConstructor.App/Program.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/OpticalConstructor.Ui.fsproj`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/OpticalConstructor.Ui.Tests.fsproj`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/EnvironmentRoundTripTests.fs`

## Decisions / issues encountered

- **`Language` defined in `Localization.fs`, which compiles before `UserEnvironment.fs`.**
  `EnvironmentSettings.language : Localization.Language`. The loader depends on nothing
  project-internal (only System.Text.Json), so it stays a standalone headless-testable
  layer and the dependency is one-way (UserEnvironment → Localization). The Ui fsproj
  registers `Localization.fs` immediately before `UserEnvironment.fs`.
- **Reused the shared JSON stack for the round-trip (I.5 / 0.7).** `Language` is a
  fieldless DU; `ProjectJson.options` already serialize fieldless DUs as bare strings
  (`WithUnionUnwrapFieldlessTags`), so it serializes as "English"/"Russian" and the
  schema constrains it with an `enum`. No second serializer / converter needed.
- **`strings.json` read mirrors the env schema exactly (I.5 / SchemaValidation.fs:17).**
  Added as a `<Content CopyToOutputDirectory=PreserveNewest>` item in the Ui fsproj
  (only there — it flows transitively to App / Ui.Tests / Tests outputs, confirmed in
  bin) and read from `AppContext.BaseDirectory`. Copying rather than embedding is what
  lets a translator edit it with no rebuild (AC-I1); a test loads from `resourcePath ()`
  to prove the copy lands at runtime.
- **Scientific symbols kept OUT of the translatable resource (I.4.1).** Ψ/Δ/°/R1-R3 +
  unit symbols are compiled `Symbols` constants resolved by `neutralSymbol` (language
  ignored). A test also asserts no shipped entry's value equals a neutral glyph, so the
  do-not-translate rule cannot regress through a `strings.json` edit.
- **Copyable error surface is additive + headless-safe (AC-I3, Risks).** `Program.fs`
  computes `Startup.localizationError` once; only when it is `Some` does it open a small
  `Window` (read-only multiline `TextBox` + Copy button guarding a null clipboard),
  deferred to the main window's `Opened`, so the shell still renders on the English
  fallback. The shipped resource is complete (asserted by a test), so the error window
  is never constructed under the smoke host and `App` framework-init is unchanged.
- **`language` added as a *required* schema field — no migration (0.6).** An older
  `environment.json` lacking `language` now fails validation; `load` is total and falls
  back to defaults (English), which is the designed no-migration behaviour. Recorded
  per "don't ask the user".
- **`EnvironmentRoundTripTests.fs` edited though not in the slice's file-scope list.**
  The testing plan explicitly routes the AC-I2 `language` round-trip through "the
  existing `EnvironmentRoundTripTests` harness" (`constructor-unit-tests`), so two
  minimal AC-I2 tests were added there (default = English; serialize/deserialize and
  on-disk save/load preserve a chosen `Russian`). No existing test/behaviour changed.
- **Host tooling emits CRLF; repo is LF (autocrlf=false, no `.gitattributes`).** Every
  touched file was normalized back to LF after editing — `git diff` then shows only the
  real ~112-line insertion with zero whole-file churn (verified 0 CR bytes).

## Testing state

All seven applicable gates pass locally, re-run after LF normalization (logs under
`C:\GitHub\Berreman\specs\0026\.artifacts\`):

- `build` — exit 0, "Build succeeded.", 0 Error(s), 0 lowercase `error` matches (the
  gate's `stdout_match` veto). Log `003-build.log`.
- `unit-tests` — exit 0, Passed 84 / Skipped 5 / Total 89; `BerremanTests` untouched.
  Baseline `berreman_unit_tests = 84` held. Log `003-unit-tests.log`.
- `constructor-unit-tests` — exit 0, Passed 220 / Failed 0 / Total 220 (218 prior + 2
  new AC-I2 `EnvironmentRoundTripTests`). Log `003-constructor-unit-tests.log`.
- `ui-tests` — exit 0, Passed 33 / Failed 0 / Total 33 (+8 new `LocalizationTests`,
  Category!=ui-smoke; verified the 8 ran via a `~LocalizationTests` filter).
  Log `003-ui-tests.log`.
- `ui-smoke` — exit 0, Passed 1; the headless host boots and renders with
  `strings.json` resolving at run time (no localization error window, resource
  complete). Log `003-ui-smoke.log`.
- `impl-log-structure`, `state-of-world-structure` — built-in structural checks; this
  impl-log and the sibling state-of-the-world carry all required ATX headings.

Coverage: AC-I1 (runtime load from the base directory + edit-without-rebuild),
AC-I2 (language default English + serialize/deserialize + on-disk save/load round-trip),
AC-I3 (English fallback + copyable completeness message naming missing keys +
missing/malformed resource surfaced not thrown + shipped resource complete so startup is
clean), AC-I4 (language-neutral symbols + no neutral glyph carried as a translatable
value). None deferred.

## Artifacts

- `003-build.log`, `003-unit-tests.log`, `003-constructor-unit-tests.log`,
  `003-ui-tests.log`, `003-ui-smoke.log`
