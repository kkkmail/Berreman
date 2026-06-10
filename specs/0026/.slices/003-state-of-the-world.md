# 003 — State of the world: Localization (EN/RU), Part I

## Where we are

Slice 003 realises **Part I** of Spec 0026 (Optical Constructor UI): EN/RU
localization, net-new per constraint 0.7 (no `CultureInfo` localization,
`ResourceManager`, or `.resx` exists or is introduced). It is sequenced before the
big UI slices so the ribbon/menu/catalogue/control text authored later resolves
through `Localization.fs` from the start. It adds a centralized, runtime-read,
human-editable string resource (`strings.json`), the `Localization.fs` loader with a
startup completeness check and copyable error surface, a persisted/restored
`language` setting on `EnvironmentSettings`, and language-neutral scientific
notation. It deliberately does NOT add the *Settings*-ribbon selector control (Part D
/ slice 006), the key-map fields (slice 005), or the constructor-as-default-landing
edit (slice 006).

## What's working

- Add `Localization.fs`: a pure, runtime-read EN/RU string loader. `strings.json`
  is read from beside the assembly via `AppContext.BaseDirectory`, mirroring the
  env-schema build-copied content item (SchemaValidation.fs:17), so editing the file
  (or adding a language) changes the strings with no rebuild (AC-I1).
- Ship `strings.json`, a `{ key: { en, ru } }` catalogue complete in both languages,
  as an `OpticalConstructor.Ui.fsproj` content item that flows beside every consuming
  assembly (App, Ui.Tests, Tests).
- Persist and restore the selected language: add a `language` field to
  `EnvironmentSettings` defaulting to `English`, extend the environment schema with a
  `language` enum, round-tripping through the existing save/load path (AC-I2).
- Fall back to the English entry for any string missing in the active language and
  surface a copyable startup error naming the missing keys (`Localization.check` /
  `startupCheck`), wired in `Program.fs` as a separate copyable error window — the app
  still runs on the English fallback, never crashing or failing silently (AC-I3).
- Keep scientific symbols (Ψ, Δ, °, R1/R2/R3, unit symbols) language-neutral as
  compiled `Symbols` constants held OUT of the translatable resource, so a translation
  pass cannot localize them (AC-I4).
- Add `LocalizationTests.fs` (8 tests: AC-I1/AC-I3/AC-I4) and two AC-I2 round-trip
  tests in `EnvironmentRoundTripTests.fs`.

## Tests

All seven applicable gates pass locally (logs under
`C:\GitHub\Berreman\specs\0026\.artifacts\`):

- `build` — PASS (exit 0, "Build succeeded.", 0 Error(s), 0 lowercase `error`
  matches — the gate's `stdout_match` veto). Log `003-build.log`.
- `unit-tests` — PASS (exit 0, Passed 84 / Skipped 5 / Total 89; `BerremanTests`
  untouched). Baseline `berreman_unit_tests = 84` held. Log `003-unit-tests.log`.
- `constructor-unit-tests` — PASS (exit 0, Passed 220 / Total 220; 218 prior + 2 new
  AC-I2 `EnvironmentRoundTripTests`). Log `003-constructor-unit-tests.log`.
- `ui-tests` — PASS (exit 0, Passed 33 / Total 33; +8 new `LocalizationTests`,
  Category!=ui-smoke). Log `003-ui-tests.log`.
- `ui-smoke` — PASS (exit 0, Passed 1; the headless host boots and renders with
  `strings.json` resolving at run time). Log `003-ui-smoke.log`.
- `impl-log-structure`, `state-of-world-structure` — PASS (required ATX headings
  present).

Coverage: AC-I1 runtime load + editable-without-rebuild; AC-I2 language default +
serialize/deserialize + on-disk save/load round-trip; AC-I3 English fallback +
copyable completeness message + missing/malformed-resource surfaced (not thrown) +
shipped resource complete so startup is clean; AC-I4 language-neutral symbols +
no neutral glyph carried as a translatable value. None deferred.

```yaml
gates:
  berreman_unit_tests:    84
  constructor_unit_tests: 220
```

## Architecture

- **`Language` lives in `Localization.fs`, which compiles before `UserEnvironment.fs`
  (one-way dependency).** `EnvironmentSettings.language : Localization.Language`. The
  loader has NO dependency on `UserEnvironment`, so the string mechanism stays a
  standalone, headless-testable layer; `UserEnvironment` depends on it only for the
  fieldless `Language` DU. The fsproj registers `Localization.fs` immediately before
  `UserEnvironment.fs`.
- **One JSON stack, reused.** `Language` is a fieldless DU, so the shared
  `ProjectJson.options` (`WithUnionUnwrapFieldlessTags`) serialize it as a bare string
  ("English"/"Russian"), matching the schema's `enum`. No second serializer, no
  per-field converter — the round-trip is free (0.7 reuse / I.5).
- **The resource read mirrors the env schema exactly (I.5 / SchemaValidation.fs:17).**
  `strings.json` is a `<Content CopyToOutputDirectory=PreserveNewest>` item read from
  `AppContext.BaseDirectory` — the SAME build-copied-content-item + base-directory
  pattern, so it ships beside the assembly in the published layout and an
  operator/translator edits it with no rebuild. Copying (not embedding) is what makes
  the resource human-editable at run time.
- **Lookup degrades gracefully, never throws.** `lookup` tries the active language,
  then the English entry, then the key itself; `loadFromFile` returns a
  `LocalizationError` (missing / parse) rather than throwing. So a missing translation
  or a missing/malformed file is visible and debuggable but never crashes the app or
  the headless smoke host (I.3.1 / AC-I3).
- **Scientific symbols are structurally untranslatable (I.4.1).** Ψ/Δ/°/R1-R3 and the
  unit symbols are compiled `Symbols` constants, deliberately NOT entries in
  `strings.json`; `neutralSymbol` resolves them ignoring the language. A test also
  asserts no shipped entry carries a neutral glyph as a translatable value, so the
  "do-not-translate" rule cannot regress by an edit to the resource.
- **The copyable error surface is additive and headless-safe.** When (and only when)
  `Startup.localizationError` is `Some`, `Program.fs` opens a small `Window` whose
  read-only multiline `TextBox` holds the message (selectable + a Copy button that
  guards a null clipboard), deferred to the main window's `Opened`. The shipped
  resource is complete, so this never constructs under the smoke host — `App`
  framework-init is unchanged and the smoke assertions still hold.

## Deferred

- The *Settings*-ribbon language-selector control (Part D / slice 006). This slice
  provides the persisted `language` field, the `Localization` lookup API, and the
  `save` path the selector writes through; the control itself is later.
- The configurable **key-map** fields on `EnvironmentSettings` / the environment
  schema (Part E.8 / slice 005). Only the `language` field is added here; the
  `language` additions are kept stable for that follow-on edit.
- The constructor-as-default-landing edit to `Program.fs` (Part D.6 / slice 006).
  Only the startup completeness check + copyable error surface are added here.
- No third language ships; the *mechanism* supports adding one by editing
  `strings.json`, but only EN+RU ship (0.6).

## Gotchas

- **Adding `language` as a *required* schema field invalidates any pre-existing
  `environment.json` that lacks it.** This is by design (0.6 — no migration): `load`
  is total and falls back to the built-in `defaults` (English) on any validation
  failure, so an older settings file simply reverts to defaults rather than upgrading.
- **`EnvironmentRoundTripTests.fs` was edited even though it is not in the slice's
  "files in scope" list.** The slice testing plan explicitly routes the AC-I2
  `language` round-trip through "the existing `EnvironmentRoundTripTests` harness"
  (`constructor-unit-tests`), so two minimal AC-I2 tests were added there; no existing
  test or behaviour was changed. Chosen per "don't ask the user" — the testing plan is
  the controlling instruction.
- **The Edit/Write tooling emits CRLF on this host; the repo is LF (autocrlf=false,
  no `.gitattributes`).** Every touched file was normalized back to LF after editing
  (verified: 0 CR bytes, `git diff` shows only the real ~112-line insertion, no
  whole-file churn). A future editor must re-normalize after any tool-driven edit or
  the diff will show spurious whole-file churn.
- **`strings.json` keys are flat dotted strings (`menu.file`, `settings.language`).**
  Later UI slices MUST resolve text through `Localization.lookup` against these keys;
  adding UI text means adding an EN+RU pair to `strings.json` (a missing pair shows the
  copyable startup warning, not a crash).
- **The shipped resource must stay complete in BOTH languages.** A dedicated test
  (`AC-I3 the shipped resource is complete …`) asserts `startupCheck` returns `None`
  for EN and RU; an incomplete addition fails that test (and would pop the error window
  under the smoke host). Keep new keys complete.

## Changelog

- 2026-06-06 (slice 003): Land Part I — EN/RU localization. New `Localization.fs`
  (runtime-read `strings.json` loader, English-fallback `lookup`, completeness `check`
  + copyable `startupCheck`, language-neutral `Symbols`) and `strings.json` (EN/RU
  catalogue, shipped as a build-copied content item). Add `language` to
  `EnvironmentSettings` + the environment schema (default English, persisted/restored).
  Wire the startup completeness check + copyable error window in `Program.fs`. Add
  `LocalizationTests.fs` (8 tests, AC-I1/AC-I3/AC-I4) and two AC-I2 round-trip tests in
  `EnvironmentRoundTripTests.fs`. All seven gates pass (berreman_unit_tests 84 held,
  constructor_unit_tests 218 → 220, ui-tests +8, ui-smoke green).
