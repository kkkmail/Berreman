# Step 0044 state of the world

## Where we are

Spec 0050 S1.6 now wires the product's `AppContext.create` through the Built-in
catalogue importer, matching the default-scene composition from step 0042 and
the shared test fixture. S16-APP-ROOT implementation is complete; formal gate
execution and acceptance remain with the arc-runner.

## What's working

- Filled empty app-scope stores from the Built-in catalogue before windows open.
- Logged typed import failures while retaining entries already imported.
- Composed the live library from imported presets, samples and fresh user sources.
- Added full catalogue parity coverage and preserved scope-isolation assertions.
- Corrected documentation affected by the startup change.

## Tests

Local Release/x64 solution build passed with zero errors and no warnings from
our code. Two existing SYSLIB0051 warnings came from vendored MathNet's
`Exceptions.cs`. All 799 constructor tests, 208 UI smoke tests and 36 selected
non-smoke UI tests passed, with no failures or skips. The selected run covers
AppContextTests, LauncherTests, TableAndElementRotationTests and
WireUiFinalCompositionTests, including the exact new and renamed facts.
The smoke report includes WireUiCompositionTests and WireUi0040CompositionTests.
Logs and TRX reports are under `specs/0050/.artifacts/0044-local-*`.

Formal gates remain pending with the supervisor: build, elevation,
constructor-unit-tests, ui-smoke, ui-tests, invariant-s06-elev-list-empty,
invariant-s06-no-vstest, invariant-s06-no-kit-kit-json,
invariant-s06-no-eps-axis-evaluated, invariant-s06-complexity-mandatory and
invariant-s06-no-seeded-string-preset-ids. No formal result is claimed.

The YAML carries inherited checkpoint values: constructor count 799 from step
41 and smoke count 208 from step 42. No checkpoint contains `ui_tests`; zero
preserves the preceding handoff's absence marker, not the 36-test local result.
Baseline updates remain the supervisor's responsibility.

```yaml
gates:
  constructor_unit_tests: 799
  ui_smoke_tests: 208
  ui_tests: 0
```

## Architecture

`AppContext.create` reuses `CatalogueImportProxy.createInMemory`,
`CatalogueProxy.createBuiltIn` and `importFrom`. The imported materials, samples
and categories retain the importer's live reference lookups. `Library.liveLibrary`
combines the imported preset store with those samples and fresh user sources.
The existing signature and every other context field are unchanged. The shared
`BuiltinCatalogue` fixture supplies the new fact without a second reader or
parser. No durable format or migration changes apply.

## Deferred

No implementation requirement was deferred. Formal supervisor acceptance is
pending, with no waiver or carried failure.

## Gotchas

- The Built-in catalogue must remain copied to the existing output location
  `Catalogue/Builtin/`; the Storage project already supplies it.
- Sorting is confined to test comparisons; import order is unchanged.
- The scope-isolation test was renamed exactly as required; its body is intact.
- Local verification follows the coding-session instructions despite the worker
  role's act-only wording; formal gate collections remain empty.
- The seven source files have no whitespace errors or CRLF churn. The runner's
  pre-existing `.manifest.state.json` change was not touched.
- No version bump, git mutation, external-repository write or catalogue edit occurred.

## Changelog

- 2026-11-05: Wired AppContext startup to Built-in catalogue import, refreshed affected docs and added composition coverage; local build, 799 constructor tests, 208 UI smoke tests and 36 selected UI tests passed.
