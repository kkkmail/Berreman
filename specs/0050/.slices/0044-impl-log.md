# Step 0044 implementation log

## Progress

- Read the shared and WIRE worker protocols, project prompt, root instructions,
  naming rules, slice, resolved gate snapshot and S16-APP-ROOT acceptance oracle.
- Confirmed `AppContext.create` still builds self-seeding stores. The existing
  `CatalogueImportProxy` already provides empty stores and live reference
  lookups; `DefaultStores.create` supplies the required import/error precedent.
- Before implementation: add the required catalogue-parity fact through the
  shared `BuiltinCatalogue` fixture and rename the independent-scope fact.
- Added the exact named fact comparing all sample, material and category records
  sorted by id, using `IncludeInactive` for versioned stores, and resolving every
  fixture preset to itself. Renamed the scope-isolation fact; its body is intact.
- Before production edits: replace self-seeding construction with the existing
  import bundle, retain partial imports on typed failure, and correct the six
  affected documentation sites without changing other composition fields.
- Replaced the composition with `CatalogueImportProxy.createInMemory`,
  `CatalogueProxy.createBuiltIn` and `importFrom`; typed failures use
  `Logger.logError` with `e.describe`. Materials, samples, categories and the
  preset library all come from that import bundle, with fresh user sources.
- Removed the obsolete opens and VersionsInUse paragraph. Updated the root,
  self-seeding-store, live-library and UI composition docs to match the wiring.
- Before local verification: preserve LF endings, review the bounded source diff,
  and build Release/x64 with the existing version-update lock left in place.
- Local Release/x64 solution build succeeded with zero errors and no warnings
  from our code. Two existing SYSLIB0051 warnings came from vendored MathNet's
  `Exceptions.cs`; no warning suppression or vendored-code edit was introduced.
- Source diff review found no whitespace errors or CRLF churn. Exactly the seven
  authorized source files are changed; the version remains `10.1.402.7`.
- Before test verification: run all constructor tests and UI smoke tests, then
  exactly the four non-smoke UI classes selected by this slice.
- All 799 constructor tests and all 208 UI smoke tests passed with zero failures
  and zero skips. Starting the selected non-smoke UI verification.
- All 36 selected non-smoke UI tests passed with zero failures and zero skips.
  The report includes exactly AppContextTests (5), LauncherTests (5),
  TableAndElementRotationTests (25) and WireUiFinalCompositionTests (1).
- The reports contain the exact new and renamed fact names and the existing
  WireUiCompositionTests / WireUi0040CompositionTests acceptance facts.
- Final source review confirms `AppContext.fs` has none of the prohibited
  self-seeding calls; the seven-file diff has no whitespace errors or CRLF churn.
- Before handoff: write the state-of-the-world with observed local results,
  inherited checkpoint counts and pending supervisor gate acceptance.
- Wrote the prescribed state-of-the-world file with every required section,
  inherited baseline values and the local build/test observations. All requested
  implementation and worker outputs are complete.

## Files modified

- Created the prescribed implementation plan and this log.
- Updated `OpticalConstructor.Ui.Tests/AppContextTests.fs`.
- Updated `OpticalConstructor.Ui/AppContext.fs`.
- Updated documentation in `OpticalConstructor.Domain/MaterialLibrary.fs`,
  `MaterialStore.fs`, `SampleStore.fs` and `ElementId.fs`.
- Updated the module documentation in `OpticalConstructor.Ui.Tests/WireUiCompositionTests.fs`.
- Created `specs/0050/.slices/0044-state-of-the-world.md`.

## Testing state

Local Release/x64 solution build passed with no errors or warnings from our
code. The existing vendored MathNet warnings are recorded above. All 799
constructor tests, 208 UI smoke tests and 36 selected UI tests passed without
failures or skips. The new fact checks full records sorted by id and every
preset's resolution, not only counts or presence. The scope-isolation fact's
body is unchanged.

Local commands used `dotnet build Berreman.slnx -c Release -nologo -v:m` from
`Berreman/`, then `dotnet test --no-build -c Release -p:Platform=x64 --no-ansi`
from each test project, with TRX reporting into the artifact paths below.
Smoke used `--filter-trait 'Category=ui-smoke'`. The selected UI run excluded
`Category=ui-smoke` and `Category=kit-acceptance` and used `--filter-class` for
each of the four classes above, matching the slice's selection membership.

No formal gate result is claimed. Formal execution and acceptance remain with
the arc-runner: build, elevation, constructor-unit-tests, ui-smoke, ui-tests,
invariant-s06-elev-list-empty, invariant-s06-no-vstest,
invariant-s06-no-kit-kit-json, invariant-s06-no-eps-axis-evaluated,
invariant-s06-complexity-mandatory and invariant-s06-no-seeded-string-preset-ids.
No implementation blocker or deferred slice requirement remains.

## Artifacts

Persistent run artifacts belong only in `specs/0050/.artifacts/`.

- `0044-local-build.log`: successful Release/x64 solution build.
- `0044-local-constructor-tests.log`, `0044-local-constructor-results/constructor.trx`.
- `0044-local-ui-smoke.log`, `0044-local-smoke-results/smoke.trx`.
- `0044-local-ui-tests.log`, `0044-local-ui-results/ui.trx`.

## Gotchas

- Preserve the pre-existing runner change to `specs/0050/.manifest.state.json`.
- Local verification is required by the coding-session instructions despite the
  worker protocol's act-only wording; local observations will be distinguished
  from formal supervisor acceptance.
- The catalogue parity assertion preserves content while changing its source;
  no pre-change failing-test claim will be made.
- No implementation deviation, new storage format or migration is needed.
- Checkpoints supply the inherited constructor count (799 at step 41) and smoke
  count (208 at step 42). No checkpoint has a `ui_tests` value; the handoff keeps
  the preceding slice's zero marker for its absence, not the selected run's 36.
- All requested implementation is ready for supervisor acceptance. Structured
  gate collections remain empty because the worker does not own formal gates.
- No version bump, git mutation, external-repository write or catalogue edit occurred.
