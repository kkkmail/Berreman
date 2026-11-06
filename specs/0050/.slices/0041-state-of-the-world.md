# Step 0041 state of the world

## Where we are

Spec 0050 S1.6 now has the shared Built-in catalogue fixture required by S16-FIXTURE. Both constructor suites compile the same source first, with a file link in the UI suite. The two declared fixture facts are implemented and pass locally. Formal gate acceptance belongs to the arc-runner.

## What's working

- Added a lazy, once-per-process read of the shipped Built-in catalogue with diagnostic failures.
- Exposed ordered decoded lists and typed-id lookups through the shared test fixture.
- Created isolated in-memory stores populated by the existing catalogue importer on every call.
- Linked the fixture first into both constructor suites and added the two required acceptance facts.

## Tests

Local Release/x64 solution build succeeded, compiling the fixture in both suites. All 799 constructor tests passed, including both exact S16-FIXTURE fact names. All 208 UI smoke tests passed with their count unchanged. The selected AppContextTests and LibraryWindowTests passed all 43 non-smoke tests. Every run had zero failures and zero skips. Logs and TRX reports are under `.artifacts/0041-local-*`, listed in the implementation log.

The build emitted two existing SYSLIB0051 warnings in vendored MathNet's Exceptions.cs; changed code emitted no warnings. The four edited source/project files use LF, and the project diff has no whitespace errors or CRLF churn.

No formal gate was run or green-lit by the worker. The supervisor's resolved roster remains pending: `build`, `elevation`, `constructor-unit-tests`, `ui-smoke`, `ui-tests`, `invariant-s06-elev-list-empty`, `invariant-s06-no-vstest`, `invariant-s06-no-kit-kit-json`, `invariant-s06-no-eps-axis-evaluated`, `invariant-s06-complexity-mandatory`, and `invariant-s06-no-seeded-string-preset-ids`.

The following carries the existing supervisor checkpoint counts: constructor checkpoint 39 and smoke checkpoint 28. There is no prior `ui_tests` checkpoint; zero denotes that absence, not a measured result. The supervisor owns baseline updates; this round's local observed counts are reported above.

```yaml
gates:
  constructor_unit_tests: 797
  ui_smoke_tests: 208
  ui_tests: 0
```

## Architecture

`BuiltinCatalogue` holds exactly one private `Lazy<CatalogueData>`, whose factory calls `CatalogueProxy.createBuiltIn().readCatalogue()` on first use. List projections call the existing entry decoders and retain manifest row order; presets retain their relative order across source, polarizer and detector rows. No decoded-list cache or seed values were added. `freshStores` creates empty stores through `CatalogueImportProxy.createInMemory`, imports the loaded catalogue, and returns only after success. Read, decode and import errors fail with their `describe`; missing lookups name the requested id.

The new tests prove population counts, deletion isolation between two calls, stability of the fixture after a store mutation, the four required known identities, and every list entry's lookup equality. The UI suite links the fixture source rather than copying it. TestApp.fs retains its assembly attribute; its obsolete first-file comment was removed. No durable format changed, so no migration applies.

## Deferred

Nothing within the slice's implementation scope. Formal acceptance is the supervisor's next action, with no waived obligation.

## Gotchas

- The catalogue is loaded only when a fixture function is called, so missing catalogue data fails a consuming fact rather than test-process startup.
- Presets can apply to more than one control kind; the population assertion counts distinct entry ids across the four preset-kind queries.
- Local build/test verification follows the coding session's validation requirement despite the worker role's act-only wording. Final formal gate collections remain empty.
- Existing seed consumers and catalogue-import test helpers remain unchanged because replacing them is outside the four authorized files.
- The pre-existing supervisor manifest-state modification is untouched. No version bump, git mutation, external repository edit or catalogue write occurred.

## Changelog

- 2026-11-05: Added the shared Built-in catalogue fixture and its two acceptance facts; local build and 799 constructor, 208 smoke and 43 selected UI tests succeeded.
