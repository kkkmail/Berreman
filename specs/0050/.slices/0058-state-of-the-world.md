# Step 0058 state of the world

## Where we are

Step 0058 completes S1.7's removal of the Domain catalogue values after callers
were converted to the shipped Built-in catalogue. The four empty stores now own
the createInMemory names. Implementation is complete and awaits the normal
supervisor verification cycle; no gate or checkpoint was changed.

## What's working

- Removed code-defined materials, categories, samples, sources, presets and fixed grouping trees.
- Deleted self-seeding factories and renamed empty stores and every caller to createInMemory.
- Preserved all well-known IDs, editor starter layers and existing store behavior.
- Updated catalogue, identity, store and live-library documentation.

## Tests

This worker ran no build, tests, analyzers or invariant gates, per the IMPLEMENT
protocol's Invariant 6. All results remain pending the supervisor's unchanged
roster: build, elevation, constructor-unit-tests, ui-smoke, ui-tests, and
invariant-s07-elev-list-empty, invariant-s07-no-vstest,
invariant-s07-no-kit-kit-json, invariant-s07-no-eps-axis-evaluated,
invariant-s07-complexity-mandatory, invariant-s07-no-seeded-string-preset-ids,
and invariant-s07-no-seeding-seam.

The ui-tests selection remains AppContextTests, TableAndElementRotationTests,
MainWorkbenchTests, LibraryControlsTests, LibraryWindowTests and
MaterialsWindowTests in OpticalConstructor.Ui.Tests. Existing test declarations
and assertions are unchanged; only empty-store calls were renamed.

These are inherited counts from `.checkpoints-json`: constructor tests at index
56, UI smoke at index 54. No ui_tests baseline is stored; zero retains step
0054's absence marker and is not a measured count. No current-cycle pass is
claimed, and the supervisor owns the next measurement.

```yaml
gates:
  constructor_unit_tests: 801
  ui_smoke_tests: 208
  ui_tests: 0
```

## Architecture

Catalogue/Builtin remains the single source of built-in entry data.
CatalogueImportProxy creates empty category, material, sample and library-entry
stores and fills them through existing write seams. The renamed public factory
signatures and bodies match the former empty factories; versioning, reference
guards, ordering and tree derivation remain unchanged. Private store builders
are retained to preserve those bodies; their sole callers supply empty lists.

MaterialIds, CategoryIds, SampleIds, PresetIds and CatalogueIds remain stable.
The Sample editor still uses starterMultilayerStructure and its two quarter-wave
thicknesses. No stored shape, catalogue content, serializer, numerical behavior
or migration path was introduced or changed.

## Deferred

No implementation work is deferred. Verification is the supervisor's required
next stage under the worker protocol.

## Gotchas

- LibraryProxyTests.fs contained one additional empty-store call omitted from
  the edit inventory. Its mechanical rename fulfills the explicit requirement
  that every caller follows; its fact and assertions are unchanged.
- The pre-rename self-seeding call search found no callers, so no existing call
  silently changed from a populated store to an empty one.
- CatalogueFiles.fs already documents its current createInMemory double;
  CatalogueImport.fs already has clean opens. No redundant documentation edit
  was made in those locations.
- The private test helpers named constantComplexity are independent of the
  deleted ConstantEpsValue extension and remain in place.
- The supervisor's initially dirty `.manifest.state.json` was left untouched.
  Normal and ignore-cr-at-eol diff sizes agree for the worker's edits.
- Commit readiness is unverified until the supervisor completes its gates.

## Changelog

- 2026-11-06, step 0058 attempt 01: retired Domain catalogue values and self-seeding factories, renamed empty stores and callers, refreshed documentation and wrote the handoff.
