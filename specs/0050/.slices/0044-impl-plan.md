# Step 0044 implementation plan

Wire `AppContext.create` to the existing `CatalogueImportProxy` and Built-in
catalogue reader, following `DefaultStores.create`. Keep the imported stores
and live library in the app scope, preserve every other field and the public
signature, and log typed import failures while retaining successful imports.

Update only the seven authorized source files: `AppContext.fs`,
`MaterialLibrary.fs`, `MaterialStore.fs`, `SampleStore.fs`, `ElementId.fs`,
`WireUiCompositionTests.fs` and `AppContextTests.fs`. Correct documentation made
false by the switch. Add the specified fresh-context catalogue parity fact and
rename the scope-isolation fact without changing its body. Reuse the already
linked `BuiltinCatalogue` fixture and compare complete records sorted by id.

The principal risks are incomplete import wiring, loss of live reference
lookups and accidental changes to independent app scopes. The existing importer
owns store construction and reference lookups; the new fact and existing scope
and window tests cover composition. No durable shape or migration changes apply.
Preserve the supervisor's existing `.manifest.state.json` modification.

Local build/test verification follows the coding-session instructions; formal
gate execution and acceptance remain with the supervisor. Save any captured
output only under `specs/0050/.artifacts/`, and write the prescribed log and
state-of-the-world files. Do not bump versions or mutate git state.
