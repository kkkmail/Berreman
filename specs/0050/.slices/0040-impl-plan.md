# Step 0040 - CatalogueImportProxy implementation plan

Replace the seed-fed orchestrator in Domain/CatalogueImport.fs with ordered
decoding and import of CatalogueData. Reuse each LibraryEntryContent decoder,
the existing four save fields and the unchanged in-memory store construction.
Add typed decode/read errors and their intrinsic descriptions, plus importFrom
as the single read-then-import member. Categories precede materials, samples,
sources, polarizers and detectors; preserve row order within each kind and stop
at the first failure without undoing earlier saves.

Replace the five seed-dependent facts in CatalogueImportProxyTests.fs and
CatalogueImportInMemoryTests.fs with the eight specified catalogue facts;
retain the reference-equality fact. Load the shipped catalogue through
CatalogueProxy.createBuiltIn, decode expectations from its rows, and exercise
both the recording proxy and the real empty stores. No seeds or new shared
fixture are needed in this slice.

Risks are eager decoding after a failure, duplicate sample writes to the preset
store, preserving obsolete reference-identity assertions, and confusing manifest
kind order with dependency order. Record lifecycle completion on the inherited
contract's existing registry entry without declaring a new contract. Leave the
supervisor's manifest change untouched. Write the implementation log and final
state at the prescribed paths; the supervisor owns acceptance-gate execution.
